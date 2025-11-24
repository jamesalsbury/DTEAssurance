

simulate_one_trial <- function(i, j,
                               n_c, n_t,
                               control_model,
                               effect_model,
                               censoring_model,
                               recruitment_model,
                               analysis_model) {

  # --- Simulate underlying event/recruitment process ---
  data <- simulate_trial_with_recruitment(
    n_c = n_c,
    n_t = n_t,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model
  )

  # --- Apply censoring ---
  if (censoring_model$method == "Time") {
    censored <- cens_data(data, cens_method = "Time", cens_time = censoring_model$time)
  } else if (censoring_model$method == "Events") {
    censored <- cens_data(data, cens_method = "Events", cens_events = censoring_model$events)
  } else if (censoring_model$method == "IF") {
    censored <- cens_data(data, cens_method = "IF", cens_IF = censoring_model$IF)
  }

  # --- Run statistical test ---
  test_result <- survival_test(
    censored$data,
    analysis_method = analysis_model$method,
    alpha = analysis_model$alpha,
    alternative = analysis_model$alternative_hypothesis,
    rho = analysis_model$rho,
    gamma = analysis_model$gamma,
    t_star = analysis_model$t_star,
    s_star = analysis_model$s_star
  )

  # --- Output ---
  list(
    Signif       = test_result$Signif,
    observed_HR  = test_result$observed_HR,
    sample_size  = censored$sample_size,
    cens_time    = censored$cens_time
  )
}



simulate_trial_with_recruitment <- function(n_c, n_t,
                                control_model,
                                effect_model,
                                recruitment_model) {
  # --- Sample control parameters ---
  lambda_c_i <- NA
  gamma_c_i <- NA

  if (control_model$dist == "Exponential") {
    if (control_model$parameter_mode == "Fixed") {
      if (control_model$fixed_type == "Parameters"){
        lambda_c_i <- control_model$lambda
      } else if (control_model$fixed_type == "Landmark"){
        lambda_c_i <- -log(control_model$surv_t1) / control_model$t1
      }
    } else if (control_model$parameter_mode == "Distribution") {
      lambda_c_i <- -log(stats::rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)) / control_model$t1
    }
    gamma_c_i <- NULL
  }

  if (control_model$dist == "Weibull") {
    if (control_model$parameter_mode == "Fixed") {
      if (control_model$fixed_type == "Parameters") {
        lambda_c_i <- control_model$lambda
        gamma_c_i <- control_model$gamma
      } else if (control_model$fixed_type == "Landmark") {
        WeibFunc <- function(params) {
          lambda <- params[1]
          k <- params[2]
          c(exp(-(control_model$t1 * lambda)^k) - control_model$surv_t1,
            exp(-(control_model$t2 * lambda)^k) - control_model$surv_t2)
        }
        solution <- nleqslv::nleqslv(c(1, 1), fn = WeibFunc)
        lambda_c_i <- solution$x[1]
        gamma_c_i <- solution$x[2]
      }
    } else if (control_model$parameter_mode == "Distribution") {
      sampledS1 <- stats::rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)
      sampledDelta <- stats::rbeta(1, control_model$diff_Beta_a, control_model$diff_Beta_b)
      sampledS2 <- sampledS1 - sampledDelta
      solution <- nleqslv::nleqslv(c(10, 1), function(params) {
        lambda <- params[1]
        k <- params[2]
        c(exp(-(control_model$t1 / lambda)^k) - sampledS1,
          exp(-(control_model$t2 / lambda)^k) - sampledS2)
      })
      lambda_c_i <- 1 / solution$x[1]
      gamma_c_i <- solution$x[2]
    }
  }

  if (is.na(lambda_c_i)) {
    stop("lambda_c_i was not assigned. Check control_model settings.")
  }

  # --- Sample treatment effect ---

  if (stats::runif(1) > effect_model$P_S) {
    delay_time <- 0
    post_delay_HR <- 1
  } else if (stats::runif(1) > effect_model$P_DTE) {
    delay_time <- 0
    post_delay_HR <- SHELF::sampleFit(effect_model$HR_SHELF, n = 1)[, effect_model$HR_dist]
  } else {
    delay_time <- SHELF::sampleFit(effect_model$delay_SHELF, n = 1)[, effect_model$delay_dist]
    post_delay_HR <- SHELF::sampleFit(effect_model$HR_SHELF, n = 1)[, effect_model$HR_dist]
  }

  # --- Simulate survival data ---
  data <- sim_dte(n_c, n_t, lambda_c_i, delay_time, post_delay_HR,
                  dist = control_model$dist, gamma_c = gamma_c_i)

  # --- Add recruitment time ---
  data <- add_recruitment_time(data,
                               rec_method = recruitment_model$method,
                               rec_period = recruitment_model$period,
                               rec_power = recruitment_model$power,
                               rec_rate = recruitment_model$rate,
                               rec_duration = recruitment_model$duration)

  return(data)
}


apply_GSD_to_trial <- function(n_c,
                               n_t,
                               trial_data,
                               design,
                               total_events,
                               GSD_model,          # added
                               control_model = NULL,       # needed for BPP
                               effect_model = NULL,
                               recruitment_model = NULL,
                               analysis_model = NULL,
                               n_BPP_sims = 50) {



  trial_data <- trial_data[order(trial_data$pseudo_time),]

  if (GSD_model$futility_type %in% c("beta", "none")) {

    info_rates <- design$informationRates

  } else if (GSD_model$futility_type == "BPP") {

    info_rates <- sort(unique(
      c(GSD_model$alpha_IF,
        GSD_model$futility_IF)
    ))
  }



  event_thresholds  <- ceiling(info_rates * total_events)
  n_interims        <- length(info_rates)

  decision          <- "Continue"
  stop_time         <- NA

  for (i in seq_len(n_interims - 1)) {

    IF_here  <- info_rates[i]
    n_events <- event_thresholds[i]
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data |>
      dplyr::filter(.data$rec_time <= t_interim)

    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time
    )

    # Z-statistic at this IA
    fit  <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    z_stat <- -summary(fit)$coefficients[, "z"]

    ## ---- efficacy boundary (map IF_here to rpact design) ----
    eff_idx <- which(abs(design$informationRates - IF_here) < 1e-8)

    if (length(eff_idx) == 1) {
      eff_bound <- design$criticalValues[eff_idx]
    } else {
      eff_bound <- NA
    }

    # 1) Efficacy check (if this IF is an alpha look)
    if (!is.na(eff_bound) && z_stat > eff_bound) {
      decision  <- "Stop for efficacy"
      stop_time <- t_interim
      break
    }

    # 2) Beta-spending futility (unchanged logic, only if futility_type == "beta")
    if (!is.null(GSD_model) &&
        GSD_model$futility_type == "beta") {

      fut_idx <- which(abs(design$informationRates - IF_here) < 1e-8)

      fut_bound <- if (length(fut_idx) == 1 &&
                       fut_idx <= length(design$futilityBounds)) {
        design$futilityBounds[fut_idx]
      } else {
        NA
      }

      if (!is.na(fut_bound) && z_stat < fut_bound) {
        decision  <- "Stop for futility (beta)"
        stop_time <- t_interim
        break
      }
    }

    # 3) BPP futility (if this IF is a futility look)
    if (!is.null(GSD_model) &&
        GSD_model$futility_type == "BPP" &&
        IF_here %in% GSD_model$futility_IF) {

      posterior_samples <- DTEAssurance::update_priors(
        eligible_df,
        control_model = control_model,
        effect_model  = effect_model,
        n_samples = 100
      )

      BPP_out <- DTEAssurance::BPP_func(
        eligible_df,
        posterior_samples,
        control_distribution = control_model$dist,
        n_c_planned = n_c,
        n_t_planned = n_t,
        rec_time_planned = recruitment_model$period,
        df_cens_time = t_interim,
        censoring_model = list(method = "Events", events = GSD_model$events),
        analysis_model = analysis_model,
        n_sims = n_BPP_sims
      )

      BPP_val <- mean(BPP_out$BPP_df$success)

      if (BPP_val < GSD_model$BPP_threshold) {
        decision  <- "Stop for futility (BPP)"
        stop_time <- t_interim
        break
      }
    }

  } # end loop


  # -------------------------------
  # 4) Final analysis (unchanged)
  # -------------------------------
  if (is.na(stop_time)) {
    i <- n_interims
    n_events <- event_thresholds[i]
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data |>
      dplyr::filter(.data$rec_time <= t_interim)

    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time
    )

    fit  <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    z_stat <- -summary(fit)$coefficients[, "z"]

    eff_bound <- design$criticalValues[i]

    decision <- ifelse(z_stat > eff_bound,
                       "Successful at final",
                       "Unsuccessful at final")

    stop_time <- t_interim
  }

  sample_size <- sum(trial_data$rec_time <= stop_time)

  return(list(
    decision    = decision,
    stop_time   = stop_time,
    sample_size = sample_size
  ))
}



summarize_gsd_results <- function(gsd_outcomes) {
  n <- length(gsd_outcomes)

  decisions   <- sapply(gsd_outcomes, `[[`, "decision")
  stop_times  <- sapply(gsd_outcomes, `[[`, "stop_time")
  sample_size <- sapply(gsd_outcomes, `[[`, "sample_size")

  # Decision rates
  assurance       <- assurance <- mean(decisions %in% c("Stop for efficacy", "Successful at final"))
  expected_duration <- mean(stop_times[is.finite(stop_times)])
  expected_sample_size <- mean(sample_size)


  return(list(
    assurance         = assurance,
    expected_duration = expected_duration,
    expected_sample_size = expected_sample_size
  ))
}


calc_BPP_hist <- function(n_c, n_t,
                          control_dist = "Exponential",
                          t1 = NULL, t2 = NULL,
                          t1_Beta_a = NULL, t1_Beta_b = NULL,
                          diff_Beta_a = NULL, diff_Beta_b = NULL,
                          delay_time_SHELF, delay_time_dist = "hist",
                          post_delay_HR_SHELF, post_delay_HR_dist = "hist",
                          P_S = 1, P_DTE = 0, cens_events = NULL, IF = NULL,
                          rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                          alpha_spending = NULL, beta_spending = NULL, IF_list = NULL, k = 2,
                          N = 50, M = 50){

  outerBPPVec <- rep(NA, N)

  design <- rpact::getDesignGroupSequential(typeOfDesign = "asUser",
                                            informationRates = IF_list,
                                            userAlphaSpending = alpha_spending,
                                            typeBetaSpending = "bsUser",
                                            userbetaSpending = beta_spending)

  designList <- list(
    critValues = design$criticalValues,
    futBounds = design$futilityBounds
  )

  for (i in 1:N){

    if (control_dist=="Exponential"){
      lambda_c <- -log(stats::rbeta(1, t1_Beta_a, t1_Beta_b)) / t1
    } else if (control_dist=="Weibull"){
      sampledS1to <- stats::rbeta(1, t1_Beta_a, t1_Beta_b)
      sampledDelta1 <- stats::rbeta(1, diff_Beta_a, diff_Beta_b)
      sampledS1toPrime <- sampledS1to - sampledDelta1

      solution <- nleqslv::nleqslv(c(10, 1), function(params) {
        lambda <- params[1]
        k <- params[2]
        c(exp(-(t1 / lambda)^k) - sampledS1to,
          exp(-(t2 / lambda)^k) - sampledS1toPrime)
      })

      lambda_c <- 1 / solution$x[1]
      gamma_c <- solution$x[2]
    }

    if (stats::runif(1) > P_S){
      delay_time <- 0
      post_delay_HR <- 1
    } else {
      if (stats::runif(1) > P_DTE){
        delay_time <- 0
        post_delay_HR_sample <- SHELF::sampleFit(post_delay_HR_SHELF, n = 1)
        post_delay_HR <- post_delay_HR_sample[,post_delay_HR_dist]
      } else{
        delay_time_sample <- SHELF::sampleFit(delay_time_SHELF, n = 1)
        post_delay_HR_sample <- SHELF::sampleFit(post_delay_HR_SHELF, n = 1)
        delay_time <- delay_time_sample[,delay_time_dist]
        post_delay_HR <- post_delay_HR_sample[,post_delay_HR_dist]
      }
    }

    data <- sim_dte(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = control_dist, gamma_c = gamma_c)

    if (rec_method=="power"){
      data <- add_recruitment_time(data, rec_method, rec_period, rec_power)
    }

    if (rec_method == "PWC"){
      data <- add_recruitment_time(data, rec_method, rec_rate, rec_duration)
    }

    stopEff <- FALSE
    stopFut <- FALSE

    if (IF_list[1] <= IF){
      data_after_cens <- DTEAssurance::cens_data(data, cens_method = "Events", cens_events = cens_events * IF_list[1])
      coxmodel <- survival::coxph(survival::Surv(.data$survival_time, .data$status) ~ .data$group, data = data_after_cens$data)
      Z_Score <- -(stats::coef(summary(coxmodel))[, 4])

      stopEff <- Z_Score > designList$critValues[1]
      stopFut <- Z_Score < designList$futBounds
    }

    if (stopEff || stopFut){
      if (stopEff) outerBPPVec[i] <- 1
      if (stopFut) outerBPPVec[i] <- 0
    } else {

      data_after_cens <- DTEAssurance::cens_data(data, cens_method = "Events", cens_events = cens_events * IF)
      data <- data_after_cens$data
      data <- data[order(data$group), ]

      if (delay_time_dist == "beta") {
        distParambigT <- paste0("bigT2 ~ dbeta(", delay_time_SHELF$Beta[1], ", ", delay_time_SHELF$Beta[2], ")")
      } else if (delay_time_dist == "gamma") {
        distParambigT <- paste0("bigT2 ~ dgamma(", delay_time_SHELF$Gamma[1], ", ", delay_time_SHELF$Gamma[2], ")")
      } else if (delay_time_dist == "lognormal") {
        distParambigT <- paste0("bigT2 ~ dlnorm(", delay_time_SHELF$Log.normal[1], ", ", 1/delay_time_SHELF$Log.normal[2]^2, ")")
      }

      if (post_delay_HR_dist == "beta") {
        distParamHR <- paste0("HR2 ~ dbeta(", post_delay_HR_SHELF$Beta[1], ", ", post_delay_HR_SHELF$Beta[2], ")")
      } else if (post_delay_HR_dist == "gamma") {
        distParamHR <- paste0("HR2 ~ dgamma(", post_delay_HR_SHELF$Gamma[1], ", ", post_delay_HR_SHELF$Gamma[2], ")")
      } else if (post_delay_HR_dist == "lognormal") {
        distParamHR <- paste0("HR2 ~ dlnorm(", post_delay_HR_SHELF$Log.normal[1], ", ", 1/post_delay_HR_SHELF$Log.normal[2]^2, ")")
      } else if (post_delay_HR_dist == "student-t") {
        distParamHR <- paste0("HR2 ~ dt(", post_delay_HR_SHELF$Student.t[1], ", ", post_delay_HR_SHELF$Student.t[2], ", ", post_delay_HR_SHELF$Student.t[3], ")")
      } else if (post_delay_HR_dist == "normal") {
        distParamHR <- paste0("HR2 ~ dnorm(", post_delay_HR_SHELF$Normal[1], ", ", 1/post_delay_HR_SHELF$Normal[2]^2, ")")
      }

      modelString <- paste0(
        "data {\n",
        "  for (j in 1:m){\n",
        "    zeros[j] <- 0\n",
        "  }\n",
        "}\n",
        "\n",
        "model {\n",
        "  C <- 10000\n",
        "  for (i in 1:n){\n",
        "    zeros[i] ~ dpois(zeros.mean[i])\n",
        "    zeros.mean[i] <-  -l[i] + C\n",
        "    l[i] <- ifelse(datEvent[i]==1, log(lambda_c)-(lambda_c*datTimes[i]), -(lambda_c*datTimes[i]))\n",
        "  }\n",
        "  for (i in (n+1):m){\n",
        "    zeros[i] ~ dpois(zeros.mean[i])\n",
        "    zeros.mean[i] <-  -l[i] + C\n",
        "    l[i] <- ifelse(datEvent[i]==1, ifelse(datTimes[i]<bigT, log(lambda_c)-(lambda_c*datTimes[i]), log(lambda_t)-lambda_t*(datTimes[i]-bigT)-(bigT*lambda_c)),\n",
        "      ifelse(datTimes[i]<bigT, -(lambda_c*datTimes[i]), -(lambda_c*bigT)-lambda_t*(datTimes[i]-bigT)))\n",
        "  }\n",
        " \n",
        "    lambda_c ~ dbeta(1, 1)T(0,)\n",
        "   \n",
        "    mixT ~ dbern(1-P_S*P_DTE)\n",
        "    bigT <- mixT * bigT1 + (1-mixT) * bigT2\n",
        "    bigT1 ~ dnorm(0, 100)T(0,)\n",
        "    ", distParambigT, "\n",
        "   \n",
        "    mixHR ~ dbern(1-P_S)\n",
        "    HR <- mixHR * HR1 + (1-mixHR) * HR2\n",
        "    HR1 ~ dnorm(1, 10000)T(0,)\n",
        "    ", distParamHR, "\n",
        "   \n",
        "    lambda_t <- lambda_c*HR\n",
        "}"
      )

      model = rjags::jags.model(textConnection(modelString), data = list(datTimes = data$survival_time,
                                                                         datEvent = data$status, n = sum(data$group=="Control"),
                                                                         m=nrow(data),
                                                                         P_S = P_S,
                                                                         P_DTE = P_DTE), quiet = TRUE)

      stats::update(model, n.iter=50, progress.bar = "none")
      output = rjags::coda.samples(model = model, variable.names = c("HR", "bigT", "lambda_c"), n.iter = 100, progress.bar = "none")

      cPatientsLeft <- n_c - sum(data$group == "Control")
      tPatientsLeft <- n_t - sum(data$group == "Treatment")

      HRoutput <- as.numeric(unlist(output[,1]))
      bigToutput <- as.numeric(unlist(output[,2]))
      lambda_coutput <- as.numeric(unlist(output[,3]))

      BPPVec <- rep(NA, M)

      for (j in 1:M){

        unenrolledrec_times <- stats::runif(cPatientsLeft + tPatientsLeft, data_after_cens$cens_time, rec_period)

        sampledHR <- sample(HRoutput, 1)
        sampledbigT <- sample(bigToutput, 1)
        sampledlambda_c <- sample(lambda_coutput, 1)
        sampledlambda_t <- sampledlambda_c * sampledHR

        CP <- exp(-(sampledlambda_c * sampledbigT))
        u <- stats::runif(tPatientsLeft)

        unenrolledData <- data.frame(
          time = c(stats::rexp(cPatientsLeft, rate = sampledlambda_c),
                   ifelse(u > CP, (-log(u)) / sampledlambda_c, (1 / sampledlambda_t) * (sampledbigT * sampledlambda_t - log(u) - sampledbigT * sampledlambda_c))),
          group = c(rep("Control", cPatientsLeft), rep("Treatment", tPatientsLeft)),
          rec_time = unenrolledrec_times
        )

        unenrolledData$pseudo_time <- unenrolledData$time + unenrolledData$rec_time

        censoredData <- data[data$status == 0, ]

        cCensored <- sum(censoredData$group == "Control")
        tCensored <- sum(censoredData$group == "Treatment")

        cCensoredData <- censoredData %>%
          dplyr::filter(.data$group == "Control")

        cCensoredData$finalsurvTime <- cCensoredData$survival_time + stats::rexp(cCensored, rate = sampledlambda_c)
        cCensoredData$finalPsuedoTime <- cCensoredData$rec_time + cCensoredData$finalsurvTime

        tBeforeDelay <- censoredData %>%
          dplyr::filter(.data$group == "Treatment") %>%
          dplyr::filter(.data$survival_time < sampledbigT)

        tAfterDelay <- censoredData %>%
          dplyr::filter(.data$group == "Treatment") %>%
          dplyr::filter(.data$survival_time > sampledbigT)

        tBeforeDelay$IASurv <- tBeforeDelay$survival_time + stats::rexp(nrow(tBeforeDelay), rate = sampledlambda_c)

        tBeforeDelay1 <- tBeforeDelay %>%
          dplyr::filter(.data$IASurv < sampledbigT)

        tBeforeDelay2 <- tBeforeDelay %>%
          dplyr::filter(.data$IASurv > sampledbigT)

        tBeforeDelay2$IASurv2 <- sampledbigT + stats::rexp(nrow(tBeforeDelay2), rate = sampledlambda_t)
        tAfterDelay$IASurv <- tAfterDelay$survival_time + stats::rexp(nrow(tAfterDelay), rate = sampledlambda_t)

        tBeforeDelay1$IApsuedoTime <- tBeforeDelay1$IASurv + tBeforeDelay1$rec_time
        tBeforeDelay2$IApsuedoTime <- tBeforeDelay2$IASurv2 + tBeforeDelay2$rec_time
        tAfterDelay$IApsuedoTime <- tAfterDelay$IASurv + tAfterDelay$rec_time

        cCensoredData <- cCensoredData[, c(8, 2, 3, 9)]
        tBeforeDelay1 <- tBeforeDelay1[, c(8, 2, 3, 9)]
        tBeforeDelay2 <- tBeforeDelay2[, c(9, 2, 3, 10)]
        tAfterDelay <- tAfterDelay[, c(8, 2, 3, 9)]

        colnames(cCensoredData) <- c("time", "group", "rec_time", "pseudo_time")
        colnames(tBeforeDelay1) <- c("time", "group", "rec_time", "pseudo_time")
        colnames(tBeforeDelay2) <- c("time", "group", "rec_time", "pseudo_time")
        colnames(tAfterDelay) <- c("time", "group", "rec_time", "pseudo_time")

        finalDataset <- data %>%
          dplyr::filter(.data$status == 1)

        finalDataset <- finalDataset[, 1:4]

        finalDataset <- rbind(finalDataset, tBeforeDelay1, tBeforeDelay2, tAfterDelay, unenrolledData, cCensoredData)

        censTime1 <- sort(finalDataset$pseudo_time)[cens_events]
        finalDataset$status <- finalDataset$pseudo_time <= censTime1
        finalDataset$status <- as.integer(finalDataset$status)
        finalDataset$enrolled <- finalDataset$rec_time <= censTime1
        finalDataset <- finalDataset[finalDataset$enrolled == TRUE, ]
        finalDataset$survival_time <- ifelse(finalDataset$pseudo_time > censTime1, censTime1 - finalDataset$rec_time, finalDataset$time)

        coxmodel <- survival::coxph(survival::Surv(.data$survival_time, .data$status) ~ .data$group, data = finalDataset)
        Z_Score <- -(stats::coef(summary(coxmodel))[, 4])

        BPPVec[j] <- Z_Score > designList$critValues[2]

      } # end j loop

      outerBPPVec[i] <- mean(BPPVec)

    } # end else (not stopped)

  } # end i loop

  return(list(outerBPPVec = outerBPPVec))

} # end function


make_prior_name_jags <- function(fit, dist) {
  dist <- tolower(dist)

  if (dist == "gamma") {
    shape <- fit$Gamma$shape[1]
    rate  <- fit$Gamma$rate[1]
    return(sprintf("dgamma(%0.6f, %0.6f)", shape, rate))
  }

  if (dist == "beta") {
    a <- fit$Beta$shape1[1]
    b <- fit$Beta$shape2[1]
    return(sprintf("dbeta(%0.6f, %0.6f)", a, b))
  }

  if (dist == "normal") {
    mu <- fit$Normal$mean[1]
    sd <- fit$Normal$sd[1]
    tau <- 1/(sd*sd)
    return(sprintf("dnorm(%0.6f, %0.6f)", mu, tau))
  }

  if (dist == "student.t") {
    mu <- fit$Student.t$location[1]
    scale <- fit$Student.t$scale[1]
    df <- fit$Student.t$df[1]
    tau <- 1/(scale*scale)
    return(sprintf("dt(%0.6f, %0.6f, %0.6f)", mu, tau, df))
  }

  if (dist == "log.normal" || dist == "lognormal") {
    mu <- fit$Log.normal$mean.log.X[1]
    sd <- fit$Log.normal$sd.log.X[1]
    tau <- 1/(sd*sd)
    return(sprintf("dlnorm(%0.6f, %0.6f)", mu, tau))
  }

  stop("Distribution '", dist, "' cannot be represented in JAGS. Please use 'Gamma', 'Beta' 'Normal', 'Student.t' or 'Log.normal'")
}


make_prior_name_stan <- function(fit, dist) {
  dist <- tolower(dist)

  # Return list with:
  # $prior     -> valid Stan prior statement
  # $constraint -> optional <lower=...,upper=...> for parameter block

  if (dist == "gamma") {
    shape <- fit$Gamma$shape[1]
    rate  <- fit$Gamma$rate[1]
    return(list(
      prior      = sprintf("gamma(%0.6f, %0.6f)", shape, rate),
      constraint = "<lower=1e-6>"
    ))
  }

  if (dist == "beta") {
    a <- fit$Beta$shape1[1]
    b <- fit$Beta$shape2[1]
    return(list(
      prior      = sprintf("beta(%0.6f, %0.6f)", a, b),
      constraint = "<lower=1e-6, upper=1>"
    ))
  }

  if (dist == "normal") {
    mu <- fit$Normal$mean[1]
    sd <- fit$Normal$sd[1]
    return(list(
      prior      = sprintf("normal(%0.6f, %0.6f)", mu, sd),
      constraint = ""
    ))
  }

  if (dist == "student.t") {
    mu    <- fit$Student.t$location[1]
    scale <- fit$Student.t$scale[1]
    df    <- fit$Student.t$df[1]
    return(list(
      prior      = sprintf("student_t(%0.6f, %0.6f, %0.6f)", df, mu, scale),
      constraint = ""
    ))
  }

  if (dist == "log.normal" || dist == "lognormal") {
    mu <- fit$Log.normal$mean.log.X[1]
    sd <- fit$Log.normal$sd.log.X[1]
    return(list(
      prior      = sprintf("lognormal(%0.6f, %0.6f)", mu, sd),
      constraint = "<lower=1e-6>"
    ))
  }

  stop("Distribution '", dist, "' cannot be represented in Stan.")
}


single_calibration_rep <- function(i,
                                    n_c, n_t,
                                    control_model,
                                    effect_model,
                                    recruitment_model,
                                    IA_model,
                                    analysis_model) {

    data <- DTEAssurance:::simulate_trial_with_recruitment(
      n_c = n_c,
      n_t = n_t,
      control_model = control_model,
      effect_model = effect_model,
      recruitment_model = recruitment_model
    )

    censored_data <- DTEAssurance::cens_data(data, cens_method = "Events", cens_events = IA_model$events*IA_model$IF)

    data <- censored_data$data

    posterior_samples <- DTEAssurance::update_priors(data, control_model = control_model,
                                                     effect_model = effect_model,
                                                     n_samples = 100)

    BPP_outcome <-  DTEAssurance::BPP_func(data,
                                           posterior_samples,
                                           control_distribution = control_model$dist,
                                           n_c_planned = n_c,
                                           n_t_planned = n_t,
                                           rec_time_planned = recruitment_model$period,
                                           df_cens_time = censored_data$cens_time,
                                           censoring_model = list(method = "Events", events = IA_model$events),
                                           analysis_model = analysis_model,
                                           n_sims = 50)


    return(list(BPP_outcome = BPP_outcome, cens_time = censored_data$cens_time))


}


make_rpact_design_from_GSD_model <- function(GSD_model) {

  # 1. Extract alpha
  alpha_IF       <- GSD_model$alpha_IF
  alpha_spending <- GSD_model$alpha_spending

  # 2. Futility type
  fut_type <- GSD_model$futility_type

  # 3. Combined IF grid
  if (fut_type == "beta") {

    beta_IF       <- GSD_model$futility_IF
    beta_spending <- GSD_model$beta_spending
    IF_all <- sort(unique(c(alpha_IF, beta_IF)))

  } else if (fut_type %in% c("none", "BPP")) {

    # For "none" and "BPP", futility boundaries not needed
    IF_all <- sort(unique(alpha_IF))

  } else {
    stop("Unknown futility type in GSD_model")
  }

  K <- length(IF_all)

  #==================================================
  # 4. Expand alpha
  #==================================================
  alpha_spending_full <- numeric(K)
  idx <- 1
  for (k in seq_len(K)) {
    if (idx <= length(alpha_IF) && IF_all[k] == alpha_IF[idx]) {
      alpha_spending_full[k] <- alpha_spending[idx]
      idx <- idx + 1
    } else {
      alpha_spending_full[k] <- if (k == 1) 0 else alpha_spending_full[k-1]
    }
  }

  #==================================================
  # 5. Expand beta
  #==================================================
  if (fut_type == "beta") {

    beta_spending_full <- numeric(K)
    idx <- 1

    for (k in seq_len(K)) {
      if (idx <= length(beta_IF) && IF_all[k] == beta_IF[idx]) {
        beta_spending_full[k] <- beta_spending[idx]   # cumulative
        idx <- idx + 1
      } else {
        beta_spending_full[k] <- if (k == 1) 0 else beta_spending_full[k-1]
      }
    }

  } else if (fut_type %in% c("none", "BPP")) {

    # No futility boundaries for rpact
    beta_spending_full <- rep(0, K)
  }

  #==================================================
  # 6. Build rpact design
  #==================================================
  if (fut_type == "beta") {

    design <- rpact::getDesignGroupSequential(
      typeOfDesign      = "asUser",
      informationRates  = IF_all,
      userAlphaSpending = alpha_spending_full,
      typeBetaSpending  = "bsUser",
      userBetaSpending  = beta_spending_full
    )

  } else {  # fut_type == "none" or "BPP"

    design <- rpact::getDesignGroupSequential(
      typeOfDesign      = "asUser",
      informationRates  = IF_all,
      userAlphaSpending = alpha_spending_full,
      typeBetaSpending  = "none"
    )
  }

  return(list(
    design              = design,
    IF_all              = IF_all,
    alpha_spending_full = alpha_spending_full,
    beta_spending_full  = beta_spending_full
  ))
}




#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Package imports
#'
#' @name utils-pipe
#' @keywords internal
#'
#' @importFrom rlang .data
NULL




