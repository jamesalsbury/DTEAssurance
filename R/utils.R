

simulate_one_trial <- function(i, j,
                               n_c, n_t,
                               control_model,
                               effect_model,
                               censoring_model,
                               recruitment_model,
                               analysis_model) {

  data <- simulate_trial_with_recruitment(n_c, n_t, control_model, effect_model, recruitment_model)

  # --- Apply censoring ---
  if (censoring_model$method == "Time") {
    censored <- cens_data(data, cens_method = "Time", cens_time = censoring_model$time)
  } else if (censoring_model$method == "Events") {
    censored <- cens_data(data, cens_method = "Events", cens_events = censoring_model$events)
  } else if (censoring_model$method == "IF") {
    censored <- cens_data(data, cens_method = "IF", cens_IF = censoring_model$IF)
  }

  # --- Run statistical test ---
  test_result <- survival_test(censored$data,
                               analysis_method = analysis_model$method,
                               alpha = analysis_model$alpha,
                               alternative = analysis_model$alternative_hypothesis,
                               rho = analysis_model$rho,
                               gamma = analysis_model$gamma,
                               t_star = analysis_model$t_star,
                               s_star = analysis_model$s_star)

  # --- Return results ---
  return(list(
    Signif = test_result$Signif,
    observed_HR = test_result$observed_HR,
    sample_size = censored$sample_size,
    cens_time = censored$cens_time
  ))
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
      lambda_c_i <- control_model$lambda
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


apply_GSD_to_trial <- function(trial_data, design, total_events) {

  trial_data <- trial_data[order(trial_data$pseudo_time),]

  # Interim setup
  info_rates        <- design$informationRates
  event_thresholds  <- ceiling(info_rates * total_events)
  n_interims        <- length(info_rates)

  # Initialize tracking
  decision          <- "Continue"
  stop_time         <- NA
  boundary_crossed  <- NA
  interim_results   <- vector("list", n_interims)

  for (i in seq_len(n_interims - 1)) {
    n_events <- event_thresholds[i]

    # Interim calendar time is defined by the nth event
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)

    # Censoring logic
    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time)

    fit     <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    fit_summary <- summary(fit)
    z_stat <- -fit_summary$coefficients[, "z"]

   # cat("The ", i, "z is: ", z_stat, "\n")


    eff_bound <- design$criticalValues[i]
    fut_bound <- if (i <= length(design$futilityBounds)) design$futilityBounds[i] else NA

    if (!is.na(eff_bound) && z_stat > eff_bound) {
      decision         <- "Stop for efficacy"
      stop_time        <- t_interim
      boundary_crossed <- "Efficacy"
      break
    } else if (!is.na(fut_bound) && z_stat < fut_bound) {
      decision         <- "Stop for futility"
      stop_time        <- t_interim
      boundary_crossed <- "Futility"
      break
    }

    interim_results[[i]] <- list(
      interim         = i,
      n_events        = n_events,
      z_stat          = z_stat,
      efficacy_bound  = eff_bound,
      futility_bound  = fut_bound
    )
  }



  # Final analysis
  if (is.na(stop_time) || !is.finite(stop_time)) {
    i <- n_interims
    n_events <- event_thresholds[i]

    # Interim calendar time is defined by the nth event
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)

    # Censoring logic
    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time)

    fit     <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    fit_summary <- summary(fit)
    z_stat <- -fit_summary$coefficients[, "z"]

    #cat("The final z is: ", z_stat, "\n")

        eff_bound <- design$criticalValues[i]

        if (!is.na(z_stat) && z_stat > eff_bound) {
          decision         <- "Successful at final"
          boundary_crossed <- "Efficacy"
        } else {
          decision         <- "Unsuccessful at final"
          boundary_crossed <- NA
        }

        stop_time <- t_interim

        interim_results[[i]] <- list(
          interim         = i,
          n_events        = n_events,
          z_stat          = z_stat,
          efficacy_bound  = eff_bound,
          futility_bound  = NA
        )
  }

  sample_size <- sum(trial_data$rec_time <= stop_time)

  return(list(
    decision         = decision,
    stop_time        = stop_time,
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
      data_after_cens <- cens_data(data, cens_method = "Events", cens_events = cens_events * IF_list[1])
      coxmodel <- survival::coxph(survival::Surv(.data$survival_time, .data$status) ~ .data$group, data = data_after_cens$data)
      Z_Score <- -(stats::coef(summary(coxmodel))[, 4])

      stopEff <- Z_Score > designList$critValues[1]
      stopFut <- Z_Score < designList$futBounds
    }

    if (stopEff || stopFut){
      if (stopEff) outerBPPVec[i] <- 1
      if (stopFut) outerBPPVec[i] <- 0
    } else {

      data_after_cens <- cens_data(data, cens_method = "Events", cens_events = cens_events * IF)
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




