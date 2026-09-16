

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
                               GSD_model,
                               control_model = NULL,
                               effect_model = NULL,
                               recruitment_model = NULL,
                               analysis_model = NULL,
                               update_priors_sims = 1000,   # was hardcoded 100
                               n_BPP_sims = 1000) {         # was hardcoded default 50

  trial_data <- trial_data[order(trial_data$pseudo_time),]

  if (GSD_model$futility_type %in% c("Beta", "none")) {
    info_rates <- design$informationRates
  } else if (GSD_model$futility_type == "BPP") {
    info_rates <- sort(unique(c(GSD_model$alpha_IF, GSD_model$futility_IF)))
  }

  event_thresholds  <- ceiling(info_rates * total_events)
  n_interims        <- length(info_rates)
  decision          <- "Continue"
  stop_time         <- NA
  BPP_val           <- NA
  converged         <- NA
  Z_probs           <- rep(NA_real_, 3)
  names(Z_probs)    <- c("P_Z1", "P_Z2", "P_Z3")

  for (i in seq_len(n_interims - 1)) {
    IF_here   <- info_rates[i]
    n_events  <- event_thresholds[i]
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)

    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time
    )

    fit    <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    z_stat_here <- -summary(fit)$coefficients[, "z"]

    eff_idx <- which(abs(design$informationRates - IF_here) < 1e-8)
    eff_bound <- if (length(eff_idx) == 1) design$criticalValues[eff_idx] else NA

    # 1) Efficacy check (if this IF is an alpha look)
    if (!is.na(eff_bound) && z_stat_here > eff_bound) {
      decision  <- "Stop for efficacy"
      stop_time <- t_interim
      break
    }

    # 2) Beta-spending futility
    if (!is.null(GSD_model) && GSD_model$futility_type == "Beta") {
      fut_idx <- which(abs(design$informationRates - IF_here) < 1e-8)
      fut_bound <- if (length(fut_idx) == 1 && fut_idx <= length(design$futilityBounds)) {
        design$futilityBounds[fut_idx]
      } else NA

      if (!is.na(fut_bound) && z_stat_here < fut_bound) {
        decision  <- "Stop for futility"
        stop_time <- t_interim
        break
      }
    }

    # 3) BPP futility (if this IF is a futility look)
    if (!is.null(GSD_model) &&
        GSD_model$futility_type == "BPP" &&
        IF_here %in% GSD_model$futility_IF) {

      # ---------------------------------------------------------------------
      # FIX: guard against unsupported multiple sequential BPP futility looks
      # (would require nested posterior-predictive simulation -- out of
      # scope; see manuscript Limitations).
      # ---------------------------------------------------------------------
      remaining_futility_IFs <- GSD_model$futility_IF[GSD_model$futility_IF > IF_here]
      if (length(remaining_futility_IFs) > 0) {
        stop("apply_GSD_to_trial: multiple sequential BPP futility looks are ",
             "not supported by this implementation (would require nested ",
             "posterior-predictive simulation at each look). See manuscript ",
             "Limitations (Section 6.3).")
      }

      # ---------------------------------------------------------------------
      # FIX: build the chain of future FIXED decision points (remaining
      # efficacy looks + final analysis) from the design object, so BPP_func
      # evaluates the true adaptive success event W (Eq. 8), instead of only
      # ever testing a single final analysis at a flat alpha.
      # ---------------------------------------------------------------------
      future_IFs <- sort(info_rates[info_rates > IF_here])
      future_boundaries <- lapply(future_IFs, function(x) {
        idx <- which(abs(design$informationRates - x) < 1e-8)
        list(
          events = ceiling(x * total_events),
          crit   = if (length(idx) == 1) design$criticalValues[idx] else NA
        )
      })

      posterior_samples <- DTEAssurance::update_priors(
        eligible_df,
        control_model = control_model,
        effect_model  = effect_model,
        n_samples     = update_priors_sims   # FIX: was hardcoded 100
      )

      # FIX: capture Z-monitoring / convergence diagnostics attached by the
      # patched update_priors()
      converged <- attr(posterior_samples, "converged")
      Z_probs_attr <- attr(posterior_samples, "Z_probs")
      if (!is.null(Z_probs_attr)) Z_probs <- Z_probs_attr

      BPP_out <- DTEAssurance::BPP_func(
        eligible_df, posterior_samples,
        control_distribution = control_model$dist,
        n_c_planned       = n_c,
        n_t_planned       = n_t,
        rec_time_planned  = recruitment_model$period,
        df_cens_time      = t_interim,
        analysis_model    = analysis_model,
        future_boundaries = future_boundaries,   # FIX: replaces censoring_model
        n_sims            = n_BPP_sims           # FIX: was hardcoded default 50
      )

      BPP_val <- mean(BPP_out$BPP_df$success)

      if (BPP_val < GSD_model$BPP_threshold) {
        decision  <- "Stop for futility"
        stop_time <- t_interim
        break
      }
    }
  } # end loop

  # -------------------------------
  # Final analysis
  # -------------------------------
  if (is.na(stop_time)) {
    eff_idx   <- length(design$criticalValues)
    eff_bound <- design$criticalValues[eff_idx]
    n_events  <- event_thresholds[length(info_rates)]
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)

    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time
    )

    fit  <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    z_stat_final <- -summary(fit)$coefficients[, "z"]

    decision  <- ifelse(z_stat_final > eff_bound, "Successful at final", "Unsuccessful at final")
    stop_time <- t_interim
  }

  sample_size <- sum(trial_data$rec_time <= stop_time)

  return(list(
    decision    = decision,
    stop_time   = stop_time,
    sample_size = sample_size,
    BPP_val     = BPP_val,
    converged   = converged,   # NEW
    Z_probs     = Z_probs      # NEW
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




single_calibration_rep <- function(i,
                                   n_c, n_t,
                                   control_model,
                                   effect_model,
                                   recruitment_model,
                                   total_events,
                                   IF,
                                   analysis_model,
                                   future_boundaries = NULL,
                                   update_priors_sims = 1000,
                                   PP_sims = 2000,
                                   seed = NULL) {

  if (!is.null(seed)) set.seed(seed * 10000 + i)

  if (!is.null(future_boundaries)) {
    candidate_events <- total_events * IF
    bad <- vapply(future_boundaries, function(fb) fb$events <= candidate_events, logical(1))
    if (any(bad)) {
      stop("single_calibration_rep: candidate IF = ", IF, " (", candidate_events,
           " events) is at or after one or more supplied future_boundaries. ",
           "Each future boundary must represent a genuinely FUTURE decision ",
           "point relative to the candidate timing being evaluated -- drop ",
           "this candidate IF from the sweep, or supply boundaries that ",
           "actually lie ahead of it.")
    }
  }

  data <- simulate_trial_with_recruitment(
    n_c = n_c,
    n_t = n_t,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model
  )

  censored_data <- cens_data(data, cens_method = "Events", cens_events = total_events * IF)
  data <- censored_data$data

  posterior_samples <- update_priors(data,
                                     control_model = control_model,
                                     effect_model  = effect_model,
                                     n_samples     = update_priors_sims)

  BPP_outcome <- BPP_func(
    data,
    posterior_samples,
    control_distribution = control_model$dist,
    n_c_planned          = n_c,
    n_t_planned          = n_t,
    rec_time_planned     = recruitment_model$period,
    df_cens_time         = censored_data$cens_time,
    analysis_model       = analysis_model,
    future_boundaries    = future_boundaries,
    censoring_model      = if (is.null(future_boundaries)) {
      list(method = "Events", events = total_events)
    } else {
      NULL
    },
    n_sims               = PP_sims
  )

  return(list(BPP_outcome = BPP_outcome, cens_time = censored_data$cens_time))
}



make_rpact_design_from_GSD_model <- function(GSD_model) {

  # 1. Extract alpha side
  alpha_IF       <- GSD_model$alpha_IF
  alpha_spending <- GSD_model$alpha_spending

  # 2. Futility type
  fut_type <- GSD_model$futility_type

  # 3. Combined IF grid
  if (fut_type %in% c("Beta", "BPP")) {
    # For Beta and BPP, include futility information fraction(s)
    fut_IF <- GSD_model$futility_IF
    IF_all <- sort(unique(c(alpha_IF, fut_IF)))
  } else if (fut_type == "none") {
    # No futility: only alpha IFs
    IF_all <- sort(unique(alpha_IF))
  } else {
    stop("Unknown futility type in GSD_model")
  }

  K <- length(IF_all)

  #==================================================
  # 4. Expand alpha spending to the full IF grid
  #==================================================
  alpha_spending_full <- numeric(K)
  idx <- 1L
  for (k in seq_len(K)) {
    if (idx <= length(alpha_IF) && IF_all[k] == alpha_IF[idx]) {
      alpha_spending_full[k] <- alpha_spending[idx]
      idx <- idx + 1L
    } else {
      alpha_spending_full[k] <- if (k == 1L) 0 else alpha_spending_full[k - 1L]
    }
  }

  #==================================================
  # 5. Expand beta spending (frequentist futility)
  #==================================================
  if (fut_type == "Beta") {

    beta_IF       <- GSD_model$futility_IF
    beta_spending <- GSD_model$beta_spending

    beta_spending_full <- numeric(K)
    idx <- 1L
    for (k in seq_len(K)) {
      if (idx <= length(beta_IF) && IF_all[k] == beta_IF[idx]) {
        beta_spending_full[k] <- beta_spending[idx]   # cumulative
        idx <- idx + 1L
      } else {
        beta_spending_full[k] <- if (k == 1L) 0 else beta_spending_full[k - 1L]
      }
    }

  } else {
    # For "none" and "BPP", no frequentist futility spending in rpact
    beta_spending_full <- rep(0, K)
  }

  #==================================================
  # 6. Build rpact design
  #==================================================
  if (fut_type == "Beta") {

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

single_grid_rep <- function(i,
                            n_c, n_t,
                            control_model,
                            effect_model,
                            recruitment_model,
                            data_generating_model,
                            futility_IF,
                            total_events,
                            future_boundaries,
                            analysis_model,
                            update_priors_sims = 1000,
                            PP_sims = 2000,
                            seed = NULL) {

  if (!is.null(seed)) set.seed(seed * 10000 + i)

  # --- Simulate the true underlying trial ---
  if (is.null(data_generating_model$gamma_c)) {
    trial_data <- sim_dte(n_c, n_t,
                          data_generating_model$lambda_c,
                          delay_time = data_generating_model$delay_time,
                          post_delay_HR = data_generating_model$post_delay_HR,
                          dist = "Exponential")
  } else {
    trial_data <- sim_dte(n_c, n_t,
                          data_generating_model$lambda_c,
                          delay_time = data_generating_model$delay_time,
                          post_delay_HR = data_generating_model$post_delay_HR,
                          dist = "Weibull",
                          gamma_c = data_generating_model$gamma_c)
  }

  trial_data <- add_recruitment_time(trial_data,
                                     rec_method   = recruitment_model$method,
                                     rec_period   = recruitment_model$period,
                                     rec_power    = recruitment_model$power,
                                     rec_rate     = recruitment_model$rate,
                                     rec_duration = recruitment_model$duration)

  trial_data <- trial_data[order(trial_data$pseudo_time), ]

  # --- Interim look: compute BPP (independent of any lambda) ---
  n_events_interim <- ceiling(futility_IF * total_events)
  t_interim <- trial_data$pseudo_time[n_events_interim]

  eligible_df <- trial_data %>% dplyr::filter(.data$rec_time <= t_interim)
  eligible_df$status <- eligible_df$pseudo_time < t_interim
  eligible_df$survival_time <- ifelse(eligible_df$status, eligible_df$time,
                                      t_interim - eligible_df$rec_time)

  sample_size_interim <- sum(trial_data$rec_time <= t_interim)

  posterior_samples <- update_priors(eligible_df,
                                     control_model = control_model,
                                     effect_model  = effect_model,
                                     n_samples     = update_priors_sims)

  BPP_out <- BPP_func(
    eligible_df, posterior_samples,
    control_distribution = control_model$dist,
    n_c_planned       = n_c,
    n_t_planned       = n_t,
    rec_time_planned  = recruitment_model$period,
    df_cens_time      = t_interim,
    analysis_model    = analysis_model,
    future_boundaries = future_boundaries,
    n_sims            = PP_sims
  )
  BPP_val <- mean(BPP_out$BPP_df$success)

  # --- True continuation: what ACTUALLY happens to this real trial if it
  #     is never stopped for futility here. Applies the same
  #     future_boundaries chain directly to the real simulated data (not a
  #     posterior-predictive draw). ---
  continuation_success <- 0
  continuation_stop_time <- NA_real_
  continuation_sample_size <- NA_integer_

  for (k in seq_along(future_boundaries)) {
    fb <- future_boundaries[[k]]
    is_last <- (k == length(future_boundaries))

    censored_k <- cens_data(trial_data, cens_method = "Events", cens_events = fb$events)
    test_k <- survival_test(censored_k$data,
                            analysis_method = analysis_model$method,
                            alpha = analysis_model$alpha,
                            alternative = analysis_model$alternative_hypothesis,
                            rho = analysis_model$rho,
                            gamma = analysis_model$gamma,
                            t_star = analysis_model$t_star,
                            s_star = analysis_model$s_star)

    if (!is.na(test_k$Z) && test_k$Z > fb$crit) {
      continuation_success <- 1
      continuation_stop_time <- censored_k$cens_time
      continuation_sample_size <- censored_k$sample_size
      break
    }
    if (is_last) {
      continuation_success <- 0
      continuation_stop_time <- censored_k$cens_time
      continuation_sample_size <- censored_k$sample_size
      break
    }
  }

  data.frame(
    BPP_val = BPP_val,
    t_interim = t_interim,
    sample_size_interim = sample_size_interim,
    continuation_success = continuation_success,
    continuation_stop_time = continuation_stop_time,
    continuation_sample_size = continuation_sample_size
  )
}


run_calibration_grid <- function(n_c, n_t,
                                 control_model,
                                 effect_model,
                                 recruitment_model,
                                 data_generating_model,
                                 futility_IF,
                                 total_events,
                                 future_boundaries,
                                 analysis_model,
                                 update_priors_sims = 1000,
                                 PP_sims = 2000,
                                 n_sims = 1000,
                                 n_cores = 1,
                                 seed = NULL) {

  if (n_cores > 1) {
    result <- parallel::mclapply(
      seq_len(n_sims), single_grid_rep,
      n_c = n_c, n_t = n_t,
      control_model = control_model,
      effect_model = effect_model,
      recruitment_model = recruitment_model,
      data_generating_model = data_generating_model,
      futility_IF = futility_IF,
      total_events = total_events,
      future_boundaries = future_boundaries,
      analysis_model = analysis_model,
      update_priors_sims = update_priors_sims,
      PP_sims = PP_sims,
      seed = seed,
      mc.cores = n_cores
    )
  } else {
    result <- lapply(
      seq_len(n_sims), single_grid_rep,
      n_c = n_c, n_t = n_t,
      control_model = control_model,
      effect_model = effect_model,
      recruitment_model = recruitment_model,
      data_generating_model = data_generating_model,
      futility_IF = futility_IF,
      total_events = total_events,
      future_boundaries = future_boundaries,
      analysis_model = analysis_model,
      update_priors_sims = update_priors_sims,
      PP_sims = PP_sims,
      seed = seed
    )
  }

  raw <- do.call(rbind, result)

  settings <- list(
    data_generating_model = data_generating_model,
    futility_IF = futility_IF,
    total_events = total_events,
    future_boundaries = future_boundaries,
    update_priors_sims = update_priors_sims,
    PP_sims = PP_sims,
    n_sims = n_sims,
    n_cores = n_cores,
    seed = seed,
    package_version = tryCatch(as.character(utils::packageVersion("DTEAssurance")),
                               error = function(e) NA_character_),
    timestamp = as.character(Sys.time())
  )

  list(raw = raw, settings = settings)
}


summarize_grid_by_lambda <- function(raw, lambda_grid, conf_level = 0.90) {

  n <- nrow(raw)

  out <- lapply(lambda_grid, function(lam) {
    stop_for_fut <- raw$BPP_val < lam

    success  <- ifelse(stop_for_fut, 0, raw$continuation_success)
    sample_n <- ifelse(stop_for_fut, raw$sample_size_interim, raw$continuation_sample_size)
    duration <- ifelse(stop_for_fut, raw$t_interim, raw$continuation_stop_time)

    n_success <- sum(success)
    phat <- n_success / n
    lcb <- stats::binom.test(n_success, n, conf.level = conf_level)$conf.int[1]

    data.frame(
      lambda = lam,
      P_early_fut = mean(stop_for_fut),
      power_or_typeI = phat,
      power_LCB = lcb,
      ESS = mean(sample_n),
      duration = mean(duration)
    )
  })

  do.call(rbind, out)
}


select_lambda_star <- function(summary_by_scenario, power_floor, null_scenario, alt_scenarios) {

  lambda_grid <- summary_by_scenario[[null_scenario]]$lambda

  feasible <- rep(TRUE, length(lambda_grid))
  for (s in alt_scenarios) {
    stopifnot(identical(summary_by_scenario[[s]]$lambda, lambda_grid))
    feasible <- feasible & (summary_by_scenario[[s]]$power_LCB >= power_floor)
  }

  null_ESS <- summary_by_scenario[[null_scenario]]$ESS

  feasibility_table <- data.frame(
    lambda = lambda_grid,
    null_ESS = null_ESS,
    feasible = feasible
  )
  for (s in alt_scenarios) {
    feasibility_table[[paste0("power_LCB_", s)]] <- summary_by_scenario[[s]]$power_LCB
  }

  if (!any(feasible)) {
    warning("select_lambda_star: no candidate lambda satisfies power_LCB >= ",
            power_floor, " under all of: ", paste(alt_scenarios, collapse = ", "),
            ". Returning lambda_star = NA; widen the lambda grid, lower the ",
            "power floor, or increase n_sims (the LCB may be overly ",
            "conservative with too few replicates).")
    return(list(lambda_star = NA_real_, feasibility_table = feasibility_table))
  }

  lambda_star <- lambda_grid[feasible][which.min(null_ESS[feasible])]

  list(lambda_star = lambda_star, feasibility_table = feasibility_table)
}


summarize_convergence <- function(posterior_list) {

  converged_vec <- vapply(posterior_list, function(x) {
    cv <- attr(x, "converged")
    if (is.null(cv)) NA else cv
  }, logical(1))

  rhat_mat <- do.call(rbind, lapply(posterior_list, function(x) {
    rh <- attr(x, "rhat")
    if (is.null(rh)) return(NULL)
    rh
  }))

  max_rhat_by_param <- if (!is.null(rhat_mat) && nrow(rhat_mat) > 0) {
    apply(rhat_mat, 2, max, na.rm = TRUE)
  } else {
    NULL
  }

  list(
    n_total = length(converged_vec),
    n_converged = sum(converged_vec, na.rm = TRUE),
    n_failed = sum(is.na(converged_vec)),
    prop_converged = mean(converged_vec, na.rm = TRUE),
    max_rhat_by_param = max_rhat_by_param
  )
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




