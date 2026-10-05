

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



# Simulate one trial (survival times plus recruitment) with control and
# treatment-effect parameters drawn from the priors.
#
# force_state: NULL (default) draws the latent state from P_S / P_DTE as
# before. Otherwise 1 = no separation (T = 0, HR = 1), 2 = immediate
# separation (T = 0, HR ~ HR_SHELF) or 3 = delayed separation
# (T ~ delay_SHELF, HR ~ HR_SHELF), and the state draw is skipped.
#
# The returned data carries attr(, "truth"): list(lambda_c, gamma_c,
# delay_time, post_delay_HR, state), with gamma_c = 1 for an exponential
# control arm.
simulate_trial_with_recruitment <- function(n_c, n_t,
                                control_model,
                                effect_model,
                                recruitment_model,
                                force_state = NULL) {

  if (!is.null(force_state) && !force_state %in% 1:3) {
    stop("force_state must be NULL, 1, 2 or 3.")
  }

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

  state <- if (!is.null(force_state)) {
    force_state
  } else if (stats::runif(1) > effect_model$P_S) {
    1L
  } else if (stats::runif(1) > effect_model$P_DTE) {
    2L
  } else {
    3L
  }

  if (state == 1) {
    delay_time <- 0
    post_delay_HR <- 1
  } else if (state == 2) {
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

  attr(data, "truth") <- list(lambda_c = lambda_c_i,
                              gamma_c = if (is.null(gamma_c_i)) 1 else gamma_c_i,
                              delay_time = delay_time,
                              post_delay_HR = post_delay_HR,
                              state = as.integer(state))

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
                               update_priors_sims = NULL,
                               PP_sims = NULL) {

  if (is.null(analysis_model)) {
    analysis_model <- list(method = "LRT", alpha = 0.025,
                           alternative_hypothesis = "one.sided")
  }

  if (identical(GSD_model$futility_type, "PP")) {
    if (is.null(update_priors_sims) || is.null(PP_sims)) {
      stop("apply_GSD_to_trial: 'update_priors_sims' and 'PP_sims' must be ",
           "supplied when GSD_model$futility_type is \"PP\".", call. = FALSE)
    }
    check_uniform_recruitment(recruitment_model, "apply_GSD_to_trial")
  }

  compute_Z <- function(eligible_df) {
    survival_test(eligible_df,
                  analysis_method = analysis_model$method,
                  alpha           = analysis_model$alpha,
                  alternative     = analysis_model$alternative_hypothesis,
                  rho             = analysis_model$rho,
                  gamma           = analysis_model$gamma,
                  t_star          = analysis_model$t_star,
                  s_star          = analysis_model$s_star,
                  return_HR       = FALSE)$Z
  }

  if (GSD_model$futility_type %in% c("Beta", "none")) {
    info_rates <- round(design$informationRates, 6)
  } else if (GSD_model$futility_type %in% c("PP", "MatchedZ")) {
    info_rates <- sort(unique(round(c(GSD_model$alpha_IF, GSD_model$futility_IF), 6)))
  }
  futility_IFs <- if (is.null(GSD_model$futility_IF)) numeric(0) else round(GSD_model$futility_IF, 6)

  n_interims        <- length(info_rates)
  decision          <- "Continue"
  stop_time         <- NA
  sample_size       <- NA
  PP_val            <- NA
  converged         <- NA
  Z_probs           <- rep(NA_real_, 3)
  names(Z_probs)    <- c("P_Z1", "P_Z2", "P_Z3")

  for (i in seq_len(n_interims - 1)) {
    IF_here   <- info_rates[i]
    cut       <- cens_data(trial_data, cens_method = "Events",
                           cens_events = n_events_at(total_events, IF_here))
    t_interim   <- cut$cens_time
    eligible_df <- cut$data

    z_stat_here <- compute_Z(eligible_df)

    eff_bound <- crit_at(design, IF_here)

    # 1) Efficacy check
    if (!is.na(eff_bound) && !is.na(z_stat_here) && z_stat_here > eff_bound) {
      decision    <- "Stop for efficacy"
      stop_time   <- t_interim
      sample_size <- cut$sample_size
      break
    }

    # 2) Beta-spending futility
    if (!is.null(GSD_model) && GSD_model$futility_type == "Beta") {
      fut_idx <- which(round(design$informationRates, 6) == IF_here)
      fut_bound <- if (length(fut_idx) == 1 && fut_idx <= length(design$futilityBounds)) {
        design$futilityBounds[fut_idx]
      } else NA

      if (!is.na(fut_bound) && !is.na(z_stat_here) && z_stat_here < fut_bound) {
        decision    <- "Stop for futility"
        stop_time   <- t_interim
        sample_size <- cut$sample_size
        break
      }
    }

    # 3a) MatchedZ futility (D4/D5)
    if (!is.null(GSD_model) &&
        GSD_model$futility_type == "MatchedZ" &&
        IF_here %in% futility_IFs) {

      if (!is.na(z_stat_here) && z_stat_here < GSD_model$futility_boundary_Z) {
        decision    <- "Stop for futility"
        stop_time   <- t_interim
        sample_size <- cut$sample_size
        break
      }
    }

    # 3b) PP futility (D3)
    if (!is.null(GSD_model) &&
        GSD_model$futility_type == "PP" &&
        IF_here %in% futility_IFs) {

      remaining_futility_IFs <- futility_IFs[futility_IFs > IF_here]
      if (length(remaining_futility_IFs) > 0) {
        stop("apply_GSD_to_trial: multiple sequential PP futility looks are ",
             "not supported by this implementation (would require nested ",
             "posterior-predictive simulation at each look). See manuscript ",
             "Limitations (Section 6.3).")
      }

      future_boundaries <- make_future_boundaries(design, total_events, IF_here)

      posterior_samples <- DTEAssurance::update_priors(
        eligible_df,
        control_model = control_model,
        effect_model  = effect_model,
        n_samples     = update_priors_sims
      )

      converged <- attr(posterior_samples, "converged")
      Z_probs_attr <- attr(posterior_samples, "Z_probs")
      if (!is.null(Z_probs_attr)) Z_probs <- Z_probs_attr

      PP_out <- DTEAssurance::PP_func(
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

      PP_val <- mean(PP_out$PP_df$success)

      if (PP_val < GSD_model$kappa) {
        decision    <- "Stop for futility"
        stop_time   <- t_interim
        sample_size <- cut$sample_size
        break
      }
    }
  } # end loop

  # Final analysis
  if (is.na(stop_time)) {
    eff_bound <- design$criticalValues[length(design$criticalValues)]
    cut <- cens_data(trial_data, cens_method = "Events",
                     cens_events = n_events_at(total_events, info_rates[n_interims]))

    z_stat_final <- compute_Z(cut$data)

    decision    <- ifelse(!is.na(z_stat_final) && z_stat_final > eff_bound,
                          "Successful at final", "Unsuccessful at final")
    stop_time   <- cut$cens_time
    sample_size <- cut$sample_size
  }

  return(list(
    decision    = decision,
    stop_time   = stop_time,
    sample_size = sample_size,
    PP_val      = PP_val,
    converged   = converged,
    Z_probs     = Z_probs
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
    candidate_events <- n_events_at(total_events, IF)
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

  censored_data <- cens_data(data, cens_method = "Events",
                             cens_events = n_events_at(total_events, IF))
  data <- censored_data$data

  posterior_samples <- update_priors(data,
                                     control_model = control_model,
                                     effect_model  = effect_model,
                                     n_samples     = update_priors_sims)

  PP_outcome <- PP_func(
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

  return(list(PP_outcome = PP_outcome, cens_time = censored_data$cens_time))
}



# Map the deprecated futility_type = "BPP" / GSD_model$BPP_threshold
# spellings onto "PP" / GSD_model$kappa, with a warning. To be removed in
# the next release.
normalise_futility_spec <- function(GSD_model) {
  if (identical(GSD_model$futility_type, "BPP")) {
    warning("'BPP' is deprecated; use 'PP'", call. = FALSE)
    GSD_model$futility_type <- "PP"
  }
  if (!is.null(GSD_model$BPP_threshold)) {
    warning("GSD_model$BPP_threshold is deprecated; use GSD_model$kappa",
            call. = FALSE)
    if (is.null(GSD_model$kappa)) GSD_model$kappa <- GSD_model$BPP_threshold
    GSD_model$BPP_threshold <- NULL
  }
  GSD_model
}


# Build the rpact group-sequential design for a GSD_model.
#
# GSD_model$alpha_spending is the user-specified CUMULATIVE alpha spent at
# each efficacy look GSD_model$alpha_IF (rpact typeOfDesign = "asUser").
# Futility looks (futility_IF) are added to the information-rate grid with
# zero additional alpha spent, so they leave the efficacy critical values
# unchanged. Information fractions are rounded to 6 dp before comparison.
make_rpact_design_from_GSD_model <- function(GSD_model) {

  GSD_model      <- normalise_futility_spec(GSD_model)
  # Information fractions are rounded to 6 dp before they are compared.
  alpha_IF       <- round(GSD_model$alpha_IF, 6)
  alpha_spending <- GSD_model$alpha_spending
  fut_type       <- GSD_model$futility_type

  # FIX: "MatchedZ" (D4/D5) needs futility_IF included in the information-
  # rate grid, with zero alpha spent there, exactly like "PP" (D3) already
  # does -- this is what lets the futility look sit as a pure monitoring
  # point without affecting the efficacy boundaries (confirmed empirically
  # earlier: design objects built with vs. without this extra point give
  # identical criticalValues). Previously only "Beta" and "PP" were
  # recognized here; "MatchedZ" fell through to the "Unknown futility type"
  # error below.
  if (fut_type %in% c("Beta", "PP", "MatchedZ")) {
    fut_IF <- round(GSD_model$futility_IF, 6)
    IF_all <- sort(unique(c(alpha_IF, fut_IF)))
  } else if (fut_type == "none") {
    IF_all <- sort(unique(alpha_IF))
  } else {
    stop("Unknown futility type in GSD_model")
  }

  K <- length(IF_all)

  # ... rest of the function is UNCHANGED -- the later beta_spending_full
  # and design-building logic already fall through correctly to the
  # generic "no beta spending, typeBetaSpending='none'" path for any
  # fut_type other than "Beta", so MatchedZ needs no further changes there.

  # [unchanged: steps 4-6 exactly as before]


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

    beta_IF       <- round(GSD_model$futility_IF, 6)
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
    # For "none" and "PP", no frequentist futility spending in rpact
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

  } else {  # fut_type == "none", "PP" or "MatchedZ"

    design <- rpact::getDesignGroupSequential(
      typeOfDesign      = "asUser",
      informationRates  = IF_all,
      userAlphaSpending = alpha_spending_full,
      typeBetaSpending  = "none"
    )

    # A futility look with zero alpha spent cannot change the efficacy
    # critical values, but rpact's numerical integration can move them
    # slightly when the futility look is close to an efficacy look (about
    # 8e-5 for looks at 0.7 and 0.75). Use the efficacy-only design's values
    # so that adding the look leaves them exactly unchanged.
    if (K > length(alpha_IF)) {
      crit_eff <- if (length(alpha_IF) == 1) {
        stats::qnorm(1 - alpha_spending)
      } else {
        rpact::getDesignGroupSequential(
          typeOfDesign      = "asUser",
          informationRates  = alpha_IF,
          userAlphaSpending = alpha_spending,
          typeBetaSpending  = "none"
        )$criticalValues
      }
      crit <- design$criticalValues
      crit[match(alpha_IF, IF_all)] <- crit_eff
      design$criticalValues <- crit
    }
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

  check_uniform_recruitment(recruitment_model, "single_grid_rep")

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

  # --- Interim look: compute PP (independent of any kappa) ---
  cut <- cens_data(trial_data, cens_method = "Events",
                   cens_events = n_events_at(total_events, futility_IF))
  t_interim <- cut$cens_time
  eligible_df <- cut$data
  sample_size_interim <- cut$sample_size

  posterior_samples <- update_priors(eligible_df,
                                     control_model = control_model,
                                     effect_model  = effect_model,
                                     n_samples     = update_priors_sims)

  # FIX: capture the convergence / latent-state diagnostics update_priors()
  # now computes, instead of discarding posterior_samples' attributes.
  converged <- attr(posterior_samples, "converged")
  Zp <- attr(posterior_samples, "Z_probs")
  if (is.null(Zp)) Zp <- c(P_Z1 = NA_real_, P_Z2 = NA_real_, P_Z3 = NA_real_)

  PP_out <- PP_func(
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
  PP_val <- mean(PP_out$PP_df$success)

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
                            s_star = analysis_model$s_star,
                            return_HR = FALSE)

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
    PP_val = PP_val,
    t_interim = t_interim,
    sample_size_interim = sample_size_interim,
    continuation_success = continuation_success,
    continuation_stop_time = continuation_stop_time,
    continuation_sample_size = continuation_sample_size,
    converged = converged,          # NEW
    P_Z1 = unname(Zp["P_Z1"]),      # NEW
    P_Z2 = unname(Zp["P_Z2"]),      # NEW
    P_Z3 = unname(Zp["P_Z3"])       # NEW
  )
}


#' Simulate PP values and true trial outcomes for PP-threshold calibration
#'
#' For a single fixed data-generating scenario, simulates \code{n_sims}
#' trials, computes the predictive probability (PP) at the
#' futility look, and records what actually happens to each trial if it is
#' allowed to continue through the remaining fixed decision points in
#' \code{future_boundaries}. Because the PP value does not depend on the
#' futility threshold \eqn{\kappa}, the output can be summarised over a
#' whole grid of thresholds afterwards via \code{\link{summarize_grid_by_kappa}}
#' without re-simulating.
#'
#' @param n_c,n_t Planned number of patients in the control / treatment group.
#' @param control_model,effect_model Prior specification used for the
#'   interim posterior update (see \code{\link{update_priors}}).
#'   \code{control_model$parameter_mode} must be \code{"Distribution"}.
#' @param recruitment_model Recruitment specification (see
#'   \code{\link{add_recruitment_time}}): \code{method}, \code{period},
#'   \code{power}, \code{rate}, \code{duration}.
#' @param data_generating_model A named list giving the true scenario used to
#'   simulate data: \code{lambda_c}, \code{delay_time}, \code{post_delay_HR},
#'   and optionally \code{gamma_c} (Weibull control arm if supplied,
#'   exponential otherwise).
#' @param futility_IF Information fraction of the PP futility look.
#' @param total_events Maximum planned number of events.
#' @param future_boundaries List of future fixed decision points, each a list
#'   with \code{events} and \code{crit} (see \code{\link{PP_func}}).
#' @param analysis_model Analysis specification (see \code{\link{survival_test}}).
#' @param update_priors_sims Number of posterior samples per interim dataset.
#' @param PP_sims Number of posterior-predictive simulations per interim dataset.
#' @param n_sims Number of simulated trials.
#' @param n_cores Number of cores (uses \code{parallel::mclapply} when > 1).
#' @param seed Optional integer seed; replicate \code{i} uses
#'   \code{seed * 10000 + i}.
#'
#' @return A list with elements
#'   \describe{
#'     \item{\code{raw}}{A data frame with one row per simulated trial and
#'       columns \code{PP_val}, \code{t_interim}, \code{sample_size_interim},
#'       \code{continuation_success}, \code{continuation_stop_time},
#'       \code{continuation_sample_size}, \code{converged}, \code{P_Z1},
#'       \code{P_Z2}, \code{P_Z3}.}
#'     \item{\code{settings}}{The settings used, package version and a timestamp.}
#'   }
#'
#' @seealso \code{\link{summarize_grid_by_kappa}}, \code{\link{select_kappa_star}}
#' @export
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


#' Summarise calibration output over a grid of PP futility thresholds
#'
#' Applies each candidate PP futility threshold \eqn{\kappa} to raw
#' replicate output (from \code{\link{run_paired_scenario}} or
#' \code{\link{run_calibration_grid}}): a trial stops for futility if its
#' interim PP is below \eqn{\kappa}, otherwise it takes its continuation
#' (D2) outcome. At \code{kappa = 0} no trial stops, so the result is the D2
#' operating characteristics.
#'
#' @param raw A data frame with columns \code{PP_val},
#'   \code{continuation_success}, \code{continuation_sample_size},
#'   \code{continuation_stop_time}, \code{sample_size_interim},
#'   \code{t_interim} and (if \code{exclude_nonconverged = TRUE})
#'   \code{converged}.
#' @param kappa_grid Numeric vector of candidate PP thresholds.
#' @param lcb_level Confidence level of the one-sided exact
#'   (Clopper-Pearson) lower bound on power (default 0.95).
#' @param exclude_nonconverged If \code{TRUE}, drop replicates whose interim
#'   MCMC did not converge (\code{converged} \code{FALSE} or \code{NA}).
#'   Default \code{FALSE}.
#'
#' @return A data frame with one row per \eqn{\kappa} and columns
#'   \code{kappa}, \code{n} (replicates used), \code{n_NA_dropped}
#'   (replicates dropped because \code{PP_val} was \code{NA}, with a
#'   warning), \code{P_early_fut}, \code{power_or_typeI}, \code{power_SE},
#'   \code{power_LCB} (one-sided exact lower bound at level
#'   \code{lcb_level}, 95\% by default), \code{ESS} (expected sample
#'   size), \code{ESS_SE} and \code{duration} (expected trial duration).
#'
#' @seealso \code{\link{select_kappa_star}}, \code{\link{compute_relative_floors}}
#' @export
summarize_grid_by_kappa <- function(raw, kappa_grid, lcb_level = 0.95,
                                    exclude_nonconverged = FALSE) {
  if (exclude_nonconverged) raw <- raw[!is.na(raw$converged) & raw$converged, ]
  n_na <- sum(is.na(raw$PP_val))
  if (n_na > 0) {
    warning(n_na, " replicates have NA PP_val; dropping them. Investigate.")
    raw <- raw[!is.na(raw$PP_val), ]
  }
  n <- nrow(raw)
  out <- lapply(kappa_grid, function(kappa) {
    stop_for_fut <- raw$PP_val < kappa
    success  <- ifelse(stop_for_fut, 0, raw$continuation_success)
    sample_n <- ifelse(stop_for_fut, raw$sample_size_interim, raw$continuation_sample_size)
    duration <- ifelse(stop_for_fut, raw$t_interim, raw$continuation_stop_time)
    n_success <- sum(success)
    phat <- n_success / n
    bt <- stats::binom.test(n_success, n, alternative = "greater", conf.level = lcb_level)
    data.frame(kappa = kappa, n = n, n_NA_dropped = n_na,
               P_early_fut = mean(stop_for_fut),
               power_or_typeI = phat, power_SE = sqrt(phat * (1 - phat) / n),
               power_LCB = bt$conf.int[1],          # one-sided exact (Clopper-Pearson) lower bound
               ESS = mean(sample_n), ESS_SE = stats::sd(sample_n) / sqrt(n),
               duration = mean(duration))
  })
  do.call(rbind, out)
}


#' Relative power floors for kappa selection
#'
#' For each alternative scenario, the D2 power (the power with no futility
#' stopping, i.e. at \code{kappa = 0}) minus \code{margin}. Pass the result
#' as \code{power_floor} to \code{\link{select_kappa_star}}.
#'
#' @param raw_by_scenario A named list of raw replicate data frames (each
#'   with a \code{continuation_success} column).
#' @param alt_scenarios Character vector of alternative scenario names.
#' @param margin Allowed loss of power relative to D2 (default 0.05).
#'
#' @return A named numeric vector with one floor per alternative scenario.
#'
#' @seealso \code{\link{select_kappa_star}}
#' @export
compute_relative_floors <- function(raw_by_scenario, alt_scenarios, margin = 0.05) {
  vapply(alt_scenarios, function(s) mean(raw_by_scenario[[s]]$continuation_success) - margin, numeric(1))
}


#' Select the PP futility threshold minimising null expected sample size
#'
#' Among candidate thresholds for which the one-sided 95\% exact lower
#' confidence bound on power (\code{power_LCB} from
#' \code{\link{summarize_grid_by_kappa}}) is at least the floor under every
#' alternative scenario, selects the one with the smallest expected sample
#' size under the null scenario. A threshold with an \code{NA} lower bound in
#' any alternative scenario is infeasible. Ties in null expected sample size
#' (within 1e-8) are broken in favour of the \emph{larger} kappa.
#'
#' @param summary_by_scenario A named list of data frames, one per scenario,
#'   each as returned by \code{\link{summarize_grid_by_kappa}} over the same
#'   \code{kappa_grid}.
#' @param power_floor Minimum acceptable lower confidence bound on power:
#'   a scalar applied to every alternative scenario, or a named numeric
#'   vector with one entry per alternative scenario (e.g. from
#'   \code{\link{compute_relative_floors}}).
#' @param null_scenario Name of the null scenario in \code{summary_by_scenario}.
#' @param alt_scenarios Character vector of alternative scenario names.
#'
#' @return A list with \code{kappa_star} (the selected threshold, or
#'   \code{NA} with a warning if none is feasible) and
#'   \code{feasibility_table} (\code{kappa}, \code{null_ESS},
#'   \code{feasible} and, per alternative scenario, \code{power_<s>},
#'   \code{power_LCB_<s>} and \code{floor_<s>}).
#'
#' @seealso \code{\link{summarize_grid_by_kappa}}, \code{\link{compute_relative_floors}}
#' @export
select_kappa_star <- function(summary_by_scenario, power_floor, null_scenario, alt_scenarios) {
  stopifnot(null_scenario %in% names(summary_by_scenario),
            all(alt_scenarios %in% names(summary_by_scenario)))
  kappa_grid <- summary_by_scenario[[null_scenario]]$kappa
  null_ESS   <- summary_by_scenario[[null_scenario]]$ESS
  floors <- if (length(power_floor) == 1) stats::setNames(rep(power_floor, length(alt_scenarios)), alt_scenarios) else power_floor
  stopifnot(all(alt_scenarios %in% names(floors)))
  feasible <- rep(TRUE, length(kappa_grid))
  for (s in alt_scenarios) {
    stopifnot(isTRUE(all.equal(summary_by_scenario[[s]]$kappa, kappa_grid)))
    ok <- summary_by_scenario[[s]]$power_LCB >= floors[[s]]
    feasible <- feasible & !is.na(ok) & ok                      # NA => infeasible
  }
  tab <- data.frame(kappa = kappa_grid, null_ESS = null_ESS, feasible = feasible)
  for (s in alt_scenarios) {
    sm <- summary_by_scenario[[s]]
    tab[[paste0("power_", s)]]     <- sm$power_or_typeI
    tab[[paste0("power_LCB_", s)]] <- sm$power_LCB
    tab[[paste0("floor_", s)]]     <- floors[[s]]
  }
  if (!any(feasible)) {
    warning("select_kappa_star: no feasible kappa. Returning NA.")
    return(list(kappa_star = NA_real_, feasibility_table = tab))
  }
  k_feas <- kappa_grid[feasible]; ess_feas <- null_ESS[feasible]
  tied <- ess_feas <= min(ess_feas) + 1e-8
  list(kappa_star = max(k_feas[tied]),                          # ties -> LARGER kappa
       feasibility_table = tab)
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

#' Simulate one interim dataset and compute Z at the futility look
#'
#' Single replicate used by \code{\link{calibrate_matched_futility_boundary}}:
#' simulates one trial under a fixed data-generating scenario, censors it at
#' \code{floor(futility_IF * total_events)} events, and returns the test
#' statistic from \code{\link{survival_test}} (positive Z favours treatment).
#'
#' @param i Replicate index (used with \code{seed}).
#' @param n_c,n_t Number of patients in the control / treatment group.
#' @param data_generating_model True scenario: \code{lambda_c},
#'   \code{delay_time}, \code{post_delay_HR}, optionally \code{gamma_c}.
#' @param recruitment_model Recruitment specification (see
#'   \code{\link{add_recruitment_time}}).
#' @param futility_IF Information fraction of the futility look.
#' @param total_events Maximum planned number of events.
#' @param analysis_model Analysis specification (see \code{\link{survival_test}}).
#' @param seed Optional integer seed; replicate \code{i} uses
#'   \code{seed * 10000 + i}.
#'
#' @return A one-row data frame with column \code{Z}.
#'
#' @keywords internal
#' @export
single_matched_futility_rep <- function(i, n_c, n_t, data_generating_model,
                                        recruitment_model, futility_IF, total_events,
                                        analysis_model, seed = NULL) {

  if (!is.null(seed)) set.seed(seed * 10000 + i)

  if (is.null(data_generating_model$gamma_c)) {
    trial_data <- sim_dte(n_c, n_t, data_generating_model$lambda_c,
                          delay_time = data_generating_model$delay_time,
                          post_delay_HR = data_generating_model$post_delay_HR,
                          dist = "Exponential")
  } else {
    trial_data <- sim_dte(n_c, n_t, data_generating_model$lambda_c,
                          delay_time = data_generating_model$delay_time,
                          post_delay_HR = data_generating_model$post_delay_HR,
                          dist = "Weibull", gamma_c = data_generating_model$gamma_c)
  }

  trial_data <- add_recruitment_time(trial_data,
                                     rec_method   = recruitment_model$method,
                                     rec_period   = recruitment_model$period,
                                     rec_power    = recruitment_model$power,
                                     rec_rate     = recruitment_model$rate,
                                     rec_duration = recruitment_model$duration)

  censored <- cens_data(trial_data, cens_method = "Events",
                        cens_events = n_events_at(total_events, futility_IF))

  Z <- survival_test(censored$data,
                     analysis_method = analysis_model$method,
                     alpha           = analysis_model$alpha,
                     alternative     = analysis_model$alternative_hypothesis,
                     rho             = analysis_model$rho,
                     gamma           = analysis_model$gamma,
                     t_star          = analysis_model$t_star,
                     s_star          = analysis_model$s_star,
                     return_HR       = FALSE)$Z

  data.frame(Z = Z)
}


#' Calibrate a fixed Z-statistic futility boundary
#'
#' Finds the Z-statistic futility boundary at \code{futility_IF} such that
#' the probability of stopping for futility under the \code{"null"}
#' scenario equals \code{target_null_futility_rate} (the corresponding
#' empirical quantile of the simulated null Z distribution), and reports
#' the resulting futility-stopping rate under every supplied scenario. The
#' result is used as \code{GSD_model$futility_boundary_Z} with
#' \code{GSD_model$futility_type = "MatchedZ"} in
#' \code{\link{calc_dte_assurance_adaptive}}.
#'
#' @param n_c,n_t Number of patients in the control / treatment group.
#' @param recruitment_model Recruitment specification (see
#'   \code{\link{add_recruitment_time}}).
#' @param futility_IF Information fraction of the futility look.
#' @param total_events Maximum planned number of events.
#' @param analysis_model Analysis specification (see \code{\link{survival_test}}).
#' @param target_null_futility_rate Target probability of stopping for
#'   futility under the null scenario.
#' @param scenarios Named list of data-generating scenarios (see
#'   \code{\link{single_matched_futility_rep}}); must include one named
#'   \code{"null"}.
#' @param n_sims Number of simulated trials per scenario (default 2000).
#' @param n_cores Number of cores (uses \code{parallel::mclapply} when > 1).
#' @param seed Optional integer seed.
#'
#' @return A list with \code{boundary}, \code{scenario_futility_rates},
#'   \code{raw_Z_by_scenario} and \code{settings}.
#'
#' @examples
#' scenarios <- list(
#'   null = list(lambda_c = log(2) / 12, delay_time = 0, post_delay_HR = 1),
#'   alt  = list(lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.6)
#' )
#' cal <- calibrate_matched_futility_boundary(
#'   n_c = 100, n_t = 100,
#'   recruitment_model = list(method = "power", period = 12, power = 1),
#'   futility_IF = 0.5, total_events = 120,
#'   analysis_model = list(method = "LRT", alpha = 0.025,
#'                         alternative_hypothesis = "one.sided"),
#'   target_null_futility_rate = 0.5,
#'   scenarios = scenarios, n_sims = 20, seed = 1)
#' cal$boundary
#' cal$scenario_futility_rates
#'
#' @export
calibrate_matched_futility_boundary <- function(n_c, n_t,
                                                recruitment_model,
                                                futility_IF, total_events,
                                                analysis_model,
                                                target_null_futility_rate,
                                                scenarios,
                                                n_sims = 2000,
                                                n_cores = 1,
                                                seed = NULL) {

  if (!"null" %in% names(scenarios)) {
    stop("calibrate_matched_futility_boundary: 'scenarios' must include an ",
         "entry named 'null', used to calibrate the boundary.")
  }

  raw_Z_by_scenario <- list()

  for (scen_name in names(scenarios)) {
    run_one <- function(i) {
      single_matched_futility_rep(
        i, n_c = n_c, n_t = n_t,
        data_generating_model = scenarios[[scen_name]],
        recruitment_model = recruitment_model,
        futility_IF = futility_IF, total_events = total_events,
        analysis_model = analysis_model, seed = seed
      )
    }

    if (n_cores > 1) {
      result <- parallel::mclapply(seq_len(n_sims), run_one, mc.cores = n_cores)
    } else {
      result <- lapply(seq_len(n_sims), run_one)
    }

    raw_Z_by_scenario[[scen_name]] <- do.call(rbind, result)$Z
  }

  # Calibrate: boundary is the target_null_futility_rate-quantile of the
  # null Z distribution, since P(Z < boundary | null) = target rate by
  # definition of the quantile function.
  boundary <- stats::quantile(raw_Z_by_scenario[["null"]],
                              probs = target_null_futility_rate, na.rm = TRUE)
  boundary <- as.numeric(boundary)

  scenario_futility_rates <- vapply(raw_Z_by_scenario, function(Z) {
    mean(Z < boundary, na.rm = TRUE)
  }, numeric(1))

  settings <- list(
    futility_IF = futility_IF,
    total_events = total_events,
    analysis_model = analysis_model,
    target_null_futility_rate = target_null_futility_rate,
    n_sims = n_sims,
    n_cores = n_cores,
    seed = seed,
    package_version = tryCatch(as.character(utils::packageVersion("DTEAssurance")),
                               error = function(e) NA_character_),
    timestamp = as.character(Sys.time())
  )

  list(boundary = boundary,
       scenario_futility_rates = scenario_futility_rates,
       raw_Z_by_scenario = raw_Z_by_scenario,
       settings = settings)
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
#' @importFrom survival Surv
NULL




