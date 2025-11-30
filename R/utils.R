

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

  if (GSD_model$futility_type %in% c("Beta", "none")) {

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
        GSD_model$futility_type == "Beta") {

      fut_idx <- which(abs(design$informationRates - IF_here) < 1e-8)

      fut_bound <- if (length(fut_idx) == 1 &&
                       fut_idx <= length(design$futilityBounds)) {
        design$futilityBounds[fut_idx]
      } else {
        NA
      }

      if (!is.na(fut_bound) && z_stat < fut_bound) {
        decision  <- "Stop for futility"
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
        decision  <- "Stop for futility"
        stop_time <- t_interim
        break
      }
    }

  } # end loop


  # -------------------------------
  # 4) Final analysis (unchanged)
  # -------------------------------
  if (is.na(stop_time)) {

    # the final alpha look is always the last critical value
    eff_idx <- length(design$criticalValues)
    eff_bound <- design$criticalValues[eff_idx]


    n_events <- event_thresholds[length(info_rates)]
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data |>
      dplyr::filter(.data$rec_time <= t_interim)

    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time
    )

    fit  <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    z_stat <- -summary(fit)$coefficients[, "z"]

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
                                    total_events,
                                    IF,
                                    analysis_model) {

    data <- simulate_trial_with_recruitment(
      n_c = n_c,
      n_t = n_t,
      control_model = control_model,
      effect_model = effect_model,
      recruitment_model = recruitment_model
    )

    censored_data <- cens_data(data, cens_method = "Events", cens_events = total_events * IF)

    data <- censored_data$data

    posterior_samples <- update_priors(data, control_model = control_model,
                                                     effect_model = effect_model,
                                                     n_samples = 100)

    BPP_outcome <-  BPP_func(data,
                                           posterior_samples,
                                           control_distribution = control_model$dist,
                                           n_c_planned = n_c,
                                           n_t_planned = n_t,
                                           rec_time_planned = recruitment_model$period,
                                           df_cens_time = censored_data$cens_time,
                                           censoring_model = list(method = "Events", events = total_events),
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
  if (fut_type == "Beta") {

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
  if (fut_type == "Beta") {

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


generate_model_inputs <- function(Scenario, Design) {


  n_c <- 400
  n_t <- 400


  recruitment_model <- list(method = "power",
                            period = 24,
                            power = 1)

  if (Scenario %in% c(1,2,3)){
    control_model <- list(dist = "Exponential",
                          parameter_mode = "Distribution",
                          t1 = 12,
                          t1_Beta_a = 3968503,
                          t1_Beta_b = 6031497)

  }




  if (Scenario == 1){
    effect_model <- list(delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                         delay_dist = "gamma",
                         HR_SHELF = SHELF::fitdist(c(0.55, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                         HR_dist = "gamma",
                         P_S = 0,
                         P_DTE = 0)
  }

  if (Scenario == 2){
    effect_model <- list(delay_SHELF = SHELF::fitdist(c(3.999, 4, 4.01), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                         delay_dist = "gamma",
                         HR_SHELF = SHELF::fitdist(c(0.599, 0.6, 0.601), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                         HR_dist = "gamma",
                         P_S = 1,
                         P_DTE = 1)
  }

  if (Scenario == 3){
    effect_model <- list(delay_SHELF = SHELF::fitdist(c(3.999, 4, 4.01), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                         delay_dist = "gamma",
                         HR_SHELF = SHELF::fitdist(c(0.599, 0.6, 0.601), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                         HR_dist = "gamma",
                         P_S = 1,
                         P_DTE = 0)
  }

  if (Scenario == 4){

    control_model <- list(dist = "Exponential",
                          parameter_mode = "Distribution",
                          t1 = 12,
                          t1_Beta_a = 10.2,
                          t1_Beta_b = 15.1)

    effect_model <- list(delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                         delay_dist = "gamma",
                         HR_SHELF = SHELF::fitdist(c(0.55, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                         HR_dist = "gamma",
                         P_S = 0.9,
                         P_DTE = 0.8)

  }

  if (Design == 1){

    censoring_model <- list(
      method = "Events",
      events = 650
    )

    analysis_model <- list(
      method = "LRT",
      alternative_hypothesis = "one.sided",
      alpha = 0.025
    )

  }


  if (Design == 2){

    GSD_model <- list(events = 650,
                      alpha_spending = c(0.0125, 0.025),
                      alpha_IF = c(0.75, 1),
                      futility_type = "none")


  }


  if (Design == 3){


    GSD_model <- list(events = 650,
                      alpha_spending = c(0.0125, 0.025),
                      alpha_IF = c(0.75, 1),
                      futility_type = "BPP",
                      futility_IF = 0.5,
                      BPP_threshold = 0.2)


    analysis_model <- list(method = "LRT",
                           alternative_hypothesis = "one.sided",
                           alpha = 0.025)


  }

  if (Design == 4){

    GSD_model <- list(events = 650,
                      alpha_spending = c(0.0125, 0.025),
                      alpha_IF = c(0.75, 1),
                      futility_type = "Beta",
                      futility_IF = c(0.5, 1),
                      beta_spending = c(0.05, 0.1))
  }

  list(
    n_c = n_c,
    n_t = n_t,
    recruitment_model = recruitment_model,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = if (exists("censoring_model")) censoring_model else NULL,
    GSD_model = if (exists("GSD_model")) GSD_model else NULL,
    analysis_model = if (exists("analysis_model")) analysis_model else NULL
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




