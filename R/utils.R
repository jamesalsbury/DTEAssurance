
# strictly_increasing_combinations <- function(...) {
#   #vectors <- list(...)  # Convert input vectors into a list
#   combinations <- expand.grid(...)  # Generate all possible combinations
#
#   # Filter rows where elements are in strictly increasing order
#   valid_combinations <- combinations[apply(combinations, 1, function(row) all(diff(as.numeric(row)) > 0)), ]
#
#   # Convert each row into a comma-separated string
#   result <- apply(valid_combinations, 1, function(row) paste(row, collapse = ", "))
#
#   return(result)  # Return as a character vector
# }


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
      lambda_c_i <- -log(rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)) / control_model$t1
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
      sampledS1 <- rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)
      sampledDelta <- rbeta(1, control_model$diff_Beta_a, control_model$diff_Beta_b)
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

  if (runif(1) > effect_model$P_S) {
    delay_time <- 0
    post_delay_HR <- 1
  } else if (runif(1) > effect_model$P_DTE) {
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
      filter(rec_time <= t_interim)

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
      filter(rec_time <= t_interim)

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




