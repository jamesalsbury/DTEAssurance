get_control_params <- function(dist, param_type, fixed_type, dist_type,
                               lambda_c, gamma_c,
                               t1, t2, surv_t1, surv_t2,
                               a1, b1, a2, b2, MCMC_sample) {
  if (dist == "Exponential") {
    if (param_type == "Fixed") {
      lambda <- if (fixed_type == "Landmark") -log(surv_t1) / t1 else lambda_c
    } else {
      lambda <- if (dist_type == "Elicitation") -log(stats::rbeta(1, a1, b1)) / t1 else sample(MCMC_sample[[1]], 1)
    }
    return(list(lambda_c = lambda, gamma_c = NULL))
  }

  if (dist == "Weibull") {
    if (param_type == "Fixed") {
      if (fixed_type == "Landmark") {
        WeibFunc <- function(params) {
          lambda <- params[1]; k <- params[2]
          c(exp(-(t1 * lambda)^k) - surv_t1,
            exp(-(t2 * lambda)^k) - surv_t1)
        }
        sol <- nleqslv::nleqslv(c(1, 1), fn = WeibFunc)
        return(list(lambda_c = sol$x[1], gamma_c = sol$x[2]))
      } else {
        return(list(lambda_c = lambda_c, gamma_c = gamma_c))
      }
    } else {
      s1 <- stats::rbeta(1, a1, b1)
      delta <- stats::rbeta(1, a2, b2)
      s2 <- s1 - delta
      sol <- nleqslv::nleqslv(c(10, 1), function(params) {
        lambda <- params[1]; k <- params[2]
        c(exp(-(t1 / lambda)^k) - s1,
          exp(-(t2 / lambda)^k) - s2)
      })
      return(list(lambda_c = 1 / sol$x[1], gamma_c = sol$x[2]))
    }
  }

  stop("Unsupported distribution type")
}

group_sequential_decision <- function(z_scores, critical_values, futility_values, sample_sizes, durations, alpha_spending) {
  num_stages <- length(z_scores)

  for (i in seq_len(num_stages - 1)) {  # Loop through all interim stages, not including the final one
    # Check futility if a futility boundary is provided
    if (!is.na(futility_values[i]) && z_scores[i] < futility_values[i]) {
      return(list(
        status = paste("Stop for Futility at IA", i),
        successful = FALSE,
        analysis = i,
        z_score = z_scores[i],
        sample_size = sample_sizes[i],
        duration = durations[i],
        final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
      ))
    }

    # Check efficacy if a critical value is provided
    if (!is.na(critical_values[i]) && z_scores[i] > critical_values[i]) {
      return(list(
        status = paste("Stop for Efficacy at IA", i),
        successful = TRUE,
        analysis = i,
        z_score = z_scores[i],
        sample_size = sample_sizes[i],
        duration = durations[i],
        final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
      ))
    }
  }

  # Now handle the final analysis (last stage)
  final_success <- z_scores[num_stages] > critical_values[num_stages]

  return(list(
    status = if (final_success) "Successful at Final Analysis" else "Unsuccessful at Final Analysis",
    successful = final_success,
    analysis = num_stages,  # This is the final analysis
    z_score = z_scores[num_stages],
    sample_size = sample_sizes[num_stages],
    duration = durations[num_stages],
    final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
  ))
}

strictly_increasing_combinations <- function(...) {
  #vectors <- list(...)  # Convert input vectors into a list
  combinations <- expand.grid(...)  # Generate all possible combinations

  # Filter rows where elements are in strictly increasing order
  valid_combinations <- combinations[apply(combinations, 1, function(row) all(diff(as.numeric(row)) > 0)), ]

  # Convert each row into a comma-separated string
  result <- apply(valid_combinations, 1, function(row) paste(row, collapse = ", "))

  return(result)  # Return as a character vector
}


simulate_one_trial <- function(i, j,
                               n_c, n_t,
                               control_dist, control_parameters, fixed_parameters_type, control_distribution,
                               lambda_c, gamma_c, MCMC_sample,
                               t1, t2, surv_t1, surv_t2,
                               t1_Beta_a, t1_Beta_b, diff_Beta_a, diff_Beta_b,
                               delay_time_samples, post_delay_HR_samples,
                               P_S, P_DTE,
                               cens_method, cens_time, cens_IF, cens_events,
                               rec_method, rec_period, rec_power, rec_rate, rec_duration,
                               analysis_method, alpha, alternative,
                               rho, gamma,
                               t_star, s_star,
                               target_HR) {

  control_params <- get_control_params(control_dist, control_parameters, fixed_parameters_type,
                                       control_distribution, lambda_c, gamma_c,
                                       t1, t2, surv_t1, surv_t2,
                                       t1_Beta_a, t1_Beta_b, diff_Beta_a, diff_Beta_b,
                                       MCMC_sample)
  lambda_i <- control_params$lambda_c
  gamma_i <- control_params$gamma_c

  separation <- runif(1) <= P_S
  delayed <- runif(1) <= P_DTE

  if (!separation) {
    delay_time <- 0
    post_delay_HR <- 1
  } else if (!delayed) {
    delay_time <- 0
    post_delay_HR <- post_delay_HR_samples[i]
  } else {
    delay_time <- delay_time_samples[i]
    post_delay_HR <- post_delay_HR_samples[i]
  }

  data <- sim_dte(n_c[j], n_t[j], lambda_i, delay_time, post_delay_HR,
                  dist = control_dist, gamma_c = gamma_i)

  data <- add_recruitment_time(data, rec_method,
                               rec_period = rec_period, rec_power = rec_power,
                               rec_rate = rec_rate, rec_duration = rec_duration)

  censored <- cens_data(data, cens_method, cens_time, cens_IF, cens_events)
  data_cens <- censored$data

  test <- survival_test(data_cens, analysis_method, alternative, alpha,
                        rho, gamma, t_star, s_star)

  return(list(
    Signif = test$Signif,
    deltad = test$deltad,
    cens_time = censored$cens_time,
    sample_size = censored$sample_size
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




