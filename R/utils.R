# group_sequential_decision <- function(z_scores, critical_values, futility_values, sample_sizes, durations, alpha_spending) {
#   num_stages <- length(z_scores)
#
#   for (i in seq_len(num_stages - 1)) {  # Loop through all interim stages, not including the final one
#     # Check futility if a futility boundary is provided
#     if (!is.na(futility_values[i]) && z_scores[i] < futility_values[i]) {
#       return(list(
#         status = paste("Stop for Futility at IA", i),
#         successful = FALSE,
#         analysis = i,
#         z_score = z_scores[i],
#         sample_size = sample_sizes[i],
#         duration = durations[i],
#         final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
#       ))
#     }
#
#     # Check efficacy if a critical value is provided
#     if (!is.na(critical_values[i]) && z_scores[i] > critical_values[i]) {
#       return(list(
#         status = paste("Stop for Efficacy at IA", i),
#         successful = TRUE,
#         analysis = i,
#         z_score = z_scores[i],
#         sample_size = sample_sizes[i],
#         duration = durations[i],
#         final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
#       ))
#     }
#   }
#
#   # Now handle the final analysis (last stage)
#   final_success <- z_scores[num_stages] > critical_values[num_stages]
#
#   return(list(
#     status = if (final_success) "Successful at Final Analysis" else "Unsuccessful at Final Analysis",
#     successful = final_success,
#     analysis = num_stages,  # This is the final analysis
#     z_score = z_scores[num_stages],
#     sample_size = sample_sizes[num_stages],
#     duration = durations[num_stages],
#     final_success = z_scores[num_stages] > stats::qnorm(1-alpha_spending)
#   ))
# }
#
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
                               analysis_model,
                               delay_time_samples,
                               post_delay_HR_samples) {

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
    post_delay_HR <- post_delay_HR_samples[i]
  } else {
    delay_time <- delay_time_samples[i]
    post_delay_HR <- post_delay_HR_samples[i]
  }

  # --- Simulate survival data ---
  data <- sim_dte(n_c[j], n_t[j], lambda_c_i, delay_time, post_delay_HR,
                  dist = control_model$dist, gamma_c = gamma_c_i)

  # --- Add recruitment time ---
  data <- add_recruitment_time(data,
                               rec_method = recruitment_model$method,
                               rec_period = recruitment_model$period,
                               rec_power = recruitment_model$power,
                               rec_rate = recruitment_model$rate,
                               rec_duration = recruitment_model$duration)

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
    deltad = test_result$deltad,
    sample_size = censored$sample_size,
    cens_time = censored$cens_time
  ))
}


simulate_full_trial <- function(n_c, n_t,
                                control_model,
                                effect_model,
                                recruitment_model) {
  #--- Sample DTE parameters
  delay_time <- SHELF::sampleFit(effect_model$delay_SHELF, n = 1)[, effect_model$delay_dist]
  post_delay_HR <- SHELF::sampleFit(effect_model$HR_SHELF, n = 1)[, effect_model$HR_dist]

  #--- Sample control parameters (Exponential or Weibull)
  lambda_c_i <- NA
  gamma_c_i <- NA

  if (control_model$dist == "Exponential") {
    if (control_model$parameter_mode == "Fixed") {
      lambda_c_i <- control_model$lambda
    } else {
      lambda_c_i <- -log(
        rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)
      ) / control_model$t1
    }
  }

  if (control_model$dist == "Weibull") {
    if (control_model$parameter_mode == "Fixed" &&
        control_model$fixed_type == "Parameters") {
      lambda_c_i <- control_model$lambda
      gamma_c_i  <- control_model$gamma
    } else if (control_model$parameter_mode == "Fixed" &&
               control_model$fixed_type == "Landmark") {
      WeibFunc <- function(p) {
        lam <- p[1]; k <- p[2]
        c(
          exp(-(control_model$t1 * lam)^k) - control_model$surv_t1,
          exp(-(control_model$t2 * lam)^k) - control_model$surv_t2
        )
      }
      sol <- nleqslv::nleqslv(c(1,1), fn = WeibFunc)
      lambda_c_i <- sol$x[1]; gamma_c_i <- sol$x[2]
    } else {
      s1 <- rbeta(1, control_model$t1_Beta_a, control_model$t1_Beta_b)
      delta <- rbeta(1, control_model$diff_Beta_a, control_model$diff_Beta_b)
      s2 <- s1 - delta
      sol <- nleqslv::nleqslv(c(10,1), function(p) {
        lam <- p[1]; k <- p[2]
        c(
          exp(-(control_model$t1 / lam)^k) - s1,
          exp(-(control_model$t2 / lam)^k) - s2
        )
      })
      lambda_c_i <- 1 / sol$x[1]; gamma_c_i <- sol$x[2]
    }
  }

  if (is.na(lambda_c_i)) {
    stop("lambda_c_i not assigned; check control_model settings.")
  }

  #--- Simulate survival data with DTE
  trial_data <- sim_dte(
    n_c, n_t,
    lambda_c = lambda_c_i,
    delay_time = delay_time,
    post_delay_HR = post_delay_HR,
    dist = control_model$dist,
    gamma_c = gamma_c_i
  )

  #--- Add recruitment times
  trial_data <- add_recruitment_time(
    trial_data,
    rec_method   = recruitment_model$method,
    rec_period   = recruitment_model$period,
    rec_power    = recruitment_model$power,
    rec_rate     = recruitment_model$rate,
    rec_duration = recruitment_model$duration
  )

  return(trial_data)
}

apply_gsd_design <- function(trial_data,
                             analysis_model,
                             GSD_design) {
  K <- length(GSD_design$IF_vec)
  success      <- FALSE
  early_stop   <- FALSE
  futility_stop<- FALSE
  stage_success <- logical(K)
  stage_futility<- logical(K)

  for (k in seq_len(K)) {
    IF_k     <- GSD_design$IF_vec[k]
    alpha_k  <- GSD_design$alpha_spending[k]
    beta_k   <- GSD_design$beta_spending[k]

    censored <- cens_data(trial_data,
                          cens_method = "IF",
                          cens_IF     = IF_k)

    test <- survival_test(
      censored$data,
      analysis_method         = analysis_model$method,
      alpha                   = 1,  # manual boundary apply
      alternative             = analysis_model$alternative_hypothesis,
      rho                     = analysis_model$rho,
      gamma                   = analysis_model$gamma,
      t_star                  = analysis_model$t_star,
      s_star                  = analysis_model$s_star
    )

    Z <- test$Z

    if (Z >= qnorm(1 - alpha_k)) {
      success      <- TRUE
      early_stop   <- (k < K)
      stage_success[k] <- TRUE
      break
    } else if (Z <= qnorm(beta_k)) {
      futility_stop  <- TRUE
      stage_futility[k] <- TRUE
      break
    }
  }

  return(list(
    success       = success,
    early_stop    = early_stop,
    futility_stop = futility_stop,
    stage_success = stage_success,
    stage_futility= stage_futility
  ))
}


compare_gsd_designs <- function(n_c, n_t,
                                control_model,
                                effect_model,
                                recruitment_model,
                                analysis_model,
                                GSD_designs,
                                n_sims = 1000) {

  D <- length(GSD_designs)
  # Containers: designs × replications
  success_mat        <- matrix(FALSE, nrow = n_sims, ncol = D)
  early_stop_mat     <- matrix(FALSE, nrow = n_sims, ncol = D)
  futility_stop_mat  <- matrix(FALSE, nrow = n_sims, ncol = D)
  stage_success_arr  <- array(FALSE, dim = c(n_sims, D, max(sapply(GSD_designs, function(x) length(x$IF_vec)))))
  stage_futility_arr <- stage_success_arr

  for (i in seq_len(n_sims)) {
    trial_data <- simulate_full_trial(n_c, n_t,
                                      control_model,
                                      effect_model,
                                      recruitment_model)

    for (d in seq_len(D)) {
      res <- apply_gsd_design(trial_data,
                              analysis_model,
                              GSD_designs[[d]])

      success_mat[i, d]        <- res$success
      early_stop_mat[i, d]     <- res$early_stop
      futility_stop_mat[i, d]  <- res$futility_stop
      stage_success_arr[i, d, ]<- res$stage_success
      stage_futility_arr[i, d, ]<- res$stage_futility
    }
  }

  # Summarize by design
  summary_list <- lapply(seq_len(D), function(d) {
    K <- length(GSD_designs[[d]]$IF_vec)
    list(
      assurance           = mean(success_mat[, d]),
      early_stop_prob     = mean(early_stop_mat[, d]),
      futility_stop_prob  = mean(futility_stop_mat[, d]),
      stage_success       = colMeans(stage_success_arr[, d, 1:K]),
      stage_futility      = colMeans(stage_futility_arr[, d, 1:K])
    )
  })

  names(summary_list) <- names(GSD_designs)
  return(summary_list)
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




