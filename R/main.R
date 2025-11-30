#' Simulates survival times for a delayed treatment effect (DTE) scenario, where the treatment group experiences a delayed onset of benefit. Control and treatment groups are generated under exponential or Weibull distributions.
#'
#' @param n_c The number of patients in the control group
#' @param n_t The number of patients in the treatment group
#' @param lambda_c The baseline hazard rate for the control group
#' @param delay_time The length of delay before treatment effect begins
#' @param post_delay_HR The hazard ratio after the delay period
#' @param dist The distribution for the control group; must be one of "Exponential" (default) or "Weibull"
#' @param gamma_c The shape parameter for the Weibull distribution (only used if \code{dist = "Weibull"})
#'
#' @return A data frame with two columns:
#' \item{time}{Simulated survival times}
#' \item{group}{Group assignment: "Control" or "Treatment"}
#' Class: \code{data.frame}
#'
#' @examples
#' set.seed(123)
#' sim_data <- sim_dte(n_c = 10, n_t = 10, lambda_c = 0.1,
#'                     delay_time = 6, post_delay_HR = 0.6)
#' head(sim_data)
#'
#' @export
#'
sim_dte <- function(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = "Exponential", gamma_c = NULL){

  #Simulate control data
  u <- stats::runif(n_c)

  if (dist == "Exponential"){
    control_times <- -log(u)/lambda_c
  }

  if (dist == "Weibull"){
    control_times <- (1/lambda_c)*(-log(u))^(1/gamma_c)
  }

  #Simulate treatment data
  u <- stats::runif(n_t)
  if (dist == "Exponential"){
    CP <- exp(-lambda_c*delay_time)
    treatment_times <- ifelse(u>CP,
                              -log(u)/lambda_c,
                              (1/(post_delay_HR*lambda_c))*(post_delay_HR*lambda_c*delay_time-log(u)-lambda_c*delay_time))
  }

  if (dist == "Weibull"){
    CP <- exp(-(lambda_c*delay_time)^gamma_c)
    lambda_e <- lambda_c*post_delay_HR^(1/gamma_c)
    treatment_times <- ifelse(u > CP,
                              (1/lambda_c)*(-log(u))^(1/gamma_c),
                              (1/lambda_e)*(-log(u)+(lambda_e*delay_time)^gamma_c-(lambda_c*delay_time)^gamma_c)^(1/gamma_c))
  }

  #Combine the two groups
  data <- data.frame(time = c(control_times, treatment_times),
                     group = c(rep("Control", n_c), rep("Treatment", n_t)))

  return(data)

}


#' Censor a survival dataset
#'
#' Applies administrative censoring to a survival dataset using one of three methods: fixed time, fixed number of events, or fixed information fraction. The input data must contain columns for pseudo survival time, recruitment time, and observed time.
#'
#' @param data A dataframe containing uncensored survival data with columns: \code{pseudo_time}, \code{rec_time}, and \code{time}
#' @param cens_method Censoring method: \code{"Time"} (default), \code{"Events"}, or \code{"IF"}
#' @param cens_time Time point for censoring (required if \code{cens_method = "Time"})
#' @param cens_IF Information fraction for censoring (required if \code{cens_method = "IF"})
#' @param cens_events Number of events for censoring (required if \code{cens_method = "Events"})
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Censored dataframe with updated \code{status} and filtered rows}
#'   \item{cens_events}{Number of events used for censoring (if applicable)}
#'   \item{cens_time}{Time point used for censoring}
#'   \item{sample_size}{Number of patients remaining after censoring}
#' }
#'
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   pseudo_time = rexp(20, rate = 0.1),
#'   rec_time = runif(20, 0, 12),
#'   time = rexp(20, rate = 0.1)
#' )
#' censored <- cens_data(df, cens_method = "Time", cens_time = 10)
#' str(censored)
#'
#' @export

cens_data <- function(data,
                      cens_method = "Time",
                      cens_time = NULL,
                      cens_IF = NULL,
                      cens_events = NULL) {


  # Validate censoring method
  valid_methods <- c("Time", "Events", "IF")
  if (!cens_method %in% valid_methods) {
    stop("cens_method must be one of 'Time', 'Events', or 'IF'")
  }

  # Determine censoring time based on method
  data <- data[order(data$pseudo_time), ]

  if (cens_method == "Events") {
    if (is.null(cens_events)) stop("Please specify 'cens_events' for method 'Events'")
    if (cens_events > nrow(data)) stop("'cens_events' exceeds number of observations")
    cens_time <- data$pseudo_time[cens_events]
  }

  if (cens_method == "IF") {
    if (is.null(cens_IF)) stop("Please specify 'cens_IF' for method 'IF'")
    index <- floor(nrow(data) * cens_IF)
    if (index < 1 || index > nrow(data)) stop("Invalid 'cens_IF' value")
    cens_time <- data$pseudo_time[index]
  }

  if (cens_method == "Time" && is.null(cens_time)) {
    stop("Please specify 'cens_time' for method 'Time'")
  }

  # Apply censoring
  data$status <- as.integer(data$pseudo_time <= cens_time)
  data$enrolled <- data$rec_time < cens_time
  data <- data[data$enrolled, ]

  # Ensure survival time is defined
  if (!"time" %in% names(data)) {
    data$time <- data$pseudo_time - data$rec_time
  }

  data$survival_time <- ifelse(data$pseudo_time > cens_time,
                               cens_time - data$rec_time,
                               data$time)

  # Return results
  return(list(
    data = data,
    cens_events = cens_events,
    cens_time = cens_time,
    sample_size = nrow(data)
  ))
}


#' Calculate Assurance for a Trial with a Delayed Treatment Effect
#'
#' Simulates operating characteristics for a clinical trial under prior uncertainty about a delayed treatment effect. The function integrates beliefs about control survival, treatment delay, post-delay hazard ratio, recruitment, censoring, and analysis method to estimate assurance and other trial metrics.
#'
#' @param n_c Vector of control group sample sizes
#' @param n_t Vector of treatment group sample sizes
#' @param control_model A named list specifying the control arm survival distribution:
#'   \itemize{
#'     \item \code{dist}: Distribution type ("Exponential" or "Weibull")
#'     \item \code{parameter_mode}: Either "Fixed" or "Distribution"
#'     \item \code{fixed_type}: If "Fixed", specify as "Parameters" or "Landmark"
#'     \item \code{lambda}, \code{gamma}: Scale and shape parameters
#'     \item \code{t1}, \code{t2}: Landmark times
#'     \item \code{surv_t1}, \code{surv_t2}: Survival probabilities at landmarks
#'     \item \code{t1_Beta_a}, \code{t1_Beta_b}, \code{diff_Beta_a}, \code{diff_Beta_b}: Beta prior parameters
#'   }
#' @param effect_model A named list specifying beliefs about the treatment effect:
#'   \itemize{
#'     \item \code{delay_SHELF}, \code{HR_SHELF}: SHELF objects encoding beliefs
#'     \item \code{delay_dist}, \code{HR_dist}: Distribution types ("hist" by default)
#'     \item \code{P_S}: Probability that survival curves separate
#'     \item \code{P_DTE}: Probability of delayed separation, conditional on separation
#'   }
#' @param censoring_model A named list specifying the censoring mechanism:
#'   \itemize{
#'     \item \code{method}: "Time", "Events", or "IF"
#'     \item \code{time}, \code{events}, \code{IF}: Parameters for each method
#'   }
#' @param recruitment_model A named list specifying the recruitment process:
#'   \itemize{
#'     \item \code{method}: "power" or "PWC"
#'     \item \code{period}, \code{power}: Parameters for power model
#'     \item \code{rate}, \code{duration}: Comma-separated strings for PWC model
#'   }
#' @param analysis_model A named list specifying the statistical test and decision rule:
#'   \itemize{
#'     \item \code{method}: "LRT", "WLRT", or "MW"
#'     \item \code{alpha}, \code{alternative_hypothesis}: Type I error and hypothesis direction
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: Parameters for WLRT or MW
#'     \item \code{success_threshold_HR}: Optional threshold for declaring success
#'   }
#' @param n_sims Number of simulations to run (default = 1000)
#'
#' @return A named list containing:
#' \describe{
#'   \item{assurance}{Estimated assurance (probability of success under prior uncertainty)}
#'   \item{CI}{95% confidence interval for assurance}
#'   \item{duration}{Mean trial duration across simulations}
#'   \item{sample_size}{Mean sample size across simulations}
#'   \item{diagnostics}{Additional diagnostics if \code{success_threshold_HR} is specified}
#' }
#' Class: \code{list}
#'
#' @examples
#' # Minimal example with placeholder inputs
#' control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
#' fixed_type = "Parameters", lambda = 0.1)
#' effect_model <- list(delay_SHELF = SHELF::fitdist(c(3, 4, 5),
#' probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
#' delay_dist = "gamma",
#' HR_SHELF = SHELF::fitdist(c(0.55, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1.5),
#' HR_dist = "gamma",
#' P_S = 1, P_DTE = 0)
#' censoring_model <- list(method = "Time", time = 12)
#' recruitment_model <- list(method = "power", period = 12, power = 1)
#' analysis_model <- list(method = "LRT", alpha = 0.025, alternative_hypothesis = "two.sided")
#' result <- calc_dte_assurance(n_c = 300, n_t = 300,
#'                                      control_model = control_model,
#'                                      effect_model = effect_model,
#'                                      censoring_model = censoring_model,
#'                                      recruitment_model = recruitment_model,
#'                                      analysis_model = analysis_model,
#'                                      n_sims = 10)
#' str(result)
#'
#' @export

calc_dte_assurance <- function(n_c,
                               n_t,
                               control_model,
                               effect_model,
                               censoring_model,
                               recruitment_model,
                               analysis_model,
                               n_sims = 1000) {

  if (censoring_model$method == "Events") {

    # Check required field
    if (is.null(censoring_model$events)) {
      stop("Error: censoring_model$events must be specified when method = 'Events'.")
    }

    # Check logical condition
    if (all((n_c + n_t) <= censoring_model$events)) {
      stop("Error: n_c + n_t needs to be greater than the number of events.")
    }

    loopVec <- (n_c + n_t) > censoring_model$events

  } else if (censoring_model$method == "Time") {

    # Check required field
    if (is.null(censoring_model$time)) {
      stop("Error: censoring_model$time must be specified when method = 'Time'.")
    }

    loopVec <- rep(TRUE, length(n_c))  # or your time-based logic here

  } else if (censoring_model$method == "IF") {

    # Check required field
    if (is.null(censoring_model$IF)) {
      stop("Error: censoring_model$IF must be specified when method = 'IF'.")
    }

    loopVec <- rep(TRUE, length(n_c))  # or your IF-based logic here

  } else {
    stop("Error: censoring_model$method must be one of 'Events', 'Time', or 'IF'.")
  }



  # Preallocate output containers
  assurance_vec <- numeric(length(n_c))
  CI_mat <- matrix(NA, nrow = length(n_c), ncol = 2)
  duration_vec <- numeric(length(n_c))
  sample_size_vec <- numeric(length(n_c))

  if (!is.null(analysis_model$success_threshold_HR)) {
    assurance_targetHR_vec <- numeric(length(n_c))
    CI_targetHR_mat <- matrix(NA, nrow = length(n_c), ncol = 2)
  }

  for (j in seq_along(n_c)) {
    if (!loopVec[j]) {
      assurance_vec[j] <- NA
      CI_mat[j, ] <- NA
      duration_vec[j] <- NA
      sample_size_vec[j] <- NA
      if (!is.null(analysis_model$success_threshold_HR)) {
        assurance_targetHR_vec[j] <- NA
        CI_targetHR_mat[j, ] <- NA
      }
      next
    }


    sim_results <- future.apply::future_lapply(seq_len(n_sims), simulate_one_trial, future.seed = TRUE,
                                 j = j,
                                 n_c = n_c[j], n_t = n_t[j],
                                 control_model = control_model,
                                 effect_model = effect_model,
                                 censoring_model = censoring_model,
                                 recruitment_model = recruitment_model,
                                 analysis_model = analysis_model)

    assurance_flags <- sapply(sim_results, `[[`, "Signif")
    cens_vec <- sapply(sim_results, `[[`, "cens_time")
    ss_vec <- sapply(sim_results, `[[`, "sample_size")

    assurance_vec[j] <- mean(assurance_flags)
    CI_mat[j, ] <- stats::binom.test(sum(assurance_flags), n_sims)$conf.int
    duration_vec[j] <- mean(cens_vec)
    sample_size_vec[j] <- mean(ss_vec)

    if (!is.null(analysis_model$success_threshold_HR)) {
      observed_HR_vec <- sapply(sim_results, `[[`, "observed_HR")
      assurance_targetHR_flags <- assurance_flags * (observed_HR_vec < analysis_model$success_threshold_HR)
      assurance_targetHR_vec[j] <- mean(assurance_targetHR_flags)
      CI_targetHR_mat[j, ] <- stats::binom.test(sum(assurance_targetHR_flags), n_sims)$conf.int
    }
  }

  # Assemble structured output
  output <- list(
    assurance = assurance_vec,
    CI = CI_mat,
    duration = duration_vec,
    sample_size = sample_size_vec
  )

  if (!is.null(analysis_model$success_threshold_HR)) {
    output$assurance_targetHR <- assurance_targetHR_vec
    output$CI_targetHR <- CI_targetHR_mat
  }

  return(output)
}


#' Calculate statistical significance on a survival dataset
#'
#' Performs a survival analysis using either the standard log-rank test (LRT) or a weighted log-rank test (WLRT). The function estimates the hazard ratio and determines whether the result is statistically significant based on the specified alpha level and alternative hypothesis.
#'
#' @param data A dataframe containing survival data. Must include columns for survival time, event status, and treatment group.
#' @param analysis_method Method of analysis: \code{"LRT"} (default) for standard log-rank test, or \code{"WLRT"} for weighted log-rank test.
#' @param alpha Type I error threshold for significance testing.
#' @param alternative String specifying the alternative hypothesis. Must be one of \code{"one.sided"} or \code{"two.sided"} (default).
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test.
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test.
#' @param t_star Parameter \eqn{t^*} used in modestly weighted tests.
#' @param s_star Parameter \eqn{s^*} used in modestly weighted tests.
#'
#' @return A list containing:
#' \describe{
#'   \item{Signif}{Logical indicator of statistical significance based on the chosen test and alpha level.}
#'   \item{observed_HR}{Estimated hazard ratio from a Cox proportional hazards model.}
#' }
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   survival_time = rexp(40, rate = 0.1),
#'   status = rbinom(40, 1, 0.8),
#'   group = rep(c("Control", "Treatment"), each = 20)
#' )
#' result <- survival_test(df, analysis_method = "LRT", alpha = 0.05)
#' str(result)
#'
#' @export


survival_test <- function(data, analysis_method = "LRT", alternative = "one.sided", alpha = 0.05, rho = 0, gamma = 0,
                          t_star = NULL, s_star = NULL){

  coxmodel <- survival::coxph(Surv(survival_time, status)~group, data = data)
  observed_HR <- as.numeric(exp(stats::coef(coxmodel)))

  Signif <- 0

  if (analysis_method=="LRT"){
    test_result <- survival::survdiff(Surv(survival_time, status) ~ group, data = data)
    Z <- (test_result$exp[2] - test_result$obs[2]) / sqrt(test_result$var[2, 2])
    if (alternative=="one.sided"){
      Signif <- Z > stats::qnorm(1-alpha)
    } else {
      Signif <- abs(Z) > stats::qnorm(1-alpha/2)
    }

  } else if (analysis_method=="WLRT"){
    test <- nph::logrank.test(data$survival_time, data$status, data$group, rho = rho, gamma = gamma)
    if (alternative=="one.sided"){
      Signif <- (test$test$Chisq > stats::qchisq(1-alpha, 1) & observed_HR<1)
    } else {
      Signif <- test$test$Chisq > stats::qchisq(1-alpha/2, 1)
    }
  } else if (analysis_method=="MW"){
    test <- nphRCT::wlrt(Surv(survival_time, status)~group,
                         data = data, method = "mw",
                         t_star = t_star, s_star = s_star)
    if (alternative=="one.sided"){
      Signif <- test$z > stats::qnorm(1-alpha)
    } else {
      Signif <- abs(test$z) > stats::qnorm(1-alpha/2)
    }
  }

  return(list(Signif = Signif, observed_HR = observed_HR, Z = Z))

}

#' Add recruitment time to a survival dataset
#'
#' Simulates recruitment timing for each patient in a survival dataset using either a power model or a piecewise constant (PWC) model. The function appends recruitment times and pseudo survival times (time from recruitment to event or censoring).
#'
#' @param data A dataframe containing survival data with columns: \code{time}, \code{status}, and \code{group}
#' @param rec_method Recruitment method: \code{"power"} for power model or \code{"PWC"} for piecewise constant model
#' @param rec_period Period length for the power model
#' @param rec_power Power parameter for the power model
#' @param rec_rate Comma-separated string of recruitment rates for the PWC model
#' @param rec_duration Comma-separated string of durations corresponding to each rate in the PWC model
#'
#' @return A dataframe with two additional columns:
#' \describe{
#'   \item{rec_time}{Simulated recruitment time for each patient}
#'   \item{pseudo_time}{Time from recruitment to event or censoring}
#' }
#' Class: \code{data.frame}
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   time = rexp(20, rate = 0.1),
#'   status = rbinom(20, 1, 0.8),
#'   group = rep(c("Control", "Treatment"), each = 10)
#' )
#' recruited <- add_recruitment_time(df, rec_method = "power", rec_period = 12, rec_power = 1)
#' head(recruited)
#'
#' @export


add_recruitment_time <- function(data, rec_method,
                                 rec_period = NULL, rec_power = NULL,
                                 rec_rate = NULL, rec_duration = NULL) {

  # --- Input validation ---
  if (!rec_method %in% c("power", "PWC")) {
    stop("rec_method must be either 'power' or 'PWC'")
  }

  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe")

  n_patients <- nrow(data)

  # --- Power model recruitment ---
  if (rec_method == "power") {
    if (is.null(rec_period) || is.null(rec_power)) {
      stop("rec_period and rec_power must be specified for power recruitment")
    }
    data$rec_time <- rec_period * stats::runif(n_patients)^(1 / rec_power)
  }

  # --- Piecewise constant recruitment ---
  if (rec_method == "PWC") {
    if (is.null(rec_rate) || is.null(rec_duration)) {
      stop("rec_rate and rec_duration must be specified for PWC recruitment")
    }

    rec_rate <- as.numeric(unlist(strsplit(rec_rate, ",")))
    rec_duration <- as.numeric(unlist(strsplit(rec_duration, ",")))

    if (any(rec_rate < 0)) stop("rec_rate values must be non-negative")
    if (length(rec_rate) != length(rec_duration)) {
      stop("rec_rate and rec_duration must have the same length")
    }

    df <- data.frame(
      rate = rec_rate,
      duration = rec_duration,
      origin = c(0, cumsum(rec_duration)[-length(rec_duration)]),
      finish = cumsum(rec_duration)
    )
    df$lambda <- df$rate * df$duration
    df$N <- sapply(df$lambda, function(x) stats::rpois(1, lambda = x))

    rec <- unlist(mapply(function(N, origin, finish) {
      if (N > 0) sort(stats::runif(N, min = origin, max = finish)) else numeric(0)
    }, df$N, df$origin, df$finish))

    if (length(rec) < n_patients) {
      final_rate <- utils::tail(df$rate, 1)
      final_time <- utils::tail(df$finish, 1)
      if (final_rate == 0) stop("Final recruitment rate must be positive to complete enrollment")
      extra <- cumsum(stats::rexp(n_patients - length(rec), rate = final_rate)) + final_time
      rec <- c(rec, extra)
    }

    data$rec_time <- rec[1:n_patients]
  }

  # --- Add pseudo time ---
  data$pseudo_time <- data$time + data$rec_time
  attr(data, "recruitment_model") <- rec_method

  return(data)
}


#' Calculates operating characteristics for a Group Sequential Trial with a Delayed Treatment Effect
#'
#' Simulates assurance and operating characteristics for a group sequential trial under prior uncertainty about a delayed treatment effect. The function integrates beliefs about control survival, treatment delay, post-delay hazard ratio, recruitment, and group sequential design (GSD) parameters.
#'
#' @param n_c Control group sample size
#' @param n_t Treatment group sample size
#' @param control_model A named list specifying the control arm survival distribution:
#'   \itemize{
#'     \item \code{dist}: Distribution type ("Exponential" or "Weibull")
#'     \item \code{parameter_mode}: Either "Fixed" or "Distribution"
#'     \item \code{fixed_type}: If "Fixed", specify as "Parameters" or "Landmark"
#'     \item \code{lambda}, \code{gamma}: Scale and shape parameters
#'     \item \code{t1}, \code{t2}: Landmark times
#'     \item \code{surv_t1}, \code{surv_t2}: Survival probabilities at landmarks
#'     \item \code{t1_Beta_a}, \code{t1_Beta_b}, \code{diff_Beta_a}, \code{diff_Beta_b}: Beta prior parameters
#'   }
#' @param effect_model A named list specifying beliefs about the treatment effect:
#'   \itemize{
#'     \item \code{delay_SHELF}, \code{HR_SHELF}: SHELF objects encoding beliefs
#'     \item \code{delay_dist}, \code{HR_dist}: Distribution types ("hist" by default)
#'     \item \code{P_S}: Probability that survival curves separate
#'     \item \code{P_DTE}: Probability of delayed separation, conditional on separation
#'   }
#' @param recruitment_model A named list specifying the recruitment process:
#'   \itemize{
#'     \item \code{method}: "power" or "PWC"
#'     \item \code{period}, \code{power}: Parameters for power model
#'     \item \code{rate}, \code{duration}: Comma-separated strings for PWC model
#'   }
#' @param GSD_model A named list specifying the group sequential design:
#'   \itemize{
#'     \item \code{events}: Total number of events
#'     \item \code{alpha_spending}: Cumulative alpha spending vector
#'     \item \code{alpha_IF}: Information Fraction at which we look for efficacy
#'     \item \code{futility_type}: \code{beta} (for beta-spending), \code{BPP} (for Bayesian Predictive Probability) or \code{none}
#'     \item \code{futility_IF}: Information Fraction at which we look for futility
#'     \item \code{beta_spending}: Cumulative beta spending vector
#'     \item \code{BPP_threshold}: BPP value at which we will stop for futility
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level.
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param n_sims Number of simulations to run (default = 1000)
#'
#' @return A data frame with one row per simulated trial and the following columns:
#' \describe{
#'   \item{Trial}{Simulation index}
#'   \item{IF}{Information fraction label used at the decision point}
#'   \item{Decision}{Interim decision outcome (e.g., "Continue", "Stop for efficacy", "Stop for futility")}
#'   \item{StopTime}{Time at which the trial stopped or completed}
#'   \item{SampleSize}{Total sample size at the time of decision}
#'   \item{Final_Decision}{Final classification of trial success based on the test statistic and threshold}
#' }
#' Class: \code{data.frame}
#'
#' @examples
#' # Minimal example with placeholder inputs
#' control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
#' fixed_type = "Parameters", lambda = 0.1)
#'effect_model <- list(P_S = 1, P_DTE = 0,
#'HR_SHELF = SHELF::fitdist(c(0.6, 0.65, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 2),
#'HR_dist = "gamma",
#'delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
#'delay_dist = "gamma"
#')
#' recruitment_model <- list(method = "power", period = 12, power = 1)
#' GSD_model <- list(events = 300, alpha_spending = c(0.0125, 0.025),
#'                   alpha_IF = c(0.75, 1), futility_type = "none")
#' result <- calc_dte_assurance_adaptive(n_c = 300, n_t = 300,
#'                         control_model = control_model,
#'                         effect_model = effect_model,
#'                         recruitment_model = recruitment_model,
#'                         GSD_model = GSD_model,
#'                         n_sims = 10)
#' str(result)
#'
#' @export


calc_dte_assurance_adaptive <- function(n_c, n_t,
                                       control_model,
                                       effect_model,
                                       recruitment_model,
                                       GSD_model,
                                       analysis_model = NULL,
                                       n_sims = 1000) {

  if (!is.null(GSD_model$futility_type) &&
      GSD_model$futility_type == "BPP" &&
      !identical(control_model$parameter_mode, "Distribution")) {

    stop(
      "Invalid specification: when `GSD_model$futility_type` is \"BPP\", ",
      "`control_model$parameter_mode` must be \"Distribution\"."
    )
  }


  results <- future.apply::future_lapply(seq_len(n_sims), function(i) {

    # --- simulate one trial ---
    trial <- simulate_trial_with_recruitment(
      n_c, n_t, control_model, effect_model, recruitment_model
    )

    trial_data <- trial[order(trial$pseudo_time),]
    n_events   <- GSD_model$events
    t_interim  <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)

    # Censoring logic
    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(
      eligible_df$status,
      eligible_df$time,
      t_interim - eligible_df$rec_time
    )

    # Interim Cox model (for final decision)
    fit         <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    fit_summary <- summary(fit)
    z_stat      <- -fit_summary$coefficients[, "z"]

    # --- Futility type: beta-spending ---
    if (GSD_model$futility_type %in% c("Beta", "none")) {

      rpact_design <- make_rpact_design_from_GSD_model(GSD_model)
      design       <- rpact_design$design

      outcome <- apply_GSD_to_trial(trial_data = trial, GSD_model = GSD_model, design = design, total_events = GSD_model$events)

      return(data.frame(
        Trial          = i,
        Decision       = outcome$decision,
        StopTime       = outcome$stop_time,
        SampleSize     = outcome$sample_size,
        Final_Decision = ifelse(
          z_stat > stats::qnorm(1 - 0.025),
          "Successful",
          "Unsuccessful"
        )
      ))
    }

    # --- placeholder for future types ---
    if (GSD_model$futility_type == "BPP") {

      # Build rpact design (still needed for efficacy boundaries)
      rpact_design <- make_rpact_design_from_GSD_model(GSD_model)
      design       <- rpact_design$design

      outcome <- apply_GSD_to_trial(
        n_c = n_c,
        n_t = n_t,
        trial_data        = trial,
        design            = design,
        total_events      = GSD_model$events,
        GSD_model         = GSD_model,
        control_model     = control_model,        # elicited prior
        effect_model      = effect_model,         # elicited prior
        recruitment_model = recruitment_model,
        analysis_model    = analysis_model,
        n_BPP_sims        = 50                    # or whatever you choose
      )

      return(data.frame(
        Trial          = i,
        Decision       = outcome$decision,
        StopTime       = outcome$stop_time,
        SampleSize     = outcome$sample_size,
        Final_Decision = ifelse(
          z_stat > stats::qnorm(1 - 0.025),
          "Successful",
          "Unsuccessful"
        )
      ))
    }



  }, future.seed = TRUE)

  # Combine into single data frame
  results_df <- do.call(rbind, results)

  return(results_df)
}


#' Update prior distributions using interim survival data
#'
#' This function updates elicited priors (defined through SHELF objects and
#' parametric prior distributions) using interim survival data under a
#' delayed-effect, piecewise-exponential model for the treatment arm and an
#' exponential or Weibull model for the control arm.
#'
#' @param data A data frame containing interim survival data with columns:
#'   \itemize{
#'     \item \code{survival_time} Observed time from randomisation to event/censoring.
#'     \item \code{status} Event indicator (1 = event, 0 = censored).
#'     \item \code{group} Group identifier (e.g., "Control", "Treatment").
#'   }
#'
#' @param control_model A named list specifying the control arm survival distribution:
#'   \itemize{
#'     \item \code{dist}: Distribution type ("Exponential" or "Weibull")
#'     \item \code{parameter_mode}: Either "Fixed" or "Distribution"
#'     \item \code{fixed_type}: If "Fixed", specify as "Parameters" or "Landmark"
#'     \item \code{lambda}, \code{gamma}: Scale and shape parameters
#'     \item \code{t1}, \code{t2}: Landmark times
#'     \item \code{surv_t1}, \code{surv_t2}: Survival probabilities at landmarks
#'     \item \code{t1_Beta_a}, \code{t1_Beta_b}, \code{diff_Beta_a}, \code{diff_Beta_b}: Beta prior parameters
#'   }
#' @param effect_model A named list specifying beliefs about the treatment effect:
#'   \itemize{
#'     \item \code{delay_SHELF}, \code{HR_SHELF}: SHELF objects encoding beliefs
#'     \item \code{delay_dist}, \code{HR_dist}: Distribution types ("hist" by default)
#'     \item \code{P_S}: Probability that survival curves separate
#'     \item \code{P_DTE}: Probability of delayed separation, conditional on separation
#'   }
#'
#' @param n_samples Number of posterior samples to generate (default: 1000).
#'
#' @return A data frame containing Monte Carlo samples from the updated (posterior)
#'   distribution of the model parameters. Columns normally include:
#'   \itemize{
#'     \item \code{lambda_c} Posterior samples for the control hazard parameter.
#'     \item \code{delay_time} Posterior samples for the delay/changepoint time \eqn{T}.
#'     \item \code{HR} Posterior samples for the post-delay hazard ratio.
#'     \item \code{gamma_c} (only if \code{control_distribution = "Weibull"})
#'           Posterior samples for the Weibull shape parameter.
#'   }
#'
#'
#' Priors for \code{lambda_c}, \code{T}, and \code{HR} are constructed from
#' elicited distributions using the SHELF framework, then updated through
#' sampling-based posterior inference.
#'
#' @export
#'
#' @examples
#'
#' interim_data = data.frame(survival_time = runif(10, min = 0, max = 1),
#' status = rbinom(10, size = 1, prob = 0.5),
#' group = c(rep("Control", 5), rep("Treatment", 5)))
#' control_model = list(dist = "Exponential",
#'                      parameter_mode = "Distribution",
#'                      t1 = 12,
#'                      t1_Beta_a = 20,
#'                      t1_Beta_b = 32)
#' effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
#'                     delay_dist = "gamma",
#'                     HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
#'                     HR_dist = "gamma",
#'                     P_S = 1,
#'                     P_DTE = 0)
#'
#' posterior_df <- update_priors(
#'   data = interim_data,
#'   control_model = control_prior_list,
#'   effect_model = effect_model,
#'   n_samples = 10)
#'
#'

update_priors <- function(data,
                          control_model,
                          effect_model,
                          n_samples = 1000) {

  if (control_model$dist == "Exponential"){

    control_jags <- paste0(
      "s1 ~ dbeta(", control_model$t1_Beta_a, ", ", control_model$t1_Beta_b, ")\n",
      "lambda_c <- -log(s1)/t1\n"
    )

  }

  if (control_model$dist == "Weibull"){


      control_jags <- paste0("s1 ~ dbeta(", control_model$t1_Beta_a, ", ", control_model$t1_Beta_b, ")",
                             "delta ~ ", control_model$diff_Beta_a, ", ", control_model$diff_Beta_b, ")",
                             "gamma_c <- log(log(s1) / log(s1 - delta) ) / log(t1 / t2)",
                             "lambda_c <- (-log(s1))^(1 / gamma_c) / t1")

  }

  delay_jags <- make_prior_name_jags(effect_model$delay_SHELF, effect_model$delay_dist)
  HR_jags <- make_prior_name_jags(effect_model$HR_SHELF, effect_model$HR_dist)

  if (control_model$dist == "Exponential"){

    modelstring <- paste0("

data {
  for (j in 1:m){
    zeros[j] <- 0
  }
}

model {
  C <- 10000
  for (i in 1:n){
    zeros[i] ~ dpois(zeros.mean[i])
    zeros.mean[i] <-  -l[i] + C
    l[i] <- ifelse(data_event[i]==1, log(lambda_c)-(lambda_c*data_time[i]), -(lambda_c*data_time[i]))
  }
  for (i in (n+1):m){
    zeros[i] ~ dpois(zeros.mean[i])
    zeros.mean[i] <-  -l[i] + C
    l[i] <- ifelse(data_event[i]==1, ifelse(data_time[i]<delay_time, log(lambda_c)-(lambda_c*data_time[i]), log(lambda_t)-lambda_t*(data_time[i]-delay_time)-(delay_time*lambda_c)),
      ifelse(data_time[i]<delay_time, -(lambda_c*data_time[i]), -(lambda_c*delay_time)-lambda_t*(data_time[i]-delay_time)))
  }



  Z ~ dcat(pi[])

  HR_slab    ~ ", HR_jags, "
  delay_slab ~ ", delay_jags, "
  HR <- equals(Z, 1) * 1
        + (1 - equals(Z, 1)) * HR_slab

  delay_time <- equals(Z, 3) * delay_slab
  ", control_jags, "
  lambda_t <- lambda_c * HR

    }
"
    )

  }


  if (control_model$dist == "Weibull"){

    modelstring <- paste0("

data {
  for (j in 1:m){
    zeros[j] <- 0
  }
}

model {
  C <- 10000
  for (i in 1:n){
    zeros[i] ~ dpois(zeros.mean[i])
    zeros.mean[i] <-  -l[i] + C
    l[i] <- ifelse(data_event[i]==1, log(gamma_c)+gamma_c*log(lambda_c*data_time[i])-(lambda_c*data_time[i])^gamma_c-log(data_time[i]), -(lambda_c*data_time[i])^gamma_c)
  }
  for (i in (n+1):m){
    zeros[i] ~ dpois(zeros.mean[i])
    zeros.mean[i] <-  -l[i] + C
    l[i] <- ifelse(data_event[i]==1, ifelse(data_time[i]<delay_time, log(gamma_c)+gamma_c*log(lambda_c*data_time[i])-(lambda_c*data_time[i])^gamma_c-log(data_time[i]), log(gamma_c)+gamma_c*log(lambda_t)+(gamma_c-1)*log(data_time[i])-lambda_t^gamma_c*(data_time[i]^gamma_c-delay_time^gamma_c)-(delay_time*lambda_c)^gamma_c),
      ifelse(data_time[i]<delay_time, -(lambda_c*data_time[i])^gamma_c, -(lambda_c*delay_time)^gamma_c-lambda_t^gamma_c*(data_time[i]^gamma_c-delay_time^gamma_c)))
  }





  Z ~ dcat(pi[])

  HR_slab    ~ ", HR_jags, "
  delay_slab ~ ", delay_jags, "
  HR <- equals(Z, 1) * 1
        + (1 - equals(Z, 1)) * HR_slab

  delay_time <- equals(Z, 3) * delay_slab

  ", control_jags, "
  lambda_t <- lambda_c*pow(HR, 1/gamma_c)

    }
"
)



  }


data <- data[order(data$group),]
n_control <- sum(data$group=="Control")
n_total <- nrow(data)

data_list <- list(data_time = data$survival_time,
                  data_event = data$status,
                  n = n_control,
                  m = n_total)

P_S <- effect_model$P_S
P_DTE <- effect_model$P_DTE

pi_vec <- c(
  1 - P_S,              # 1: no separation
  P_S * (1 - P_DTE),    # 2: immediate separation
  P_S * P_DTE           # 3: delayed separation
)

data_list$pi <- pi_vec


  if (control_model$dist == "Exponential"){
    data_list$t1 <- control_model$t1
  }

  if (control_model$dist == "Weibull"){
    data_list$t1 <- control_model$t1
    data_list$t2 <- control_model$t2
  }



model = rjags::jags.model(textConnection(modelstring), data = data_list, quiet = T)


stats::update(model, n.iter=100)

var_names <- if (control_model$dist == "Exponential") {
  c("lambda_c", "HR", "delay_time")
} else {
  c("lambda_c", "gamma_c", "HR", "delay_time")
}

output = rjags::coda.samples(model=model, variable.names=var_names, n.iter = n_samples)


posterior_df <- as.data.frame(as.matrix(output))

return(posterior_df)

}

#' Calculate Bayesian Predictive Probability given interim data and posterior samples
#'
#' @param data A data frame containing interim survival data, censored at \code{df_cens_time}, with columns:
#'   \itemize{
#'     \item \code{time} Final observed/event time at the interim (on the analysis time scale).
#'     \item \code{group} Treatment group indicator (e.g. "Control", "Treatment").
#'     \item \code{rec_time} Recruitment (calendar) time.
#'     \item \code{pseudo_time} \code{time + rec_time} (calendar time at event/censoring).
#'     \item \code{status} Event indicator at the interim (1 = event, 0 = censored).
#'     \item \code{survival_time} Observed follow-up time from randomisation to event/censoring at the interim.
#'   }
#' @param posterior_df A data frame of posterior samples with columns:
#'   \code{lambda_c}, \code{delay_time} and \code{HR}, corresponding to the control hazard,
#'   the delay (changepoint) time and the post-delay hazard ratio, respectively.
#' @param control_distribution Distributional form assumed for the control arm:
#'   either \code{"Exponential"} (default) or \code{"Weibull"}.
#' @param n_c_planned Planned maximum number of patients in the control group.
#' @param n_t_planned Planned maximum number of patients in the treatment group.
#' @param rec_time_planned Planned maximum recruitment calendar time for the full trial.
#' @param df_cens_time Calendar time at which \code{df} has been censored (interim analysis time).
#' @param censoring_model A named list specifying the censoring mechanism for the future data:
#'   \itemize{
#'     \item \code{method}: one of \code{"Time"}, \code{"Events"}, or \code{"IF"}.
#'     \item \code{time}, \code{events}, \code{IF}: parameters for the corresponding method.
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level.
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param n_sims Number of predictive simulations to run (default is 1000).
#'
#' @return A single numeric value giving the Bayesian predictive probability of success at the final analysis under the specified design, censoring model and analysis model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   BPP_func(df, posterior_df, n_c_planned = 500, n_t_planned = 500,
#'            rec_time_planned = 34, df_cens_time = 20,
#'            censoring_model = list(method = "Events", events = 1200),
#'            analysis_model = list(method = "LRT",
#'                                  alpha = 0.025,
#'                                  alternative_hypothesis = "one.sided"))
#' }
BPP_func <- function(data, posterior_df, control_distribution = "Exponential", n_c_planned, n_t_planned,
                     rec_time_planned, df_cens_time,
                     censoring_model, analysis_model,
                     n_sims = 500) {


  #The number of unenrolled patients in each group
  n_unenrolled_control <- (n_c_planned) - sum(data$group=="Control")
  n_unenrolled_treatment <- (n_t_planned) - sum(data$group=="Treatment")

  #Extract realisations from the MCMC
  lambda_c_samples <- posterior_df$lambda_c
  delay_time_samples <- posterior_df$delay_time
  post_delay_HR_samples <- posterior_df$HR

  if (control_distribution == "Weibull"){
    gamma_c_samples <- posterior_df$gamma_c
  }

  BPP_df <- data.frame(success = numeric(n_sims),
                       Z_val = numeric(n_sims))

  for (j in 1:n_sims){


    #Sampling the recruitment times for the unenrolled patients
    unenrolled_rec_times <- stats::runif(n_unenrolled_control+n_unenrolled_treatment, df_cens_time, rec_time_planned)

    idx <- sample(seq_len(nrow(posterior_df)), 1)
    sampled_lambda_c      <- lambda_c_samples[idx]
    sampled_delay_time    <- delay_time_samples[idx]
    sampled_post_delay_HR <- post_delay_HR_samples[idx]

    if (control_distribution == "Exponential"){
      sampled_lambda_t <- sampled_lambda_c*sampled_post_delay_HR

      #For the unenrolled data, we can sample the remaining data according to the updated (sampled) parameters
      #First we do the control group
      unenrolled_control_times <- stats::rexp(n_unenrolled_control, sampled_lambda_c)

      #Now we do the treatment group
      CP <- exp(-(sampled_lambda_c*sampled_delay_time))
      u <- stats::runif(n_unenrolled_treatment)
      unenrolled_treatment_times <- ifelse(u > CP,
                                           (-log(u))/sampled_lambda_c,
                                           (1/sampled_lambda_t)*(sampled_delay_time*sampled_lambda_t-log(u)-sampled_delay_time*sampled_lambda_c))

    }

    if (control_distribution == "Weibull") {

      sampled_gamma_c <- gamma_c_samples[idx]

      u <- stats::runif(n_unenrolled_control)
      unenrolled_control_times <- (-log(u))^(1 / sampled_gamma_c) / sampled_lambda_c

      CP <- exp(-(sampled_lambda_c*sampled_delay_time)^sampled_gamma_c)
      u <- stats::runif(n_unenrolled_treatment)


      unenrolled_treatment_times <- ifelse(
        u > CP,
        (-log(u))^(1 / sampled_gamma_c) / sampled_lambda_c,
        (
          (-log(u) - (1 - sampled_post_delay_HR) * (sampled_lambda_c * sampled_delay_time)^sampled_gamma_c) /
            sampled_post_delay_HR
        )^(1 / sampled_gamma_c) / sampled_lambda_c
      )



    }




    #Now combine them together
    unenrolled_df <- data.frame(time = c(unenrolled_control_times, unenrolled_treatment_times),
                                group = c(rep("Control", n_unenrolled_control), rep("Treatment", n_unenrolled_treatment)),
                                rec_time = unenrolled_rec_times)

    unenrolled_df$pseudo_time <- unenrolled_df$time + unenrolled_df$rec_time



    #Extracting the observations that were censored at the IA
    censored_df <- data[data$status==0,]

    #Number of censored observations in each group
    n_censored_control <- sum(censored_df$group=="Control")
    n_censored_treatment <- sum(censored_df$group=="Treatment")

    #Extracting the censored observations in the control group
    control_censored_df <- censored_df %>%
      dplyr::filter(.data$group=="Control")


    if (control_distribution == "Exponential"){
      #Adding a exp(lambda_c) value to the censored value
      control_censored_df$final_time <- control_censored_df$survival_time + stats::rexp(n_censored_control, rate = sampled_lambda_c)
    }


    if (control_distribution == "Weibull"){

      V  <- stats::runif(n_censored_control)

      control_censored_df$final_time <- (
        ( (sampled_lambda_c * control_censored_df$survival_time)^sampled_gamma_c - log(V) )^(1 / sampled_gamma_c)
      ) / sampled_lambda_c


    }

    #Calculating the pseudo time
    control_censored_df$final_pseudo_time <- control_censored_df$rec_time + control_censored_df$final_time

    # Subset: treatment patients censored before the delay
    censored_treatment_before_delay <- censored_df %>%
      dplyr::filter(.data$group == "Treatment") %>%
      dplyr::filter(.data$survival_time <= sampled_delay_time)

    n_before <- nrow(censored_treatment_before_delay)

    if (n_before > 0) {

      if (control_distribution == "Exponential") {

        # ------- EXISTING EXPONENTIAL LOGIC (unchanged) -------

        # Conditional uniform U ~ Unif(0,1)
        u <- stats::runif(n_before)

        # Correct conditional probability that event occurs before sampled_delay_time
        p_before <- 1 - exp(-sampled_lambda_c * (sampled_delay_time - censored_treatment_before_delay$survival_time))

        # Early event time (conditional truncated exponential)
        t_event_before <- censored_treatment_before_delay$survival_time + (-log(1 - u * p_before)) / sampled_lambda_c

        # Late event time (conditional on surviving to sampled_delay_time)
        t_event_after <- sampled_delay_time +
          (-log(u) - sampled_lambda_c * (sampled_delay_time - censored_treatment_before_delay$survival_time)) / sampled_lambda_t

        # Branch: before vs after sampled_delay_time
        resid_times <- ifelse(
          u <= p_before,
          t_event_before - censored_treatment_before_delay$survival_time,   # residual time
          t_event_after - censored_treatment_before_delay$survival_time     # residual time
        )

      } else if (control_distribution == "Weibull") {

        # Control cumulative hazards at t0 and tau
        H_t0  <- (sampled_lambda_c * censored_treatment_before_delay$survival_time)^sampled_gamma_c
        H_tau <- (sampled_lambda_c * sampled_delay_time)^sampled_gamma_c

        # Survival at t0 and tau
        S_t0  <- exp(-H_t0)
        S_tau <- exp(-H_tau)

        # Probability event occurs between t0 and tau, conditional on T > t0
        # p_before = (S_t0 - S_tau) / S_t0 = 1 - exp(-(H_tau - H_t0))
        p_before <- 1 - exp(-(H_tau - H_t0))

        # Branch draw: which patients have event before vs after tau
        u_branch <- stats::runif(n_before)

        early_idx <- u_branch <= p_before
        late_idx  <- !early_idx

        resid_times <- numeric(n_before)

        ## --- EARLY BRANCH: t0 < T <= tau (truncated Weibull) ---
        if (any(early_idx)) {
          k <- sum(early_idx)

          v_early <- stats::runif(k)               # conditional position within [t0, tau]
          S_t0_e  <- S_t0[early_idx]

          S_t_e <- S_t0_e - v_early * (S_t0_e - S_tau)

          H_t_e <- -log(S_t_e)
          t_early <- (H_t_e)^(1 / sampled_gamma_c) / sampled_lambda_c

          resid_times[early_idx] <- t_early - censored_treatment_before_delay$survival_time[early_idx]
        }

        ## --- LATE BRANCH: T > tau ---
        if (any(late_idx)) {
          k <- sum(late_idx)


          v_late <- stats::runif(k)

          H_C_t <- H_tau - (log(v_late)) / sampled_post_delay_HR
          t_late <- (H_C_t)^(1 / sampled_gamma_c) / sampled_lambda_c

          resid_times[late_idx] <- t_late - censored_treatment_before_delay$survival_time[late_idx]
        }
      }

      # Store final times (common to both distributions)
      censored_treatment_before_delay$final_time <- censored_treatment_before_delay$survival_time + resid_times
      censored_treatment_before_delay$final_pseudo_time <-
        censored_treatment_before_delay$rec_time +
        censored_treatment_before_delay$final_time
    }



    # Extract censored treatment observations with t0 > tau
    censored_treatment_after_delay <- censored_df %>%
      dplyr::filter(.data$group == "Treatment") %>%
      dplyr::filter(.data$survival_time > sampled_delay_time)

    n_after <- nrow(censored_treatment_after_delay)

    if (n_after > 0) {


      if (control_distribution == "Exponential") {

        # existing exponential logic (unchanged)
        censored_treatment_after_delay$final_time <-
          censored_treatment_after_delay$survival_time + stats::rexp(n_after, rate = sampled_lambda_t)

      } else if (control_distribution == "Weibull") {


        # effective Weibull scale after delay
        sampled_lambda_t <- sampled_lambda_c * sampled_post_delay_HR^(1 / sampled_gamma_c)

        # conditional Weibull residual life
        V <- stats::runif(n_after)


        H_t0 <- (sampled_lambda_t * censored_treatment_after_delay$survival_time)^sampled_gamma_c

        T_after <- (H_t0 - log(V))^(1 / sampled_gamma_c) / sampled_lambda_t

        censored_treatment_after_delay$final_time <- T_after
      }

      censored_treatment_after_delay$final_pseudo_time <-
        censored_treatment_after_delay$rec_time +
        censored_treatment_after_delay$final_time
    }



    non_censored_df <- data %>%
      dplyr::filter(.data$status == 1)

    final_non_censored_df <- non_censored_df[,1:4]

    final_unenrolled_df <-
      if (nrow(unenrolled_df) > 0) {
        unenrolled_df[c("time", "group", "rec_time", "pseudo_time")]
      } else {
        data.frame(
          time        = numeric(0),
          group       = character(0),
          rec_time    = numeric(0),
          pseudo_time = numeric(0)
        )
      }

    final_control_censored_df <-
      if (nrow(control_censored_df) > 0) {
        control_censored_df[c("final_time", "group", "rec_time", "final_pseudo_time")]
      } else {
        data.frame(
          time        = numeric(0),
          group       = character(0),
          rec_time    = numeric(0),
          pseudo_time = numeric(0)
        )
      }


    final_control_censored_df <-
      if (nrow(control_censored_df) > 0) {
        control_censored_df[c("final_time", "group", "rec_time", "final_pseudo_time")]
      } else {
        data.frame(
          time        = numeric(0),
          group       = character(0),
          rec_time    = numeric(0),
          pseudo_time = numeric(0)
        )
      }

    final_censored_treatment_before_delay <-
      if (nrow(censored_treatment_before_delay) > 0) {
        censored_treatment_before_delay[c("final_time", "group", "rec_time", "final_pseudo_time")]
      } else {
        data.frame(
          time        = numeric(0),
          group       = character(0),
          rec_time    = numeric(0),
          pseudo_time = numeric(0)
        )
      }

    final_censored_treatment_after_delay <-
      if (nrow(censored_treatment_after_delay) > 0) {
        censored_treatment_after_delay[c("final_time", "group", "rec_time", "final_pseudo_time")]
      } else {
        data.frame(
          time        = numeric(0),
          group       = character(0),
          rec_time    = numeric(0),
          pseudo_time = numeric(0)
        )
      }

    colnames(final_control_censored_df) <- c("time", "group", "rec_time", "pseudo_time")
    colnames(final_censored_treatment_before_delay) <- c("time", "group", "rec_time", "pseudo_time")
    colnames(final_censored_treatment_after_delay) <- c("time", "group", "rec_time", "pseudo_time")

    final_df <- rbind(final_non_censored_df, final_unenrolled_df, final_control_censored_df, final_censored_treatment_before_delay, final_censored_treatment_after_delay)

    # --- Apply censoring ---
    if (censoring_model$method == "Time") {
      censored <- cens_data(final_df, cens_method = "Time", cens_time = censoring_model$time)
    } else if (censoring_model$method == "Events") {
      censored <- cens_data(final_df, cens_method = "Events", cens_events = censoring_model$events)
    } else if (censoring_model$method == "IF") {
      censored <- cens_data(final_df, cens_method = "IF", cens_IF = censoring_model$IF)
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


    BPP_df[j,] <- c(test_result$Signif, test_result$Z)

  }

  return(list(BPP_df = BPP_df))

}



#' Function to calculate the 'optimal' information fraction to calculate BPP
#'
#' @param n_c Number of control patients
#' @param n_t Number of treatment patients
#' @param control_model A named list specifying the control arm survival distribution:
#'   \itemize{
#'     \item \code{dist}: Distribution type ("Exponential" or "Weibull")
#'     \item \code{parameter_mode}: Either "Fixed" or "Distribution"
#'     \item \code{fixed_type}: If "Fixed", specify as "Parameters" or "Landmark"
#'     \item \code{lambda}, \code{gamma}: Scale and shape parameters
#'     \item \code{t1}, \code{t2}: Landmark times
#'     \item \code{surv_t1}, \code{surv_t2}: Survival probabilities at landmarks
#'     \item \code{t1_Beta_a}, \code{t1_Beta_b}, \code{diff_Beta_a}, \code{diff_Beta_b}: Beta prior parameters
#'   }
#' @param effect_model A named list specifying beliefs about the treatment effect:
#'   \itemize{
#'     \item \code{delay_SHELF}, \code{HR_SHELF}: SHELF objects encoding beliefs
#'     \item \code{delay_dist}, \code{HR_dist}: Distribution types ("hist" by default)
#'     \item \code{P_S}: Probability that survival curves separate
#'     \item \code{P_DTE}: Probability of delayed separation, conditional on separation
#'   }
#' @param recruitment_model A named list specifying the recruitment process:
#'   \itemize{
#'     \item \code{method}: "power" or "PWC"
#'     \item \code{period}, \code{power}: Parameters for power model
#'     \item \code{rate}, \code{duration}: Comma-separated strings for PWC model
#'   }
#' @param IA_model A named list specifying the censoring mechanism for the future data:
#'   \itemize{
#'     \item \code{events}: Number of events which is 100% information fraction
#'     \item \code{IF}: The information fraction at which to censor and calculate BPP
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level.
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param n_sims Number of data sets to simulate (default is 100).
#'
#' @return A vector of length `n_sims` corresponding to the value of BPP for each simulated trial
#'
#' @export
#'
calibrate_BPP_timing <- function(n_c, n_t,
                                 control_model,
                                 effect_model,
                                 recruitment_model,
                                 IA_model,
                                 analysis_model,
                                 n_sims = 50){

  outcome_list <- vector("list", length(IA_model$IF))

  for (i in 1:length(IA_model$IF)){

    result <- lapply(
      seq_len(n_sims),
      FUN = single_calibration_rep,
      n_c = n_c,
      n_t = n_t,
      control_model = control_model,
      effect_model = effect_model,
      recruitment_model = recruitment_model,
      total_events = IA_model$events,
      IF = IA_model$IF[i],
      analysis_model = analysis_model
    )


    BPP_values <- vapply(
      result,
      FUN = function(x) mean(x$BPP_outcome$BPP_df$success),
      FUN.VALUE = numeric(1)
    )

    cens_time <- vapply(result,
                        function (x) x$cens_time,
                        FUN.VALUE =numeric(1)
    )


    outcome_list[[i]]$BPP_values <- BPP_values
    outcome_list[[i]]$cens_time <- cens_time
  }



  return(list(outcome_list = outcome_list))

}


#' Function to calculate the 'optimal' BPP threshold value
#'
#' @param n_c Number of control patients
#' @param n_t Number of treatment patients
#' @param control_model A named list specifying the control arm survival distribution:
#'   \itemize{
#'     \item \code{dist}: Distribution type ("Exponential" or "Weibull")
#'     \item \code{parameter_mode}: Either "Fixed" or "Distribution"
#'     \item \code{fixed_type}: If "Fixed", specify as "Parameters" or "Landmark"
#'     \item \code{lambda}, \code{gamma}: Scale and shape parameters
#'     \item \code{t1}, \code{t2}: Landmark times
#'     \item \code{surv_t1}, \code{surv_t2}: Survival probabilities at landmarks
#'     \item \code{t1_Beta_a}, \code{t1_Beta_b}, \code{diff_Beta_a}, \code{diff_Beta_b}: Beta prior parameters
#'   }
#' @param effect_model A named list specifying beliefs about the treatment effect:
#'   \itemize{
#'     \item \code{delay_SHELF}, \code{HR_SHELF}: SHELF objects encoding beliefs
#'     \item \code{delay_dist}, \code{HR_dist}: Distribution types ("hist" by default)
#'     \item \code{P_S}: Probability that survival curves separate
#'     \item \code{P_DTE}: Probability of delayed separation, conditional on separation
#'   }
#' @param recruitment_model A named list specifying the recruitment process:
#'   \itemize{
#'     \item \code{method}: "power" or "PWC"
#'     \item \code{period}, \code{power}: Parameters for power model
#'     \item \code{rate}, \code{duration}: Comma-separated strings for PWC model
#'   }
#' @param IA_model A named list specifying the censoring mechanism for the future data:
#'   \itemize{
#'     \item \code{events}: Number of events which is 100% information fraction
#'     \item \code{IF}: The information fraction at which to censor and calculate BPP
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level.
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param data_generating_model A named list specifying the parameters for the data-generating mechanism
#'   \itemize{
#'     \item \code{lambda_c}: hazard rate for the control group
#'     \item \code{delay_time}: time at which the treatment starts to take effect
#'     \item \code{post_delay_HR}: hazard ratio, after `delay_time`
#'   }
#' @param n_sims Number of data sets to simulate (default is 100).
#'
#' @return A vector of length `n_sims` corresponding to the value of BPP for each simulated trial
#'
#' @export
#'

calibrate_BPP_threshold <- function(n_c,
                                     n_t,
                                     control_model,
                                     effect_model,
                                     recruitment_model,
                                     IA_model,
                                     analysis_model,
                                     data_generating_model,
                                     n_sims = 100){


  BPP_vec <- rep(NA, n_sims)


  ##### Now the fixed delay model
  for (i in 1:n_sims){

    # --- Simulate survival data ---
    data <- sim_dte(n_c, n_t,
                    data_generating_model$lambda_c,
                    delay_time = data_generating_model$delay_time,
                    post_delay_HR = data_generating_model$post_delay_HR,
                    dist = "Exponential")

    # --- Add recruitment time ---
    data <- add_recruitment_time(data,
                                 rec_method = recruitment_model$method,
                                 rec_period = recruitment_model$period,
                                 rec_power = recruitment_model$power,
                                 rec_rate = recruitment_model$rate,
                                 rec_duration = recruitment_model$duration)


    censored_data <- cens_data(data, cens_method = "Events", cens_events = IA_model$events*IA_model$IF)

    data <- censored_data$data

    posterior_samples <- DTEAssurance::update_priors(data,
                                                     control_model = control_model,
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

    BPP_vec[i] <- mean(BPP_outcome$BPP_df$success)


  }

  return(list(BPP_vec = BPP_vec))
}














