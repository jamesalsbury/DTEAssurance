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
#'   \item{sample_size}{Number of subjects remaining after censoring}
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

  return(list(Signif = Signif, observed_HR = observed_HR))

}

#' Add recruitment time to a survival dataset
#'
#' Simulates recruitment timing for each subject in a survival dataset using either a power model or a piecewise constant (PWC) model. The function appends recruitment times and pseudo survival times (time from recruitment to event or censoring).
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
#'   \item{rec_time}{Simulated recruitment time for each subject}
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
#'     \item \code{beta_spending}: Cumulative beta spending vector
#'     \item \code{IF_vec}: Vector of information fractions
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
#' GSD_model <- list(events = 300, alpha_spending = c("0.01, 0.025"),
#'                   beta_spending = c("0.05, 0.1"), IF_vec = c("0.5, 1"))
#' result <- calc_dte_assurance_interim(n_c = 300, n_t = 300,
#'                         control_model = control_model,
#'                         effect_model = effect_model,
#'                         recruitment_model = recruitment_model,
#'                         GSD_model = GSD_model,
#'                         n_sims = 10)
#' str(result)
#'
#' @export


calc_dte_assurance_interim <- function(n_c, n_t,
                                       control_model,
                                       effect_model,
                                       recruitment_model,
                                       GSD_model,
                                       n_sims = 1000) {
  IF_list   <- lapply(GSD_model$IF_vec, function(x) as.numeric(strsplit(x, ",\\s*")[[1]]))
  alpha_list <- lapply(GSD_model$alpha_spending, function(x) as.numeric(strsplit(x, ",\\s*")[[1]]))
  beta_list <- lapply(GSD_model$beta_spending, function(x) as.numeric(strsplit(x, ",\\s*")[[1]]))
  IF_labels <- GSD_model$IF_vec

  results <- future.apply::future_lapply(seq_len(n_sims), function(i) {
    trial <- simulate_trial_with_recruitment(n_c, n_t, control_model, effect_model, recruitment_model)

    #Pretend we are contuining to the end
    trial_data <- trial[order(trial$pseudo_time),]
    n_events <- GSD_model$events
    t_interim <- trial_data$pseudo_time[n_events]

    eligible_df <- trial_data %>%
      dplyr::filter(.data$rec_time <= t_interim)


    # Censoring logic
    eligible_df$status <- eligible_df$pseudo_time < t_interim
    eligible_df$survival_time <- ifelse(eligible_df$status, eligible_df$time, t_interim - eligible_df$rec_time)

    fit     <- survival::coxph(Surv(survival_time, status) ~ group, data = eligible_df)
    fit_summary <- summary(fit)
    z_stat <- -fit_summary$coefficients[, "z"]


    do.call(rbind, lapply(seq_along(IF_list), function(j) {
      design <- rpact::getDesignGroupSequential(
        typeOfDesign = "asUser",
        informationRates = IF_list[[j]],
        userAlphaSpending = alpha_list[[j]],
        typeBetaSpending = "bsUser",
        userBetaSpending = beta_list[[j]]
      )

      outcome <- apply_GSD_to_trial(trial, design, GSD_model$events)


      data.frame(
        Trial = i,
        IF = IF_labels[j],
        Decision = outcome$decision,
        StopTime = outcome$stop_time,
        SampleSize = outcome$sample_size,
        Final_Decision = ifelse(z_stat > stats::qnorm(1-0.025), "Successful", "Unsuccessful")
      )
    }))
  }, future.seed = TRUE)

  results_df <- do.call(rbind, results)


  return(results_df)
}



