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
#' This is the single routine used throughout the package to cut a trial at
#' an analysis. The cut is made at calendar time \code{t}, the calendar time
#' of the \code{k}-th event (\code{"Events"}); patients recruited before
#' \code{t} are included, and a patient has an event if
#' \code{pseudo_time <= t}. Cutting at \code{k} events therefore leaves
#' exactly \code{k} events in the data (assuming no tied event times).
#' \code{cens_events} is floored with a small tolerance, so a non-integer
#' such as \code{840 * 0.3} is treated as 252.
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
    cens_events <- as.integer(floor(cens_events + 1e-8))
    if (cens_events < 1) stop("'cens_events' must be at least 1")
    if (cens_events > nrow(data)) stop("'cens_events' exceeds number of observations")
    cens_time <- data$pseudo_time[cens_events]
  }

  if (cens_method == "IF") {
    if (is.null(cens_IF)) stop("Please specify 'cens_IF' for method 'IF'")
    index <- floor(nrow(data) * round(cens_IF, 6) + 1e-8)
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
#' set.seed(123)
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
#' Performs a survival analysis using the standard log-rank test (LRT), a
#' weighted log-rank test in the Fleming-Harrington family (WLRT), or the
#' modestly-weighted log-rank test of Magirr and Burman (MW), and determines
#' whether the result is statistically significant at the specified alpha
#' level and alternative hypothesis.
#'
#' @param data A dataframe containing survival data. Must include columns for survival time, event status, and treatment group.
#' @param analysis_method Method of analysis: \code{"LRT"} (default) for standard log-rank test, \code{"WLRT"} for a Fleming-Harrington-family weighted log-rank test, or \code{"MW"} for the modestly-weighted log-rank test.
#' @param alternative String specifying the alternative hypothesis: \code{"one.sided"} (default) or \code{"two.sided"}.
#' @param alpha Type I error level for the significance test (required, no
#'   default). Pass \code{NULL} if only \code{Z} is needed; \code{Signif} is
#'   then \code{NA}.
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test.
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test.
#' @param t_star Parameter \eqn{t^*} used in the modestly weighted test.
#' @param s_star Parameter \eqn{s^*} used in the modestly weighted test.
#' @param return_HR If \code{TRUE} (default), fit a Cox model and return the
#'   estimated hazard ratio as \code{observed_HR}. If \code{FALSE}, the Cox
#'   fit is skipped (it does not affect \code{Signif} or \code{Z}) and
#'   \code{observed_HR} is \code{NA}. The group-sequential and predictive
#'   probability code uses \code{FALSE}.
#'
#' @return A list containing:
#' \describe{
#'   \item{Signif}{Logical: whether the test is significant at level
#'     \code{alpha}. \code{FALSE} if \code{Z} is \code{NA}; \code{NA} if
#'     \code{alpha} is \code{NULL}.}
#'   \item{observed_HR}{Estimated hazard ratio from a Cox proportional hazards
#'     model, or \code{NA} if \code{return_HR = FALSE}.}
#'   \item{Z}{Signed test statistic. For all three methods, positive values
#'     favour the arm coded as "Treatment" (the second factor level of
#'     \code{group}), i.e. positive Z corresponds to a hazard ratio below 1.}
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
survival_test <- function(data, analysis_method = "LRT", alternative = "one.sided",
                          alpha, rho = 0, gamma = 0,
                          t_star = NULL, s_star = NULL, return_HR = TRUE){

  if (missing(alpha)) {
    stop("survival_test: 'alpha' must be supplied (use alpha = NULL if only Z is needed).")
  }

  observed_HR <- NA_real_
  if (return_HR) {
    coxmodel <- survival::coxph(Surv(survival_time, status) ~ group, data = data)
    observed_HR <- as.numeric(exp(stats::coef(coxmodel)))
  }

  Z <- NA_real_

  if (analysis_method == "LRT") {
    test_result <- survival::survdiff(Surv(survival_time, status) ~ group, data = data)
    Z <- (test_result$exp[2] - test_result$obs[2]) / sqrt(test_result$var[2, 2])
  } else if (analysis_method == "WLRT") {
    test <- nph::logrank.test(data$survival_time, data$status, data$group,
                              rho = rho, gamma = gamma)
    # nph::logrank.test()'s z already has positive Z = benefit.
    Z <- test$test$z
  } else if (analysis_method == "MW") {
    test <- nphRCT::wlrt(Surv(survival_time, status) ~ group,
                         data = data, method = "mw",
                         t_star = t_star, s_star = s_star)
    # nphRCT::wlrt() has the opposite sign convention; negate so positive Z = benefit.
    Z <- -test$z
  }
  Z <- unname(Z)

  Signif <- if (is.null(alpha)) {
    NA
  } else if (is.na(Z)) {
    FALSE
  } else if (alternative == "one.sided") {
    Z > stats::qnorm(1 - alpha)
  } else {
    abs(Z) > stats::qnorm(1 - alpha/2)
  }

  return(list(Signif = as.logical(Signif), observed_HR = observed_HR, Z = Z))
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
#'     \item \code{alpha_spending}: User-specified \emph{cumulative} alpha
#'       spent by each efficacy look (one value per element of
#'       \code{alpha_IF}; the design uses \code{rpact}'s
#'       \code{typeOfDesign = "asUser"}). Futility looks are added to the
#'       design with zero alpha spent, so they do not change the efficacy
#'       critical values.
#'     \item \code{alpha_IF}: Information Fraction(s) at which we look for efficacy
#'     \item \code{futility_type}: One of \code{"none"}, \code{"Beta"}
#'       (pre-specified beta-spending, via \code{rpact}), \code{"PP"}
#'       (predictive probability futility, D3-style), or
#'       \code{"MatchedZ"} (a fixed, externally-calibrated Z-statistic
#'       cutoff, D4/D5-style; the rule is applied as binding, i.e. the
#'       simulated trial stops when Z falls below the cutoff; see
#'       \code{\link{calibrate_matched_futility_boundary}} for how to
#'       obtain \code{futility_boundary_Z}).
#'     \item \code{futility_IF}: Information Fraction at which we look for futility
#'       (required for \code{"PP"} and \code{"MatchedZ"}).
#'     \item \code{beta_spending}: Cumulative beta spending vector (\code{"Beta"} only).
#'     \item \code{kappa}: PP value below which we stop for futility (\code{"PP"} only).
#'     \item \code{futility_boundary_Z}: Z-statistic value below which we
#'       stop for futility (\code{"MatchedZ"} only).
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level.
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param update_priors_sims Number of posterior samples per chain for
#'   each interim dataset, passed to \code{\link{update_priors}}. Required
#'   when \code{GSD_model$futility_type == "PP"}; ignored otherwise.
#' @param PP_sims Number of predictive simulations per interim dataset,
#'   passed to \code{\link{PP_func}}. Required when
#'   \code{GSD_model$futility_type == "PP"}; ignored otherwise.
#' @param n_sims Number of simulations to run (default = 1000)
#'
#' @return A data frame with one row per simulated trial and the following columns:
#' \describe{
#'   \item{Trial}{Simulation index}
#'   \item{Decision}{Final interim/final decision outcome -- one of
#'     \code{"Stop for efficacy"}, \code{"Stop for futility"},
#'     \code{"Successful at final"}, or \code{"Unsuccessful at final"}.
#'     This is the single source of truth for trial outcome; use it
#'     directly rather than deriving success/failure independently.}
#'   \item{StopTime}{Time at which the trial stopped or completed}
#'   \item{SampleSize}{Total sample size at the time of decision}
#'   \item{Success}{Logical recode of \code{Decision} for convenience:
#'     \code{TRUE} if \code{Decision \%in\% c("Stop for efficacy",
#'     "Successful at final")}, \code{FALSE} otherwise. Derived directly
#'     and only from \code{Decision} -- see "Bug fix" below.}
#'   \item{Converged}{For \code{"PP"} designs, whether the interim MCMC
#'     fit converged (see \code{\link{update_priors}}); \code{NA} for
#'     other futility types, which involve no MCMC step.}
#'   \item{PP_val}{For \code{"PP"} designs, the predictive probability at
#'     the futility look; \code{NA} otherwise (or if the trial stopped for
#'     efficacy before the futility look).}
#'   \item{P_Z1, P_Z2, P_Z3}{For \code{"PP"} designs, the posterior
#'     probabilities of the three latent states at the futility look;
#'     \code{NA} otherwise.}
#' }
#' Class: \code{data.frame}, with attribute \code{settings}: a list of all
#' arguments, the package version, R version, git commit (if available) and
#' a timestamp.
#'
#' @section Bug fix (this version): previous versions of this function
#'   independently recomputed a separate \code{Final_Decision} field from
#'   a hardcoded Cox proportional-hazards Wald statistic at a flat
#'   \code{qnorm(0.975)} threshold, regardless of \code{analysis_model$method}
#'   or the design's actual group-sequential boundaries. This was a
#'   second, separate copy of the same bug fixed in
#'   \code{apply_GSD_to_trial()} (see its documentation), and could
#'   silently disagree with the trial's own \code{Decision}. This version
#'   removes that duplicate computation entirely: \code{Success} is now
#'   derived only from \code{Decision}, which is itself computed once,
#'   correctly, inside \code{apply_GSD_to_trial()}, via
#'   \code{analysis_model$method} and the design's real boundaries. This
#'   also collapses what were previously three near-duplicate branches
#'   (one per futility type) into a single call path, since
#'   \code{apply_GSD_to_trial()} already dispatches correctly on
#'   \code{GSD_model$futility_type} -- removing the code duplication that
#'   allowed the two copies of the bug to drift apart in the first place.
#'
#' @examples
#' set.seed(123)
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
                                        update_priors_sims = NULL,
                                        PP_sims = NULL,
                                        n_sims = 1000) {

  GSD_model <- normalise_futility_spec(GSD_model)

  if (identical(GSD_model$futility_type, "PP") &&
      (is.null(update_priors_sims) || is.null(PP_sims))) {
    stop("calc_dte_assurance_adaptive: 'update_priors_sims' and 'PP_sims' ",
         "must be supplied when GSD_model$futility_type is \"PP\".", call. = FALSE)
  }

  if (is.null(GSD_model$futility_type) ||
      !GSD_model$futility_type %in% c("none", "Beta", "PP", "MatchedZ")) {
    stop("GSD_model$futility_type must be one of 'none', 'Beta', 'PP', or 'MatchedZ'.")
  }

  if (GSD_model$futility_type == "PP" &&
      !identical(control_model$parameter_mode, "Distribution")) {
    stop(
      "Invalid specification: when `GSD_model$futility_type` is \"PP\", ",
      "`control_model$parameter_mode` must be \"Distribution\"."
    )
  }

  if (GSD_model$futility_type == "MatchedZ" &&
      (is.null(GSD_model$futility_boundary_Z) || is.null(GSD_model$futility_IF))) {
    stop(
      "Invalid specification: when `GSD_model$futility_type` is ",
      "\"MatchedZ\", `GSD_model$futility_boundary_Z` and ",
      "`GSD_model$futility_IF` must both be specified -- see ",
      "calibrate_matched_futility_boundary() for how to obtain these."
    )
  }

  if (is.null(analysis_model)) {
    analysis_model <- list(method = "LRT", alpha = 0.025,
                           alternative_hypothesis = "one.sided")
  }

  rpact_design <- make_rpact_design_from_GSD_model(GSD_model)
  design       <- rpact_design$design

  results <- future.apply::future_lapply(seq_len(n_sims), function(i) {

    trial <- simulate_trial_with_recruitment(
      n_c, n_t, control_model, effect_model, recruitment_model
    )

    # --- FIX: single, unified call to apply_GSD_to_trial() for every
    # futility_type. apply_GSD_to_trial() already dispatches correctly on
    # GSD_model$futility_type internally (none/Beta -> boundary-only;
    # PP -> posterior-predictive futility; MatchedZ -> fixed calibrated
    # Z-cutoff futility), so no branching is needed here. control_model/
    # effect_model/recruitment_model/update_priors_sims/PP_sims are
    # only actually used inside apply_GSD_to_trial()'s PP branch; passing
    # them unconditionally is harmless for other futility types. ---
    outcome <- apply_GSD_to_trial(
      n_c = n_c, n_t = n_t,
      trial_data          = trial,
      design              = design,
      total_events        = GSD_model$events,
      GSD_model           = GSD_model,
      control_model       = control_model,
      effect_model        = effect_model,
      recruitment_model   = recruitment_model,
      analysis_model      = analysis_model,
      update_priors_sims  = update_priors_sims,
      PP_sims             = PP_sims
    )

    # --- FIX: Success is now derived ONLY from outcome$decision (the
    # single source of truth, computed once inside apply_GSD_to_trial()
    # using analysis_model$method and the design's real boundaries). The
    # previous, separate, independently-computed Final_Decision field
    # (hardcoded Cox Wald statistic vs. a flat qnorm(0.975) threshold,
    # ignoring analysis_model$method and the design's actual boundaries)
    # has been removed entirely -- it was a second copy of the same bug
    # fixed in apply_GSD_to_trial(), and could silently disagree with
    # Decision. ---
    data.frame(
      Trial      = i,
      Decision   = outcome$decision,
      StopTime   = outcome$stop_time,
      SampleSize = outcome$sample_size,
      Success    = outcome$decision %in% c("Stop for efficacy", "Successful at final"),
      Converged  = if (!is.null(outcome$converged)) outcome$converged else NA,
      PP_val     = outcome$PP_val,
      P_Z1       = unname(outcome$Z_probs["P_Z1"]),
      P_Z2       = unname(outcome$Z_probs["P_Z2"]),
      P_Z3       = unname(outcome$Z_probs["P_Z3"])
    )

  }, future.seed = TRUE)

  out <- do.call(rbind, results)
  attr(out, "settings") <- c(
    list(n_c = n_c, n_t = n_t,
         control_model = control_model, effect_model = effect_model,
         recruitment_model = recruitment_model, GSD_model = GSD_model,
         analysis_model = analysis_model,
         update_priors_sims = update_priors_sims, PP_sims = PP_sims,
         n_sims = n_sims),
    run_provenance()
  )
  out
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
#' @param n.chains Number of MCMC chains to run (default is 2)
#' @param n_burnin Number of burn-in samples for the MCMC chain(s) (default is 500)
#' @param n_samples Number of posterior samples retained \strong{per chain}
#'   after burn-in (default is 1000). The returned data frame has
#'   \code{n.chains * n_samples} rows (2,000 with the defaults).
#' @param rhat_threshold Convergence threshold on the Gelman-Rubin
#'   potential scale reduction factor (default 1.1). Used only to compute
#'   the \code{"converged"} attribute on the return value; does not affect
#'   sampling.
#' @param jags_seed Optional integer seed for JAGS. If supplied, chain
#'   \code{ch} is initialised with the \code{"base::Wichmann-Hill"} RNG and
#'   seed \code{jags_seed + ch}, so repeated calls with the same
#'   \code{jags_seed} return identical draws. If \code{NULL} (default), JAGS
#'   seeds its chains itself and the draws are not reproducible with
#'   \code{set.seed()}.
#'
#' @return A data frame containing Monte Carlo samples from the updated
#'   (posterior) distribution of the model parameters, with columns
#'   \code{lambda_c}, \code{delay_time}, \code{HR}, \code{gamma_c} (Weibull
#'   only), and \code{Z} (the latent scenario indicator: 1 = no separation,
#'   2 = immediate separation, 3 = delayed separation). Column access
#'   (\code{posterior_df$lambda_c}, etc.) is unchanged from previous
#'   versions of this function -- existing calling code does not need to be
#'   modified. In addition, three attributes are attached to the returned
#'   data frame for diagnostic purposes:
#'   \describe{
#'     \item{\code{rhat}}{A named numeric vector of per-parameter
#'       Gelman-Rubin point estimates (accessed via
#'       \code{attr(posterior_df, "rhat")}).}
#'     \item{\code{converged}}{\code{TRUE} if every \emph{finite} R-hat
#'       is below \code{rhat_threshold}, \code{FALSE} if not, or \code{NA}
#'       if there are no finite R-hat values (or the diagnostic could not be
#'       computed). Non-finite values (\code{NaN} arises for parameters that
#'       are constant across draws, e.g. \code{delay_time} when \code{Z != 3}
#'       throughout) are not treated as failures; they are counted in
#'       \code{n_nonfinite_rhat}.}
#'     \item{\code{rhat_max}}{The largest finite R-hat (\code{NA} if none).}
#'     \item{\code{n_nonfinite_rhat}}{The number of non-finite R-hat values.}
#'     \item{\code{n_retained_total}}{The number of retained draws over all
#'       chains, \code{n.chains * n_samples}.}
#'     \item{\code{Z_probs}}{A named numeric vector
#'       \code{c(P_Z1=, P_Z2=, P_Z3=)}, the posterior probability of each
#'       latent state, i.e. \code{table(posterior_df$Z) / nrow(posterior_df)}
#'       (accessed via \code{attr(posterior_df, "Z_probs")}).}
#'   }
#'
#' Priors for \code{lambda_c}, \code{T}, and \code{HR} are constructed from
#' elicited distributions using the SHELF framework, then updated through
#' sampling-based posterior inference.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' interim_data = data.frame(survival_time = runif(10, min = 0, max = 10),
#' status = rbinom(10, size = 1, prob = 0.5),
#' group = c(rep("Control", 5), rep("Treatment", 5)))
#' control_model = list(dist = "Exponential",
#'                      parameter_mode = "Distribution",
#'                      t1 = 12,
#'                      t1_Beta_a = 20,
#'                      t1_Beta_b = 32)
#'
#' effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
#'                     delay_dist = "gamma",
#'                     HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
#'                     HR_dist = "gamma",
#'                     P_S = 1,
#'                     P_DTE = 0)
#'
#' posterior_df <- update_priors(
#'   data = interim_data,
#'   control_model = control_model,
#'   effect_model = effect_model,
#'   n_samples = 10)
#'
#' # Diagnostics, e.g.:
#' attr(posterior_df, "rhat")
#' attr(posterior_df, "converged")
#' attr(posterior_df, "Z_probs")
#'
update_priors <- function(data,
                          control_model,
                          effect_model,
                          n.chains = 2,
                          n_burnin = 500,
                          n_samples = 1000,
                          rhat_threshold = 1.1,
                          jags_seed = NULL) {

  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("This function requires the 'rjags' package. Please install it with install.packages('rjags').")
  }

  if (control_model$dist == "Exponential"){

    control_jags <- paste0(
      "s1 ~ dbeta(", control_model$t1_Beta_a, ", ", control_model$t1_Beta_b, ")\n",
      "lambda_c <- -log(s1)/t1\n"
    )

  }

  if (control_model$dist == "Weibull"){


    control_jags <- paste0(
      "s1 ~ dbeta(", control_model$t1_Beta_a, ", ", control_model$t1_Beta_b, ")\n",
      "delta ~ dbeta(", control_model$diff_Beta_a, ", ", control_model$diff_Beta_b, ")\n",
      "gamma_c <- log(log(s1) / log(s1 - delta)) / log(t1 / t2)\n",
      "lambda_c <- (-log(s1))^(1 / gamma_c) / t1\n"
    )


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
    l[i] <- ifelse(data_event[i]==1, ifelse(data_time[i]<delay_time, log(lambda_c)-(lambda_c*data_time[i]), log(lambda_e)-lambda_e*(data_time[i]-delay_time)-(delay_time*lambda_c)),
      ifelse(data_time[i]<delay_time, -(lambda_c*data_time[i]), -(lambda_c*delay_time)-lambda_e*(data_time[i]-delay_time)))
  }



  Z ~ dcat(pi[])

  HR_slab    ~ ", HR_jags, "
  delay_slab ~ ", delay_jags, "
  HR <- equals(Z, 1) * 1
        + (1 - equals(Z, 1)) * HR_slab

  delay_time <- equals(Z, 3) * delay_slab
  ", control_jags, "
  lambda_e <- lambda_c * HR

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
    l[i] <- ifelse(data_event[i]==1, ifelse(data_time[i]<delay_time, log(gamma_c)+gamma_c*log(lambda_c*data_time[i])-(lambda_c*data_time[i])^gamma_c-log(data_time[i]), log(gamma_c)+gamma_c*log(lambda_e)+(gamma_c-1)*log(data_time[i])-lambda_e^gamma_c*(data_time[i]^gamma_c-delay_time^gamma_c)-(delay_time*lambda_c)^gamma_c),
      ifelse(data_time[i]<delay_time, -(lambda_c*data_time[i])^gamma_c, -(lambda_c*delay_time)^gamma_c-lambda_e^gamma_c*(data_time[i]^gamma_c-delay_time^gamma_c)))
  }





  Z ~ dcat(pi[])

  HR_slab    ~ ", HR_jags, "
  delay_slab ~ ", delay_jags, "
  HR <- equals(Z, 1) * 1
        + (1 - equals(Z, 1)) * HR_slab

  delay_time <- equals(Z, 3) * delay_slab

  ", control_jags, "
  lambda_e <- lambda_c*pow(HR, 1/gamma_c)

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


# Seeds stay within the valid integer range even if jags_seed is close to
# .Machine$integer.max (identical to jags_seed + ch otherwise).
inits <- if (is.null(jags_seed)) {
  NULL
} else {
  lapply(seq_len(n.chains), function(ch)
    list(.RNG.name = "base::Wichmann-Hill",
         .RNG.seed = as.integer((jags_seed + ch - 1) %% (.Machine$integer.max - 1) + 1)))
}

model = rjags::jags.model(
  textConnection(modelstring),
  data = data_list,
  inits = inits,
  n.chains = n.chains,
  quiet = TRUE
)

# --- FIX: Z is now monitored alongside the continuous parameters, giving
#     direct access to the posterior state probabilities P(Z=k | data). ---
var_names <- if (control_model$dist == "Exponential") {
  c("lambda_c", "HR", "delay_time", "Z")
} else {
  c("lambda_c", "gamma_c", "HR", "delay_time", "Z")
}

stats::update(model, n.iter = n_burnin)

output <- rjags::coda.samples(
  model = model,
  variable.names = var_names,
  n.iter = n_samples
)

# --- FIX: convergence diagnostic (per-parameter Gelman-Rubin Rhat),
#     computed here so it can be aggregated across a large simulation
#     study without needing to re-run anything. Wrapped in tryCatch since
#     this will be called many thousands of times in a full simulation
#     study, and an occasional numerical failure (e.g. a near-zero-variance
#     fit) should not halt the study -- it is instead recorded as NA. ---
conv_diag <- tryCatch({
  gd <- coda::gelman.diag(output, autoburnin = FALSE, multivariate = FALSE)
  psrf_vec <- gd$psrf[, "Point est."]
  names(psrf_vec) <- rownames(gd$psrf)
  c(list(rhat = psrf_vec, error = NA_character_),
    rhat_summary(psrf_vec, rhat_threshold))
}, error = function(e) {
  rhat <- stats::setNames(rep(NA_real_, length(var_names)), var_names)
  c(list(rhat = rhat, error = conditionMessage(e)),
    rhat_summary(rhat, rhat_threshold))
})

posterior_df <- as.data.frame(as.matrix(output))

# --- FIX: posterior state probabilities, directly from the monitored Z
#     column (now present in posterior_df since Z is in var_names). ---
Z_tab <- table(factor(posterior_df$Z, levels = c(1, 2, 3)))
Z_probs <- as.numeric(Z_tab) / sum(Z_tab)
names(Z_probs) <- c("P_Z1", "P_Z2", "P_Z3")

attr(posterior_df, "rhat") <- conv_diag$rhat
attr(posterior_df, "converged") <- conv_diag$converged
attr(posterior_df, "rhat_max") <- conv_diag$rhat_max
attr(posterior_df, "n_nonfinite_rhat") <- conv_diag$n_nonfinite_rhat
attr(posterior_df, "n_retained_total") <- nrow(posterior_df)
attr(posterior_df, "convergence_error") <- conv_diag$error
attr(posterior_df, "Z_probs") <- Z_probs

return(posterior_df)

}


#' Calculate the predictive probability (PP) given interim data and posterior samples
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
#'   the delay (changepoint) time and the post-delay hazard ratio, respectively,
#'   plus \code{gamma_c} (the Weibull shape) when
#'   \code{control_distribution = "Weibull"}.
#' @param control_distribution Distributional form assumed for the control arm:
#'   either \code{"Exponential"} (default) or \code{"Weibull"}.
#' @param n_c_planned Planned maximum number of patients in the control group.
#' @param n_t_planned Planned maximum number of patients in the treatment group.
#' @param rec_time_planned Planned maximum recruitment calendar time for the full trial.
#' @param df_cens_time Calendar time at which \code{df} has been censored (interim analysis time).
#' @param analysis_model A named list specifying the analysis method and decision rule:
#'   \itemize{
#'     \item \code{method}: e.g. \code{"LRT"}, \code{"WLRT"}, or \code{"MW"}.
#'     \item \code{alpha}: one-sided type I error level (only used in the legacy
#'       fallback path, see \code{censoring_model} below).
#'     \item \code{alternative_hypothesis}: direction of the alternative (e.g. \code{"one.sided"}).
#'     \item \code{rho}, \code{gamma}, \code{t_star}, \code{s_star}: additional parameters for WLRT or MW (if applicable).
#'   }
#' @param future_boundaries \strong{Recommended.} A list of future, pre-specified,
#'   fixed decision points to evaluate on each posterior-predictive draw, in
#'   ascending chronological order, matching the trial's own group-sequential
#'   design. Each element is a list with:
#'   \itemize{
#'     \item \code{events}: the cumulative event count at that analysis (the
#'       final element should be the maximum planned event count).
#'     \item \code{crit}: the pre-specified critical value for the test
#'       statistic \code{Z} at that analysis (e.g. from
#'       \code{design$criticalValues} of an \code{rpact} group-sequential
#'       design).
#'   }
#'   On each predictive draw, boundaries are checked in order: if \code{Z}
#'   crosses a boundary, the draw is recorded as successful and evaluation
#'   stops (mirroring the group-sequential design's own early-stopping
#'   logic); if the final boundary in the chain is reached without crossing,
#'   the draw is recorded as unsuccessful. This correctly evaluates the
#'   adaptive trial-success event (crossing any future efficacy boundary, or
#'   rejecting \eqn{H_0} at the final analysis), rather than only checking a
#'   single final test.
#'
#'   \strong{Scope:} \code{future_boundaries} must contain only fixed
#'   (non-Bayesian) decision points. A design with more than one future
#'   Bayesian-predictive-probability futility look is not supported here, as
#'   it would require nested posterior-predictive simulation at each look;
#'   see the package/manuscript Limitations.
#' @param censoring_model \strong{Legacy fallback, deprecated.} Used only if
#'   \code{future_boundaries} is \code{NULL}. A named list specifying a
#'   single censoring mechanism for the future data (\code{method}: one of
#'   \code{"Time"}, \code{"Events"}, or \code{"IF"}; plus the corresponding
#'   \code{time}/\code{events}/\code{IF} parameter), with success determined
#'   by \code{analysis_model$alpha} rather than a design-specific critical
#'   value. Retained only for backward compatibility with callers that do
#'   not have access to a full group-sequential design object; new code
#'   should always supply \code{future_boundaries}.
#' @param n_sims Number of posterior-predictive draws (required, no
#'   default).
#'
#' @details
#' \strong{Model.} The control arm is Weibull with rate \code{lambda_c} and
#' shape \code{gamma_c}; an exponential control arm
#' (\code{control_distribution = "Exponential"}) is handled as the Weibull
#' case with \code{gamma_c = 1}, using the same code path. The treatment arm
#' follows the control hazard up to \code{delay_time} and the control hazard
#' multiplied by \code{HR} afterwards. Each predictive draw samples one row
#' of \code{posterior_df}, simulates event times for patients not yet
#' recruited, and simulates residual event times for patients censored at
#' the interim, conditional on their follow-up so far.
#'
#' \strong{Recruitment assumption.} Recruitment times for patients not yet
#' recruited at the interim are drawn from
#' \code{Uniform(df_cens_time, rec_time_planned)}. This is correct for
#' uniform recruitment (the \code{"power"} recruitment model with
#' \code{power = 1}) only; the simulation functions that call
#' \code{PP_func()} stop if another recruitment model is supplied.
#'
#' @return A list with a single element \code{PP_df}, a data frame with
#'   columns \code{success} (0/1, whether this predictive draw ultimately
#'   rejects \eqn{H_0}, accounting for any future fixed efficacy boundaries)
#'   and \code{Z_val} (the test statistic at the analysis where the draw was
#'   decided). The predictive probability is \code{mean(PP_df$success)}.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 30
#' cens_time <- 15
#'
#' time <- runif(n, 0, 12)
#' rec_time <- runif(n, 0, 12)
#'
#' df <- data.frame(
#'   time = time,
#'   group = c(rep("Control", n/2), rep("Treatment", n/2)),
#'   rec_time = rec_time
#' )
#'
#' df$pseudo_time <- df$time + df$rec_time
#' df$status <- df$pseudo_time < cens_time
#' df$survival_time <- ifelse(df$status == TRUE, df$time, cens_time - df$rec_time)
#'
#' posterior_df <- data.frame(HR = rnorm(20, mean = 0.75, sd = 0.05),
#'                            delay_time = rep(0, 20),
#'                            lambda_c = rnorm(20, log(2)/9, sd = 0.01))
#'
#' analysis_model <- list(method = "LRT", alpha = 0.025,
#'                        alternative_hypothesis = "one.sided")
#'
#' # Recommended usage: pass the design's own future boundaries, e.g. a single
#' # efficacy look at 20 events (Z > 2.24) followed by a final analysis at
#' # 28 events (Z > 2.00):
#' future_boundaries <- list(
#'   list(events = 20, crit = 2.24),
#'   list(events = 28, crit = 2.00)
#' )
#'
#' PP_outcome <- PP_func(df, posterior_df,
#'            control_distribution = "Exponential",
#'            n_c_planned = n/2, n_t_planned = n/2,
#'            rec_time_planned = 12, df_cens_time = 15,
#'            analysis_model = analysis_model,
#'            future_boundaries = future_boundaries,
#'            n_sims = 10)
#'
PP_func <- function(data, posterior_df, control_distribution = "Exponential", n_c_planned, n_t_planned,
                     rec_time_planned, df_cens_time,
                     analysis_model,
                     censoring_model = NULL,
                     future_boundaries = NULL,
                     n_sims) {

  if (missing(n_sims) || is.null(n_sims)) {
    stop("PP_func: 'n_sims' (the number of posterior-predictive draws) must be ",
         "supplied explicitly; there is no default.")
  }

  if (is.null(future_boundaries) && is.null(censoring_model)) {
    stop("PP_func: supply either 'future_boundaries' (recommended -- a chain ",
         "of fixed future efficacy/final decision points matching the trial's ",
         "group-sequential design) or 'censoring_model' (legacy single-stage ",
         "fallback, uses a flat alpha rather than design-specific critical ",
         "values).")
  }

  #The number of unenrolled patients in each group
  n_unenrolled_control <- (n_c_planned) - sum(data$group=="Control")
  n_unenrolled_treatment <- (n_t_planned) - sum(data$group=="Treatment")

  #Extract realisations from the MCMC
  lambda_c_samples <- posterior_df$lambda_c
  delay_time_samples <- posterior_df$delay_time
  post_delay_HR_samples <- posterior_df$HR

  if (!control_distribution %in% c("Exponential", "Weibull")) {
    stop("PP_func: control_distribution must be \"Exponential\" or \"Weibull\".")
  }

  # An exponential control arm is a Weibull with shape 1, so a single
  # (Weibull) code path is used for both distributions.
  gamma_c_samples <- if (control_distribution == "Weibull") {
    posterior_df$gamma_c
  } else {
    rep(1, nrow(posterior_df))
  }

  PP_df <- data.frame(success = numeric(n_sims),
                       Z_val = numeric(n_sims))

  # Patients censored at the interim, by group (fixed across draws)
  censored_df <- data[data$status == 0, ]
  n_censored_control <- sum(censored_df$group == "Control")

  for (j in 1:n_sims){


    #Sampling the recruitment times for the unenrolled patients
    unenrolled_rec_times <- stats::runif(n_unenrolled_control+n_unenrolled_treatment, df_cens_time, rec_time_planned)

    idx <- sample(seq_len(nrow(posterior_df)), 1)
    sampled_lambda_c      <- lambda_c_samples[idx]
    sampled_delay_time    <- delay_time_samples[idx]
    sampled_post_delay_HR <- post_delay_HR_samples[idx]
    sampled_gamma_c       <- gamma_c_samples[idx]

    # Unenrolled control patients: Weibull(lambda_c, gamma_c)
    u <- stats::runif(n_unenrolled_control)
    unenrolled_control_times <- (-log(u))^(1 / sampled_gamma_c) / sampled_lambda_c

    # Unenrolled treated patients: control hazard up to the delay, then
    # hazard ratio sampled_post_delay_HR
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

    #Now combine them together
    unenrolled_df <- data.frame(time = c(unenrolled_control_times, unenrolled_treatment_times),
                                group = c(rep("Control", n_unenrolled_control), rep("Treatment", n_unenrolled_treatment)),
                                rec_time = unenrolled_rec_times)

    unenrolled_df$pseudo_time <- unenrolled_df$time + unenrolled_df$rec_time

    #Extracting the censored observations in the control group
    control_censored_df <- censored_df %>%
      dplyr::filter(.data$group=="Control")

    # Censored control patients: conditional Weibull residual life
    V  <- stats::runif(n_censored_control)

    control_censored_df$final_time <- (
      ( (sampled_lambda_c * control_censored_df$survival_time)^sampled_gamma_c - log(V) )^(1 / sampled_gamma_c)
    ) / sampled_lambda_c

    #Calculating the pseudo time
    control_censored_df$final_pseudo_time <- control_censored_df$rec_time + control_censored_df$final_time

    # Subset: treatment patients censored before the delay
    censored_treatment_before_delay <- censored_df %>%
      dplyr::filter(.data$group == "Treatment") %>%
      dplyr::filter(.data$survival_time <= sampled_delay_time)

    n_before <- nrow(censored_treatment_before_delay)

    if (n_before > 0) {

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

      # effective Weibull scale after delay
      sampled_lambda_e <- sampled_lambda_c * sampled_post_delay_HR^(1 / sampled_gamma_c)

      # conditional Weibull residual life
      V <- stats::runif(n_after)

      H_t0 <- (sampled_lambda_e * censored_treatment_after_delay$survival_time)^sampled_gamma_c

      T_after <- (H_t0 - log(V))^(1 / sampled_gamma_c) / sampled_lambda_e

      censored_treatment_after_delay$final_time <- T_after

      censored_treatment_after_delay$final_pseudo_time <-
        censored_treatment_after_delay$rec_time +
        censored_treatment_after_delay$final_time
    }

    non_censored_df <- data %>%
      dplyr::filter(.data$status == 1)

    final_non_censored_df <- non_censored_df[, c("time", "group", "rec_time", "pseudo_time")]

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

    # =========================================================================
    # FIX: evaluate the trial-success event W (Eq. 8) correctly -- check every
    # future fixed decision point in order, stopping at the first crossed
    # boundary, rather than testing only a single final analysis at a flat
    # alpha. This replaces the previous single-stage censor-and-test block.
    # =========================================================================

    if (!is.null(future_boundaries)) {

      success   <- 0
      Z_current <- NA_real_

      for (k in seq_along(future_boundaries)) {

        fb <- future_boundaries[[k]]
        is_last <- (k == length(future_boundaries))

        censored_k <- cens_data(final_df, cens_method = "Events", cens_events = fb$events)

        test_k <- run_test(censored_k$data, analysis_model)

        Z_current <- test_k$Z

        if (!is.na(test_k$Z) && test_k$Z > fb$crit) {
          # Boundary crossed (whether an early efficacy look or the final
          # analysis) -> trial succeeds, stop evaluating this draw.
          success <- 1
          break
        }

        if (is_last) {
          # Reached the final analysis without crossing its boundary -> failure.
          success <- 0
          break
        }
        # Otherwise: not crossed at an early look -> continue to the next
        # future boundary in the chain (mirrors the real trial continuing).
      }

      PP_df[j,] <- c(success, Z_current)

    } else {

      # --- Legacy single-stage fallback (censoring_model + flat alpha) ---
      if (censoring_model$method == "Time") {
        censored <- cens_data(final_df, cens_method = "Time", cens_time = censoring_model$time)
      } else if (censoring_model$method == "Events") {
        censored <- cens_data(final_df, cens_method = "Events", cens_events = censoring_model$events)
      } else if (censoring_model$method == "IF") {
        censored <- cens_data(final_df, cens_method = "IF", cens_IF = censoring_model$IF)
      }

      test_result <- run_test(censored$data, analysis_model)

      PP_df[j,] <- c(test_result$Signif, test_result$Z)
    }

  }

  return(list(PP_df = PP_df))

}


#' Function to calculate the 'optimal' PP threshold value
#'
#' @param n_c Number of control patients
#' @param n_t Number of treatment patients
#' @param control_model A named list specifying the control arm survival distribution
#'   (see \code{\link{update_priors}} for details).
#' @param effect_model A named list specifying beliefs about the treatment effect
#'   (see \code{\link{update_priors}} for details).
#' @param recruitment_model A named list specifying the recruitment process
#'   (see \code{\link{add_recruitment_time}} for details).
#' @param IA_model A named list specifying the interim analysis timing:
#'   \itemize{
#'     \item \code{events}: total planned event count (100% information fraction).
#'     \item \code{IF}: the information fraction at which the interim futility
#'       look occurs (i.e. where the data are censored to compute PP).
#'   }
#' @param analysis_model A named list specifying the analysis method
#'   (see \code{\link{PP_func}} for details).
#' @param data_generating_model A named list specifying the true data-generating
#'   parameters used to simulate trials for calibration:
#'   \itemize{
#'     \item \code{lambda_c}: hazard rate for the control group.
#'     \item \code{gamma_c}: Weibull shape parameter (\code{NULL} for Exponential).
#'     \item \code{delay_time}: true delay before the treatment effect begins.
#'     \item \code{post_delay_HR}: true post-delay hazard ratio.
#'   }
#' @param future_boundaries \strong{Recommended.} A list of future, pre-specified,
#'   fixed decision points (efficacy look(s) and the final analysis) to check on
#'   each posterior-predictive draw, matching the true group-sequential design
#'   under which this PP threshold will actually be used -- see
#'   \code{\link{PP_func}} for the exact structure. If \code{NULL} (not
#'   recommended, kept only for backward compatibility), PP is computed via
#'   the legacy single-stage fallback: censoring directly to \code{IA_model$events}
#'   and testing at a flat \code{analysis_model$alpha}, which does NOT account
#'   for any future efficacy boundary the real design may have. A message is
#'   emitted if this fallback is used.
#' @param n_df_sims Number of interim datasets to simulate (default is 100).
#'   For calibration decisions near a specific power/Type I error target,
#'   consider a substantially larger number and report Monte Carlo standard
#'   errors alongside the result.
#' @param update_priors_sims Number of posterior samples to generate per
#'   interim dataset via \code{\link{update_priors}} (default is 1000).
#' @param PP_sims Number of predictive simulations used to estimate PP for
#'   each interim dataset, passed to \code{\link{PP_func}} (default is 2000).
#' @param n_cores Number of cores to parallelise over via
#'   \code{parallel::mclapply} (default is 1, i.e. sequential
#'   \code{lapply}; not supported on Windows for \code{n_cores > 1}, per
#'   base R's \code{mclapply} limitations).
#' @param seed Optional integer seed. If supplied, each simulated interim
#'   dataset \code{i} is seeded reproducibly as \code{seed * 10000 + i}. If
#'   \code{NULL} (default), no seed is set internally -- set one in the
#'   calling script if reproducibility across runs is required.
#'
#' @return A list with:
#'   \describe{
#'     \item{PP_vec}{A numeric vector of length \code{n_df_sims}, the
#'       estimated PP for each simulated interim dataset.}
#'     \item{settings}{A list recording the exact settings used (all
#'       arguments above, plus the installed \code{DTEAssurance} package
#'       version), for provenance -- save this alongside \code{PP_vec}
#'       so it is always possible to confirm what generated a given result.}
#'   }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' control_model <- list(dist = "Exponential", parameter_mode = "Distribution",
#'                       t1 = 12, t1_Beta_a = 20, t1_Beta_b = 32)
#'
#' effect_model <- list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
#'                     delay_dist = "gamma",
#'                     HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
#'                     HR_dist = "gamma",
#'                     P_S = 1, P_DTE = 0)
#'
#' recruitment_model <- list(method = "power", period = 12, power = 1)
#'
#' IA_model <- list(events = 40, IF = 0.5)
#'
#' analysis_model <- list(method = "LRT", alpha = 0.025,
#'                        alternative_hypothesis = "one.sided")
#'
#' data_generating_model <- list(lambda_c = log(2)/12, gamma_c = NULL,
#'                               delay_time = 3, post_delay_HR = 0.75)
#'
#' # A single future efficacy look at 30 events (Z > 2.24), then final at 40
#' # events (Z > 2.00) -- match this to the true design's own boundaries.
#' future_boundaries <- list(
#'   list(events = 30, crit = 2.24),
#'   list(events = 40, crit = 2.00)
#' )
#'
#' threshold <- calibrate_PP_threshold(n_c = 25, n_t = 25,
#'                      control_model = control_model,
#'                      effect_model = effect_model,
#'                      recruitment_model = recruitment_model,
#'                      IA_model = IA_model,
#'                      analysis_model = analysis_model,
#'                      data_generating_model = data_generating_model,
#'                      future_boundaries = future_boundaries,
#'                      n_df_sims = 2)
#'
calibrate_PP_threshold <- function(n_c,
                                    n_t,
                                    control_model,
                                    effect_model,
                                    recruitment_model,
                                    IA_model,
                                    analysis_model,
                                    data_generating_model,
                                    future_boundaries = NULL,
                                    n_df_sims = 100,
                                    update_priors_sims = 1000,
                                    PP_sims = 2000,
                                    n_cores = 1,
                                    seed = NULL) {

  if (is.null(future_boundaries)) {
    message("calibrate_PP_threshold: no 'future_boundaries' supplied -- ",
            "falling back to the legacy single-stage PP calculation ",
            "(censoring directly to IA_model$events, testing at a flat ",
            "analysis_model$alpha). This does NOT account for any future ",
            "efficacy boundary the real design may have, and will not match ",
            "the true operating characteristics of a group-sequential design. ",
            "Supply 'future_boundaries' matching the true design whenever ",
            "one exists.")
  }

  run_one <- function(i) {
    if (!is.null(seed)) set.seed(seed * 10000 + i)

    data <- simulate_trial_from_truth(data_generating_model, n_c, n_t,
                                      recruitment_model)

    censored_data <- cens_data(data, cens_method = "Events",
                               cens_events = n_events_at(IA_model$events, IA_model$IF))
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
        list(method = "Events", events = IA_model$events)
      } else {
        NULL
      },
      n_sims               = PP_sims
    )

    mean(PP_outcome$PP_df$success)
  }

  if (n_cores > 1) {
    PP_vec <- parallel::mclapply(seq_len(n_df_sims), run_one, mc.cores = n_cores)
  } else {
    PP_vec <- lapply(seq_len(n_df_sims), run_one)
  }
  PP_vec <- unlist(PP_vec)

  settings <- list(
    n_df_sims          = n_df_sims,
    update_priors_sims = update_priors_sims,
    PP_sims            = PP_sims,
    n_cores            = n_cores,
    seed               = seed,
    future_boundaries  = future_boundaries,
    IA_model           = IA_model,
    package_version    = tryCatch(
      as.character(utils::packageVersion("DTEAssurance")),
      error = function(e) NA_character_
    ),
    timestamp          = as.character(Sys.time())
  )

  return(list(PP_vec = PP_vec, settings = settings))
}



#' Function to calculate the 'optimal' information fraction to calculate PP
#'
#' @param n_c Number of control patients
#' @param n_t Number of treatment patients
#' @param control_model A named list specifying the control arm survival distribution
#'   (see \code{\link{update_priors}} for details).
#' @param effect_model A named list specifying beliefs about the treatment effect
#'   (see \code{\link{update_priors}} for details).
#' @param recruitment_model A named list specifying the recruitment process
#'   (see \code{\link{add_recruitment_time}} for details).
#' @param IA_model A named list specifying the censoring mechanism for the future data:
#'   \itemize{
#'     \item \code{events}: Number of events which is 100% information fraction
#'     \item \code{IF}: A vector of candidate information fractions to evaluate.
#'       \strong{Every candidate must genuinely precede \code{future_boundaries}}
#'       (see below) -- e.g. if the true design has an efficacy look at
#'       IF = 0.75, do not include candidates at or beyond 0.75 in this sweep;
#'       a futility look scheduled at or after the design's own efficacy look
#'       is not a well-posed question about that design.
#'   }
#' @param analysis_model A named list specifying the final analysis and decision rule
#'   (see \code{\link{PP_func}} for details).
#' @param future_boundaries \strong{Recommended.} The chain of fixed future
#'   decision points (efficacy look(s) and final analysis) of the true
#'   group-sequential design under consideration -- see \code{\link{PP_func}}.
#'   The same chain is used for every candidate in \code{IA_model$IF} (each
#'   candidate is checked to ensure it genuinely precedes every boundary in
#'   the chain; see \code{single_calibration_rep()}). If \code{NULL}
#'   (not recommended), falls back to the legacy single-stage calculation,
#'   and a message is emitted.
#' @param update_priors_sims Number of posterior samples per interim dataset
#'   (default 1000; was previously hardcoded to 100).
#' @param PP_sims Number of predictive simulations per interim dataset
#'   (default 2000; was previously hardcoded to 50).
#' @param n_sims Number of interim datasets to simulate per candidate timing
#'   (default is 100).
#' @param n_cores Number of cores to parallelise over via
#'   \code{parallel::mclapply} (default is 1, i.e. sequential \code{lapply}).
#' @param seed Optional integer seed; if supplied, replicate \code{i} under
#'   candidate timing \code{k} is seeded as \code{seed * 10000 + i} (note:
#'   replicates are re-seeded identically across candidate timings, so the
#'   same underlying trial trajectories are reused -- i.e. paired comparison
#'   across timings -- unless you deliberately want independent draws per
#'   timing, in which case pass a different \code{seed} per call).
#'
#' @return A list with:
#'   \describe{
#'     \item{outcome_list}{A list, one element per candidate information
#'       fraction, each containing \code{PP_values} (a vector of estimated
#'       PP, one per simulated interim dataset) and \code{cens_time} (the
#'       corresponding calendar times).}
#'     \item{settings}{A list recording the exact settings used, plus the
#'       installed \code{DTEAssurance} package version, for provenance.}
#'   }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' control_model <- list(dist = "Exponential", parameter_mode = "Distribution",
#'                       t1 = 12, t1_Beta_a = 20, t1_Beta_b = 32)
#'
#' effect_model <- list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
#'                     delay_dist = "gamma",
#'                     HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7),
#'                     probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
#'                     HR_dist = "gamma",
#'                     P_S = 1, P_DTE = 0)
#'
#' recruitment_model <- list(method = "power", period = 12, power = 1)
#'
#' # Sweep candidates from 0.2 to 0.7 only -- all genuinely precede the
#' # design's own efficacy look at IF = 0.75.
#' IA_model <- list(events = 40, IF = seq(0.2, 0.7, by = 0.1))
#'
#' analysis_model <- list(method = "LRT", alpha = 0.025,
#'                       alternative_hypothesis = "one.sided")
#'
#' future_boundaries <- list(
#'   list(events = 30, crit = 2.24),   # efficacy look at IF = 0.75
#'   list(events = 40, crit = 2.00)    # final analysis
#' )
#'
#' timing <- calibrate_PP_timing(n_c = 25, n_t = 25,
#'                      control_model = control_model,
#'                      effect_model = effect_model,
#'                      recruitment_model = recruitment_model,
#'                      IA_model = IA_model,
#'                      analysis_model = analysis_model,
#'                      future_boundaries = future_boundaries,
#'                      n_sims = 2)
#'
calibrate_PP_timing <- function(n_c, n_t,
                                 control_model,
                                 effect_model,
                                 recruitment_model,
                                 IA_model,
                                 analysis_model,
                                 future_boundaries = NULL,
                                 update_priors_sims = 1000,
                                 PP_sims = 2000,
                                 n_sims = 100,
                                 n_cores = 1,
                                 seed = NULL) {

  if (is.null(future_boundaries)) {
    message("calibrate_PP_timing: no 'future_boundaries' supplied -- falling ",
            "back to the legacy single-stage PP calculation, which does NOT ",
            "account for any future efficacy boundary the real design may ",
            "have. Supply 'future_boundaries' matching the true design ",
            "whenever one exists.")
  }

  outcome_list <- vector("list", length(IA_model$IF))

  for (i in seq_along(IA_model$IF)) {

    if (n_cores > 1) {
      result <- parallel::mclapply(
        seq_len(n_sims),
        FUN = single_calibration_rep,
        n_c = n_c, n_t = n_t,
        control_model = control_model,
        effect_model = effect_model,
        recruitment_model = recruitment_model,
        total_events = IA_model$events,
        IF = IA_model$IF[i],
        analysis_model = analysis_model,
        future_boundaries = future_boundaries,
        update_priors_sims = update_priors_sims,
        PP_sims = PP_sims,
        seed = seed,
        mc.cores = n_cores
      )
    } else {
      result <- lapply(
        seq_len(n_sims),
        FUN = single_calibration_rep,
        n_c = n_c, n_t = n_t,
        control_model = control_model,
        effect_model = effect_model,
        recruitment_model = recruitment_model,
        total_events = IA_model$events,
        IF = IA_model$IF[i],
        analysis_model = analysis_model,
        future_boundaries = future_boundaries,
        update_priors_sims = update_priors_sims,
        PP_sims = PP_sims,
        seed = seed
      )
    }

    PP_values <- vapply(
      result,
      FUN = function(x) mean(x$PP_outcome$PP_df$success),
      FUN.VALUE = numeric(1)
    )

    cens_time <- vapply(result,
                        function(x) x$cens_time,
                        FUN.VALUE = numeric(1)
    )

    outcome_list[[i]]$PP_values <- PP_values
    outcome_list[[i]]$cens_time  <- cens_time
    outcome_list[[i]]$IF         <- IA_model$IF[i]
  }

  settings <- list(
    IA_model           = IA_model,
    update_priors_sims = update_priors_sims,
    PP_sims            = PP_sims,
    n_sims             = n_sims,
    n_cores            = n_cores,
    seed               = seed,
    future_boundaries  = future_boundaries,
    package_version    = tryCatch(
      as.character(utils::packageVersion("DTEAssurance")),
      error = function(e) NA_character_
    ),
    timestamp          = as.character(Sys.time())
  )

  return(list(outcome_list = outcome_list, settings = settings))
}
