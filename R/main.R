#' Simulate a DTE data set
#'
#' @param n_c number of patients in the control group
#' @param n_t number of patients in the treatment group
#' @param lambda_c control hazard
#' @param delay_time length of delay
#' @param post_delay_HR the hazard ratio, once the treatment begins to take effect
#' @param dist distribution of control distribution, must be one of "exponential" (default) or "weibull"
#' @param gamma_c gamma parameter in the Weibull distribution
#'
#' @return a DTE data set
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
#' @param data A dataframe containing uncensored survival data with columns: pseudo_time, rec_time, and time
#' @param cens_method Censoring method: "Time" (default), "Events", or "IF"
#' @param cens_time Time point for censoring (required if cens_method = "Time")
#' @param cens_IF Information fraction for censoring (required if cens_method = "IF")
#' @param cens_events Number of events for censoring (required if cens_method = "Events")
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Censored dataframe}
#'   \item{cens_events}{Number of events used for censoring (if applicable)}
#'   \item{cens_time}{Time point used for censoring}
#'   \item{sample_size}{Number of subjects remaining after censoring}
#' }
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
  data <- subset(data, enrolled)

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


#' Calculate Assurance for a trial with a Delayed Treatment Effect
#'
#' @param n_c Number of patients in the control group
#' @param n_t Number of patients in the treatment group
#' @param control_dist Distribution of control group, must be one of "Exponential" (default) or "weibull"
#' @param control_parameters The parameters for the control group are either "Fixed" (default) or "Distribution"
#' @param fixed_parameters_type The fixed parameters are either a "Parameters" (default) or "Landmark"
#' @param control_distribution Control parameters will be parameterised through "Elicitation" (default) or a "MCMC_sample"
#' @param lambda_c Control group parameter
#' @param gamma_c Control group parameter
#' @param t1 Time 1
#' @param t2 Time 2
#' @param surv_t1 Survival probability at time 1
#' @param surv_t2 Survival probability at time 2
#' @param MCMC_sample A MCMC sample for the control parameters
#' @param t1_Beta_a Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param t1_Beta_b Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param diff_Beta_a Hyperparameter a for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param diff_Beta_b Hyperparameter b for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param delay_time_SHELF A SHELF object, beliefs about the delay time
#' @param delay_time_dist Distribution of the delay time, "hist" is default. See SHELF help for more details
#' @param post_delay_HR_SHELF A SHELF object, beliefs about the post-delay hazard ratio
#' @param post_delay_HR_dist Distribution of the post-delay hazard ratio, "hist" is default. See SHELF help for more details
#' @param P_S Probability of the survival curves separating
#' @param P_DTE Probability of the survival curves being subject to a DTE, given they separate
#' @param cens_method Method of censoring, must be either "Time" (default), "Events" or "IF"
#' @param cens_time Time at which you wish to perform the censoring
#' @param cens_events Number of events at which you wish to perform the censoring
#' @param cens_IF Information Fraction at which you wish to perform the censoring
#' @param rec_method Recruitment method, must be one of "power" or "PWC" (piecewise constant)
#' @param rec_period Parameter used to model recruitment according to power model
#' @param rec_power Parameter used to model recruitment according to power model
#' @param rec_rate Parameter used to model recruitment according to piecewise constant model
#' @param rec_duration Parameter used to model recruitment according to piecewise constant model
#' @param analysis_method Method of analysis, "LRT" (default) for standard log-rank test, or "WLRT" for weighted log-rank test
#' @param alpha Type I error
#' @param alternative String specifying the alternative hypothesis, must be one of "one.sided" or "two-sided" (default)
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test
#' @param t_star Parameter t^* in the modestly weighted test
#' @param s_star Parameter s^* in the modestly weighted test
#' @param target_HR Only claim success if the observed HR is less than this value
#' @param nSims Number of simulations, default is 1000
#'
#' @return A list: Assurance value, 95% CI for assurance, Duration
#' @export
#'
calc_dte_assurance <- function(n_c, n_t,
                               control_dist = "Exponential", control_parameters = "Fixed",
                               fixed_parameters_type = "Parameter", control_distribution = "Elicitation",
                               lambda_c = NULL, gamma_c = NULL,
                               MCMC_sample = NULL,
                               t1 = NULL, t2 = NULL,
                               surv_t1 = NULL, surv_t2 = NULL,
                               t1_Beta_a = NULL, t1_Beta_b = NULL,
                               diff_Beta_a = NULL, diff_Beta_b = NULL,
                               delay_time_SHELF, delay_time_dist = "hist",
                               post_delay_HR_SHELF, post_delay_HR_dist = "hist",
                               P_S = 1, P_DTE = 0,
                               cens_method = "Time", cens_time = NULL, cens_IF = NULL, cens_events = NULL,
                               rec_method, rec_period = NULL, rec_power = NULL, rec_rate = NULL, rec_duration = NULL,
                               analysis_method = "LRT", alpha = 0.05, alternative = "one.sided",
                               rho = 0, gamma = 0,
                               t_star = NULL, s_star = NULL,
                               target_HR = NULL,
                               nSims = 1e3) {

  loopVec <- if (cens_method == "Events") (n_c + n_t) > cens_events else rep(TRUE, length(n_c))
  calc_dte_assurance_list <- vector("list", length(n_c))

  for (j in seq_along(n_c)) {
    if (!loopVec[j]) {
      calc_dte_assurance_list[[j]] <- if (!is.null(target_HR)) {
        list(assurance = NA, CI_assurance = NA,
             assurance_targetHR = NA, CI_assurance_targetHR = NA,
             duration = NA, sample_size = NA)
      } else {
        list(assurance = NA, CI_assurance = NA,
             duration = NA, sample_size = NA)
      }
      next
    }

    delay_time_samples <- SHELF::sampleFit(delay_time_SHELF, n = nSims)[, delay_time_dist]
    post_delay_HR_samples <- SHELF::sampleFit(post_delay_HR_SHELF, n = nSims)[, post_delay_HR_dist]

    sim_results <- future_lapply(seq_len(nSims), simulate_one_trial, future.seed = TRUE,
                                 j = j,
                                 n_c = n_c, n_t = n_t,
                                 control_dist = control_dist,
                                 control_parameters = control_parameters,
                                 fixed_parameters_type = fixed_parameters_type,
                                 control_distribution = control_distribution,
                                 lambda_c = lambda_c, gamma_c = gamma_c,
                                 MCMC_sample = MCMC_sample,
                                 t1 = t1, t2 = t2,
                                 surv_t1 = surv_t1, surv_t2 = surv_t2,
                                 t1_Beta_a = t1_Beta_a, t1_Beta_b = t1_Beta_b,
                                 diff_Beta_a = diff_Beta_a, diff_Beta_b = diff_Beta_b,
                                 delay_time_samples = delay_time_samples,
                                 post_delay_HR_samples = post_delay_HR_samples,
                                 P_S = P_S, P_DTE = P_DTE,
                                 cens_method = cens_method, cens_time = cens_time,
                                 cens_IF = cens_IF, cens_events = cens_events,
                                 rec_method = rec_method, rec_period = rec_period,
                                 rec_power = rec_power, rec_rate = rec_rate,
                                 rec_duration = rec_duration,
                                 analysis_method = analysis_method,
                                 alpha = alpha, alternative = alternative,
                                 rho = rho, gamma = gamma,
                                 t_star = t_star, s_star = s_star,
                                 target_HR = target_HR)

    assurance_vec <- sapply(sim_results, `[[`, "Signif")
    cens_vec <- sapply(sim_results, `[[`, "cens_time")
    ss_vec <- sapply(sim_results, `[[`, "sample_size")

    if (!is.null(target_HR)) {
      deltad_vec <- sapply(sim_results, `[[`, "deltad")
      assurance_targetHR_vec <- assurance_vec * (deltad_vec < target_HR)
    }

    assurance <- mean(assurance_vec)
    CI_assurance <- stats::binom.test(sum(assurance_vec), nSims)$conf.int

    if (!is.null(target_HR)) {
      assurance_targetHR <- mean(assurance_targetHR_vec)
      CI_targetHR <- stats::binom.test(sum(assurance_targetHR_vec), nSims)$conf.int
      calc_dte_assurance_list[[j]] <- list(
        assurance = assurance,
        CI_assurance = CI_assurance,
        assurance_targetHR = assurance_targetHR,
        CI_assurance_targetHR = CI_targetHR,
        duration = mean(cens_vec),
        sample_size = mean(ss_vec)
      )
    } else {
      calc_dte_assurance_list[[j]] <- list(
        assurance = assurance,
        CI_assurance = CI_assurance,
        duration = mean(cens_vec),
        sample_size = mean(ss_vec)
      )
    }
  }

  return(calc_dte_assurance_list)
}


#' Calculate statistical significance on a survival data set
#'
#' @import nph
#'
#' @param data A survival dataframe
#' @param analysis_method Method of analysis, "LRT" (default) for standard log-rank test, or "WLRT" for weighted log-rank test
#' @param alpha Type I error
#' @param alternative String specifying the alternative hypothesis, must be one of "one.sided" or "two-sided" (default)
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test
#' @param t_star Parameter t^* in the modestly weighted test
#' @param s_star Parameter s^* in the modestly weighted test
#' @return A list with:
#' \describe{
#'   \item{Signif}{Logical indicator of statistical significance}
#'   \item{deltad}{Estimated hazard ratio from Cox model}
#' }
#' @export


survival_test <- function(data, analysis_method = "LRT", alternative = "one.sided", alpha = 0.05, rho = 0, gamma = 0,
                          t_star = NULL, s_star = NULL){

  coxmodel <- survival::coxph(Surv(survival_time, status)~group, data = data)
  deltad <- as.numeric(exp(stats::coef(coxmodel)))

  Signif <- 0

  if (analysis_method=="LRT"){
    test <- survival::survdiff(Surv(survival_time, status)~group, data = data)
    if (alternative=="one.sided"){
      Signif <- (test$chisq > stats::qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$chisq > stats::qchisq(1-alpha/2, 1)
    }

  } else if (analysis_method=="WLRT"){
    test <- nph::logrank.test(data$survival_time, data$status, data$group, rho = rho, gamma = gamma)
    if (alternative=="one.sided"){
      Signif <- (test$test$Chisq > stats::qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$test$Chisq > stats::qchisq(1-alpha/2, 1)
    }
  } else if (analysis_method=="MW"){
    test <- nphRCT::wlrt(Surv(survival_time, status)~group,
                         data = data, method = "mw",
                         t_star = t_star, s_star = s_star)
    if (alternative=="one.sided"){
      Signif <- test$z < stats::qnorm(alpha)
    } else {
      Signif <- test$z < stats::qnorm(alpha/2) | test$z > stats::qnorm(1-alpha/2)
    }
  }

  return(list(Signif = Signif, deltad = deltad))

}

#' Add recruitment time to a survival dataset
#'
#' @param data A survival dataframe with columns: time, status, group
#' @param rec_method Recruitment method: "power" or "PWC" (piecewise constant)
#' @param rec_period Period length for power model
#' @param rec_power Power parameter for power model
#' @param rec_rate Comma-separated string of rates for PWC model
#' @param rec_duration Comma-separated string of durations for PWC model
#'
#' @return A dataframe with added columns: rec_time and pseudo_time
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
      final_rate <- tail(df$rate, 1)
      final_time <- tail(df$finish, 1)
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


#' Simulates a Group Sequential Trial for a trial with a Delayed Treatment Effect, with elicited priors
#'
#' @param n_c Number of patients in the control group
#' @param n_t Number of patients in the treatment group
#' @param control_dist Distribution of control group, must be one of "Exponential" (default) or "weibull"
#' @param control_parameters The parameters for the control group are either "Fixed" (default) or "Distribution"
#' @param fixed_parameters_type The fixed parameters are either a "Parameters" (default) or "Landmark"
#' @param lambda_c Control group parameter
#' @param gamma_c Control group parameter
#' @param t1 Time 1
#' @param t2 Time 2
#' @param surv_t1 Survival probability at time 1
#' @param surv_t2 Survival probability at time 2
#' @param t1_Beta_a Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param t1_Beta_b Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param diff_Beta_a Hyperparameter a for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param diff_Beta_b Hyperparameter b for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param delay_time_SHELF A SHELF object, beliefs about the delay time
#' @param delay_time_dist Distribution of the delay time, "hist" is default. See SHELF help for more details
#' @param post_delay_HR_SHELF A SHELF object, beliefs about the post-delay hazard ratio
#' @param post_delay_HR_dist Distribution of the post-delay hazard ratio, "hist" is default. See SHELF help for more details
#' @param P_S Probability of the survival curves separating
#' @param P_DTE Probability of the survival curves being subject to a DTE, given they separate
#' @param cens_events Number of events at which you wish to perform the censoring (must be less than n_c + n_t)
#' @param rec_method Recruitment method, must be one of "power" or "PWC" (piecewise constant)
#' @param rec_period Parameter used to model recruitment according to power model
#' @param rec_power Parameter used to model recruitment according to power model
#' @param rec_rate Parameter used to model recruitment according to piecewise constant model
#' @param rec_duration Parameter used to model recruitment according to piecewise constant model
#' @param alpha_spending Cumulative alpha spending
#' @param beta_spending Cumulative beta spending
#' @param IF_list Vector of information fractions - must be in the format: `c("0.25, 1", "0.5, 1", "0.75, 1")'
#' @param k Number of stages
#' @param type_one_error Alpha (if k = 1)
#' @param nSims Number of simulations, default is 1000
#' @export

calc_dte_assurance_interim <- function(n_c, n_t,
                                       control_dist = "Exponential", control_parameters = "Fixed",
                                       fixed_parameters_type = "Parameter",
                                       lambda_c = NULL, gamma_c = NULL,
                                       t1 = NULL, t2 = NULL,
                                       surv_t1 = NULL, surv_t2 = NULL,
                                       t1_Beta_a = NULL, t1_Beta_b = NULL,
                                       diff_Beta_a = NULL, diff_Beta_b = NULL,
                                       delay_time_SHELF, delay_time_dist = "hist",
                                       post_delay_HR_SHELF, post_delay_HR_dist = "hist",
                                       P_S = 1, P_DTE = 0, cens_events = NULL,
                                       rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                                       alpha_spending = NULL, beta_spending = NULL, IF_list = NULL, k = 2,
                                       type_one_error = NULL,  nSims=1e3){

  if (k > 1 ){

    designList <- vector("list", length(IF_list))

    for (j in 1:length(IF_list)){

      info_rates <- as.numeric(strsplit(IF_list[j], ", ")[[1]])

      design <- rpact::getDesignGroupSequential(typeOfDesign = "asUser",
                                         informationRates = info_rates,
                                         userAlphaSpending = alpha_spending,
                                         typeBetaSpending = "bsUser",
                                         userbetaSpending = beta_spending)

      designList[[j]] <- list(
        IF = info_rates,
        critValues = design$criticalValues,
        futBounds = design$futilityBounds,
        assurance = rep(NA, nSims),
        ss = rep(NA, nSims),
        duration = rep(NA, nSims),
        status = rep(NA, nSims)

      )

    }

    unique_IF <- sort(unique(unlist(lapply(strsplit(IF_list, ", "), as.numeric))))

    for (i in 1:nSims){

      if (control_dist=="Exponential"){
        if (control_parameters=="Fixed"){
          if (fixed_parameters_type=="Parameter"){
            lambda_c <- lambda_c
          } else if (fixed_parameters_type=="Landmark") {
            lambda_c <-  -log(surv_t1)/t1
          }
        } else if (control_parameters=="Distribution"){
          lambda_c <- -log(stats::rbeta(1, t1_Beta_a, t1_Beta_b)) / t1
        }
      } else if (control_dist=="Weibull"){
        if (control_parameters=="Fixed"){
          if (fixed_parameters_type=="Parameter"){
            lambda_c <- lambda_c
            gamma_c <- gamma_c
          } else if (fixed_parameters_type=="Landmark"){
            WeibFunc <- function(params) {
              lambda <- params[1]
              k <- params[2]
              c(exp(-(t1*lambda)^k) - surv_t1,
                exp(-(t2*lambda)^k) - surv_t1)
            }

            solution <- nleqslv::nleqslv(c(1, 1), fn = WeibFunc)

            lambda_c <- solution$x[1]
            gamma_c <- solution$x[2]
          }
        } else if (control_parameters=="Distribution"){
          sampledS1to <- stats::rbeta(1, t1_Beta_a, t1_Beta_b)
          sampledDelta1 <- stats::rbeta(1, diff_Beta_a, diff_Beta_b)
          sampledS1toPrime <- sampledS1to - sampledDelta1

          # Solve for lambda and gamma using sampled values
          solution <- nleqslv::nleqslv(c(10, 1), function(params) {
            lambda <- params[1]
            k <- params[2]
            c(exp(-(t1 / lambda)^k) - sampledS1to,
              exp(-(t2 / lambda)^k) - sampledS1toPrime)
          })

          lambda_c <- 1 / solution$x[1]
          gamma_c <- solution$x[2]
        }
      }

      if (stats::runif(1) > P_S){
        #Curves do not separate
        delay_time <- 0
        post_delay_HR <- 1
      } else {
        if (stats::runif(1) > P_DTE){
          #Curves separate with no delay
          delay_time <- 0
          post_delay_HR_sample <- SHELF::sampleFit(post_delay_HR_SHELF, n = 1)
          post_delay_HR <- post_delay_HR_sample[,post_delay_HR_dist]
        } else{
          #Curves separate with a delay
          delay_time_sample <- SHELF::sampleFit(delay_time_SHELF, n = 1)
          post_delay_HR_sample <- SHELF::sampleFit(post_delay_HR_SHELF, n = 1)
          delay_time <- delay_time_sample[,delay_time_dist]
          post_delay_HR <- post_delay_HR_sample[,post_delay_HR_dist]
        }
      }

      data <- sim_dte(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = control_dist, gamma_c = gamma_c)

      if (rec_method=="power"){
        data <- add_recruitment_time(data, rec_method,
                                     rec_period, rec_power)
      }

      if (rec_method == "PWC"){
        data <- add_recruitment_time(data, rec_method,
                                     rec_rate, rec_duration)
      }


      unique_IF_DF <- data.frame(matrix(NA, nrow = length(unique_IF), ncol = 4))
      unique_IF_DF[,1] <- unique_IF
      colnames(unique_IF_DF) <- c("IF", "SS", "Duration", "Z-Score")



      for (j in 1:length(unique_IF)){
        data_after_cens <- cens_data(data, cens_method = "Events", cens_events = unique_IF_DF$IF[j]*cens_events)
        coxmodel <- survival::coxph(Surv(survival_time, status) ~ group, data = data_after_cens$data)
        unique_IF_DF$`Z-Score`[j] <- -(stats::coef(summary(coxmodel))[, 4])
        unique_IF_DF$SS[j] <- data_after_cens$sample_size
        unique_IF_DF$Duration[j] <- data_after_cens$cens_time
      }


      for (l in 1:length(designList)){
        subset_table <- unique_IF_DF[unique_IF_DF$IF %in% designList[[l]]$IF,]
        GSD_output <- group_sequential_decision(z_scores = subset_table$`Z-Score` ,
                                                critical_values = designList[[l]]$critValues,
                                                futility_values = designList[[l]]$futBounds,
                                                sample_sizes = subset_table$SS,
                                                durations = subset_table$Duration,
                                                alpha_spending = alpha_spending[k])


        designList[[l]]$assurance[i] <- GSD_output$successful
        designList[[l]]$ss[i] <- GSD_output$sample_size
        designList[[l]]$duration[i] <- GSD_output$duration
        designList[[l]]$status[i] <- GSD_output$status
        designList[[l]]$IATimes[i] <- paste(subset_table$Duration, collapse = ", ")
        designList[[l]]$final_success[i] <- GSD_output$final_success
      }

    }

    for (j in 1:length(designList)){
      designList[[j]]$assurance_mean <- mean(designList[[j]]$assurance)
      designList[[j]]$ss_mean <- mean(designList[[j]]$ss)
      designList[[j]]$duration_mean <- mean(designList[[j]]$duration)
      designList[[j]]$eff_mean <- mean(grepl("Stopped for efficacy at IA", designList[[j]]$status))
      designList[[j]]$fut_mean <- mean(grepl("Stopped for futility at IA", designList[[j]]$status))
    }

    for (j in 1:length(designList)){
      designList[[j]]$stop_mean <- designList[[j]]$eff_mean + designList[[j]]$fut_mean

    }

    return(designList)
  } else {


    outcome <- calc_dte_assurance(n_c = n_c,
                                  n_t = n_t,
                                  control_dist = control_dist,
                                  control_parameters = control_parameters,
                                  fixed_parameters_type = fixed_parameters_type,
                                  lambda_c = lambda_c,
                                  gamma_c = gamma_c,
                                  t1 = t1,
                                  t2 = t2,
                                  surv_t1 = surv_t1,
                                  surv_t2 = surv_t2,
                                  t1_Beta_a = t1_Beta_a,
                                  t1_Beta_b = t1_Beta_b,
                                  diff_Beta_a = diff_Beta_a,
                                  diff_Beta_b = diff_Beta_b,
                                  delay_time_SHELF = delay_time_SHELF,
                                  delay_time_dist = delay_time_dist,
                                  post_delay_HR_SHELF = post_delay_HR_SHELF,
                                  post_delay_HR_dist = post_delay_HR_dist,
                                  P_S = P_S,
                                  P_DTE = P_DTE,
                                  cens_method = "Events",
                                  cens_events = cens_events,
                                  rec_method = rec_method,
                                  rec_period=rec_period,
                                  rec_power=rec_power,
                                  rec_rate=rec_rate,
                                  rec_duration=rec_duration,
                                  analysis_method = "LRT",
                                  alpha = type_one_error,
                                  nSims=nSims)

    return(outcome)

  }

}

#'  Calculates the Bayesian Predictive Probability for a trial with a Delayed Treatment Effect
#'
#' @param n_c Number of patients in the control group
#' @param n_t Number of patients in the treatment group
#' @param control_dist Distribution of control group, must be one of "Exponential" (default) or "weibull"
#' @param t1 Time 1
#' @param t2 Time 2
#' @param t1_Beta_a Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param t1_Beta_b Hyperparameter a for the Beta distribution for the survival probability at time 1
#' @param diff_Beta_a Hyperparameter a for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param diff_Beta_b Hyperparameter b for the Beta distribution for the difference in survival probabilities (t2-t1)
#' @param delay_time_SHELF A SHELF object, beliefs about the delay time
#' @param delay_time_dist Distribution of the delay time, "hist" is default. See SHELF help for more details
#' @param post_delay_HR_SHELF A SHELF object, beliefs about the post-delay hazard ratio
#' @param post_delay_HR_dist Distribution of the post-delay hazard ratio, "hist" is default. See SHELF help for more details
#' @param P_S Probability of the survival curves separating
#' @param P_DTE Probability of the survival curves being subject to a DTE, given they separate
#' @param cens_events Number of events at which you wish to perform the censoring (must be less than n_c + n_t)
#' @param IF Information fraction at which BPP is calculated
#' @param rec_method Recruitment method, must be one of "power" or "PWC" (piecewise constant)
#' @param rec_period Parameter used to model recruitment according to power model
#' @param rec_power Parameter used to model recruitment according to power model
#' @param rec_rate Parameter used to model recruitment according to piecewise constant model
#' @param rec_duration Parameter used to model recruitment according to piecewise constant model
#' @param alpha_spending Cumulative alpha spending
#' @param beta_spending Cumulative beta spending
#' @param IF_list Vector of information fractions - must be in the format: `c("0.5, 1")'
#' @param k Number of stages
#' @param N Number of trials simulated (default is 50)
#' @param M Number of samples from the posterior (default is 50)
#'
#' @return Numeric vector of length `N` containing the simulated Bayesian Predictive Probabilities.
#'
#' @examples
#' set.seed(123)
#' # Minimal example for CRAN: small n_sim for fast execution
#' res <- calc_BPP_hist(n_c = 300,
#'                      n_t = 300,
#'                      control_dist = "Exponential",
#'                      t1 = 12,
#'                      t1_Beta_a = 20,
#'                      t1_Beta_b = 32,
#'                      delay_time_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
#'                      delay_time_dist = "gamma",
#'                      post_delay_HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
#'                      post_delay_HR_dist = "gamma",
#'                      P_S = 1,
#'                      P_DTE = 0,
#'                      cens_events = 300,
#'                      rec_method = "power",
#'                      rec_period = 12,
#'                      rec_power = 1,
#'                      alpha_spending = c(0.01, 0.025),
#'                      beta_spending = c(0.05, 0.1),
#'                      IF_list = c("0.5, 1"),
#'                      k = 2,
#'                      M = 5,
#'                      N = 5)
#' str(res)
#'

#' @export


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
