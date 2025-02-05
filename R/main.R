#' Simulate a DTE data set
#'
#' @param n_c number of patients in the control group
#' @param n_t number of patients in the treatment group
#' @param lambda_c control hazard
#' @param delay_time length of delay
#' @param post_delay_HR the hazard ratio, once the treatment begins to take effect
#' @param dist distribution of control distribution, must be one of "exponential" (default) or "weibull"
#' @param gamma_c
#'
#' @return a DTE data set
#' @export
#'
sim_dte <- function(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = "exponential", gamma_c = NULL){

  #Simulate control data
  u <- runif(n_c)

  if (dist == "exponential"){
    control_times <- -log(u)/lambda_c
  }

  if (dist == "weibull"){
    control_times <- (1/lambda_c)*(-log(u))^(1/gamma_c)
  }

  #Simulate treatment data
  u <- runif(n_t)
  if (dist == "exponential"){
    CP <- exp(-lambda_c*delay_time)
    treatment_times <- ifelse(u>CP,
                              -log(u)/lambda_c,
                              (1/(post_delay_HR*lambda_c))*(post_delay_HR*lambda_c*delay_time-log(u)-lambda_c*delay_time))
  }

  if (dist == "weibull"){
    CP <- exp(-(lambda_c*delay_time)^gamma_c)
    treatment_times <- ifelse(u > CP,
                              (1/lambda_c)*(-log(u))^(1/gamma_c),
                              (1/lambda_e)*(-log(u)+(lambda_e*delay_time)^gamma_c-(lambda_c*delay_time)^gamma_c)^(1/gamma_c))
  }

  #Combine the two groups
  data <- data.frame(time = c(control_times, treatment_times),
                     group = c(rep("Control", n_c), rep("Treatment", n_t)))

  return(data)

}


#' Censor a survival data set
#'
#' @param data An uncensored dataframe
#' @param cens_events Number of events at which you wish to perform the censoring
#' @param cens_time Time at which you wish to perform the censoring
#' @return A list: censored dataframe, cens_events and cens_time
#' @export

cens_data <- function(data, cens_events = NULL, cens_time = NULL){

  if (is.null(cens_events) && is.null(cens_time)) {
    stop("Either cens_events or cens_time must be specified")
  }

  if (!is.null(cens_events)){
    data <- data[order(data$pseudo_time),]
    cens_time <- data$pseudo_time[cens_events]
  }

  data$status <- data$pseudo_time <= cens_time
  data$status <- data$status * 1
  data$enrolled <- data$rec_time < cens_time
  data <- data[data$enrolled, ]
  data$survival_time <- ifelse(data$pseudo_time > cens_time,
                               cens_time - data$rec_time,
                               data$time)

  return(list(data = data, cens_events = cens_events, cens_time = cens_time))
}

#' Calculate Assurance for a trial with a Delayed Treatment Effect
#'
#' @param n_c number of patients in the control group
#' @param n_t number of patients in the control group
#' @param lambda_c control hazard
#' @param control_dist distribution of control distribution, must be one of "exponential" (default) or "weibull"
#' @param delay_time_SHELF A SHELF object, beliefs about the delay time
#' @param delay_time_dist Distribution of the delay time, "hist" is default. See SHELF help for more details
#' @param post_delay_HR_SHELF A SHELF object, beliefs about the post-delay hazard ratio
#' @param post_delay_HR_dist Distribution of the post-delay hazard ratio, "hist" is default. See SHELF help for more details
#' @param P_S Probability of the survival curves separating
#' @param P_DTE Probability of the survival curves being subject to a DTE, given they separate
#' @param cens_events Number of events at which you wish to perform the censoring (must be less than n_c + n_t)
#' @param cens_time Time at which you wish to perform the censoring
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
#' @param nSims Number of simulations, default is 1000
#'
#' @return A list: Assurance value, 95% CI for assurance, Duration
#' @export
#'

calc_dte_assurance <- function(n_c, n_t, lambda_c, control_dist = "Exponential",
                               delay_time_SHELF, delay_time_dist = "hist",
                               post_delay_HR_SHELF, post_delay_HR_dist = "hist",
                               P_S = 1, P_DTE = 0,
                               cens_events = NULL, cens_time = NULL,
                               rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                               analysis_method = "LRT", alpha = 0.05, alternative = "one.sided", rho = 0, gamma = 0, nSims=1e3){


  assurance_vec <- rep(NA, nSims)
  cens_vec <- rep(NA, nSims)

  for (i in 1:nSims){

    if (runif(1) > P_S){
      #Curves do not separate
      delay_time <- 0
      post_delay_HR <- 1
    } else {
      if (runif(1) > P_DTE){
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


    data <- sim_dte(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = "exponential", gamma_c = NULL)

    if (rec_method=="power"){
      data <- add_recruitment_time(data, rec_method,
                                   rec_period, rec_power)
    }

    if (rec_method == "PWC"){
      data <- add_recruitment_time(data, rec_method,
                                   rec_rate, rec_duration)
    }


    data_after_cens <- cens_data(data, cens_events, cens_time)
    data <- data_after_cens$data

    cens_vec[i] <- data_after_cens$cens_time

    assurance_vec[i] <- survival_test(data, analysis_method, alternative, alpha = 0.05, rho, gamma)


  }

  assurance <- mean(assurance_vec)

  return(list(assurance = assurance, CI_Assurance = c(assurance - 1.96*sqrt(assurance*(1-assurance)/nSims),
              UBAssurance = assurance + 1.96*sqrt(assurance*(1-assurance)/nSims)), duration = mean(cens_vec)))

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
#' @return An indicator of significance
#' @export


survival_test <- function(data, analysis_method = "LRT", alternative = "one.sided", alpha = 0.05, rho = 0, gamma = 0){

  coxmodel <- coxph(Surv(survival_time, status)~group, data = data)
  deltad <- as.numeric(exp(coef(coxmodel)))

  Signif <- 0

  if (analysis_method=="LRT"){
    test <- survdiff(Surv(survival_time, status)~group, data = data)
    if (alternative=="one.sided"){
      Signif <- (test$chisq > qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$chisq > qchisq(1-alpha, 1)
    }

  } else if (analysis_method=="WLRT"){
    test <- nph::logrank.test(data$survival_time, data$status, data$group, rho = rho, gamma = gamma)
    if (alternative=="one.sided"){
      Signif <- (test$test$Chisq > qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$test$Chisq > qchisq(1-alpha, 1)
    }
  }

  return(Signif)

}

#' Add a recruitment time to a survival data
#'
#' @param data A survival dataframe
#' @param rec_method Recruitment method, must be one of "power" or "PWC" (piecewise constant)
#' @param rec_period Parameter used to model recruitment according to power model
#' @param rec_power Parameter used to model recruitment according to power model
#' @param rec_rate Parameter used to model recruitment according to piecewise constant model
#' @param rec_duration Parameter used to model recruitment according to piecewise constant model
#'
#' @return a DTE data set
#' @export
#'

add_recruitment_time <- function(data, rec_method,
                                 rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL){

  n_patients <- nrow(data)

  if (rec_method=="power"){

    data$rec_time <- rec_period * stats::runif(n_patients)^(1/rec_power)

  }

  if (rec_method == "PWC") {
    # Parse recruitment rate and duration inputs
    rec_rate <- as.numeric(unlist(strsplit(rec_rate, ",")))
    rec_duration <- as.numeric(unlist(strsplit(rec_duration, ",")))

    # Ensure valid inputs
    if (any(rec_rate < 0)) stop("rec_rate should be non-negative")
    if (length(rec_rate) != length(rec_duration)) stop("Lengths of rec_rate and rec_duration should match")

    n_periods <- length(rec_duration)

    if (length(rec_rate) == 1) { # Simple case with only one rate
      rec <- cumsum(stats::rexp(n = n_patients, rate = rec_rate))
    } else { # Piecewise recruitment
      # Create a data frame for the piecewise periods
      df <- data.frame(
        rate = rec_rate,
        duration = rec_duration,
        period = 1:n_periods,
        finish = cumsum(rec_duration),
        lambda = rec_duration * rec_rate,
        origin = c(0, cumsum(rec_duration)[-n_periods])
      )

      # Generate the number of recruits in each period using Poisson distribution
      df$N <- sapply(df$lambda, function(x) stats::rpois(n = 1, lambda = x))

      # Check if any recruits were generated
      if (sum(df$N) == 0) {
        if (df$rate[n_periods] == 0) stop("Please specify positive rec_rate for the last period; otherwise, enrollment cannot finish.")
        rec <- cumsum(stats::rexp(n = n_patients, rate = df$rate[n_periods])) + df$finish[n_periods]
      } else {
        # Generate recruitment times for each period
        rec <- unlist(apply(df, 1, function(x) {
          sort(stats::runif(n = x[["N"]], min = x[["origin"]], max = x[["finish"]]))
        }))

        # Check if we have enough recruits
        if (length(rec) >= n_patients) {
          rec <- rec[1:n_patients]
        } else {
          # Ensure enrollment completion if needed
          if (df$rate[n_periods] == 0) stop("Please specify positive rec_rate for the last period; otherwise, enrollment cannot finish.")

          # Generate additional recruitment times if needed
          rec <- c(rec, cumsum(stats::rexp(n_patients - length(rec), rate = df$rate[n_periods])) + df$finish[n_periods])
        }
      }
    }

    # Assign recruitment times to the data frame
    data$rec_time <- rec
  }

  data$pseudo_time <- data$time + data$rec_time

  return(data)

}


