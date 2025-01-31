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


#' Function to censor a data set
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


#' Function to simulate a data set, in the context of delayed treatment effects
#'
#' @param n_C Sample size in control group
#' @param n_E Sample size in experimental treatment group
#' @param lambda_C Weibull control parameter
#' @param HRStar The post-delay hazard ratio
#' @param HRStarDist The chosen parametric distribution for the post-delay hazard ratio
#' @param gamma_C Weibull control parameter
#' @param gamma_E Weibull experimental treatment parameter
#' @param delayT Length of delay
#' @param delayTDist The chosen parametric distribution for the length of delay
#' @param P_S Probability of the Kaplan-Meier curves separating at some time
#' @param P_DTE Probability of the treatment being subject to a delay (given the K-M curves will separate)
#' @param censEvents Number of events to be censored at
#' @param censTime Time of censoring
#' @param rec_method Recruitment method
#' @param rec_period Recruitment period (for the power method)
#' @param rec_power Recruitment power (for the power method)
#' @param rec_rate Recruitment rate (for the piecewise constant method)
#' @param rec_duration Recruitment duration (for the piecewise constant method)
#' @param analysis_method Method of analysis (log-rank test or weighted log-rank test)
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test
#' @param nSims Number of simulations
#'
#' @return A value
#' @export

calc_dte_assurance <- function(n_c, n_t, lambda_c, HRStar, HRStarDist = "hist", gamma_C, gamma_E, delayT, delayTDist = "hist",
                               P_S = 1, P_DTE = 0, censEvents = NULL, censTime = NULL, rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                               analysis_method, rho = 0, gamma = 0, nSims=1e4){

  control_n <- length(lambda_C)


  assVec <- rep(NA, nSims)
  censVec <- rep(NA, nSims)

  for (i in 1:nSims){

    #Sample the control parameters
    u <- sample(1:control_n, size = 1)
    sampled_lambdac <- lambda_C[u]
    sampled_gammac <- gamma_C[u]

    #Sample the treatment effect parameters
    sampled_HRStar <- SHELF::sampleFit(HRStar, n = 1)[,HRStarDist]
    sampled_delayT <- SHELF::sampleFit(delayT, n = 1)[,delayTDist]

    #Make the simplifications
    sampled_gammae <- sampled_gammac


    dataCombined <- SimDTEDataSet(n_C, n_E, sampled_lambdac, sampled_HRStar, sampled_gammac, sampled_gammae, sampled_delayT, P_S, P_DTE,
                                  rec_method, rec_period, rec_power, rec_rate, rec_duration)


    censoredDF <- CensFunc(dataCombined, censEvents, censTime)

    dataCombined <- censoredDF$dataCombined

    censVec[i] <- censoredDF$censTime

    assVec[i] <- survivalAnalysis(dataCombined, analysis_method, alpha = 0.05, rho, gamma)


  }

  pHat <- mean(assVec)

  return(list(assurance = pHat, duration = mean(censVec), LBAssurance = pHat - 1.96*sqrt(pHat*(1-pHat)/nSims),
              UBAssurance = pHat + 1.96*sqrt(pHat*(1-pHat)/nSims)))

}


#' Test a data set for statistical significance
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

  #Performs a test on the data

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
