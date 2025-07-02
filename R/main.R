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
sim_dte <- function(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = "Exponential", gamma_c = NULL){

  #Simulate control data
  u <- runif(n_c)

  if (dist == "Exponential"){
    control_times <- -log(u)/lambda_c
  }

  if (dist == "Weibull"){
    control_times <- (1/lambda_c)*(-log(u))^(1/gamma_c)
  }

  #Simulate treatment data
  u <- runif(n_t)
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


#' Censor a survival data set
#'
#' @param data An uncensored dataframe
#' @param cens_method Method of censoring, must be either "Time" (default) or "Events"
#' @param cens_IF Information Fraction at which you wish to perform the censoring
#' @param cens_time Time at which you wish to perform the censoring
#' @return A list: censored dataframe, cens_IF, cens_time and sample size
#' @export

cens_data <- function(data, cens_method = "Time", cens_time = NULL, cens_IF = NULL, cens_events = NULL){

  if (cens_method=="Events"){
    data <- data[order(data$pseudo_time),]
    cens_time <- data$pseudo_time[cens_events]
  }

  if (cens_method=="IF"){
    data <- data[order(data$pseudo_time),]
    cens_time <- data$pseudo_time[nrow(data)*cens_IF]
  }

  data$status <- data$pseudo_time <= cens_time
  data$status <- data$status * 1
  data$enrolled <- data$rec_time < cens_time
  data <- data[data$enrolled, ]
  data$survival_time <- ifelse(data$pseudo_time > cens_time,
                               cens_time - data$rec_time,
                               data$time)

  return(list(data = data,
              cens_events = cens_events,
              cens_time = cens_time,
              sample_size = nrow(data)))
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
                               rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                               analysis_method = "LRT", alpha = 0.05, alternative = "one.sided",
                               rho = 0, gamma = 0,
                               t_star = NULL, s_star = NULL,
                               target_HR = NULL,
                               nSims=1e3){



  if (cens_method=="Events"){
    loopVec <- (n_c+n_t) > cens_events
  } else {
    loopVec <- rep(T, length(n_c))
  }

  numDiffPatients <- length(n_c)
  calc_dte_assurance_list <- list()
  for (j in 1:numDiffPatients){

    if (loopVec[j]==F){
      if (!is.null(target_HR)){
        calc_dte_assurance_list[[j]] <- list(assurance = NA,
                                             CI_assurance = NA,
                                             assurance_targetHR = NA,
                                             CI_assurance_targetHR = NA,
                                             duration = NA,
                                             sample_size = NA)
      } else {
        calc_dte_assurance_list[[j]] <- list(assurance = NA,
                                             CI_assurance = NA,
                                             duration = NA,
                                             sample_size = NA)
      }


    } else {


    assurance_vec <- rep(NA, nSims)
    if (!is.null(target_HR)) {
      assurance_targetHR_vec <- rep(NA, nSims)
    }
    cens_vec <- rep(NA, nSims)
    ss_vec <- rep(NA, nSims)

    for (i in 1:nSims){

      if (control_dist=="Exponential"){
        if (control_parameters=="Fixed"){
          if (fixed_parameters_type=="Parameter"){
            lambda_c <- lambda_c
          } else if (fixed_parameters_type=="Landmark") {
            lambda_c <-  -log(surv_t1)/t1
          }
        } else if (control_parameters=="Distribution"){
          if (control_distribution == "Elicitation"){
            lambda_c <- -log(rbeta(1, t1_Beta_a, t1_Beta_b)) / t1
          } else if (control_distribution == "MCMC"){
            lambda_c <- sample(MCMC_sample[[1]], 1)
          }
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

            solution <- nleqslv(c(1, 1), fn = WeibFunc)

            lambda_c <- solution$x[1]
            gamma_c <- solution$x[2]
          }
        } else if (control_parameters=="Distribution"){
          sampledS1to <- rbeta(1, t1_Beta_a, t1_Beta_b)
          sampledDelta1 <- rbeta(1, diff_Beta_a, diff_Beta_b)
          sampledS1toPrime <- sampledS1to - sampledDelta1

          # Solve for lambda and gamma using sampled values
          solution <- nleqslv(c(10, 1), function(params) {
            lambda <- params[1]
            k <- params[2]
            c(exp(-(t1 / lambda)^k) - sampledS1to,
              exp(-(t2 / lambda)^k) - sampledS1toPrime)
          })

          lambda_c <- 1 / solution$x[1]
          gamma_c <- solution$x[2]
        }
      }

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

      data <- sim_dte(n_c[j], n_t[j], lambda_c, delay_time, post_delay_HR, dist = control_dist, gamma_c = gamma_c)

      if (rec_method=="power"){
        data <- add_recruitment_time(data, rec_method,
                                     rec_period, rec_power)
      }

      if (rec_method == "PWC"){
        data <- add_recruitment_time(data, rec_method,
                                     rec_rate, rec_duration)
      }

      data_after_cens <- cens_data(data, cens_method, cens_time, cens_IF, cens_events)
      data <- data_after_cens$data

      cens_vec[i] <- data_after_cens$cens_time
      ss_vec[i] <- data_after_cens$sample_size


      test <- survival_test(data, analysis_method,
                            alternative, alpha = alpha,
                            rho, gamma,
                            t_star, s_star)

      assurance_vec[i] <- test$Signif

      if (!is.null(target_HR)) {
        assurance_targetHR_vec[i] <- test$Signif * (test$deltad < target_HR)
      }

    }

    assurance <- mean(assurance_vec)

    if (!is.null(target_HR)) {
      assurance_targetHR <- mean(assurance_targetHR_vec)
    }


    if (!is.null(target_HR)) {
      calc_dte_assurance_list[[j]] <- list(assurance = assurance, CI_assurance = c(assurance - 1.96*sqrt(assurance*(1-assurance)/nSims),
                                                                                   assurance + 1.96*sqrt(assurance*(1-assurance)/nSims)),
                                           assurance_targetHR = assurance_targetHR, CI_assurance_targetHR = c(assurance_targetHR - 1.96*sqrt(assurance_targetHR*(1-assurance_targetHR)/nSims),
                                                                                                              assurance_targetHR + 1.96*sqrt(assurance_targetHR*(1-assurance_targetHR)/nSims)),
                                           duration = mean(cens_vec), sample_size = mean(ss_vec))
    } else {
      calc_dte_assurance_list[[j]] <- list(assurance = assurance, CI_assurance = c(assurance - 1.96*sqrt(assurance*(1-assurance)/nSims),
                                                                                   assurance + 1.96*sqrt(assurance*(1-assurance)/nSims)),
                                           duration = mean(cens_vec),sample_size = mean(ss_vec))
    }
    }
  }

  return(calc_dte_assurance_list = calc_dte_assurance_list)


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
#' @param target_HR Hazard Ratio that needs to be met for success
#' @return An indicator of significance
#' @export


survival_test <- function(data, analysis_method = "LRT", alternative = "one.sided", alpha = 0.05, rho = 0, gamma = 0,
                          t_star = NULL, s_star = NULL){

  coxmodel <- coxph(Surv(survival_time, status)~group, data = data)
  deltad <- as.numeric(exp(coef(coxmodel)))

  Signif <- 0

  if (analysis_method=="LRT"){
    test <- survdiff(Surv(survival_time, status)~group, data = data)
    if (alternative=="one.sided"){
      Signif <- (test$chisq > qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$chisq > qchisq(1-alpha/2, 1)
    }

  } else if (analysis_method=="WLRT"){
    test <- nph::logrank.test(data$survival_time, data$status, data$group, rho = rho, gamma = gamma)
    if (alternative=="one.sided"){
      Signif <- (test$test$Chisq > qchisq(1-alpha, 1) & deltad<1)
    } else {
      Signif <- test$test$Chisq > qchisq(1-alpha/2, 1)
    }
  } else if (analysis_method=="MW"){
    test <- nphRCT::wlrt(Surv(survival_time, status)~group,
                         data = data, method = "mw",
                         t_star = t_star, s_star = s_star)
    if (alternative=="one.sided"){
      Signif <- test$z < qnorm(alpha)
    } else {
      Signif <- test$z < qnorm(alpha/2) | test$z > qnorm(1-alpha/2)
    }
  }

  return(list(Signif = Signif, deltad = deltad))

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
#' @param cens_IF Number of events at which you wish to perform the censoring (must be less than n_c + n_t)
#' @param rec_method Recruitment method, must be one of "power" or "PWC" (piecewise constant)
#' @param rec_period Parameter used to model recruitment according to power model
#' @param rec_power Parameter used to model recruitment according to power model
#' @param rec_rate Parameter used to model recruitment according to piecewise constant model
#' @param rec_duration Parameter used to model recruitment according to piecewise constant model
#' @param alpha_spending Cumulative alpha spending
#' @param beta_spending Cumulative beta spending
#' @param IF_vec Vector of information fraction's
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

      design <- getDesignGroupSequential(typeOfDesign = "asUser",
                                         informationRates = info_rates,
                                         userAlphaSpending = alpha_spending,
                                         typeBetaSpending = "bsUser",
                                         userBetaSpending = beta_spending)

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
          lambda_c <- -log(rbeta(1, t1_Beta_a, t1_Beta_b)) / t1
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

            solution <- nleqslv(c(1, 1), fn = WeibFunc)

            lambda_c <- solution$x[1]
            gamma_c <- solution$x[2]
          }
        } else if (control_parameters=="Distribution"){
          sampledS1to <- rbeta(1, t1_Beta_a, t1_Beta_b)
          sampledDelta1 <- rbeta(1, diff_Beta_a, diff_Beta_b)
          sampledS1toPrime <- sampledS1to - sampledDelta1

          # Solve for lambda and gamma using sampled values
          solution <- nleqslv(c(10, 1), function(params) {
            lambda <- params[1]
            k <- params[2]
            c(exp(-(t1 / lambda)^k) - sampledS1to,
              exp(-(t2 / lambda)^k) - sampledS1toPrime)
          })

          lambda_c <- 1 / solution$x[1]
          gamma_c <- solution$x[2]
        }
      }

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
        coxmodel <- coxph(Surv(survival_time, status) ~ group, data = data_after_cens$data)
        unique_IF_DF$`Z-Score`[j] <- -(coef(summary(coxmodel))[, 4])
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


calc_BPP_hist <- function(n_c, n_t,
                          control_dist = "Exponential",
                          t1 = NULL, t2 = NULL,
                          t1_Beta_a = NULL, t1_Beta_b = NULL,
                          diff_Beta_a = NULL, diff_Beta_b = NULL,
                          delay_time_SHELF, delay_time_dist = "hist",
                          post_delay_HR_SHELF, post_delay_HR_dist = "hist",
                          P_S = 1, P_DTE = 0, cens_events = NULL, IF = NULL,
                          rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                          type_one_error = NULL, N = 50, M = 50){


  outerBPPVec <- rep(NA, N)

  for (i in 1:N){

    if (control_dist=="Exponential"){
      lambda_c <- -log(rbeta(1, t1_Beta_a, t1_Beta_b)) / t1
    } else if (control_dist=="Weibull"){
      sampledS1to <- rbeta(1, t1_Beta_a, t1_Beta_b)
      sampledDelta1 <- rbeta(1, diff_Beta_a, diff_Beta_b)
      sampledS1toPrime <- sampledS1to - sampledDelta1

      # Solve for lambda and gamma using sampled values
      solution <- nleqslv(c(10, 1), function(params) {
        lambda <- params[1]
        k <- params[2]
        c(exp(-(t1 / lambda)^k) - sampledS1to,
          exp(-(t2 / lambda)^k) - sampledS1toPrime)
      })

      lambda_c <- 1 / solution$x[1]
      gamma_c <- solution$x[2]
    }

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

    data <- sim_dte(n_c, n_t, lambda_c, delay_time, post_delay_HR, dist = control_dist, gamma_c = gamma_c)

    if (rec_method=="power"){
      data <- add_recruitment_time(data, rec_method,
                                   rec_period, rec_power)
    }

    if (rec_method == "PWC"){
      data <- add_recruitment_time(data, rec_method,
                                   rec_rate, rec_duration)
    }


    data_after_cens <- cens_data(data, cens_method = "Events", cens_events = cens_events*IF)
    data <- data_after_cens$data

    data <- data[order(data$group), ]

    #Choose the correct elicited distribution for bigT
    if (delay_time_dist == "beta") {
      distParambigT <- paste0("bigT2 ~ dbeta(", delay_time_SHELF$Beta[1], ", ", delay_time_SHELF$Beta[2], ")")
    } else if (delay_time_dist == "gamma") {
      distParambigT <- paste0("bigT2 ~ dgamma(", delay_time_SHELF$Gamma[1], ", ", delay_time_SHELF$Gamma[2], ")")
    } else if (delay_time_dist == "lognormal") {
      distParambigT <- paste0("bigT2 ~ dlnorm(", delay_time_SHELF$Log.normal[1], ", ", 1/delay_time_SHELF$Log.normal[2]^2, ")")
    }

    #Choose the correct elicited distribution for HR*
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

    model = jags.model(textConnection(modelString), data = list(datTimes = data$survival_time,
                                                                datEvent = data$status, n = sum(data$group=="Control"),
                                                                m=nrow(data),
                                                                P_S = P_S,
                                                                P_DTE = P_DTE), quiet = T)

    update(model, n.iter=50, progress.bar = "none")
    output=coda.samples(model=model, variable.names=c("HR", "bigT", "lambda_c"), n.iter = 100, progress.bar = "none")

    #The number of unenrolled patients in each group
    cPatientsLeft <- n_c - sum(data$group=="Control")
    tPatientsLeft <- n_t - sum(data$group=="Treatment")

    #Extract realisations from the MCMC
    HRoutput <- as.numeric(unlist(output[,1]))
    bigToutput <- as.numeric(unlist(output[,2]))
    lambda_coutput <- as.numeric(unlist(output[,3]))

    BPPVec <- rep(NA, M)

    for (j in 1:M){

      #Need to sample a recuitment time for the unenrolled patients
      #NEEDS CHANGING
      unenrolledrec_times <- runif(cPatientsLeft+tPatientsLeft, data_after_cens$cens_time, rec_period)

      #Sample values from the MCMC output
      sampledHR <- sample(HRoutput, 1)
      sampledbigT <- sample(bigToutput, 1)
      sampledlambda_c <- sample(lambda_coutput, 1)
      sampledlambda_t <- sampledlambda_c*sampledHR


      #For the unenrolled data, we can sample the remaining data according to the updated (sampled) parameters
      CP <- exp(-(sampledlambda_c*sampledbigT))
      u <- runif(tPatientsLeft)

      unenrolledData <- data.frame(time = c(rexp(cPatientsLeft, rate = sampledlambda_c), ifelse(u>CP, (-log(u))/sampledlambda_c, (1/sampledlambda_t)*(sampledbigT*sampledlambda_t-log(u)-sampledbigT*sampledlambda_c))), group = c(rep("Control", cPatientsLeft),
                                                                                                                                                                                                                                   rep("Treatment", tPatientsLeft)), rec_time = unenrolledrec_times)

      unenrolledData$pseudo_time <- unenrolledData$time + unenrolledData$rec_time


      #Extracting the observations that were censored at the IA
      censoredData <- data[data$status==0,]


      #Number of censored observations in each group
      cCensored <- sum(censoredData$group=="Control")
      tCensored <- sum(censoredData$group=="Treatment")

      #Extracting the censored observations in the control group
      cCensoredData <- censoredData %>%
        filter(group=="Control")

      #Adding a exp(sampledlambda_c) value to the censored value
      cCensoredData$finalsurvTime <- cCensoredData$survival_time + rexp(cCensored, rate = sampledlambda_c)

      #Calculating the psuedo time
      cCensoredData$finalPsuedoTime <- cCensoredData$rec_time + cCensoredData$finalsurvTime


      #Extacting the observations in the treatment group which may still be influenced by the delay (their observation time is smaller than the sampled delay time)
      tBeforeDelay <- censoredData %>%
        filter(group=="Treatment") %>%
        filter(survival_time < sampledbigT)

      #Extracting the observations in the treatment group which will not be influenced by the delay (their observation time is bigger than the sampled delay time)
      tAfterDelay <- censoredData %>%
        filter(group=="Treatment") %>%
        filter(survival_time > sampledbigT)

      #As these observations are still subject to a delay, we add on a Exp(lambda_c) (lambdac) time
      tBeforeDelay$IASurv <- tBeforeDelay$survival_time + rexp(nrow(tBeforeDelay), rate = sampledlambda_c)

      #Extracting the observations in which the survival time is smaller than the sampled delay time
      tBeforeDelay1 <- tBeforeDelay %>%
        filter(IASurv < sampledbigT)

      #Extracting the observations in which the survival time is bigger than the sampled delay time
      tBeforeDelay2 <- tBeforeDelay %>%
        filter(IASurv > sampledbigT)

      #For the observations in which the survival time is bigger, we sample a Exp(lambda_t) and add it to the sampled delay time
      tBeforeDelay2$IASurv2 <- sampledbigT + rexp(nrow(tBeforeDelay2), rate = sampledlambda_t)

      #For the observations not influenced by the delay, we sample a Exp(lambda_t) time and add it to the current survival time
      tAfterDelay$IASurv <- tAfterDelay$survival_time + rexp(nrow(tAfterDelay), rate = sampledlambda_t)

      #Calculate the pseudo time for all the data frames
      tBeforeDelay1$IApsuedoTime <- tBeforeDelay1$IASurv + tBeforeDelay1$rec_time
      tBeforeDelay2$IApsuedoTime <- tBeforeDelay2$IASurv2 + tBeforeDelay2$rec_time
      tAfterDelay$IApsuedoTime <- tAfterDelay$IASurv + tAfterDelay$rec_time

      #Only keeping the columns of interest
      cCensoredData <- cCensoredData[,c(8, 2, 3, 9)]
      tBeforeDelay1 <- tBeforeDelay1[,c(8, 2, 3, 9)]
      tBeforeDelay2 <- tBeforeDelay2[,c(9, 2, 3, 10)]
      tAfterDelay <- tAfterDelay[,c(8, 2, 3, 9)]

      #Keeping the column names consistent
      colnames(cCensoredData) <- c("time", "group", "rec_time", "pseudo_time")
      colnames(tBeforeDelay1) <- c("time", "group", "rec_time", "pseudo_time")
      colnames(tBeforeDelay2) <- c("time", "group", "rec_time", "pseudo_time")
      colnames(tAfterDelay) <- c("time", "group", "rec_time", "pseudo_time")

      #Only keeping observations from the censored data set which are dead
      finalDataset <- data %>%
        filter(status==1)

      finalDataset <- finalDataset[,1:4]

      #Combining all the above data sets
      finalDataset <- rbind(finalDataset, tBeforeDelay1)
      finalDataset <- rbind(finalDataset, tBeforeDelay2)
      finalDataset <- rbind(finalDataset, tAfterDelay)
      finalDataset <- rbind(finalDataset, unenrolledData)
      finalDataset <- rbind(finalDataset, cCensoredData)



      #Making sure the final data set is correct
      censTime1 <- sort(finalDataset$pseudo_time)[cens_events]
      finalDataset$status <- finalDataset$pseudo_time <= censTime1
      finalDataset$status <- finalDataset$status*1
      finalDataset$enrolled <- finalDataset$rec_time <= censTime1
      finalDataset <-  finalDataset[finalDataset$enrolled==T,]
      finalDataset$survival_time <- ifelse(finalDataset$pseudo_time>censTime1, censTime1  - finalDataset$rec_time, finalDataset$time)

      #Testing for significance
      test <- survdiff(Surv(survival_time, status)~group, data = finalDataset)

      #Making sure the significance is in the correct direction
      coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDataset)
      deltad <- as.numeric(exp(coef(coxmodel)))


      BPPVec[j] <- (test$chisq > qchisq(0.95, 1) & deltad<1)

    }

    outerBPPVec[i] <- mean(BPPVec)

  }

  return(list(outerBPPVec = outerBPPVec))

}
