#' Function to simulate a data set, in the context of delayed treatment effects
#'
#' @param n_C Sample size in control group
#' @param n_E Sample size in experimental treatment group
#' @param lambda_C Weibull control parameter
#' @param HRSTar The post-delay hazard ratio
#' @param HRStarDist The chosen parametric distribution for the post-delay hazard ratio
#' @param gamma_C Weibull control parameter
#' @param gamma_E Weibull experimental treatment parameter
#' @param delayT Length of delay
#' @param delayTDist The chosen parametric distribution for the length of delay
#' @param P_S Probability of the Kaplan-Meier curves separating at some time
#' @param P_DTE Probability of the treatment being subject to a delay (given the K-M curves will separate)
#' @param nEvents Number of events to be censored at
#' @param censTime Time of censoring
#' @param rec_method Recruitment method
#' @param rec_period Recruitment period (for the power method)
#' @param rec_power Recruitment power (for the power method)
#' @param rec_rate Recruitment rate (for the piecewise constant method)
#' @param rec_duration Recruitment duration (for the piecewise constant method)
#' @param analysis_method Method of analysis (log-rank test or weighted log-rank test)
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test
#'
#' @return A value
#' @export

calculateAssurance <- function(n_C, n_E, lambda_C, HRStar, HRStarDist = "hist", gamma_C, gamma_E, delayT, delayTDist = "hist",
                               P_S = 1, P_DTE = 0, censEvents=NULL, censTime = NULL, rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
                               analysis_method, rho = 0, gamma = 0, nSims=1e5){

  control_n <- length(lambda_C)

  assVec <- rep(NA, nSims)

  for (i in 1:nSims){

    #Sample the control parameters
    u <- sample(1:control_n, size = 1)
    sampled_lambdac <- lambda_C[u]
    sampled_gammac <- gamma_C[u]

    #Sample the treatment effect parameters
    sampled_HRStar <- SHELF::sampleFit(HRStar, n = 1)[,HRStarDist]
    sampled_delayT <- SHELF::sampleFit(delayT, n = 1)[,delayTDist]

    #Make the simplifications
    sampled_lambdae <- sampled_HRStar*sampled_lambdac
    sampled_gammae <- sampled_gammac

    dataCombined <- SimDTEDataSet(n_C, n_E, lambda_C, sampled_HRStar, sampled_gammac, sampled_gammae, sampled_delayT, P_S, P_DTE,
                              rec_method, rec_period, rec_power, rec_rate, rec_duration)


    dataCombined <- CensFunc(dataCombined, censEvents, censTime)

    assVec[i] <- survivalAnalysis(dataCombined, analysis_method, alpha = 0.05, rho, gamma)


  }

}


