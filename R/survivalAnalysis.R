#' Function to test a data set for significance
#'
#' @import nph
#'
#' @param dataCombined A survival dataframe
#' @param analysis_method Method of analysis (log-rank test or weighted log-rank test)
#' @param alpha Type I error
#' @param rho Rho parameter for the Fleming-Harrington weighted log-rank test
#' @param gamma Gamma parameter for the Fleming-Harrington weighted log-rank test
#' @return An indicator of significance
#' @export


survivalAnalysis <- function(dataCombined, analysis_method = "LRT", alpha = 0.05, rho = 0, gamma = 0){

  coxmodel <- coxph(Surv(survival_time, status)~group, data = dataCombined)
  deltad <- as.numeric(exp(coef(coxmodel)))

  #Performs a test on the data

  Signif <- 0

  if (analysis_method=="LRT"){
    test <- survdiff(Surv(survival_time, status)~group, data = dataCombined)
    Signif <- (test$chisq > qchisq(1-alpha, 1) & deltad<1)
  } else if (analysis_method=="WLRT"){
    test <- nph::logrank.test(dataCombined$survival_time, dataCombined$status, dataCombined$group, rho = rho, gamma = gamma)
    Signif <- (test$test$Chisq > qchisq(1-alpha, 1) & deltad<1)
  }

  return(Signif)

}
