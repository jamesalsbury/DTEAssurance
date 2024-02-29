#' Function to simulate a data set, in the context of delayed treatment effects - using the power recruitment method
#'
#' @param n1 Sample size in control group
#' @param n2 Sample size in treatment group
#' @param gammat treatment parameter - gamma_t
#' @param gammac control parameter - gamma_c
#' @param lambdat treatment parameter - lambda_t
#' @param lambdac control parameter - lambda_c
#' @param bigT length of delay, T
#' @param rec_period recruitment period
#' @param rec_power recruitment power
#' @return A dataframe
#' @export
SimDTEDataSetPower <- function(n1, n2, gammat, gammac, lambdat, lambdac, bigT, rec_period, rec_power) {
  #Simulates the treatment data
  CP <- exp(-(lambdac*bigT)^gammac)
  u <- runif(n2)

  treatmenttime <- ifelse(u>CP, (1/lambdac)*(-log(u))^(1/gammac), (1/(lambdat^gammat)*((lambdat*bigT)^gammat-log(u)-(lambdac*bigT)^gammac))^(1/gammat))

  dataCombined <- data.frame(time = c(rweibull(n1, gammac, 1/lambdac), treatmenttime),
                             group = c(rep("Control", n1), rep("Treatment", n2)))

  n_total <- n1+n2
  dataCombined$recTime <- rec_period * stats::runif(n_total)^(1/rec_power)

  dataCombined$pseudoTime <- dataCombined$time + dataCombined$recTime

  return(dataCombined)
}
