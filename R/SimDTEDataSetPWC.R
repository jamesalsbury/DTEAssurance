#' Function to simulate a data set, in the context of delayed treatment effects - using the piecewise constant recruitment method
#'
#' @param n1 Sample size in control group
#' @param n2 Sample size in treatment group
#' @param gammat treatment parameter - gamma_t
#' @param gammac control parameter - gamma_c
#' @param lambdat treatment parameter - lambda_t
#' @param lambdac control parameter - lambda_c
#' @param bigT length of delay, T
#' @param rec_rate recruitment rate
#' @param rec_duration recruitment duration
#' @return A dataframe
#' @export
SimDTEDataSetPWC <- function(n1, n2, gammat, gammac, lambdat, lambdac, bigT, rec_rate, rec_duration) {
  #Simulates the treatment data
  CP <- exp(-(lambdac*bigT)^gammac)
  u <- runif(n2)

  treatmenttime <- ifelse(u>CP, (1/lambdac)*(-log(u))^(1/gammac), (1/(lambdat^gammat)*((lambdat*bigT)^gammat-log(u)-(lambdac*bigT)^gammac))^(1/gammat))

  dataCombined <- data.frame(time = c(rweibull(n1, gammac, 1/lambdac), treatmenttime),
                             group = c(rep("Control", n1), rep("Treatment", n2)))

  n_total <- n1+n2


  if(any(rec_rate<0)){stop("rec_rate should be non-negative")}
  if(length(rec_rate)==1){#simple case with only one rate
    rec<-cumsum(stats::rexp(n=n_total,rate=rec_rate))
  }else{#piecewise
    if(length(rec_duration)!=length(rec_rate)){stop("Lengths of rec_duration and rec_rate should match")}
    n_periods<-length(rec_duration)
    df<-data.frame(rate=rec_rate,
                   duration=rec_duration,
                   period=1:n_periods,
                   finish=cumsum(rec_duration),
                   lambda=rec_duration*rec_rate,
                   origin=c(0,cumsum(rec_duration)[-n_periods]))
    df$N<-sapply(df$lambda,function(x){stats::rpois(n=1,lambda = x)})
    if (sum(df$N)==0){
      if (df$rate[n_periods]==0) stop("Please specify positive rec_rate for the last period; otherwise enrollment cannot finish.")
      rec<-c(cumsum(stats::rexp(n_total,rate=df$rate[n_periods]))+df$finish[n_periods])
    }else{
      rec<-unlist(apply(df,1,function(x){sort(stats::runif(n=x[["N"]],min=x[["origin"]],max=x[["finish"]]))}))
      if (length(rec) >= n_total){rec<-rec[1:n_total]} # if n already achieved, return first n observations
      # stop with error message if enrollment has not finished but enrollment rate for last period is less or equal with 0
      else{if (df$rate[n_periods]==0){stop("Please specify positive rec_rate for the last period; otherwise enrollment cannot finish.")}
        # Otherwise, return inter-arrival exponential times
        rec<-c(rec, cumsum(stats::rexp(n_total-nrow(rec),rate=df$rate[n_periods]))+df$finish[n_periods])
      }
    }

    dataCombined$recTime <- rec

  }

  dataCombined$pseudoTime <- dataCombined$time + dataCombined$recTime


  return(dataCombined)
}
