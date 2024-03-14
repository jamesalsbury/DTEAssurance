#' Function to simulate a data set, in the context of delayed treatment effects
#'
#' @param n_C Sample size in control group
#' @param n_E Sample size in experimental treatment group
#' @param lambda_C Weibull control parameter
#' @param lambda_E Weibull experimental treatment parameter
#' @param gamma_C Weibull control parameter
#' @param gamma_E Weibull experimental treatment parameter
#' @param delayT Length of delay
#' @param rec_method Recruitment method
#' @param rec_period Recruitment period (for the power method)
#' @param rec_power Recruitment power (for the power method)
#' @param rec_rate Recruitment rate (for the piecewise constant method)
#' @param rec_duration Recruitment duration (for the piecewise constant method)
#'
#' @return A dataframe
#' @export
SimDTEDataSet <- function(n_C, n_E, lambda_C, lambda_E, gamma_C, gamma_E, delayT,
                               rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL) {

  #Simulates the control data
  u <- runif(n_C)
  controlTimes <- (1/lambda_C)*(-log(u))^(1/gamma_C)

  #Simulates the treatment data
  CP <- exp(-(lambda_C*delayT)^gamma_C)
  u <- runif(n_E)
  treatmentTimes <- ifelse(u>CP, (1/lambda_C)*(-log(u))^(1/gamma_C),
                           (1/(lambda_E^gamma_E)*((lambda_E*delayT)^gamma_E-log(u)-(lambda_C*delayT)^gamma_C))^(1/gamma_E))


  #Combines the control and treatment data
  dataCombined <- data.frame(time = c(controlTimes, treatmentTimes),
                             group = c(rep("Control", n_C), rep("Treatment", n_E)))

  n_total <- n_C + n_E

  if (rec_method=="power"){

    dataCombined$recTime <- rec_period * stats::runif(n_total)^(1/rec_power)

    dataCombined$pseudoTime <- dataCombined$time + dataCombined$recTime
  }

  if (rec_method=="PWC"){
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
  }



  return(dataCombined)
}
