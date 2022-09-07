SimDTEDataSet <- function(n1, n2, gamma1, gamma2, lambda1, lambda2, bigT, recTime, censTime){
  #Simulates the treatment data
  CP <- exp(-(lambda2*bigT)^gamma2)
  u <- runif(n2)

  treatmenttime <- ifelse(u>CP, (1/lambda2)*(-log(u))^(1/gamma2), (1/(lambda1^gamma1)*((lambda1*bigT)^gamma1-log(u)-(lambda2*bigT)^gamma2))^(1/gamma1))

  dataCombined <- data.frame(time = c(rweibull(n1, gamma2, 1/lambda2), treatmenttime),
                             group = c(rep("Control", n1), rep("Treatment", n2)))


  #Adds a random uniformly distributed value, based on the recruitment time
  dataCombined$time <- dataCombined$time + runif(n1+n2, min = 0, max = recTime)

  #If the time is less than the total trial length time then the event has happened
  dataCombined$event <- dataCombined$time < censTime

  #Making it a binary value (rather than T/F), for ease to read
  dataCombined$event <- dataCombined$event*1

  #Need to set the event time to be the censoring time
  if (sum(dataCombined$event)==(n1+n2)){

  } else{
    dataCombined[dataCombined$time>censTime,]$time <- censTime
  }

  return(dataCombined)
}
