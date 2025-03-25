

# BPPFunc <- function(dataset, numPatients, numIAEvents, numFinalEvents, recTime, elicitedDists){
#
#   BPPOutcome <- CensFunc(dataset, numIAEvents)
#
#   dataCombined <- BPPOutcome$dataCombined
#
#   dataCombined <- dataCombined[order(dataCombined$group), ]
#
#   #Choose the correct elicited distribution for bigT
#   if (elicitedDists$d[1] == "beta") {
#     distParambigT <- paste0("bigT2 ~ dbeta(", elicitedDists$fit1$Beta[1], ", ", elicitedDists$fit1$Beta[2], ")")
#   } else if (elicitedDists$d[1] == "gamma") {
#     distParambigT <- paste0("bigT2 ~ dgamma(", elicitedDists$fit1$Gamma[1], ", ", elicitedDists$fit1$Gamma[2], ")")
#   } else if (elicitedDists$d[1] == "lognormal") {
#     distParambigT <- paste0("bigT2 ~ dlnorm(", elicitedDists$fit1$Log.normal[1], ", ", 1/elicitedDists$fit1$Log.normal[2]^2, ")")
#   }
#
#   #Choose the correct elicited distribution for HR*
#   if (elicitedDists$d[2] == "beta") {
#     distParamHR <- paste0("HR2 ~ dbeta(", elicitedDists$fit2$Beta[1], ", ", elicitedDists$fit2$Beta[2], ")")
#   } else if (elicitedDists$d[2] == "gamma") {
#     distParamHR <- paste0("HR2 ~ dgamma(", elicitedDists$fit2$Gamma[1], ", ", elicitedDists$fit2$Gamma[2], ")")
#   } else if (elicitedDists$d[2] == "lognormal") {
#     distParamHR <- paste0("HR2 ~ dlnorm(", elicitedDists$fit2$Log.normal[1], ", ", 1/elicitedDists$fit2$Log.normal[2]^2, ")")
#   } else if (elicitedDists$d[2] == "student-t") {
#     distParamHR <- paste0("HR2 ~ dt(", elicitedDists$fit2$Student.t[1], ", ", elicitedDists$fit2$Student.t[2], ", ", elicitedDists$fit2$Student.t[3], ")")
#   } else if (elicitedDists$d[2] == "normal") {
#     distParamHR <- paste0("HR2 ~ dnorm(", elicitedDists$fit2$Normal[1], ", ", 1/elicitedDists$fit2$Normal[2]^2, ")")
#   }
#
#
#   #JAGS code which calculates posterior distributions
#
#   modelString <- paste0(
#     "data {\n",
#     "  for (j in 1:m){\n",
#     "    zeros[j] <- 0\n",
#     "  }\n",
#     "}\n",
#     "\n",
#     "model {\n",
#     "  C <- 10000\n",
#     "  for (i in 1:n){\n",
#     "    zeros[i] ~ dpois(zeros.mean[i])\n",
#     "    zeros.mean[i] <-  -l[i] + C\n",
#     "    l[i] <- ifelse(datEvent[i]==1, log(lambda2)-(lambda2*datTimes[i]), -(lambda2*datTimes[i]))\n",
#     "  }\n",
#     "  for (i in (n+1):m){\n",
#     "    zeros[i] ~ dpois(zeros.mean[i])\n",
#     "    zeros.mean[i] <-  -l[i] + C\n",
#     "    l[i] <- ifelse(datEvent[i]==1, ifelse(datTimes[i]<bigT, log(lambda2)-(lambda2*datTimes[i]), log(lambda1)-lambda1*(datTimes[i]-bigT)-(bigT*lambda2)),\n",
#     "      ifelse(datTimes[i]<bigT, -(lambda2*datTimes[i]), -(lambda2*bigT)-lambda1*(datTimes[i]-bigT)))\n",
#     "  }\n",
#     " \n",
#     "    lambda2 ~ dbeta(1, 1)T(0,)\n",
#     "   \n",
#     "    mixT ~ dbern(1-P_S*P_DTE)\n",
#     "    bigT <- mixT * bigT1 + (1-mixT) * bigT2\n",
#     "    bigT1 ~ dnorm(0, 100)\n",
#     "    ", distParambigT, "\n",
#     "   \n",
#     "    mixHR ~ dbern(1-P_S)\n",
#     "    HR <- mixHR * HR1 + (1-mixHR) * HR2\n",
#     "    HR1 ~ dnorm(1, 10000)\n",
#     "    ", distParamHR, "\n",
#     "   \n",
#     "    lambda1 <- lambda2*HR\n",
#     "}"
#   )
#
#   model = jags.model(textConnection(modelString), data = list(datTimes = dataCombined$survival_time,
#                                                               datEvent = dataCombined$status, n = sum(dataCombined$group=="Control"),
#                                                               m=nrow(dataCombined),
#                                                               P_S = elicitedDists$P_S,
#                                                               P_DTE = elicitedDists$P_DTE), quiet = T)
#
#   update(model, n.iter=50, progress.bar = "none")
#   output=coda.samples(model=model, variable.names=c("HR", "bigT", "lambda2"), n.iter = 100, progress.bar = "none")
#
#
#   #The number of unenrolled patients in each group
#   cPatientsLeft <- numPatients - sum(dataCombined$group=="Control")
#   tPatientsLeft <- numPatients - sum(dataCombined$group=="Treatment")
#
#   #Extract realisations from the MCMC
#   HRoutput <- as.numeric(unlist(output[,1]))
#   bigToutput <- as.numeric(unlist(output[,2]))
#   lambda2output <- as.numeric(unlist(output[,3]))
#
#   BPPVec <- rep(NA, 50)
#
#   for (j in 1:50){
#
#     #Sampling the recruitment times for the unenrolled patients
#     unenrolledRecTimes <- runif(cPatientsLeft+tPatientsLeft, BPPOutcome$censTime, recTime)
#
#     #Sample values from the MCMC output
#     sampledHR <- sample(HRoutput, 1)
#     sampledbigT <- sample(bigToutput, 1)
#     sampledlambda2 <- sample(lambda2output, 1)
#     sampledlambda1 <- sampledlambda2*sampledHR
#
#
#     #For the unenrolled data, we can sample the remaining data according to the updated (sampled) parameters
#     CP <- exp(-(sampledlambda2*sampledbigT))
#     u <- runif(tPatientsLeft)
#
#     unenrolledData <- data.frame(time = c(rexp(cPatientsLeft, rate = sampledlambda2), ifelse(u>CP, (-log(u))/sampledlambda2, (1/sampledlambda1)*(sampledbigT*sampledlambda1-log(u)-sampledbigT*sampledlambda2))), group = c(rep("Control", cPatientsLeft),
#                                                                                                                                                                                                                             rep("Treatment", tPatientsLeft)), recTime = unenrolledRecTimes)
#
#     unenrolledData$pseudoTime <- unenrolledData$time + unenrolledData$recTime
#
#
#     #Extracting the observations that were censored at the IA
#     censoredData <- dataCombined[dataCombined$status==0,]
#
#     #Number of censored observations in each group
#     cCensored <- sum(censoredData$group=="Control")
#     tCensored <- sum(censoredData$group=="Treatment")
#
#     #Extracting the censored observations in the control group
#     cCensoredData <- censoredData %>%
#       filter(group=="Control")
#
#     #Adding a exp(sampledlambda2) value to the censored value
#     cCensoredData$finalsurvTime <- cCensoredData$survival_time + rexp(cCensored, rate = sampledlambda2)
#
#     #Calculating the psuedo time
#     cCensoredData$finalPsuedoTime <- cCensoredData$recTime + cCensoredData$finalsurvTime
#
#     #Extacting the observations in the treatment group which may still be influenced by the delay (their observation time is smaller than the sampled delay time)
#     tBeforeDelay <- censoredData %>%
#       filter(group=="Treatment") %>%
#       filter(survival_time < sampledbigT)
#
#     #Extracting the observations in the treatment group which will not be influenced by the delay (their observation time is bigger than the sampled delay time)
#     tAfterDelay <- censoredData %>%
#       filter(group=="Treatment") %>%
#       filter(survival_time > sampledbigT)
#
#     #As these observations are still subject to a delay, we add on a Exp(lambda2) (lambdac) time
#     tBeforeDelay$IASurv <- tBeforeDelay$survival_time + rexp(nrow(tBeforeDelay), rate = sampledlambda2)
#
#     #Extracting the observations in which the survival time is smaller than the sampled delay time
#     tBeforeDelay1 <- tBeforeDelay %>%
#       filter(IASurv < sampledbigT)
#
#     #Extracting the observations in which the survival time is bigger than the sampled delay time
#     tBeforeDelay2 <- tBeforeDelay %>%
#       filter(IASurv > sampledbigT)
#
#     #For the observations in which the survival time is bigger, we sample a Exp(lambda1) and add it to the sampled delay time
#     tBeforeDelay2$IASurv2 <- sampledbigT + rexp(nrow(tBeforeDelay2), rate = sampledlambda1)
#
#     #For the observations not influenced by the delay, we sample a Exp(lambda1) time and add it to the current survival time
#     tAfterDelay$IASurv <- tAfterDelay$survival_time + rexp(nrow(tAfterDelay), rate = sampledlambda1)
#
#     #Calculate the pseudo time for all the data frames
#     tBeforeDelay1$IApsuedoTime <- tBeforeDelay1$IASurv + tBeforeDelay1$recTime
#     tBeforeDelay2$IApsuedoTime <- tBeforeDelay2$IASurv2 + tBeforeDelay2$recTime
#     tAfterDelay$IApsuedoTime <- tAfterDelay$IASurv + tAfterDelay$recTime
#
#     #Only keeping the columns of interest
#     cCensoredData <- cCensoredData[,c(8, 2, 3, 9)]
#     tBeforeDelay1 <- tBeforeDelay1[,c(8, 2, 3, 9)]
#     tBeforeDelay2 <- tBeforeDelay2[,c(9, 2, 3, 10)]
#     tAfterDelay <- tAfterDelay[,c(8, 2, 3, 9)]
#
#     #Keeping the column names consistent
#     colnames(cCensoredData) <- c("time", "group", "recTime", "pseudoTime")
#     colnames(tBeforeDelay1) <- c("time", "group", "recTime", "pseudoTime")
#     colnames(tBeforeDelay2) <- c("time", "group", "recTime", "pseudoTime")
#     colnames(tAfterDelay) <- c("time", "group", "recTime", "pseudoTime")
#
#     #Only keeping observations from the censored data set which are dead
#     finalDataset <- dataCombined %>%
#       filter(status==1)
#
#     finalDataset <- finalDataset[,1:4]
#
#     #Combining all the above data sets
#     finalDataset <- rbind(finalDataset, tBeforeDelay1)
#     finalDataset <- rbind(finalDataset, tBeforeDelay2)
#     finalDataset <- rbind(finalDataset, tAfterDelay)
#     finalDataset <- rbind(finalDataset, unenrolledData)
#     finalDataset <- rbind(finalDataset, cCensoredData)
#
#     #Making sure the final data set is correct
#     censTime1 <- sort(finalDataset$pseudoTime)[numFinalEvents]
#     finalDataset$status <- finalDataset$pseudoTime <= censTime1
#     finalDataset$status <- finalDataset$status*1
#     finalDataset$enrolled <- finalDataset$recTime <= censTime1
#     finalDataset <-  finalDataset[finalDataset$enrolled==T,]
#     finalDataset$survival_time <- ifelse(finalDataset$pseudoTime>censTime1, censTime1  - finalDataset$recTime, finalDataset$time)
#
#     #Testing for significance
#     test <- survdiff(Surv(survival_time, status)~group, data = finalDataset)
#
#     #kmfit <- survfit(Surv(survival_time, status)~group, data = finalDataset)
#     #plot(kmfit, col = c("blue", "red"), xlim=c(0,50))
#
#     #Making sure the significance is in the correct direction
#     coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDataset)
#     deltad <- as.numeric(exp(coef(coxmodel)))
#
#
#     BPPVec[j] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
#
#   }
#
#   return(list(BPP = mean(BPPVec), BPPSS = nrow(dataCombined), BPPCensTime = BPPOutcome$censTime, propEffect = propEffect))
#
# }

proposedRuleFunc <- function(dataCombined, numEventsRequired, monthsDelay, propEvents){

  # Sorting the pseudoTime
  sortedPseudoTimes <- sort(dataCombined$pseudoTime)

  # Initialize the proportion vector
  propVec <- numeric(numEventsRequired)

  # Calculate cutoff index
  cutoff_index <- ceiling(numEventsRequired / 2)

  # Loop through from cutoff_index to numEventsRequired
  for (k in cutoff_index:numEventsRequired) {
    censTime <- sortedPseudoTimes[k]

    # Calculate the status for the entire dataset
    status <- as.integer(dataCombined$pseudoTime <= censTime)

    # Filter data based on recTime <= censTime
    filteredData <- dataCombined[dataCombined$recTime <= censTime, ]

    # Ensure status and filteredData are appropriately handled
    survival_time <- ifelse(status == 1, filteredData$time, censTime - filteredData$recTime)

    # Ensure that survival_time is calculated correctly
    propVec[k] <- mean(survival_time[status == 1] > monthsDelay)
  }

  # Determine Stop1 and Stop2
  if (sum(propVec > propEvents) == 0) {
    Stop1 <- numEventsRequired
    Stop2 <- numEventsRequired
  } else {
    firstTrueIndex <- which(propVec > propEvents)[1]
    Stop1 <- max(ceiling(numEventsRequired * 0.5), firstTrueIndex)
    Stop2 <- max(ceiling(numEventsRequired * 0.75), firstTrueIndex)
  }


  Prop1Outcome <- 1
  Prop1Cens <- CensFunc(dataCombined, Stop1)
  Prop1 <- Prop1Cens$dataCombined
  Prop1censTime <- Prop1Cens$censTime
  Prop1SS <- Prop1Cens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = Prop1)
  deltad <- as.numeric(exp(coef(coxmodel)))
  if (deltad > 1) Prop1Outcome <- 0

  Prop2Outcome <- 1
  Prop2Cens <- CensFunc(dataCombined, Stop2)
  Prop2 <- Prop2Cens$dataCombined
  Prop2censTime <- Prop2Cens$censTime
  Prop2SS <- Prop2Cens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = Prop2)
  deltad <- as.numeric(exp(coef(coxmodel)))
  if (deltad > 1) Prop2Outcome <- 0

  PropFinalOutcome <- 0
  PropFinalCens <- CensFunc(dataCombined, numEventsRequired)
  PropFinal <- PropFinalCens$dataCombined
  PropFinalcensTime <- PropFinalCens$censTime
  PropFinalSS <- PropFinalCens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = PropFinal)
  deltad <- as.numeric(exp(coef(coxmodel)))
  PropLRT <- survdiff(Surv(time, status) ~ group, data = PropFinal)
  if (PropLRT$chisq > qchisq(0.95, 1) & deltad < 1) PropFinalOutcome <- 1

  return(list(Prop1Outcome = Prop1Outcome, Prop2Outcome = Prop2Outcome, PropFinalOutcome = PropFinalOutcome,
              Prop1SS = Prop1SS, Prop2SS = Prop2SS, PropFinalSS = PropFinalSS,
              Prop1censTime = Prop1censTime, Prop2censTime = Prop2censTime, PropFinalcensTime = PropFinalcensTime))


}

WieandRuleFunc <- function(dataCombined, numEventsRequired){

  Wieand1Outcome <- 1
  Wieand1Cens <- CensFunc(dataCombined, numEventsRequired*0.5)
  Wieand1 <- Wieand1Cens$dataCombined
  Wieand1censTime <- Wieand1Cens$censTime
  Wieand1SS <- Wieand1Cens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = Wieand1)
  deltad <- as.numeric(exp(coef(coxmodel)))
  if (deltad > 1) Wieand1Outcome <- 0

  Wieand2Outcome <- 1
  Wieand2Cens <- CensFunc(dataCombined, numEventsRequired*0.75)
  Wieand2 <- Wieand2Cens$dataCombined
  Wieand2censTime <- Wieand2Cens$censTime
  Wieand2SS <- Wieand2Cens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = Wieand2)
  deltad <- as.numeric(exp(coef(coxmodel)))
  if (deltad > 1) Wieand2Outcome <- 0

  WieandFinalOutcome <- 0
  WieandFinalCens <- CensFunc(dataCombined, numEventsRequired)
  WieandFinal <- WieandFinalCens$dataCombined
  WieandFinalcensTime <- WieandFinalCens$censTime
  WieandFinalSS <- WieandFinalCens$SS
  coxmodel <- coxph(Surv(survival_time, status) ~ group, data = WieandFinal)
  deltad <- as.numeric(exp(coef(coxmodel)))
  WieandLRT <- survdiff(Surv(time, status) ~ group, data = WieandFinal)
  if (WieandLRT$chisq > qchisq(0.95, 1) & deltad < 1) WieandFinalOutcome <- 1

  return(list(Wieand1Outcome = Wieand1Outcome, Wieand2Outcome = Wieand2Outcome, WieandFinalOutcome = WieandFinalOutcome,
              Wieand1SS = Wieand1SS, Wieand2SS = Wieand2SS, WieandFinalSS = WieandFinalSS,
              Wieand1censTime = Wieand1censTime, Wieand2censTime = Wieand2censTime, WieandFinalcensTime = WieandFinalcensTime))


}

group_sequential_decision <- function(z_scores, critical_values, futility_values, sample_sizes, durations) {
  num_stages <- length(z_scores)

  for (i in seq_len(num_stages - 1)) {  # Loop through all interim stages, not including the final one
    # Check futility if a futility boundary is provided
    if (!is.na(futility_values[i]) && z_scores[i] < futility_values[i]) {
      return(list(
        status = paste("Stopped for futility at IA", i),
        successful = FALSE,
        analysis = i,
        z_score = z_scores[i],
        sample_size = sample_sizes[i],
        duration = durations[i]
      ))
    }

    # Check efficacy if a critical value is provided
    if (!is.na(critical_values[i]) && z_scores[i] > critical_values[i]) {
      return(list(
        status = paste("Stopped for efficacy at IA", i),
        successful = TRUE,
        analysis = i,
        z_score = z_scores[i],
        sample_size = sample_sizes[i],
        duration = durations[i]
      ))
    }
  }

  # Now handle the final analysis (last stage)
  final_success <- z_scores[num_stages] > critical_values[num_stages]

  return(list(
    status = if (final_success) "Successful at final analysis" else "Unsuccessful at final analysis",
    successful = final_success,
    analysis = num_stages,  # This is the final analysis
    z_score = z_scores[num_stages],
    sample_size = sample_sizes[num_stages],
    duration = durations[num_stages]
  ))
}





strictly_increasing_combinations <- function(...) {
  #vectors <- list(...)  # Convert input vectors into a list
  combinations <- expand.grid(...)  # Generate all possible combinations

  # Filter rows where elements are in strictly increasing order
  valid_combinations <- combinations[apply(combinations, 1, function(row) all(diff(as.numeric(row)) > 0)), ]

  # Convert each row into a comma-separated string
  result <- apply(valid_combinations, 1, function(row) paste(row, collapse = ", "))

  return(result)  # Return as a character vector
}



