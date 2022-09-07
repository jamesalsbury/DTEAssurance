#' App to calculate assurance for when DTE are likely to be present
#'
#' @export
#' @import SHELF
#' @import shiny
#' @import survival
#' @import shinydashboard
#' @import readxl
#' @import ggplot2
#' @import ggfortify
#' @import dplyr
#' @import shinyjs
#' @import nleqslv
#' @import pbapply
#'

DTEAssuranceApp <- function(){
  ui <- fluidPage(

    # Application title
    titlePanel("Delayed Treatment Effects - Weibull parameterisation"),

    # sidebarLayout(
    mainPanel(tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
    ),

    tabsetPanel(
      # Control UI ---------------------------------

      tabPanel("Control",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("uploadSample", "Upload your control sample"),
                   numericInput("lambda2", "lambda2", value=0.05),
                   numericInput("gamma2", "gamma2", value=1)
                 ),
                 mainPanel = mainPanel(
                   plotOutput("plotControl"),
                   htmlOutput("recommendedParams")
                 )
               ),
      ),

      # T UI ---------------------------------
      tabPanel("Eliciting T",
               fluidRow(
                 column(4,
                        textInput("limits1", label = h5("T limits"),
                                  value = "0, 6")
                 ),
                 column(4,
                        textInput("values1", label = h5("T values"),
                                  value = "2, 3, 4")
                 ),
                 column(4,
                        textInput("probs1", label = h5("Cumulative probabilities"),
                                  value = "0.25, 0.5, 0.75")
                 )
               ),
               fluidRow(
                 column(4,
                        selectInput("dist1", label = h5("Distribution"),
                                    choices =  list(Histogram = "hist",
                                                    Normal = "normal",
                                                    'Student-t' = "t",
                                                    Gamma = "gamma",
                                                    'Log normal' = "lognormal",
                                                    'Log Student-t' = "logt",
                                                    Beta = "beta",
                                                    'Mirror gamma' = "mirrorgamma",
                                                    'Mirror log normal' = "mirrorlognormal",
                                                    'Mirror log Student-t' = "mirrorlogt",
                                                    'Best fitting' = "best"),
                                    #choiceValues = 1:8,
                                    selected = 1
                        )),
                 column(4,conditionalPanel(
                   condition = "input.dist1 == 't' || input.dist1 == 'logt' || input.dist1 == 'mirrorlogt'",
                   numericInput("tdf1", label = h5("Student-t degrees of freedom"),
                                value = 3)
                 )
                 )

               ),
               plotOutput("distPlot1")
      ),
      # HR UI ---------------------------------
      tabPanel("Eliciting HR",
               fluidRow(
                 column(4,
                        textInput("limits2", label = h5("HR limits"),
                                  value = "0, 1")
                 ),
                 column(4,
                        textInput("values2", label = h5("HR values"),
                                  value = "0.5, 0.6, 0.7")
                 ),
                 column(4,
                        textInput("probs2", label = h5("Cumulative probabilities"),
                                  value = "0.25, 0.5, 0.75")
                 )
               ),
               fluidRow(
                 column(4,
                        selectInput("dist2", label = h5("Distribution"),
                                    choices =  list(Histogram = "hist",
                                                    Normal = "normal",
                                                    'Student-t' = "t",
                                                    Gamma = "gamma",
                                                    'Log normal' = "lognormal",
                                                    'Log Student-t' = "logt",
                                                    Beta = "beta",
                                                    'Mirror gamma' = "mirrorgamma",
                                                    'Mirror log normal' = "mirrorlognormal",
                                                    'Mirror log Student-t' = "mirrorlogt",
                                                    'Best fitting' = "best"),
                                    #choiceValues = 1:8,
                                    selected = 1
                        )),
                 column(4,
                        conditionalPanel(
                          condition = "input.dist2 == 't' || input.dist2 == 'logt' || input.dist1 == 'mirrorlogt'",
                          numericInput("tdf2", label = h5("degrees of freedom"),
                                       value = 3)


                        ))

               ),

               plotOutput("distPlot2"),
               htmlOutput("HRProportion")
      ),

      # Feedback UI ---------------------------------
      tabPanel("Feedback",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   checkboxGroupInput("showfeedback", "Add to plot", choices = c("Median survival line", "95% CI for T", "CI for Treatment Curve (0.1 and 0.9)")),
                 ),
                 mainPanel = mainPanel(
                   plotOutput("plotFeedback"),
                   htmlOutput("errorFeedback")
                 )
               ),
      ),

      # Assurance UI ---------------------------------
      tabPanel("Assurance",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   useShinyjs(),
                   numericInput("numofpatients", "How many patients could you enrol into the trial?", value=1000),
                   numericInput("rectime", "How long would it take to enrol all of these patients?", value=6),

                   box(width = 10, title = "Ratio of patients in each group?",
                       splitLayout(
                         numericInput("n1", "Control", value=1, min=1),
                         numericInput("n2", "Treatment", value=1, min=1)
                       )
                   ),
                   numericInput("chosenLength", "How long do you want to run the trial for? (Including recruitment time)", value=60),
                   actionButton("drawAssurance", "Produce plot")
                 ),
                 mainPanel = mainPanel(
                   plotOutput("assurancePlot"),
                   htmlOutput("assuranceText")
                 )
               ),

      ),

      #Help UI ---------------------------------
      #Need to say what files can be uploaded in the control sample
      #Link to SHELF for the elicitation
      tabPanel("Help",
               HTML("<p>This app implements the method as outlined in this <a href='https://jamesalsbury.github.io/'>paper.</a></p>"),
               HTML("<p>The eliciation technique is based on SHELF, more guidance can be found <a href='https://shelf.sites.sheffield.ac.uk/'>here.</a></p>")
      ),


    ), style='width: 1200px; height: 1200px',
    wellPanel(
      fluidRow(
        column(3, selectInput("outFormat", label = "Report format",
                              choices = list('html' = "html_document",
                                             'pdf' = "pdf_document",
                                             'Word' = "word_document"))
        ),
        column(3, offset = 1,
               numericInput("fs", label = "Font size", value = 12)
        )),
      fluidRow(
        column(3, downloadButton("report", "Download report")
        ),
        column(3, downloadButton("downloadData", "Download sample")
        ),
        column(3, actionButton("exit", "Quit")
        )
      )

    )
    )
  )



  server = function(input, output, session) {

    # Functions for the control tab ---------------------------------

    inputData <- reactive({
      #Allows the user to upload a control sample
      chosenFile <- input$uploadSample
      if (is.null(chosenFile)){
        return(NULL)
      } else {
        #lambda2 and gamma2 are estimated from the uploaded control sample
        controlSample <- read_excel(chosenFile$datapath, sheet=1)
        weibfit <- survreg(Surv(time, cens)~1, data = controlSample, dist = "weibull")
        updateTextInput(session, "lambda2", value = round(as.numeric(1/(exp(weibfit$icoef[1]))), 3))
        updateTextInput(session, "gamma2", value = round(as.numeric(exp(-weibfit$icoef[2])), 3))
        return(list(gamma2 = as.numeric(exp(-weibfit$icoef[2])), lambda2 = as.numeric(1/(exp(weibfit$icoef[1]))), controltime = controlSample$time, controlcens = controlSample$cens))
      }

    })

    output$recommendedParams <- renderUI({
      if (is.null(inputData())){

      } else {
        #Tells the user what the best fitting parameters are for their uploaded sample
        str1 <- paste0("For your uploaded sample, the best fitting parameters are:")
        str2 <- paste0("Lambda2 = ", round(inputData()$lambda2, 3))
        str3 <- paste0("Gamma2 = ", round(inputData()$gamma2, 3))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }

    })

    output$plotControl <- renderPlot({

      #Shows the user what their control parameters look like
      controltime <- seq(0, exp((1.527/input$gamma2)-log(input$lambda2))*1.1, by=0.01)
      controlsurv <- exp(-(input$lambda2*controltime)^input$gamma2)
      controldf <- data.frame(controltime = controltime,
                              controlsurv = controlsurv)
      theme_set(theme_grey(base_size = input$fs))
      p1 <- ggplot(data=controldf, aes(x=controltime, y=controlsurv)) +
        geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1)

      print(p1)

      if (is.null(inputData())){

      } else {
        #Shows well the Weibull parameters fit the uploaded data set
        controlSample <- data.frame(time = inputData()$controltime, cens = inputData()$controlcens)
        km <- survival::survfit(Surv(time, cens)~1, data = controlSample)
        autoplot(km, conf.int = F, surv.colour = "red", xlab = "Time", ylab="Survival")  +
          geom_line(data = controldf, aes(x = controltime, y = controlsurv), colour = "blue")

      }

    })

    # Functions for the eliciting distributions tabs ---------------------------------


    limits1 <- reactive({
      eval(parse(text = paste("c(", input$limits1, ")")))
    })

    limits2 <- reactive({
      eval(parse(text = paste("c(", input$limits2, ")")))
    })

    p1 <- reactive({
      eval(parse(text = paste("c(", input$probs1, ")")))
    })

    p2 <- reactive({
      eval(parse(text = paste("c(", input$probs2, ")")))
    })

    v1 <- reactive({
      eval(parse(text = paste("c(", input$values1, ")")))
    })

    v2 <- reactive({
      eval(parse(text = paste("c(", input$values2, ")")))
    })

    m1 <- reactive({
      approx(p1(), v1(), 0.5)$y
    })

    m2 <- reactive({
      approx(p2(), v2(), 0.5)$y
    })

    myfit1 <- reactive({
      fitdist(vals = v1(), probs = p1(), lower = limits1()[1],
              upper = limits1()[2],
              tdf = input$tdf1)
    })

    myfit2 <- reactive({
      fitdist(vals = v2(), probs = p2(), lower = limits2()[1],
              upper = limits2()[2],
              tdf = input$tdf2)
    })


    # Functions for the T tab ---------------------------------

    output$distPlot1 <- renderPlot({


      #d = dist[as.numeric(input$radio1)]
      # dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit1(), d = input$dist1,
                               ql = 0.05, qu = 0.95,
                               xl = limits1()[1], xu = limits1()[2],
                               fs = input$fs))

    })

    # Functions for the HR tab ---------------------------------


    output$distPlot2 <- renderPlot({


      #  dist<-c("hist","normal", "t", "gamma", "lognormal", "logt","beta", "best")
      suppressWarnings(plotfit(myfit2(), d = input$dist2,
                               ql = 0.05, qu = 0.95,
                               xl = limits2()[1], xu = limits2()[2],
                               fs = input$fs))


    })

    # Functions for the Feedback tab ---------------------------------

    #This function allows the 10 and 90% CI lines to be drawn
    drawsimlines <- reactive({

      gamma1 <- input$gamma2
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5
      #Simulates 500 samples from the elicited T and HR distributions
      mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = 500, d = c(input$dist1, input$dist2)))

      time <- seq(0, exp((1.527/input$gamma2)-log(input$lambda2))*1.1, by=0.01)
      #We fill a matrix with the treatment survival probabilities at each time
      SimMatrix <- matrix(NA, nrow = 500, ncol=length(time))

      for (i in 1:500){
        bigT <- mySample[i,1]
        HR <- mySample[i,2]
        lambda1 <- input$lambda2*HR^(1/input$gamma2)

        controltime <- seq(0, bigT, by=0.01)
        controlsurv <- exp(-(input$lambda2*controltime)^input$gamma2)

        treatmenttime <- seq(bigT, exp((1.527/input$gamma2)-log(input$lambda2))*1.1, by=0.01)
        treatmentsurv <- exp(-(input$lambda2*bigT)^input$gamma2 - lambda1^gamma1*(treatmenttime^gamma1-bigT^gamma1))

        timecombined <- c(controltime, treatmenttime)[1:length(time)]
        survcombined <- c(controlsurv, treatmentsurv)[1:length(time)]

        #The i'th row of the matrix is filled with the survival probabilities for these sampled T and HR
        SimMatrix[i,] <- survcombined

      }

      #We now look at each time iteration at the distribution
      #We look at the 0.1 and 0.9 quantile of the distribution
      #These quantiles can be thought of as confidence intervals for the treatment curve, taken from
      #the elicited distributions
      lowerbound <- rep(NA, length(time))
      upperbound <- rep(NA, length(time))
      for (j in 1:length(time)){
        lowerbound[j] <- quantile(SimMatrix[,j], 0.1)
        upperbound[j] <- quantile(SimMatrix[,j], 0.9)
      }

      return(list(lowerbound=lowerbound, upperbound=upperbound, time=time, SimMatrix = SimMatrix))

    })

    output$plotFeedback <- renderPlot({

      #This plots the feedback plot
      gamma1 <- input$gamma2
      controltime <- seq(0, exp((1.527/input$gamma2)-log(input$lambda2))*1.1, by=0.01)
      controlcurve <- exp(-(input$lambda2*controltime)^input$gamma2)
      controldf <- data.frame(controltime = controltime, controlcurve = controlcurve)
      theme_set(theme_grey(base_size = input$fs))
      p1 <- ggplot(data=controldf, aes(x=controltime, y=controlcurve)) +
        geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1)


      zeroval <- feedback(myfit1(), quantiles = 0.01)$fitted.quantiles[input$dist1][, 1]

      if (zeroval<0){

      } else {

        #We use the medians of the elicited distributions to show the treatment curve
        bigTMedian <- feedback(myfit1(), quantiles = 0.5)$fitted.quantiles[input$dist1][, 1]
        HRMedian <- feedback(myfit2(), quantiles = 0.5)$fitted.quantiles[input$dist2][, 1]
        lambda1 <- input$lambda2*HRMedian^(1/input$gamma2)

        treatmenttime1 <- seq(0, bigTMedian, by=0.01)
        treatmentsurv1 <- exp(-(input$lambda2*treatmenttime1)^input$gamma2)
        treatmenttime1df <- data.frame(treatmenttime1 = treatmenttime1, treatmentsurv1 = treatmentsurv1)
        p1 <-  p1 + geom_line(data = treatmenttime1df, aes(x = treatmenttime1, y = treatmentsurv1), colour = "green")


        treatmenttime2 <- seq(bigTMedian, exp((1.527/input$gamma2)-log(input$lambda2))*1.1, by=0.01)
        treatmentsurv2 <- exp(-(input$lambda2*bigTMedian)^input$gamma2 - lambda1^gamma1*(treatmenttime2^gamma1-bigTMedian^gamma1))
        treatmenttime2df <- data.frame(treatmenttime2 = treatmenttime2, treatmentsurv2 = treatmentsurv2)
        p1 <-  p1 + geom_line(data = treatmenttime2df, aes(x = treatmenttime2, y = treatmentsurv2), colour = "red")

        print(p1)

        #This code adds the three choices to the plots
        addfeedback <- input$showfeedback

        if (!is.null(addfeedback)){
          for (i in 1:length(addfeedback)){
            #This adds the median survival line (onto the control and treatment)
            if (addfeedback[i]=="Median survival line"){
              #Looks at whether the median time is before or after the delay
              if (exp(-(input$lambda2*bigTMedian)^input$gamma2)<0.5){
                mediandf <- data.frame(x = seq(0, controltime[sum(controlcurve>0.5)], length=2), y = rep(0.5, 2))
                mediandf1 <- data.frame(x = rep(controltime[sum(controlcurve>0.5)], 2), y = seq(0, 0.5, length=2))
                p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") + geom_line(data = mediandf1, aes(x = x, y=y), linetype="dashed")
              } else {
                mediandf <- data.frame(x = seq(0, treatmenttime2[sum(treatmentsurv2>0.5)], length=2), y = rep(0.5, 2))
                mediandf1 <- data.frame(x = rep(treatmenttime2[sum(treatmentsurv2>0.5)], 2), y = seq(0, 0.5, length=2))
                mediandf2 <- data.frame(x = rep(controltime[sum(controlcurve>0.5)], 2), y = seq(0, 0.5, length=2))
                p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") + geom_line(data = mediandf1, aes(x = x, y=y), linetype="dashed") +
                  geom_line(data = mediandf2, aes(x = x, y=y), linetype="dashed")
              }
              #This uses the elicited distribution for T and adds 95% points onto the control curve
            } else if (addfeedback[i]=="95% CI for T"){
              p1 <- p1 + geom_point(aes(x = feedback(myfit1(), quantiles = 0.025)$fitted.quantiles[input$dist1][, 1], y = controlcurve[sum(controltime<feedback(myfit1(), quantiles = 0.025)$fitted.quantiles[input$dist1][, 1])]), colour="orange", size = 4) +
                geom_point(aes(x = feedback(myfit1(), quantiles = 0.975)$fitted.quantiles[input$dist1][, 1], y = controlcurve[sum(controltime<feedback(myfit1(), quantiles = 0.975)$fitted.quantiles[input$dist1][, 1])]), colour="orange", size = 4)
            } else if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
              #This adds the simulated confidence interval lines
              simlineslower <- data.frame(x = drawsimlines()$time, y = drawsimlines()$lowerbound)
              simlinesupper <- data.frame(x = drawsimlines()$time, y = drawsimlines()$upperbound)
              p1 <- p1 + geom_line(data = simlineslower, aes(x=x, y=y), linetype="dashed")+
                geom_line(data = simlinesupper, aes(x=x, y=y), linetype="dashed")
            }
          }
        }
        print(p1)
      }
    })

    output$errorFeedback <- renderUI({

      zeroval <- feedback(myfit1(), quantiles = 0.01)$fitted.quantiles[input$dist1][, 1]

      #Adds an error message if the elicited distribution for T includes nonsensical values
      if (zeroval<0){
        paste0("Your elicited distribution of T includes values that are less than 0, please change your limits or choose another distribution.")
      }

    })

    # Functions for the Assurance tab ---------------------------------

    #This function calculates the normal assurance given the elicited distributions and other simple questions about the trial
    calculateNormalAssurance <- eventReactive(input$drawAssurance, {

      #Makes the simplification
      gamma1 <- input$gamma2
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5

      assFunc <- function(n1, n2){

        #Simulate 400 observations for T and HR given the elicited distributions
        #For each n1, n2, simulate 400 trials
        assnum <- 400
        assvec <- rep(NA, assnum)
        eventsvec <- rep(NA, assnum)
        mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = assnum, d = c(input$dist1, input$dist2)))


        for (i in 1:assnum){

          bigT <- mySample[i,1]
          HR <- mySample[i,2]

          lambda1 <- input$lambda2*HR^(1/input$gamma2)

          dataCombined <- SimDTEDataSet(n1, n2, gamma1, input$gamma2, lambda1, input$lambda2, bigT, input$rectime, input$chosenLength)

          #Performs a log rank test on the data
          test <- survdiff(Surv(time, event)~group, data = dataCombined)
          #If the p-value of the test is less than 0.05 then assvec = 1, 0 otherwise
          assvec[i] <- test$chisq > qchisq(0.95, 1)

          #Counts how many events have been seen up until the total trial length time
          eventsvec[i] <-  sum(dataCombined$time<input$chosenLength)

        }

        return(list(assvec = mean(assvec), eventvec = mean(eventsvec)))
      }

      #Looking at assurance for varying sample sizes
      samplesizevec <- seq(30, input$numofpatients, length=15)
      n1vec <- floor(input$n1*(samplesizevec/(input$n1+input$n2)))
      n2vec <- ceiling(input$n2*(samplesizevec/(input$n1+input$n2)))
      calcassvec <- rep(NA, length = length(samplesizevec))

      pboptions(type="shiny", title = "Calculating assurance (1/2)")

      calcassvec <- pbmapply(assFunc, n1vec, n2vec)

      calcassvec <- unlist(calcassvec[1,])

      #How many events are seen given this set up
      eventsseen <- assFunc(n1vec[length(samplesizevec)], n2vec[length(samplesizevec)])$eventvec

      #Smooth the assurance, compared to the the sample size vector
      asssmooth <- loess(calcassvec~samplesizevec)

      return(list(calcassvec = calcassvec, asssmooth = asssmooth, samplesizevec = samplesizevec,
                  eventsseen = eventsseen))


    })


    #This function calculates the normal assurance given the elicited distributions and other simple questions about the trial
    calculateFlexibleAssurance <- eventReactive(input$drawAssurance, {

      timechosen1 <- floor(0.4*input$chosenLength)
      timechosen2 <- floor(0.75*input$chosenLength)
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5
      gamma1 <- input$gamma2


      assFunc <- function(n1, n2){
        #For each n1, n2, simulate 300 trials
        assnum <- 300
        assvec <- rep(NA, assnum)
        eventsvec <- rep(NA, assnum)

        for (i in 1:assnum){

          sampledpoint1 <- sample(na.omit(drawsimlines()$SimMatrix[,which(drawsimlines()$time==timechosen1)]), 1)
          sampledpoint2 <- sample(na.omit(drawsimlines()$SimMatrix[,which(drawsimlines()$time==timechosen2)]), 1)

          if (sampledpoint1>sampledpoint2){
            #This needs looking at
            mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = 100, d = c(input$dist1, input$dist2)))
            bigT <- mySample[1,1]

            dslnex <- function(x) {
              y <- numeric(2)
              y[1] <- exp(-(input$lambda2*bigT)^input$gamma2-x[1]^x[2]*(timechosen1^x[2]-bigT^x[2])) - sampledpoint1
              y[2] <- exp(-(input$lambda2*bigT)^input$gamma2-x[1]^x[2]*(timechosen2^x[2]-bigT^x[2])) - sampledpoint2
              y
            }


            xstart <- c(0.05,1)

            output <- nleqslv(xstart, dslnex)

            lambda1 <- output$x[1]
            gamma1 <- output$x[2]

            if (lambda1<0){
              lambda1 <- 0.000001
            }


            dataCombined <- SimDTEDataSet(n1, n2, gamma1, input$gamma2, lambda1, input$lambda2, bigT, input$rectime, input$chosenLength)

            #Performs a log rank test on the data
            test <- survdiff(Surv(time, event)~group, data = dataCombined)
            #If the p-value of the test is less than 0.05 then assvec = 1, 0 otherwise
            assvec[i] <- test$chisq > qchisq(0.95, 1)

            #Counts how many events have been seen up until the total trial length time
            eventsvec[i] <-  sum(dataCombined$time<input$chosenLength)

          }
        }

        return(list(assvec = mean(na.omit(assvec)), eventvec = mean(na.omit(eventsvec))))
      }


      #Looking at assurance for varying sample sizes
      samplesizevec <- seq(30, input$numofpatients, length=15)
      n1vec <- floor(input$n1*(samplesizevec/(input$n1+input$n2)))
      n2vec <- ceiling(input$n2*(samplesizevec/(input$n1+input$n2)))
      calcassvec <- rep(NA, length = length(samplesizevec))

      pboptions(type="shiny", title = "Calculating assurance (2/2)")

      calcassvec <- pbmapply(assFunc, n1vec, n2vec)

      calcassvec <- unlist(calcassvec[1,])

      #How many events are seen given this set up
      eventsseen <- assFunc(n1vec[length(samplesizevec)], n2vec[length(samplesizevec)])$eventvec

      #Smooth the assurance, compared to the the sample size vector
      asssmooth <- loess(calcassvec~samplesizevec)

      return(list(calcassvec = calcassvec, asssmooth = asssmooth, samplesizevec = samplesizevec,
                  eventsseen = eventsseen))

    })

    output$assurancePlot <- renderPlot({

      #Plot the assurance calculated in the function
      theme_set(theme_grey(base_size = input$fs))
      assurancenormaldf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$asssmooth))
      assuranceflexibledf <- data.frame(x = calculateFlexibleAssurance()$samplesizevec, y = predict(calculateFlexibleAssurance()$asssmooth))
      p1 <- ggplot() + geom_line(data = assurancenormaldf, aes(x = x, y = y, colour="Normal"), linetype="solid") + xlab("Number of patients") +
        ylab("Assurance") + ylim(0, 1.05) + geom_line(data = assuranceflexibledf, aes(x=x, y=y, colour = "Flexible"), linetype="dashed") +
        scale_color_manual(name='Type of assurance',
                           breaks=c('Normal', 'Flexible'),
                           values=c('Normal'='blue', 'Flexible'='red'))
      print(p1)
    })


    output$assuranceText  <- renderUI({

      #Show how many events are seen given the set up
      str1 <- paste0("On average, ", round(calculateNormalAssurance()$eventsseen), " events are seen when ", input$numofpatients, " patients are enroled for ", input$chosenLength, " months")
      HTML(paste(str1, sep = '<br/>'))
    })


    # observeEvent(input$exit, {
    #   stopApp(list(parameter1 = myfit1(), parameter2 = myfit2(),
    #                cp = input$concProb))
    # })

    # output$downloadData <- downloadHandler(
    #   filename = "joint-sample.csv",
    #   content = function(file) {
    #     utils::write.csv(df1(), file, row.names = FALSE)
    #   }
    # )

    # output$report <- downloadHandler(
    #   filename = function(){switch(input$outFormat,
    #                                html_document = "distributions-report.html",
    #                                pdf_document = "distributions-report.pdf",
    #                                word_document = "distributions-report.docx")},
    #   content = function(file) {
    #     # Copy the report file to a temporary directory before processing it, in
    #     # case we don't have write permissions to the current working dir (which
    #     # can happen when deployed).
    #     tempReport <- file.path(tempdir(), "elicitationShinySummaryBivariate.Rmd")
    #     file.copy(system.file("shinyAppFiles", "elicitationShinySummaryBivariate.Rmd",
    #                           package="SHELF"),
    #               tempReport, overwrite = TRUE)
    #
    #     # Set up parameters to pass to Rmd document
    #     params <- list(fit1 = myfit1(), fit2 = myfit2(), cp = input$concProb,
    #                    d = c(input$dist1, input$dist2), m1 = m1(), m2 = m2())
    #
    #     # Knit the document, passing in the `params` list, and eval it in a
    #     # child of the global environment (this isolates the code in the document
    #     # from the code in this app).
    #     rmarkdown::render(tempReport, output_file = file,
    #                       params = params,
    #                       output_format = input$outFormat,
    #                       envir = new.env(parent = globalenv())
    #     )
    #   }
    # )

  }

  shinyApp(ui, server)

}






