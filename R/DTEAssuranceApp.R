#' App to calculate assurance for when DTE are likely to be present
#'
#' @export
#'
#' @import SHELF
#' @importFrom survMisc autoplot
#' @rawNamespace import(ggplot2, except = autoplot)
#' @import pbapply
#' @import readxl
#' @import shiny
#' @import shinydashboard
#' @import survival
#' @import plyr
#' @import rmarkdown
#' @import stats
#'
#'

DTEAssuranceApp <- function(){
  x <- y <- quantiletime <- NULL
  ui <- fluidPage(
    withMathJax(),

    # Application title
    titlePanel("Assurance: Delayed Treatment Effects"),

    # sidebarLayout(
    mainPanel(

    tabsetPanel(
      # Control UI ---------------------------------

      tabPanel("Control",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("uploadSample", "Upload your control sample"),
                   numericInput('lambdac', label =  HTML(paste0("scale (\u03bb",tags$sub("c"), ")")), value = 0.05),
                   numericInput('gammac', label =  HTML(paste0("scale (\u03b3",tags$sub("c"), ")")), value = 1)
                 ),
                 mainPanel = mainPanel(
                   plotOutput("plotControl"),
                   htmlOutput("recommendedParams")
                 )
               ),
      ),

      # Length of delay UI ---------------------------------
      tabPanel("Eliciting the length of delay",
               fluidRow(
                 column(4,
                        textInput("limits1", label = h5("Length of delay limits"),
                                  value = "0, 12")
                 ),
                 column(4,
                        textInput("values1", label = h5("Length of delay values"),
                                  value = "5.5, 6, 6.5")
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
                                                    Gamma = "gamma",
                                                    'Log normal' = "lognormal",
                                                    Beta = "beta",
                                                    'Best fitting' = "best"),
                                    #choiceValues = 1:8,
                                    selected = 1
                        )),
                 column(4,conditionalPanel(
                   condition = "input.dist1 == 't' || input.dist1 == 'logt' || input.dist1 == 'mirrorlogt'",
                   numericInput("tdf1", label = h5("Student-t degrees of freedom"),
                                value = 3)
                 )
                 ),
                 column(4,
                        numericInput("massT0", label = h5("Pr(T=0)"), value = 0.05, min = 0, max = 1)
                 )

               ),
               plotOutput("distPlot1")
      ),
      # post-delay HR UI ---------------------------------
      tabPanel("Eliciting the post-delay hazard ratio",
               fluidRow(
                 column(4,
                        textInput("limits2", label = h5("Post-delay hazard ratio limits"),
                                  value = "0, 1")
                 ),
                 column(4,
                        textInput("values2", label = h5("Post-delay hazard ratio values"),
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


                        )
                 ),
                 column(4,
                        numericInput("massHR1", label = h5("Pr(HR=1)"), value = 0.05, min = 0, max = 1)
                 )

               ),

               plotOutput("distPlot2"),
               htmlOutput("HRProportion")
      ),

      # Feedback UI ---------------------------------
      tabPanel("Feedback",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   checkboxGroupInput("showfeedback", "Add to plot", choices = c("Median survival line(s)", "95% CI for T", "CI for Treatment Curve (0.1 and 0.9)")),
                   shinyjs::hidden(numericInput("feedbackQuantile", "Uncertainty about the following survival quantile:", value = 0.5, min = 0, max = 1)),
                 ),
                 mainPanel = mainPanel(
                   plotOutput("plotFeedback"),
                   htmlOutput("errorFeedback"),
                   plotOutput("quantilePlot")
                 )
               ),
      ),

      # Assurance UI ---------------------------------
      tabPanel("Assurance",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   shinyjs::useShinyjs(),
                   numericInput("numofpatients", "Maximum number of patients in the trial", value=1000),
                   numericInput("rectime", "Recruitment length", value=6),


                   splitLayout(
                     numericInput("n1", "Ratio control", value=1, min=1),
                     numericInput("n2", "Ratio treatment", value=1, min=1)

                   ),
                   numericInput("chosenLength", "Maximum trial duration (including recruitment time)", value=60),
                   numericInput("TPP", "Target effect (average hazard ratio)", value = 0.8),
                   actionButton("drawAssurance", "Produce plot")
                 ),
                 mainPanel = mainPanel(
                   plotOutput("assurancePlot"),
                   htmlOutput("assuranceText"),
                   plotOutput("AHRPlot")
                 )
               ),

      ),

      #Help UI ---------------------------------
      #Need to say what files can be uploaded in the control sample
      tabPanel("Help",
               HTML("<p>This app implements the method as outlined in this <a href='https://jamesalsbury.github.io/'>paper</a>. For every tab, there is a brief summary below, for any other questions, comments or
                  feedback, please contact <a href='mailto:jsalsbury1@sheffield.ac.uk'>James Salsbury</a>.</p>"),
               HTML("<p><u>Control</u></p>"),
               HTML("<p>Here, the parameters for the control survival are specified, the parameterisation for the control survival curve is:</p>"),
               withMathJax(paste0(" $$S_c(t) = \\text{exp}\\{-(\\lambda_ct)^{\\gamma_c}\\}$$")),
               HTML("<p>The control tab also allows an upload of an Excel file containing survival data for the control sample.
                  The Excel file needs to have two columns: the first column containing the survival time, and the second column containing the event status (1 for dead, 0 for alive).</p>"),
               HTML("<p><u>Eliciting the two parameters: T and post-delay HR</u></p>"),
               HTML("<p>These two tabs elicit beliefs from the user about two quantities: the length of delay, T, and the post-delay HR. The parameterisation for the treatment survival curve is:</p>"),
               withMathJax(paste0(" $$S_t(t) = \\text{exp}\\{-(\\lambda_ct)^{\\gamma_c}\\}, t \\leq T$$")),
               withMathJax(paste0(" $$S_t(t) = \\text{exp}\\{-(\\lambda_cT)^{\\gamma_c}-\\lambda_t^{\\gamma_t}(t^{\\gamma+t}-T^{\\gamma_t})\\}, t > T$$")),
               HTML("<p>The elicitation technique is based on SHELF, more guidance can be found <a href='https://shelf.sites.sheffield.ac.uk/'>here.</a></p>"),
               HTML("<p>There is also an option to include some mass at T = 0 and HR = 1.</p>"),
               HTML("<p><u>Feedback</u></p>"),
               HTML("<p>The initial plot may take ~ 5 seconds to load. The plot shows the control survival curve (from the control tab), along with the median elicited treatment line (calculated from the previous two tabs).
                  There are also three optional quantities to view: the first is the median survival time for both groups, the second is a 95% confidence interval for T and the
                  third shows a 80% confidence interval for the treatment curve. When the median survival time is added to the plot, a second plot is shown below. Initially, this plot is a histogram for the
                  treatment median survival time. However, the user is able to change this median to any other quantile of interest.</p>"),
               HTML("<p><u>Calculating assurance</u></p>"),
               HTML("<p>This tab allows the user to calculate assurance, given the control parameters and elicited prior distributions  for T and post-delay HR. Some additional questions
                  about the trial are found on the left-hand panel. The app assumes uniform recruitment and uses a log-rank test for analysis of the simulated data. Depending on your processor speed, this
                  calculation can take between 30-40 seconds. Once the calculation is complete, the app shows two plots. The top plot shows assurance (along with standard error curves) and target effect curve - which
                  shows the proportion of trials in which the target effect was observed. The bottom plot shows the average hazard ratio observed and the corresponding confidence intervals for this. </p>")
      ),


    ), style='width: 1000px; height: 600px',
    #Well panel UI ---------------------------------
    wellPanel(
      fluidRow(
        column(3, selectInput("outFormat", label = "Report format",
                              choices = list('html' = "html_document",
                                             'pdf' = "pdf_document",
                                             'Word' = "word_document"))
        ),
        column(3, offset = 1,
               numericInput("fs", label = "Font size", value = 12)
        ),
        column(3, offset = 1,
               numericInput("ss", label = "Sample size (when downloading sample)", value = 500)
        )
        ),
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

    #shinyjs::hide(id = "feedbackQuantile")

    # Functions for the control tab ---------------------------------

    inputData <- reactive({
      #Allows the user to upload a control sample
      chosenFile <- input$uploadSample
      if (is.null(chosenFile)){
        return(NULL)
      } else {
        #lambdac and gammac are estimated from the uploaded control sample
        controlSample <- read_excel(chosenFile$datapath, sheet=1)
        weibfit <- survreg(Surv(time, cens)~1, data = controlSample, dist = "weibull")
        updateTextInput(session, "lambdac", value = round(as.numeric(1/(exp(weibfit$icoef[1]))), 3))
        updateTextInput(session, "gammac", value = round(as.numeric(exp(-weibfit$icoef[2])), 3))
        return(list(gammac = as.numeric(exp(-weibfit$icoef[2])), lambdac = as.numeric(1/(exp(weibfit$icoef[1]))), controltime = controlSample$time, controlcens = controlSample$cens))
      }

    })

    output$recommendedParams <- renderUI({
      if (is.null(inputData())){

      } else {
        #Tells the user what the best fitting parameters are for their uploaded sample
        str1 <- paste0("For your uploaded sample, the best fitting parameters are:")
        str2 <- paste0("Lambdac = ", round(inputData()$lambdac, 3))
        str3 <- paste0("Gammac = ", round(inputData()$gammac, 3))
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }

    })

    output$plotControl <- renderPlot({

      #Shows the user what their control parameters look like
      controltime <- seq(0, exp((1.527/input$gammac)-log(input$lambdac))*1.1, by=0.01)
      controlsurv <- exp(-(input$lambdac*controltime)^input$gammac)
      controldf <- data.frame(x = controltime,
                              y = controlsurv)
      theme_set(theme_grey(base_size = input$fs))
      p1 <- ggplot(data=controldf, aes(x=x, y=y)) +
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
      fitdistdelayT(vals = v1(), probs = p1(), lower = limits1()[1],
                    upper = limits1()[2],
                    tdf = input$tdf1)
    })

    myfit2 <- reactive({
      fitdist(vals = v2(), probs = p2(), lower = limits2()[1],
              upper = limits2()[2],
              tdf = input$tdf2)
    })


    # Functions for the eliciting length of delay tab ---------------------------------

    output$distPlot1 <- renderPlot({

      mySample <- drawsamples()$mySample
      Tsamples <- data.frame(time = mySample[,1])
      d <- input$dist1
      if(d == "best"){
        d <- myfit1()$best.fitting[1, 1]
      }

      dist.title <- ""
      if(d == "hist"){
        dist.title = "histogram fit"
      }

      if(d == "gamma"){
        dist.title = paste("Gamma(",
                           signif(myfit1()$Gamma[1,1], 3),
                           ", ",
                           signif(myfit1()$Gamma[1,2], 3),
                           ")", sep="")
      }

      if(d == "lognormal"){
        dist.title = paste("Log normal(",
                           signif(myfit1()$Log.normal[1,1], 3),
                           ", ",
                           signif(myfit1()$Log.normal[1,2], 3), ")",
                           sep="")
      }

      if(d == "beta"){
        dist.title =paste("Beta(",
                          signif(myfit1()$Beta[1,1], 3),
                          ", ", signif(myfit1()$Beta[1,2], 3),
                          ")", sep="")
      }


      if (input$massT0>0){
        dist.title <- paste(input$massT0, "\u22c5 0 +", 1-input$massT0, "\u22c5", dist.title)
      }


      p1 <- ggplot(data=Tsamples, aes(x=time)) + geom_histogram(aes_string(y = "..density..")) + labs(title = dist.title) +  theme(plot.title = element_text(hjust = 0.5))

      print(p1)


    })

    # Functions for the post-delay HR tab ---------------------------------

    output$distPlot2 <- renderPlot({


      mySample <- drawsamples()$mySample
      HRsamples <- data.frame(HR = mySample[,2])

      d <- input$dist2
      if(d == "best"){
        d <- myfit2()$best.fitting[1, 1]
      }

      dist.title <- ""
      if(d == "normal"){

        dist.title <- paste("Normal (mean = ",
                            signif(myfit2()$Normal[1,1], 3),
                            ", sd = ",
                            signif(myfit2()$Normal[1,2], 3), ")",
                            sep="")
      }

      if(d == "t"){


        dist.title=paste("Student-t(",
                         signif(myfit2()$Student.t[1,1], 3),
                         ", ",
                         signif(myfit2()$Student.t[1,2], 3),
                         "), df = ",
                         myfit2()$Student.t[1, 3],
                         sep="")
      }

      if(d == "gamma"){

        dist.title = paste("Gamma(",
                           signif(myfit2()$Gamma[1,1], 3),
                           ", ",
                           signif(myfit2()$Gamma[1,2], 3),
                           ")", sep="")
      }

      if(d == "lognormal"){

        dist.title = paste("Log normal(",
                           signif(myfit2()$Log.normal[1,1], 3),
                           ", ",
                           signif(myfit2()$Log.normal[1,2], 3), ")",
                           sep="")
      }

      if(d == "logt"){ # log student t

        dist.title = paste("Log T(",
                           signif(myfit2()$Log.Student.t[1,1], 3),
                           ", ",
                           signif(myfit2()$Log.Student.t[1,2], 3),
                           "), df = ",
                           myfit2()$Log.Student.t[1,3], sep="")

      }

      if(d == "beta"){

        dist.title =paste("Beta(",
                          signif(myfit2()$Beta[1,1], 3),
                          ", ", signif(myfit2()$Beta[1,2], 3),
                          ")", sep="")
      }

      if(d == "hist"){

        dist.title = "histogram fit"

      }

      if(d == "mirrorgamma"){

        dist.title = paste("Mirror gamma(",
                           signif(myfit2()$mirrorgamma[1,1], 3),
                           ", ",
                           signif(myfit2()$mirrorgamma[1,2], 3),
                           ")", sep="")
      }

      if(d == "mirrorlognormal"){

        dist.title = paste("Mirror log normal(",
                           signif(myfit2()$mirrorlognormal[1,1], 3),
                           ", ",
                           signif(myfit2()$mirrorlognormal[1,2], 3), ")",
                           sep="")
      }

      if(d == "mirrorlogt"){ # mirror log student t

        dist.title = paste("Mirror log T(",
                           signif(myfit2()$mirrorlogt[1,1], 3),
                           ", ",
                           signif(myfit2()$mirrorlogt[1,2], 3),
                           "), df = ",
                           myfit2()$mirrorlogt[1,3], sep="")

      }

      if (input$massHR1>0){
        dist.title <- paste(input$massHR1, "\u22c5 1 +", 1-input$massHR1, "\u22c5", dist.title)
      }

      p1 <- ggplot(data=HRsamples, aes(x=HR)) + geom_histogram(aes_string(y = "..density..")) + labs(title = dist.title) +  theme(plot.title = element_text(hjust = 0.5))

      print(p1)
    })

    # Functions for the Feedback tab ---------------------------------

    #This function allows the 10 and 90% CI lines to be drawn

    drawsamples <- reactive({

      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5
      nsamples <- 10000
      mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = nsamples, d = c(input$dist1, input$dist2)))
      u <- runif(nsamples, 0, 1)

      mySample[u<input$massT0,1] <- 0
      mySample[u<input$massHR1,2] <- 1

      return(list(mySample = mySample, nsamples = nsamples))
    })

    drawsimlines <- reactive({

      gammat <- input$gammac


      mySample <- drawsamples()$mySample
      nsamples <- 500

      time <- seq(0, exp((1.527/input$gammac)-log(input$lambdac))*1.1, by=0.01)

      #We fill a matrix with the treatment survival probabilities at each time
      SimMatrix <- matrix(NA, nrow = nsamples, ncol=length(time))

      for (i in 1:nsamples){

        bigT <- sample(mySample[,1], 1)
        HR <- sample(mySample[,2], 1)

        lambdat <- input$lambdac*HR^(1/input$gammac)
        if (bigT!=0){
          controltime <- seq(0, bigT, by=0.01)
          controlsurv <- exp(-(input$lambdac*controltime)^input$gammac)
        }


        treatmenttime <- seq(bigT, exp((1.527/input$gammac)-log(input$lambdac))*1.1, by=0.01)
        treatmentsurv <- exp(-(input$lambdac*bigT)^input$gammac - lambdat^gammat*(treatmenttime^gammat-bigT^gammat))

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
      medianTreatment <- rep(NA, length(time))
      for (j in 1:length(time)){
        lowerbound[j] <- quantile(SimMatrix[,j], 0.1)
        upperbound[j] <- quantile(SimMatrix[,j], 0.9)
        medianTreatment[j] <- quantile(SimMatrix[,j], 0.5)
      }

      return(list(lowerbound=lowerbound, upperbound=upperbound, time=time, SimMatrix = SimMatrix,
                  medianTreatment = medianTreatment))

    })


    output$plotFeedback <- renderPlot({

      #This plots the feedback plot
      gammat <- input$gammac
      controltime <- seq(0, exp((1.527/input$gammac)-log(input$lambdac))*1.1, by=0.01)
      controlsurv <- exp(-(input$lambdac*controltime)^input$gammac)
      controldf <- data.frame(x = controltime,
                              y = controlsurv)
      theme_set(theme_grey(base_size = input$fs))
      p1 <- ggplot(data=controldf, aes(x=x, y=y)) +
        geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1)




     simlinesmedian <- data.frame(x = drawsimlines()$time, y = drawsimlines()$medianTreatment)
     mybreaks <- plyr::round_any(seq(0, exp((1.527/input$gammac)-log(input$lambdac))*1.1, length=5), accuracy = 5)
     p1 <-  p1 + geom_line(data = simlinesmedian, aes(x = x, y = y), colour = "red") + scale_x_continuous(breaks = mybreaks)

     print(p1)


      #This code adds the three choices to the plots
      addfeedback <- input$showfeedback
      shinyjs::hide(id = "feedbackQuantile")

      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          #This adds the median survival line (onto the control and treatment)
          if (addfeedback[i]=="Median survival line(s)"){
            shinyjs::show(id = "feedbackQuantile")
            #Looks at whether the median time is before or after the delay
            medianTTime <- drawsimlines()$time[sum(drawsimlines()$medianTreatment>0.5)]
            medianCTime <- (1/input$lambdac)*(-log(0.5))^(1/input$gammac)
            if (abs(medianTTime-medianCTime)<0.001){
              mediandf <- data.frame(x = seq(0, medianCTime, length=2), y = rep(0.5, 2))
              mediandf1 <- data.frame(x = rep(medianCTime, 2), y = seq(0, 0.5, length=2))
              p1 <- p1 + geom_line(data = mediandf, aes(x = x, y = y), linetype = "dashed") + geom_line(data = mediandf1, aes(x = x, y = y), linetype="dashed") +
                scale_x_continuous(breaks = c(mybreaks, medianCTime), labels = c(mybreaks, round(medianCTime, 1)))
            } else {
              mediandf <- data.frame(x = seq(0, medianTTime, length=2), y = rep(0.5, 2))
              mediandf1 <- data.frame(x = rep(medianTTime, 2), y = seq(0, 0.5, length=2))
              mediandf2 <- data.frame(x = rep(medianCTime, 2), y = seq(0, 0.5, length=2))
              p1 <- p1 + geom_line(data = mediandf, aes(x = x, y = y), linetype = "dashed") + geom_line(data = mediandf1, aes(x = x, y = y), linetype="dashed") +
                geom_line(data = mediandf2, aes(x = x, y = y), linetype="dashed") +
                scale_x_continuous(breaks = c(mybreaks,medianCTime, medianTTime), labels = c(mybreaks,  round(medianCTime, 1), round(medianTTime, 1)))
            }
            #This uses the elicited distribution for T and adds 95% points onto the control curve
          } else if (addfeedback[i]=="95% CI for T"){
            mySample <- drawsamples()$mySample
            lowerT <- quantile(mySample[,1], 0.025)
            upperT <- quantile(mySample[,1], 0.975)
            upperTdf <- data.frame(x = upperT, y = controlsurv[sum(controltime<upperT)])
            p1 <- p1 + geom_point(data = upperTdf, aes(x = x, y = y), colour="orange", size = 4)
            if (lowerT==0){
              lowerTdf <- data.frame(x = lowerT, y = 1)
              p1 <- p1 + geom_point(data = lowerTdf, aes(x = x, y = y), colour="orange", size = 4)
            } else {
              lowerTdf <- data.frame(x = lowerT, y = controlsurv[sum(controltime<lowerT)])
              p1 <- p1 + geom_point(aes(x = x, y = y, colour="orange", size = 4))
            }


          } else if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
            #This adds the simulated confidence interval lines
            simlineslower <- data.frame(x = drawsimlines()$time, y = drawsimlines()$lowerbound)
            simlinesupper <- data.frame(x = drawsimlines()$time, y = drawsimlines()$upperbound)
            p1 <- p1 + geom_line(data = simlineslower, aes(x = x, y = y), linetype="dashed")+
              geom_line(data = simlinesupper, aes(x = x, y = y), linetype="dashed")
          }
        }
      }
      print(p1)

    })


    radiobuttons <- reactive({
      addfeedback <- input$showfeedback
    })



    output$quantilePlot <- renderPlot({

      addfeedback <- radiobuttons()


      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          if (addfeedback[i]=="Median survival line(s)"){

            quantileMatrix <- drawsimlines()$SimMatrix

            quantileTime <- drawsimlines()$time

            quantileVec <- rep(NA, length = nrow(quantileMatrix))

            for (j in 1:nrow(quantileMatrix)){
              quantileVec[j] <- quantileTime[which.min(abs(quantileMatrix[j,]-input$feedbackQuantile))]
            }

            quantiledf <- data.frame(quantiletime = quantileVec)

            theme_set(theme_grey(base_size = input$fs))
            p1 <- ggplot(data=quantiledf, aes(x=quantiletime)) + geom_histogram(aes_string(y = "..density.."), binwidth = 5) + xlim(0, exp((1.527/input$gammac)-log(input$lambdac))*1.1) +
              xlab("Time")

            print(p1)

          }
        }
      }


    })


    # Functions for the Assurance tab ---------------------------------

    #This function calculates the normal assurance given the elicited distributions and other simple questions about the trial
    calculateNormalAssurance <- eventReactive(input$drawAssurance, {

      #Makes the simplification
      gammat <- input$gammac
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5

      assFunc <- function(n1, n2){


        #Simulate 400 observations for T and HR given the elicited distributions
        #For each n1, n2, simulate 400 trials
        assnum <- 400
        assvec <- rep(NA, assnum)
        AHRvec <- rep(NA, assnum)
        LBAHRvec <- rep(NA, assnum)
        UBAHRvec <- rep(NA, assnum)
        TPPvec <- rep(NA, assnum)
        eventsvec <- rep(NA, assnum)
        mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = assnum, d = c(input$dist1, input$dist2)))


        for (i in 1:assnum){

          u <- runif(1, 0, 1)

          if (u>input$massT0){
            bigT <- mySample[i,1]
          } else {
            bigT <- 0
          }

          u <- runif(1, 0, 1)

          if (u>input$massHR1){
            HR <- mySample[i,2]
          } else {
            HR <- 1
          }

          lambdat <- input$lambdac*HR^(1/input$gammac)

          dataCombined <- SimDTEDataSet(n1, n2, gammat, input$gammac, lambdat, input$lambdac, bigT, input$rectime, input$chosenLength)

          coxmodel <- coxph(Surv(time, event)~group, data = dataCombined)

          AHRvec[i] <- exp(coef(coxmodel))

          TPPvec[i] <- exp(coef(coxmodel)) < input$TPP

          CI <- exp(confint(coxmodel))

          LBAHRvec[i] <- CI[1]

          UBAHRvec[i] <- CI[2]

          #Performs a log rank test on the data
          test <- survdiff(Surv(time, event)~group, data = dataCombined)
          #If the p-value of the test is less than 0.05 then assvec = 1, 0 otherwise
          assvec[i] <- test$chisq > qchisq(0.95, 1)

          #Counts how many events have been seen up until the total trial length time
          eventsvec[i] <-  sum(dataCombined$time<input$chosenLength)

        }

        AHRvec[is.infinite(AHRvec)]<-NA
        TPPvec[is.infinite(TPPvec)]<-NA
        LBAHRvec[is.infinite(LBAHRvec)]<-NA
        UBAHRvec[is.infinite(UBAHRvec)]<-NA


        return(list(assvec = mean(assvec), LBAHRvec = mean(LBAHRvec, na.rm=T), UBAHRvec = mean(UBAHRvec, na.rm = T),
                    TPPvec = mean(TPPvec, na.rm = T), AHRvec = mean(AHRvec, na.rm=T), eventvec = mean(eventsvec), assnum=assnum))
      }

      #Looking at assurance for varying sample sizes
      samplesizevec <- seq(30, input$numofpatients, length=15)
      n1vec <- floor(input$n1*(samplesizevec/(input$n1+input$n2)))
      n2vec <- ceiling(input$n2*(samplesizevec/(input$n1+input$n2)))
      calcassvec <- rep(NA, length = length(samplesizevec))

      pboptions(type="shiny", title = "Calculating assurance")

      calcassvec <- pbmapply(assFunc, n1vec, n2vec)

      assvec <- unlist(calcassvec[1,])

      LBAHRvec <- unlist(calcassvec[2,])

      UBAHRvec <- unlist(calcassvec[3,])

      TPPvec <- unlist(calcassvec[4,])

      AHRvec <- unlist(calcassvec[5,])

      eventvec <- unlist(calcassvec[6,])

      assnumvec <- unlist(calcassvec[7,])

      LBassvec <- assvec-1.96*sqrt(assvec*(1-assvec)/assnumvec)

      UBassvec <- assvec+1.96*sqrt(assvec*(1-assvec)/assnumvec)

      LBTPPvec <- TPPvec-1.96*sqrt(TPPvec*(1-TPPvec)/assnumvec)

      UBTPPvec <- TPPvec+1.96*sqrt(TPPvec*(1-TPPvec)/assnumvec)

      #How many events are seen given this set up
      eventsseen <- eventvec[length(eventvec)]

      #Smooth the assurance, compared to the the sample size vector
      asssmooth <- loess(assvec~samplesizevec)

      AHRsmooth <- loess(AHRvec~samplesizevec)

      LBsmooth <- loess(LBAHRvec~samplesizevec)

      UBsmooth <- loess(UBAHRvec~samplesizevec)

      TPPsmooth <- loess(TPPvec~samplesizevec)

      LBasssmooth <- loess(LBassvec~samplesizevec)

      UBasssmooth <- loess(UBassvec~samplesizevec)

      LBTPPsmooth <- loess(LBTPPvec~samplesizevec)

      UBTPPsmooth <- loess(UBTPPvec~samplesizevec)


      return(list(calcassvec = calcassvec, asssmooth = asssmooth, samplesizevec = samplesizevec,
                  eventsseen = eventsseen, AHRsmooth = AHRsmooth, LBsmooth = LBsmooth, UBsmooth = UBsmooth, TPPsmooth = TPPsmooth,
                  LBasssmooth = LBasssmooth, UBasssmooth = UBasssmooth, LBTPPsmooth = LBTPPsmooth, UBTPPsmooth = UBTPPsmooth))

    })

    output$assurancePlot <- renderPlot({

      #Plot the assurance calculated in the function
      theme_set(theme_grey(base_size = input$fs))
      assurancenormaldf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$asssmooth))
      assurancenormalLBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBasssmooth))
      assurancenormalUBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBasssmooth))
      TPPdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$TPPsmooth))
      TPPLBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBTPPsmooth))
      TPPUBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBTPPsmooth))
      p1 <- ggplot() + geom_line(data = assurancenormaldf, aes(x = x, y = y, colour="Assurance"), linetype="solid") + xlab("Number of patients") +
        ylab("Assurance") + ylim(0, 1.05) + geom_line(data = TPPdf, aes(x = x, y = y, colour = 'target effect'), linetype="solid") +
        geom_line(data = assurancenormalLBdf, aes(x = x, y = y, colour = 'Assurance'), linetype='dashed') +
        geom_line(data = assurancenormalUBdf, aes(x = x, y = y, colour = 'Assurance'), linetype='dashed') +
        geom_line(data = TPPLBdf, aes(x = x, y = y, colour = 'target effect'), linetype='dashed') +
        geom_line(data = TPPUBdf, aes(x = x, y = y, colour = 'target effect'), linetype='dashed')
      scale_color_manual(name='Type of assurance',
                         breaks=c('Assurance', 'target effect'),
                         values=c('Assurance'='blue', 'target effect' = 'green'))
      print(p1)
    })


    output$AHRPlot <- renderPlot({

      AHRdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$AHRsmooth))
      LBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBsmooth))
      UBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBsmooth))

      p1 <- ggplot() + geom_line(data = AHRdf, aes(x = x, y = y, colour="Average HR"), linetype="solid") + xlab("Number of patients") +
        ylab("Average hazard ratio") + geom_line(data = LBdf, aes(x = x, y = y, colour = "CI"), linetype="dashed") +
        geom_line(data = UBdf, aes(x = x, y = y, colour = "CI"), linetype="dashed") +
        scale_color_manual(name='Average hazard ratio',
                           breaks=c('Average HR', 'CI'),
                           values=c('Average HR'='red', 'CI'='black'))
      print(p1)


    })


    output$assuranceText  <- renderUI({

      #Show how many events are seen given the set up
      str1 <- paste0("On average, ", round(calculateNormalAssurance()$eventsseen), " events are seen when ", input$numofpatients, " patients are enroled for ", input$chosenLength, " months")
      HTML(paste(str1, sep = '<br/>'))
    })


    # Functions for the well panel ---------------------------------

    df1 <- reactive({
      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5
      data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs,
                                     n = input$ss,
                                     d = c(input$dist1, input$dist2)))
    })

    observeEvent(input$exit, {
      stopApp(list(parameter1 = myfit1(), parameter2 = myfit2(),
                   cp = 0.5))
    })

    output$downloadData <- downloadHandler(
      filename = "DTEsample.csv",
      content = function(file) {
        utils::write.csv(df1(), file, row.names = FALSE)
      }
    )

    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "distributions-report.html",
                                   pdf_document = "distributions-report.pdf",
                                   word_document = "distributions-report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "DTEShinySummary.Rmd")
        file.copy(system.file("DTEAppFiles", "DTEShinySummary.Rmd",
                              package="DTEAssurance"),
                  tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(fit1 = myfit1(), fit2 = myfit2(), cp = 0.5,
                       d = c(input$dist1, input$dist2), m1 = m1(), m2 = m2(),
                       massT0 = input$massT0, massHR1 = input$massHR1)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          output_format = input$outFormat,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  }

  shiny::shinyApp(ui, server)

}






