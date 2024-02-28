#' Shiny app which calculates assurance for trials with DTE
#'
#' Launches a shiny app to use for elicitation for when delayed treatment effects are
#' likely to be present in the clinical trial you are designing: takes these elicited distributions
#' and calculates assurance.
#'
#' You should run the function with no arguments.
#
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
#' @rawNamespace import(shinyjs, except = runExample)
#' @import utils
#'

DTEAssuranceApp <- function(){

  source("R/helperfunctions.R")

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
                 hidden(wellPanel(
                   id = "instruct_panel",
                   h4("Instructions"),
                   tags$ol(
                     tags$li("You are able to upload an MCMC sample to estimate the control survival curve.
                   The MCMC sample must be in one of the following three file formats: '.csv', '.xlsx' or '.rds'. The
                           file must contain nothing but two columns of samples. The first column must contain the output for the SHAPE parameter
                           and the second column must contain the output for the SCALE parameter.")))),
                 sidebarLayout(
                   sidebarPanel = sidebarPanel(
                     radioButtons("uploadSampleCheck", "Do you wish to upload a MCMC sample?", choices = c("Yes", "No"), selected = "No"),
                     hidden(fileInput("uploadSample", "Upload your control sample",accept = c(".csv", ".rds", ".xlsx"))),
                     numericInput('lambdacmean', label =  HTML(paste0("scale (\u03bb",tags$sub("c"), ")")), value = 0.08, min=0),
                     numericInput('gammacmean', label =  HTML(paste0("scale (\u03b3",tags$sub("c"), ")")), value = 0.8)


                   ),
                   mainPanel = mainPanel(
                     plotOutput("plotControl"),
                     htmlOutput("recommendedParams")
                   )
                 ),
        ),

        # Conditional probabilities UI ---------------------------------
        tabPanel("Conditional probabilities",
                 fluidRow(
                   column(6,
                          numericInput("P_S", "Pr(survival curves separate)", value = 0.9, min = 0, max = 1)

                   ),
                   column(6,
                          numericInput("P_DTE", "Pr(treatment subject to a delay|survival curves separate)", value = 0.6, min = 0, max = 1)

                   )
                 )
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
                   )


                 ),
                 plotOutput("distPlot1"),
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
                   )


                 ),

                 plotOutput("distPlot2"),
        ),

        # Feedback UI ---------------------------------
        tabPanel("Feedback",
                 sidebarLayout(
                   sidebarPanel = sidebarPanel(
                     checkboxGroupInput("showfeedback", "Add to plot", choices = c("Median survival line", "95% CI for T", "CI for Treatment Curve (0.1 and 0.9)")),
                     hidden(numericInput("feedbackQuantile", "Uncertainty about the following survival quantile:", value = 0.5, min = 0, max = 1)),
                     hidden(numericInput("timeInputFeedback", "Prior information about time:", value = 25, min = 0, max = 100))
                   ),
                   mainPanel = mainPanel(
                     plotOutput("plotFeedback"),
                     htmlOutput("medianSurvivalFeedback"),
                     htmlOutput("priorWorthFeedback"),
                     plotOutput("quantilePlot"),
                     htmlOutput("quantileFeedback")
                   )
                 ),
        ),

        # Assurance UI ---------------------------------
        tabPanel("Calculating assurance",
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
                     plotOutput("AHRPlot"),
                     htmlOutput("AHRFeedback")
                   )
                 ),

        ),

        #Help UI ---------------------------------
        #Need to say what files can be uploaded in the control sample
        #Link to SHELF for the elicitation
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
                 HTML("<p>The plot shows the control survival curve (from the control tab), along with the median elicited treatment line (calculated from the previous two tabs).
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

    # Functions for the control tab ---------------------------------

    #Reactive which determines whether addKM button has been clicked
    v <- reactiveValues(upload = NULL)

    #Simulates control curves and plots time-wise CI
    controlCILines <- reactive({

      #Generate n control curves
      nsamples <- 500

      #Calculate the x-axis limits
      controlTime <- seq(0, 10000, by=0.01)
      controlCurve <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)
      finalSurvTime <- controlTime[which(controlCurve<0.01)[1]]

      #Re-define the x-axis
      controlTime <- seq(0, finalSurvTime, length = 100)


      #We fill a matrix with the control survival probabilities at each time
      SimMatrix <- matrix(NA, nrow = nsamples, ncol=length(controlTime))

      #Sample lambdac and gammac from the inputs
      lambdacsample <- inputData()$scale
      gammacsample <- inputData()$shape

      chosenMCMCsample <- sample(1:nsamples, size = nsamples, replace = T)
      #Fills matrix with control curves
      for (i in 1:nsamples){
        SimMatrix[i,] <- exp(-(lambdacsample[chosenMCMCsample[i]]*controlTime)^gammacsample[chosenMCMCsample[i]])
      }


      #Finding 95% estimates for the control curve(s)
      lowerbound <- rep(NA, length(controlTime))
      upperbound <- rep(NA, length(controlTime))
      mediancontrol <- rep(NA, length(controlTime))
      for (j in 1:length(controlTime)){
        lowerbound[j] <- quantile(SimMatrix[,j], 0.05)
        upperbound[j] <- quantile(SimMatrix[,j], 0.95)
        mediancontrol[j] <- quantile(SimMatrix[,j], 0.5)
      }

      return(list(lowerbound=lowerbound, upperbound=upperbound, controlTime=controlTime, SimMatrix = SimMatrix,
                  mediancontrol = mediancontrol))

    })

    observe({
      if (input$uploadSampleCheck=="No"){
        shinyjs::hide(id = "uploadSample")
        shinyjs::reset(id = "uploadSample")
        shinyjs::hide(id = "instruct_panel")
        v$upload <- "no"
      } else if (input$uploadSampleCheck=="Yes"){
        shinyjs::show(id = "uploadSample")
        shinyjs::show(id = "instruct_panel")
      }

    })

    observeEvent(input$uploadSample,{
      v$upload <- "yes"
    })

    inputData <- reactive({
      #Allows the user to upload a control sample

      if (v$upload=="no"){
        return(NULL)
      } else {
        chosenFile <- input$uploadSample
        req(chosenFile)
        if (endsWith(chosenFile$name, ".xlsx")){
          controlMCMC <- read_excel(chosenFile$datapath, sheet = 1)
        } else if (endsWith(chosenFile$name, "csv")){
          controlMCMC <- read.csv(chosenFile$datapath)
        } else if (endsWith(chosenFile$name, "rds")){
          controlMCMC <- readRDS(chosenFile$datapath)
        }

        #lambdac and gammac are estimated from the uploaded control sample
        shape <- unlist(controlMCMC[,1])
        scale <- unlist(controlMCMC[,2])
        updateTextInput(session, "lambdacmean", value = signif(mean(scale), 3))
        updateTextInput(session, "gammacmean", value = signif(mean(shape), 3))
        return(list(shape = shape, scale = scale))
      }

    })

    output$recommendedParams <- renderUI({
      if (v$upload=="yes"){
        #Tells the user what the best fitting parameters are for their uploaded sample
        str1 <- paste0("For your uploaded sample, the best fitting parameters are:")
        str2 <- withMathJax(paste0("$$\\lambda_c =  ",signif(mean(inputData()$scale), 3),"$$", "and", "$$\\gamma_c =  ",signif(mean(inputData()$shape), 3),"$$"))
        HTML(paste(str1, str2, sep = '<br/>'))
      } else {

      }

    })

    output$plotControl <- renderPlot({

      #Calculate the x-axis limits
      controlTime <- seq(0, 10000, by=0.01)
      controlCurve <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)
      finalSurvTime <- controlTime[which(controlCurve<0.01)[1]]

      if (v$upload=="no"){

        #Re-define the x-axis
        controlTime <- seq(0, finalSurvTime, length = 100)
        controlSurv <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)

        # #Plots the median control curve along with the CI
        controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)

        theme_set(theme_grey(base_size = 12))
        p1 <- ggplot(data=controlDF, aes(x=controlTime, y=controlSurv)) + xlim(0, finalSurvTime) +
          geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1)

        print(p1)
      } else {
        controlDF <- data.frame(controlTime = controlCILines()$controlTime, controlSurv = controlCILines()$mediancontrol)
        controlLB <- data.frame(controlTime = controlCILines()$controlTime, controlSurv = controlCILines()$lowerbound)
        controlUB <- data.frame(controlTime = controlCILines()$controlTime, controlSurv = controlCILines()$upperbound)

        theme_set(theme_grey(base_size = 12))

        p1 <- ggplot(data=controlDF, aes(x=controlTime, y=controlSurv)) + xlim(0, finalSurvTime) +
          geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1) +
          geom_line(data = controlLB, aes(x=controlTime, y=controlSurv), linetype="dashed") +
          geom_line(data = controlUB, aes(x=controlTime, y=controlSurv), linetype="dashed")

        print(p1)

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


    elicitedSamples <- reactive({

      conc.probs <- matrix(0, 2, 2)
      conc.probs[1, 2] <- 0.5
      nsamples <- 10000
      mySample <- data.frame(copulaSample(myfit1(), myfit2(), cp = conc.probs, n = nsamples, d = c(input$dist1, input$dist2)))

      for (i in 1:nsamples){
        u <- runif(1)
        if (u < input$P_S){
          w <- runif(1)
          if (w > input$P_DTE){
            mySample[i,1] <- 0
          }
        } else {
          mySample[i,2] <- 1
          mySample[i,1] <- 0
        }
      }


      return(list(mySample = mySample, nsamples = nsamples))
    })

    # Functions for the eliciting length of delay tab ---------------------------------

    output$distPlot1 <- renderPlot({

      mySample <- elicitedSamples()$mySample
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

      p1 <- ggplot(data=Tsamples, aes(x=time)) + geom_histogram(aes(y = after_stat(density))) + labs(title = dist.title) +  theme(plot.title = element_text(hjust = 0.5))

      print(p1)

    })

    # Functions for the post-delay HR tab ---------------------------------

    output$distPlot2 <- renderPlot({


      mySample <- elicitedSamples()$mySample
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

      p1 <- ggplot(data=HRsamples, aes(x=HR)) + geom_histogram(aes(y = after_stat(density))) + labs(title = dist.title) +  theme(plot.title = element_text(hjust = 0.5))

      print(p1)

    })



    # Functions for the Feedback tab ---------------------------------


    treatmentCILines <- reactive({

      gammat <- input$gammacmean

      mySample <- elicitedSamples()$mySample

      #Generate n control curves
      nsamples <- 500

      #Calculate the x-axis limits
      controlTime <- seq(0, 10000, by=0.01)
      controlCurve <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)
      finalSurvTime <- controlTime[which(controlCurve<0.01)[1]]


      #We fill a matrix with the treatment survival probabilities at each time
      SimMatrix <- matrix(NA, nrow = nsamples, ncol=200)

      bigT <- sample(mySample[,1], size = nsamples, replace = T)
      HR <- sample(mySample[,2], size = nsamples, replace = T)


      for (i in 1:nsamples){

        lambdat <- input$lambdacmean*HR[i]^(1/input$gammacmean)

        TreatmentTime1 <- seq(0, bigT[i], length = 100)
        TreatmentSurv1 <- exp(-(input$lambdacmean*TreatmentTime1)^input$gammacmean)

        TreatmentTime2 <- seq(bigT[i], finalSurvTime, length = 100)
        TreatmentSurv2 <- exp(-(input$lambdacmean*bigT[i])^input$gammacmean - lambdat^gammat*(TreatmentTime2^gammat-bigT[i]^gammat))

        TreatmentTimeCombined <- c(TreatmentTime1, TreatmentTime2)
        TreatmentSurvCombined <- c(TreatmentSurv1, TreatmentSurv2)

        #The i'th row of the matrix is filled with the survival probabilities for these sampled T and HR
        SimMatrix[i,] <- TreatmentSurvCombined

      }

      #We now look at each time iteration at the distribution
      #We look at the 0.1 and 0.9 quantile of the distribution
      #These quantiles can be thought of as confidence intervals for the treatment curve, taken from
      #the elicited distributions
      lowerbound <- rep(NA, length(TreatmentTimeCombined))
      upperbound <- rep(NA, length(TreatmentTimeCombined))
      medianTreatment <- rep(NA, length(TreatmentTimeCombined))
      for (j in 1:length(TreatmentTimeCombined)){
        lowerbound[j] <- quantile(SimMatrix[,j], 0.1)
        upperbound[j] <- quantile(SimMatrix[,j], 0.9)
        medianTreatment[j] <- quantile(SimMatrix[,j], 0.5)
      }

      return(list(lowerbound=lowerbound, upperbound=upperbound, TreatmentTimeCombined=TreatmentTimeCombined, SimMatrix = SimMatrix,
                  medianTreatment = medianTreatment))

    })


    output$priorWorthFeedback <- renderUI({

      addfeedback <- radiobuttons()

      str1 <- ""

      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
            simlineslower <- data.frame(x = treatmentCILines()$TreatmentTimeCombined, y = treatmentCILines()$lowerbound)
            simlinesupper <- data.frame(x = treatmentCILines()$TreatmentTimeCombined, y = treatmentCILines()$upperbound)


            CIwidth <- simlinesupper[(which.min(abs(simlinesupper$x-input$timeInputFeedback))),]$y - simlineslower[(which.min(abs(simlineslower$x-input$timeInputFeedback))),]$y
            midpoint <- (simlinesupper[(which.min(abs(simlinesupper$x-input$timeInputFeedback))),]$y + simlineslower[(which.min(abs(simlineslower$x-input$timeInputFeedback))),]$y)/2
            n <- (16*midpoint*(1-midpoint))/(CIwidth^2)
            str1 <- paste0("The confidence interval width at t = ", input$timeInputFeedback, " is equivalent to  ", round(n, 0),
                           " patients from a Binomial distribution")
          }
        }
      }

      HTML(paste(str1, sep = '<br/>'))

    })

    output$medianSurvivalFeedback <- renderUI({

      addfeedback <- radiobuttons()

      str1 <- ""

      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          if (addfeedback[i]=="Median survival line"){
            medianTTime <- round(treatmentCILines()$TreatmentTimeCombined[sum(treatmentCILines()$medianTreatment>0.5)], 1)
            medianCTime <- round((1/input$lambdacmean)*(-log(0.5))^(1/input$gammacmean), 1)
            str1 <- paste0("The median survival time on the control is ", medianCTime, " and the median survival time on the treatment is ", medianTTime)
          }
        }
      }

      HTML(paste(str1, sep = '<br/>'))

    })

    output$plotFeedback <- renderPlot({
      #This plots the feedback plot

      if (v$upload=="no"){

        controlTime <- seq(0, 10000, by=0.01)
        controlCurve <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)
        finalSurvTime <- controlTime[which(controlCurve<0.01)[1]]

        #Re-define the x-axis
        controlTime <- seq(0, finalSurvTime, length = 100)
        controlSurv <- exp(-(input$lambdacmean*controlTime)^input$gammacmean)

        controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
      } else {
        controlDF <- data.frame(controltime = controlCILines()$controlTime, controlSurv = controlCILines()$mediancontrol)
      }

      theme_set(theme_grey(base_size = 12))
      p1 <- ggplot(data=controlDF, aes(x=controlTime, y=controlSurv)) +
        geom_line(colour="blue") + xlab("Time") + ylab("Survival") + ylim(0,1)


      treatmentDF <- data.frame(x = treatmentCILines()$TreatmentTimeCombined, y = treatmentCILines()$medianTreatment)
      p1 <-  p1 + geom_line(data = treatmentDF, aes(x = x, y = y), colour = "red")

      print(p1)


      #This code adds the three choices to the plots
      addfeedback <- input$showfeedback
      shinyjs::hide(id = "feedbackQuantile")
      shinyjs::hide(id = "timeInputFeedback")

      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          #This adds the median survival line (onto the control and treatment)
          if (addfeedback[i]=="Median survival line"){
            #Looks at whether the median time is before or after the delay
            medianTTime <- treatmentCILines()$TreatmentTimeCombined[sum(treatmentCILines()$medianTreatment>0.5)]
            medianCTime <- (1/input$lambdacmean)*(-log(0.5))^(1/input$gammacmean)
              mediandf <- data.frame(x = seq(0, medianTTime, length=2), y = rep(0.5, 2))
              mediandf1 <- data.frame(x = rep(medianTTime, 2), y = seq(0, 0.5, length=2))
              mediandf2 <- data.frame(x = rep(medianCTime, 2), y = seq(0, 0.5, length=2))
              p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") + geom_line(data = mediandf1, aes(x = x, y=y), linetype="dashed") +
                geom_line(data = mediandf2, aes(x = x, y=y), linetype="dashed")

            #This uses the elicited distribution for T and adds 95% points onto the control curve
          } else if (addfeedback[i]=="95% CI for T"){

            mySample <- elicitedSamples()$mySample
            lowerT <- quantile(mySample[,1], 0.025)
            upperT <- quantile(mySample[,1], 0.975)
            p1 <- p1 + geom_point(aes(x = upperT, y = controlSurv[sum(controlTime<upperT)]), colour="orange", size = 4)
            if (lowerT==0){
              p1 <- p1 + geom_point(aes(x = lowerT, y = 1), colour="orange", size = 4)
            } else {
              p1 <- p1 + geom_point(aes(x = lowerT, y = controlSurv[sum(controlTime<lowerT)]), colour="orange", size = 4)
            }

          } else if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
            shinyjs::show(id = "timeInputFeedback")
            shinyjs::show(id = "feedbackQuantile")
            #This adds the simulated confidence interval lines
            simlineslower <- data.frame(x = treatmentCILines()$TreatmentTimeCombined, y = treatmentCILines()$lowerbound)
            simlinesupper <- data.frame(x = treatmentCILines()$TreatmentTimeCombined, y = treatmentCILines()$upperbound)
            p1 <- p1 + geom_line(data = simlineslower, aes(x=x, y=y), linetype="dashed")+
              geom_line(data = simlinesupper, aes(x=x, y=y), linetype="dashed")


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
          if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){

            quantileMatrix <- treatmentCILines()$SimMatrix

            quantileTime <- treatmentCILines()$TreatmentTimeCombined

            quantileVec <- rep(NA, length = nrow(quantileMatrix))

            for (j in 1:nrow(quantileMatrix)){
              quantileVec[j] <- quantileTime[which.min(abs(quantileMatrix[j,]-input$feedbackQuantile))]
            }

            quantiledf <- data.frame(quantiletime = quantileVec)

            theme_set(theme_grey(base_size = 12))
            p1 <- ggplot(data=quantiledf, aes(x=quantiletime)) + geom_histogram(aes(y = after_stat(density)), binwidth = 5) + xlim(0, exp((1.527/input$gammacmean)-log(input$lambdacmean))*1.1) +
              xlab("Time")

            print(p1)

          }
        }
      }


    })

    output$quantileFeedback <- renderUI({

      addfeedback <- radiobuttons()

      str1 <- ""

      if (!is.null(addfeedback)){
        for (i in 1:length(addfeedback)){
          if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
            str1 <- paste0("This plot shows the distribution of samples for treatment group for the ", input$feedbackQuantile, " quantile")
          }
        }
      }

      HTML(paste(str1, sep = '<br/>'))

    })

    # Functions for the Assurance tab ---------------------------------

    #This function calculates the normal assurance given the elicited distributions and other simple questions about the trial
    calculateNormalAssurance <- eventReactive(input$drawAssurance, {

      #Makes the simplification
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

          if (is.null(v$upload)){
            lambdac <- input$lambdacmean
            gammac <- input$gammacmean
          } else {
            lambdac <- sample(as.numeric(inputData()$scale), size = 1)
            gammac <- sample(as.numeric(inputData()$shape), size = 1)
          }

          gammat <- gammac

          lambdat <- lambdac*HR^(1/gammac)

          dataCombined <- SimDTEDataSet(n1, n2, gammat, gammac, lambdat, lambdac, bigT, input$rectime, input$chosenLength)

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
      theme_set(theme_grey(base_size = 12))
      assurancenormaldf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$asssmooth))
      assurancenormalLBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBasssmooth))
      assurancenormalUBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBasssmooth))
      TPPdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$TPPsmooth))
      TPPLBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBTPPsmooth))
      TPPUBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBTPPsmooth))
      p1 <- ggplot() + geom_line(data = assurancenormaldf, aes(x = x, y = y, colour="Assurance"), linetype="solid") + xlab("Number of patients") +
        ylab("Assurance") + ylim(0, 1.05) +
        geom_line(data = TPPdf, aes(x=x, y=y, colour = 'Target effect'), linetype="solid") +
        geom_line(data = assurancenormalLBdf, aes(x=x, y=y, colour = 'Assurance'), linetype='dashed') +
        geom_line(data = assurancenormalUBdf, aes(x=x, y=y, colour = 'Assurance'), linetype='dashed') +
        geom_line(data = TPPLBdf, aes(x=x, y=y, colour = 'Target effect'), linetype='dashed') +
        geom_line(data = TPPUBdf, aes(x=x, y=y, colour = 'Target effect'), linetype='dashed') + theme(
          legend.position = c(.05, .95),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(name=NULL,
                                                                   breaks=c('Assurance', 'Target effect'),
                                                                   values=c('Assurance'='blue', 'Target effect' = 'orange'))
      print(p1)
    })


    output$AHRPlot <- renderPlot({

      AHRdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$AHRsmooth))
      LBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$LBsmooth))
      UBdf <- data.frame(x = calculateNormalAssurance()$samplesizevec, y = predict(calculateNormalAssurance()$UBsmooth))

      p1 <- ggplot() + geom_line(data = AHRdf, aes(x = x, y = y, colour="Average HR"), linetype="solid") + xlab("Number of patients") +
        ylab("Average hazard ratio") + geom_line(data = LBdf, aes(x=x, y=y, colour = "CI"), linetype="dashed") +
        geom_line(data = UBdf, aes(x=x, y=y, colour = "CI"), linetype="dashed") +
        theme(
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(name=NULL,
                                                                   breaks=c('Average HR', 'CI'),
                                                                   values=c('Average HR'='red', 'CI' = 'black'))
      print(p1)




    })


    output$assuranceText  <- renderUI({
      #Show how many events are seen given the set up
      str1 <- paste0("The ","<font color=\"#0000FF\"><b>blue</b></font>", " line is the proportion of trials that give rise to a 'successful' outcome.")
      str2 <- paste0("The ", "<font color=\"#FFA500\"><b>orange</b></font>", " line is the  proportion of trials in which the estimated average hazard ratio is less than the target effect - ", input$TPP, ".")
      str3 <- paste0("On average, ", round(calculateNormalAssurance()$eventsseen), " events are seen when ", input$numofpatients, " patients are enroled for ", input$chosenLength, " months.")
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    output$AHRFeedback  <- renderUI({
      #Show how many events are seen given the set up
      x <-  round(calculateNormalAssurance()$eventsseen)
      str1 <- paste0("The ","<font color=\"##FF0000\"><b>red</b></font>", " line is the average estimated hazard ratio.")
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






