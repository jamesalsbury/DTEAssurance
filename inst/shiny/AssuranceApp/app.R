library(SHELF)
library(survMisc)
library(ggplot2)
library(pbapply)
library(readxl)
library(shiny)
library(shinydashboard)
library(survival)
library(plyr)
library(rmarkdown)
library(stats)
library(nleqslv)
library(graphics)
library(shinyjs)
library(utils)
library(nleqslv)


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
                     selectInput("ControlDist", "Distribution", choices = c("Exponential", "Weibull"), selected = "Exponential"),
                     # Conditional UI for Exponential distribution
                     conditionalPanel(
                       condition = "input.ControlDist == 'Exponential'",
                       selectInput("ExpChoice", "Choice", choices = c("Single Value", "Distribution", "rBEST"), selected = "Single Value"),
                       conditionalPanel(
                         condition = "input.ExpChoice == 'Single Value'",
                         numericInput("ExpRate", label =  HTML(paste0("rate (\u03bb)")), value = 0.08, min=0)
                       ),
                       conditionalPanel(
                         condition = "input.ExpChoice == 'Distribution'",
                         numericInput("ExpSurvTime", label = "Survival Time", value = 12),
                         fluidRow(
                           column(6,
                                  uiOutput("ExpDistText")
                           ),
                           column(4,
                                  numericInput("ExpBetaA", label = NULL, value = 20)
                           ),
                           column(1,
                                  tags$div(", ")
                           ),
                           column(4,
                                  numericInput("ExpBetaB", label = NULL, value = 32)
                           ),
                           column(1,
                                  tags$div(")")
                           )
                         )


                       ),
                       conditionalPanel(
                         condition = "input.ExpChoice == 'rBEST'",

                       ),
                     ),
                     # Conditional UI for Weibull distribution
                     conditionalPanel(
                       condition = "input.ControlDist == 'Weibull'",
                       selectInput("WeibullChoice", "Choice", choices = c("Single Value", "Distribution"), selected = "Single Value"),
                       conditionalPanel(
                         condition = "input.WeibullChoice == 'Single Value'",
                         fluidRow(
                           column(6,
                                  numericInput("WeibullScale", label =  HTML(paste0("scale (\u03bb",tags$sub("c"), ")")), value = 0.08, min=0)

                           ),
                           column(6,
                                  numericInput("WeibullShape", label =  HTML(paste0("shape (\u03b3",tags$sub("c"), ")")), value = 1, min=0)

                           )
                         )
                       ),

                       conditionalPanel(
                         condition = "input.WeibullChoice == 'Distribution'",
                         fluidRow(
                           column(6,
                                  numericInput("WeibullDistT1", label =  HTML(paste0("t",tags$sub("1"))), value = 12)

                           ),
                           column(6,
                                  numericInput("WeibullDistT2", label =  HTML(paste0("t",tags$sub("2"))), value = 18)

                           )
                         ),
                         fluidRow(
                           column(6,
                                  uiOutput("WeibullDistS1Text")
                           ),
                           column(4,
                                  numericInput("WeibullDistS1BetaA", label = NULL, value = 20)
                           ),
                           column(1,
                                  tags$div(", ")
                           ),
                           column(4,
                                  numericInput("WeibullDistS1BetaB", label = NULL, value = 32)
                           ),
                           column(1,
                                  tags$div(")")
                           )
                         ),
                         fluidRow(
                           column(6,
                                  uiOutput("WeibullDistDelta1Text")
                           ),
                           column(4,
                                  numericInput("WeibullDistDelta1BetaA", label = NULL, value = 20)
                           ),
                           column(1,
                                  tags$div(", ")
                           ),
                           column(4,
                                  numericInput("WeibullDistDelta1BetaB", label = NULL, value = 120)
                           ),
                           column(1,
                                  tags$div(")")
                           )
                         )
                       ),

                     )


                   ),
                   mainPanel = mainPanel(
                     plotOutput("plotControl")
                   )
                 ),
        ),

        # Conditional probabilities UI ---------------------------------
        tabPanel("Conditional probabilities",
                 fluidRow(
                   column(6,
                          numericInput("P_S", "Pr(survival curves separate)", value = 1, min = 0, max = 1)

                   ),
                   column(6,
                          numericInput("P_DTE", "Pr(treatment subject to a delay|survival curves separate)", value = 0, min = 0, max = 1)

                   )
                 )
        ),

        # Length of delay UI ---------------------------------
        tabPanel("Eliciting the length of delay",
                 fluidRow(
                   column(4,
                          textInput("TLimits", label = h5("Length of delay limits"),
                                    value = "0, 12")
                   ),
                   column(4,
                          textInput("TValues", label = h5("Length of delay values"),
                                    value = "5.5, 6, 6.5")
                   ),
                   column(4,
                          textInput("TProbs", label = h5("Cumulative probabilities"),
                                    value = "0.25, 0.5, 0.75")
                   )
                 ),
                 fluidRow(
                   column(4,
                          selectInput("TDist", label = h5("Distribution"),
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
                     condition = "input.TDist == 't' || input.TDist == 'logt' || input.TDist == 'mirrorlogt'",
                     numericInput("tdf1", label = h5("Student-t degrees of freedom"),
                                  value = 3)
                   )
                   )


                 ),
                 plotOutput("TPlot"),
        ),
        # post-delay HR UI ---------------------------------
        tabPanel("Eliciting the post-delay hazard ratio",
                 fluidRow(
                   column(4,
                          textInput("HRLimits", label = h5("Post-delay hazard ratio limits"),
                                    value = "0, 1")
                   ),
                   column(4,
                          textInput("HRValues", label = h5("Post-delay hazard ratio values"),
                                    value = "0.5, 0.6, 0.7")
                   ),
                   column(4,
                          textInput("HRProbs", label = h5("Cumulative probabilities"),
                                    value = "0.25, 0.5, 0.75")
                   )
                 ),
                 fluidRow(
                   column(4,
                          selectInput("HRDist", label = h5("Distribution"),
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
                            condition = "input.HRDist == 't' || input.HRDist == 'logt' || input.HRDist == 'mirrorlogt'",
                            numericInput("tdf2", label = h5("degrees of freedom"),
                                         value = 3)

                          )
                   )

                 ),

                 plotOutput("HRPlot"),
        ),

        # Feedback UI ---------------------------------
        tabPanel("Feedback",
                 sidebarLayout(
                   sidebarPanel = sidebarPanel(
                     checkboxGroupInput("showfeedback", "Add to plot",
                                        choices = c("Median survival line" = "median_line",
                                                    "95% CI for T" = "ci_t",
                                                    "CI for Treatment Curve (0.025 and 0.975)" = "ci_treatment_curve")),                     hidden(numericInput("feedbackQuantile", "Uncertainty about the following survival quantile:", value = 0.5, min = 0, max = 1)),
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
                     selectInput("rec_method", "Recruitment method", choices = c("Power"="power", "Piecewise constant"="PWC"), selected = "power"),

                     splitLayout(
                       numericInput("rec_power", "Power", value=1, min=1),
                       numericInput("rec_period", "Period", value=12, min=1)

                     ),

                     splitLayout(
                       hidden(textInput("rec_rate", "Rate", value="30, 50")),
                       hidden(textInput("rec_duration", "Duration", value="10, 5"))

                     ),


                     splitLayout(
                       numericInput("n1", "Ratio control", value=1, min=1),
                       numericInput("n2", "Ratio treatment", value=1, min=1)

                     ),
                     numericInput("chosenLength", "Maximum trial duration (including recruitment time)", value=60),
                     selectInput("analysisType", label = "Analysis method", choices = c("Logrank test" = "LRT", "Fleming-Harrington test" = "FHT"), selected = "LRT"),
                     splitLayout(
                       hidden(numericInput("t_star", ' \\( t^* \\)', value=3, min=0)),
                       hidden(numericInput("s_star", ' \\( \\hat{S}(t^*) \\)', value=NA, min=0, max = 1))

                     ),
                     splitLayout(
                       hidden(numericInput("rho", ' \\( \\rho \\)', value=0, min=0, max = 1)),
                       hidden(numericInput("gamma", " \\( \\gamma \\)", value=0, min=0, max = 1))

                     ),
                     actionButton("calcAssurance", "Calculate Assurance")
                   ),
                   mainPanel = mainPanel(
                     fluidRow(
                       column(6,
                              plotOutput("pdfRec")
                       ),
                       column(6,
                              plotOutput("cdfRec")
                       )
                     ),


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
                 HTML("<p>This app implements the method as outlined in this <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.10136'>paper</a>. For every tab, there is a brief summary below, for any other questions, comments or
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


      ), style='width: 1200px; height: 600px',
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
          column(3, downloadButton("downloadObjects", "Download objects")
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

    output$ExpDistText <- renderUI({
      paste0("S(", input$ExpSurvTime, ") ~ Beta(")
    })

    output$WeibullDistS1Text <- renderUI({
      paste0("S(", input$WeibullDistT1, ") ~ Beta(")
    })

    output$WeibullDistDelta1Text <- renderUI({
      paste0("S(", input$WeibullDistT2, ") - S(", input$WeibullDistT1,") ~ Beta(")
    })


    # Reactive for common calculations based on distribution type and inputs
    controlSurvivalData <- reactive({
      # Initialize an empty list to hold output data
      result <- list()

      # Calculate control survival data based on distribution type and input options
      if (input$ControlDist == "Exponential") {
        if (input$ExpChoice == "Single Value") {
          finalSurvTime <- -log(0.01) / input$ExpRate
          controlTime <- seq(0, finalSurvTime, length.out = 100)
          controlSurv <- exp(-input$ExpRate * controlTime)

          result$controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
          result$type <- "single"

        } else if (input$ExpChoice == "Distribution") {
          nSamples <- 1000
          sampledLambda <- -log(rbeta(nSamples, input$ExpBetaA, input$ExpBetaB)) / input$ExpSurvTime
          finalSurvTime <- -log(0.05) / min(sampledLambda)
          controlTime <- seq(0, finalSurvTime, length.out = 100)

          # Vectorized calculation for survival curves
          survivalMatrix <- exp(-outer(sampledLambda, controlTime, "*"))
          medVec <- apply(survivalMatrix, 2, median)
          UBVec <- apply(survivalMatrix, 2, quantile, 0.975)
          LBVec <- apply(survivalMatrix, 2, quantile, 0.025)

          result$controlDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)
          result$type <- "distribution"
        }

      } else if (input$ControlDist == "Weibull") {
        if (input$WeibullChoice == "Single Value") {
          finalSurvTime <- (1 / input$WeibullScale) * (-log(0.01))^(1 / input$WeibullShape)
          controlTime <- seq(0, finalSurvTime, length.out = 100)
          controlSurv <- exp(-(input$WeibullScale * controlTime)^input$WeibullShape)

          result$controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
          result$type <- "single"

        } else if (input$WeibullChoice == "Distribution") {
          n <- 500
          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = n, ncol = length(controlTime))

          for (i in 1:n) {
            sampledS1to <- rbeta(1, input$WeibullDistS1BetaA, input$WeibullDistS1BetaB)
            sampledDelta1 <- rbeta(1, input$WeibullDistDelta1BetaA, input$WeibullDistDelta1BetaB)
            sampledS1toPrime <- sampledS1to - sampledDelta1

            # Solve for lambda and gamma using sampled values
            solution <- nleqslv(c(10, 1), function(params) {
              lambda <- params[1]
              k <- params[2]
              c(exp(-(input$WeibullDistT1 / lambda)^k) - sampledS1to,
                exp(-(input$WeibullDistT2 / lambda)^k) - sampledS1toPrime)
            })

            lambda <- 1 / solution$x[1]
            gamma <- solution$x[2]
            survival_curves[i, ] <- exp(-(lambda * controlTime)^gamma)
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          result$controlDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)
          result$type <- "distribution"
        }
      }

      return(result)
    })

    # Reactive for plotting
    controlPlot <- reactive({
      data <- controlSurvivalData()$controlDF
      plotType <- controlSurvivalData()$type

      if (plotType == "single") {
        ggplot(data, aes(x = controlTime, y = controlSurv)) +
          geom_line(colour = "blue") +
          xlim(0, max(data$controlTime)) +
          ylim(0, 1) +
          xlab("Time") + ylab("Survival")

      } else if (plotType == "distribution") {
        ggplot(data, aes(x = controlTime)) +
          geom_line(aes(y = medVec), colour = "blue") +
          geom_line(aes(y = UBVec), colour = "blue", linetype = "dashed") +
          geom_line(aes(y = LBVec), colour = "blue", linetype = "dashed") +
          xlim(0, max(data$controlTime)) + ylim(0, 1) +
          xlab("Time") + ylab("Survival")
      }
    })

    # Render plot in the UI
    output$plotControl <- renderPlot({
      controlPlot()
    })

    # Functions for the eliciting distributions tabs ---------------------------------

    TLimits <- reactive({
      eval(parse(text = paste("c(", input$TLimits, ")")))
    })

    HRLimits <- reactive({
      eval(parse(text = paste("c(", input$HRLimits, ")")))
    })

    TProbs <- reactive({
      eval(parse(text = paste("c(", input$TProbs, ")")))
    })

    HRProbs <- reactive({
      eval(parse(text = paste("c(", input$HRProbs, ")")))
    })

    TValues <- reactive({
      eval(parse(text = paste("c(", input$TValues, ")")))
    })

    HRValues <- reactive({
      eval(parse(text = paste("c(", input$HRValues, ")")))
    })

    TMedian <- reactive({
      approx(TProbs(), TValues(), 0.5)$y
    })

    HRMedian <- reactive({
      approx(HRProbs(), HRValues(), 0.5)$y
    })

    TFit <- reactive({
      fitdist(vals = TValues(), probs = TProbs(), lower = TLimits()[1],
              upper = TLimits()[2],
              tdf = input$tdf1)
    })

    HRFit <- reactive({
      fitdist(vals = HRValues(), probs = HRProbs(), lower = HRLimits()[1],
              upper = HRLimits()[2],
              tdf = input$tdf2)
    })


    # Functions for the eliciting length of delay tab ---------------------------------

    output$TPlot <- renderPlot({


      suppressWarnings(plotfit(TFit(), d = input$TDist,
                               ql = 0.05, qu = 0.95,
                               xl = TLimits()[1], xu = TLimits()[2],
                               fs = input$fs))

    })

    # Functions for the post-delay HR tab ---------------------------------

    output$HRPlot <- renderPlot({


      suppressWarnings(plotfit(HRFit(), d = input$HRDist,
                               ql = 0.05, qu = 0.95,
                               xl = HRLimits()[1], xu = HRLimits()[2],
                               fs = input$fs))

    })



    # Functions for the Feedback tab ---------------------------------

    # Reactive for common calculations based on distribution type and inputs
    treatmentSurvivalData <- reactive({
      # Initialize an empty list to hold output data
      result <- list()

      # Calculate control survival data based on distribution type and input options
      if (input$ControlDist == "Exponential") {
        if (input$ExpChoice == "Single Value") {
          sampledTrtHazard <- rep(NA, 100)
          sampledbigT <- rep(NA, 100)
          for (i in 1:100){
            if (runif(1) > input$P_S){
              #Curves do not separate
              sampledbigT[i] <- 0
              sampledTrtHazard[i] <- input$ExpRate
            } else {
              if (runif(1) > input$P_DTE){
                #Curves separate with no delay
                HRSample <- sampleFit(HRFit(), n = 1)
                sampledTrtHazard[i] <- input$ExpRate*HRSample[,input$HRDist]
                sampledbigT[i] <- 0
              } else{
                #Curves separate with a delay
                HRSample <- sampleFit(HRFit(), n = 1)
                bigTSample <- sampleFit(TFit(), n = 1)
                sampledbigT[i] <- bigTSample[,input$TDist]
                sampledTrtHazard[i] <- input$ExpRate*HRSample[,input$HRDist]
              }
            }
          }

          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = 100, ncol = length(controlTime))
          for (i in 1:100){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-input$ExpRate*controlTime), exp(-input$ExpRate*sampledbigT[i]-sampledTrtHazard[i]*(controlTime-sampledbigT[i])))
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          #myX <<- sampledbigT

          lowerT <- quantile(sampledbigT, 0.025)
          upperT <- quantile(sampledbigT, 0.975)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec,
                                           UBVec = UBVec, LBVec = LBVec)
          result$lowerT <- lowerT
          result$upperT <- upperT

        } else if (input$ExpChoice == "Distribution") {

          nSamples <- 1000
          sampledLambda <- -log(rbeta(nSamples, input$ExpBetaA, input$ExpBetaB)) / input$ExpSurvTime
          sampledTrtHazard <- rep(NA, nSamples)
          sampledbigT <- rep(NA, nSamples)
          for (i in 1:nSamples){


            if (runif(1) > input$P_S){
              #Curves do not separate
              sampledbigT[i] <- 0
              sampledTrtHazard[i] <- sampledLambda[i]
            } else {
              if (runif(1) > input$P_DTE){
                #Curves separate with no delay
                HRSample <- sampleFit(HRFit(), n = 1)
                sampledTrtHazard[i] <- sampledLambda[i]*HRSample[,input$HRDist]
                sampledbigT[i] <- 0
              } else{
                #Curves separate with a delay
                HRSample <- sampleFit(HRFit(), n = 1)
                bigTSample <- sampleFit(TFit(), n = 1)
                sampledbigT[i] <- bigTSample[,input$TDist]
                sampledTrtHazard[i] <- sampledLambda[i]*HRSample[,input$HRDist]
              }
            }
          }

          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = nSamples, ncol = length(controlTime))
          for (i in 1:nSamples){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-sampledLambda[i]*controlTime), exp(-sampledLambda[i]*sampledbigT[i]-sampledTrtHazard[i]*(controlTime-sampledbigT[i])))
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)

        }

      } else if (input$ControlDist == "Weibull") {
        if (input$WeibullChoice == "Single Value") {

          sampledTrtHazard <- rep(NA, 100)
          sampledbigT <- rep(NA, 100)
          for (i in 1:100){
            if (runif(1) > input$P_S){
              #Curves do not separate
              sampledbigT[i] <- 0
              sampledTrtHazard[i] <- input$WeibullScale
            } else {
              if (runif(1) > input$P_DTE){
                #Curves separate with no delay
                HRSample <- sampleFit(HRFit(), n = 1)
                sampledTrtHazard[i] <- input$WeibullScale*(HRSample[,input$HRDist])^(1/input$WeibullShape)
                sampledbigT[i] <- 0
              } else{
                #Curves separate with a delay
                HRSample <- sampleFit(HRFit(), n = 1)
                bigTSample <- sampleFit(TFit(), n = 1)
                sampledbigT[i] <- bigTSample[,input$TDist]
                sampledTrtHazard[i] <- input$WeibullScale*(HRSample[,input$HRDist])^(1/input$WeibullShape)
              }
            }
          }

          #print(sampledTrtHazard)
          #print(sampledbigT)
          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = 100, ncol = length(controlTime))
          for (i in 1:100){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-(input$WeibullScale*controlTime)^input$WeibullShape),
                                          exp(-(input$WeibullScale*sampledbigT[i])^input$WeibullShape-(sampledTrtHazard[i])^input$WeibullShape*(controlTime^input$WeibullShape-(sampledbigT[i])^input$WeibullShape)))
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)


        } else if (input$WeibullChoice == "Distribution") {

          nSamples <- 1000
          lambdaVec <- rep(NA, nSamples)
          gammaVec <- rep(NA, nSamples)


          for (i in 1:nSamples) {
            sampledS1to <- rbeta(1, input$WeibullDistS1BetaA, input$WeibullDistS1BetaB)
            sampledDelta1 <- rbeta(1, input$WeibullDistDelta1BetaA, input$WeibullDistDelta1BetaB)
            sampledS1toPrime <- sampledS1to - sampledDelta1

            # Solve for lambda and gamma using sampled values
            solution <- nleqslv(c(10, 1), function(params) {
              lambda <- params[1]
              k <- params[2]
              c(exp(-(input$WeibullDistT1 / lambda)^k) - sampledS1to,
                exp(-(input$WeibullDistT2 / lambda)^k) - sampledS1toPrime)
            })

            lambdaVec[i] <- 1 / solution$x[1]
            gammaVec[i] <- solution$x[2]
          }



          sampledTrtHazard <- rep(NA, nSamples)
          sampledbigT <- rep(NA, nSamples)
          for (i in 1:nSamples){


            if (runif(1) > input$P_S){
              #Curves do not separate
              sampledbigT[i] <- 0
              sampledTrtHazard[i] <- lambdaVec[i]
            } else {
              if (runif(1) > input$P_DTE){
                #Curves separate with no delay
                HRSample <- sampleFit(HRFit(), n = 1)
                sampledTrtHazard[i] <- lambdaVec[i]*(HRSample[,input$HRDist])^(1/gammaVec[i])
                sampledbigT[i] <- 0
              } else{
                #Curves separate with a delay
                HRSample <- sampleFit(HRFit(), n = 1)
                bigTSample <- sampleFit(TFit(), n = 1)
                sampledbigT[i] <- bigTSample[,input$TDist]
                sampledTrtHazard[i] <- lambdaVec[i]*(HRSample[,input$HRDist])^(1/gammaVec[i])
              }
            }
          }

          print(sampledTrtHazard)
          print(sampledbigT)
          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = nSamples, ncol = length(controlTime))
          for (i in 1:100){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-(lambdaVec[i]*controlTime)^gammaVec[i]),
                                          exp(-(lambdaVec[i]*sampledbigT[i])^gammaVec[i]-(sampledTrtHazard[i])^gammaVec[i]*(controlTime^gammaVec[i]-(sampledbigT[i])^gammaVec[i])))
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)

        }
      }

      return(result)
    })



    # output$priorWorthFeedback <- renderUI({
    #
    #   addfeedback <- radiobuttons()
    #
    #   str1 <- ""
    #
    #   if (!is.null(addfeedback)){
    #     for (i in 1:length(addfeedback)){
    #       if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
    #         simlineslower <- data.frame(x = treatmentCILines()$TreatmentTime, y = treatmentCILines()$lowerbound)
    #         simlinesupper <- data.frame(x = treatmentCILines()$TreatmentTime, y = treatmentCILines()$upperbound)
    #
    #
    #         CIwidth <- simlinesupper[(which.min(abs(simlinesupper$x-input$timeInputFeedback))),]$y - simlineslower[(which.min(abs(simlineslower$x-input$timeInputFeedback))),]$y
    #         midpoint <- (simlinesupper[(which.min(abs(simlinesupper$x-input$timeInputFeedback))),]$y + simlineslower[(which.min(abs(simlineslower$x-input$timeInputFeedback))),]$y)/2
    #         n <- (16*midpoint*(1-midpoint))/(CIwidth^2)
    #         str1 <- paste0("The confidence interval width at t = ", input$timeInputFeedback, " is equivalent to  ", round(n, 0),
    #                        " patients from a Binomial distribution")
    #       }
    #     }
    #   }
    #
    #   HTML(paste(str1, sep = '<br/>'))
    #
    # })

    # output$medianSurvivalFeedback <- renderUI({
    #
    #   addfeedback <- radiobuttons()
    #
    #   str1 <- ""
    #
    #   if (!is.null(addfeedback)){
    #     for (i in 1:length(addfeedback)){
    #       if (addfeedback[i]=="Median survival line"){
    #         medianTTime <- round(treatmentCILines()$TreatmentTime[sum(treatmentCILines()$medianTreatment>0.5)], 1)
    #         medianCTime <- round((1/input$lambdacmean)*(-log(0.5))^(1/input$gammacmean), 1)
    #         str1 <- paste0("The median survival time on the control is ", medianCTime, " and the median survival time on the treatment is ", medianTTime)
    #       }
    #     }
    #   }
    #
    #   HTML(paste(str1, sep = '<br/>'))
    #
    # })


      # Observe WeibullChoice and update checkboxGroupInput choices
    observe({
      # Define the common choices
      common_choices <- c("Median survival line" = "median_line",
                          "95% CI for T" = "ci_t",
                          "CI for Treatment Curve (0.025 and 0.975)" = "ci_treatment_curve")

      # Initialize the final choices
      final_choices <- common_choices

      # Check ControlDist and respective choices
      if (input$ControlDist == "Exponential" || input$ControlDist == "Weibull") {
        if (input$ControlDist == "Exponential" && input$ExpChoice == "Distribution") {
          # If Exponential and Distribution, add CI for Control
          final_choices["95% CI for Control"] <- "ci_control"
        } else if (input$ControlDist == "Weibull" && input$WeibullChoice == "Distribution") {
          # If Weibull and Distribution, add CI for Control
          final_choices["95% CI for Control"] <- "ci_control"
        }
      }

      # Update the checkbox group input with the final choices
      updateCheckboxGroupInput(session, "showfeedback", choices = final_choices)
    })


    feedbackPlot <- reactive({
      controlData <- controlSurvivalData()$controlDF
      treatmentData <- treatmentSurvivalData()$treatmentDF
      plotType <- controlSurvivalData()$type

      if (plotType == "single") {
       p1 <- ggplot(controlData, aes(x = controlTime, y = controlSurv)) +
          geom_line(colour = "blue") +
          xlim(0, max(controlData$controlTime)) +
          ylim(0, 1) +
          xlab("Time") + ylab("Survival")

       if ("median_line" %in% input$showfeedback){
         median_time_control <- approx(controlData$controlSurv, controlData$controlTime, xout = 0.5)$y
         median_time_treatment <- approx(treatmentData$medVec, treatmentData$controlTime, xout = 0.5)$y
         mediandf <- data.frame(x = seq(0, max(median_time_control, median_time_treatment), length=2), y = rep(0.5, 2))
         mediandf1 <- data.frame(x = rep(median_time_control, 2), y = seq(0, 0.5, length=2))
         mediandf2 <- data.frame(x = rep(median_time_treatment, 2), y = seq(0, 0.5, length=2))
         p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") +
           geom_line(data = mediandf1, aes(x = x, y=y), linetype = "dashed") +
           geom_line(data = mediandf2, aes(x = x, y=y), linetype = "dashed")
       }

      if ("ci_t" %in% input$showfeedback){
        lowerSurv <- approx(treatmentData$controlTime, treatmentData$medVec, xout = treatmentSurvivalData()$lowerT)$y
        upperSurv <- approx(treatmentData$controlTime, treatmentData$medVec, xout = treatmentSurvivalData()$upperT)$y
        p1 <- p1 + annotate("point", x = treatmentSurvivalData()$lowerT, y = lowerSurv) +
          annotate("point", x = treatmentSurvivalData()$upperT, y = upperSurv)
        #cat("this is", treatmentSurvivalData()$upperT)
      }

      } else if (plotType == "distribution") {
        p1 <- ggplot(controlData, aes(x = controlTime)) +
          geom_line(aes(y = medVec), colour = "blue")  +
          xlim(0, max(controlData$controlTime)) + ylim(0, 1) +
          xlab("Time") + ylab("Survival")

        if ("ci_control" %in% input$showfeedback)
          p1 <- p1 + geom_line(aes(y = UBVec), colour = "blue", linetype = "dashed") +
            geom_line(aes(y = LBVec), colour = "blue", linetype = "dashed")


        if ("median_line" %in% input$showfeedback){
          median_time_control <- approx(controlData$medVec, controlData$controlTime, xout = 0.5)$y
          median_time_treatment <- approx(treatmentData$medVec, treatmentData$controlTime, xout = 0.5)$y
          mediandf <- data.frame(x = seq(0, max(median_time_control, median_time_treatment), length=2), y = rep(0.5, 2))
          mediandf1 <- data.frame(x = rep(median_time_control, 2), y = seq(0, 0.5, length=2))
          mediandf2 <- data.frame(x = rep(median_time_treatment, 2), y = seq(0, 0.5, length=2))
          p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") +
            geom_line(data = mediandf1, aes(x = x, y=y), linetype = "dashed") +
            geom_line(data = mediandf2, aes(x = x, y=y), linetype = "dashed")
        }

      }

      p1 <- p1 + geom_line(data = treatmentData, aes(x=controlTime, y = medVec), colour = "red")

      if ("ci_treatment_curve" %in% input$showfeedback){
        p1 <- p1 + geom_line(data = treatmentData, aes(x=controlTime, y = UBVec), colour = "red", linetype = "dashed") +
          geom_line(data = treatmentData, aes(x=controlTime, y = LBVec), colour = "red", linetype = "dashed")
      }

      p1

    })

    output$plotFeedback <- renderPlot({
      feedbackPlot()

  })

    # radiobuttons <- reactive({
    #   addfeedback <- input$showfeedback
    # })



    # output$quantilePlot <- renderPlot({
    #
    #   addfeedback <- radiobuttons()
    #
    #
    #   if (!is.null(addfeedback)){
    #     for (i in 1:length(addfeedback)){
    #       if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
    #
    #         quantileMatrix <- treatmentCILines()$SimMatrix
    #
    #         quantileTime <- treatmentCILines()$TreatmentTime
    #
    #         quantileVec <- rep(NA, length = nrow(quantileMatrix))
    #
    #         for (j in 1:nrow(quantileMatrix)){
    #           quantileVec[j] <- quantileTime[which.min(abs(quantileMatrix[j,]-input$feedbackQuantile))]
    #         }
    #
    #         quantiledf <- data.frame(quantiletime = quantileVec)
    #
    #         theme_set(theme_grey(base_size = 12))
    #         p1 <- ggplot(data=quantiledf, aes(x=quantiletime)) + geom_histogram(aes(y = after_stat(density)), binwidth = 5) + xlim(0, exp((1.527/input$gammacmean)-log(input$lambdacmean))*1.1) +
    #           xlab("Time")
    #
    #         print(p1)
    #
    #       }
    #     }
    #   }
    #
    #
    # })

    # output$quantileFeedback <- renderUI({
    #
    #   addfeedback <- radiobuttons()
    #
    #   str1 <- ""
    #
    #   if (!is.null(addfeedback)){
    #     for (i in 1:length(addfeedback)){
    #       if (addfeedback[i]=="CI for Treatment Curve (0.1 and 0.9)"){
    #         str1 <- paste0("This plot shows the distribution of samples for treatment group for the ", input$feedbackQuantile, " quantile")
    #       }
    #     }
    #   }
    #
    #   HTML(paste(str1, sep = '<br/>'))
    #
    # })

    # Functions for the Assurance tab ---------------------------------

    observe({
      if (input$rec_method=="power"){
        shinyjs::show("rec_power")
        shinyjs::show("rec_period")
      } else{
        shinyjs::hide("rec_power")
        shinyjs::hide("rec_period")
      }
    })

    observe({
      if (input$rec_method=="PWC"){
        shinyjs::show("rec_rate")
        shinyjs::show("rec_duration")
      } else{
        shinyjs::hide("rec_rate")
        shinyjs::hide("rec_duration")
      }
    })

    observe({
      if (input$analysisType=="FHT"){
        shinyjs::show("rho")
        shinyjs::show("gamma")
      } else{
        shinyjs::hide("rho")
        shinyjs::hide("gamma")
      }
    })

    output$pdfRec <- renderPlot({

      if (input$rec_method=="power"){

        # Calculate the correct PDF values
        x_values <- seq(0, input$rec_period, length.out = 1000)
        pdf_values <- (input$rec_power / input$rec_period) * (x_values/input$rec_period)^(input$rec_power - 1)

        # Overlay correct PDF on the histogram
        plot(x_values, pdf_values, col = "red", type = "l", xlab = "Recruitment time", ylab = "Density", main = "Probability Density Function")
      } else if (input$rec_method == "PWC"){

        rec_rate <- as.numeric(unlist(strsplit(input$rec_rate,",")))
        rec_duration <- as.numeric(unlist(strsplit(input$rec_duration,",")))

        n <- length(rec_rate)

        # Define a function that returns the residuals
        equations <- function(vars) {
          x <- vars[1:n]
          eq1 <- sum(x * rec_duration) - 1
          eq_rest <- sapply(2:n, function(i) x[1] / x[i] - rec_rate[1] / rec_rate[i])
          return(c(eq1, eq_rest))
        }

        # Initial guess
        initial_guess <- rep(0.1, n)

        # Solve the nonlinear system of equations
        solution <- nleqslv(initial_guess, equations)

        plot(c(0, rec_duration[1]), c(solution$x[1], solution$x[1]), type= "l", col = "red",
             xlim = c(0, sum(rec_duration)), ylim = c(0, max(solution$x)), xlab = "Recruitment time", ylab = "Density",
             main = "Probability Density Function")

        for (i in 1:(n-1)){
          graphics::lines(c(sum(rec_duration[1:i]), sum(rec_duration[1:i])), c(solution$x[i], solution$x[i+1]), col = "red")
          graphics::lines(c(sum(rec_duration[1:i]), sum(rec_duration[1:(i+1)])), c(solution$x[i+1], solution$x[i+1]), col = "red")
        }

      }
    })

    output$cdfRec <- renderPlot({

      if (input$rec_method=="power"){

        # Calculate the correct CDF values
        x_values <- seq(0, input$rec_period, length.out = 1000)
        cdf_values <- (x_values/input$rec_period)^(input$rec_power)*input$numofpatients


        plot(x_values, cdf_values, col = "red", type = "l", xlab = "Recruitment time", ylab = "Number of patients",  main = "Cumulative Density Function")

      } else if (input$rec_method == "PWC"){

        rec_rate <- as.numeric(unlist(strsplit(input$rec_rate,",")))
        rec_duration <- as.numeric(unlist(strsplit(input$rec_duration,",")))

        # Calculate cumulative resource allocation over time
        cumulative_allocation <- cumsum(rec_rate * rec_duration)

        # Create x-axis and y-axis data for step function
        xaxis <- c(0, cumsum(rec_duration))
        yaxis <- c(0, cumulative_allocation)

        # Plotting
        plot(xaxis, yaxis, type = "l", xlab = "Recruitment time", ylab = "Number of patients", col = "red",
             main = "Cumulative Density Function")
      }
    })


    #This function calculates the normal assurance given the elicited distributions and other simple questions about the trial
    calculateAssurance <- eventReactive(input$calcAssurance, {


      assFunc <- function(n1, n2){


        #Simulate 400 observations for T and HR given the elicited distributions
        #For each n1, n2, simulate 400 trials
        assnum <- 500
        assvec <- rep(NA, assnum)
        AHRvec <- rep(NA, assnum)
        LBAHRvec <- rep(NA, assnum)
        UBAHRvec <- rep(NA, assnum)
        eventsvec <- rep(NA, assnum)

        mySample <- elicitedSamples()$mySample


        for (i in 1:assnum){


          if (v$upload=="no"){
            lambdac <- input$lambdacmean
            gammac <- input$gammacmean
          } else {
            lambdac <- sample(as.numeric(inputData()$scale), size = 1)
            gammac <- sample(as.numeric(inputData()$shape), size = 1)
          }

          gammat <- gammac

          bigT <- sample(mySample[,1], 1)
          HR <- sample(mySample[,2], 1)

          lambdat <- lambdac*HR^(1/gammac)

          dataCombined <- SimDTEDataSet(n_C = n1, n_E = n2, lambda_C = lambdac, HRStar = HR, gamma_C = gammac, gamma_E = gammat, delayT = bigT,
                                        rec_method = input$rec_method, rec_period = input$rec_period, rec_power = input$rec_power, rec_rate = input$rec_rate, rec_duration = input$rec_duration)

          dataCombined <- CensFunc(dataCombined = dataCombined, censTime = input$chosenLength)$dataCombined

          coxmodel <- coxph(Surv(survival_time, status)~group, data = dataCombined)

          AHRvec[i] <- as.numeric(exp(coef(coxmodel)))

          CI <- exp(confint(coxmodel))

          LBAHRvec[i] <- CI[1]

          UBAHRvec[i] <- CI[2]

          #Performs a log rank test on the data
          test <- survdiff(Surv(survival_time, status)~group, data = dataCombined)
          #If the p-value of the test is less than 0.05 then assvec = 1, 0 otherwise
          assvec[i] <- test$chisq > qchisq(0.95, 1)

          #Counts how many events have been seen up until the total trial length time
          eventsvec[i] <-  sum(dataCombined$time<input$chosenLength)

        }

        AHRvec[is.infinite(AHRvec)]<-NA
        LBAHRvec[is.infinite(LBAHRvec)]<-NA
        UBAHRvec[is.infinite(UBAHRvec)]<-NA


        return(list(assvec = mean(assvec), LBAHRvec = mean(LBAHRvec, na.rm=T), UBAHRvec = mean(UBAHRvec, na.rm = T),
                    AHRvec = mean(AHRvec, na.rm=T), eventvec = mean(eventsvec), assnum=assnum))
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

      AHRvec <- unlist(calcassvec[4,])

      eventvec <- unlist(calcassvec[5,])

      assnumvec <- unlist(calcassvec[6,])

      LBassvec <- assvec-1.96*sqrt(assvec*(1-assvec)/assnumvec)

      UBassvec <- assvec+1.96*sqrt(assvec*(1-assvec)/assnumvec)


      #How many events are seen given this set up
      eventsseen <- eventvec[length(eventvec)]

      #Smooth the assurance, compared to the the sample size vector
      asssmooth <- loess(assvec~samplesizevec)

      AHRsmooth <- loess(AHRvec~samplesizevec)

      LBsmooth <- loess(LBAHRvec~samplesizevec)

      UBsmooth <- loess(UBAHRvec~samplesizevec)

      LBasssmooth <- loess(LBassvec~samplesizevec)

      UBasssmooth <- loess(UBassvec~samplesizevec)


      return(list(calcassvec = calcassvec, asssmooth = asssmooth, samplesizevec = samplesizevec,
                  eventsseen = eventsseen, AHRsmooth = AHRsmooth, LBsmooth = LBsmooth, UBsmooth = UBsmooth,
                  LBasssmooth = LBasssmooth, UBasssmooth = UBasssmooth))

    })


    output$assurancePlot <- renderPlot({

      #Plot the assurance calculated in the function
      theme_set(theme_grey(base_size = 12))
      assurancenormaldf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$asssmooth))
      assurancenormalLBdf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$LBasssmooth))
      assurancenormalUBdf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$UBasssmooth))
      p1 <- ggplot() + geom_line(data = assurancenormaldf, aes(x = x, y = y, colour="Assurance"), linetype="solid") + xlab("Total number of patients") +
        ylab("Assurance") + ylim(0, 1.05) +
        geom_line(data = assurancenormalLBdf, aes(x=x, y=y, colour = 'Assurance'), linetype='dashed') +
        geom_line(data = assurancenormalUBdf, aes(x=x, y=y, colour = 'Assurance'), linetype='dashed') +
        theme(
          legend.position = c(.05, .95),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.margin = margin(6, 6, 6, 6)) + scale_color_manual(name=NULL,
                                                                   breaks=c('Assurance'),
                                                                   values=c('Assurance'='blue'))
      print(p1)
    })


    output$AHRPlot <- renderPlot({

      AHRdf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$AHRsmooth))
      LBdf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$LBsmooth))
      UBdf <- data.frame(x = calculateAssurance()$samplesizevec, y = predict(calculateAssurance()$UBsmooth))

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
      str2 <- paste0("On average, ", round(calculateAssurance()$eventsseen), " events are seen when ", input$numofpatients, " patients are enroled for ", input$chosenLength, " months.")
      HTML(paste(str1, str2, sep = '<br/>'))
    })

    output$AHRFeedback  <- renderUI({
      #Show how many events are seen given the set up
      x <-  round(calculateAssurance()$eventsseen)
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
        params <- list(fit1 = myfit1(), fit2 = myfit2(),
                       d = c(input$dist1, input$dist2),
                       P_S = input$P_S, P_DTE = input$P_DTE)

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
    output$downloadObjects <- downloadHandler(
      filename = function() {
        "DTEAssurance.rds"
      },
      content = function(file) {
        object_list <- list(fit1 = myfit1(), fit2 = myfit2(),
                            d = c(input$dist1, input$dist2),
                            P_S = input$P_S, P_DTE = input$P_DTE)
        saveRDS(object_list, file)
      }
    )

  }

  shiny::shinyApp(ui, server)





