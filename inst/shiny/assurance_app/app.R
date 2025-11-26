library(SHELF)
library(ggplot2)
library(survival)
library(shinyjs)
library(nleqslv)
library(shinyBS)
#remotes::install_github("jamesalsbury/DTEAssurance")
library(DT)
library(rlang)
library(shinyAce)



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
                       selectInput("ExpChoice", "Choice", choices = c("Fixed", "Distribution"), selected = "Fixed"),
                       conditionalPanel(
                         condition = "input.ExpChoice == 'Fixed'",
                         selectInput("ExpRateorTime", "Input type", choices = c("Parameter", "Landmark")),
                         conditionalPanel(
                           condition = "input.ExpRateorTime == 'Parameter'",
                           numericInput("ExpRate", label =  HTML(paste0("Rate (\u03bb",tags$sub("c"), ")")), value = 0.08, min=0),
                           bsTooltip(id = "ExpRate", title = "Rate parameter")
                         ),
                         conditionalPanel(
                           condition = "input.ExpRateorTime == 'Landmark'",
                           fluidRow(
                             column(6,
                                    numericInput("ExpTime", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                    bsTooltip(id = "ExpTime", title = "Time 1")

                             ),
                             column(6,
                                    numericInput("ExpSurv", label =  HTML(paste0("S(t",tags$sub("1"), ")")), 0.5),
                                    bsTooltip(id = "ExpSurv", title = "Survival probability at time 1")
                             )
                           )
                         )
                       ),
                       conditionalPanel(
                         condition = "input.ExpChoice == 'Distribution'",
                         numericInput("ExpSurvTime", label = HTML(paste0("t",tags$sub("1"))), value = 12),
                         bsTooltip(id = "ExpSurvTime", title = "Time 1"),
                         fluidRow(
                           column(4,
                                  uiOutput("ExpDistText"),
                                  bsTooltip(id = "ExpDistText", title = "The distribution of survival probability at time 1")
                           ),
                           column(8,
                                  div(style = "display: flex; align-items: center;",
                                      numericInput("ExpBetaA", label = NULL, value = 20, width = "45%"),
                                      bsTooltip(id = "ExpBetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                      tags$span(", "),
                                      numericInput("ExpBetaB", label = NULL, value = 32, width = "45%"),
                                      bsTooltip(id = "ExpBetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
                                      tags$span(")")
                                  )
                           )
                         ),
                       ),





                     ),

                     # Conditional UI for Weibull distribution
                     conditionalPanel(
                       condition = "input.ControlDist == 'Weibull'",
                       selectInput("WeibullChoice", "Choice", choices = c("Fixed", "Distribution"), selected = "Fixed"),
                       conditionalPanel(
                         condition = "input.WeibullChoice == 'Fixed'",
                         selectInput("WeibRateorTime", "Input type", choices = c("Parameters", "Landmark")),
                         conditionalPanel(
                           condition = "input.WeibRateorTime == 'Parameters'",
                           fluidRow(
                             column(6,
                                    numericInput("WeibullScale", label =  HTML(paste0("Scale (\u03bb",tags$sub("c"), ")")), value = 0.08, min=0),
                                    bsTooltip(id = "WeibullScale", title = "Scale parameter"),

                             ),
                             column(6,
                                    numericInput("WeibullShape", label =  HTML(paste0("Shape (\u03b3",tags$sub("c"), ")")), value = 1, min=0),
                                    bsTooltip(id = "WeibullShape", title = "Shape parameter"),

                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.WeibRateorTime == 'Landmark'",
                           fluidRow(
                             column(6,
                                    numericInput("WeibullTime1", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                    bsTooltip(id = "WeibullTime1", title = "Time 1")
                             ),
                             column(6,
                                    numericInput("WeibullSurv1", label =  HTML(paste0("S(t",tags$sub("1"), ")")), value = 0.38),
                                    bsTooltip(id = "WeibullSurv1", title = "Survival Probability at Time 1")
                             )
                           ),
                           fluidRow(
                             column(6,
                                    numericInput("WeibullTime2", label =  HTML(paste0("t",tags$sub("2"))), value = 18),
                                    bsTooltip(id = "WeibullTime2", title = "Time 2")

                             ),
                             column(6,
                                    numericInput("WeibullSurv2", label =  HTML(paste0("S(t",tags$sub("2"), ")")), value = 0.24),
                                    bsTooltip(id = "WeibullSurv2", title = "Survival Probability at Time 2")
                             )
                           ),
                         )
                       ),

                       conditionalPanel(
                         condition = "input.WeibullChoice == 'Distribution'",
                         fluidRow(
                           column(6,
                                  numericInput("WeibullDistT1", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                  bsTooltip(id = "WeibullDistT1", title = "Time 1"),


                           ),
                           column(6,
                                  numericInput("WeibullDistT2", label =  HTML(paste0("t",tags$sub("2"))), value = 18),
                                  bsTooltip(id = "WeibullDistT2", title = "Time 2"),

                           )
                         ),
                         fluidRow(
                           column(4,
                                  uiOutput("WeibullDistS1Text"),
                                  bsTooltip(id = "WeibullDistS1Text", title = "The distribution of survival probability at time 1"),
                           ),
                           column(8,
                                  div(style = "display: flex; align-items: center;",
                                      numericInput("WeibullDistS1BetaA", label = NULL, value = 20, width = "45%"),
                                      bsTooltip(id = "WeibullDistS1BetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                      tags$span(", "),
                                      numericInput("WeibullDistS1BetaB", label = NULL, value = 32, width = "45%"),
                                      bsTooltip(id = "WeibullDistS1BetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
                                      tags$span(")")
                                  )
                           )
                         ),
                         fluidRow(
                           column(4,
                                  uiOutput("WeibullDistDelta1Text"),
                                  bsTooltip(id = "WeibullDistDelta1Text", title = "The distribution of the difference in survival probabilities between time 2 and time 1"),

                           ),
                           column(8,
                                  div(style = "display: flex; align-items: center;",
                                      numericInput("WeibullDistDelta1BetaA", label = NULL, value = 20, width = "45%"),
                                      bsTooltip(id = "WeibullDistDelta1BetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                      tags$span(", "),
                                      numericInput("WeibullDistDelta1BetaB", label = NULL, value = 140, width = "45%"),
                                      bsTooltip(id = "WeibullDistDelta1BetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
                                      tags$span(")")
                                  )
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
                   sidebarPanel(
                     checkboxGroupInput("showfeedback", "Add to plot",
                                        choices = c("Median survival probability" = "median_probability",
                                                    "95% CI for T" = "ci_t",
                                                    "CI for Treatment Curve (0.025 and 0.975)" = "ci_treatment_curve")),
                     hidden(numericInput("feedbackQuantile", "Uncertainty about the following survival quantile:", value = 0.5, min = 0, max = 1)),
                     hidden(numericInput("timeInputFeedback", "Prior information about time:", value = 25, min = 0, max = 100))
                   ),
                   mainPanel(
                     plotOutput("plotFeedback"),
                     htmlOutput("medianSurvivalFeedback"),
                     htmlOutput("CI_For_T_Feedback"),
                     htmlOutput("priorWorthFeedback"),
                     plotOutput("quantilePlot"),
                     htmlOutput("quantileFeedback")
                   )
                 )
        ),

        # Recruitment UI ---------------------------------

        tabPanel("Recruitment",
                 sidebarLayout(
                   sidebarPanel(
                     shinyjs::useShinyjs(),
                     numericInput("numofpatients", "Maximum number of patients in the trial", value=1000),
                     fluidRow(
                       column(6,
                              numericInput("ControlRatio", "Ratio control", value=1, min=1)
                       ),
                       column(6,
                              numericInput("TreatmentRatio", "Ratio treatment", value=2, min=1)
                       )
                     ),
                     selectInput("rec_method", "Recruitment method", choices = c("Power"="power", "Piecewise constant"="PWC"), selected = "power"),

                     conditionalPanel(
                       condition = "input.rec_method == 'power'",
                       fluidRow(
                         column(6,
                                numericInput("rec_power", "Power", value=1, min=1)
                         ),
                         column(6,
                                numericInput("rec_period", "Period", value=12, min=1)
                         )
                       )
                     ),

                     conditionalPanel(
                       condition = "input.rec_method == 'PWC'",
                       fluidRow(
                         column(6,
                                textInput("rec_rate", "Rate", value="30, 50")
                         ),
                         column(6,
                                textInput("rec_duration", "Duration", value="10, 5")
                         )
                       )
                     )
                   ),
                   mainPanel(
                     plotOutput("cdfRec")
                   )
                 )
        ),


        # Assurance UI ---------------------------------
        tabPanel("Calculating assurance",
                 sidebarLayout(
                   sidebarPanel = sidebarPanel(
                     selectInput("censType", "Type of censoring", choices = c("Time", "Events", "Information Fraction" = "IF"), selected = "Time"),
                     conditionalPanel(
                       condition = "input.censType == 'Time'",
                       numericInput("cens_time", "Censoring time", value = 60)
                     ),
                     conditionalPanel(
                       condition = "input.censType == 'Events'",
                       numericInput("cens_events", "Number of events", value = 200)
                     ),
                     conditionalPanel(
                       condition = "input.censType == 'IF'",
                       numericInput("cens_IF", "Information Fraction", value = 0.8)
                     ),
                     selectInput("analysisType", label = "Analysis method",
                                 choices = c("Logrank test" = "LRT",
                                             "Fleming-Harrington test" = "FHT",
                                             "Modestly-Weighted LRT" = "MW"),
                                 selected = "LRT"),
                     conditionalPanel(
                       condition = "input.analysisType == 'FHT'",
                       fluidRow(
                         column(6,
                                numericInput("rho", ' \\( \\rho \\)', value=0, min=0, max = 1)
                         ),
                         column(6,
                                numericInput("gamma", " \\( \\gamma \\)", value=0, min=0, max = 1)
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.analysisType == 'MW'",
                       fluidRow(
                         column(6,
                                selectInput("MW_Parameter", label = "Parameter",
                                            choices = c("T Star" = "t_star",
                                                        "S Star" = "s_star"),
                                            selected = "t_star")
                         ),
                         column(6,
                                conditionalPanel(
                                  condition = "input.MW_Parameter == 't_star'",
                                  withMathJax(
                                    numericInput("t_star", "\\( t^* \\)", value = 3, min = 0, max = 1)
                                  )
                                  ),
                                conditionalPanel(
                                  condition = "input.MW_Parameter == 's_star'",
                                  withMathJax(
                                    numericInput("s_star", " \\( s^* \\)", value=0.5, min=0, max = 1)
                                  )
                                ),
                                ),
                         )
                       ),
                     radioButtons(
                       inputId = "test_type",
                       label = "Test:",
                       choices = c("One-sided" = "one.sided", "Two-sided" = "two.sided"),
                       selected = "one.sided"
                     ),
                     numericInput("alphaLevel", paste0("Type I error (", "\u03B1", ")"), value = 0.025),
                     fluidRow(
                       column(6,
                              checkboxInput("targetHRTF", "Target HR?", value = FALSE)
                       ),
                       column(6,
                              conditionalPanel(
                                condition = "input.targetHRTF == true",
                                  numericInput("target_HR", "HR < ", value = 0.8, min = 0, max = 1)
                              )
                       )
                     ),


                     numericInput("nSamples", "Number of simulations (per sample size)", value=250),
                     numericInput("nSampleSize", "Number of different sample sizes", value=10),


                     actionButton("calcAssurance", "Calculate Assurance"),
                     hidden(numericInput("feedbackN", "Number of Patients", value=500)),
                     hidden(actionButton("calcAssuranceFeedback", "Calculate Feedback"))

                   ),
                   mainPanel = mainPanel(
                     tags$h4(
                       id = "toggleHeader",
                       style = "cursor: pointer; display: flex; align-items: center; justify-content: space-between;",
                       div(
                         style = "display: flex; align-items: center;",
                         tags$span(id = "arrow", "►"),  # Arrow icon
                         " Show/hide the function"
                       ),
                       actionButton(
                         inputId = "copy_btn",
                         label = "📋 Copy",
                         class = "btn btn-sm btn-outline-primary"
                       )
                     ),

                     tags$div(
                       id = "collapseText",
                       class = "collapse",
                       aceEditor(
                         outputId = "display_func",
                         value = "",
                         mode = "r",
                         theme = "monokai",
                         readOnly = TRUE,
                         height = "400px"
                       )
                     ),

                     tags$script(HTML("
    // Toggle collapse + arrow
    $(document).on('click', '#toggleHeader', function(e) {
      if (!$(e.target).is('#copy_btn')) {   // avoid toggle when clicking copy
        $('#collapseText').collapse('toggle');
        var arrow = $('#arrow');
        if (arrow.text() == '►') {
          arrow.text('▼');
        } else {
          arrow.text('►');
        }
      }
    });

    // Copy-to-clipboard
    $(document).on('click', '#copy_btn', function() {
      var editorText = ace.edit('display_func').getValue();
      navigator.clipboard.writeText(editorText);
    });
  ")),

                     plotOutput("assurancePlot")
                   )


                 ),

        ),

        #Help UI ---------------------------------
        tabPanel("Help",
                 HTML("<p><u>Introduction</u></p>"),
                 HTML("<p>This app implements the method as outlined in the following <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.10136'>paper</a>. For each tab, a brief summary is provided below. For further questions, comments, or feedback, please contact <a href='mailto:jsalsbury1@sheffield.ac.uk'>James Salsbury</a>.</p>"),

                 HTML("<p><u>Control Group Parameterization</u></p>"),
                 HTML("<p>This section specifies the parameters for the control group's survival distribution, which can be modeled using either an Exponential or a Weibull distribution.</p>"),

                 HTML("<p><b>Exponential Distribution:</b></p>"),
                 HTML("<p>The parameterization for the Exponential distribution is:</p>"),
                 withMathJax("$$S_c(t) = \\exp\\{-\\lambda_c t\\}$$"),

                 HTML("<p><b>Weibull Distribution:</b></p>"),
                 HTML("<p>The parameterization for the Weibull distribution is:</p>"),
                 withMathJax("$$S_c(t) = \\exp\\{-(\\lambda_c t)^{\\gamma_c}\\}$$"),

                 HTML("<p>In both cases, users have the option to directly input parameter values or account for uncertainty by specifying probability distributions for survival probabilities.</p>"),

                 HTML("<p><b>Exponential Distribution Parameters:</b></p>"),
                 HTML("<p>For the Exponential distribution, specify a single time point, \\( t_1 \\). You are asked to quantify your uncertainty regarding the survival probability at \\( t_1 \\) using a \\( Beta(a, b) \\) distribution.</p>"),

                 HTML("<p><b>Weibull Distribution Parameters:</b></p>"),
                 HTML("<p>For the Weibull distribution, specify two time points, \\( t_1 \\) and \\( t_2 \\). First, quantify your uncertainty regarding the survival probability at \\( t_1 \\) using a \\( Beta(a, b) \\) distribution.</p>"),
                 HTML("<p>Next, quantify the uncertainty in the difference between survival probabilities at the two time points as:</p>"),
                 withMathJax("$$S(t_1) - S(t_2) \\sim Beta(c, d)$$"),

                 HTML("<p><u>Conditional Probabilities</u></p>"),
                 HTML("<p>In this section, there are two probabilities to specify. The first is the probability that the survival curves will separate, indicating that the treatment will show some benefit over the control. The second is the conditional probability that, given the survival curves separate, the separation occurs with a delay. In other words, there may be a period where the control and treatment curves follow a similar trajectory before diverging.</p>"),

                 HTML("<p><u>Eliciting the Length of Delay and Post-Delay Hazard Ratio</u></p>"),
                 HTML("<p>These two tabs are used to elicit the length of delay (assuming a delay in treatment effect) and the post-delay hazard ratio (assuming the treatment eventually has an effect). The quantile method, as outlined in the <a href='https://shelf.sites.sheffield.ac.uk/'>SHELF</a> framework, is used. You specify limits, values, and cumulative probabilities, and the app identifies the best-fitting distribution based on your inputs.</p>"),

                 HTML("<p><u>Feedback</u></p>"),
                 HTML("<p>This section displays the median survival curve for the control group, along with the median elicited treatment curve (calculated from the previous tabs). Additionally, it includes three optional quantities: (1) the median survival time for both groups, (2) a 95% confidence interval for \\( T \\), and (3) a 95% confidence interval for the treatment curve. If uncertainty was provided for the control curve, a 95% confidence interval for this can also be viewed.</p>"),

                 HTML("<p><u>Recruitment</u></p>"),
                 HTML("<p>This tab allows users to specify the recruitment schedule for the prospective trial. Two options are available: 'power' or 'piecewise constant' recruitment. The plot shows the total number of patients recruited over time and, if applicable, the breakdown of control versus treatment group recruitment.</p>"),

                 HTML("<p><u>Calculating Assurance</u></p>"),
                 HTML("<p>This tab enables the calculation of assurance based on all previous inputs. The app simulates \\( N \\) trials using a log-rank test (can be Fleming-Harrington) for analysis. Once the simulations are complete, an assurance plot (akin to a power curve) is displayed, showing the proportion of successful trials for different sample sizes.</p>")
        )


      ), style='width: 1200px; height: 600px',
      #Well panel UI ---------------------------------
      wellPanel(
        fluidRow(
          column(5, selectInput("outFormat", label = "Report format",
                                choices = list('html' = "html_document",
                                               'pdf' = "pdf_document",
                                               'Word' = "word_document"))
          ),

        ),
        fluidRow(
          column(3, downloadButton("report", "Download report")
          ),
          column(3, downloadButton("downloadObjects", "Download objects")
          ),
        )
      )
    )
  )

  server = function(input, output, session) {

    # Functions for the control tab ---------------------------------


    output$ExpDistText <- renderUI({
      HTML(paste0("S(", input$ExpSurvTime, ") ~ Beta("))
    })


    output$WeibullDistS1Text <- renderUI({
      paste0("S(", input$WeibullDistT1, ") ~ Beta(")
    })

    output$WeibullDistDelta1Text <- renderUI({
      paste0("S(", input$WeibullDistT2, ") - S(", input$WeibullDistT1,") ~ Beta(")
    })

    controlSurvivalData <- reactive({
      result <- list()

      if (input$ControlDist == "Exponential") {
        if (input$ExpChoice == "Fixed") {
          if (input$ExpRateorTime == "Parameter"){
            ExpRate <- input$ExpRate
          } else {
            ExpRate <- -log(input$ExpSurv)/input$ExpTime
          }

           finalSurvTime <- -log(0.01) / ExpRate
           controlTime <- seq(0, finalSurvTime, length.out = 100)
           controlSurv <- exp(-ExpRate * controlTime)

           result$controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
           result$type <- "single"


        } else if (input$ExpChoice == "Distribution") {

            sampledLambda <- -log(rbeta(100, input$ExpBetaA, input$ExpBetaB)) / input$ExpSurvTime
            controlTime <- seq(0, -log(0.05) / min(sampledLambda), length.out = 100)

            survivalMatrix <- exp(-outer(sampledLambda, controlTime, "*")) # Vectorized calculation
            medVec <- apply(survivalMatrix, 2, median)
            UBVec <- apply(survivalMatrix, 2, quantile, 0.975)
            LBVec <- apply(survivalMatrix, 2, quantile, 0.025)

            result$controlDF <- data.frame(controlTime = controlTime, medVec = medVec, UBVec = UBVec, LBVec = LBVec)
            result$type <- "distribution"

          }


      } else if (input$ControlDist == "Weibull") {
        if (input$WeibullChoice == "Fixed") {
          if (input$WeibRateorTime == "Parameters") {
            finalSurvTime <- (1 / input$WeibullScale) * (-log(0.01))^(1 / input$WeibullShape)
            controlTime <- seq(0, finalSurvTime, length.out = 100)
            controlSurv <- exp(-(input$WeibullScale * controlTime)^input$WeibullShape)

            result$controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
            result$type <- "single"
          } else {

            # Solve for lambda and gamma
            WeibFunc <- function(params) {
              lambda <- params[1]
              k <- params[2]
              c(exp(-(input$WeibullTime1*lambda)^k) - input$WeibullSurv1,
                exp(-(input$WeibullTime2*lambda)^k) - input$WeibullSurv2)
            }

            solution <- nleqslv(c(1, 1), fn = WeibFunc)

            lambda <- solution$x[1]
            gamma <- solution$x[2]

            finalSurvTime <- (1 / lambda) * (-log(0.01))^(1 / gamma)
            controlTime <- seq(0, finalSurvTime, length.out = 100)
            controlSurv <- exp(-(lambda * controlTime)^gamma)

            result$controlDF <- data.frame(controlTime = controlTime, controlSurv = controlSurv)
            result$type <- "single"


          }


        } else if (input$WeibullChoice == "Distribution") {
          controlTime <- seq(0, 100, length.out = 100)
          n <- 500

          # Vectorized sampling
          sampledS1to <- rbeta(n, input$WeibullDistS1BetaA, input$WeibullDistS1BetaB)
          sampledDelta1 <- rbeta(n, input$WeibullDistDelta1BetaA, input$WeibullDistDelta1BetaB)
          sampledS1toPrime <- sampledS1to - sampledDelta1

          # Solve for lambda and gamma in a vectorized manner
          solutions <- lapply(1:n, function(i) {
            nleqslv(c(10, 1), function(params) {
              lambda <- params[1]
              k <- params[2]
              c(exp(-(input$WeibullDistT1 / lambda)^k) - sampledS1to[i],
                exp(-(input$WeibullDistT2 / lambda)^k) - sampledS1toPrime[i])
            })$x
          })

          lambdas <- sapply(solutions, `[`, 1)
          gammas <- sapply(solutions, `[`, 2)

          # Vectorized survival curve calculation
          survivalMatrix <- exp(-outer(1 / lambdas, controlTime, function(lambda, t) (lambda * t)^gammas))
          medVec <- apply(survivalMatrix, 2, median)
          UBVec <- apply(survivalMatrix, 2, quantile, 0.975)
          LBVec <- apply(survivalMatrix, 2, quantile, 0.025)

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
                               xl = TLimits()[1], xu = TLimits()[2]
                               ))

    })

    # Functions for the post-delay HR tab ---------------------------------

    output$HRPlot <- renderPlot({

      suppressWarnings(plotfit(HRFit(), d = input$HRDist,
                               ql = 0.05, qu = 0.95,
                               xl = HRLimits()[1], xu = HRLimits()[2]
                               ))

    })



    # Functions for the feedback tab ---------------------------------

    treatmentSurvivalData <- reactive({
      # Initialize an empty list to hold output data
      result <- list()
      nSamples <- 2500
      # Calculate treatment survival data based on distribution type and input options
      if (input$ControlDist == "Exponential") {
        if (input$ExpChoice == "Fixed") {
          if (input$ExpRateorTime == "Parameter") {
            ExpRate <- input$ExpRate
          } else {
            ExpRate <- -log(input$ExpSurv)/input$ExpTime
          }
            sampledTrtHazard <- rep(NA, nSamples)
            sampledbigT <- rep(NA, nSamples)
            for (i in 1:nSamples){
              if (runif(1) > input$P_S){
                #Curves do not separate
                sampledbigT[i] <- 0
                sampledTrtHazard[i] <- ExpRate
              } else {
                if (runif(1) > input$P_DTE){
                  #Curves separate with no delay
                  HRSample <- sampleFit(HRFit(), n = 1)
                  sampledTrtHazard[i] <- ExpRate*HRSample[,input$HRDist]
                  sampledbigT[i] <- 0
                } else{
                  #Curves separate with a delay
                  HRSample <- sampleFit(HRFit(), n = 1)
                  bigTSample <- sampleFit(TFit(), n = 1)
                  sampledbigT[i] <- bigTSample[,input$TDist]
                  sampledTrtHazard[i] <- ExpRate*HRSample[,input$HRDist]
                }
              }
            }

            controlTime <- seq(0, 100, length.out = 100)
            survival_curves <- matrix(NA, nrow = nSamples, ncol = length(controlTime))
            for (i in 1:nSamples){
              survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                            exp(-ExpRate*controlTime), exp(-ExpRate*sampledbigT[i]-sampledTrtHazard[i]*(controlTime-sampledbigT[i])))
            }

            medVec <- apply(survival_curves, 2, median)
            UBVec <- apply(survival_curves, 2, quantile, 0.975)
            LBVec <- apply(survival_curves, 2, quantile, 0.025)

            lowerT <- quantile(sampledbigT, 0.025)
            upperT <- quantile(sampledbigT, 0.975)

            result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec,
                                             UBVec = UBVec, LBVec = LBVec)
            result$lowerT <- lowerT
            result$upperT <- upperT

        } else if (input$ExpChoice == "Distribution") {

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

          lowerT <- quantile(sampledbigT, 0.025)
          upperT <- quantile(sampledbigT, 0.975)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec,
                                           UBVec = UBVec, LBVec = LBVec)
          result$lowerT <- lowerT
          result$upperT <- upperT

        }

      } else if (input$ControlDist == "Weibull") {
        if (input$WeibullChoice == "Fixed") {

          sampledTrtHazard <- rep(NA, nSamples)
          sampledbigT <- rep(NA, nSamples)
          for (i in 1:nSamples){
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

          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = nSamples, ncol = length(controlTime))
          for (i in 1:nSamples){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-(input$WeibullScale*controlTime)^input$WeibullShape),
                                          exp(-(input$WeibullScale*sampledbigT[i])^input$WeibullShape-(sampledTrtHazard[i])^input$WeibullShape*(controlTime^input$WeibullShape-(sampledbigT[i])^input$WeibullShape)))
          }

          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          lowerT <- quantile(sampledbigT, 0.025)
          upperT <- quantile(sampledbigT, 0.975)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec,
                                           UBVec = UBVec, LBVec = LBVec)
          result$lowerT <- lowerT
          result$upperT <- upperT


        } else if (input$WeibullChoice == "Distribution") {

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

          controlTime <- seq(0, 100, length.out = 100)
          survival_curves <- matrix(NA, nrow = nSamples, ncol = length(controlTime))
          for (i in 1:nSamples){
            survival_curves[i,] <- ifelse(controlTime<sampledbigT[i],
                                          exp(-(lambdaVec[i]*controlTime)^gammaVec[i]),
                                          exp(-(lambdaVec[i]*sampledbigT[i])^gammaVec[i]-(sampledTrtHazard[i])^gammaVec[i]*(controlTime^gammaVec[i]-(sampledbigT[i])^gammaVec[i])))
          }


          medVec <- apply(survival_curves, 2, median)
          UBVec <- apply(survival_curves, 2, quantile, 0.975)
          LBVec <- apply(survival_curves, 2, quantile, 0.025)

          lowerT <- quantile(sampledbigT, 0.025)
          upperT <- quantile(sampledbigT, 0.975)

          result$treatmentDF <- data.frame(controlTime = controlTime, medVec = medVec,
                                           UBVec = UBVec, LBVec = LBVec)
          result$lowerT <- lowerT
          result$upperT <- upperT
        }
      }

      return(result)
    })

    output$medianSurvivalFeedback <- renderUI({

      str1 <- ""

      if ("median_probability" %in% input$showfeedback){

        controlData <- controlSurvivalData()$controlDF
        treatmentData <- treatmentSurvivalData()$treatmentDF
        plotType <- controlSurvivalData()$type

        if (plotType == "single") {
          median_time_control <- approx(controlData$controlSurv, controlData$controlTime, xout = 0.5)$y
        }

        if (plotType == "distribution") {
          median_time_control <- approx(controlData$medVec, controlData$controlTime, xout = 0.5)$y
        }


        median_time_treatment <- approx(treatmentData$medVec, treatmentData$controlTime, xout = 0.5)$y
        str1 <- paste0("The median survival in the control group is ", round(median_time_control,1),
                       " and the median survival time in the treatment group is ", round(median_time_treatment, 1))

      }

      HTML(paste(str1, sep = '<br/>'))

    })

    output$CI_For_T_Feedback <- renderUI({

      str1 <- ""

      if ("ci_t" %in% input$showfeedback){

        str1 <- paste0("The 95% CI for T is:  (", round(treatmentSurvivalData()$lowerT,3), ",", round(treatmentSurvivalData()$upperT, 3), ")")

      }

      HTML(paste(str1, sep = '<br/>'))

    })

    observe({
      # Define the common choices
      common_choices <- c("Median survival probability" = "median_probability",
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

       if ("median_probability" %in% input$showfeedback){
         median_time_control <- approx(controlData$controlSurv, controlData$controlTime, xout = 0.5)$y
         median_time_treatment <- approx(treatmentData$medVec, treatmentData$controlTime, xout = 0.5)$y
         mediandf <- data.frame(x = seq(0, max(median_time_control, median_time_treatment), length=2), y = rep(0.5, 2))
         mediandf1 <- data.frame(x = rep(median_time_control, 2), y = seq(0, 0.5, length=2))
         mediandf2 <- data.frame(x = rep(median_time_treatment, 2), y = seq(0, 0.5, length=2))
         p1 <- p1 + geom_line(data = mediandf, aes(x = x, y=y), linetype = "dashed") +
           geom_line(data = mediandf1, aes(x = x, y=y), linetype = "dashed") +
           geom_line(data = mediandf2, aes(x = x, y=y), linetype = "dashed")
       }


      } else if (plotType == "distribution") {
        p1 <- ggplot(controlData, aes(x = controlTime)) +
          geom_line(aes(y = medVec), colour = "blue")  +
          xlim(0, max(controlData$controlTime)) + ylim(0, 1) +
          xlab("Time") + ylab("Survival")

        if ("ci_control" %in% input$showfeedback)
          p1 <- p1 + geom_line(aes(y = UBVec), colour = "blue", linetype = "dashed") +
            geom_line(aes(y = LBVec), colour = "blue", linetype = "dashed")


        if ("median_probability" %in% input$showfeedback){
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

      if ("ci_t" %in% input$showfeedback){
        lowerSurv <- approx(treatmentData$controlTime, treatmentData$medVec, xout = treatmentSurvivalData()$lowerT)$y
        upperSurv <- approx(treatmentData$controlTime, treatmentData$medVec, xout = treatmentSurvivalData()$upperT)$y

        p1 <- p1 + annotate("point", x = treatmentSurvivalData()$lowerT, y = lowerSurv) +
          annotate("point", x = treatmentSurvivalData()$upperT, y = upperSurv)
      }

      p1

    })

    output$plotFeedback <- renderPlot({
      feedbackPlot()
  })


    # Functions for the recruitment tab ---------------------------------

    output$cdfRec <- renderPlot({

      create_plot <- function(time, total, control, treatment) {
        # Create a data frame for ggplot
        data <- data.frame(
          time = rep(time, 3),
          patients = c(total, control, treatment),
          type = rep(c("Total", "Control", "Treatment"), each = length(time))
        )

        # Generate the plot using ggplot
        ggplot(data, aes(x = time, y = patients, color = type)) +
          geom_line() +
          labs(x = "Time", y = "Total Number of Patients") +
          scale_color_manual(values = c("red", "blue", "green"))
      }

      # Power Recruitment Method
      if (input$rec_method == "power") {
        timeValues <- seq(0, input$rec_period, length.out = 100)
        Total_Patients <- (timeValues / input$rec_period) ^ input$rec_power * input$numofpatients

        ratio_sum <- input$ControlRatio + input$TreatmentRatio
        Control_Patients <- Total_Patients * (input$ControlRatio / ratio_sum)
        Treatment_Patients <- Total_Patients * (input$TreatmentRatio / ratio_sum)

        create_plot(timeValues, Total_Patients, Control_Patients, Treatment_Patients)

      } else if (input$rec_method == "PWC") {
        rec_rate <- as.numeric(unlist(strsplit(input$rec_rate, ",")))
        rec_duration <- as.numeric(unlist(strsplit(input$rec_duration, ",")))

        # Calculate cumulative allocation
        cumulative_allocation <- cumsum(rec_rate * rec_duration)

        if (any(cumulative_allocation >= input$numofpatients)) {
          # Find first exceed index
          first_exceed_idx <- which(cumulative_allocation >= input$numofpatients)[1]
          z <- approx(cumulative_allocation, cumsum(rec_duration), xout = input$numofpatients)$y

          # Cap the cumulative allocation and zero out remaining durations
          cumulative_allocation[first_exceed_idx] <- input$numofpatients
          if (first_exceed_idx < length(cumulative_allocation)) {
            cumulative_allocation[(first_exceed_idx + 1):length(cumulative_allocation)] <- input$numofpatients
            rec_duration[(first_exceed_idx + 1):length(rec_duration)] <- 0
          }
          xaxis <- c(0, cumsum(rec_duration)[-length(rec_duration)], z)
          yaxis <- c(0, cumulative_allocation)

        } else {
          # Extend the last duration if needed
          remaining_patients <- input$numofpatients - cumulative_allocation[length(cumulative_allocation)]
          last_duration_extension <- remaining_patients / rec_rate[length(rec_rate)]
          rec_duration[length(rec_duration)] <- rec_duration[length(rec_duration)] + last_duration_extension
          cumulative_allocation <- cumsum(rec_rate * rec_duration)
          xaxis <- c(0, cumsum(rec_duration))
          yaxis <- c(0, cumulative_allocation[-length(cumulative_allocation)], input$numofpatients)
        }

        ratio_sum <- input$ControlRatio + input$TreatmentRatio
        Control_Patients <- yaxis * (input$ControlRatio / ratio_sum)
        Treatment_Patients <- yaxis * (input$TreatmentRatio / ratio_sum)

        create_plot(xaxis, yaxis, Control_Patients, Treatment_Patients)
      }
    })

    # Functions for the assurance tab ---------------------------------

    function_call <- reactive({
      n <- seq(10, input$numofpatients, length = input$nSampleSize)
      n_c <- n*(input$ControlRatio)/(sum(input$ControlRatio+input$TreatmentRatio))
      n_t <- n*(input$TreatmentRatio)/(sum(input$ControlRatio+input$TreatmentRatio))

      base_call <- paste0("calc_dte_assurance(n_c = c(",
                          paste(round(n_c), collapse = ", "),
                          "), \n n_t = c(",
                          paste(round(n_t), collapse = ", "),
                          "), \n control_model = list(dist = \"",
                          input$ControlDist,
                          "\"")

      if (input$ControlDist=="Exponential"){
        base_call <- paste0(base_call,
                            ", \n parameter_mode = \"",
                            input$ExpChoice,
                            "\"")
        if (input$ExpChoice=="Fixed"){
          base_call <- paste0(base_call,
                              ", \n fixed_type = \"",
                              input$ExpRateorTime,
                              "\"")
          if (input$ExpRateorTime == "Parameter"){
            base_call <-  paste0(base_call, ", \n lambda = ",
                                 input$ExpRate)
          } else if (input$ExpRateorTime == "Landmark"){
            base_call <-  paste0(base_call, ", \n t1 = ",
                                 input$ExpTime,
                                 ", \n surv_t1 = ",
                                 input$ExpSurv)
          }
        } else {
          base_call <-  paste0(base_call, ", \n t1 = ",
                               input$ExpSurvTime,
                               ", \n t1_Beta_a = ",
                               input$ExpBetaA,
                               ", \n t1_Beta_b = ",
                               input$ExpBetaB)
        }



        }


      if (input$ControlDist == "Weibull"){
        base_call <- paste0(base_call,
                            ", \n control_parameters = \"",
                            input$WeibullChoice,
                            "\"")

        if (input$WeibullChoice == "Fixed"){
          base_call <- paste0(base_call,
                              ", \n fixed_parameters_type = \"",
                              input$WeibRateorTime,
                              "\"")

          if (input$WeibRateorTime == "Parameter"){
            base_call <-  paste0(base_call, ", \n lambda_c = ",
                                 input$WeibullScale,
                                 ", \n gamma_c = ",
                                 input$WeibullShape)
          } else if (input$WeibRateorTime == "Landmark"){
            base_call <-  paste0(base_call, ", \n t1 = ",
                                 input$WeibullTime1,
                                 ", \n t2 = ",
                                 input$WeibullSurv1,
                                 ", \n surv_t1 = ",
                                 input$WeibullTime2,
                                 ", \n surv_t2 = ",
                                 input$WeibullSurv2)
          }

        } else if (input$WeibullChoice == "Distribution"){
          base_call <-  paste0(base_call, ", \n t1 = ",
                               input$WeibullDistT1,
                               ", \n t2 = ",
                               input$WeibullDistT2,
                               ", \n t1_Beta_a = ",
                               input$WeibullDistS1BetaA,
                               ", \n t1_Beta_b = ",
                               input$WeibullDistS1BetaB,
                               ", \n diff_Beta_a = ",
                               input$WeibullDistDelta1BetaA,
                               ", \n diff_Beta_b = ",
                               input$WeibullDistDelta1BetaB)
        }
      }


      base_call <- paste0(base_call,
                          "), \n effect_model = list(delay_SHELF = SHELF::fitdist(c(",
                          input$TValues,
                          "), probs = c(",
                          input$TProbs,
                          "), lower = ",
                          strsplit(input$TLimits, ", ")[[1]][1],
                          ", upper = ",
                          strsplit(input$TLimits, ", ")[[1]][2],
                          "), \n delay_dist = \"",
                          input$TDist,
                          "\", \n HR_SHELF = SHELF::fitdist(c(",
                          input$HRValues,
                          "), probs = c(",
                          input$HRProbs,
                          "), lower = ",
                          strsplit(input$HRLimits, ", ")[[1]][1],
                          ", upper = ",
                          strsplit(input$HRLimits, ", ")[[1]][2],
                          "), \n HR_dist = \"",
                          input$HRDist,
                          "\",  \n P_S = ",
                          input$P_S,
                          ", \n P_DTE = ",
                          input$P_DTE,
                          "), \n censoring_model = list(method = \"",
                          input$censType,
                          "\"")

      if (input$censType == "Time"){
        base_call <- paste0(base_call,
                            ", \n time = ",
                            input$cens_time)
      }

      if (input$censType == "Events"){
        base_call <- paste0(base_call,
                            ", \n events = ",
                            input$cens_events)
      }

      if (input$censType == "IF"){
        base_call <- paste0(base_call,
                            ", \n IF = ",
                            input$cens_IF)
      }

      base_call <- paste0(base_call,
                          "), \n recruitment_model = list(method = \"",
                          input$rec_method,
                          "\"")

      if (input$rec_method == "power"){
        base_call <- paste0(base_call,
                            ", \n period = ",
                            input$rec_period,
                            ", \n power = ",
                            input$rec_power)
      }
      if (input$rec_method == "PWC"){
        base_call <- paste0(base_call,
                            ", \n rate = ",
                            input$rec_rate,
                            ", \n duration = ",
                            input$rec_duration)
      }

      base_call <- paste0(base_call,
                          "), \n analysis_model = list(method =  \"",
                          input$analysisType,
                          "\"")

      if (input$analysisType == "FHT"){
        base_call <- paste0(base_call,
                            ", \n rho = ",
                            input$rho,
                            ", \n gamma = ",
                            input$gamma)

      }

      if (input$analysisType == "MW"){
        if (input$MW_Parameter == "t_star"){
          base_call <- paste0(base_call,
                              ", \n t_star = ",
                              input$t_star)
        }

        if (input$MW_Parameter == "s_star"){
          base_call <- paste0(base_call,
                              ", \n s_star = ",
                              input$s_star)
        }


      }


      base_call <- paste0(base_call,
                          ", \n alternative_hypothesis = \"",
                          input$test_type,
                          "\", \n alpha = ",
                          input$alphaLevel)

      if (input$targetHRTF){
        base_call <- paste0(base_call,
                            ", \n success_threshold_HR = ",
                            input$target_HR)
      }

      base_call <- paste0(base_call,
                          "), \n n_sims = ",
                          input$nSamples,
                          ")")


      base_call

    })



    observe({
      updateAceEditor(session, "display_func", value = function_call())
    })



    calculateAssurance <- eventReactive(input$calcAssurance, {
      call_string <- function_call()  # e.g. "assurance(MCMC_sample = MCMC_sample)"

      # Create an environment with necessary objects
      eval_env <- new.env()

      result <- tryCatch({
        eval(parse(text = call_string), envir = eval_env)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      })

      return(result)
    })


    output$assurancePlot <- renderPlot({

      assOutput <- calculateAssurance()

      n <- seq(10, input$numofpatients, length = input$nSampleSize)

      if (input$targetHRTF){


        assuranceDF <- data.frame(N = n,
                                  Ass = assOutput$assurance,
                                  LB = assOutput$CI[,1],
                                  UB = assOutput$CI[,2],
                                  Ass_Target = assOutput$assurance_targetHR,
                                  LB_Target = assOutput$CI_targetHR[,1],
                                  UB_Target = assOutput$CI_targetHR[,2])




        # Generate the plot using ggplot
        ggplot(assuranceDF, aes(x = N)) +
          geom_line(aes(y = Ass), colour = "blue")+
          geom_line(aes(y = LB), colour = "blue", linetype = "dashed") +
          geom_line(aes(y = UB), colour = "blue", linetype = "dashed") +
          geom_line(aes(y = Ass_Target), colour = "red")+
          geom_line(aes(y = LB_Target), colour = "red", linetype = "dashed") +
          geom_line(aes(y = UB_Target), colour = "red", linetype = "dashed") +
          labs(x = "Number of Patients", y = "Assurance") +
          ylim(0, 1.05)



      } else {
        assuranceDF <- data.frame(N = n,
                                  Ass = assOutput$assurance,
                                  LB = assOutput$CI[,1],
                                  UB = assOutput$CI[,2])


        # Generate the plot using ggplot
        ggplot(assuranceDF, aes(x = N)) +
          geom_line(aes(y = Ass), colour = "blue")+
          geom_line(aes(y = LB), colour = "blue", linetype = "dashed") +
          geom_line(aes(y = UB), colour = "blue", linetype = "dashed") +
          labs(x = "Number of Patients", y = "Assurance") +
          ylim(0, 1.05)
      }


    })

    output$OC_table <- renderDT({

      assOutput <- calculateAssurance()

      n <- seq(10, input$numofpatients, length = input$nSampleSize)

      assuranceDF <- data.frame(Total_Sample_Size = n,
                                Assurance = round(sapply(assOutput, function(x) x$assurance), 2),
                                Average_Duration = round(sapply(assOutput, function(x) x$duration), 2),
                                Average_Sample_Size = round(sapply(assOutput, function(x) x$sample_size), 2)
      )

      datatable(assuranceDF)


    })

    # Functions for the well panel ---------------------------------

    output$report <- downloadHandler(
      filename = function(){switch(input$outFormat,
                                   html_document = "DTE_Assurance_Report.html",
                                   pdf_document = "DTE_Assurance_Report.pdf",
                                   word_document = "DTE_Assurance_Report.docx")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("shiny", "AssuranceApp", "report.Rmd",
                              package = "DTEAssurance"),
                  tempReport, overwrite = TRUE)


        # Set up parameters to pass to Rmd document
        params <- list(fit1 = TFit(), fit2 = HRFit(),
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
        object_list <- list(control_dist = input$ControlDist)

        if (input$ControlDist == "Exponential") {

          object_list$control_parameters <- input$ExpChoice

          if (input$ExpChoice == "Fixed") {
            object_list$fixed_parameters_type <- input$ExpRateorTime

            if (input$ExpRateorTime == "Parameter") {
              object_list$lambda_c <- input$ExpRate
            } else if (input$ExpRateorTime == "Landmark") {
              object_list$t1 <- input$ExpTime
              object_list$surv_t1 <- input$ExpSurv
            }
          }

          if (input$ExpChoice == "Distribution") {
            object_list$t1 <- input$ExpSurvTime
            object_list$t1_Beta_a <- input$ExpBetaA
            object_list$t1_Beta_b <- input$ExpBetaB
          }
        }

        if (input$ControlDist == "Weibull"){
          object_list$control_parameters = input$WeibullChoice



          if (input$WeibullChoice == "Fixed"){
            object_list$fixed_parameters_type = input$WeibRateorTime


            if (input$WeibRateorTime == "Parameter"){
              object_list$lambda_c = input$WeibullScale
              object_list$gamma_c = input$WeibullShape
            } else if (input$WeibRateorTime == "Landmark"){
              object_list$t1 = input$WeibullTime1
              object_list$t2 = input$WeibullTime2
              object_list$surv_t1 = input$WeibullSurv1
              object_list$surv_t2 = input$WeibullSurv2
            }

          } else if (input$WeibullChoice == "Distribution"){
            object_list$t1 = input$WeibullDistT1
            object_list$t2 = input$WeibullDistT2
            object_list$t1_Beta_a = input$WeibullDistS1BetaA
            object_list$t1_Beta_b = input$WeibullDistS1BetaB
            object_list$diff_Beta_a = input$WeibullDistDelta1BetaA
            object_list$diff_Beta_b = input$WeibullDistDelta1BetaB

          }
        }

        object_list$delay_time_SHELF = TFit()
        object_list$post_delay_HR_SHELF = HRFit()

        object_list$P_S = input$P_S
        object_list$P_DTE = input$P_DTE

        object_list$rec_method = input$rec_method


        if (input$rec_method == "power"){
          object_list$rec_period = input$rec_period
          object_list$rec_power = input$rec_power

        }
        if (input$rec_method == "PWC"){
          object_list$rec_rate = input$rec_rate
          object_list$rec_duration = input$rec_duration
        }

        object_list$t_dist = input$TDist
        object_list$HR_dist = input$HRDist

        object_list$ratio_groups = c(input$ControlRatio, input$TreatmentRatio)

        saveRDS(object_list, file)
      }
    )

  }

  shiny::shinyApp(ui, server)

