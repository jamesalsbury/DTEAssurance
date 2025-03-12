# library(shiny)
library(shinyjs)
library(survival)
# library(doParallel)
# library(ggplot2)
library(DT)
library(rhandsontable)
library(rpact)
# library(SHELF)
library(plotly)
# library(survminer)
library(shinyBS)

# rowCallback <- c(
#   "function(row, data){",
#   "  for(var i=0; i<data.length; i++){",
#   "    if(data[i] === null){",
#   "      $('td:eq('+i+')', row).html('NA')",
#   "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
#   "    }",
#   "  }",
#   "}"
# )


# UI definition
ui <- fluidPage(

  tags$style(HTML("
    .error-message {
      color: red;
      display: none;
    }
  ")),


  withMathJax(),
  shinyjs::useShinyjs(),

  # Application title
  titlePanel("Interim Analyses: Delayed Treatment Effects - Elicited Distributions"),

  mainPanel(
    tabsetPanel(
      # Design UI ---------------------------------
      tabPanel("Design",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("parametersFile", "Choose RDS File", accept = ".rds"),
                   h3("Control"),
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
                                bsTooltip(id = "ExpDistText", title = "The distribution of survival probability at time 1"),
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
                       )

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
                   ),
                   h3("Conditional Probabilities"),
                   fluidRow(
                     column(6,
                            numericInput("P_S", "Pr(survival curves separate)", value = 1, min = 0, max = 1)

                     ),
                     column(6,
                            numericInput("P_DTE", "Pr(treatment subject to a delay|survival curves separate)", value = 0, min = 0, max = 1)

                     )
                   ),
                   h3("Length of Delay - Elicit"),
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
                   h3("Post-delay hazard ratio - Elicit"),
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
                   h3("Trial Setup"),
                   numericInput("numofpatients", "Number of patients in the trial", value=1000),
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
                   ),


                   numericInput("censEvents", "Number of events", value = 100),
                   numericInput("nSamples", "Number of simulations", value=250),

                 ),
                 mainPanel = mainPanel(
                   uiOutput("P_S"),
                   uiOutput("P_DTE"),
                   br(),br(),
                   plotOutput("TDist"),
                   br(),br(),
                   plotOutput("HRDist")

                 )
               )
      ),



      # Simulate UI ---------------------------------

      tabPanel("Simulate",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   numericInput("number_of_looks", "Number of Stages", value = 2),
                    uiOutput("IA_inputs"),
                   wellPanel(
                     rHandsontableOutput("error_spending_table")),
                   actionButton("calc_GSD_assurance", label  = "Calculate")
                 ),
                 mainPanel = mainPanel(
                   tags$h4(
                     id = "toggleHeader",
                     style = "cursor: pointer; display: flex; align-items: center;",
                     tags$span(id = "arrow", "►"),  # Arrow icon
                     "Show/hide the function"
                   ),
                   tags$div(
                     id = "collapseText",
                     class = "collapse",
                     verbatimTextOutput("display_func_one_look")
                   ),
                   tags$script(
                     "$(document).on('click', '#toggleHeader', function() {
      $('#collapseText').collapse('toggle');
      var arrow = $('#arrow');
      if (arrow.text() == '►') {
        arrow.text('▼');
      } else {
        arrow.text('►');
      }
    });"
                   ),
                   tabsetPanel(
                     tabPanel("Tables",
                              hidden(selectizeInput("selectedOptionsIATableOneLook", "Selected Metrics",
                                                    choices = c("Interim Analysis Time", "Assurance", "Duration", "Sample Size",
                                                                "% Stop", "% Stop for Efficacy", "% Stop for Futility",
                                                                "% Correctly Stop", "% Correctly Stop for Efficacy", "% Correctly Stop for Futility",
                                                                "% Correctly Continue"),
                                                    selected = c("Assurance", "Duration", "Sample Size"),
                                                    multiple = TRUE)),
                              DTOutput("sim_table"),
                              hidden(uiOutput("finalAssTable1LookText"))),
                     tabPanel("Plots",
                              selectInput("BoundaryIA", "Choose the IF (to view)", choices = NULL),
                              plotlyOutput("boundary_plot"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("oneLookObservedHRBoundaries"),
                              br(), br(),
                              br(), br(),
                              plotOutput("oneLookPlotDuration"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("oneLookPlotSS")),
                   ),

                 )
               )
      ),
      # Bayesian UI ---------------------------------

      tabPanel("Bayesian",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   numericInput("IFBayesian", "Information Fraction", value = 0.5),
                   numericInput("tEffBayesian", "Target Effect", value = 0.8),
                   div(id = "bayesianErrorMessage", class = "error-message", textOutput("bayesianErrorMessage")),
                   actionButton("calcBayesian", label  = "Calculate", disabled = T)
                 ),
                 mainPanel = mainPanel(
                   plotOutput("BayesianPlot"),
                   plotOutput("BayesianEffPlot"),
                   plotOutput("BayesianBPPvTE"),
                   tableOutput("BayesianSS"),
                   tableOutput("BayesianDuration")

                 )
               )
      ),

      # Report UI ---------------------------------

      tabPanel("Report",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fluidRow(
                     column(4, hidden(checkboxInput("checkDesign", "Design", value = FALSE))),
                     column(4, hidden(selectizeInput("checkDesignOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c(""),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, hidden(selectizeInput("checkDesignOptionsPlots", "Selected Plots",
                                                     choices = c("Elicited T", "Elicited post-delay HR"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, hidden(checkboxInput("checkNoLook", "No Look", value = FALSE))),
                     column(4, hidden(selectizeInput("checkNoLookOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c("Assurance OC"),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, hidden(selectizeInput("checkNoLookOptionsPlots", "Selected Plots",
                                                     choices = c("Assurance Plot"),
                                                     selected = c(),
                                                     multiple = TRUE)))

                   ),

                   fluidRow(
                     column(4, hidden(checkboxInput("checkOneLook", "One Look", value = FALSE))),
                     column(4, hidden(selectizeInput("checkOneLookOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c("Interim Analysis Time", "Assurance", "Duration", "Sample Size",
                                                                 "% Stop", "% Stop for Efficacy", "% Stop for Futility",
                                                                 "% Correctly Stop", "% Correctly Stop for Efficacy", "% Correctly Stop for Futility",
                                                                 "% Correctly Continue"),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, hidden(selectizeInput("checkOneLookOptionsPlots", "Selected Plots",
                                                     choices = c("Boundary Plot", "Assurance vs Duration", "Assurance vs Sample Size"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, hidden(checkboxInput("checkTwoLooks", "Two Looks", value = FALSE))),
                     column(4, hidden(selectizeInput("checkTwoLooksOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c("Assurance", "Duration", "Sample Size",
                                                                 "Interim Analysis 1 Time", "Interim Analysis 2 Time",
                                                                 "% Stop", "% Stop Look 1", "% Stop Look 2",
                                                                 "% Stop Look 1 for Futility", "% Stop Look 2 for Futility",
                                                                 "% Stop Look 1 for Efficacy", "% Stop Look 2 for Efficacy",
                                                                 "% Stop for Efficacy", "% Stop for Futility",
                                                                 "Correctly Stop", "Correctly Stop for Efficacy", "Correctly Stop for Futility",
                                                                 "Correctly Stop at Look 1", "Correctly Stop at Look 2",
                                                                 "Correctly Stop for Efficacy at Look 1", "Correctly Stop for Efficacy at Look 2",
                                                                 "Correctly Stop for Futility at Look 1", "Correctly Stop for Futility at Look 2",
                                                                 "Correctly Continue", "Correctly Continue at Look 1", "Correctly Continue at Look 2"),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, hidden(selectizeInput("checkTwoLooksOptionsPlots", "Selected Plots",
                                                     choices = c("Boundary Plot", "Assurance vs Duration", "Assurance vs Sample Size"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, hidden(checkboxInput("checkBayesian", "Bayesian", value = FALSE))),
                     column(4, hidden(selectizeInput("checkBayesianOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c(""),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, hidden(selectizeInput("checkBayesianOptionsPlots", "Selected Plots",
                                                     choices = c("BPP Plot", "Target Effectiveness Plot", "BPP vs TE Plot"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   )


                 ),
                 mainPanel = mainPanel(
                   downloadButton("downloadHTML", "Html"),
                   downloadButton("downloadPDF", "Pdf")

                 )
               )
      )



    ), style='width: 1200px; height: 1000px'
  )
)


# Server logic
server <- function(input, output, session) {

  # Design Logic ---------------------------------

  output$ExpDistText <- renderUI({
    HTML(paste0("S(", input$ExpSurvTime, ") ~ Beta("))
  })


  output$WeibullDistS1Text <- renderUI({
    paste0("S(", input$WeibullDistT1, ") ~ Beta(")
  })

  output$WeibullDistDelta1Text <- renderUI({
    paste0("S(", input$WeibullDistT2, ") - S(", input$WeibullDistT1,") ~ Beta(")
  })


  reactValues <- reactiveValues(treatmentSamplesDF = NULL,
                                errorSeqOneLook = FALSE,
                                errorSeqTwoLooks = FALSE,
                                errorBayesian = FALSE,
                                iterationList = NULL,
                                TwoLooksSeq1 = NULL,
                                TwoLooksSeq2 = NULL,
                                lambdac = NULL)

  rds_data <- reactive({
    req(input$parametersFile)  # Ensure file is uploaded

    # Read the RDS file
    readRDS(input$parametersFile$datapath)
  })

  observe({
    req(rds_data())  # Ensure data is loaded

    inputData <<- rds_data()

    updateNumericInput(session, inputId = "P_S", value = inputData$P_S)
    updateNumericInput(session, inputId = "P_DTE", value = inputData$P_DTE)

    updateTextInput(session, inputId = "ControlDist", value = inputData$control_dist)

    if (inputData$control_dist=="Exponential"){
      updateTextInput(session, inputId = "ExpChoice", value = inputData$control_parameters)
      if (inputData$control_parameters=="Fixed"){
        updateTextInput(session, inputId = "ExpRateorTime", value = inputData$fixed_parameters_type)
        if (inputData$fixed_parameters_type=="Parameter"){
          updateTextInput(session, inputId = "ExpRate", value = inputData$lambda_c)
        } else if (inputData$fixed_parameters_type == "Landmark"){
          updateTextInput(session, inputId = "ExpTime", value = inputData$t1)
          updateTextInput(session, inputId = "ExpSurv", value = inputData$surv_t1)
        }
      } else if (inputData$control_parameters=="Distribution"){
        updateTextInput(session, inputId = "ExpSurvTime", value = inputData$t1)
        updateTextInput(session, inputId = "ExpBetaA", value = inputData$t1_Beta_a)
        updateTextInput(session, inputId = "ExpBetaB", value = inputData$t1_Beta_b)
      }
    }


    if (inputData$control_dist=="Weibull"){
      updateTextInput(session, inputId = "WeibullChoice", value = inputData$control_parameters)
      if (inputData$control_parameters=="Fixed"){
        updateTextInput(session, inputId = "WeibRateorTime", value = inputData$fixed_parameters_type)
        if (inputData$fixed_parameters_type=="Parameter"){
          updateTextInput(session, inputId = "WeibullScale", value = inputData$lambda_c)
          updateTextInput(session, inputId = "WeibullShape", value = inputData$gamma_c)
        } else if (inputData$fixed_parameters_type == "Landmark"){
          updateTextInput(session, inputId = "WeibullTime1", value = inputData$t1)
          updateTextInput(session, inputId = "WeibullSurv1", value = inputData$surv_t1)
          updateTextInput(session, inputId = "WeibullTime2", value = inputData$t2)
          updateTextInput(session, inputId = "WeibullSurv2", value = inputData$surv_t2)
        }
      } else if (inputData$control_parameters=="Distribution"){
        updateTextInput(session, inputId = "WeibullDistT1", value = inputData$t1)
        updateTextInput(session, inputId = "WeibullDistT2", value = inputData$t2)
        updateTextInput(session, inputId = "WeibullDistS1BetaA", value = inputData$t1_Beta_a)
        updateTextInput(session, inputId = "WeibullDistS1BetaB", value = inputData$t1_Beta_b)
        updateTextInput(session, inputId = "WeibullDistDelta1BetaA", value = inputData$diff_Beta_a)
        updateTextInput(session, inputId = "WeibullDistDelta1BetaB", value = inputData$diff_Beta_b)

      }
    }


    updateTextInput(session, inputId = "TLimits", value = toString(inputData$delay_time_SHELF$limits))
    updateTextInput(session, inputId = "TValues", value = toString(inputData$delay_time_SHELF$vals))
    updateTextInput(session, inputId = "TProbs", value = toString(inputData$delay_time_SHELF$probs))
    updateTextInput(session, inputId = "TDist", value = toString(inputData$t_dist))

    updateTextInput(session, inputId = "HRLimits", value = toString(inputData$post_delay_HR_SHELF$limits))
    updateTextInput(session, inputId = "HRValues", value = toString(inputData$post_delay_HR_SHELF$vals))
    updateTextInput(session, inputId = "HRProbs", value = toString(inputData$post_delay_HR_SHELF$probs))
    updateTextInput(session, inputId = "HRDist", value = toString(inputData$HR_dist))

    updateTextInput(session, inputId = "rec_method", value = inputData$rec_method)

    if (inputData$rec_method=="power"){
      updateTextInput(session, inputId = "rec_period", value = inputData$rec_period)
      updateTextInput(session, inputId = "rec_power", value = inputData$rec_power)
    }

    if (inputData$rec_method=="PWC"){
      updateTextInput(session, inputId = "rec_rate", value = inputData$rec_rate)
      updateTextInput(session, inputId = "rec_duration", value = inputData$rec_duration)
    }

    updateNumericInput(session, inputId = "ControlRatio", value = inputData$ratio_groups[1])
    updateNumericInput(session, inputId = "TreatmentRatio", value = inputData$ratio_groups[2])

  })


  # Simulate Logic ---------------------------------

  output$IA_inputs <- renderUI({
    num_looks <- input$number_of_looks - 1
    input_list <- lapply(1:num_looks, function(i) {
      fluidRow(
        column(4, numericInput(paste0("OneLookLB_", i), paste0("IA", i, ", from:"), value = 0.25)),
        column(4, numericInput(paste0("OneLookUB_", i), "to:", value = 0.75)),
        column(4, numericInput(paste0("OneLookBy_", i), "by:", value = 0.25))
      )
    })
    do.call(tagList, input_list)
  })

  function_call_one_look <- reactive({
    n_c <- (input$numofpatients*input$ControlRatio)/(sum(input$ControlRatio+input$TreatmentRatio))
    n_t <- (input$numofpatients*input$TreatmentRatio)/(sum(input$ControlRatio+input$TreatmentRatio))

    if (input$number_of_looks > 2){
      IA_List <- vector("list", input$number_of_looks-1)

      for (k in 1:(input$number_of_looks-1)){
        IA_List[[k]] <- seq(from = input[[paste0("OneLookLB_", k)]], to = input[[paste0("OneLookUB_", k)]], by = input[[paste0("OneLookBy_", k)]])
      }
      IF_list <- strictly_increasing_combinations(IA_List)
    } else {
      IF_list <- seq(input$OneLookLB_1, input$OneLookUB_1, by = input$OneLookBy_1)

    }
    IF_list <- paste(IF_list, ", 1", sep = "")


    spendingTable <- hot_to_r(input$error_spending_table)

    base_call <- paste0("calc_dte_assurance_interim(n_c = ",
                        paste(round(n_c), collapse = ", "),
                        ", \n n_t = ",
                        paste(round(n_t), collapse = ", "),
                        ", \n control_dist = \"",
                        input$ControlDist,
                        "\"")

    if (input$ControlDist=="Exponential"){
      base_call <- paste0(base_call,
                          ", \n control_parameters = \"",
                          input$ExpChoice,
                          "\"")
      if (input$ExpChoice=="Fixed"){
        base_call <- paste0(base_call,
                            ", \n fixed_parameters_type = \"",
                            input$ExpRateorTime,
                            "\"")
        if (input$ExpRateorTime == "Parameter"){
          base_call <-  paste0(base_call, ", \n lambda_c = ",
                               input$ExpRate)
        } else if (input$ExpRateorTime == "Landmark"){
          base_call <-  paste0(base_call, ", \n t1 = ",
                               input$ExpTime,
                               ", \n surv_t1 = ",
                               input$ExpSurv)
        }
      }

      if (input$ExpChoice=="Distribution"){
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

        if (input$WeibRateorTime == "Parameters"){
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
                        ", \n delay_time_SHELF = SHELF::fitdist(c(",
                        input$TValues,
                        "), probs = c(",
                        input$TProbs,
                        "), lower = ",
                        strsplit(input$TLimits, ", ")[[1]][1],
                        ", upper = ",
                        strsplit(input$TLimits, ", ")[[1]][2],
                        "), \n delay_time_dist = \"",
                        input$TDist,
                        "\", \n post_delay_HR_SHELF = SHELF::fitdist(c(",
                        input$HRValues,
                        "), probs = c(",
                        input$HRProbs,
                        "), lower = ",
                        strsplit(input$HRLimits, ", ")[[1]][1],
                        ", upper = ",
                        strsplit(input$HRLimits, ", ")[[1]][2],
                        "), \n post_delay_HR_dist = \"",
                        input$HRDist,
                        "\",  \n P_S = ",
                        input$P_S,
                        ", \n P_DTE = ",
                        input$P_DTE,
                        ", \n cens_events = ",
                        input$censEvents,
                        ", \n rec_method = \"",
                        input$rec_method,
                        "\"")


    if (input$rec_method == "power"){
      base_call <- paste0(base_call,
                          ", \n rec_period = ",
                          input$rec_period,
                          ", \n rec_power = ",
                          input$rec_power)
    }
    if (input$rec_method == "PWC"){
      base_call <- paste0(base_call,
                          ", \n rec_rate = ",
                          input$rec_rate,
                          ", \n rec_duration = ",
                          input$rec_duration)
    }




    base_call <- paste0(base_call,
                        ", \n nSims = ",
                        input$nSamples,
                        ", \n k = ",
                        input$number_of_looks,
                        ", \n IF_list = ",
                        paste0("c(", paste0('"', IF_list, '"', collapse = ", "), ")"),
                        paste0(", \n alpha_spending = c(", paste(spendingTable[,2], collapse = ", "), ")"),
                        paste0(", \n beta_spending = c(", paste(spendingTable[,3], collapse = ", "), ")")
    )


    base_call <- paste0(base_call, ")")

    return(base_call)

  })

  output$display_func_one_look <- renderText({
    function_call_one_look()
  })

  calculateGSDAssurance <- eventReactive(input$calc_GSD_assurance, {
    call_string <- function_call_one_look()
    result <- eval(parse(text = call_string))
    return(result)
  })


  observe({
    if (reactValues$errorSeqOneLook==TRUE){
      shinyjs::show("oneLookErrorMessage")
    } else{
      shinyjs::hide("oneLookErrorMessage")
    }
  })

  output$oneLookErrorMessage <- renderText({
    if (reactValues$errorSeqOneLook==TRUE){
      return("Your inputs are incorrect!")
    }

    return("")

  })


  output$error_spending_table <- renderRHandsontable({

    k <- input$number_of_looks  # Set the desired number of rows

    stage_names <- paste0("IA", 1:(k-1))
    stage_names <- c(stage_names, "Final")


    # Create the data frame with k rows
    initial_data <- data.frame(
      Stage = stage_names,
      alphaspending = seq(0.01, 0.025, length.out = k),  # Example values
      betaspending = seq(0.05, 0.1, length.out = k)      # Example values
    )

    colnames(initial_data) <- c("Stage", "Alpha spending (cumulative)", "Beta spending (cumulative)")

    rhandsontable(initial_data, rowHeaders = FALSE)  %>%
      hot_col(col = "Stage", readOnly = TRUE)
  })


  output$sim_table <- renderDT({

    sim_output <- calculateGSDAssurance()

    x<<- sim_output

    print(sim_output)

    length(x)

    sim_output_DF <- data.frame(matrix(NA, nrow = length(x), ncol = 3))

    colnames(sim_output_DF) <- c("Assurance", "Sample Size", "Duration")

    for (i in 1:length(x)){
      sim_output_DF[i,]$Assurance <- x[[i]]$power_mean
      sim_output_DF[i,]$`Sample Size` <- x[[i]]$ss_mean
      sim_output_DF[i,]$`Sample Size` <- x[[i]]$duration_mean
    }

    sim_output_DF

  })


  # Bayesian Logic ---------------------------------

  observe({
    if (!is.null(reactValues$treatmentSamplesDF)&reactValues$errorBayesian==F) {
      updateActionButton(session, "calcBayesian", disabled = FALSE)
    } else {
      updateActionButton(session, "calcBayesian", disabled = TRUE)
    }
  })


  observe({
    # Check if any of the inputs are NA
    if (is.na(input$IFBayesian) ||
        is.na(input$tEffBayesian)){
      reactValues$errorBayesian <- TRUE
    } else {
      # Check validity of input ranges
      if (input$IFBayesian <= 0 | input$IFBayesian >= 1 ||
          input$tEffBayesian <= 0) {
        reactValues$errorBayesian <- TRUE
      } else {
        reactValues$errorBayesian <- FALSE
      }
    }
  })

  observe({
    if (reactValues$errorBayesian==TRUE){
      shinyjs::show("bayesianErrorMessage")
    } else{
      shinyjs::hide("bayesianErrorMessage")
    }
  })

  output$bayesianErrorMessage <- renderText({
    if (reactValues$errorBayesian==TRUE){
      return("Your inputs are incorrect!")
    }

    return("")

  })


  bayesianFunc <- reactive({

    #Parallel: # Set up parallel processing
    cl <- makeCluster(detectCores())  # Use all available cores
    registerDoParallel(cl)

    NRep <- 50

    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5



    # Extract required input values
    ratioControl <- input$ratioControl
    ratioTreatment <- input$ratioTreatment
    numPatients <- input$numPatients
    lambdac <- reactValues$lambdac
    recTime <- input$recTime
    numEvents <- input$numEvents
    IFBayesian <- input$IFBayesian
    tEffBayesian <- input$tEffBayesian
    P_S <-  reactValues$treatmentSamplesDF$P_S
    P_DTE <- reactValues$treatmentSamplesDF$P_DTE
    elicitedDists <- reactValues$treatmentSamplesDF

    treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                              cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

    BPPVec <- foreach(i = 1:NRep, .combine = c, .export = c("SimDTEDataSet", "CensFunc", "BPPFunc"),
                      .packages = c("survival", "rjags", "dplyr")) %dopar% {

                        u <- runif(1)
                        if (u > P_S) {
                          HRStar <- 1
                          bigT <- 0
                        } else {
                          HRStar <- sample(treatmentSamplesDF[, 2], 1)
                          w <- runif(1)
                          bigT <- ifelse(w > P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
                        }

                        # Simulate control and treatment data

                        dataCombined <- SimDTEDataSet(round(ratioControl*numPatients/(ratioControl+ratioTreatment)),
                                                      round(ratioTreatment*numPatients/(ratioControl+ratioTreatment)),
                                                      lambdac, bigT, HRStar, recTime)


                        # Perform looks at different Information Fractions
                        finalDF <- CensFunc(dataCombined, numEvents)

                        test <- survdiff(Surv(survival_time, status) ~ group, data = finalDF$dataCombined)
                        coxmodel <- coxph(Surv(survival_time, status) ~ group, data = finalDF$dataCombined)
                        deltad <- as.numeric(exp(coef(coxmodel)))

                        BPPOutcome <- BPPFunc(dataCombined, numPatients, numEvents * IFBayesian, numEvents, recTime, tEffBayesian, elicitedDists)

                        Success <- (test$chisq > qchisq(0.95, 1) & deltad<1)

                        return(list(BPP = BPPOutcome$BPP, Success = Success, propEffect = BPPOutcome$propEffect))
                      }

    stopCluster(cl)  # Stop the cluster

    BPPVec <- data.frame(BPP = unlist(BPPVec[seq(1, length(BPPVec), by = 3)]),
                         Success = unlist(BPPVec[seq(2, length(BPPVec), by = 3)]),
                         propEffect = unlist(BPPVec[seq(3, length(BPPVec), by = 3)]))


    return(list(BPPVec = BPPVec))

  })

  observeEvent(input$calcBayesian, {

    shinyjs::show("checkBayesian")

    BPPVec <- bayesianFunc()

    output$BayesianPlot <- renderPlot({

      # Plotting histogram colored by ColorVar
      ggplot(BPPVec$BPPVec, aes(x = BPP, fill = Success)) +
        geom_histogram(position = "identity", alpha = 0.5) +
        scale_x_continuous(limits = c(0, 1)) + xlab("Bayesian Predictive Probability")

    })


    output$BayesianEffPlot <- renderPlot({

      # Plotting histogram colored by ColorVar
      ggplot(BPPVec$BPPVec, aes(x = propEffect, fill = Success)) +
        geom_histogram(position = "identity", alpha = 0.5) +
        scale_x_continuous(limits = c(0, 1)) + xlab("Proportion less than target effect")

    })

    output$BayesianBPPvTE <- renderPlot({

      plot(BPPVec$BPPVec$BPP, BPPVec$BPPVec$propEffect, xlim = c(0,1), ylim = c(0,1),
           xlab = "Bayesian Predictive Probability", ylab = "Proportion less than target effect")
      abline(a = 0, b = 1, lty = 2)

    })


  })


  # Interim Look Logic ---------------------------------

  observeEvent(input$generateButton, {

    n_control <- round((input$ratioControl*input$numPatients)/(input$ratioControl+input$ratioTreatment))
    n_treatment <- round((input$ratioTreatment*input$numPatients)/(input$ratioControl+input$ratioTreatment))

    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5
    treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                              cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

    u <- runif(1)
    if (u > reactValues$treatmentSamplesDF$P_S) {
      HRStar <- 1
      bigT <- 0
    } else {
      HRStar <- sample(treatmentSamplesDF[, 2], 1)
      w <- runif(1)
      bigT <- ifelse(w > reactValues$treatmentSamplesDF$P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
    }

    sampledData <- SimDTEDataSet(n_control, n_treatment, reactValues$lambdac, bigT, HRStar, input$recTime)

    #Now need to censor this
    if (input$censChoice=="Time"){
      sampledData <- CensFunc(sampledData, censTime = input$censTimeInterim)
    }

    if (input$censChoice=="Events"){
      sampledData <- CensFunc(sampledData, censEvents = input$censEventsInterim)
    }

    output$interimPlotKM <- renderPlot({

      kmfit <- survfit(Surv(survival_time, status)~group, data = sampledData$dataCombined)

      ggsurvplot(
        kmfit,
        data = sampledData$dataCombined,
        risk.table = TRUE,         # Add number at risk table
        risk.table.col = "strata", # Color by strata (group)
        ggtheme = theme_minimal(), # Apply minimal theme
        xlab = "Time",      # X-axis label
        ylab = "Survival Probability", # Y-axis label
        risk.table.y.text.col = TRUE,  # Use colored text for groups
        risk.table.y.text = F      # Turn off group names on the y-axis of the risk table
      )

    })

    output$summaryOutput <- renderText({


      # Fit Cox PH model
      cox_model <- coxph(Surv(survival_time, status) ~ group, data = sampledData$dataCombined)

      # Extract key results
      cox_summary <- summary(cox_model)
      hazard_ratio <- exp(cox_summary$coefficients[, "coef"])
      conf_int <-cox_summary$conf.int[, c("lower .95", "upper .95")]
      p_value <- cox_summary$coefficients[, "Pr(>|z|)"]

      # Create a formatted output
      paste0(
        "Cox Proportional Hazards Model Summary:\n",
        "--------------------------------------\n",
        "Variable: group\n",
        "Hazard Ratio: ", round(hazard_ratio, 2), "\n",
        "95% Confidence Interval: (", round(conf_int[1], 2), ", ", round(conf_int[2], 2), ")\n",
        "p-value: ", signif(p_value, 3), "\n"
      )
    })


  })





  # Report Logic ---------------------------------

  observe({
    if (input$checkDesign){
      shinyjs::show("checkDesignOptionsTables")
      shinyjs::show("checkDesignOptionsPlots")
    } else {
      shinyjs::hide("checkDesignOptionsTables")
      shinyjs::hide("checkDesignOptionsPlots")
    }
  })

  observe({
    if (input$checkNoLook){
      shinyjs::show("checkNoLookOptionsTables")
      shinyjs::show("checkNoLookOptionsPlots")
    } else {
      shinyjs::hide("checkNoLookOptionsTables")
      shinyjs::hide("checkNoLookOptionsPlots")
    }
  })

  observe({
    if (input$checkOneLook){
      shinyjs::show("checkOneLookOptionsTables")
      shinyjs::show("checkOneLookOptionsPlots")
    } else {
      shinyjs::hide("checkOneLookOptionsTables")
      shinyjs::hide("checkOneLookOptionsPlots")
    }
  })

  observe({
    if (input$checkTwoLooks){
      shinyjs::show("checkTwoLooksOptionsTables")
      shinyjs::show("checkTwoLooksOptionsPlots")
    } else {
      shinyjs::hide("checkTwoLooksOptionsTables")
      shinyjs::hide("checkTwoLooksOptionsPlots")
    }
  })

  observe({
    if (input$checkBayesian){
      shinyjs::show("checkBayesianOptionsTables")
      shinyjs::show("checkBayesianOptionsPlots")
    } else {
      shinyjs::hide("checkBayesianOptionsTables")
      shinyjs::hide("checkBayesianOptionsPlots")
    }
  })

  observe({
    if (!is.null(reactValues$treatmentSamplesDF)) {
      shinyjs::show("checkDesign")
    } else {
      shinyjs::hide("checkDesign")
    }
  })

  output$downloadHTML <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {

      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)


      params <- list(reactValues = reactValues,
                     checkDesign = input$checkDesign,
                     checkNoLook = input$checkNoLook,
                     checkOneLook = input$checkOneLook,
                     checkTwoLooks = input$checkTwoLooks,
                     checkBayesian = input$checkBayesian
      )

      if (input$checkDesign) {
        params <- c(params,
                    list(checkDesignOptionsTables = input$checkDesignOptionsTables,
                         checkDesignOptionsPlots = input$checkDesignOptionsPlots))

      }

      if (input$checkNoLook) {
        params <- c(params,
                    list(checkNoLookOptionsTables = input$checkNoLookOptionsTables,
                         checkNoLookOptionsPlots = input$checkNoLookOptionsPlots,
                         noLookFuncPlot = noLookFuncPlot(),
                         noLookFuncSS = noLookFuncSS()))
      }


      if (input$checkOneLook) {

        params <- c(params,
                    list(checkOneLookOptionsTables = input$checkOneLookOptionsTables,
                         checkOneLookOptionsPlots = input$checkOneLookOptionsPlots,
                         error_spending_table = input$error_spending_table,
                         oneLookBoundaryIA = input$oneLookBoundaryIA,
                         oneLookFunc = oneLookFunc()))

      }

      if (input$checkTwoLooks) {

        params <- c(params,
                    list(checkTwoLooksOptionsTables = input$checkTwoLooksOptionsTables,
                         checkTwoLooksOptionsPlots = input$checkTwoLooksOptionsPlots,
                         spendingTwoLooks = input$spendingTwoLooks,
                         twoLooksBoundaryIA = input$twoLooksBoundaryIA,
                         twoLooksFunc = twoLooksFunc()))

      }

      if (input$checkBayesian) {

        params <- c(params,
                    list(checkBayesianOptionsTables = input$checkBayesianOptionsTables,
                         checkBayesianOptionsPlots = input$checkBayesianOptionsPlots,
                         BPPVec = bayesianFunc()))

      }


      # Render the Rmd file to HTML with parameters
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )

}

# Run the Shiny app
shinyApp(ui, server)
