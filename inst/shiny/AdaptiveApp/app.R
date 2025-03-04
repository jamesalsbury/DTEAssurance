library(shiny)
library(shinyjs)
library(survival)
library(doParallel)
library(ggplot2)
library(DT)
library(rhandsontable)
library(rpact)
library(SHELF)
library(plotly)
library(survminer)

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"
)


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
                   h3("Recruitment"),
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
                   ),
                   selectInput("censType", "Type of censoring", choices = c("Time", "Events"), selected = "Time"),
                   conditionalPanel(
                     condition = "input.censType == 'Time'",
                     numericInput("censTime", "Censoring time", value = 60)
                   ),
                   conditionalPanel(
                     condition = "input.censType == 'Events'",
                     numericInput("censEvents", "Number of events", value = 100)
                   ),
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

      # No Look UI ---------------------------------

      tabPanel("No Look",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   actionButton("calcNoLook", label  = "Calculate")
                 ),
                 mainPanel = mainPanel(
                   hidden(numericInput("noLookAssuranceValue", "Number of events", value = 10)),
                   textOutput("noLookAssuranceText"),
                   plotOutput("noLookAssurancePlot"),
                   tableOutput("finalAssTableNoLook"),

                 )
               )
      ),

      # One Look UI ---------------------------------


      tabPanel("One Look",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   wellPanel(
                     fluidRow(
                       column(4, numericInput("OneLookLB", "IA1, from:", value = 0.25)),
                       column(4, numericInput("OneLookUB", "to:", value = 0.75)),
                       column(4, numericInput("OneLookBy", "by:", value = 0.25))
                     ),
                     textOutput("OneLookText"),
                     div(id = "oneLookErrorMessage", class = "error-message", textOutput("oneLookErrorMessage"))),
                   wellPanel(
                     rHandsontableOutput("spendingOneLook")),
                   actionButton("calcOneLook", label  = "Calculate", disabled = T)
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Tables",
                              hidden(selectizeInput("selectedOptionsIATableOneLook", "Selected Metrics",
                                                    choices = c("Interim Analysis Time", "Assurance", "Duration", "Sample Size",
                                                                "% Stop", "% Stop for Efficacy", "% Stop for Futility",
                                                                "% Correctly Stop", "% Correctly Stop for Efficacy", "% Correctly Stop for Futility",
                                                                "% Correctly Continue"),
                                                    selected = c("Assurance", "Duration", "Sample Size"),
                                                    multiple = TRUE)),
                              DTOutput("IATableOneLook"),
                              hidden(uiOutput("finalAssTable1LookText")),
                              tableOutput("noIATableOneLook")),
                     tabPanel("Plots",
                              selectInput("oneLookBoundaryIA", "Choose the IF (to view)", choices = NULL),
                              plotlyOutput("oneLookBoundaries"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("oneLookObservedHRBoundaries"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("oneLookPlotDuration"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("oneLookPlotSS")),
                   ),

                 )
               )
      ),

      # Two Looks UI ---------------------------------

      tabPanel("Two Looks",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   wellPanel(
                     fluidRow(
                       column(4, numericInput("TwoLooksLB1", "IA1, from:", value = 0.2)),
                       column(4, numericInput("TwoLooksUB1", "to:", value = 0.8)),
                       column(4, numericInput("TwoLooksBy1", "by:", value = 0.2))
                     ),
                     fluidRow(
                       column(4, numericInput("TwoLooksLB2", "IA2, from:", value = 0.3)),
                       column(4, numericInput("TwoLooksUB2", "to:", value = 0.9)),
                       column(4, numericInput("TwoLooksBy2", "by:", value = 0.2))
                     ),
                     textOutput("TwoLooksText1"),
                     textOutput("TwoLooksText2"),
                     div(id = "twoLooksErrorMessage", class = "error-message", textOutput("twoLooksErrorMessage"))),
                   #selectInput("sidedTwoLooks", "Test", choices = c("One-sided", "Two-sided"), selected = "One-sided"),
                   wellPanel(
                     rHandsontableOutput("spendingTwoLooks")),
                   numericInput("KFMonths", label = "KF rule", value = 3),
                   actionButton("calcTwoLooks", label  = "Calculate", disabled = T)
                 ),
                 mainPanel = mainPanel(
                   tabsetPanel(
                     tabPanel("Tables",
                              hidden(selectizeInput("selectedOptionsIATableTwoLooks", "Selected Metrics",
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
                                                    selected = c("Assurance", "Duration", "Sample Size"),
                                                    multiple = TRUE)),
                              DTOutput("IATableTwoLooks"),
                              hidden(uiOutput("finalAssTable2LooksText")),
                              tableOutput("noIATableTwoLooks"),
                              hidden(uiOutput("proposedTable2LooksText")),
                              tableOutput("proposedTableTwoLooks"),
                              hidden(uiOutput("wieandTable2LooksText")),
                              tableOutput("wieandTableTwoLooks")),
                     tabPanel("Plots",
                              selectInput("twoLooksBoundaryIA", "Choose the IFs (to view)", choices = NULL),
                              plotlyOutput("twoLooksBoundaries"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("twoLooksObservedHRBoundaries"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("twoLooksPlotDuration"),
                              br(), br(),
                              br(), br(),
                              plotlyOutput("twoLooksPlotSS"))
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

  })


  # No Look Logic ---------------------------------


  noLookFuncPlot <- reactive({

    NRep <- 500

    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5

    iterationList <- vector("list", length = 10)

    controlSS <- round(input$ratioControl*input$numPatients/(input$ratioControl+input$ratioTreatment))
    treatmentSS <- round(input$ratioTreatment*input$numPatients/(input$ratioControl+input$ratioTreatment))

    nEventsVec <- ceiling(seq(30, 1000, length = length(iterationList)))
    controlSSVec <- round(controlSS*nEventsVec/input$numEvents)
    treatmentSSVec <- round(treatmentSS*nEventsVec/input$numEvents)
    totalSSVec <- controlSSVec + treatmentSSVec


    for (i in 1:length(iterationList)){
      iterationList[[i]]$SampleSizeControl <- controlSSVec[i]
      iterationList[[i]]$SampleSizeTreatment <- treatmentSSVec[i]
      iterationList[[i]]$SampleSizeTotal <- totalSSVec[i]
      iterationList[[i]]$nEvents <- nEventsVec[i]
    }

    withProgress(message = 'Calculating', value = 0, {
      for (i in 1:length(iterationList)){
        treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                                  cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

        for (j in 1:NRep){

          u <- runif(1)
          if (u > reactValues$treatmentSamplesDF$P_S) {
            HRStar <- 1
            bigT <- 0
          } else {
            HRStar <- sample(treatmentSamplesDF[, 2], 1)
            w <- runif(1)
            bigT <- ifelse(w > reactValues$treatmentSamplesDF$P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
          }


          #Simulate control and treatment data
          dataCombined <- SimDTEDataSet(iterationList[[i]]$SampleSizeControl, iterationList[[i]]$SampleSizeTreatment,
                                        reactValues$lambdac,
                                        bigT, HRStar, input$recTime)

          #Perform looks at different Information Fractions
          finalDF <- CensFunc(dataCombined, iterationList[[i]]$nEvents)
          test <- survdiff(Surv(survival_time, status)~group, data = finalDF$dataCombined)
          coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDF$dataCombined)
          deltad <- as.numeric(exp(coef(coxmodel)))

          iterationList[[i]]$PowerVec[j] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
          iterationList[[i]]$DurationVec[j] <- finalDF$censTime
          iterationList[[i]]$SSVec[j] <- finalDF$SS

        }
        incProgress(1/length(iterationList))
      }
    })


    for (i in 1:length(iterationList)){
      iterationList[[i]]$Power <- mean(iterationList[[i]]$PowerVec)
      iterationList[[i]]$Duration <- mean(iterationList[[i]]$DurationVec)
    }

    myDF <- data.frame(SampleSize = rep(NA, length(iterationList)), Power = rep(NA, length(iterationList)),
                       Duration = rep(NA, length(iterationList)), nEvents = rep(NA, length(iterationList)))

    for (i in 1:length(iterationList)){
      myDF[i,]$Power <- iterationList[[i]]$Power
      myDF[i,]$SampleSize <- iterationList[[i]]$SampleSizeTotal
      myDF[i,]$Duration <- iterationList[[i]]$Duration
      myDF[i,]$nEvents <- iterationList[[i]]$nEvents
    }


    smoothedPower <- loess(Power ~  nEvents, data = myDF)
    smoothedDuration <- loess(Duration ~ nEvents, data = myDF)


    updateNumericInput(session, "noLookAssuranceValue", value = input$numEvents)
    shinyjs::show("noLookAssuranceValue")

    return(list(smoothedPower = smoothedPower, smoothedDuration = smoothedDuration, myDF = myDF))

  })

  noLookFuncSS <- reactive({

    NRep <- 500

    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5

    treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                              cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

    PowerVec <- rep(NA, NRep)
    DurationVec <- rep(NA, NRep)
    SSVec <- rep(NA, NRep)


    for (j in 1:NRep){

      u <- runif(1)
      if (u > reactValues$treatmentSamplesDF$P_S) {
        HRStar <- 1
        bigT <- 0
      } else {
        HRStar <- sample(treatmentSamplesDF[, 2], 1)
        w <- runif(1)
        bigT <- ifelse(w > reactValues$treatmentSamplesDF$P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
      }

      #Simulate control and treatment data
      dataCombined <- SimDTEDataSet(round(input$ratioControl*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                    round(input$ratioTreatment*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                    reactValues$lambdac, bigT, HRStar, input$recTime)


      #Perform looks at different Information Fractions
      finalDF <- CensFunc(dataCombined, input$numEvents)
      test <- survdiff(Surv(survival_time, status)~group, data = finalDF$dataCombined)
      coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDF$dataCombined)
      deltad <- as.numeric(exp(coef(coxmodel)))

      PowerVec[j] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
      DurationVec[j] <- finalDF$censTime
      SSVec[j] <- finalDF$SS

    }

    myDF <- data.frame(Power = mean(PowerVec), Duration = mean(DurationVec), SampleSize = mean(SSVec))


    return(list(myDF = myDF))

  })

  observeEvent(input$calcNoLook, {

    shinyjs::show("checkNoLook")

    noLookPlotOutput <- noLookFuncPlot()

    noLookSSOutput <- noLookFuncSS()

    output$noLookAssurancePlot <- renderPlot({

      plot(noLookPlotOutput$myDF$nEvents, predict(noLookPlotOutput$smoothedPower), ylim = c(0,1), type = "l", xlab = "Number of Events", ylab = "Assurance")
      abline(v = input$noLookAssuranceValue, lty = 2)

    })


    output$noLookAssuranceText <- renderText({

      estAss <- predict(noLookPlotOutput$smoothedPower, newdata = input$noLookAssuranceValue)

      paste0("With ", input$noLookAssuranceValue, " events, the assurance is estimated to be: ",
             round(estAss, 2))

    })


    output$finalAssTableNoLook <- renderTable({

      FinalAss <- noLookSSOutput$myDF

      colnames(FinalAss) <- c("Assurance", "Duration", "Sample Size")

      FinalAss
    }, digits = 3)

  })

  # One Look Logic ---------------------------------

  observe({
    if (!is.null(reactValues$treatmentSamplesDF)&reactValues$errorSeqOneLook==F) {
      updateActionButton(session, "calcOneLook", disabled = FALSE)
    } else {
      updateActionButton(session, "calcOneLook", disabled = TRUE)
    }
  })


  observe({
    # Check if any of the inputs are NA
    if (is.na(input$OneLookLB) ||
        is.na(input$OneLookUB) ||
        is.na(input$OneLookBy))  {
      reactValues$errorSeqOneLook <- TRUE
    } else {
      # Check validity of input ranges
      if (input$OneLookLB <= 0 | input$OneLookLB >= 1 ||
          input$OneLookUB <= 0 | input$OneLookUB >= 1 ||
          input$OneLookBy <= 0 | input$OneLookBy >= 1 ||
          input$OneLookLB > input$OneLookUB) {
        reactValues$errorSeqOneLook <- TRUE
      } else {
        reactValues$errorSeqOneLook <- FALSE
      }
    }
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

  output$OneLookText <- renderText({
    if (reactValues$errorSeqOneLook == TRUE) {
      return("")
    }

    OneLookSeq <- seq(input$OneLookLB, input$OneLookUB, by = input$OneLookBy)
    value <- paste0("We perform IA1 at: ", paste(OneLookSeq, collapse = ", "))
    return(value)
  })

  output$spendingOneLook <- renderRHandsontable({
    initial_data <- data.frame(
      Stage = c("IA1", "Final"),
      alphaspending = c("0.0125", "0.025"),
      betaspending = c("0.05", "0.1")
    )

    colnames(initial_data) <- c("Stage", "Alpha spending (cumulative)", "Beta spending (cumulative)")

    rhandsontable(initial_data, rowHeaders = FALSE)  %>%
      hot_col(col = "Stage", readOnly = TRUE)
  })

  observe({
    if (reactValues$errorSeqOneLook==F){
      updateSelectInput(session, "oneLookBoundaryIA", choices = seq(input$OneLookLB, input$OneLookUB, by = input$OneLookBy))
    }
  })

  observeEvent(c(input$spendingOneLook, input$oneLookBoundaryIA), {

    values <- hot_to_r(input$spendingOneLook)

    if (!is.null(values)){
      design <- getDesignGroupSequential(typeOfDesign = "asUser",
                                         informationRates = c(as.numeric(input$oneLookBoundaryIA), 1),
                                         userAlphaSpending = as.numeric(values[,2]),
                                         typeBetaSpending = "bsUser",
                                         userBetaSpending = as.numeric(values[,3]))


      output$oneLookBoundaries <- renderPlotly({


        boundaryDFEff <- data.frame(IF = c(as.numeric(input$oneLookBoundaryIA), 1),
                                    zStat = design$criticalValues)

        boundaryDFFut <- data.frame(IF = c(as.numeric(input$oneLookBoundaryIA), 1),
                                    zStat = c(design$futilityBounds, design$criticalValues[2]))

        # Calculate dynamic y-axis limits
        all_zStat <- c(boundaryDFEff$zStat, boundaryDFFut$zStat)
        ylim <- range(all_zStat)

        # Extend the limits by 10% on each side
        buffer <- 0.1 * (ylim[2] - ylim[1])
        extended_ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

        # Create the plot using plotly
        p <- plot_ly() %>%
          add_trace(data = boundaryDFEff, x = ~IF, y = ~zStat, type = 'scatter', mode = 'lines+markers',
                    line = list(color = 'red', width = 3),
                    marker = list(color = 'red', size = 10, symbol = 'circle'),
                    name = "Critical value") %>%
          add_trace(data = boundaryDFFut, x = ~IF, y = ~zStat, type = 'scatter', mode = 'lines+markers',
                    line = list(color = 'blue', width = 3),
                    marker = list(color = 'blue', size = 10, symbol = 'circle'),
                    name = "Futility bound") %>%
          layout(yaxis = list(range = extended_ylim, title = "Futility Bound and Critical Value"),
                 title = "Boundaries",
                 xaxis = list(title = "Information Fraction"))


        # Show the plot
        p

      })
    }


  })

  oneLookFunc <- reactive({

    NRep <- 200
    IAVec <- seq(input$OneLookLB, input$OneLookUB, by = input$OneLookBy)
    iterationList <- vector("list", length = length(IAVec))

    noLooksDF <- data.frame(Power = numeric(NRep),
                            Duration = numeric(NRep),
                            SS = numeric(NRep))

    values <- hot_to_r(input$spendingOneLook)

    for (k in 1:length(IAVec)){

      iterationList[[k]]$IF <- c(IAVec[k], 1)

      design <- getDesignGroupSequential(typeOfDesign = "asUser",
                                         informationRates = iterationList[[k]]$IF,
                                         userAlphaSpending = as.numeric(values[,2]),
                                         typeBetaSpending = "bsUser",
                                         userBetaSpending = as.numeric(values[,3]))

      iterationList[[k]]$EffBounds <- design$criticalValues
      iterationList[[k]]$futBounds <- design$futilityBounds

    }


    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5

    treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                              cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

    withProgress(message = 'Calculating', value = 0, {
      for (i in 1:NRep){

        u <- runif(1)
        if (u > reactValues$treatmentSamplesDF$P_S) {
          HRStar <- 1
          bigT <- 0
        } else {
          HRStar <- sample(treatmentSamplesDF[, 2], 1)
          w <- runif(1)
          bigT <- ifelse(w > reactValues$treatmentSamplesDF$P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
        }

        #Simulate control and treatment data
        dataCombined <- SimDTEDataSet(round(input$ratioControl*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                      round(input$ratioTreatment*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                      reactValues$lambdac, bigT, HRStar, input$recTime)

        #Perform looks at different Information Fractions
        finalDF <- CensFunc(dataCombined, input$numEvents)
        test <- survdiff(Surv(survival_time, status)~group, data = finalDF$dataCombined)
        coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDF$dataCombined)
        deltad <- as.numeric(exp(coef(coxmodel)))

        noLooksDF$Power[i] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
        noLooksDF$Duration[i] <- finalDF$censTime
        noLooksDF$SS[i] <- finalDF$SS


        for (k in 1:length(IAVec)){

          GSDOC <- GSDOneIAFunc(dataCombined, iterationList[[k]]$futBounds,
                                iterationList[[k]]$EffBounds, iterationList[[k]]$IF, input$numEvents)

          iterationList[[k]]$Power[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy", "Successful"), 1, 0)
          iterationList[[k]]$Duration[i] <- GSDOC$Duration
          iterationList[[k]]$SS[i] <- GSDOC$SS
          iterationList[[k]]$IA1Time[i] <- GSDOC$IA1Time
          iterationList[[k]]$Outcome[i] <- GSDOC$Outcome
          iterationList[[k]]$Stop[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy", "Futility"), 1, 0)
          iterationList[[k]]$StopEff[i] <- ifelse(GSDOC$Outcome == "Efficacy", 1, 0)
          iterationList[[k]]$StopFut[i] <- ifelse(GSDOC$Outcome == "Futility", 1, 0)
          iterationList[[k]]$Truth[i] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
          iterationList[[k]]$delta1[i] <- GSDOC$delta1
          iterationList[[k]]$delta2[i] <- GSDOC$delta2



        }

        incProgress(1/NRep)
      }
    })

    for (k in 1:length(IAVec)){

      observedHRDF <- data.frame(Outcome = iterationList[[k]]$Outcome,
                                 D1 = iterationList[[k]]$delta1,
                                 D2 = iterationList[[k]]$delta2)

      iterationList[[k]]$E1 <- mean(max(observedHRDF[observedHRDF$Outcome=="Efficacy",]$D1), min(observedHRDF[observedHRDF$Outcome!="Efficacy",]$D1))
      iterationList[[k]]$F1 <- mean(min(observedHRDF[observedHRDF$Outcome=="Futility",]$D1), max(observedHRDF[observedHRDF$Outcome!="Futility",]$D1))
      iterationList[[k]]$E2 <- mean(max(observedHRDF[observedHRDF$Outcome %in% c("Successful"),]$D2), min(observedHRDF[observedHRDF$Outcome %in% c("Unsuccessful"),]$D2))

    }



    correctlyStop <- rep(NA, length(IAVec))
    correctlyStopEff <- rep(NA, length(IAVec))
    correctlyStopFut <- rep(NA, length(IAVec))
    correctlyContinue <- rep(NA, length(IAVec))

    for (k in 1:length(IAVec)) {

      # Calculating Correctly Stop
      IndStopVec <- which(iterationList[[k]]$Stop == 1)
      if (length(IndStopVec) != 0) {
        OutcomeVec <- iterationList[[k]]$Outcome[IndStopVec]
        TruthVec <- iterationList[[k]]$Truth[IndStopVec]
        correctlyStopSum <- 0

        for (i in 1:length(IndStopVec)) {
          if ((OutcomeVec[i] == "Efficacy" & TruthVec[i] == 1) | (OutcomeVec[i] == "Futility" & TruthVec[i] == 0)) {
            correctlyStopSum <- correctlyStopSum + 1
          }
        }

        correctlyStop[k] <- correctlyStopSum / length(IndStopVec)
      } else {
        correctlyStop[k] <- NA
      }

      # Calculating Correctly Stopping for Efficacy
      IndStopEffVec <- which(iterationList[[k]]$StopEff == 1)
      if (length(IndStopEffVec) != 0) {
        TruthVec <- iterationList[[k]]$Truth[IndStopEffVec]
        correctlyStopEffSum <- 0

        for (i in 1:length(IndStopEffVec)) {
          if (TruthVec[i] == 1) {
            correctlyStopEffSum <- correctlyStopEffSum + 1
          }
        }

        correctlyStopEff[k] <- correctlyStopEffSum / length(IndStopEffVec)
      } else {
        correctlyStopEff[k] <- NA
      }

      # Calculating Correctly Stopping for Futility
      IndStopFutVec <- which(iterationList[[k]]$StopFut == 1)
      if (length(IndStopFutVec) != 0) {
        TruthVec <- iterationList[[k]]$Truth[IndStopFutVec]
        correctlyStopFutSum <- 0

        for (i in 1:length(IndStopFutVec)) {
          if (TruthVec[i] == 0) {
            correctlyStopFutSum <- correctlyStopFutSum + 1
          }
        }

        correctlyStopFut[k] <- correctlyStopFutSum / length(IndStopFutVec)
      } else {
        correctlyStopFut[k] <- NA
      }

      # Calculating Correctly Continuing
      IndContinueCorrectlyVec <- which(iterationList[[k]]$Stop == 0)
      if (length(IndContinueCorrectlyVec) != 0) {
        TruthVec <- iterationList[[k]]$Truth[IndContinueCorrectlyVec]
        correctlyContinueSum <- 0

        for (i in 1:length(IndContinueCorrectlyVec)) {
          if (TruthVec[i] == 1) {
            correctlyContinueSum <- correctlyContinueSum + 1
          }
        }

        correctlyContinue[k] <- correctlyContinueSum / length(IndContinueCorrectlyVec)
      } else {
        correctlyContinue[k] <- NA
      }

    }



    IADFOneLook <- data.frame(IF = IAVec,
                              IATime = unlist(lapply(iterationList, function(x) mean(x$IA1Time))),
                              Assurance = unlist(lapply(iterationList, function(x) mean(x$Power))),
                              Duration = unlist(lapply(iterationList, function(x) mean(x$Duration))),
                              SS = unlist(lapply(iterationList, function(x) mean(x$SS))),
                              Stop = unlist(lapply(iterationList, function(x) mean(x$Stop))),
                              StopEff = unlist(lapply(iterationList, function(x) mean(x$StopEff))),
                              StopFut = unlist(lapply(iterationList, function(x) mean(x$StopFut))),
                              correctlyStop = correctlyStop,
                              correctlyStopEff = correctlyStopEff,
                              correctlyStopFut = correctlyStopFut,
                              correctlyContinue = correctlyContinue)





    #Continuing is positive!

    colnames(IADFOneLook) <- c("Information Fraction", "Interim Analysis Time", "Assurance", "Duration", "Sample Size",
                               "% Stop", "% Stop for Efficacy", "% Stop for Futility",
                               "% Correctly Stop", "% Correctly Stop for Efficacy", "% Correctly Stop for Futility",
                               "% Correctly Continue")

    FinalAss <- t(colMeans(noLooksDF))
    FinalAss <- as.data.frame(FinalAss)
    colnames(FinalAss) <- c("Assurance", "Duration", "Sample Size")

    return(list(IADFOneLook = IADFOneLook, FinalAss = FinalAss,
                iterationList = iterationList))


  })


  observeEvent(input$calcOneLook, {

    shinyjs::show("checkOneLook")

    shinyjs::show("selectedOptionsIATableOneLook")

    oneLookoutput <- oneLookFunc()

    output$IATableOneLook <- renderDT({

      IADFOneLook <- subset(oneLookoutput$IADFOneLook, select = c("Information Fraction", input$selectedOptionsIATableOneLook))

      datatable(IADFOneLook, options = list(rowCallback = JS(rowCallback)),
                rownames = F) %>% formatStyle(
                  columns = colnames(IADFOneLook)
                ) %>%
        formatSignif(
          columns = colnames(IADFOneLook),
          digits = 3
        )
    })


    output$noIATableOneLook <- renderTable({
      oneLookoutput$FinalAss
    }, digits = 3)



    output$oneLookObservedHRBoundaries <- renderPlotly({

      seqChosen <- seq(input$OneLookLB, input$OneLookUB, by = input$OneLookBy)

      whichChosen <- which(seqChosen==as.numeric(input$oneLookBoundaryIA))


      boundaryDFEff <- data.frame(IF = oneLookoutput$iterationList[[whichChosen]]$IF,
                                  observedHR = c(oneLookoutput$iterationList[[whichChosen]]$E1, oneLookoutput$iterationList[[whichChosen]]$E2))

      boundaryDFFut <- data.frame(IF = oneLookoutput$iterationList[[whichChosen]]$IF,
                                  observedHR = c(oneLookoutput$iterationList[[whichChosen]]$F1, oneLookoutput$iterationList[[whichChosen]]$E2))

      # Calculate dynamic y-axis limits
      all_observedHR <- c(boundaryDFEff$observedHR, boundaryDFFut$observedHR)
      ylim <- range(all_observedHR)

      # Extend the limits by 10% on each side
      buffer <- 0.1 * (ylim[2] - ylim[1])
      extended_ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

      # Create the plot using plotly
      p <- plot_ly() %>%
        add_trace(data = boundaryDFEff, x = ~IF, y = ~observedHR, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'red', width = 3),
                  marker = list(color = 'red', size = 10, symbol = 'circle'),
                  name = "Critical value") %>%
        add_trace(data = boundaryDFFut, x = ~IF, y = ~observedHR, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'blue', width = 3),
                  marker = list(color = 'blue', size = 10, symbol = 'circle'),
                  name = "Futility bound") %>%
        layout(yaxis = list(range = extended_ylim, title = "Observed HR"),
               title = "Boundaries",
               xaxis = list(title = "Information Fraction"))


      # Show the plot
      p


    })


    output$oneLookPlotDuration <- renderPlotly({

      p <- plot_ly(oneLookoutput$IADFOneLook, x = ~Assurance, y = ~Duration,
                   text = ~ paste0("Information Fraction = ", `Information Fraction`), mode = "markers",
                   type = "scatter", marker = list(size = 10, color = "blue"), name = "Chosen Rules") %>%
        add_trace(x = ~oneLookoutput$FinalAss$Assurance, y = ~oneLookoutput$FinalAss$Duration, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "red"),
                  text = ~ "No Interim Analysis",
                  name = "No Interim Analysis") %>%
        layout(
          xaxis = list(title = list(text = "Assurance", font = list(color = "black", size = 14, family = "Arial", weight = "bold")), range = c(0, 1)),
          yaxis = list(title = list(text = "Duration", font = list(color = "black", size = 14, family = "Arial", weight = "bold"))),
          legend = list(orientation = "v", x = 1.05, y = 0.5),  # Position legend to the right
          title = "Assurance vs Duration for the different stopping rules"
        )

      p



    })

    output$oneLookPlotSS <- renderPlotly({

      p <- plot_ly(oneLookoutput$IADFOneLook, x = ~Assurance, y = ~`Sample Size`,
                   text = ~ paste0("Information Fraction = ", `Information Fraction`), mode = "markers",
                   type = "scatter", marker = list(size = 10, color = "blue"), name = "Chosen Rules") %>%
        add_trace(x = ~oneLookoutput$FinalAss$Assurance, y = ~oneLookoutput$FinalAss$`Sample Size`, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "red"),
                  text = ~ "No Interim Analysis",
                  name = "No Interim Analysis") %>%
        layout(
          xaxis = list(title = list(text = "Assurance", font = list(color = "black", size = 14, family = "Arial", weight = "bold")), range = c(0, 1)),
          yaxis = list(title = list(text = "Sample Size", font = list(color = "black", size = 14, family = "Arial", weight = "bold"))),
          legend = list(orientation = "v", x = 1.05, y = 0.5),  # Position legend to the right
          title = "Assurance vs Sample Size for the different stopping rules"
        )

      p


    })

    output$finalAssTable1LookText <- renderUI({
      p(HTML("<b>No Interim Analysis</b>"))
    })

    shinyjs::show("finalAssTable1LookText")


  })

  # Two Looks Logic ---------------------------------

  observe({
    if (!is.null(reactValues$treatmentSamplesDF)&reactValues$errorSeqTwoLooks==F) {
      updateActionButton(session, "calcTwoLooks", disabled = FALSE)
    } else {
      updateActionButton(session, "calcTwoLooks", disabled = TRUE)
    }
  })


  observe({
    # Check if any of the inputs are NA
    if (is.na(input$TwoLooksLB1) ||
        is.na(input$TwoLooksUB1) ||
        is.na(input$TwoLooksBy1) ||
        is.na(input$TwoLooksLB2) ||
        is.na(input$TwoLooksUB2) ||
        is.na(input$TwoLooksBy2)) {
      reactValues$errorSeqTwoLooks <- TRUE
    } else {
      # Check validity of input ranges
      if (input$TwoLooksLB1 <= 0 | input$TwoLooksLB1 >= 1 ||
          input$TwoLooksUB1 <= 0 | input$TwoLooksUB1 >= 1 ||
          input$TwoLooksBy1 <= 0 | input$TwoLooksBy1 >= 1 ||
          input$TwoLooksLB2 <= 0 | input$TwoLooksLB2 >= 1 ||
          input$TwoLooksUB2 <= 0 | input$TwoLooksUB2 >= 1 ||
          input$TwoLooksBy2 <= 0 | input$TwoLooksBy2 >= 1 ||
          input$TwoLooksLB1 > input$TwoLooksUB1 ||
          input$TwoLooksLB2 > input$TwoLooksUB2) {
        reactValues$errorSeqTwoLooks <- TRUE
      } else {
        # Check the generated sequences
        TwoLooksSeq1 <- seq(input$TwoLooksLB1, input$TwoLooksUB1, by = input$TwoLooksBy1)
        TwoLooksSeq2 <- seq(input$TwoLooksLB2, input$TwoLooksUB2, by = input$TwoLooksBy2)

        if (TwoLooksSeq1[1] >= TwoLooksSeq2[1] ||
            TwoLooksSeq1[length(TwoLooksSeq1)] >= TwoLooksSeq2[length(TwoLooksSeq2)]) {
          reactValues$errorSeqTwoLooks <- TRUE
        } else {
          reactValues$errorSeqTwoLooks <- FALSE
        }
      }
    }
  })


  observe({
    if (reactValues$errorSeqTwoLooks) {
      shinyjs::show("twoLooksErrorMessage")
    } else {
      shinyjs::hide("twoLooksErrorMessage")
    }
  })

  output$twoLooksErrorMessage <- renderText({
    if (reactValues$errorSeqTwoLooks) {
      return("Your inputs are incorrect!")
    } else {
      return("")
    }
  })

  output$TwoLooksText1 <- renderText({

    if (reactValues$errorSeqTwoLooks==TRUE) {
      return("")
    }

    TwoLooksSeq1 <- seq(input$TwoLooksLB1, input$TwoLooksUB1, by = input$TwoLooksBy1)
    value <- paste0("We perform IA1 at: ", paste(TwoLooksSeq1, collapse = ", "))
    return(value)
  })

  output$TwoLooksText2 <- renderText({

    if (reactValues$errorSeqTwoLooks==TRUE) {
      return("")
    }

    TwoLooksSeq2 <- seq(input$TwoLooksLB2, input$TwoLooksUB2, by = input$TwoLooksBy2)
    value <- paste0("We perform IA2 at: ", paste(TwoLooksSeq2, collapse = ", "))
    return(value)
  })

  output$spendingTwoLooks <- renderRHandsontable({
    initial_data <- data.frame(
      Stage = c("IA1", "IA2", "Final"),
      alphaspending = c("0.0083", "0.0167", "0.0250"),
      betaspending = c("0.0333", "0.0667", "0.1000")
    )

    colnames(initial_data) <- c("Stage", "Alpha spending (cumulative)", "Beta spending (cumulative)")

    rhandsontable(initial_data, rowHeaders = FALSE)  %>%
      hot_col(col = "Stage", readOnly = TRUE)
  })

  observe({

    if (reactValues$errorSeqTwoLooks==FALSE) {

      TwoLooksSeq1 <- seq(input$TwoLooksLB1, input$TwoLooksUB1, by = input$TwoLooksBy1)
      TwoLooksSeq2 <- seq(input$TwoLooksLB2, input$TwoLooksUB2, by = input$TwoLooksBy2)

      TwoLooksBoundaryIAChoices <- vector("list", length = sum(outer(TwoLooksSeq1, TwoLooksSeq2, "<")))

      myCount <- 1

      for (j in 1:length(TwoLooksSeq1)){
        for (k in 1:length(TwoLooksSeq2)){
          if (TwoLooksSeq1[j] < TwoLooksSeq2[k]){
            TwoLooksBoundaryIAChoices[[myCount]] <- c(TwoLooksSeq1[j], TwoLooksSeq2[k])
            myCount <- myCount + 1
          }
        }
      }

      TwoLooksBoundaryIAChoices <- sapply(TwoLooksBoundaryIAChoices, function(x) paste(x, collapse = ", "))
      TwoLooksBoundaryIAChoices <- setNames(TwoLooksBoundaryIAChoices, sapply(TwoLooksBoundaryIAChoices, function(x) paste(x, collapse = ", ")))
      updateSelectInput(session, "twoLooksBoundaryIA", choices = TwoLooksBoundaryIAChoices)

    }
  })


  observeEvent(c(input$spendingTwoLooks, input$twoLooksBoundaryIA), {
    values <- hot_to_r(input$spendingTwoLooks)

    if (!is.null(values)){
      design <- getDesignGroupSequential(typeOfDesign = "asUser",
                                         informationRates = as.numeric(c(strsplit(input$twoLooksBoundaryIA, ", ")[[1]], 1)),
                                         userAlphaSpending = as.numeric(values[,2]),
                                         typeBetaSpending = "bsUser",
                                         userBetaSpending = as.numeric(values[,3]))

      output$twoLooksBoundaries <- renderPlotly({

        boundaryDFEff <- data.frame(IF = as.numeric(c(strsplit(input$twoLooksBoundaryIA, ", ")[[1]], 1)),
                                    zStat = design$criticalValues)

        boundaryDFFut <- data.frame(IF = as.numeric(c(strsplit(input$twoLooksBoundaryIA, ", ")[[1]], 1)),
                                    zStat = c(design$futilityBounds, design$criticalValues[3]))

        # Calculate dynamic y-axis limits
        all_zStat <- c(boundaryDFEff$zStat, boundaryDFFut$zStat)
        ylim <- range(all_zStat)

        # Extend the limits by 10% on each side
        buffer <- 0.1 * (ylim[2] - ylim[1])
        extended_ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

        # Create the plot using plotly
        p <- plot_ly() %>%
          add_trace(data = boundaryDFEff, x = ~IF, y = ~zStat, type = 'scatter', mode = 'lines+markers',
                    line = list(color = 'red', width = 3),
                    marker = list(color = 'red', size = 10, symbol = 'circle'),
                    name = "Critical value") %>%
          add_trace(data = boundaryDFFut, x = ~IF, y = ~zStat, type = 'scatter', mode = 'lines+markers',
                    line = list(color = 'blue', width = 3),
                    marker = list(color = 'blue', size = 10, symbol = 'circle'),
                    name = "Futility bound") %>%
          layout(yaxis = list(range = extended_ylim, title = "Futility Bound and Critical Value"),
                 title = "Boundaries",
                 xaxis = list(title = "Information Fraction"))


        # Show the plot
        p



      })
    }

  })


  # Reactive expression to calculate metrics
  calculateTablemetrics <- function(iterationList, TwoLooksSeq1, TwoLooksSeq2) {


    PowerArray <- rep(NA, length(iterationList))
    DurationArray <- rep(NA, length(iterationList))
    SSArray <- rep(NA, length(iterationList))
    IATime1 <- rep(NA, length(iterationList))
    IATime2 <- rep(NA, length(iterationList))
    PercentStop <- rep(NA, length(iterationList))
    PercentStopLook1 <- rep(NA, length(iterationList))
    PercentStopLook2 <- rep(NA, length(iterationList))
    PercentStopLook1Fut <- rep(NA, length(iterationList))
    PercentStopLook2Fut <- rep(NA, length(iterationList))
    PercentStopLook1Eff <- rep(NA, length(iterationList))
    PercentStopLook2Eff <- rep(NA, length(iterationList))
    PercentStopEff <- rep(NA, length(iterationList))
    PercentStopFut <- rep(NA, length(iterationList))
    correctlyStopDF <- rep(NA, length(iterationList))
    correctlyStopEffDF <- rep(NA, length(iterationList))
    correctlyStopFutDF <- rep(NA, length(iterationList))
    correctlyStopLook1DF <- rep(NA, length(iterationList))
    correctlyStopLook2DF <- rep(NA, length(iterationList))
    correctlyStopEffLook1DF <- rep(NA, length(iterationList))
    correctlyStopEffLook2DF <- rep(NA, length(iterationList))
    correctlyStopFutLook1DF <- rep(NA, length(iterationList))
    correctlyStopFutLook2DF <- rep(NA, length(iterationList))
    correctlyContinueDF <- rep(NA, length(iterationList))
    correctlyContinueLook1DF <- rep(NA, length(iterationList))
    correctlyContinueLook2DF <- rep(NA, length(iterationList))

    myCount <- 1

    for (j in 1:length(TwoLooksSeq1)){
      for (k in 1:length(TwoLooksSeq2)){
        if (TwoLooksSeq1[j]<TwoLooksSeq2[k]){

          PowerArray[myCount] <- mean(iterationList[[myCount]]$Power)
          DurationArray[myCount] <- mean(iterationList[[myCount]]$Duration)
          SSArray[myCount] <- mean(iterationList[[myCount]]$SS)
          IATime1[myCount] <- mean(iterationList[[myCount]]$IA1Time)
          IATime2[myCount] <- mean(iterationList[[myCount]]$IA2Time)
          PercentStop[myCount] <- mean(iterationList[[myCount]]$Stop)
          PercentStopLook1[myCount] <- mean(iterationList[[myCount]]$StopLook1)
          PercentStopLook2[myCount] <- mean(iterationList[[myCount]]$StopLook2)
          PercentStopLook1Fut[myCount] <- mean(iterationList[[myCount]]$Outcome=="Futility1")
          PercentStopLook2Fut[myCount] <- mean(iterationList[[myCount]]$Outcome=="Futility2")
          PercentStopLook1Eff[myCount] <- mean(iterationList[[myCount]]$Outcome=="Efficacy1")
          PercentStopLook2Eff[myCount] <- mean(iterationList[[myCount]]$Outcome=="Efficacy2")
          PercentStopEff[myCount] <- mean(iterationList[[myCount]]$Outcome %in% c("Efficacy1", "Efficacy2"))
          PercentStopFut[myCount] <- mean(iterationList[[myCount]]$Outcome %in% c("Futility1", "Futility2"))

          myCount <- myCount + 1

        }
      }
    }

    myCount <- 1

    for (j in 1:length(TwoLooksSeq1)){
      for (k in 1:length(TwoLooksSeq2)){
        if (TwoLooksSeq1[j]<TwoLooksSeq2[k]){


          #Calculating Correctly Stop
          IndStopVec <- which(iterationList[[myCount]]$Stop==1)
          if (length(IndStopVec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopVec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopVec]
            correctlyStopSum <- 0

            for (i in 1:length(IndStopVec)){
              if ((OutcomeVec[i]=="Efficacy1"&TruthVec[i]==1)|(OutcomeVec[i]=="Futility1"&TruthVec[i]==0)|
                  (OutcomeVec[i]=="Efficacy2"&TruthVec[i]==1)|(OutcomeVec[i]=="Futility2"&TruthVec[i]==0)){
                correctlyStopSum <- correctlyStopSum + 1
              }
            }

            correctlyStopDF[myCount] <- correctlyStopSum/length(IndStopVec)

          } else {
            correctlyStopDF[myCount] <- NA
          }

          #Calculating Correctly Stopping for Efficacy
          IndStopEffVec <- which(iterationList[[myCount]]$StopEff==1)
          if (length(IndStopEffVec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopEffVec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopEffVec]
            correctlyStopEffSum <- 0

            for (i in 1:length(IndStopEffVec)){
              if (TruthVec[i]==1){
                correctlyStopEffSum <- correctlyStopEffSum + 1
              }
            }

            correctlyStopEffDF[myCount] <- correctlyStopEffSum/length(IndStopEffVec)

          } else {
            correctlyStopEffDF[myCount] <- NA
          }

          #Calculating Correctly Stopping for Futility
          IndStopFutVec <- which(iterationList[[myCount]]$StopFut==1)
          if (length(IndStopFutVec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopFutVec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopFutVec]
            correctlyStopFutSum <- 0

            for (i in 1:length(IndStopFutVec)){
              if (TruthVec[i]==0){
                correctlyStopFutSum <- correctlyStopFutSum + 1
              }
            }

            correctlyStopFutDF[myCount] <- correctlyStopFutSum/length(IndStopFutVec)

          } else {
            correctlyStopFutDF[myCount] <- NA
          }



          #Calculating Correctly Stopping at Look 1
          IndStopLook1Vec <- which(iterationList[[myCount]]$StopLook1==1)
          if (length(IndStopLook1Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopLook1Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopLook1Vec]
            correctlyStopLook1Sum <- 0

            for (i in 1:length(IndStopLook1Vec)){
              if ((OutcomeVec[i]=="Efficacy1"&TruthVec[i]==1)|(OutcomeVec[i]=="Futility1"&TruthVec[i]==0)){
                correctlyStopLook1Sum <- correctlyStopLook1Sum + 1
              }
            }

            correctlyStopLook1DF[myCount] <- correctlyStopLook1Sum/length(IndStopLook1Vec)

          } else {
            correctlyStopLook1DF[myCount] <- NA
          }

          #Calculating Correctly Stopping at Look 2
          IndStopLook2Vec <- which(iterationList[[myCount]]$StopLook2==1)
          if (length(IndStopLook2Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopLook2Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopLook2Vec]
            correctlyStopLook2Sum <- 0

            for (i in 1:length(IndStopLook2Vec)){
              if ((OutcomeVec[i]=="Efficacy2"&TruthVec[i]==1)|(OutcomeVec[i]=="Futility2"&TruthVec[i]==0)){
                correctlyStopLook2Sum <- correctlyStopLook2Sum + 1
              }
            }

            correctlyStopLook2DF[myCount] <- correctlyStopLook2Sum/length(IndStopLook2Vec)

          } else {
            correctlyStopLook2DF[myCount] <- NA
          }


          #Calculating Correctly Stopping for Efficacy at Look 1
          IndStopEffLook1Vec <- which(iterationList[[myCount]]$Outcome=="Efficacy1")
          if (length(IndStopEffLook1Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopEffLook1Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopEffLook1Vec]
            correctlyStopEffLook1Sum <- 0

            for (i in 1:length(IndStopEffLook1Vec)){
              if (TruthVec[i]==1){
                correctlyStopEffLook1Sum <- correctlyStopEffLook1Sum + 1
              }
            }

            correctlyStopEffLook1DF[myCount] <- correctlyStopEffLook1Sum/length(IndStopEffLook1Vec)

          } else {
            correctlyStopEffLook1DF[myCount] <- NA
          }

          #Calculating Correctly Stopping for Efficacy at Look 2
          IndStopEffLook2Vec <- which(iterationList[[myCount]]$Outcome=="Efficacy2")
          if (length(IndStopEffLook2Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopEffLook2Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopEffLook2Vec]
            correctlyStopEffLook2Sum <- 0

            for (i in 1:length(IndStopEffLook2Vec)){
              if (TruthVec[i]==1){
                correctlyStopEffLook2Sum <- correctlyStopEffLook2Sum + 1
              }
            }

            correctlyStopEffLook2DF[myCount] <- correctlyStopEffLook2Sum/length(IndStopEffLook2Vec)

          } else {
            correctlyStopEffLook2DF[myCount] <- NA
          }

          #Calculating Correctly Stopping for Futility at Look 1
          IndStopFutLook1Vec <- which(iterationList[[myCount]]$Outcome=="Futility1")
          if (length(IndStopFutLook1Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopFutLook1Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopFutLook1Vec]
            correctlyStopFutLook1Sum <- 0

            for (i in 1:length(IndStopFutLook1Vec)){
              if (TruthVec[i]==0){
                correctlyStopFutLook1Sum <- correctlyStopFutLook1Sum + 1
              }
            }

            correctlyStopFutLook1DF[myCount] <- correctlyStopFutLook1Sum/length(IndStopFutLook1Vec)

          } else {
            correctlyStopFutLook1DF[myCount] <- NA
          }

          #Calculating Correctly Stopping for Futility at Look 2
          IndStopFutLook2Vec <- which(iterationList[[myCount]]$Outcome=="Futility2")
          if (length(IndStopFutLook2Vec)!=0){

            OutcomeVec <- iterationList[[myCount]]$Outcome[IndStopFutLook2Vec]
            TruthVec <- iterationList[[myCount]]$Truth[IndStopFutLook2Vec]
            correctlyStopFutLook2Sum <- 0

            for (i in 1:length(IndStopFutLook2Vec)){
              if (TruthVec[i]==0){
                correctlyStopFutLook2Sum <- correctlyStopFutLook2Sum + 1
              }
            }

            correctlyStopFutLook2DF[myCount] <- correctlyStopFutLook2Sum/length(IndStopFutLook2Vec)

          } else {
            correctlyStopFutLook2DF[myCount] <- NA
          }



          #Calculating Correctly continuing
          IndContinueCorrectlyVec <- which(iterationList[[myCount]]$Stop==0)
          if (length(IndContinueCorrectlyVec)!=0){

            TruthVec <- iterationList[[myCount]]$Truth[IndContinueCorrectlyVec]
            correctlyContinueSum <- 0

            for (i in 1:length(IndContinueCorrectlyVec)){
              if (TruthVec[i]==1){
                correctlyContinueSum <- correctlyContinueSum + 1
              }
            }

            correctlyContinueDF[myCount] <- correctlyContinueSum/length(IndContinueCorrectlyVec)

          } else {
            correctlyContinueDF[myCount] <- NA
          }

          #Calculating Correctly continuing at Look 1
          IndContinueCorrectlyLook1Vec <- which(iterationList[[myCount]]$StopLook1==0)
          if (length(IndContinueCorrectlyLook1Vec)!=0){

            TruthVec <- iterationList[[myCount]]$Truth[IndContinueCorrectlyLook1Vec]
            correctlyContinueLook1Sum <- 0

            for (i in 1:length(IndContinueCorrectlyLook1Vec)){
              if (TruthVec[i]==1){
                correctlyContinueLook1Sum <- correctlyContinueLook1Sum + 1
              }
            }

            correctlyContinueLook1DF[myCount] <- correctlyContinueLook1Sum/length(IndContinueCorrectlyLook1Vec)

          } else {
            correctlyContinueLook1DF[myCount] <- NA
          }

          #Calculating Correctly continuing at Look 2
          IndContinueCorrectlyLook2Vec <- which(iterationList[[myCount]]$StopLook2==0)
          if (length(IndContinueCorrectlyLook2Vec)!=0){

            TruthVec <- iterationList[[myCount]]$Truth[IndContinueCorrectlyLook2Vec]
            correctlyContinueLook2Sum <- 0

            for (i in 1:length(IndContinueCorrectlyLook2Vec)){
              if (TruthVec[i]==1){
                correctlyContinueLook2Sum <- correctlyContinueLook2Sum + 1
              }
            }

            correctlyContinueLook2DF[myCount] <- correctlyContinueLook2Sum/length(IndContinueCorrectlyLook2Vec)

          } else {
            correctlyContinueLook2DF[myCount] <- NA
          }


          myCount <- myCount + 1

        }
      }

    }


    return(list(PowerArray = PowerArray, DurationArray = DurationArray,
                SSArray = SSArray, IATime1 = IATime1, IATime2 = IATime2,
                PercentStop = PercentStop,
                PercentStopLook1 = PercentStopLook1, PercentStopLook2 = PercentStopLook2,
                PercentStopLook1Fut = PercentStopLook1Fut, PercentStopLook2Fut = PercentStopLook2Fut,
                PercentStopLook1Eff = PercentStopLook1Eff, PercentStopLook2Eff = PercentStopLook2Eff,
                PercentStopEff = PercentStopEff, PercentStopFut = PercentStopFut,
                correctlyStopDF = correctlyStopDF,
                correctlyStopEffDF = correctlyStopEffDF, correctlyStopFutDF = correctlyStopFutDF,
                correctlyStopLook1DF = correctlyStopLook1DF, correctlyStopLook2DF = correctlyStopLook2DF,
                correctlyStopEffLook1DF = correctlyStopEffLook1DF, correctlyStopEffLook2DF = correctlyStopEffLook2DF,
                correctlyStopFutLook1DF = correctlyStopFutLook1DF, correctlyStopFutLook2DF = correctlyStopFutLook2DF,
                correctlyContinueDF = correctlyContinueDF, correctlyContinueLook1DF = correctlyContinueLook1DF,
                correctlyContinueLook2DF = correctlyContinueLook2DF))

  }


  twoLooksFunc <- reactive({


    NRep <- 200
    TwoLooksSeq1 <- seq(input$TwoLooksLB1, input$TwoLooksUB1, by = input$TwoLooksBy1)
    TwoLooksSeq2 <- seq(input$TwoLooksLB2, input$TwoLooksUB2, by = input$TwoLooksBy2)

    iterationList <- vector("list", length = sum(outer(TwoLooksSeq1, TwoLooksSeq2, "<")))

    noLooksDF <- data.frame(Power = numeric(NRep),
                            Duration = numeric(NRep),
                            SS = numeric(NRep))

    values <- hot_to_r(input$spendingTwoLooks)

    myCount <- 1

    for (j in 1:length(TwoLooksSeq1)){
      for (k in 1:length(TwoLooksSeq2)){
        if (TwoLooksSeq1[j]<TwoLooksSeq2[k]){

          iterationList[[myCount]]$IF <- c(TwoLooksSeq1[j], TwoLooksSeq2[k], 1)

          design <- getDesignGroupSequential(typeOfDesign = "asUser",
                                             informationRates = iterationList[[myCount]]$IF,
                                             userAlphaSpending = as.numeric(values[,2]),
                                             typeBetaSpending = "bsUser",
                                             userBetaSpending = as.numeric(values[,3]))

          iterationList[[myCount]]$EffBounds <- design$criticalValues
          iterationList[[myCount]]$futBounds <- design$futilityBounds

          myCount <- myCount + 1

        }
      }
    }



    conc.probs <- matrix(0, 2, 2)
    conc.probs[1, 2] <- 0.5


    treatmentSamplesDF <- SHELF::copulaSample(reactValues$treatmentSamplesDF$fit1, reactValues$treatmentSamplesDF$fit2,
                                              cp = conc.probs, n = 1e4, d = reactValues$treatmentSamplesDF$d)

    proposedDF <- data.frame(matrix(NA, ncol = 9, nrow = NRep))

    colnames(proposedDF) <- c("Look1Power", "Look2Power", "FinalLookPower",
                              "Look1SS", "Look2SS", "FinalLookSS",
                              "Look1Duration", "Look2Duration", "FinalLookDuration")

    WieandDF <- data.frame(matrix(NA, ncol = 9, nrow = NRep))

    colnames(WieandDF) <- c("Look1Power", "Look2Power", "FinalLookPower",
                            "Look1SS", "Look2SS", "FinalLookSS",
                            "Look1Duration", "Look2Duration", "FinalLookDuration")


    withProgress(message = 'Calculating', value = 0, {
      for (i in 1:NRep){

        u <- runif(1)
        if (u > reactValues$treatmentSamplesDF$P_S) {
          HRStar <- 1
          bigT <- 0
        } else {
          HRStar <- sample(treatmentSamplesDF[, 2], 1)
          w <- runif(1)
          bigT <- ifelse(w > reactValues$treatmentSamplesDF$P_DTE, 0, sample(treatmentSamplesDF[, 1], 1))
        }

        #Simulate control and treatment data
        dataCombined <- SimDTEDataSet(round(input$ratioControl*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                      round(input$ratioTreatment*input$numPatients/(input$ratioControl+input$ratioTreatment)),
                                      reactValues$lambdac, bigT, HRStar, input$recTime)

        #Do the proposed rule on this data set
        proposedDF[i,] <- proposedRuleFunc(dataCombined, input$numEvents, input$KFMonths, 2/3)

        WieandDF[i,] <- WieandRuleFunc(dataCombined, input$numEvents)


        #Perform looks at different Information Fractions
        finalDF <- CensFunc(dataCombined, input$numEvents)
        test <- survdiff(Surv(survival_time, status)~group, data = finalDF$dataCombined)
        coxmodel <- coxph(Surv(survival_time, status)~group, data = finalDF$dataCombined)
        deltad <- as.numeric(exp(coef(coxmodel)))

        noLooksDF$Power[i] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
        noLooksDF$Duration[i] <- finalDF$censTime
        noLooksDF$SS[i] <- finalDF$SS


        for (k in 1:length(iterationList)){

          GSDOC <- GSDTwoIAFunc(dataCombined, iterationList[[k]]$futBounds,
                                iterationList[[k]]$EffBounds, iterationList[[k]]$IF, input$numEvents)

          iterationList[[k]]$Power[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy1", "Efficacy2", "Successful"), 1, 0)
          iterationList[[k]]$Duration[i] <- GSDOC$Duration
          iterationList[[k]]$SS[i] <- GSDOC$SS
          iterationList[[k]]$IA1Time[i] <- GSDOC$IA1Time
          iterationList[[k]]$IA2Time[i] <- GSDOC$IA2Time
          iterationList[[k]]$Outcome[i] <- GSDOC$Outcome
          iterationList[[k]]$Stop[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy1", "Futility1", "Efficacy2", "Futility2"), 1, 0)
          iterationList[[k]]$StopLook1[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy1", "Futility1"), 1, 0)
          iterationList[[k]]$StopLook2[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy2", "Futility2"), 1, 0)
          iterationList[[k]]$StopEff[i] <- ifelse(GSDOC$Outcome %in% c("Efficacy1", "Efficacy2"), 1, 0)
          iterationList[[k]]$StopFut[i] <- ifelse(GSDOC$Outcome %in% c("Futility1", "Futility2"), 1, 0)
          iterationList[[k]]$Truth[i] <- (test$chisq > qchisq(0.95, 1) & deltad<1)
          iterationList[[k]]$delta1[i] <- GSDOC$delta1
          iterationList[[k]]$delta2[i] <- GSDOC$delta2
          iterationList[[k]]$delta3[i] <- GSDOC$delta3

        }

        incProgress(1/NRep)
      }

    })



    for (k in 1:length(iterationList)){

      observedHRDF <- data.frame(Outcome = iterationList[[1]]$Outcome,
                                 D1 = iterationList[[1]]$delta1,
                                 D2 = iterationList[[1]]$delta2,
                                 D3 = iterationList[[1]]$delta3)

      StopLook1DF <- observedHRDF[observedHRDF$Outcome %in% c("Efficacy1", "Futility1"), ]

      StopLook2DF <- observedHRDF[observedHRDF$Outcome %in% c("Efficacy2", "Futility2"), ]



      iterationList[[k]]$E1 <- mean(max(StopLook1DF[StopLook1DF$Outcome=="Efficacy1",]$D1), min(StopLook1DF[StopLook1DF$Outcome!="Efficacy1",]$D1))
      iterationList[[k]]$F1 <- mean(min(StopLook1DF[StopLook1DF$Outcome=="Futility1",]$D1), max(StopLook1DF[StopLook1DF$Outcome!="Futility1",]$D1))
      iterationList[[k]]$E2 <- mean(max(StopLook2DF[StopLook2DF$Outcome=="Efficacy2",]$D2), min(StopLook2DF[StopLook2DF$Outcome!="Efficacy2",]$D2))
      iterationList[[k]]$F2 <- mean(min(StopLook2DF[StopLook2DF$Outcome=="Futility2",]$D2), max(StopLook2DF[StopLook2DF$Outcome!="Futility2",]$D2))
      iterationList[[k]]$E3 <- mean(max(observedHRDF[observedHRDF$Outcome %in% c("Successful"),]$D3), min(observedHRDF[observedHRDF$Outcome %in% c("Unsuccessful"),]$D3))

    }

    reactValues$iterationList <- iterationList
    reactValues$TwoLooksSeq1 <- TwoLooksSeq1
    reactValues$TwoLooksSeq2 <- TwoLooksSeq2


    #Do proposed rule logic
    proposedDF$power <- proposedDF$Look1Power*proposedDF$Look2Power*proposedDF$FinalLookPower
    proposedDF$SS <- ifelse(proposedDF$Look1Power==0, proposedDF$Look1SS, ifelse(proposedDF$Look2Power==0,
                                                                                 proposedDF$Look2SS, proposedDF$FinalLookSS))
    proposedDF$Duration <- ifelse(proposedDF$Look1Power==0, proposedDF$Look1Duration, ifelse(proposedDF$Look2Power==0,
                                                                                             proposedDF$Look2Duration, proposedDF$FinalLookDuration))


    #Do Wieand rule logic
    WieandDF$power <- WieandDF$Look1Power*WieandDF$Look2Power*WieandDF$FinalLookPower
    WieandDF$SS <- ifelse(WieandDF$Look1Power==0, WieandDF$Look1SS, ifelse(WieandDF$Look2Power==0,
                                                                           WieandDF$Look2SS, WieandDF$FinalLookSS))
    WieandDF$Duration <- ifelse(WieandDF$Look1Power==0, WieandDF$Look1Duration, ifelse(WieandDF$Look2Power==0,
                                                                                       WieandDF$Look2Duration, WieandDF$FinalLookDuration))




    #Making the proposed DF correctly
    FinalProposedDF <- data.frame(Assurance = mean(proposedDF$power),
                                  Duration = mean(proposedDF$Duration),
                                  SS = mean(proposedDF$SS))

    colnames(FinalProposedDF) <- c("Assurance", "Duration", "Sample Size")

    #Making the Wieand DF correctly
    FinalWieandDF <- data.frame(Assurance = mean(WieandDF$power),
                                Duration = mean(WieandDF$Duration),
                                SS = mean(WieandDF$SS))

    colnames(FinalWieandDF) <- c("Assurance", "Duration", "Sample Size")


    FinalAss <- t(colMeans(noLooksDF))
    FinalAss <- as.data.frame(FinalAss)
    colnames(FinalAss) <- c("Assurance", "Duration", "Sample Size")

    data <- calculateTablemetrics(reactValues$iterationList,
                                  reactValues$TwoLooksSeq1,
                                  reactValues$TwoLooksSeq2)



    myDFLength <- sum(outer(TwoLooksSeq1, TwoLooksSeq2, "<"))

    IADFTwoLooks <- data.frame(IF1 = rep(NA, myDFLength), IF2 = rep(NA, myDFLength))

    myCount <- 1

    for (j in 1:length(TwoLooksSeq1)){
      for (k in 1:length(TwoLooksSeq2)){
        if (TwoLooksSeq1[j]<TwoLooksSeq2[k]){

          IADFTwoLooks[myCount,]$IF1 <- TwoLooksSeq1[j]
          IADFTwoLooks[myCount,]$IF2 <- TwoLooksSeq2[k]

          myCount <- myCount + 1

        }
      }
    }


    IADFTwoLooks$Power <- data$PowerArray
    IADFTwoLooks$Duration <- data$DurationArray
    IADFTwoLooks$SSArray <- data$SSArray
    IADFTwoLooks$IATime1 <-  data$IATime1
    IADFTwoLooks$IATime2 <- data$IATime2
    IADFTwoLooks$PercentStop <- data$PercentStop
    IADFTwoLooks$PercentStopLook1 <- data$PercentStopLook1
    IADFTwoLooks$PercentStopLook2 <- data$PercentStopLook2
    IADFTwoLooks$PercentStopLook1Fut <- data$PercentStopLook1Fut
    IADFTwoLooks$PercentStopLook2Fut <- data$PercentStopLook2Fut
    IADFTwoLooks$PercentStopLook1Eff <- data$PercentStopLook1Eff
    IADFTwoLooks$PercentStopLook2Eff <- data$PercentStopLook2Eff
    IADFTwoLooks$PercentStopEff <- data$PercentStopEff
    IADFTwoLooks$PercentStopFut <- data$PercentStopFut
    IADFTwoLooks$correctlyStopDF <- data$correctlyStopDF
    IADFTwoLooks$correctlyStopEffDF <- data$correctlyStopEffDF
    IADFTwoLooks$correctlyStopFutDF <- data$correctlyStopFutDF
    IADFTwoLooks$correctlyStopLook1DF <- data$correctlyStopLook1DF
    IADFTwoLooks$correctlyStopLook2DF <- data$correctlyStopLook2DF
    IADFTwoLooks$correctlyStopEffLook1DF <- data$correctlyStopEffLook1DF
    IADFTwoLooks$correctlyStopEffLook2DF <- data$correctlyStopEffLook2DF
    IADFTwoLooks$correctlyStopFutLook1DF <- data$correctlyStopFutLook1DF
    IADFTwoLooks$correctlyStopFutLook2DF <- data$correctlyStopFutLook2DF
    IADFTwoLooks$correctlyContinueDF <- data$correctlyContinueDF
    IADFTwoLooks$correctlyContinueLook1DF <- data$correctlyContinueLook1DF
    IADFTwoLooks$correctlyContinueLook2DF <- data$correctlyContinueLook2DF


    colnames(IADFTwoLooks) <- c("Information Fraction 1", "Information Fraction 2", "Assurance", "Duration", "Sample Size",
                                "Interim Analysis 1 Time", "Interim Analysis 2 Time",
                                "% Stop", "% Stop Look 1", "% Stop Look 2",
                                "% Stop Look 1 for Futility", "% Stop Look 2 for Futility",
                                "% Stop Look 1 for Efficacy", "% Stop Look 2 for Efficacy",
                                "% Stop for Efficacy", "% Stop for Futility",
                                "Correctly Stop", "Correctly Stop for Efficacy", "Correctly Stop for Futility",
                                "Correctly Stop at Look 1", "Correctly Stop at Look 2",
                                "Correctly Stop for Efficacy at Look 1", "Correctly Stop for Efficacy at Look 2",
                                "Correctly Stop for Futility at Look 1", "Correctly Stop for Futility at Look 2",
                                "Correctly Continue", "Correctly Continue at Look 1", "Correctly Continue at Look 2")

    return(list(IADFTwoLooks = IADFTwoLooks, FinalProposedDF = FinalProposedDF, FinalAss = FinalAss,
                iterationList = iterationList, FinalWieandDF = FinalWieandDF))


  })


  observeEvent(input$calcTwoLooks, {

    shinyjs::show("checkTwoLooks")

    shinyjs::show("selectedOptionsIATableTwoLooks")

    shinyjs::show("selectedOptionstwoLooks")

    updateSelectizeInput(session, "selectedOptionstwoLooks", choices = c("Assurance", "Duration", "Sample Size",
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
                         selected = c("Assurance", "Duration", "Sample Size"))

    twoLooksOutput <- twoLooksFunc()


    output$twoLooksObservedHRBoundaries <- renderPlotly({


      TwoLooksSeq1 <- seq(input$TwoLooksLB1, input$TwoLooksUB1, by = input$TwoLooksBy1)
      TwoLooksSeq2 <- seq(input$TwoLooksLB2, input$TwoLooksUB2, by = input$TwoLooksBy2)

      # Create a data frame of all combinations of TwoLooksSeq1 and TwoLooksSeq2
      combinations <- expand.grid(TwoLooksSeq1, TwoLooksSeq2)

      # Filter the combinations where the first element is less than the second
      valid_combinations <- combinations[combinations$Var1 < combinations$Var2, ]

      # Convert the valid combinations into the desired format
      seqChosen <- apply(valid_combinations, 1, function(x) paste(x, collapse = ", "))

      whichChosen <- which(seqChosen==input$twoLooksBoundaryIA)

      boundaryDFEff <- data.frame(IF = twoLooksOutput$iterationList[[whichChosen]]$IF,
                                  observedHR = c(twoLooksOutput$iterationList[[whichChosen]]$E1,
                                                 twoLooksOutput$iterationList[[whichChosen]]$E2,
                                                 twoLooksOutput$iterationList[[whichChosen]]$E3))

      boundaryDFFut <- data.frame(IF = twoLooksOutput$iterationList[[whichChosen]]$IF,
                                  observedHR = c(twoLooksOutput$iterationList[[whichChosen]]$F1,
                                                 twoLooksOutput$iterationList[[whichChosen]]$F2,
                                                 twoLooksOutput$iterationList[[whichChosen]]$E3))

      # Calculate dynamic y-axis limits
      all_observedHR <- c(boundaryDFEff$observedHR, boundaryDFFut$observedHR)
      ylim <- range(all_observedHR)

      # Extend the limits by 10% on each side
      buffer <- 0.1 * (ylim[2] - ylim[1])
      extended_ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

      # Create the plot using plotly
      p <- plot_ly() %>%
        add_trace(data = boundaryDFEff, x = ~IF, y = ~observedHR, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'red', width = 3),
                  marker = list(color = 'red', size = 10, symbol = 'circle'),
                  name = "Critical value") %>%
        add_trace(data = boundaryDFFut, x = ~IF, y = ~observedHR, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'blue', width = 3),
                  marker = list(color = 'blue', size = 10, symbol = 'circle'),
                  name = "Futility bound") %>%
        layout(yaxis = list(range = extended_ylim, title = "Observed HR"),
               title = "Boundaries",
               xaxis = list(title = "Information Fraction"))


      # Show the plot
      p

    })

    output$IATableTwoLooks <- renderDT({

      IADFTwoLooks <- subset(twoLooksOutput$IADFTwoLooks, select = c("Information Fraction 1", "Information Fraction 2", input$selectedOptionsIATableTwoLooks))

      datatable(IADFTwoLooks, options = list(rowCallback = JS(rowCallback)),
                rownames = F) %>% formatStyle(
                  columns = colnames(IADFTwoLooks)
                ) %>%
        formatSignif(
          columns = colnames(IADFTwoLooks),
          digits = 3
        )
    })

    output$noIATableTwoLooks <- renderTable({
      twoLooksOutput$FinalAss
    }, digits = 3)


    output$proposedTableTwoLooks <- renderTable({

      twoLooksOutput$FinalProposedDF

    }, digits = 3)

    output$wieandTableTwoLooks <- renderTable({

      twoLooksOutput$FinalWieandDF

    }, digits = 3)




    output$twoLooksPlotDuration <- renderPlotly({


      p <- plot_ly(twoLooksOutput$IADFTwoLooks, x = ~Assurance, y = ~Duration,
                   text = ~ paste0("Information Fraction = ", `Information Fraction 1`, ", ", `Information Fraction 2`), mode = "markers",
                   type = "scatter", marker = list(size = 10, color = "blue"), name = "Chosen Rules") %>%
        add_trace(x = ~twoLooksOutput$FinalAss$Assurance, y = ~twoLooksOutput$FinalAss$Duration, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "red"),
                  text = ~ "No Interim Analysis",
                  name = "No Interim Analysis") %>%
        add_trace(x = ~twoLooksOutput$FinalProposedDF$Assurance, y = ~twoLooksOutput$FinalProposedDF$Duration, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "green"),
                  text = ~ "Proposed Rule",
                  name = "Proposed Rule") %>%
        add_trace(x = ~twoLooksOutput$FinalWieandDF$Assurance, y = ~twoLooksOutput$FinalWieandDF$Duration, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "yellow"),
                  text = ~ "Wieand Rule",
                  name = "Wieand Rule") %>%
        layout(
          xaxis = list(title = list(text = "Assurance", font = list(color = "black", size = 14, family = "Arial", weight = "bold")), range = c(0, 1)),
          yaxis = list(title = list(text = "Duration", font = list(color = "black", size = 14, family = "Arial", weight = "bold"))),
          legend = list(orientation = "v", x = 1.05, y = 0.5),  # Position legend to the right
          title = "Assurance vs Duration for the different stopping rules"
        )

      p


    })


    output$twoLooksPlotSS <- renderPlotly({


      p <- plot_ly(twoLooksOutput$IADFTwoLooks, x = ~Assurance, y = ~`Sample Size`,
                   text = ~ paste0("Information Fraction = ", `Information Fraction 1`, ", ", `Information Fraction 2`), mode = "markers",
                   type = "scatter", marker = list(size = 10, color = "blue"), name = "Chosen Rules") %>%
        add_trace(x = ~twoLooksOutput$FinalAss$Assurance, y = ~twoLooksOutput$FinalAss$`Sample Size`, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "red"),
                  text = ~ "No Interim Analysis",
                  name = "No Interim Analysis") %>%
        add_trace(x = ~twoLooksOutput$FinalProposedDF$Assurance, y = ~twoLooksOutput$FinalProposedDF$`Sample Size`, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "green"),
                  text = ~ "Proposed Rule",
                  name = "Proposed Rule") %>%
        add_trace(x = ~twoLooksOutput$FinalWieandDF$Assurance, y = ~twoLooksOutput$FinalWieandDF$`Sample Size`, type = "scatter", mode = "markers",
                  marker = list(size = 10, color = "yellow"),
                  text = ~ "Wieand Rule",
                  name = "Wieand Rule") %>%
        layout(
          xaxis = list(title = list(text = "Assurance", font = list(color = "black", size = 14, family = "Arial", weight = "bold")), range = c(0, 1)),
          yaxis = list(title = list(text = "Sample Size", font = list(color = "black", size = 14, family = "Arial", weight = "bold"))),
          legend = list(orientation = "v", x = 1.05, y = 0.5),  # Position legend to the right
          title = "Assurance vs Sample Size for the different stopping rules"
        )

      p


    })

    output$finalAssTable2LooksText <- renderUI({
      p(HTML("<b>No Interim Analysis</b>"))
    })

    shinyjs::show("finalAssTable2LooksText")

    output$proposedTable2LooksText <- renderUI({
      p(HTML("<b>Proposed Rule</b>"))
    })

    shinyjs::show("proposedTable2LooksText")

    output$wieandTable2LooksText <- renderUI({
      p(HTML("<b>Wieand Rule</b>"))
    })

    shinyjs::show("wieandTable2LooksText")


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
                         spendingOneLook = input$spendingOneLook,
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
