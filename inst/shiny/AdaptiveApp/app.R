library(shiny)
library(shinyjs)
library(survival)
library(DT)
library(rhandsontable)
library(rpact)
library(dplyr)
library(plotly)
library(shinyBS)
#remotes::install_github("jamesalsbury/DTEAssurance")
library(DTEAssurance)
library(shinyAce)
library(tidyr)


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

table_choices <- c("% Stop", "% Stop for Efficacy", "% Stop for Futility")


default_table <- function(n) {
  data.frame(
    Stage = 1:n,
    IF = seq(1/n, 1, length.out = n),
    `α-spending` = seq(0.025/n, 0.025, length.out = n),
    `β-spending` = seq(0.1/n, 0.1, length.out = n),
    check.names = FALSE
  )
}


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
                   #fileInput("parametersFile", "Choose RDS File", accept = ".rds"),
                   h3("sample_size"),
                   fluidRow(
                     column(6,
                            numericInput("n_c", label =  HTML(paste0("n",tags$sub("c"))), value = 500)
                     ),
                     column(6,
                            numericInput("n_t", label =  HTML(paste0("n",tags$sub("t"))), value = 500)
                     )
                   ),
                   h3("control_model"),
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
                   h3("effect_model"),
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
                     ),


                   ),
                   fluidRow(
                     column(6,
                            numericInput("P_S", "Pr(survival curves separate)", value = 1, min = 0, max = 1)

                     ),
                     column(6,
                            numericInput("P_DTE", "Pr(treatment subject to a delay|survival curves separate)", value = 0, min = 0, max = 1)

                     )
                   ),
                   h3("recruitment_model"),
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
                   h3("GSD_model"),
                   fluidRow(
                     column(6,
                            numericInput("total_events", "Number of Events", value = 300)
                            ),
                     column(6,
                            numericInput("n_designs", "Number of Designs", value = 1, min = 1, max = 20)
                            )
                   ),
                   uiOutput("design_blocks"),
                   numericInput("n_sims", "Number of simulations", value=1000),
                   actionButton("calc_GSD_assurance", label  = "Calculate")
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
                   outputId = "display_func_simulate",
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
      var editorText = ace.edit('display_func_simulate').getValue();
      navigator.clipboard.writeText(editorText);
    });
  ")),

               tabsetPanel(
                 tabPanel("Tables",
                          hidden(selectizeInput("selected_columns_sim_table", "Metrics to View",
                                                choices = c("Assurance", "Duration", "Sample Size"), selected = c("Assurance", "Duration", "Sample Size"),
                                                multiple = TRUE)),
                          DTOutput("sim_table")),
                 tabPanel("Plots",
                          selectInput("Boundary_IA", "Choose the IF (to view)", choices = NULL),
                          plotlyOutput("boundary_plot"),
                          br(), br(),
                          plotOutput("Prop_Barchart"))
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
                   actionButton("calcBayesian", label  = "Calculate")
                 ),
                 mainPanel = mainPanel(
                   verbatimTextOutput("display_func_Bayesian"),
                   plotOutput("BayesianPlot")
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


  rds_data <- reactive({
    req(input$parametersFile)  # Ensure file is uploaded

    # Read the RDS file
    readRDS(input$parametersFile$datapath)
  })

  observe({
    req(rds_data())  # Ensure data is loaded

    inputData <- rds_data()

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


  # Store table data for each design
  tables <- reactiveValues()

  # Dynamic UI
  output$design_blocks <- renderUI({
    n <- input$n_designs
    blocks <- lapply(1:n, function(i) {
      fluidRow(
        column(3,
               numericInput(paste0("n_interims_", i),
                            "Stages",
                            value = 2, min = 1, max = 20)
        ),
        column(9,
               rHandsontableOutput(paste0("hot_", i))
        )
      )
    })
    do.call(tagList, blocks)
  })

  observe({
    n <- input$n_designs
    lapply(1:n, function(i) {
      output[[paste0("hot_", i)]] <- renderRHandsontable({
        n_interims <- input[[paste0("n_interims_", i)]]
        if (is.null(n_interims)) return(NULL)

        # If table doesn't exist yet, create it
        if (is.null(tables[[paste0("design_", i)]])) {
          tables[[paste0("design_", i)]] <- default_table(n_interims)
        } else {
          # Adjust rows if n_interims changed
          old_data <- tables[[paste0("design_", i)]]
          n_old <- nrow(old_data)
          if (n_interims > n_old) {
            new_rows <- default_table(n_interims)[(n_old+1):n_interims, ]
            old_data <- rbind(old_data, new_rows)
          } else if (n_interims < n_old) {
            old_data <- old_data[1:n_interims, ]
          }
          tables[[paste0("design_", i)]] <- old_data
        }

        rhandsontable(tables[[paste0("design_", i)]], rowHeaders = NULL) %>%
          hot_col("Stage", readOnly = TRUE)  # Stage column not editable
      })
    })
  })

  # Capture edits
  observe({
    n <- input$n_designs
    lapply(1:n, function(i) {
      observeEvent(input[[paste0("hot_", i)]], {
        tbl <- hot_to_r(input[[paste0("hot_", i)]])
        tables[[paste0("design_", i)]] <- tbl
      })
    })
  })

  observe({
    extracted_tables <- lapply(1:input$n_designs, function(i) tables[[paste0("design_", i)]])
    IF_vector <- sapply(extracted_tables, function(df) paste(df$IF, collapse = ", "))
    updateSelectInput(session, "Boundary_IA", choices = IF_vector)
  })

  observe({


    output$boundary_plot <- renderPlotly({

      extracted_tables <- lapply(1:input$n_designs, function(i) tables[[paste0("design_", i)]])
      alpha_vector <- sapply(extracted_tables, function(df) paste(df$`α-spending`, collapse = ", "))
      beta_vector <- sapply(extracted_tables, function(df) paste(df$`β-spending`, collapse = ", "))
      IF_vector <- sapply(extracted_tables, function(df) paste(df$IF, collapse = ", "))

      chosen_design <- which(input$Boundary_IA==IF_vector)

      design <- rpact::getDesignGroupSequential(
        typeOfDesign = "asUser",
        informationRates = as.numeric(trimws(unlist(strsplit(IF_vector[chosen_design], ",")))),
        userAlphaSpending = as.numeric(trimws(unlist(strsplit(alpha_vector[chosen_design], ",")))),
        typeBetaSpending = "bsUser",
        userBetaSpending = as.numeric(trimws(unlist(strsplit(beta_vector[chosen_design], ",")))))


         boundaryDFEff <- data.frame(IF = as.numeric(trimws(unlist(strsplit(IF_vector[chosen_design], ",")))),
                                  zStat = design$criticalValues)


        boundaryDFFut <- data.frame(IF = as.numeric(trimws(unlist(strsplit(IF_vector[chosen_design], ",")))),
                                zStat = c(design$futilityBounds, design$criticalValues[length(design$criticalValues)]))

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

  })


  function_call_simulate <- reactive({

    base_call <- paste0("calc_dte_assurance_interim(n_c = ",
                        input$n_c,
                        ", \n n_t = ",
                        input$n_t,
                        ", \n control_model = list(dist = \"",
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
                        "), \n effect_model = list(delay_SHELF = SHELF::fitdist(c(",
                        input$TValues,
                        "), probs = c(",
                        input$TProbs,
                        "), lower = ",
                        strsplit(input$TLimits, ", ")[[1]][1],
                        ", upper = ",
                        strsplit(input$TLimits, ", ")[[1]][2],
                        "), \n delay_time_dist = \"",
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
                        "), \n GSD_model = list(events = ",
                        input$total_events)



    extracted_tables <- lapply(1:input$n_designs, function(i) tables[[paste0("design_", i)]])

    alpha_vector <- sapply(extracted_tables, function(df) paste(df$`α-spending`, collapse = ", "))
    beta_vector <- sapply(extracted_tables, function(df) paste(df$`β-spending`, collapse = ", "))
    IF_vector <- sapply(extracted_tables, function(df) paste(df$IF, collapse = ", "))

    base_call <- paste0(base_call,
                        ", \n alpha_spending = c(",
                        paste0('"', alpha_vector, '"', collapse = ", "),
                        ")")


    base_call <- paste0(base_call,
                        ", \n beta_spending = c(",
                        paste0('"', beta_vector, '"', collapse = ", "),
                        ")")


    base_call <- paste0(base_call,
                        ", \n IF_vec = c(",
                        paste0('"', IF_vector, '"', collapse = ", "),
                        ")), \n n_sims = ",
                        input$n_sims,
                        ")")


    return(base_call)

  })

  observe({
    updateAceEditor(session, "display_func_simulate", value = function_call_simulate())
  })

  calculateGSDAssurance <- eventReactive(input$calc_GSD_assurance, {
    call_string <- function_call_simulate()
    result <- eval(parse(text = call_string))
    shinyjs::show("selected_columns_sim_table")
    shinyjs::show("selected_metrics_sim_plot")
    return(result)
  })


    output$sim_table <- renderDT({
      sim_output <- calculateGSDAssurance()

      design_summary <- sim_output %>%
        group_by(IF) %>%
        summarise(
          Assurance = mean(Decision %in% c("Stop for efficacy", "Successful at final")),
          `Average Duration` = round(mean(StopTime, na.rm = TRUE), 2),
          `Average Sample Size` = round(mean(SampleSize, na.rm = TRUE), 2),
          .groups = "drop"
        )

    })


    # output$sim_plot <- renderPlotly({
    #   sim_output <- calculateGSDAssurance()
    #
    #   if (input$number_of_looks>1){
    #     sim_output_DF <- do.call(rbind, lapply(sim_output, function(x) {
    #       data.frame(
    #         `Information Fraction` = paste(x$IF, collapse = ", "),
    #         Assurance = round(x$assurance_mean, 2),
    #         `Sample Size` = round(x$ss_mean, 1),
    #         Duration = round(x$duration_mean, 1),
    #         `Stop Early`= round(x$stop_mean, 2),
    #         `Stop Early for Efficacy` = round(x$eff_mean, 2),
    #         `Stop Early for Futility` = round(x$fut_mean, 2)
    #       )
    #     }))
    #
    #     colnames(sim_output_DF) <- c("Information Fraction", "Assurance", "Sample Size", "Duration",
    #                                  "% Stop", "% Stop for Efficacy", "% Stop for Futility")
    #
    #
    #     for (k in 1:length(sim_output)){
    #       num_IAs <- input$number_of_looks - 1
    #
    #       for (ia in 1:num_IAs){
    #         column_name <- paste("Timing_IA", ia, sep="")
    #
    #         sim_output_DF[[column_name]][k] <- mean(as.numeric(sapply(strsplit(sim_output[[k]]$IATimes, ", "), function(x) x[ia])))
    #       }
    #     }
    #
    #     sim_output_DF <- subset(sim_output_DF, select = c("Information Fraction", input$selected_metrics_sim_plot))
    #
    #     # Get the second and third column names
    #     x_col <- colnames(sim_output_DF)[2]
    #     y_col <- colnames(sim_output_DF)[3]
    #
    #     # Create the plot using the second and third columns
    #     plot_ly(sim_output_DF,
    #             x = ~get(x_col),  # Dynamically refer to the second column
    #             y = ~get(y_col),  # Dynamically refer to the third column
    #             type = "scatter",
    #             mode = "markers",
    #             marker = list(color = "blue", size = 8),
    #             text = ~`Information Fraction`,  # Hover text
    #             hoverinfo = "text") %>%
    #       layout(
    #         title = paste(x_col, "vs", y_col),
    #         xaxis = list(title = x_col),
    #         yaxis = list(title = y_col)
    #       )
    #
    #   } else {
    #
    #     sim_output_DF <- data.frame(
    #       Assurance = round(sim_output[[1]]$assurance, 2),
    #       `Sample Size` = round(sim_output[[1]]$sample_size, 1),
    #       Duration = round(sim_output[[1]]$duration, 1)
    #     )
    #
    #     colnames(sim_output_DF) <- c("Assurance", "Sample Size", "Duration")
    #
    #     sim_output_DF <- subset(sim_output_DF, select = input$selected_metrics_sim_plot)
    #
    #     # Get the second and third column names
    #     x_col <- colnames(sim_output_DF)[1]
    #     y_col <- colnames(sim_output_DF)[2]
    #
    #     # Create the plot using the second and third columns
    #     plot_ly(sim_output_DF,
    #             x = ~get(x_col),  # Dynamically refer to the second column
    #             y = ~get(y_col),  # Dynamically refer to the third column
    #             type = "scatter",
    #             mode = "markers",
    #             marker = list(color = "blue", size = 8)
    #             ) %>%
    #       layout(
    #         title = paste(x_col, "vs", y_col),
    #         xaxis = list(title = x_col),
    #         yaxis = list(title = y_col)
    #       )
    #
    #   }
    #
    # })
    #
    #
    output$Prop_Barchart <- renderPlot({
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
    sim_output <- calculateGSDAssurance()

    sim_output$Outcome <- with(sim_output, ifelse(
      Decision == "Stop for efficacy" & Final_Decision == "Successful", "Correctly stopped for Efficacy",
      ifelse(Decision == "Stop for efficacy" & Final_Decision == "Unsuccessful", "Incorrectly stopped for Efficacy",
             ifelse(Decision == "Stop for futility" & Final_Decision == "Successful", "Incorrectly stopped for Futility",
                    ifelse(Decision == "Stop for futility" & Final_Decision == "Unsuccessful", "Correctly stopped for Futility",
                           ifelse(Decision == "Successful at final" & Final_Decision == "Successful", "Successful at Final Analysis",
                                  # Everything else (including final boundary failed due to interim analyses) is treated as Unsuccessful
                                  "Unsuccessful at Final Analysis"
                           ))))))



      outcomes_decisions <- c("Successful at Final Analysis", "Correctly stopped for Efficacy",
                              "Incorrectly stopped for Efficacy", "Incorrectly stopped for Futility",
                              "Correctly stopped for Futility", "Unsuccessful at Final Analysis")

      sim_output$Outcome <- factor(sim_output$Outcome, levels = outcomes_decisions)

      outcome_decisions_colors <- setNames(c("green", "green", "green", "red", "red", "red"), outcomes_decisions)



      # Create matrix with all combinations
      IF_outcomes_mat <- sim_output %>%
        group_by(IF, Outcome) %>%
        summarise(n = n(), .groups = "drop") %>%
        complete(IF, Outcome = outcomes_decisions, fill = list(n = 0)) %>%
        group_by(IF) %>%
        mutate(Proportion = round(n / sum(n), 3)) %>%
        ungroup() %>%
        mutate(Outcome = factor(Outcome, levels = outcomes_decisions)) %>%
        arrange(Outcome) %>%
        select(-n) %>%
        pivot_wider(names_from = IF, values_from = Proportion, values_fill = 0)

      # Remove the Outcome column and convert to matrix
      numeric_mat <- as.matrix(IF_outcomes_mat[, -1])

      # Now apply sum to rows 1:3

      density_vals <- c(NA, 20, 50, 50, 20, NA)
      angle_vals <- c(0, 45, -45, -45, 45, 0)

      par(mar = c(5, 4, 4, 20))  # Increase right margin

      # Get bar heights to determine where to draw lines
      bar_heights <- apply(numeric_mat[1:3, , drop = F], 2, sum)


      bar_positions <- barplot(numeric_mat,
                               beside = FALSE,
                               width = 1,  # Optional, but explicit
                               col = outcome_decisions_colors[outcomes_decisions],
                               density = density_vals,
                               angle = angle_vals,
                               xlab = "Information Fraction",
                               ylab = "Proportion",
                               main = "Proportion of Trial Outcomes at Different Information Fractions")



      legend("topright",
             legend = c(rev(outcomes_decisions), "Assurance"),
             fill = c(rev(outcome_decisions_colors[outcomes_decisions]), NA),
             border = c(rep("black", length(outcomes_decisions)), NA),
             density = c(rev(density_vals), NA),
             angle = c(rev(angle_vals), NA),
             col = c(rep(NA, length(outcomes_decisions)), "black"),
             cex = 0.6,
             xpd = TRUE,
             inset = c(-0.4, 0),
             bty = "n")



      # Add black separator lines between top and bottom outcome segments
      segments(x0 = bar_positions - 0.5,  # left edge of each bar
               x1 = bar_positions + 0.5,  # right edge
               y0 = bar_heights,          # y position (cumulative height of top 3)
               y1 = bar_heights,
               col = "black",
               lwd = 5)

      # Add text labels just below the separator line
      text(x = bar_positions,
           y = bar_heights,  # Adjust this for spacing
           labels = round(bar_heights, 2),
           cex = 1,                # text size
           pos = 1)                  # below the specified y (1 = below)


    })


  # Bayesian Logic ---------------------------------

    # calc_BPP_hist <- function(n_c, n_t,
    #                           control_dist = "Exponential",
    #                           t1 = NULL, t2 = NULL,
    #                           t1_Beta_a = NULL, t1_Beta_b = NULL,
    #                           diff_Beta_a = NULL, diff_Beta_b = NULL,
    #                           delay_time_SHELF, delay_time_dist = "hist",
    #                           post_delay_HR_SHELF, post_delay_HR_dist = "hist",
    #                           P_S = 1, P_DTE = 0, cens_events = NULL, IF = NULL,
    #                           rec_method, rec_period=NULL, rec_power=NULL, rec_rate=NULL, rec_duration=NULL,
    #                           type_one_error = NULL, N = 50, M = 50)


      function_call_Bayesian <- reactive({
        n_c <- (input$numofpatients*input$ControlRatio)/(sum(input$ControlRatio+input$TreatmentRatio))
        n_t <- (input$numofpatients*input$TreatmentRatio)/(sum(input$ControlRatio+input$TreatmentRatio))



        base_call <- paste0("calc_BPP_hist(n_c = ",
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
                            input$number_of_looks)



        base_call <- paste0(base_call, ")")

        return(base_call)

      })

    output$display_func_Bayesian <- renderText({
      function_call_Bayesian()
    })

    calculate_Bayesian <- eventReactive(input$calcBayesian, {
      call_string <- function_call_Bayesian()
      result <- eval(parse(text = call_string))
      return(result)
    })

    output$BayesianPlot <- renderPlot({
      calculate_Bayesian()
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
                         oneLookBoundary_IA = input$oneLookBoundary_IA,
                         oneLookFunc = oneLookFunc()))

      }

      if (input$checkTwoLooks) {

        params <- c(params,
                    list(checkTwoLooksOptionsTables = input$checkTwoLooksOptionsTables,
                         checkTwoLooksOptionsPlots = input$checkTwoLooksOptionsPlots,
                         spendingTwoLooks = input$spendingTwoLooks,
                         twoLooksBoundary_IA = input$twoLooksBoundary_IA,
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
