
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
  titlePanel("Adaptive Design: Delayed Treatment Effects - Elicited Distributions"),

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
                       selectInput("ExpRateorTime", "Input type", choices = c("Parameters", "Landmark")),
                       conditionalPanel(
                         condition = "input.ExpRateorTime == 'Parameters'",
                         numericInput("ExpRate", label =  HTML(paste0("Rate (\u03bb",tags$sub("c"), ")")), value = 0.08, min=0),
                         shinyBS::bsTooltip(id = "ExpRate", title = "Rate parameter")
                       ),
                       conditionalPanel(
                         condition = "input.ExpRateorTime == 'Landmark'",
                         fluidRow(
                           column(6,
                                  numericInput("ExpTime", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                  shinyBS::bsTooltip(id = "ExpTime", title = "Time 1")

                           ),
                           column(6,
                                  numericInput("ExpSurv", label =  HTML(paste0("S(t",tags$sub("1"), ")")), 0.5),
                                  shinyBS::bsTooltip(id = "ExpSurv", title = "Survival probability at time 1")


                           )
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.ExpChoice == 'Distribution'",
                       numericInput("ExpSurvTime", label = HTML(paste0("t",tags$sub("1"))), value = 12),
                       shinyBS::bsTooltip(id = "ExpSurvTime", title = "Time 1"),
                       fluidRow(
                         column(4,
                                uiOutput("ExpDistText"),
                                shinyBS::bsTooltip(id = "ExpDistText", title = "The distribution of survival probability at time 1"),
                         ),
                         column(8,
                                div(style = "display: flex; align-items: center;",
                                    numericInput("ExpBetaA", label = NULL, value = 20, width = "45%"),
                                    shinyBS::bsTooltip(id = "ExpBetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                    tags$span(", "),
                                    numericInput("ExpBetaB", label = NULL, value = 32, width = "45%"),
                                    shinyBS::bsTooltip(id = "ExpBetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
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
                                  shinyBS::bsTooltip(id = "WeibullScale", title = "Scale parameter"),

                           ),
                           column(6,
                                  numericInput("WeibullShape", label =  HTML(paste0("Shape (\u03b3",tags$sub("c"), ")")), value = 1, min=0),
                                  shinyBS::bsTooltip(id = "WeibullShape", title = "Shape parameter"),

                           )
                         )
                       ),
                       conditionalPanel(
                         condition = "input.WeibRateorTime == 'Landmark'",
                         fluidRow(
                           column(6,
                                  numericInput("WeibullTime1", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                  shinyBS::bsTooltip(id = "WeibullTime1", title = "Time 1")
                           ),
                           column(6,
                                  numericInput("WeibullSurv1", label =  HTML(paste0("S(t",tags$sub("1"), ")")), value = 0.38),
                                  shinyBS::bsTooltip(id = "WeibullSurv1", title = "Survival Probability at Time 1")
                           )
                         ),
                         fluidRow(
                           column(6,
                                  numericInput("WeibullTime2", label =  HTML(paste0("t",tags$sub("2"))), value = 18),
                                  shinyBS::bsTooltip(id = "WeibullTime2", title = "Time 2")

                           ),
                           column(6,
                                  numericInput("WeibullSurv2", label =  HTML(paste0("S(t",tags$sub("2"), ")")), value = 0.24),
                                  shinyBS::bsTooltip(id = "WeibullSurv2", title = "Survival Probability at Time 2")
                           )
                         ),
                       )
                     ),

                     conditionalPanel(
                       condition = "input.WeibullChoice == 'Distribution'",
                       fluidRow(
                         column(6,
                                numericInput("WeibullDistT1", label =  HTML(paste0("t",tags$sub("1"))), value = 12),
                                shinyBS::bsTooltip(id = "WeibullDistT1", title = "Time 1"),


                         ),
                         column(6,
                                numericInput("WeibullDistT2", label =  HTML(paste0("t",tags$sub("2"))), value = 18),
                                shinyBS::bsTooltip(id = "WeibullDistT2", title = "Time 2"),

                         )
                       ),
                       fluidRow(
                         column(4,
                                uiOutput("WeibullDistS1Text"),
                                shinyBS::bsTooltip(id = "WeibullDistS1Text", title = "The distribution of survival probability at time 1"),
                         ),
                         column(8,
                                div(style = "display: flex; align-items: center;",
                                    numericInput("WeibullDistS1BetaA", label = NULL, value = 20, width = "45%"),
                                    shinyBS::bsTooltip(id = "WeibullDistS1BetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                    tags$span(", "),
                                    numericInput("WeibullDistS1BetaB", label = NULL, value = 32, width = "45%"),
                                    shinyBS::bsTooltip(id = "WeibullDistS1BetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
                                    tags$span(")")
                                )
                         )
                       ),
                       fluidRow(
                         column(4,
                                uiOutput("WeibullDistDelta1Text"),
                                shinyBS::bsTooltip(id = "WeibullDistDelta1Text", title = "The distribution of the difference in survival probabilities between time 2 and time 1"),

                         ),
                         column(8,
                                div(style = "display: flex; align-items: center;",
                                    numericInput("WeibullDistDelta1BetaA", label = NULL, value = 20, width = "45%"),
                                    shinyBS::bsTooltip(id = "WeibullDistDelta1BetaA", title = "The mean of a Beta(a, b) distribution is a/(a+b)"),
                                    tags$span(", "),
                                    numericInput("WeibullDistDelta1BetaB", label = NULL, value = 140, width = "45%"),
                                    shinyBS::bsTooltip(id = "WeibullDistDelta1BetaB", title = "The variance of a Beta(a, b) distribution is (a*b)/[(a+b)^2*(a+b+1)]"),
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
                   numericInput("total_events_simulate", "Number of Events", value = 300),
                   fluidRow(
                     column(4,
                            numericInput("n_stages_alpha", "Number of Stages", value = 2)
                     ),
                     column(8,
                            rhandsontable::rHandsontableOutput("alpha_spending_table"),
                     )
                   ),
                   selectInput("fut_method", "Type of Futility", choices = c("None"="none", "Beta-Spending"="Beta", "BPP" = "BPP"), selected = "none"),
                   conditionalPanel(
                     condition = "input.fut_method == 'Beta'",
                     fluidRow(
                       column(4,
                              numericInput("n_stages_beta", "Number of Stages", value = 2)
                              ),
                       column(8,
                              rhandsontable::rHandsontableOutput("beta_spending_table"),
                       ),
                     )
                   ),
                   conditionalPanel(
                     condition = "input.fut_method == 'BPP'",
                     fluidRow(
                       column(6,
                              numericInput("BPP_timing", label =  "BPP timing", value = 0.5)
                       ),
                       column(6,
                              numericInput("BPP_threshold", label =  "BPP threshold", value = 0.2)
                       )
                     )
                   ),
                   numericInput("n_sims_simulate", "Number of simulations", value=1000),
                   actionButton("calc_GSD_assurance", label  = "Calculate")
                 ),
                 mainPanel = mainPanel(
                   tags$h4(
                     id = "toggleHeader_simulate",
                     style = "cursor: pointer; display: flex; align-items: center; justify-content: space-between;",
                     div(
                       style = "display: flex; align-items: center;",
                       tags$span(id = "arrow_simulate", "►"),  # Arrow icon
                       " Show/hide the function"
                     ),
                 actionButton(
                   inputId = "copy_btn_simulate",
                   label = "📋 Copy",
                   class = "btn btn-sm btn-outline-primary"
                 )
               ),

               tags$div(
                 id = "collapseText_simulate",
                 class = "collapse",
                 shinyAce::aceEditor(
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
    $(document).on('click', '#toggleHeader_simulate', function(e) {
      if (!$(e.target).is('#copy_btn_simulate')) {   // avoid toggle when clicking copy
        $('#collapseText_simulate').collapse('toggle');
        var arrow = $('#arrow_simulate');
        if (arrow.text() == '►') {
          arrow.text('▼');
        } else {
          arrow.text('►');
        }
      }
    });

    // Copy-to-clipboard
    $(document).on('click', '#copy_btn_simulate', function() {
      var editorText = ace.edit('display_func_simulate').getValue();
      navigator.clipboard.writeText(editorText);
    });
  ")),

               tabsetPanel(
                 tabPanel("Tables",
                          tableOutput("sim_table")),
                 tabPanel("Plots",
                          plotly::plotlyOutput("boundary_plot"))
               ),
      )
               )
      ),
      # BPP - Timing UI ---------------------------------

      tabPanel("BPP - Timing",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fluidRow(
                     column(6,
                            numericInput("total_events_BPP_timing", "Number of Events", value = 300)
                            ),
                    column(6,
                           numericInput("BPP_Timing_IF", "IF timing to look at", value = 0.5)
                           )
                   ),
                   numericInput("n_sims_BPP_Timing", "Number of simulations", value=10),
                   actionButton("calc_BPP_Timing", label  = "Calculate"),
                   shinyjs::hidden(numericInput("n_breaks_BPP_timing", "Number of bins", value=10)),
                   fluidRow(
                            column(6,
                                   shinyjs::hidden(numericInput("lower_bound_BPP_timing", "Lower bound", value=0.1))
                            ),
                            column(6,
                                   shinyjs::hidden(numericInput("upper_bound_BPP_timing", "Upper bound", value=0.9))
                            )
                            )

                 ),
                 mainPanel = mainPanel(
                   tags$h4(
                     id = "toggleHeader_BPP_Timing",
                     style = "cursor: pointer; display: flex; align-items: center; justify-content: space-between;",
                     div(
                       style = "display: flex; align-items: center;",
                       tags$span(id = "arrow_BPP_Timing", "►"),  # Arrow icon
                       " Show/hide the function"
                     ),
                     actionButton(
                       inputId = "copy_btn_BPP_Timing",
                       label = "📋 Copy",
                       class = "btn btn-sm btn-outline-primary"
                     )
                   ),

                   tags$div(
                     id = "collapseText_BPP_Timing",
                     class = "collapse",
                     shinyAce::aceEditor(
                       outputId = "display_func_BPP_Timing",
                       value = "",
                       mode = "r",
                       theme = "monokai",
                       readOnly = TRUE,
                       height = "400px"
                     )
                   ),

                   tags$script(HTML("
    // Toggle collapse + arrow
    $(document).on('click', '#toggleHeader_BPP_Timing', function(e) {
      if (!$(e.target).is('#copy_btn_BPP_Timing')) {   // avoid toggle when clicking copy
        $('#collapseText_BPP_Timing').collapse('toggle');
        var arrow = $('#arrow_BPP_Timing');
        if (arrow.text() == '►') {
          arrow.text('▼');
        } else {
          arrow.text('►');
        }
      }
    });

    // Copy-to-clipboard
    $(document).on('click', '#copy_btn_BPP_Timing', function() {
      var editorText = ace.edit('display_func_BPP_Timing').getValue();
      navigator.clipboard.writeText(editorText);
    });
  ")),

                   plotOutput("BPP_timing_hist"),
                   textOutput("BPP_Timing_text")




                 )
               )
      ),

      # BPP - Threshold UI ---------------------------------

      tabPanel("BPP - Threshold",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fluidRow(
                     column(6,
                            numericInput("total_events_BPP_threshold", "Number of Events", value = 300)
                     ),
                     column(6,
                            numericInput("BPP_Threshold_IF", "IF timing to look at", value = 0.5)
                     )
                   ),
                   fluidRow(
                     column(
                       width = 4,
                       numericInput(
                         inputId = "lambda_c_threshold",
                         label = HTML("&lambda;<sub>c</sub>"),
                         value = 0.1
                       )
                     ),
                     column(
                       width = 4,
                       numericInput(
                         inputId = "delay_time_threshold",
                         label = "Delay Time",
                         value = 3
                       )
                     ),
                     column(
                       width = 4,
                       numericInput(
                         inputId = "post_delay_hr_threshold",
                         label = HTML("Post-Delay HR"),
                         value = 0.75
                       )
                     )
                   ),
                   numericInput("n_sims_BPP_Threshold", "Number of simulations", value=10),
                   actionButton("calc_BPP_Threshold", label  = "Calculate"),
                   fluidRow(
                     column(6,
                            shinyjs::hidden(numericInput("n_breaks_BPP_threshold", "Number of bins", value=10))
                            ),
                     column(6,
                            shinyjs::hidden(numericInput("threshold_value", "Threshold Value", value=0.2))
                            )

                   )

                 ),
                 mainPanel = mainPanel(
                   tags$h4(
                     id = "toggleHeader_BPP_Threshold",
                     style = "cursor: pointer; display: flex; align-items: center; justify-content: space-between;",
                     div(
                       style = "display: flex; align-items: center;",
                       tags$span(id = "arrow_BPP_Threshold", "►"),  # Arrow icon
                       " Show/hide the function"
                     ),
                     actionButton(
                       inputId = "copy_btn_BPP_Threshold",
                       label = "📋 Copy",
                       class = "btn btn-sm btn-outline-primary"
                     )
                   ),

                   tags$div(
                     id = "collapseText_BPP_Threshold",
                     class = "collapse",
                     shinyAce::aceEditor(
                       outputId = "display_func_BPP_Threshold",
                       value = "",
                       mode = "r",
                       theme = "monokai",
                       readOnly = TRUE,
                       height = "400px"
                     )
                   ),

                   tags$script(HTML("
    // Toggle collapse + arrow
    $(document).on('click', '#toggleHeader_BPP_Threshold', function(e) {
      if (!$(e.target).is('#copy_btn_BPP_Threshold')) {   // avoid toggle when clicking copy
        $('#collapseText_BPP_Threshold').collapse('toggle');
        var arrow = $('#arrow_BPP_Threshold');
        if (arrow.text() == '►') {
          arrow.text('▼');
        } else {
          arrow.text('►');
        }
      }
    });

    // Copy-to-clipboard
    $(document).on('click', '#copy_btn_BPP_Threshold', function() {
      var editorText = ace.edit('display_func_BPP_Threshold').getValue();
      navigator.clipboard.writeText(editorText);
    });
  ")),
                   plotOutput("BPP_Threshold_hist"),
                   textOutput("BPP_Threshold_text")




                 )
               )
      ),


      # Report UI ---------------------------------

      tabPanel("Report",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fluidRow(
                     column(4, shinyjs::hidden(checkboxInput("checkDesign", "Design", value = FALSE))),
                     column(4, shinyjs::hidden(selectizeInput("checkDesignOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c(""),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, shinyjs::hidden(selectizeInput("checkDesignOptionsPlots", "Selected Plots",
                                                     choices = c("Elicited T", "Elicited post-delay HR"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, shinyjs::hidden(checkboxInput("checkNoLook", "No Look", value = FALSE))),
                     column(4, shinyjs::hidden(selectizeInput("checkNoLookOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c("Assurance OC"),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, shinyjs::hidden(selectizeInput("checkNoLookOptionsPlots", "Selected Plots",
                                                     choices = c("Assurance Plot"),
                                                     selected = c(),
                                                     multiple = TRUE)))

                   ),

                   fluidRow(
                     column(4, shinyjs::hidden(checkboxInput("checkOneLook", "One Look", value = FALSE))),
                     column(4, shinyjs::hidden(selectizeInput("checkOneLookOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c("Interim Analysis Time", "Assurance", "Duration", "Sample Size",
                                                                 "% Stop", "% Stop for Efficacy", "% Stop for Futility",
                                                                 "% Correctly Stop", "% Correctly Stop for Efficacy", "% Correctly Stop for Futility",
                                                                 "% Correctly Continue"),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, shinyjs::hidden(selectizeInput("checkOneLookOptionsPlots", "Selected Plots",
                                                     choices = c("Boundary Plot", "Assurance vs Duration", "Assurance vs Sample Size"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, shinyjs::hidden(checkboxInput("checkTwoLooks", "Two Looks", value = FALSE))),
                     column(4, shinyjs::hidden(selectizeInput("checkTwoLooksOptionsTables", "Selected (Metrics) Tables",
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
                     column(4, shinyjs::hidden(selectizeInput("checkTwoLooksOptionsPlots", "Selected Plots",
                                                     choices = c("Boundary Plot", "Assurance vs Duration", "Assurance vs Sample Size"),
                                                     selected = c(""),
                                                     multiple = TRUE)))
                   ),
                   fluidRow(
                     column(4, shinyjs::hidden(checkboxInput("checkBayesian", "Bayesian", value = FALSE))),
                     column(4, shinyjs::hidden(selectizeInput("checkBayesianOptionsTables", "Selected (Metrics) Tables",
                                                     choices = c(""),
                                                     selected = c(""),
                                                     multiple = TRUE))),
                     column(4, shinyjs::hidden(selectizeInput("checkBayesianOptionsPlots", "Selected Plots",
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
        if (inputData$fixed_parameters_type=="Parameters"){
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

    output$alpha_spending_table <- rhandsontable::renderRHandsontable({

    req(input$n_stages_alpha)
    k <- input$n_stages_alpha
        df <- data.frame(
          Stage          = 1:k,
          IF             = seq_len(k) / k,
          alpha.spending = (1:k)/k * 0.025,
          check.names    = FALSE
        )

        rhandsontable::rhandsontable(df, rowHeaders = FALSE) %>%
          rhandsontable::hot_col("Stage", readOnly = TRUE) %>%
          rhandsontable::hot_col("IF", type = "numeric", format = "0.00") %>%
          rhandsontable::hot_col("alpha.spending", type = "numeric", format = "0.0000")

    })

  output$beta_spending_table <- rhandsontable::renderRHandsontable({

    req(input$n_stages_beta)
    k <- input$n_stages_beta
    df <- data.frame(
      Stage          = 1:k,
      IF             = seq_len(k) / k,
      beta.spending = (1:k)/k * 0.1,
      check.names    = FALSE
    )

    rhandsontable::rhandsontable(df, rowHeaders = FALSE) %>%
      rhandsontable::hot_col("Stage", readOnly = TRUE) %>%
      rhandsontable::hot_col("IF", type = "numeric", format = "0.00") %>%
      rhandsontable::hot_col("beta.spending", type = "numeric", format = "0.0000")

  })



  observe({


    output$boundary_plot <- plotly::renderPlotly({

      df <- rhandsontable::hot_to_r(input$alpha_spending_table)
      alpha_vector <- df$`alpha.spending`
      IF_vector <- df$IF

      if (input$fut_method %in% c("none", "BPP")){
        design <- rpact::getDesignGroupSequential(
          typeOfDesign = "asUser",
          informationRates = IF_vector,
          userAlphaSpending = alpha_vector,
          typeBetaSpending = "none")

      efficacy_boundary <- data.frame(IF = IF_vector,
                                    Z_Stat = design$criticalValues)

      # Calculate dynamic y-axis limits
      full_Z_Stat <- c(0, efficacy_boundary$Z_Stat)
      ylim <- range(full_Z_Stat)

      # Extend the limits by 10% on each side
      buffer <- 0.1 * (ylim[2] - ylim[1])
      extended_ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

      # Create the plot using plotly
      p <- plot_ly() %>%
        add_trace(data = efficacy_boundary, x = ~IF, y = ~Z_Stat, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'red', width = 3),
                  marker = list(color = 'red', size = 10, symbol = 'circle'),
                  name = "Critical value") %>%
        layout(yaxis = list(range = extended_ylim, title = "Futility Bound and Critical Value"),
               title = "Boundaries",
               xaxis = list(title = "Information Fraction"))


      # Show the plot
      p

      } else if (input$fut_method  == "Beta"){

        df_alpha <- rhandsontable::hot_to_r(input$alpha_spending_table)
        alpha_vec <- df_alpha$`alpha.spending`
        eff_IF <- df_alpha$IF

        df_beta <- rhandsontable::hot_to_r(input$beta_spending_table)
        beta_vec <- df_beta$`beta.spending`
        fut_IF <- df_beta$IF




        eff_design <- rpact::getDesignGroupSequential(
          typeOfDesign      = "asUser",
          informationRates  = eff_IF,
          userAlphaSpending = alpha_vec,
          typeBetaSpending  = "none"
        )

        eff_df <- data.frame(
          IF     = eff_IF,
          Z_Stat = eff_design$criticalValues
        )



        fut_design <- rpact::getDesignGroupSequential(
          typeOfDesign      = "noEarlyEfficacy",
          informationRates  = fut_IF,
          typeBetaSpending  = "bsUser",
          userBetaSpending  = beta_vec
        )


        fut_length <- length(fut_IF)

        fut_df <- data.frame(
          IF     = numeric(fut_length),
          Z_Stat = numeric(fut_length)
        )


        fut_df$IF <- fut_IF
        fut_df$Z_Stat[1:(fut_length-1)] <- fut_design$futilityBounds

        fut_df$Z_Stat[fut_length] <-  eff_design$criticalValues[length(eff_design$criticalValues)]



        all_vals <- c(0, eff_df$Z_Stat, fut_df$Z_Stat)
        plot_range <- range(all_vals)

        margin <- 0.1 * diff(plot_range)
        extended_ylim <- c(plot_range[1] - margin, plot_range[2] + margin)


        p <- plot_ly() %>%
          add_trace(
            data = eff_df, x = ~IF, y = ~Z_Stat,
            type = "scatter", mode = "lines+markers",
            line = list(color = "red", width = 3),
            marker = list(color = "red", size = 10),
            name = "Efficacy Boundary"
          ) %>%
          add_trace(
            data = fut_df, x = ~IF, y = ~Z_Stat,
            type = "scatter", mode = "lines+markers",
            line = list(color = "blue", width = 3, dash = "dash"),
            marker = list(color = "blue", size = 10, symbol = "square"),
            name = "Futility Boundary (Beta Spending)"
          ) %>%
          layout(
            yaxis = list(range = extended_ylim, title = "Z-Value Boundaries"),
            xaxis = list(title = "Information Fraction"),
            title = "Efficacy and Futility Boundaries (Distinct IFs)"
          )

        p


}



      })

  })


  function_call_simulate <- reactive({

    base_call <- paste0(
      "calc_dte_assurance_adaptive(n_c = ",
      input$n_c,
      ", \n n_t = ",
      input$n_t,
      ", \n control_model = list(\n   dist = \"",
      input$ControlDist,
      "\""
    )

    if (input$ControlDist=="Exponential") {
      base_call <- paste0(
        base_call,
        ", \n   parameter_mode = \"",
        input$ExpChoice,
        "\""
      )

      if (input$ExpChoice=="Fixed") {
        base_call <- paste0(
          base_call,
          ", \n   fixed_type = \"",
          input$ExpRateorTime,
          "\""
        )

        if (input$ExpRateorTime == "Parameters") {
          base_call <- paste0(
            base_call,
            ", \n   lambda = ",
            input$ExpRate
          )
        } else if (input$ExpRateorTime == "Landmark") {
          base_call <- paste0(
            base_call,
            ", \n   t1 = ",
            input$ExpTime,
            ", \n   surv_t1 = ",
            input$ExpSurv
          )
        }
      }

      if (input$ExpChoice=="Distribution") {
        base_call <- paste0(
          base_call,
          ", \n   t1 = ",
          input$ExpSurvTime,
          ", \n   t1_Beta_a = ",
          input$ExpBetaA,
          ", \n   t1_Beta_b = ",
          input$ExpBetaB
        )
      }
    }

    if (input$ControlDist == "Weibull") {

      base_call <- paste0(
        base_call,
        ", \n   control_parameters = \"",
        input$WeibullChoice,
        "\""
      )

      if (input$WeibullChoice == "Fixed") {

        base_call <- paste0(
          base_call,
          ", \n   fixed_parameters_type = \"",
          input$WeibRateorTime,
          "\""
        )

        if (input$WeibRateorTime == "Parameters") {
          base_call <- paste0(
            base_call,
            ", \n   lambda_c = ",
            input$WeibullScale,
            ", \n   gamma_c = ",
            input$WeibullShape
          )
        } else if (input$WeibRateorTime == "Landmark") {
          base_call <- paste0(
            base_call,
            ", \n   t1 = ",
            input$WeibullTime1,
            ", \n   t2 = ",
            input$WeibullSurv1,
            ", \n   surv_t1 = ",
            input$WeibullTime2,
            ", \n   surv_t2 = ",
            input$WeibullSurv2
          )
        }

      } else if (input$WeibullChoice == "Distribution") {
        base_call <- paste0(
          base_call,
          ", \n   t1 = ",
          input$WeibullDistT1,
          ", \n   t2 = ",
          input$WeibullDistT2,
          ", \n   t1_Beta_a = ",
          input$WeibullDistS1BetaA,
          ", \n   t1_Beta_b = ",
          input$WeibullDistS1BetaB,
          ", \n   diff_Beta_a = ",
          input$WeibullDistDelta1BetaA,
          ", \n   diff_Beta_b = ",
          input$WeibullDistDelta1BetaB
        )
      }
    }

    # Close control model and open effect model
    base_call <- paste0(
      base_call,
      "), \n effect_model = list(\n   delay_SHELF = SHELF::fitdist(c(",
      input$TValues,
      "), probs = c(",
      input$TProbs,
      "), lower = ",
      strsplit(input$TLimits, ", ")[[1]][1],
      ", upper = ",
      strsplit(input$TLimits, ", ")[[1]][2],
      "), \n   delay_dist = \"",
      input$TDist,
      "\", \n   HR_SHELF = SHELF::fitdist(c(",
      input$HRValues,
      "), probs = c(",
      input$HRProbs,
      "), lower = ",
      strsplit(input$HRLimits, ", ")[[1]][1],
      ", upper = ",
      strsplit(input$HRLimits, ", ")[[1]][2],
      "), \n   HR_dist = \"",
      input$HRDist,
      "\", \n   P_S = ",
      input$P_S,
      ", \n   P_DTE = ",
      input$P_DTE,
      ")"
    )

    # recruitment_model
    base_call <- paste0(
      base_call,
      ", \n recruitment_model = list(\n   method = \"",
      input$rec_method,
      "\""
    )

    if (input$rec_method == "power") {
      base_call <- paste0(
        base_call,
        ", \n   period = ",
        input$rec_period,
        ", \n   power = ",
        input$rec_power
      )
    }

    if (input$rec_method == "PWC") {
      base_call <- paste0(
        base_call,
        ", \n   rate = ",
        input$rec_rate,
        ", \n   duration = ",
        input$rec_duration
      )
    }

    base_call <- paste0(base_call, "\n )")

    # GSD_model
    base_call <- paste0(
      base_call,
      ", \n GSD_model = list(\n   events = ",
      input$total_events_simulate
    )

    alpha_df <- rhandsontable::hot_to_r(input$alpha_spending_table)
    alpha_vector <- alpha_df$`alpha.spending`
    alpha_IF <- alpha_df$IF

    base_call <- paste0(
      base_call,
      ", \n   alpha_spending = c(",
      paste(alpha_vector, collapse = ", "),
      ")"
    )

    base_call <- paste0(
      base_call,
      ", \n   alpha_IF = c(",
      paste(alpha_IF, collapse = ", "),
      ")"
    )

    base_call <- paste0(
      base_call,
      ", \n   futility_type = \"",
      input$fut_method,
      "\""
    )

    # futility options
    if (input$fut_method == "Beta") {
      beta_df <- rhandsontable::hot_to_r(input$beta_spending_table)
      beta_IF <- beta_df$IF
      beta_vector <- beta_df$`beta.spending`

      base_call <- paste0(
        base_call,
        ", \n   futility_IF = c(",
        paste(beta_IF, collapse = ", "),
        ")"
      )

      base_call <- paste0(
        base_call,
        ", \n   beta_spending = c(",
        paste(beta_vector, collapse = ", "),
        ")"
      )
    }

    if (input$fut_method == "BPP") {

      base_call <- paste0(
        base_call,
        ", \n   futility_IF = ",
        input$BPP_timing
      )

      base_call <- paste0(
        base_call,
        ", \n   BPP_threshold = ",
        input$BPP_threshold
      )

      base_call <- paste0(
        base_call,
        "\n ), \n analysis_model = list(\n",
        "   method = \"LRT\",\n",
        "   alternative_hypothesis = \"one.sided\",\n",
        "   alpha = ",
        alpha_vector[length(alpha_vector)]
      )
    }

    # Close GSD_model and finish
    base_call <- paste0(
      base_call,
      "\n ), \n n_sims = ",
      input$n_sims_simulate,
      ")"
    )

    return(base_call)

  })

  observe({
    shinyAce::updateAceEditor(session, "display_func_simulate", value = function_call_simulate())
  })

  calculate_assurance_interim <- eventReactive(input$calc_GSD_assurance, {
    call_string <- function_call_simulate()
    result <- eval(parse(text = call_string))
    shinyjs::show("selected_metrics_sim_plot")
    return(result)
  })


    output$sim_table <- renderTable({
      sim_output <- calculate_assurance_interim()

      print(sim_output)

      design_summary <- sim_output %>%
        dplyr::summarise(
          Assurance = mean(Decision %in% c("Stop for efficacy", "Successful at final")),
          `Pr(Early Fut.)` = mean(Decision %in% c("Stop for futility")),
          `Pr(Early Eff.)` = mean(Decision %in% c("Stop for efficacy")),
          `Average Duration` = round(mean(StopTime, na.rm = TRUE), 2),
          `Average Sample Size` = round(mean(SampleSize, na.rm = TRUE), 2)
        )

    })


    # output$sim_plot <- plotly::renderPlotly({
    #   sim_output <- calculate_assurance_interim()
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
    # output$Prop_Barchart <- renderPlot({
    #   oldpar <- par(no.readonly = TRUE)
    #   on.exit(par(oldpar))
    # sim_output <- calculate_assurance_interim()
    #
    # sim_output$Outcome <- with(sim_output, ifelse(
    #   Decision == "Stop for efficacy" & Final_Decision == "Successful", "Correctly stopped for Efficacy",
    #   ifelse(Decision == "Stop for efficacy" & Final_Decision == "Unsuccessful", "Incorrectly stopped for Efficacy",
    #          ifelse(Decision == "Stop for futility" & Final_Decision == "Successful", "Incorrectly stopped for Futility",
    #                 ifelse(Decision == "Stop for futility" & Final_Decision == "Unsuccessful", "Correctly stopped for Futility",
    #                        ifelse(Decision == "Successful at final" & Final_Decision == "Successful", "Successful at Final Analysis",
    #                               # Everything else (including final boundary failed due to interim analyses) is treated as Unsuccessful
    #                               "Unsuccessful at Final Analysis"
    #                        ))))))
    #
    #
    #
    #   outcomes_decisions <- c("Successful at Final Analysis", "Correctly stopped for Efficacy",
    #                           "Incorrectly stopped for Efficacy", "Incorrectly stopped for Futility",
    #                           "Correctly stopped for Futility", "Unsuccessful at Final Analysis")
    #
    #   sim_output$Outcome <- factor(sim_output$Outcome, levels = outcomes_decisions)
    #
    #   outcome_decisions_colors <- setNames(c("green", "green", "green", "red", "red", "red"), outcomes_decisions)
    #
    #
    #
    #   # Create matrix with all combinations
    #   IF_outcomes_mat <- sim_output %>%
    #     group_by(IF, Outcome) %>%
    #     summarise(n = n(), .groups = "drop") %>%
    #     complete(IF, Outcome = outcomes_decisions, fill = list(n = 0)) %>%
    #     group_by(IF) %>%
    #     mutate(Proportion = round(n / sum(n), 3)) %>%
    #     ungroup() %>%
    #     mutate(Outcome = factor(Outcome, levels = outcomes_decisions)) %>%
    #     arrange(Outcome) %>%
    #     select(-n) %>%
    #     pivot_wider(names_from = IF, values_from = Proportion, values_fill = 0)
    #
    #   # Remove the Outcome column and convert to matrix
    #   numeric_mat <- as.matrix(IF_outcomes_mat[, -1])
    #
    #   # Now apply sum to rows 1:3
    #
    #   density_vals <- c(NA, 20, 50, 50, 20, NA)
    #   angle_vals <- c(0, 45, -45, -45, 45, 0)
    #
    #   par(mar = c(5, 4, 4, 20))  # Increase right margin
    #
    #   # Get bar heights to determine where to draw lines
    #   bar_heights <- apply(numeric_mat[1:3, , drop = F], 2, sum)
    #
    #
    #   bar_positions <- barplot(numeric_mat,
    #                            beside = FALSE,
    #                            width = 1,  # Optional, but explicit
    #                            col = outcome_decisions_colors[outcomes_decisions],
    #                            density = density_vals,
    #                            angle = angle_vals,
    #                            xlab = "Information Fraction",
    #                            ylab = "Proportion",
    #                            main = "Proportion of Trial Outcomes at Different Information Fractions")
    #
    #
    #
    #   legend("topright",
    #          legend = c(rev(outcomes_decisions), "Assurance"),
    #          fill = c(rev(outcome_decisions_colors[outcomes_decisions]), NA),
    #          border = c(rep("black", length(outcomes_decisions)), NA),
    #          density = c(rev(density_vals), NA),
    #          angle = c(rev(angle_vals), NA),
    #          col = c(rep(NA, length(outcomes_decisions)), "black"),
    #          cex = 0.6,
    #          xpd = TRUE,
    #          inset = c(-0.4, 0),
    #          bty = "n")
    #
    #
    #
    #   # Add black separator lines between top and bottom outcome segments
    #   segments(x0 = bar_positions - 0.5,  # left edge of each bar
    #            x1 = bar_positions + 0.5,  # right edge
    #            y0 = bar_heights,          # y position (cumulative height of top 3)
    #            y1 = bar_heights,
    #            col = "black",
    #            lwd = 5)
    #
    #   # Add text labels just below the separator line
    #   text(x = bar_positions,
    #        y = bar_heights,  # Adjust this for spacing
    #        labels = round(bar_heights, 2),
    #        cex = 1,                # text size
    #        pos = 1)                  # below the specified y (1 = below)
    #
    #
    # })


  # BPP - Timing Logic ---------------------------------


      function_call_BPP_Timing <- reactive({


        base_call <- paste0("calibrate_BPP_timing(n_c = ",
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
            if (input$ExpRateorTime == "Parameters"){
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



        base_call <- paste0(base_call, "), \n IA_model = list(events = ",
                            input$total_events_BPP_timing,
                            ", \n IF = ", input$BPP_Timing_IF)


        df <- rhandsontable::hot_to_r(input$alpha_spending_table)
        alpha_vector <- df$`alpha.spending`

        base_call <- paste0(base_call,
                            "), \n analysis_model  = list(
          method = \"LRT\",
          alternative_hypothesis = \"one.sided\",
          alpha = ", alpha_vector[length(alpha_vector)])


        base_call <- paste0(base_call,
                            "), \n n_sims = ",
                            input$n_sims_BPP_Timing,
                            ")")




        return(base_call)

      })

    observe({
      shinyAce::updateAceEditor(session, "display_func_BPP_Timing", value = function_call_BPP_Timing())
    })

    calculate_BPP_Timing <- eventReactive(input$calc_BPP_Timing, {
      call_string <- function_call_BPP_Timing()
      result <- eval(parse(text = call_string))
      shinyjs::show("n_breaks_BPP_timing")
      shinyjs::show("lower_bound_BPP_timing")
      shinyjs::show("upper_bound_BPP_timing")
      return(result)
    })

    output$BPP_timing_hist <- renderPlot({

      outcome <- calculate_BPP_Timing()

      hist(outcome$outcome_list[[1]]$BPP_values,
           breaks = input$n_breaks_BPP_timing,
           xlim = c(0,1),
           freq = F,
           xlab = "BPP",
           main = paste0("Distribution of BPP values at IF = ", input$BPP_Timing_IF))

          })


    output$BPP_Timing_text <- renderText({

      outcome <- calculate_BPP_Timing()
      dat     <- outcome$outcome_list[[1]]

      # Mean censoring time
      mean_cens <- mean(dat$cens_time)

      # Informativeness metric
      BPP_outcome <- dat$BPP_values
      inform_metric <- mean(BPP_outcome < input$lower_bound_BPP_timing |
                              BPP_outcome > input$upper_bound_BPP_timing)

      paste0(
        "The mean time of censoring is ", round(mean_cens, 2), ". ",
        "The informativeness metric P(BPP < ", input$lower_bound_BPP_timing,
        " OR BPP > ", input$upper_bound_BPP_timing, ") = ",
        round(inform_metric, 3), "."
      )
    })


    # BPP - Threshold Logic ---------------------------------

    function_call_BPP_Threshold <- reactive({


      base_call <- paste0(
        "calibrate_BPP_threshold(n_c = ", input$n_c,
        ", \n n_t = ", input$n_t,
        base_call <- paste0(
          ", \n control_model = list(\n   dist = \"", input$ControlDist, "\""
        )

      )

      if (input$ControlDist == "Exponential") {

        base_call <- paste0(
          base_call,
          ", \n   parameter_mode = \"", input$ExpChoice, "\""
        )

        if (input$ExpChoice == "Fixed") {

          base_call <- paste0(
            base_call,
            ", \n   fixed_type = \"", input$ExpRateorTime, "\""
          )

          if (input$ExpRateorTime == "Parameters") {

            base_call <- paste0(
              base_call,
              ", \n   lambda = ", input$ExpRate
            )

          } else if (input$ExpRateorTime == "Landmark") {

            base_call <- paste0(
              base_call,
              ", \n   t1 = ", input$ExpTime,
              ", \n   surv_t1 = ", input$ExpSurv
            )
          }
        }

        if (input$ExpChoice == "Distribution") {

          base_call <- paste0(
            base_call,
            ", \n   t1 = ", input$ExpSurvTime,
            ", \n   t1_Beta_a = ", input$ExpBetaA,
            ", \n   t1_Beta_b = ", input$ExpBetaB
          )
        }
      }

      if (input$ControlDist == "Weibull") {

        base_call <- paste0(
          base_call,
          ", \n   control_parameters = \"", input$WeibullChoice, "\""
        )

        if (input$WeibullChoice == "Fixed") {

          base_call <- paste0(
            base_call,
            ", \n   fixed_parameters_type = \"", input$WeibRateorTime, "\""
          )

          if (input$WeibRateorTime == "Parameters") {

            base_call <- paste0(
              base_call,
              ", \n   lambda_c = ", input$WeibullScale,
              ", \n   gamma_c = ", input$WeibullShape
            )

          } else if (input$WeibRateorTime == "Landmark") {

            base_call <- paste0(
              base_call,
              ", \n   t1 = ", input$WeibullTime1,
              ", \n   t2 = ", input$WeibullSurv1,
              ", \n   surv_t1 = ", input$WeibullTime2,
              ", \n   surv_t2 = ", input$WeibullSurv2
            )
          }

        } else if (input$WeibullChoice == "Distribution") {

          base_call <- paste0(
            base_call,
            ", \n   t1 = ", input$WeibullDistT1,
            ", \n   t2 = ", input$WeibullDistT2,
            ", \n   t1_Beta_a = ", input$WeibullDistS1BetaA,
            ", \n   t1_Beta_b = ", input$WeibullDistS1BetaB,
            ", \n   diff_Beta_a = ", input$WeibullDistDelta1BetaA,
            ", \n   diff_Beta_b = ", input$WeibullDistDelta1BetaB
          )
        }
      }


      # Close control_model
      base_call <- paste0(
        base_call,
        "), \n effect_model = list(\n   delay_SHELF = SHELF::fitdist(c(",
        input$TValues,
        "), probs = c(",
        input$TProbs,
        "), lower = ",
        strsplit(input$TLimits, ", ")[[1]][1],
        ", upper = ",
        strsplit(input$TLimits, ", ")[[1]][2],
        "), \n   delay_dist = \"",
        input$TDist,
        "\", \n   HR_SHELF = SHELF::fitdist(c(",
        input$HRValues,
        "), probs = c(",
        input$HRProbs,
        "), lower = ",
        strsplit(input$HRLimits, ", ")[[1]][1],
        ", upper = ",
        strsplit(input$HRLimits, ", ")[[1]][2],
        "), \n   HR_dist = \"",
        input$HRDist,
        "\",  \n   P_S = ",
        input$P_S,
        ", \n   P_DTE = ",
        input$P_DTE,
        "\n )"
      )


      base_call <- paste0(
        base_call,
        ", \n recruitment_model = list(\n   method = \"", input$rec_method, "\""
      )



      if (input$rec_method == "power") {

        base_call <- paste0(
          base_call,
          ", \n   period = ", input$rec_period,
          ", \n   power = ", input$rec_power
        )
      }

      if (input$rec_method == "PWC") {

        base_call <- paste0(
          base_call,
          ", \n   rate = ", input$rec_rate,
          ", \n   duration = ", input$rec_duration
        )
      }

      base_call <- paste0(base_call, ")")


      base_call <- paste0(
        base_call,
        ", \n IA_model = list(\n    events = ", input$total_events_BPP_threshold,
        ", \n   IF = ", input$BPP_Threshold_IF, ")"
      )




      df <- rhandsontable::hot_to_r(input$alpha_spending_table)
      alpha_vector <- df$`alpha.spending`

      base_call <- paste0(
        base_call,
        ", \n analysis_model = list(\n",
        "    method = \"LRT\",\n",
        "    alternative_hypothesis = \"one.sided\",\n",
        "    alpha = ", alpha_vector[length(alpha_vector)], "\n  )"
      )

      base_call <- paste0(
        base_call,
        ", \n data_generating_model = list(\n",
        "    lambda_c = ", input$lambda_c_threshold,
        ", \n    delay_time = ", input$delay_time_threshold,
        ", \n    post_delay_HR = ", input$post_delay_hr_threshold,
        "\n  )"
      )

      base_call <- paste0(
        base_call,
        ", \n n_sims = ", input$n_sims_BPP_Threshold,
        ")"
      )

      return(base_call)

    })


    observe({
      shinyAce::updateAceEditor(session, "display_func_BPP_Threshold", value = function_call_BPP_Threshold())
    })

    calculate_BPP_Threshold <- eventReactive(input$calc_BPP_Threshold, {
      call_string <- function_call_BPP_Threshold()
      result <- eval(parse(text = call_string))
      shinyjs::show("threshold_value")
      shinyjs::show("n_breaks_BPP_threshold")
      return(result)
    })

    output$BPP_Threshold_hist <- renderPlot({

      outcome <- calculate_BPP_Threshold()

      print(outcome)

      hist(outcome$BPP_vec,
           breaks = input$n_breaks_BPP_threshold,
           xlim = c(0,1),
           freq = F,
           xlab = "BPP",
           main = paste0("Distribution of BPP values at IF = ", input$BPP_Threshold_IF))
      abline(v = input$threshold_value, col = "red", lty = 2)


    })



    output$BPP_Threshold_text <- renderText({

      outcome <- calculate_BPP_Threshold()

      # Informativeness metric
      BPP_cutoff <- mean(outcome$BPP_vec < input$threshold_value)

      paste0(
        "P(BPP < ", input$threshold_value, ") = ", round(BPP_cutoff, 2))

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
