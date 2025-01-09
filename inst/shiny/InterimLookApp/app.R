# library(SHELF)
# library(survMisc)
# library(ggplot2)
# library(pbapply)
# library(readxl)
# library(shiny)
# library(shinydashboard)
library(survival)
# library(plyr)
# library(rmarkdown)
# library(stats)
# library(nleqslv)
# library(graphics)
# library(shinyjs)
# library(utils)
# library(nleqslv)
# library(dplyr)
library(GenSA)
library(survminer)

ui <- fluidPage(
  withMathJax(),

  # Application title
  titlePanel("Interim Analysis: Delayed Treatment Effects"),

  # sidebarLayout(
  mainPanel(

    tabsetPanel(
      # Upload UI ---------------------------------

      tabPanel("Upload",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("file", "Choose an RDS File",
                             multiple = FALSE,
                             accept = c(".rds")),

                     ),

                 mainPanel = mainPanel(
                   plotOutput("plotKM"),
                   verbatimTextOutput("summaryOutput")
                 )
               ),
      ),

      # MLEs UI ---------------------------------
      tabPanel("MLEs",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   selectInput("ControlDist", "Distribution", choices = c("Exponential", "Weibull"), selected = "Exponential"),
                   actionButton("calcMLE", "Calculate MLEs"),
                 ),
                 mainPanel = mainPanel(
                   plotOutput("plotMLEs"),
                   verbatimTextOutput("mleOutput")
                 )
               ),
      ),


      # Prediction UI ---------------------------------
      tabPanel("Prediction",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   selectInput("modelInput", "Modelling", choices = c("Proportional Hazards (Exp)", "Exponential with Delay", "Weibull with Delay"), selected = "Exponential"),
                   numericInput("totalEvents", "Total Number of Events", value = 0),
                   numericInput("critValue", "Critical Z-Value", value = 1.96),
                   numericInput("DeltaPrime", "Projected trend", value = 0),
                   actionButton("calcCP", "Calculate Conditional Power"),
                 ),
                 mainPanel = mainPanel(
                   verbatimTextOutput("predictionOutput"),
                   verbatimTextOutput("CPOutput")

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

  # Functions for the upload tab ---------------------------------

  uploadedData <- reactive({
    req(input$file)  # Ensure the file is uploaded
    dataCombined <- readRDS(input$file$datapath)

    updateNumericInput(session, "totalEvents", value = sum(dataCombined$status))

    # Extract HR from Cox
    cox_model <- coxph(Surv(survivalTime, status) ~ group, data = dataCombined)
    cox_summary <- summary(cox_model)
    hazard_ratio <- exp(cox_summary$coefficients[, "coef"])
    updateNumericInput(session, "DeltaPrime", value = round(hazard_ratio, 3))

    dataCombined
  })

  output$plotKM <- renderPlot({
    dataCombined <- uploadedData()
    kmfit <- survfit(Surv(survivalTime, status)~group, data = dataCombined)

    ggsurvplot(
      kmfit,
      data = dataCombined,
      risk.table = TRUE,         # Add number at risk table
      risk.table.col = "strata", # Color by strata (group)
      ggtheme = theme_minimal(), # Apply minimal theme
      xlab = "Time",      # X-axis label
      ylab = "Survival Probability", # Y-axis label
      risk.table.y.text.col = TRUE,  # Use colored text for groups
      risk.table.y.text = F      # Turn off group names on the y-axis of the risk table
    )
    #plot(kmfit, col = c("blue", "red"), xlab = "Time", ylab = "Survival Probability")
  })


  output$summaryOutput <- renderText({
    # Access uploaded data
    dataCombined <- uploadedData()

    # Fit Cox PH model
    cox_model <- coxph(Surv(survivalTime, status) ~ group, data = dataCombined)

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


  # Functions for the MLE tab ---------------------------------

  observeEvent(input$calcMLE, {

    dataCombined <- uploadedData()

    # Optimized custom log-likelihood function with improved performance
    loglikExp <- function(params) {

      lambda <- params[1]
      HR2 <- params[2]
      delayT <- params[3]

      # Precompute values for different groups
      control_mask <- data$group == "Control"
      treatment_mask <- !control_mask

      # Control group
      control_status <- data$status[control_mask]
      control_time <- data$survivalTime[control_mask]

      control_loglik <- ifelse(
        control_status,
        log(lambda) - lambda * control_time,   # Event
        -lambda * control_time                # Censoring
      )

      # Treatment group
      treatment_status <- data$status[treatment_mask]
      treatment_time <- data$survivalTime[treatment_mask]

      # Delayed hazard calculation
      treatment_loglik <- ifelse(
        treatment_status,
        ifelse(
          treatment_time < delayT,
          log(lambda) - lambda * treatment_time,                        # Event before delay
          log(HR2 * lambda) - lambda * delayT - HR2 * lambda * (treatment_time - delayT)  # Event after delay
        ),
        ifelse(
          treatment_time < delayT,
          -lambda * treatment_time,                                     # Censoring before delay
          -lambda * delayT - HR2 * lambda * (treatment_time - delayT)   # Censoring after delay
        )
      )

      # Combine log-likelihoods
      logL <- sum(control_loglik) + sum(treatment_loglik)

      return(-logL)
    }

    loglikWeibull <- function(params) {

      lambda_c <- params[1]
      gamma <- params[2]
      lambda_e <- params[3]
      delayT <- params[4]

      # Precompute values for different groups
      control_mask <- data$group == "Control"
      treatment_mask <- !control_mask

      # Control group
      control_status <- data$status[control_mask]
      control_time <- data$survivalTime[control_mask]

      control_loglik <- ifelse(
        control_status,
        log(gamma) + gamma*log(lambda_c) + gamma*log(control_time) - (lambda_c*control_time)^gamma - log(control_time),   # Event
        -(lambda_c*control_time)^gamma                # Censoring
      )

      # Treatment group
      treatment_status <- data$status[treatment_mask]
      treatment_time <- data$survivalTime[treatment_mask]

      # Delayed hazard calculation
      treatment_loglik <- ifelse(
        treatment_status,
        ifelse(
          treatment_time < delayT,
          log(gamma) + gamma*log(lambda_c) + gamma*log(treatment_time) - (lambda_c*treatment_time)^gamma - log(treatment_time),                        # Event before delay
          log(gamma) + gamma*log(lambda_e) + (gamma-1)*log(treatment_time) - lambda_e^gamma*(treatment_time^gamma - delayT^gamma) - (lambda_c*delayT)^gamma  # Event after delay
        ),
        ifelse(
          treatment_time < delayT,
          -(lambda_c*treatment_time)^gamma,                                     # Censoring before delay
          -(lambda_c*delayT)^gamma - lambda_e^gamma*(treatment_time^gamma - delayT^gamma)   # Censoring after delay
        )
      )

      # Combine log-likelihoods
      logL <- sum(control_loglik) + sum(treatment_loglik)

      return(-logL)
    }

    if (input$ControlDist=="Exponential"){
      data <- dataCombined
      # Initial guesses for the parameters
      # lambda_control, lambda_treatment, tau
      initial_params <- c(0.1, 0.1, 2)

      # Optimization using optim()
      result <- GenSA(
        par = initial_params,
        fn = loglikExp,
        lower = c(1e-6, 1e-6, 0),  # Lower bounds for parameters
        upper = c(1, 1, 10)  # Upper bounds
      )

      # Extract results
      lambda_control_mle <- result$par[1]
      lambda_treatment_mle <- result$par[2]
      tau_mle <- result$par[3]

      output$mleOutput <- renderText({
        paste0(
          "MLE Estimates:\n",
          "Control Hazard: ", round(lambda_control_mle, 4), "\n",
          "Post-Delay Hazard Ratio: ", round(lambda_treatment_mle, 4), "\n",
          "Length of Delay: ", round(tau_mle, 4), "\n"
        )
      })

      output$plotMLEs <- renderPlot({

        kmFit <- survfit(Surv(survivalTime, status)~group, data = data)
        plot(kmFit, col = c("blue", "red"), xlab = "Time", ylab = "Survival Probability")
        legend("topright", legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)

        trialTime <- seq(0, 60, by=0.1)
        survControl <- exp(-lambda_control_mle*trialTime)
        lines(trialTime, survControl, col = "blue", lty = 2)
        survTreatment <- ifelse(trialTime < tau_mle,
                                exp(-lambda_control_mle*trialTime),
                                exp(-lambda_control_mle*tau_mle - lambda_control_mle*lambda_treatment_mle*(trialTime-tau_mle)))
        lines(trialTime, survTreatment, col = "red", lty = 2)

      })

    } else{
      data <- dataCombined
      # Initial guesses for the parameters
      # lambda_control, lambda_treatment, tau
      initial_params <- c(0.1, 0.1, 0.1, 2)

      # Optimization using optim()
      result <- GenSA(
        par = initial_params,
        fn = loglikWeibull,
        lower = c(1e-6, 1e-6, 1e-6, 0),  # Lower bounds for parameters
        upper = c(10, 10, 10, 10)  # Upper bounds
      )

      # Extract results
      lambda_control_mle <- result$par[1]
      gamma_mle <- result$par[2]
      lambda_treatment_mle <- result$par[3]
      tau_mle <- result$par[4]

      output$mleOutput <- renderText({
        paste0(
          "MLE Estimates:\n",
          "Lambda_c: ", round(lambda_control_mle, 4), "\n",
          "Gamma: ", round(gamma_mle, 4), "\n",
          "Lambda_t: ", round(lambda_treatment_mle, 4), "\n",
          "Length of Delay: ", round(tau_mle, 4), "\n"
        )
      })

      output$plotMLEs <- renderPlot({
      kmFit <- survfit(Surv(survivalTime, status)~group, data = data)
      plot(kmFit, col = c("blue", "red"), xlab = "Time", ylab = "Survival Probability")
      legend("topright", legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)

      trialTime <- seq(0, 60, by=0.1)
      survControl <- exp(-(lambda_control_mle*trialTime)^gamma_mle)
      lines(trialTime, survControl, col = "blue", lty = 2)
      survTreatment <- ifelse(trialTime < tau_mle,
                              exp(-(lambda_control_mle*trialTime)^gamma_mle),
                              exp(-(lambda_control_mle*tau_mle)^gamma_mle - lambda_treatment_mle^gamma_mle*(trialTime^gamma_mle-tau_mle^gamma_mle)))
      lines(trialTime, survTreatment, col = "red", lty = 2)


      })
    }


  })


  # Functions for the Predictions tab ---------------------------------


  output$predictionOutput <- renderText({

    dataCombined <- uploadedData()

    n_control <- sum(dataCombined$group=="Control")
    n_treatment <- sum(dataCombined$group=="Treatment")

    cox_model <- coxph(Surv(survivalTime, status) ~ group, data = dataCombined)
    cox_summary <- summary(cox_model)
    hazard_ratio <- exp(cox_summary$coefficients[, "coef"])

    paste0(
      "Total number of events: ", input$totalEvents, "\n",
      "Current number of events: ", sum(dataCombined$status), "\n",
      "Ratio of control to treatment: 1:", n_treatment/n_control, "\n",
      "Current trend: ", round(hazard_ratio, 3), "\n",
      "Critical Z-Value: ", input$critValue, "\n",
      "Projected HR: ", input$DeltaPrime, "\n"
    )

  })

  observeEvent(input$calcCP, {

    output$CPOutput <- renderText({

      dataCombined <- uploadedData()

      n_control <- sum(dataCombined$group=="Control")
      n_treatment <- sum(dataCombined$group=="Treatment")

      cox_model <- coxph(Surv(survivalTime, status) ~ group, data = dataCombined)
      cox_summary <- summary(cox_model)
      hazard_ratio <- exp(cox_summary$coefficients[, "coef"])

      delta_d <- hazard_ratio

      d <- sum(dataCombined$status)
      a <- n_treatment/n_control
      r <- (a+1)/sqrt(a)

      Delta1 <- 1

      result <- (1 / r) * sqrt(input$totalEvents / (input$totalEvents - d)) * ( (d / sqrt(input$totalEvents)) * log(Delta1 / delta_d) +
                                                  ((input$totalEvents - d) / sqrt(input$totalEvents)) * log(Delta1 / input$DeltaPrime) - r * input$critValue )

      pnorm(result)


      paste0("The conditional power is calculated to be: ", round(pnorm(result), 3))

    })

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





