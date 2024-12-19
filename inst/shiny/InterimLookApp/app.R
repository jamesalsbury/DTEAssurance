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
library(dplyr)

ui <- fluidPage(
  withMathJax(),

  # Application title
  titlePanel("Assurance: Delayed Treatment Effects"),

  # sidebarLayout(
  mainPanel(

    tabsetPanel(
      # Control UI ---------------------------------

      tabPanel("Upload",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   fileInput("file", "Choose an RDS File",
                             multiple = FALSE,
                             accept = c(".rds")),

                     ),



                 mainPanel = mainPanel(
                   plotOutput("plotControl")
                 )
               ),
      ),

      tabPanel("MLEs",
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   selectInput("ControlDist", "Distribution", choices = c("Exponential", "Weibull"), selected = "Exponential"),


                 ),
                 mainPanel = mainPanel(

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
        nSamples <- 100
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
    req(input$file)
    dataCombined <- readRDS(input$file$datapath)
    kmfit <- survfit(Surv(survivalTime, status)~group, data = dataCombined)
    plot(kmfit, col = c("blue", "red"))
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





