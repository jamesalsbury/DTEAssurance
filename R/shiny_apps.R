#' Launch the 'shiny' Assurance app
#'
#' Launches a 'shiny' application to calculate assurance for clinical trials
#' where delayed treatment effects (DTE) may be present. The app allows
#' elicitation of prior distributions and calculates assurance metrics.
#'
#' @return No return value, called for side effects (invisibly returns NULL).
#'         The function launches an interactive 'shiny' application.
#'
#' @examples
#' if (interactive()) {
#'   # Launch the interactive app in an R session
#'   assurance_shiny_app()
#' }
#'
#' @export
assurance_shiny_app <- function() {
  app_dir <- system.file("shiny/AssuranceApp/app.R", package = "DTEAssurance")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `DTEAssurance`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}



#' Launch the 'shiny' GSD Assurance app
#'
#' Launches a 'shiny' application to simulate group sequential trials with
#' delayed treatment effects (DTE) using elicited prior distributions. The app
#' allows interactive exploration of trial designs and assurance calculations.
#'
#' @return No return value, called for side effects (invisibly returns NULL).
#'         The function launches an interactive 'shiny' application.
#'
#' @examples
#' if (interactive()) {
#'   # Launch the interactive app in an R session
#'   assurance_GSD_shiny_app()
#' }
#'
#' @export
assurance_GSD_shiny_app <- function() {
  app_dir <- system.file("shiny/AdaptiveApp/app.R", package = "DTEAssurance")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `DTEAssurance`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
