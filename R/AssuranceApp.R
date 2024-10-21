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

assurance_shiny_app <- function() {
  app_dir <- system.file("shiny/AssuranceApp/app", package = "DTEAssurance")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `DTEAssurance`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
