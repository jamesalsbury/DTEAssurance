#' Deprecated functions in DTEAssurance
#'
#' These functions were renamed to match the manuscript notation (PP for the
#' predictive probability, kappa for the futility threshold) and will be
#' removed in the next release. Each one forwards its arguments to its
#' replacement.
#'
#' Only the function names are translated. The elements and columns they
#' return use the new names (\code{PP_df}, \code{PP_vec}, \code{PP_values},
#' \code{PP_val}, \code{kappa}, \code{kappa_star}), so code that reads
#' \code{BPP_df}, \code{BPP_vec}, \code{lambda_star}, etc. must be updated.
#'
#' \itemize{
#'   \item \code{BPP_func()}: use \code{\link{PP_func}}.
#'   \item \code{calibrate_BPP_threshold()}: use \code{\link{calibrate_PP_threshold}}.
#'   \item \code{calibrate_BPP_timing()}: use \code{\link{calibrate_PP_timing}}.
#'   \item \code{summarize_grid_by_lambda()}: use \code{\link{summarize_grid_by_kappa}}.
#'   \item \code{select_lambda_star()}: use \code{\link{select_kappa_star}}.
#' }
#'
#' @param ... Arguments passed to the replacement function.
#' @param raw,lambda_grid,conf_level See \code{\link{summarize_grid_by_kappa}}
#'   (\code{lambda_grid} is passed as \code{kappa_grid}).
#'
#' @return The return value of the replacement function.
#'
#' @name DTEAssurance-deprecated
#' @keywords internal
NULL

deprecated_msg <- function(old, new) {
  paste0("'", old, "' is deprecated; use '", new, "' instead. ",
         "Note that returned element and column names have also changed ",
         "(BPP_* -> PP_*, lambda -> kappa) and are not translated.\n",
         "See help(\"DTEAssurance-deprecated\").")
}

#' @rdname DTEAssurance-deprecated
#' @export
BPP_func <- function(...) {
  .Deprecated("PP_func", package = "DTEAssurance",
              msg = deprecated_msg("BPP_func", "PP_func"))
  PP_func(...)
}

#' @rdname DTEAssurance-deprecated
#' @export
calibrate_BPP_threshold <- function(...) {
  .Deprecated("calibrate_PP_threshold", package = "DTEAssurance",
              msg = deprecated_msg("calibrate_BPP_threshold", "calibrate_PP_threshold"))
  calibrate_PP_threshold(...)
}

#' @rdname DTEAssurance-deprecated
#' @export
calibrate_BPP_timing <- function(...) {
  .Deprecated("calibrate_PP_timing", package = "DTEAssurance",
              msg = deprecated_msg("calibrate_BPP_timing", "calibrate_PP_timing"))
  calibrate_PP_timing(...)
}

#' @rdname DTEAssurance-deprecated
#' @export
summarize_grid_by_lambda <- function(raw, lambda_grid, conf_level = 0.90) {
  .Deprecated("summarize_grid_by_kappa", package = "DTEAssurance",
              msg = deprecated_msg("summarize_grid_by_lambda", "summarize_grid_by_kappa"))
  summarize_grid_by_kappa(raw, kappa_grid = lambda_grid, conf_level = conf_level)
}

#' @rdname DTEAssurance-deprecated
#' @export
select_lambda_star <- function(...) {
  .Deprecated("select_kappa_star", package = "DTEAssurance",
              msg = deprecated_msg("select_lambda_star", "select_kappa_star"))
  select_kappa_star(...)
}
