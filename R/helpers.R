# Internal helpers shared across the simulation code.

# Number of events at an information fraction. Floor with tolerance, matching
# the manuscript's floor(F_j * E).
n_events_at <- function(total_events, IF) {
  as.integer(floor(total_events * round(IF, 6) + 1e-8))
}

# Future fixed decision points (efficacy looks and final) strictly after
# `from_IF`, taken from an rpact design.
make_future_boundaries <- function(design, total_events, from_IF) {
  IFs <- round(design$informationRates, 6)
  keep <- which(IFs > round(from_IF, 6) + 1e-9)
  lapply(keep, function(k) list(events = n_events_at(total_events, IFs[k]),
                                crit   = design$criticalValues[k]))
}

# Critical value of an rpact design at information fraction IF (NA if IF is
# not one of the design's looks).
crit_at <- function(design, IF) {
  idx <- which(round(design$informationRates, 6) == round(IF, 6))
  if (length(idx) == 1) design$criticalValues[idx] else NA_real_
}

# PP_func draws future recruitment times from Uniform(df_cens_time,
# rec_time_planned), which is only valid for uniform recruitment.
check_uniform_recruitment <- function(recruitment_model, caller) {
  if (!identical(recruitment_model$method, "power") ||
      !isTRUE(all.equal(recruitment_model$power, 1))) {
    stop(caller, ": the predictive probability calculation draws future ",
         "recruitment times from Uniform(interim time, recruitment_model$period), ",
         "which requires recruitment_model$method = \"power\" with power = 1.",
         call. = FALSE)
  }
}

# Summarise per-parameter Gelman-Rubin R-hat values. Non-finite values (NaN
# arises legitimately for parameters that are constant across draws, e.g.
# delay_time when Z != 3 throughout) are counted, not treated as failures.
rhat_summary <- function(rhat, threshold) {
  finite <- is.finite(rhat)
  list(
    converged = if (any(finite)) all(rhat[finite] < threshold) else NA,
    rhat_max = if (any(finite)) max(rhat[finite]) else NA_real_,
    n_nonfinite_rhat = sum(!finite)
  )
}

# Git commit of the DTEAssurance source, if it can be determined.
package_git_commit <- function() {
  sha <- tryCatch(utils::packageDescription("DTEAssurance")$RemoteSha,
                  error = function(e) NULL)
  if (!is.null(sha) && nzchar(sha)) return(sha)
  path <- tryCatch(find.package("DTEAssurance"), error = function(e) "")
  if (nzchar(path) && file.exists(file.path(path, ".git"))) {
    sha <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(path), "rev-parse", "HEAD"),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) character(0))
    if (length(sha) == 1 && nzchar(sha)) return(sha)
  }
  NA_character_
}

# Provenance fields recorded alongside simulation output.
run_provenance <- function() {
  list(
    package_version = tryCatch(as.character(utils::packageVersion("DTEAssurance")),
                               error = function(e) NA_character_),
    R_version = R.version.string,
    git_commit = package_git_commit(),
    timestamp = as.character(Sys.time())
  )
}
