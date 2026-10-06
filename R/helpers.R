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

# Settings recorded alongside simulation output: the arguments passed in
# ..., plus package, R, rjags and JAGS versions, the git commit of the
# working directory (the analysis repository, when run from its scripts),
# hostname and timestamp.
make_settings <- function(...) {
  c(list(...),
    list(package_version = tryCatch(as.character(utils::packageVersion("DTEAssurance")), error = function(e) NA_character_),
         r_version  = R.version.string,
         rjags_version = tryCatch(as.character(utils::packageVersion("rjags")), error = function(e) NA_character_),
         jags_version  = tryCatch(rjags::jags.version(), error = function(e) NA_character_),
         git_commit = tryCatch(system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE),
                               error = function(e) NA_character_, warning = function(w) NA_character_),
         hostname = Sys.info()[["nodename"]],
         timestamp = as.character(Sys.time())))
}

# Run the test specified by an analysis_model list (method, alpha,
# alternative_hypothesis, rho, gamma, t_star, s_star) via survival_test().
run_test <- function(data, analysis_model, return_HR = FALSE) {
  survival_test(data,
                analysis_method = analysis_model$method,
                alpha           = analysis_model$alpha,
                alternative     = analysis_model$alternative_hypothesis,
                rho             = analysis_model$rho,
                gamma           = analysis_model$gamma,
                t_star          = analysis_model$t_star,
                s_star          = analysis_model$s_star,
                return_HR       = return_HR)
}

# Simulate a trial (survival times plus recruitment) under a fixed truth:
# list(lambda_c, delay_time, post_delay_HR, gamma_c), with an exponential
# control arm if gamma_c is NULL.
simulate_trial_from_truth <- function(truth, n_c, n_t, recruitment_model) {
  dist <- if (is.null(truth$gamma_c)) "Exponential" else "Weibull"
  d <- sim_dte(n_c, n_t, truth$lambda_c,
               delay_time = truth$delay_time, post_delay_HR = truth$post_delay_HR,
               dist = dist, gamma_c = truth$gamma_c)
  add_recruitment_time(d, rec_method = recruitment_model$method,
                       rec_period = recruitment_model$period, rec_power = recruitment_model$power,
                       rec_rate = recruitment_model$rate, rec_duration = recruitment_model$duration)
}

# Weibull rate and shape from two landmark survival probabilities,
# S(t) = exp(-(lambda * t)^gamma), S(t1) = s1, S(t2) = s2. Same closed form
# as the control prior in the update_priors() JAGS model.
weibull_from_landmarks <- function(s1, s2, t1, t2) {
  ok <- is.finite(c(s1, s2, t1, t2))
  if (!all(ok) || !(s1 > s2 && s2 > 0 && s1 < 1 && t1 < t2 && t1 > 0)) {
    stop("weibull_from_landmarks: need 1 > s1 > s2 > 0 and 0 < t1 < t2 ",
         "(got s1 = ", s1, ", s2 = ", s2, ", t1 = ", t1, ", t2 = ", t2, ").",
         call. = FALSE)
  }
  gamma <- log(log(s1) / log(s2)) / log(t1 / t2)
  lambda <- (-log(s1))^(1 / gamma) / t1
  list(lambda = lambda, gamma = gamma)
}
