# Benchmark: seconds per paired replicate at the manuscript settings
# (600 per arm, 840 events, futility IF 0.5, efficacy IF 0.75,
# PP_sims = 2000, update_priors_sims = 1000), with the Cox fit inside the
# PP calculation switched off (return_HR = FALSE, the package default for
# PP) and on (return_HR = TRUE, the pre-1.3.0 behaviour).
#
# Not part of the package API. Run with:
#   Rscript inst/benchmark/time_one_rep.R [n_reps]

library(DTEAssurance)

n_reps <- as.integer(commandArgs(trailingOnly = TRUE)[1])
if (is.na(n_reps)) n_reps <- 10L

design <- DTEAssurance:::make_rpact_design_from_GSD_model(
  list(alpha_IF = c(0.75, 1), alpha_spending = c(0.0125, 0.025),
       futility_type = "PP", futility_IF = 0.5)
)$design

args <- list(
  n_c = 600, n_t = 600, total_events = 840,
  futility_IF = 0.5, efficacy_IF = 0.75, design = design,
  control_model = list(dist = "Exponential", parameter_mode = "Distribution",
                       t1 = 12, t1_Beta_a = 20, t1_Beta_b = 32),
  effect_model = list(
    P_S = 0.9, P_DTE = 0.7,
    HR_SHELF = SHELF::fitdist(c(0.55, 0.65, 0.75), probs = c(0.25, 0.5, 0.75),
                              lower = 0, upper = 1.5),
    HR_dist = "gamma",
    delay_SHELF = SHELF::fitdist(c(2, 3, 4), probs = c(0.25, 0.5, 0.75),
                                 lower = 0, upper = 12),
    delay_dist = "gamma"
  ),
  recruitment_model = list(method = "power", period = 34, power = 1),
  truth = list(lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.65),
  analysis_model_LRT = list(method = "LRT", alpha = 0.025,
                            alternative_hypothesis = "one.sided"),
  update_priors_sims = 1000,
  PP_sims = 2000
)

time_reps <- function() {
  t0 <- proc.time()[["elapsed"]]
  rows <- lapply(seq_len(n_reps), function(i)
    do.call(single_paired_rep, c(list(i = i, seed = 1), args)))
  el <- proc.time()[["elapsed"]] - t0
  raw <- do.call(rbind, rows)
  list(sec_per_rep = el / n_reps, n_failed = sum(!is.na(raw$error)), raw = raw)
}

# return_HR = FALSE (package behaviour)
res_false <- time_reps()

# return_HR = TRUE: temporarily force the Cox fit in every survival_test() call
ns <- asNamespace("DTEAssurance")
orig <- get("survival_test", envir = ns)
forced <- function(...) {
  a <- list(...)
  a$return_HR <- TRUE
  do.call(orig, a)
}
unlockBinding("survival_test", ns)
assign("survival_test", forced, envir = ns)
res_true <- tryCatch(time_reps(), finally = {
  assign("survival_test", orig, envir = ns)
  lockBinding("survival_test", ns)
})

cat(sprintf("Replicates per setting: %d\n", n_reps))
cat(sprintf("return_HR = FALSE: %.1f s per replicate (%d failed)\n",
            res_false$sec_per_rep, res_false$n_failed))
cat(sprintf("return_HR = TRUE:  %.1f s per replicate (%d failed)\n",
            res_true$sec_per_rep, res_true$n_failed))
cat(sprintf("Speed-up from skipping the Cox fit: %.2fx\n",
            res_true$sec_per_rep / res_false$sec_per_rep))
cat(sprintf("Identical PP_val with and without the Cox fit: %s\n",
            identical(res_false$raw$PP_val, res_true$raw$PP_val)))
