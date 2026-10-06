# Generates PP_func_equivalence.rds: fixed inputs and the output of the
# pre-rename BPP_func() (tag v1.2.0-submission1). Run from a checkout of that
# tag with the package loaded (devtools::load_all()). Kept for provenance;
# test-PP_func_equivalence.R checks PP_func() reproduces these outputs.

set.seed(2024)
n <- 40
cens_time <- 15
rec_time <- runif(n, 0, 14)
group <- rep(c("Control", "Treatment"), each = n / 2)
time <- ifelse(group == "Control", rexp(n, log(2) / 9), rexp(n, log(2) / 12))
df <- data.frame(time = time, group = group, rec_time = rec_time)
df$pseudo_time <- df$time + df$rec_time
df$status <- df$pseudo_time < cens_time
df$survival_time <- ifelse(df$status, df$time, cens_time - df$rec_time)

n_post <- 25
posterior_exp <- data.frame(
  HR = rnorm(n_post, 0.7, 0.08),
  delay_time = runif(n_post, 0, 4),
  lambda_c = rnorm(n_post, log(2) / 9, 0.01)
)
posterior_weib <- cbind(posterior_exp, gamma_c = runif(n_post, 0.9, 1.4))

analysis_model <- list(method = "LRT", alpha = 0.025,
                       alternative_hypothesis = "one.sided")
future_boundaries <- list(list(events = 45, crit = 2.24),
                          list(events = 60, crit = 2.00))
censoring_model <- list(method = "Events", events = 60)

cases <- list(
  exp_boundaries = list(control_distribution = "Exponential",
                        posterior_df = posterior_exp,
                        future_boundaries = future_boundaries),
  weib_boundaries = list(control_distribution = "Weibull",
                         posterior_df = posterior_weib,
                         future_boundaries = future_boundaries),
  exp_legacy = list(control_distribution = "Exponential",
                    posterior_df = posterior_exp,
                    censoring_model = censoring_model)
)
common <- list(data = df, n_c_planned = 30, n_t_planned = 30,
               rec_time_planned = 20, df_cens_time = cens_time,
               analysis_model = analysis_model, n_sims = 50)

outputs <- lapply(cases, function(case) {
  set.seed(1)
  suppressMessages(do.call(BPP_func, c(common, case)))$BPP_df
})

saveRDS(list(common = common, cases = cases, outputs = outputs),
        "PP_func_equivalence.rds", version = 2)
