test_that("simulate_one_trial returns expected structure", {
  control_model <- list(
    dist = "Exponential",
    parameter_mode = "Fixed",
    fixed_type = "Parameters",
    lambda = 0.1
  )

  effect_model <- list(
    delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
    delay_dist = "gamma",
    HR_SHELF = SHELF::fitdist(c(0.55, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1.5),
    HR_dist = "gamma",
    P_S = 1,
    P_DTE = 0
  )

  censoring_model <- list(
    method = "Time",
    time = 12
  )

  recruitment_model <- list(
    method = "power",
    period = 12,
    power = 1
  )

  analysis_model <- list(
    method = "LRT",
    alpha = 0.025,
    alternative_hypothesis = "one.sided",
    rho = 0,
    gamma = 0,
    t_star = NULL,
    s_star = NULL
  )

  result <- simulate_one_trial(
    i = 1, j = 1,
    n_c = 50, n_t = 50,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = censoring_model,
    recruitment_model = recruitment_model,
    analysis_model = analysis_model
  )

  expect_type(result, "list")
  expect_named(result, c("Signif", "observed_HR", "sample_size", "cens_time"))
  expect_type(result$Signif, "logical")
  expect_type(result$observed_HR, "double")
  expect_type(result$sample_size, "integer")
  expect_type(result$cens_time, "double")
})
