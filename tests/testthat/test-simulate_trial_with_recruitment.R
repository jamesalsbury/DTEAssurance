test_that("simulate_trial_with_recruitment returns expected structure", {
  control_model <- list(
    dist = "Exponential",
    parameter_mode = "Fixed",
    fixed_type = "Parameters",
    lambda = 0.1
  )

  effect_model <- list(
    P_S = 1,
    P_DTE = 0,
    HR_SHELF = SHELF::fitdist(c(0.6, 0.65, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 2),
    HR_dist = "gamma",
    delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
    delay_dist = "gamma"
  )

  recruitment_model <- list(
    method = "power",
    period = 12,
    power = 1
  )

  result <- simulate_trial_with_recruitment(
    n_c = 50,
    n_t = 50,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("time", "group", "rec_time", "pseudo_time") %in% names(result)))
  expect_equal(nrow(result), 100)
  expect_true(all(result$time >= 0))
  expect_true(all(result$rec_time >= 0))
  expect_true(all(result$pseudo_time >= result$time))
})
