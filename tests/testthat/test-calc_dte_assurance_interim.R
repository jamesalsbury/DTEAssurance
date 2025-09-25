test_that("calc_dte_assurance_interim returns expected structure and values", {
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

  recruitment_model <- list(
    method = "power",
    period = 12,
    power = 1
  )

  GSD_model <- list(
    events = 100,
    alpha_spending = c("0.0125, 0.025"),
    beta_spending = c("0.05, 0.1"),
    IF_vec = c("0.5, 1")
  )

  result <- calc_dte_assurance_interim(
    n_c = 50,
    n_t = 50,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model,
    GSD_model = GSD_model,
    n_sims = 5
  )

  expect_false(is.null(result))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true(all(c("Trial", "IF", "Decision", "StopTime", "SampleSize", "Final_Decision") %in% names(result)))
  expect_type(result$Trial, "integer")
  expect_type(result$IF, "character")
  expect_type(result$Decision, "character")
  expect_type(result$StopTime, "double")
  expect_type(result$SampleSize, "integer")
  expect_type(result$Final_Decision, "character")
})
