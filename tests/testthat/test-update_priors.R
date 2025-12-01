test_that("update_priors runs with Exponential control model and returns posterior samples", {
  skip_on_cran()  # optional: uncomment if runtime becomes too slow
  skip_if_not_installed("rjags")

  set.seed(123)

  # Minimal synthetic dataset
  data <- data.frame(
    survival_time = c(1, 2, 3, 1.5),
    status = c(1, 0, 1, 0),
    group = c("Control", "Control", "Treatment", "Treatment")
  )

  control_model <- list(
    dist = "Exponential",
    t1 = 2,
    t1_Beta_a = 2,
    t1_Beta_b = 2
  )

  effect_model <- list(
    delay_SHELF = NULL,    # placeholder for your internal structure
    delay_dist = "lognormal",
    HR_SHELF = NULL,
    HR_dist = "lognormal",
    P_S = 0.5,
    P_DTE = 0.5
  )

  # make_prior_name_jags should return valid JAGS expressions
  expect_true(is.character(make_prior_name_jags(effect_model$delay_SHELF, effect_model$delay_dist)))
  expect_true(is.character(make_prior_name_jags(effect_model$HR_SHELF, effect_model$HR_dist)))

  out <- update_priors(
    data = data,
    control_model = control_model,
    effect_model = effect_model,
    n_samples = 50
  )

  # structural tests
  expect_true(is.data.frame(out))
  expect_true(nrow(out) > 0)

  # expected columns for Exponential model
  expect_true(all(c("lambda_c", "HR", "delay_time") %in% names(out)))

  # probabilistic sanity checks
  expect_true(all(out$HR > 0))
  expect_true(all(out$lambda_c > 0))
  expect_true(all(out$delay_time >= 0))
})
