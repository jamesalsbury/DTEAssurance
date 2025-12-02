test_that("update_priors runs with Exponential control model and returns posterior samples", {
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

  effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                      delay_dist = "gamma",
                      HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                      HR_dist = "gamma",
                      P_S = 1,
                      P_DTE = 0)

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

test_that("update_priors runs with Weibull control model and returns posterior samples", {

  skip_if_not_installed("rjags")

  set.seed(123)

  # Minimal dataset with both groups
  data <- data.frame(
    survival_time = c(1.2, 2.5, 3.1, 4.0),
    status        = c(1, 1, 0, 1),
    group         = c("Control", "Control", "Treatment", "Treatment")
  )

  # Minimal valid Weibull control model
  control_model <- list(
    dist           = "Weibull",
    parameter_mode = "Distribution",  # required for mixture model logic
    t1             = 3,
    t2             = 6,
    t1_Beta_a      = 5,
    t1_Beta_b      = 10,
    diff_Beta_a    = 2,
    diff_Beta_b    = 5
  )

  effect_model <- list(
    delay_SHELF = SHELF::fitdist(c(5, 6, 7),
                                 probs = c(0.25, 0.5, 0.75),
                                 lower = 0, upper = 12),
    delay_dist = "gamma",
    HR_SHELF  = SHELF::fitdist(c(0.5, 0.6, 0.7),
                               probs = c(0.25, 0.5, 0.75),
                               lower = 0, upper = 1),
    HR_dist   = "gamma",
    P_S       = 1,
    P_DTE     = 0
  )

  out <- update_priors(
    data          = data,
    control_model = control_model,
    effect_model  = effect_model,
    n_samples     = 30   # small, keeps runtime short
  )

  # Tests
  expect_true(is.data.frame(out))
  expect_true(nrow(out) > 0)

  # Weibull must include gamma_c column
  expect_true(all(c("lambda_c", "gamma_c", "HR", "delay_time") %in% names(out)))

  # sanity checks
  expect_true(all(out$lambda_c > 0))
  expect_true(all(out$gamma_c > 0))
  expect_true(all(out$HR > 0))
  expect_true(all(out$delay_time >= 0))
})

