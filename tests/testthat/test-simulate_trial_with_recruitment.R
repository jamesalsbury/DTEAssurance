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

test_that("simulate_trial_with_recruitment works: Exponential + Fixed + Landmark", {
  set.seed(123)

  control_model <- list(
    dist = "Exponential",
    parameter_mode = "Fixed",
    fixed_type = "Landmark",
    t1 = 5,
    surv_t1 = 0.7
  )

  effect_model <- list(
    P_S = 1, P_DTE = 1,
    delay_SHELF = SHELF::fitdist(c(3,4,5), probs=c(0.25,0.5,0.75), lower=0, upper=10),
    delay_dist = "gamma",
    HR_SHELF = SHELF::fitdist(c(0.5,0.6,0.7), probs=c(0.25,0.5,0.75), lower=0, upper=1),
    HR_dist = "gamma"
  )

  recruitment_model <- list(method="power", period=12, power=1)

  out <- simulate_trial_with_recruitment(20,20,control_model,effect_model,recruitment_model)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("time","group","rec_time","pseudo_time") %in% names(out)))
})

test_that("simulate_trial_with_recruitment works: Weibull + Fixed + Parameters", {
  set.seed(321)

  control_model <- list(
    dist="Weibull",
    parameter_mode="Fixed",
    fixed_type="Parameters",
    lambda=0.1,
    gamma=1.5
  )

  effect_model <- list(
    P_S = 0, P_DTE = 0  # force no separation branch
  )

  recruitment_model <- list(method="power", period=12, power=1)

  out <- simulate_trial_with_recruitment(10,10,control_model,effect_model,recruitment_model)

  expect_true(nrow(out)==20)
})

test_that("simulate_trial_with_recruitment works: Weibull + Fixed + Landmark", {
  set.seed(99)

  control_model <- list(
    dist="Weibull",
    parameter_mode="Fixed",
    fixed_type="Landmark",
    t1=3, t2=6,
    surv_t1=0.8,
    surv_t2=0.6
  )

  effect_model <- list(
    P_S=1, P_DTE=0,  # force immediate separation branch
    HR_SHELF = SHELF::fitdist(c(0.6,0.7,0.8), probs=c(0.25,0.5,0.75), lower=0, upper=2),
    HR_dist="gamma"
  )

  recruitment_model <- list(method="power", period=10, power=1)

  out <- simulate_trial_with_recruitment(10,10,control_model,effect_model,recruitment_model)

  expect_true(nrow(out)==20)
})

test_that("simulate_trial_with_recruitment works: Weibull + Distribution", {
  set.seed(456)

  control_model <- list(
    dist="Weibull",
    parameter_mode="Distribution",
    t1=3, t2=6,
    t1_Beta_a=5, t1_Beta_b=5,
    diff_Beta_a=3, diff_Beta_b=3
  )

  effect_model <- list(
    P_S=1, P_DTE=1,  # force delayed separation branch
    delay_SHELF=SHELF::fitdist(c(3,4,5), probs=c(0.25,0.5,0.75), lower=0, upper=10),
    delay_dist="gamma",
    HR_SHELF=SHELF::fitdist(c(0.6,0.7,0.8), probs=c(0.25,0.5,0.75), lower=0, upper=2),
    HR_dist="gamma"
  )

  recruitment_model <- list(method="PWC", rate="5,5", duration="6,6")

  out <- simulate_trial_with_recruitment(12,12,control_model,effect_model,recruitment_model)

  expect_equal(nrow(out), 24)
})



