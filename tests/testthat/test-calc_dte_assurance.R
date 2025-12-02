control_model = list(dist = "Exponential",
                     parameter_mode = "Distribution",
                     t1 = 12,
                     t1_Beta_a = 20,
                     t1_Beta_b = 32)
effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                    delay_dist = "gamma",
                    HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                    HR_dist = "gamma",
                    P_S = 1,
                    P_DTE = 0)

recruitment_model <- list(method = "power", period = 12, power = 1)

censoring_model <- list(
  method = "Time",
  time = 12
)

IA_model = list(events = 5, IF = 0.5)

analysis_model = list(method = "LRT",
                      alpha = 0.025,
                      alternative_hypothesis = "one.sided")


data_generating_model = list(lambda_c = log(2)/12,
                             delay_time = 3,
                             post_delay_HR = 0.75)




test_that("calc_dte_assurance returns expected structure", {

  result <- calc_dte_assurance(
    n_c = 50,
    n_t = 50,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = censoring_model,
    recruitment_model = recruitment_model,
    analysis_model = analysis_model,
    n_sims = 5
  )

  expect_type(result, "list")
  expect_named(result, c("assurance", "CI", "duration", "sample_size"))
  expect_type(result$assurance, "double")
  expect_length(result$CI, 2)
  expect_true(result$duration > 0)
  expect_true(result$sample_size > 0)
})

test_that("calc_dte_assurance handles Events censoring", {
  set.seed(1)

  out <- calc_dte_assurance(
    n_c = 10,
    n_t = 10,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = list(method = "Events", events = 5),
    recruitment_model = recruitment_model,
    analysis_model = analysis_model,
    n_sims = 2
  )

  expect_type(out, "list")
  expect_length(out$assurance, 1)
})

test_that("calc_dte_assurance handles IF censoring", {
  set.seed(1)

  out <- calc_dte_assurance(
    n_c = 10,
    n_t = 10,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = list(method = "IF", IF = 0.5),
    recruitment_model = recruitment_model,
    analysis_model = analysis_model,
    n_sims = 2
  )

  expect_type(out, "list")
})

test_that("calc_dte_assurance skips entries when loopVec is FALSE", {
  set.seed(1)

  out <- calc_dte_assurance(
    n_c = c(5, 20),
    n_t = c(5, 20),
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = list(method = "Events", events = 15),  # causes skip for j=1
    recruitment_model = recruitment_model,
    analysis_model = analysis_model,
    n_sims = 2
  )

  expect_true(is.na(out$assurance[1]))   # first row skipped
  expect_false(is.na(out$assurance[2]))  # second row not skipped
})

test_that("calc_dte_assurance handles success_threshold_HR logic", {
  set.seed(1)

  analysis_model_thresh <- modifyList(
    analysis_model,
    list(success_threshold_HR = 0.8)
  )

  out <- calc_dte_assurance(
    n_c = 10,
    n_t = 10,
    control_model = control_model,
    effect_model = effect_model,
    censoring_model = list(method = "Time", time = 12),
    recruitment_model = recruitment_model,
    analysis_model = analysis_model_thresh,
    n_sims = 2
  )

  expect_true("assurance_targetHR" %in% names(out))
  expect_true("CI_targetHR" %in% names(out))
})

