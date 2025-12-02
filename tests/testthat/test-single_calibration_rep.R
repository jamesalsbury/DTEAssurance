test_that("single_calibration_rep returns correctly structured output", {

  # --- create lightweight mock versions of heavy functions ---
  fake_data <- data.frame(
    time = c(1, 2, 1),
    group = c("Control", "Treatment", "Treatment"),
    rec_time = c(0, 0, 0),
    pseudo_time = c(1, 2, 1),
    status = c(1, 1, 1),
    survival_time = c(1, 2, 1)
  )

  fake_posterior <- data.frame(
    lambda_c = rep(0.1, 5),
    delay_time = rep(0, 5),
    HR = rep(0.7, 5)
  )

  fake_BPP <- list(BPP_df = data.frame(success = 1, Z_val = 2))

  local_mocked_bindings(
    simulate_trial_with_recruitment = function(...) fake_data,
    update_priors = function(...) fake_posterior,
    BPP_func = function(...) fake_BPP
  )

  control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
                        fixed_type = "Parameters", lambda = 0.1)
  effect_model <- list(P_S = 1, P_DTE = 0,
                       delay_SHELF = NULL, HR_SHELF = NULL)
  recruitment_model <- list(method = "power", period = 12, power = 1)
  analysis_model <- list(method = "LRT", alpha = 0.025,
                         alternative_hypothesis = "one.sided")

  out <- single_calibration_rep(
    i = 1,
    n_c = 2,
    n_t = 1,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model,
    total_events = 2,
    IF = 1,
    analysis_model = analysis_model
  )

  # --- structural checks ---
  expect_type(out, "list")
  expect_true("BPP_outcome" %in% names(out))
  expect_true("cens_time" %in% names(out))

  expect_true(is.list(out$BPP_outcome))
  expect_true(is.numeric(out$cens_time))
})
