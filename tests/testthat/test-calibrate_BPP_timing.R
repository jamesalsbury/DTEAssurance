test_that("calibrate_BPP_timing returns correct structure with mocked single_calibration_rep", {

  # ---- Correctly mocked function (first argument = iter index) ----
  fake_single_rep <- function(iter_index,
                              n_c, n_t,
                              control_model,
                              effect_model,
                              recruitment_model,
                              total_events,
                              IF,
                              analysis_model) {

    BPP_df <- data.frame(
      success = rep(IF > 0.5, 3), # deterministic
      Z_val   = c(2, 2, 2)
    )

    list(
      BPP_outcome = list(BPP_df = BPP_df),
      cens_time   = IF * 100
    )
  }

  # ---- Minimal stub inputs ----
  n_c <- 10
  n_t <- 10

  control_model <- list(dist = "Exponential")
  effect_model   <- list(P_S = 0.5, P_DTE = 0.5)
  recruitment_model <- list(method = "power")

  IA_model <- list(events = 200, IF = c(0.3, 0.6))

  analysis_model <- list(
    method = "LRT",
    alpha = 0.025,
    alternative_hypothesis = "one.sided"
  )

  out <- testthat::with_mocked_bindings(
    single_calibration_rep = fake_single_rep,
    {
      calibrate_BPP_timing(
        n_c = n_c, n_t = n_t,
        control_model = control_model,
        effect_model = effect_model,
        recruitment_model = recruitment_model,
        IA_model = IA_model,
        analysis_model = analysis_model,
        n_sims = 5
      )
    }
  )

  # ---- Structural tests ----
  expect_true(is.list(out))
  expect_true("outcome_list" %in% names(out))

  outcome_list <- out$outcome_list
  expect_equal(length(outcome_list), 2)

  expect_true(all(c("BPP_values", "cens_time") %in% names(outcome_list[[1]])))
  expect_true(all(c("BPP_values", "cens_time") %in% names(outcome_list[[2]])))

  expect_equal(length(outcome_list[[1]]$BPP_values), 5)
  expect_equal(length(outcome_list[[2]]$BPP_values), 5)

  # IF = 0.3 → success = FALSE
  expect_true(all(outcome_list[[1]]$BPP_values == 0))
  expect_true(all(outcome_list[[1]]$cens_time == 30))

  # IF = 0.6 → success = TRUE
  expect_true(all(outcome_list[[2]]$BPP_values == 1))
  expect_true(all(outcome_list[[2]]$cens_time == 60))
})
