test_that("apply_GSD_to_trial returns expected structure", {

  set.seed(123)

  # --- trial data ---
  trial_data <- data.frame(
    time       = rexp(100, rate = 0.1),
    status     = rbinom(100, 1, 0.9),
    group      = rep(c(0, 1), each = 50),
    rec_time   = runif(100, 0, 12)
  )
  trial_data$pseudo_time <- trial_data$time + trial_data$rec_time

  # --- group sequential design ---
  design <- list(
    informationRates = c(0.5, 1),
    criticalValues   = c(1.96, 1.96),
    futilityBounds   = c(-0.5, NA)
  )

  # --- GSD model (minimal; no BPP mode) ---
  GSD_model <- list(
    futility_type = "none"  # avoids futility blocks in function
  )

  # --- mock models so that BPP code paths are skipped safely ---
  control_model    <- NULL
  effect_model     <- NULL
  recruitment_model <- list(period = 12)
  analysis_model    <- NULL

  # --- call function with full argument set ---
  result <- apply_GSD_to_trial(
    n_c = 50,
    n_t = 50,
    trial_data = trial_data,
    design = design,
    total_events = 100,
    GSD_model = GSD_model,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model,
    analysis_model = analysis_model,
    n_BPP_sims = 10
  )

  # --- validation ---
  expect_type(result, "list")
  expect_named(result, c("decision", "stop_time", "sample_size"))

  expect_type(result$decision, "character")
  expect_true(is.numeric(result$stop_time))
  expect_true(is.numeric(result$sample_size))

  # Structural sanity checks
  expect_true(result$sample_size > 0)
  expect_false(is.na(result$decision))
})
