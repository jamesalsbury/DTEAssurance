test_that("apply_GSD_to_trial returns expected structure", {
  set.seed(123)

  trial_data <- data.frame(
    time = rexp(100, rate = 0.1),
    status = rbinom(100, 1, 0.9),
    group = rep(c("Control", "Treatment"), each = 50),
    rec_time = runif(100, 0, 12)
  )
  trial_data$pseudo_time <- trial_data$time + trial_data$rec_time

  design <- list(
    informationRates = c(0.5, 1),
    criticalValues = c(1.96, 1.96),
    futilityBounds = c(-0.5, NA)
  )

  result <- apply_GSD_to_trial(trial_data, design, total_events = 100)

  expect_type(result, "list")
  expect_named(result, c("decision", "stop_time", "sample_size"))
  expect_type(result$decision, "character")
  expect_type(result$stop_time, "double")
  expect_type(result$sample_size, "integer")
  expect_true(result$sample_size > 0)
})
