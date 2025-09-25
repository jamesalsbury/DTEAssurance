test_that("summarize_gsd_results returns expected structure and values", {
  gsd_outcomes <- list(
    list(decision = "Stop for efficacy", stop_time = 15.2, sample_size = 80),
    list(decision = "Successful at final", stop_time = 120.5, sample_size = 100),
    list(decision = "Unsuccessful at final", stop_time = 110.3, sample_size = 100),
    list(decision = "Stop for futility", stop_time = 20.1, sample_size = 85),
    list(decision = "Stop for efficacy", stop_time = 18.7, sample_size = 90)
  )

  result <- summarize_gsd_results(gsd_outcomes)

  expect_type(result, "list")
  expect_named(result, c("assurance", "expected_duration", "expected_sample_size"))
  expect_type(result$assurance, "double")
  expect_type(result$expected_duration, "double")
  expect_type(result$expected_sample_size, "double")
  expect_true(result$assurance > 0)
  expect_true(result$expected_duration > 0)
  expect_true(result$expected_sample_size > 0)
})
