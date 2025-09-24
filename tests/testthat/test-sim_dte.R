test_that("sim_dte returns correct structure", {
  df <- sim_dte(10, 10, lambda_c = 0.1, delay_time = 6, post_delay_HR = 0.6)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("time", "group") %in% names(df)))
  expect_equal(nrow(df), 20)
})
