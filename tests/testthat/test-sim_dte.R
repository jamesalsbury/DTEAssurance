test_that("sim_dte returns correct structure", {
  df <- sim_dte(10, 10, lambda_c = 0.1, delay_time = 6, post_delay_HR = 0.6)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("time", "group") %in% names(df)))
  expect_equal(nrow(df), 20)
})

test_that("sim_dte generates Weibull survival times for control and treatment", {
  set.seed(123)

  out <- sim_dte(
    n_c = 5,
    n_t = 5,
    lambda_c = 0.1,
    delay_time = 2,
    post_delay_HR = 0.7,
    dist = "Weibull",
    gamma_c = 1.5
  )

  # Structure checks
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 10)
  expect_true("time" %in% names(out))
  expect_true("group" %in% names(out))

  # Groups should be correctly assigned
  expect_equal(sum(out$group == "Control"), 5)
  expect_equal(sum(out$group == "Treatment"), 5)

  # Times must be positive (Weibull guarantees this)
  expect_true(all(out$time > 0))

  # Variation in time values verifies non-degenerate simulation
  expect_true(var(out$time) > 0)
})

