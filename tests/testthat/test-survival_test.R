test_that("survival_test returns expected structure with LRT", {
  set.seed(123)
  df <- data.frame(
    survival_time = rexp(40, rate = 0.1),
    status = rbinom(40, 1, 0.8),
    group = rep(c("Control", "Treatment"), each = 20)
  )

  result <- survival_test(df, analysis_method = "LRT", alpha = 0.05, alternative = "one.sided")

  expect_type(result, "list")
  expect_named(result, c("Signif", "observed_HR"))
  expect_type(result$Signif, "logical")
  expect_type(result$observed_HR, "double")
  expect_true(result$observed_HR > 0)
})

