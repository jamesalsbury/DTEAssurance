make_surv_df <- function(n = 40) {
  data.frame(
    survival_time = rexp(n, rate = 0.1),
    status = rbinom(n, 1, 0.8),
    group = rep(c("Control", "Treatment"), each = n / 2)
  )
}

test_that("survival_test works for WLRT, one-sided", {
  skip_if_not_installed("nph")

  set.seed(123)
  df <- make_surv_df(40)

  res <- survival_test(
    data = df,
    analysis_method = "WLRT",
    alternative = "one.sided",
    alpha = 0.05,
    rho = 0,
    gamma = 1
  )

  expect_type(res, "list")
  expect_true(all(c("Signif", "observed_HR", "Z") %in% names(res)))
  expect_false(is.na(res$observed_HR))
  expect_true(is.logical(res$Signif) || res$Signif %in% c(0, 1))  # depending on how you interpret Signif
})

test_that("survival_test works for MW, two-sided", {
  skip_if_not_installed("nphRCT")

  set.seed(123)
  df <- make_surv_df(40)

  res <- survival_test(
    data = df,
    analysis_method = "MW",
    alternative = "two.sided",
    alpha = 0.05,
    t_star = 6
  )

  expect_type(res, "list")
  expect_true(all(c("Signif", "observed_HR", "Z") %in% names(res)))
  expect_false(is.na(res$observed_HR))
  expect_true(is.logical(res$Signif) || res$Signif %in% c(0, 1))
})



test_that("survival_test returns expected structure with LRT", {
  set.seed(123)
  df <- data.frame(
    survival_time = rexp(40, rate = 0.1),
    status = rbinom(40, 1, 0.8),
    group = rep(c("Control", "Treatment"), each = 20)
  )

  result <- survival_test(df, analysis_method = "LRT", alpha = 0.05, alternative = "one.sided")

  expect_type(result, "list")
  expect_named(result, c("Signif", "observed_HR", "Z"))
  expect_type(result$Signif, "logical")
  expect_type(result$observed_HR, "double")
  expect_type(result$Z, "double")
  expect_true(result$observed_HR > 0)
})

