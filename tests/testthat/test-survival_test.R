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
  expect_type(res$Signif, "logical")
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
  expect_type(res$Signif, "logical")
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


test_that("LRT, WLRT and MW all give positive Z for a clear treatment benefit", {
  skip_if_not_installed("nph")
  skip_if_not_installed("nphRCT")

  set.seed(1)
  df <- sim_dte(500, 500, lambda_c = log(2) / 12, delay_time = 0,
                post_delay_HR = 0.2)
  df <- add_recruitment_time(df, rec_method = "power", rec_period = 12,
                             rec_power = 1)
  df <- cens_data(df, cens_method = "Events", cens_events = 300)$data

  Z_LRT  <- survival_test(df, analysis_method = "LRT", alpha = 0.025)$Z
  Z_WLRT <- survival_test(df, analysis_method = "WLRT", alpha = 0.025, rho = 0, gamma = 0)$Z
  Z_MW   <- survival_test(df, analysis_method = "MW", alpha = 0.025, t_star = 12)$Z

  expect_gt(Z_LRT, 5)
  expect_gt(Z_WLRT, 5)
  expect_gt(Z_MW, 5)
  # WLRT with rho = gamma = 0 is the standard log-rank test
  expect_equal(Z_WLRT, Z_LRT, tolerance = 1e-6)
})


# Sign convention: positive Z = benefit for all three methods (the MW
# statistic from nphRCT::wlrt() is negated inside survival_test()).
test_that("all three methods give Z > 0 and similar magnitude for a large benefit", {
  skip_if_not_installed("nph")
  skip_if_not_installed("nphRCT")
  set.seed(42)
  df <- sim_dte(300, 300, lambda_c = log(2) / 12, delay_time = 0, post_delay_HR = 0.25)
  df <- add_recruitment_time(df, rec_method = "power", rec_period = 12, rec_power = 1)
  df <- cens_data(df, cens_method = "Events", cens_events = 400)$data

  Z <- c(LRT  = survival_test(df, "LRT", alpha = 0.025)$Z,
         WLRT = survival_test(df, "WLRT", alpha = 0.025, rho = 0, gamma = 1)$Z,
         MW   = survival_test(df, "MW", alpha = 0.025, t_star = 6)$Z)
  expect_true(all(Z > 0))
  expect_lt(max(Z) / min(Z), 1.3)
})

test_that("survival_test: alpha is required and Signif is always logical", {
  set.seed(1)
  df <- make_surv_df(40)
  expect_error(survival_test(df, "LRT"), "'alpha' must be supplied")
  expect_identical(survival_test(df, "LRT", alpha = NULL)$Signif, NA)
  expect_type(survival_test(df, "LRT", alpha = 0.05)$Signif, "logical")
  expect_type(survival_test(df, "LRT", alpha = 0.05, alternative = "two.sided")$Signif, "logical")
  # an unknown method gives Z = NA and Signif = FALSE
  r <- survival_test(df, "unknown", alpha = 0.05, return_HR = FALSE)
  expect_true(is.na(r$Z))
  expect_identical(r$Signif, FALSE)
})

test_that("run_test() equals the direct survival_test() call (item 1)", {
  set.seed(3)
  df <- make_surv_df(80)
  ams <- list(
    list(method = "LRT", alpha = 0.025, alternative_hypothesis = "one.sided"),
    list(method = "WLRT", alpha = 0.025, alternative_hypothesis = "one.sided", rho = 0, gamma = 1),
    list(method = "MW", alpha = 0.05, alternative_hypothesis = "two.sided", t_star = 6)
  )
  for (am in ams) {
    for (hr in c(TRUE, FALSE)) {
      direct <- survival_test(df, analysis_method = am$method, alpha = am$alpha,
                              alternative = am$alternative_hypothesis,
                              rho = am$rho, gamma = am$gamma,
                              t_star = am$t_star, s_star = am$s_star,
                              return_HR = hr)
      expect_identical(run_test(df, am, return_HR = hr), direct)
    }
  }
})
