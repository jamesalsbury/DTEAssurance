# Equivalence tests for the step 2 refactors (items 2 and 4).

test_that("simulate_trial_from_truth() matches the previous inline code (item 2)", {
  rec <- list(method = "power", period = 12, power = 1)
  truths <- list(
    exponential = list(lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.7),
    weibull = list(lambda_c = 0.0745, gamma_c = 1.211, delay_time = 4, post_delay_HR = 0.6)
  )
  for (tr in truths) {
    set.seed(1)
    new <- simulate_trial_from_truth(tr, 50, 60, rec)

    set.seed(1)
    if (is.null(tr$gamma_c)) {
      old <- sim_dte(50, 60, tr$lambda_c, delay_time = tr$delay_time,
                     post_delay_HR = tr$post_delay_HR, dist = "Exponential")
    } else {
      old <- sim_dte(50, 60, tr$lambda_c, delay_time = tr$delay_time,
                     post_delay_HR = tr$post_delay_HR, dist = "Weibull",
                     gamma_c = tr$gamma_c)
    }
    old <- add_recruitment_time(old, rec_method = rec$method, rec_period = rec$period,
                                rec_power = rec$power, rec_rate = rec$rate,
                                rec_duration = rec$duration)
    expect_identical(new, old)
  }
})

test_that("weibull_from_landmarks() agrees with the previous nleqslv solution (item 4)", {
  skip_if_not_installed("nleqslv")
  t1 <- 8; t2 <- 12
  grid <- expand.grid(s1 = seq(0.3, 0.8, by = 0.05), delta = seq(0.05, 0.3, by = 0.05))
  grid <- grid[grid$s1 - grid$delta > 0, ]
  for (k in seq_len(nrow(grid))) {
    s1 <- grid$s1[k]; s2 <- s1 - grid$delta[k]
    # previous code (Distribution mode): solve for the scale 1 / lambda
    sol <- nleqslv::nleqslv(c(10, 1), function(p) {
      c(exp(-(t1 / p[1])^p[2]) - s1, exp(-(t2 / p[1])^p[2]) - s2)
    })
    wb <- weibull_from_landmarks(s1, s2, t1, t2)
    expect_equal(sol$termcd, 1)
    expect_equal(wb$lambda, 1 / sol$x[1], tolerance = 1e-6)
    expect_equal(wb$gamma, sol$x[2], tolerance = 1e-6)
  }
})

test_that("weibull_from_landmarks() round-trips the manuscript control parameters (item 4)", {
  lambda_c <- 0.0745; gamma_c <- 1.211
  S <- function(t) exp(-(lambda_c * t)^gamma_c)
  wb <- weibull_from_landmarks(S(8), S(12), 8, 12)
  expect_equal(wb$lambda, lambda_c, tolerance = 1e-10)
  expect_equal(wb$gamma, gamma_c, tolerance = 1e-10)
  # the formula matches the update_priors() JAGS prior for (s1, delta)
  s1 <- S(8); delta <- S(8) - S(12)
  expect_equal(log(log(s1) / log(s1 - delta)) / log(8 / 12), wb$gamma)
  expect_equal((-log(s1))^(1 / wb$gamma) / 8, wb$lambda)
})

test_that("weibull_from_landmarks() gives an informative error for invalid input (item 4)", {
  expect_error(weibull_from_landmarks(0.4, 0.5, 8, 12), "need 1 > s1 > s2 > 0")
  expect_error(weibull_from_landmarks(0.5, 0.5, 8, 12), "need 1 > s1 > s2 > 0")
  expect_error(weibull_from_landmarks(0.5, -0.1, 8, 12), "need 1 > s1 > s2 > 0")
  expect_error(weibull_from_landmarks(1, 0.5, 8, 12), "need 1 > s1 > s2 > 0")
  expect_error(weibull_from_landmarks(0.5, 0.4, 12, 8), "0 < t1 < t2")
  expect_error(weibull_from_landmarks(NA, 0.4, 8, 12), "need")
})

test_that("simulate_trial_with_recruitment() Weibull landmarks use the closed form (item 4)", {
  cm <- list(dist = "Weibull", parameter_mode = "Fixed", fixed_type = "Landmark",
             t1 = 8, t2 = 12, surv_t1 = exp(-(0.0745 * 8)^1.211),
             surv_t2 = exp(-(0.0745 * 12)^1.211))
  em <- list(P_S = 0, P_DTE = 0)
  set.seed(1)
  d <- simulate_trial_with_recruitment(10, 10, cm, em,
                                       list(method = "power", period = 12, power = 1))
  expect_equal(attr(d, "truth")$lambda_c, 0.0745, tolerance = 1e-10)
  expect_equal(attr(d, "truth")$gamma_c, 1.211, tolerance = 1e-10)
})
