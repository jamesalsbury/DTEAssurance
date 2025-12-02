test_that("make_prior_name_jags works for gamma", {
  fit <- list(
    Gamma = data.frame(shape = 2.5, rate = 0.7)
  )
  out <- make_prior_name_jags(fit, "gamma")
  expect_equal(out, "dgamma(2.500000, 0.700000)")
})

test_that("make_prior_name_jags works for beta", {
  fit <- list(
    Beta = data.frame(shape1 = 3, shape2 = 4)
  )
  out <- make_prior_name_jags(fit, "beta")
  expect_equal(out, "dbeta(3.000000, 4.000000)")
})

test_that("make_prior_name_jags works for normal", {
  fit <- list(
    Normal = data.frame(mean = 1.1, sd = 0.5)
  )
  out <- make_prior_name_jags(fit, "normal")
  # tau = 1 / sd^2 = 4
  expect_equal(out, "dnorm(1.100000, 4.000000)")
})

test_that("make_prior_name_jags works for student.t", {
  fit <- list(
    Student.t = data.frame(location = 0.2, scale = 0.3, df = 7)
  )
  # tau = 1 / scale^2 = 11.111111
  out <- make_prior_name_jags(fit, "student.t")
  expect_equal(out, sprintf("dt(0.200000, %0.6f, 7.000000)", 1/0.3^2))
})

test_that("make_prior_name_jags works for lognormal and log.normal", {
  fit <- list(
    Log.normal = data.frame(mean.log.X = 0.4, sd.log.X = 0.2)
  )

  out1 <- make_prior_name_jags(fit, "lognormal")
  out2 <- make_prior_name_jags(fit, "log.normal")

  tau <- 1 / (0.2^2)

  expect_equal(out1, sprintf("dlnorm(0.400000, %0.6f)", tau))
  expect_equal(out2, sprintf("dlnorm(0.400000, %0.6f)", tau))
})

test_that("make_prior_name_jags gives informative error for unsupported distribution", {
  fit <- list()
  expect_error(
    make_prior_name_jags(fit, "unsupported"),
    "cannot be represented in JAGS"
  )
})
