interim_for_priors <- function() {
  set.seed(11)
  d <- sim_dte(40, 40, lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.6)
  d <- add_recruitment_time(d, rec_method = "power", rec_period = 12, rec_power = 1)
  cens_data(d, cens_method = "Events", cens_events = 40)$data
}

test_that("update_priors is reproducible with jags_seed (spec test 3)", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  d <- interim_for_priors()
  run <- function(seed) update_priors(d, s$control_model, s$effect_model,
                                      n_burnin = 100, n_samples = 100, jags_seed = seed)
  a <- run(123); b <- run(123); c <- run(456)
  expect_identical(as.data.frame(a), as.data.frame(b))
  expect_false(identical(as.data.frame(a), as.data.frame(c)))
  expect_equal(nrow(a), 200)                       # n.chains * n_samples
  expect_equal(attr(a, "n_retained_total"), 200)
  expect_true(all(c("rhat_max", "n_nonfinite_rhat") %in% names(attributes(a))))
})

test_that("converged is NA, not TRUE, when every R-hat is non-finite (spec test 4)", {
  expect_identical(rhat_summary(c(a = NaN, b = NA), 1.1)$converged, NA)
  expect_identical(rhat_summary(c(a = NaN, b = NA), 1.1)$n_nonfinite_rhat, 2L)
  expect_identical(rhat_summary(c(a = NaN, b = NA), 1.1)$rhat_max, NA_real_)
  # NaN for a constant parameter is counted, not treated as a failure
  r <- rhat_summary(c(HR = 1.01, delay_time = NaN), 1.1)
  expect_true(r$converged); expect_equal(r$rhat_max, 1.01); expect_equal(r$n_nonfinite_rhat, 1L)
  expect_false(rhat_summary(c(HR = 1.3, delay_time = NaN), 1.1)$converged)

  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  local_mocked_bindings(
    gelman.diag = function(x, ...) {
      nm <- colnames(as.matrix(x))
      list(psrf = matrix(NaN, length(nm), 2, dimnames = list(nm, c("Point est.", "Upper C.I."))))
    },
    .package = "coda"
  )
  post <- update_priors(interim_for_priors(), s$control_model, s$effect_model,
                        n_burnin = 50, n_samples = 50, jags_seed = 1)
  expect_identical(attr(post, "converged"), NA)
  expect_equal(attr(post, "n_nonfinite_rhat"), 4)
  expect_identical(attr(post, "rhat_max"), NA_real_)
})
