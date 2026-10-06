# Spec test 5: an exponential control arm is the Weibull case with gamma_c = 1.
test_that("PP_func: Exponential and Weibull(gamma = 1) agree", {
  fx <- readRDS(test_path("fixtures", "PP_func_equivalence.rds"))
  args <- fx$common
  post_exp <- fx$cases$exp_boundaries$posterior_df
  post_weib <- cbind(post_exp, gamma_c = 1)
  fb <- fx$cases$exp_boundaries$future_boundaries
  pp <- function(dist, post, seed, n) {
    set.seed(seed)
    args$n_sims <- n
    out <- do.call(PP_func, c(args, list(control_distribution = dist, posterior_df = post,
                                         future_boundaries = fb)))
    mean(out$PP_df$success)
  }

  # Same code path, so identical draws under the same seed
  expect_identical(pp("Exponential", post_exp, 5, 50), pp("Weibull", post_weib, 5, 50))

  # Independent runs agree within Monte Carlo error (n_sims = 2000)
  n <- 2000
  p_exp <- pp("Exponential", post_exp, 1, n)
  p_weib <- pp("Weibull", post_weib, 2, n)
  p <- (p_exp + p_weib) / 2
  se <- sqrt(2 * p * (1 - p) / n)
  expect_lt(abs(p_exp - p_weib), 4 * se)
})

test_that("PP_func requires n_sims", {
  fx <- readRDS(test_path("fixtures", "PP_func_equivalence.rds"))
  args <- fx$common; args$n_sims <- NULL
  expect_error(do.call(PP_func, c(args, fx$cases$exp_boundaries)), "'n_sims'")
})
