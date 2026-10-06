# Regression test: PP_func() must reproduce the output of BPP_func() from
# v1.2.0-submission1 exactly (same seed, same inputs) for a Weibull control
# arm. See fixtures/make_PP_func_equivalence.R. The Exponential cases in the
# fixture are not checked: from 1.3.0 an exponential control arm runs the
# Weibull code path with gamma_c = 1 (fixing the pre-delay branch for
# censored treated patients), which changes the random draws.

test_that("PP_func reproduces v1.2.0 BPP_func output (Weibull control)", {
  fx <- readRDS(test_path("fixtures", "PP_func_equivalence.rds"))

  for (nm in "weib_boundaries") {
    set.seed(1)
    out <- suppressMessages(do.call(PP_func, c(fx$common, fx$cases[[nm]])))
    expect_identical(out$PP_df$success, fx$outputs[[nm]]$success, label = nm)
    expect_identical(out$PP_df$Z_val, fx$outputs[[nm]]$Z_val, label = nm)
  }
})
