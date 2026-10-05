# Regression test for the BPP -> PP rename: PP_func() must reproduce the
# output of BPP_func() from v1.2.0-submission1 exactly (same seed, same
# inputs). See fixtures/make_PP_func_equivalence.R.

test_that("PP_func reproduces pre-rename BPP_func output", {
  fx <- readRDS(test_path("fixtures", "PP_func_equivalence.rds"))

  for (nm in names(fx$cases)) {
    set.seed(1)
    out <- suppressMessages(do.call(PP_func, c(fx$common, fx$cases[[nm]])))
    expect_identical(out$PP_df$success, fx$outputs[[nm]]$success, label = nm)
    expect_identical(out$PP_df$Z_val, fx$outputs[[nm]]$Z_val, label = nm)
  }
})
