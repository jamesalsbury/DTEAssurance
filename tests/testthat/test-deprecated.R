test_that("deprecated BPP/lambda functions warn and forward to the PP/kappa versions", {
  local_mocked_bindings(
    PP_func = function(...) "PP_func",
    calibrate_PP_threshold = function(...) "calibrate_PP_threshold",
    calibrate_PP_timing = function(...) "calibrate_PP_timing",
    select_kappa_star = function(...) "select_kappa_star"
  )
  expect_warning(out <- BPP_func(1), class = "deprecatedWarning")
  expect_equal(out, "PP_func")
  expect_warning(out <- calibrate_BPP_threshold(1), class = "deprecatedWarning")
  expect_equal(out, "calibrate_PP_threshold")
  expect_warning(out <- calibrate_BPP_timing(1), class = "deprecatedWarning")
  expect_equal(out, "calibrate_PP_timing")
  expect_warning(out <- select_lambda_star(1), class = "deprecatedWarning")
  expect_equal(out, "select_kappa_star")

  raw <- data.frame(PP_val = c(0.1, 0.5), continuation_success = c(1, 1),
                    sample_size_interim = 10, continuation_sample_size = 20,
                    t_interim = 1, continuation_stop_time = 2)
  expect_warning(out <- summarize_grid_by_lambda(raw, c(0, 0.3)),
                 class = "deprecatedWarning")
  expect_equal(out, summarize_grid_by_kappa(raw, c(0, 0.3)))
})

test_that("futility_type 'BPP' and BPP_threshold are mapped to 'PP' and kappa with warnings", {
  G <- list(futility_type = "BPP", BPP_threshold = 0.2)
  expect_warning(
    expect_warning(G2 <- normalise_futility_spec(G), "'BPP' is deprecated; use 'PP'"),
    "BPP_threshold is deprecated"
  )
  expect_equal(G2$futility_type, "PP")
  expect_equal(G2$kappa, 0.2)
  expect_null(G2$BPP_threshold)

  G <- list(futility_type = "PP", kappa = 0.3)
  expect_no_warning(expect_identical(normalise_futility_spec(G), G))

  base <- list(alpha_IF = c(0.75, 1), alpha_spending = c(0.0125, 0.025),
               futility_IF = 0.5)
  expect_warning(
    old <- make_rpact_design_from_GSD_model(c(base, futility_type = "BPP")),
    "'BPP' is deprecated"
  )
  new <- make_rpact_design_from_GSD_model(c(base, futility_type = "PP"))
  expect_equal(old$IF_all, new$IF_all)
  expect_equal(old$design$criticalValues, new$design$criticalValues)
})
