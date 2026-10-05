test_that("n_events_at floors with tolerance (spec test 1)", {
  expect_identical(n_events_at(840, c(0.3, 0.5, 0.6, 0.7, 0.75, 1)),
                   c(252L, 420L, 504L, 588L, 630L, 840L))
  expect_identical(n_events_at(100, 1/3), 33L)
})

test_that("cens_data at k events leaves exactly k events (spec test 2)", {
  set.seed(1)
  d <- sim_dte(100, 100, lambda_c = 0.1, delay_time = 2, post_delay_HR = 0.6)
  d <- add_recruitment_time(d, rec_method = "power", rec_period = 12, rec_power = 1)
  for (k in c(1, 37, 120, 200)) {
    cut <- cens_data(d, cens_method = "Events", cens_events = k)
    expect_equal(sum(cut$data$status), k)
    expect_true(all(c("time", "group", "rec_time", "pseudo_time", "status",
                      "survival_time") %in% names(cut$data)))
  }
  # non-integer event counts are floored with tolerance
  expect_equal(sum(cens_data(d, "Events", cens_events = 840 * 0.3 / 4.2)$data$status), 60)
  expect_equal(sum(cens_data(d, "Events", cens_events = 59.9999999999)$data$status), 60)
})

test_that("make_future_boundaries returns the looks strictly after from_IF", {
  design <- make_gsd_design(
    list(alpha_IF = c(0.75, 1), alpha_spending = c(0.0125, 0.025),
         futility_type = "PP", futility_IF = 0.5))$design
  fb <- make_future_boundaries(design, 840, 0.5)
  expect_length(fb, 2)
  expect_equal(vapply(fb, `[[`, numeric(1), "events"), c(630, 840))
  expect_equal(vapply(fb, `[[`, numeric(1), "crit"), design$criticalValues[2:3])
  expect_length(make_future_boundaries(design, 840, 0.75), 1)
})

test_that("rpact critical values at 0.75 and 1 do not change when a futility look is added (spec test 6)", {
  base <- list(alpha_IF = c(0.75, 1), alpha_spending = c(0.0125, 0.025))
  ref <- make_gsd_design(c(base, futility_type = "none"))$design
  for (f in c(0.3, 0.4, 0.5, 0.6, 0.7)) {
    d <- make_gsd_design(c(base, futility_type = "PP",
                                            futility_IF = f))$design
    expect_equal(d$criticalValues[d$informationRates %in% c(0.75, 1)],
                 ref$criticalValues, tolerance = 1e-6)
  }
})

test_that("survival_test(return_HR = FALSE) skips the Cox fit but keeps Z", {
  set.seed(2)
  d <- data.frame(survival_time = rexp(60, 0.1), status = rbinom(60, 1, 0.8),
                  group = rep(c("Control", "Treatment"), each = 30))
  a <- survival_test(d, "LRT", alpha = 0.025)
  b <- survival_test(d, "LRT", alpha = 0.025, return_HR = FALSE)
  expect_true(is.finite(a$observed_HR))
  expect_identical(b$observed_HR, NA_real_)
  expect_identical(a$Z, b$Z)
  expect_identical(a$Signif, b$Signif)
})

test_that("simulate_trial_with_recruitment honours force_state and records the truth", {
  s <- paired_test_setup()
  for (st in 1:3) {
    set.seed(st)
    d <- simulate_trial_with_recruitment(20, 20, s$control_model, s$effect_model,
                                         s$recruitment_model, force_state = st)
    tr <- attr(d, "truth")
    expect_equal(tr$state, st)
    expect_equal(tr$gamma_c, 1)
    if (st == 1) expect_equal(c(tr$delay_time, tr$post_delay_HR), c(0, 1))
    if (st == 2) expect_true(tr$delay_time == 0 && tr$post_delay_HR != 1)
    if (st == 3) expect_true(tr$delay_time > 0)
  }
  set.seed(1)
  d <- simulate_trial_with_recruitment(20, 20, s$control_model, s$effect_model,
                                       s$recruitment_model)
  expect_true(attr(d, "truth")$state %in% 1:3)
  expect_error(simulate_trial_with_recruitment(20, 20, s$control_model, s$effect_model,
                                               s$recruitment_model, force_state = 4))
})

test_that("PP code paths stop for non-uniform recruitment", {
  s <- paired_test_setup()
  pwc <- list(method = "PWC", rate = "10, 20", duration = "5, 5")
  expect_error(check_uniform_recruitment(pwc, "x"), "power = 1")
  expect_error(check_uniform_recruitment(list(method = "power", period = 12, power = 2), "x"),
               "power = 1")
  expect_silent(check_uniform_recruitment(s$recruitment_model, "x"))
  expect_error(
    single_paired_rep(1, 1, s$n_c, s$n_t, s$total_events, s$futility_IF,
                      design = s$design, control_model = s$control_model,
                      effect_model = s$effect_model, recruitment_model = pwc,
                      truth = s$truth, analysis_model_LRT = s$analysis_model_LRT,
                      update_priors_sims = 10, PP_sims = 5),
    "power = 1")
})
