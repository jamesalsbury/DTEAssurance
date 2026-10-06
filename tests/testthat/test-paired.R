run_tiny <- function(s, n_sims = 20, ...) {
  suppressMessages(run_paired_scenario(
    n_sims = n_sims, seed = 7,
    n_c = s$n_c, n_t = s$n_t, total_events = s$total_events,
    futility_IF = s$futility_IF, efficacy_IF = s$efficacy_IF, design = s$design,
    control_model = s$control_model, effect_model = s$effect_model,
    recruitment_model = s$recruitment_model, truth = s$truth,
    analysis_model_LRT = s$analysis_model_LRT,
    update_priors_sims = 100, PP_sims = 50, ...))
}

test_that("run_paired_scenario: columns, no NAs, reproducible, resumable (spec test 7)", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()

  res <- run_tiny(s)
  raw <- res$raw
  expect_equal(nrow(raw), 20)
  expect_equal(names(raw), paired_rep_columns(c(2, 6)))
  ok <- is.na(raw$error)
  expect_true(all(ok))
  expect_false(anyNA(raw[ok, setdiff(names(raw), "error")]))
  expect_equal(raw$rep_id, 1:20)
  expect_equal(raw$seed_used, 7 * 1e5 + 1:20)
  expect_equal(res$settings$n_failed, 0)
  expect_true(all(c("package_version", "r_version", "rjags_version", "jags_version",
                    "git_commit", "hostname", "timestamp") %in%
                    names(res$settings)))

  # the interim cut has floor(0.5 * 80) = 40 events, efficacy 60, final 80
  expect_true(all(raw$t_int < raw$t_eff & raw$t_eff < raw$t_fin))
  expect_true(all(raw$n_int <= raw$n_eff & raw$n_eff <= raw$n_fin))

  # same seed -> identical raw
  expect_identical(run_tiny(s)$raw, raw)

  # checkpointing: stop after the first chunk, then resume
  ck <- tempfile(fileext = ".rds")
  local({
    part <- run_tiny(s, n_sims = 8, checkpoint_file = ck, chunk_size = 8)
    expect_equal(readRDS(ck)$raw$rep_id, 1:8)
  })
  expect_message(resumed <- run_paired_scenario(
    n_sims = 20, seed = 7,
    n_c = s$n_c, n_t = s$n_t, total_events = s$total_events,
    futility_IF = s$futility_IF, efficacy_IF = s$efficacy_IF, design = s$design,
    control_model = s$control_model, effect_model = s$effect_model,
    recruitment_model = s$recruitment_model, truth = s$truth,
    analysis_model_LRT = s$analysis_model_LRT,
    update_priors_sims = 100, PP_sims = 50,
    checkpoint_file = ck, chunk_size = 5), "resuming from 8")
  expect_identical(resumed$raw, raw)
  # a checkpoint written with another seed is refused
  expect_error(run_paired_scenario(n_sims = 20, seed = 8, checkpoint_file = ck),
               "written with seed = 7")
  unlink(ck)
})

test_that("compute_PP = FALSE leaves every non-PP column unchanged", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  pp_cols <- c("PP_val", "converged", "rhat_max", "n_nonfinite_rhat",
               "P_Z1", "P_Z2", "P_Z3")

  with_pp <- run_tiny(s, n_sims = 5)
  no_pp <- run_tiny(s, n_sims = 5, compute_PP = FALSE)
  expect_true(all(is.na(with_pp$raw$error)))
  expect_true(all(is.na(no_pp$raw$error)))
  expect_equal(names(no_pp$raw), names(with_pp$raw))

  other <- setdiff(names(with_pp$raw), pp_cols)
  expect_identical(no_pp$raw[other], with_pp$raw[other])
  expect_true(all(is.na(no_pp$raw[pp_cols])))
  expect_false(anyNA(with_pp$raw[pp_cols]))
  expect_identical(with_pp$settings$compute_PP, TRUE)
  expect_identical(no_pp$settings$compute_PP, FALSE)

  # update_priors_sims / PP_sims are not needed without PP
  row <- single_paired_rep(
    i = 1, seed = 7, n_c = s$n_c, n_t = s$n_t, total_events = s$total_events,
    futility_IF = s$futility_IF, efficacy_IF = s$efficacy_IF, design = s$design,
    control_model = s$control_model, effect_model = s$effect_model,
    recruitment_model = s$recruitment_model, truth = s$truth,
    analysis_model_LRT = s$analysis_model_LRT, compute_PP = FALSE)
  expect_identical(row[other], no_pp$raw[1, other])

  # D2 decisions agree; D3 refuses PP-free output
  b <- list(type = "D2", crit_eff = crit_at(s$design, 0.75), crit_fin = crit_at(s$design, 1))
  expect_identical(apply_design_rule(no_pp$raw, b), apply_design_rule(with_pp$raw, b))
  expect_error(apply_design_rule(no_pp$raw, c(b[-1], type = "D3", kappa = 0.1)),
               "compute_PP = FALSE")

  # a checkpoint is not resumed with a different compute_PP
  ck <- tempfile(fileext = ".rds")
  run_tiny(s, n_sims = 2, compute_PP = FALSE, checkpoint_file = ck)
  expect_error(run_tiny(s, n_sims = 4, checkpoint_file = ck), "compute_PP = FALSE")
  unlink(ck)
})

test_that("summarize_grid_by_kappa(raw, 0) reproduces D2 power from the Z columns (spec test 8)", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  raw <- run_tiny(s)$raw
  c_eff <- s$design$criticalValues[s$design$informationRates == 0.75]
  c_fin <- s$design$criticalValues[s$design$informationRates == 1]
  d2 <- mean(raw$Z_eff_LRT > c_eff | raw$Z_fin_LRT > c_fin)
  expect_equal(summarize_grid_by_kappa(raw, 0)$power_or_typeI, d2)
  expect_equal(summarize_grid_by_kappa(raw, 0)$P_early_fut, 0)
})

test_that("a failing replicate becomes an error row rather than aborting", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()

  # failure inside single_paired_rep is caught there
  local({
    local_mocked_bindings(PP_func = function(...) stop("injected PP failure"))
    row <- single_paired_rep(1, 7, s$n_c, s$n_t, s$total_events, s$futility_IF,
                             design = s$design, control_model = s$control_model,
                             effect_model = s$effect_model,
                             recruitment_model = s$recruitment_model, truth = s$truth,
                             analysis_model_LRT = s$analysis_model_LRT,
                             update_priors_sims = 50, PP_sims = 5)
    expect_equal(names(row), paired_rep_columns(c(2, 6)))
    expect_equal(row$error, "injected PP failure")
    expect_true(is.na(row$PP_val))
  })

  # failure of a whole replicate is caught by run_paired_scenario
  orig <- single_paired_rep
  local_mocked_bindings(single_paired_rep = function(i, ...) {
    if (i == 3) stop("injected replicate failure")
    orig(i, ...)
  })
  expect_warning(res <- run_tiny(s, n_sims = 5), "more than 1%")
  expect_equal(nrow(res$raw), 5)
  expect_equal(which(!is.na(res$raw$error)), 3L)
  expect_equal(res$raw$error[3], "injected replicate failure")
  expect_equal(res$settings$n_failed, 1)
})

test_that("single_paired_rep with truth = NULL uses force_state and records the truth", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  row <- single_paired_rep(1, 3, s$n_c, s$n_t, s$total_events, s$futility_IF,
                           design = s$design, control_model = s$control_model,
                           effect_model = s$effect_model,
                           recruitment_model = s$recruitment_model,
                           truth = NULL, force_state = 3,
                           analysis_model_LRT = s$analysis_model_LRT,
                           update_priors_sims = 50, PP_sims = 5)
  expect_true(is.na(row$error))
  expect_equal(row$state, 3L)
  expect_true(row$true_delay > 0)
  expect_equal(row$true_gamma_c, 1)
})
