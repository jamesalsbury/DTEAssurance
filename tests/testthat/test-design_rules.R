synthetic_raw <- function() {
  data.frame(
    rep_id = 1:6, error = c(rep(NA_character_, 5), "boom"),
    PP_val = c(0.05, 0.5, 0.5, 0.5, NA, 0.5),
    Z_int_LRT = c(-1, 0.5, 0.5, 0.5, NA, 1), Z_int_MW_t6 = c(1, -1, 1, 1, 1, 1),
    Z_eff_LRT = c(3, 3, 1, NA, 1, 1), Z_eff_MW_t6 = c(3, 3, 3, 1, 1, 1),
    Z_fin_LRT = c(3, 3, 2.5, 1, 2.5, 1), Z_fin_MW_t6 = c(1, 1, 1, 1, 1, 1),
    n_int = 10L, n_eff = 20L, n_fin = 30L, t_int = 1, t_eff = 2, t_fin = 3
  )
}

test_that("apply_design_rule: decisions, labels and outputs on known inputs", {
  raw <- synthetic_raw()
  d1 <- apply_design_rule(raw, list(type = "D1", crit_final = 1.96))
  expect_equal(d1$decision, c(rep("Successful at final", 3), "Unsuccessful at final",
                              "Successful at final", NA))
  expect_equal(d1$sample_size, c(30, 30, 30, 30, 30, NA))

  b <- list(crit_eff = 2.24, crit_fin = 2)
  d2 <- apply_design_rule(raw, c(type = "D2", b))
  expect_equal(d2$decision, c("Stop for efficacy", "Stop for efficacy", "Successful at final",
                              "Unsuccessful at final", "Successful at final", NA))
  expect_equal(d2$duration, c(2, 2, 3, 3, 3, NA))
  expect_equal(d2$early_eff, c(TRUE, TRUE, FALSE, FALSE, FALSE, NA))

  # row 5 has NA PP_val without an error, so D3 refuses it
  expect_error(apply_design_rule(raw, c(type = "D3", kappa = 0.1, b)),
               "D3 needs PP_val, but it is NA for 1 replicate")
  d3 <- apply_design_rule(raw[-5, ], c(type = "D3", kappa = 0.1, b))
  expect_equal(d3$decision[1:4], c("Stop for futility", "Stop for efficacy",
                                   "Successful at final", "Unsuccessful at final"))
  expect_true(all(is.na(d3[5, ])))   # the error row
  expect_equal(d3$sample_size[1], 10)
  expect_false(d3$success[1])

  d4 <- apply_design_rule(raw, c(type = "D4", z_fut = 0, b))
  expect_equal(d4$decision[1:5], c("Stop for futility", "Stop for efficacy",
                                   "Successful at final", "Unsuccessful at final",
                                   "Successful at final"))   # NA Z_int never stops

  d5 <- apply_design_rule(raw, c(type = "D5", z_fut = 0, mw_t_star = 6, b))
  expect_equal(d5$decision[1:5], c("Stop for efficacy", "Stop for futility",
                                   "Stop for efficacy", "Unsuccessful at final",
                                   "Unsuccessful at final"))

  expect_setequal(stats::na.omit(unique(c(d1$decision, d2$decision, d3$decision, d4$decision))),
                  c("Stop for efficacy", "Stop for futility", "Successful at final",
                    "Unsuccessful at final"))
  expect_error(apply_design_rule(raw, list(type = "D3", crit_eff = 2)), "needs kappa, crit_fin")
  expect_error(apply_design_rule(raw, c(type = "D5", z_fut = 0, mw_t_star = 2, b)),
               "Z_int_MW_t2 not found")
  expect_error(apply_design_rule(raw, list(type = "D9")), "D1-D5")
})

test_that("apply_design_rule: D1, D2, D4, D5 ignore PP_val; D3 needs it", {
  raw <- synthetic_raw()
  raw_noPP <- raw
  raw_noPP$PP_val <- NA_real_
  b <- list(crit_eff = 2.24, crit_fin = 2)
  rules <- list(list(type = "D1", crit_final = 1.96),
                c(type = "D2", b),
                c(type = "D4", z_fut = 0, b),
                c(type = "D5", z_fut = 0, mw_t_star = 6, b))
  for (rule in rules) {
    expect_identical(apply_design_rule(raw_noPP, rule), apply_design_rule(raw, rule))
  }
  expect_error(apply_design_rule(raw_noPP, c(type = "D3", kappa = 0.1, b)),
               "NA for 5 replicate.*compute_PP = FALSE")
  # NA PP_val on a failed replicate alone is fine
  expect_silent(apply_design_rule(raw[-5, ], c(type = "D3", kappa = 0.1, b)))
})

# Item 6: the post hoc rules must reproduce apply_GSD_to_trial() on the same
# simulated trials, replicate by replicate.
cross_check <- function(s, truth, n_sims, seed) {
  res <- suppressMessages(run_paired_scenario(
    n_sims = n_sims, seed = seed,
    n_c = s$n_c, n_t = s$n_t, total_events = s$total_events,
    futility_IF = s$futility_IF, efficacy_IF = s$efficacy_IF, design = s$design,
    control_model = s$control_model, effect_model = s$effect_model,
    recruitment_model = s$recruitment_model, truth = truth,
    analysis_model_LRT = s$analysis_model_LRT, mw_t_stars = 2,
    update_priors_sims = 100, PP_sims = 50))
  raw <- res$raw
  expect_true(all(is.na(raw$error)))

  # rebuild each replicate's trial from its seed, exactly as single_paired_rep()
  trials <- lapply(raw$seed_used, function(rep_seed) {
    set.seed(rep_seed)
    sample.int(.Machine$integer.max, 1)        # the replicate's jags_seed draw
    if (!is.null(truth)) {
      simulate_trial_from_truth(truth, s$n_c, s$n_t, s$recruitment_model)
    } else {
      simulate_trial_with_recruitment(s$n_c, s$n_t, s$control_model, s$effect_model,
                                      s$recruitment_model)
    }
  })

  base <- list(events = s$total_events, alpha_spending = c(0.0125, 0.025),
               alpha_IF = c(0.75, 1))
  am_mw <- utils::modifyList(s$analysis_model_LRT, list(method = "MW", t_star = 2))
  kappa <- stats::median(raw$PP_val)
  z_fut_lrt <- stats::median(raw$Z_int_LRT)
  z_fut_mw <- stats::median(raw$Z_int_MW_t2)

  specs <- list(
    D2 = list(GSD = c(base, futility_type = "none"), am = s$analysis_model_LRT,
              rule = list(type = "D2")),
    D3 = list(GSD = c(base, futility_type = "PP", futility_IF = s$futility_IF, kappa = kappa),
              am = s$analysis_model_LRT, rule = list(type = "D3", kappa = kappa)),
    D4 = list(GSD = c(base, futility_type = "MatchedZ", futility_IF = s$futility_IF,
                      futility_boundary_Z = z_fut_lrt),
              am = s$analysis_model_LRT, rule = list(type = "D4", z_fut = z_fut_lrt)),
    D5 = list(GSD = c(base, futility_type = "MatchedZ", futility_IF = s$futility_IF,
                      futility_boundary_Z = z_fut_mw),
              am = am_mw, rule = list(type = "D5", z_fut = z_fut_mw, mw_t_star = 2))
  )

  decisions <- list()
  for (nm in names(specs)) {
    sp <- specs[[nm]]
    design <- make_gsd_design(sp$GSD)$design
    rule <- c(sp$rule, crit_eff = crit_at(design, 0.75), crit_fin = crit_at(design, 1))
    post_hoc <- apply_design_rule(raw, rule)

    for (i in seq_len(nrow(raw))) {
      orig <- apply_GSD_to_trial(s$n_c, s$n_t, trials[[i]], design, s$total_events,
                                 sp$GSD, recruitment_model = s$recruitment_model,
                                 analysis_model = sp$am,
                                 .PP_val = if (nm == "D3") raw$PP_val[i])
      info <- paste(nm, "replicate", i)
      expect_identical(post_hoc$decision[i], orig$decision, info = info)
      expect_identical(post_hoc$duration[i], orig$stop_time, info = info)
      expect_equal(post_hoc$sample_size[i], orig$sample_size, info = info)
    }
    decisions[[nm]] <- table(post_hoc$decision)
  }
  decisions
}

test_that("apply_design_rule() matches apply_GSD_to_trial() on every replicate (item 6)", {
  skip_if_not_installed("rjags")
  s <- paired_test_setup()
  s$total_events <- 90

  # fixed truth, and truth drawn from the prior
  dec_truth <- cross_check(s, s$truth, n_sims = 20, seed = 11)
  dec_prior <- cross_check(s, NULL, n_sims = 10, seed = 12)

  # every decision type actually occurs, so the comparison is not vacuous
  all_dec <- unique(unlist(lapply(c(dec_truth, dec_prior), names)))
  expect_setequal(all_dec, c("Stop for efficacy", "Stop for futility",
                             "Successful at final", "Unsuccessful at final"))
})
