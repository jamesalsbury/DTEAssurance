# Regression checks: every supported futility type runs end-to-end through
# apply_GSD_to_trial() and calc_dte_assurance_adaptive(), and the
# Converged column is populated only for PP designs.

futility_setup <- function() {
  control_model <- list(dist = "Exponential", parameter_mode = "Distribution",
                        t1 = 12, t1_Beta_a = 20, t1_Beta_b = 20)
  effect_model <- list(
    P_S = 1, P_DTE = 0.5,
    HR_SHELF = SHELF::fitdist(c(0.6, 0.65, 0.7), probs = c(0.25, 0.5, 0.75),
                              lower = 0, upper = 2),
    HR_dist = "gamma",
    delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75),
                                 lower = 0, upper = 10),
    delay_dist = "gamma"
  )
  recruitment_model <- list(method = "power", period = 12, power = 1)
  analysis_model <- list(method = "LRT", alpha = 0.025,
                         alternative_hypothesis = "one.sided")
  base <- list(events = 100, alpha_spending = c(0.0125, 0.025),
               alpha_IF = c(0.75, 1))
  GSD_models <- list(
    none     = c(base, list(futility_type = "none")),
    PP       = c(base, list(futility_type = "PP", futility_IF = 0.5,
                            kappa = 0.2)),
    MatchedZ = c(base, list(futility_type = "MatchedZ", futility_IF = 0.5,
                            futility_boundary_Z = 0))
  )
  list(control_model = control_model, effect_model = effect_model,
       recruitment_model = recruitment_model,
       analysis_model = analysis_model, GSD_models = GSD_models)
}

test_that("apply_GSD_to_trial runs for none, PP and MatchedZ", {
  skip_if_not_installed("rjags")
  s <- futility_setup()
  set.seed(42)

  for (nm in names(s$GSD_models)) {
    G <- s$GSD_models[[nm]]
    design <- make_rpact_design_from_GSD_model(G)$design
    converged <- vapply(seq_len(3), function(i) {
      trial <- simulate_trial_with_recruitment(75, 75, s$control_model,
                                               s$effect_model,
                                               s$recruitment_model)
      out <- apply_GSD_to_trial(75, 75, trial, design, G$events, G,
                                s$control_model, s$effect_model,
                                s$recruitment_model, s$analysis_model,
                                update_priors_sims = 100, PP_sims = 20)
      expect_true(out$decision %in% c("Stop for efficacy", "Stop for futility",
                                      "Successful at final",
                                      "Unsuccessful at final"))
      as.logical(out$converged)
    }, logical(1))

    if (nm == "PP") {
      expect_false(anyNA(converged), info = nm)
    } else {
      expect_true(all(is.na(converged)), info = nm)
    }
  }
})

test_that("apply_GSD_to_trial stops for futility under MatchedZ when Z is below the boundary", {
  s <- futility_setup()
  G <- s$GSD_models$MatchedZ
  G$futility_boundary_Z <- 100  # impossible to exceed -> always stop at the futility look
  design <- make_rpact_design_from_GSD_model(G)$design
  set.seed(7)
  trial <- simulate_trial_with_recruitment(75, 75, s$control_model,
                                           s$effect_model, s$recruitment_model)
  out <- apply_GSD_to_trial(75, 75, trial, design, G$events, G,
                            analysis_model = s$analysis_model)
  expect_equal(out$decision, "Stop for futility")
})

test_that("calc_dte_assurance_adaptive runs for none, PP and MatchedZ", {
  skip_if_not_installed("rjags")
  s <- futility_setup()
  set.seed(42)

  for (nm in names(s$GSD_models)) {
    res <- calc_dte_assurance_adaptive(
      75, 75, s$control_model, s$effect_model, s$recruitment_model,
      s$GSD_models[[nm]], s$analysis_model,
      update_priors_sims = 100, PP_sims = 20, n_sims = 3
    )
    expect_equal(names(res), c("Trial", "Decision", "StopTime", "SampleSize",
                               "Success", "Converged"))
    expect_equal(res$Success,
                 res$Decision %in% c("Stop for efficacy", "Successful at final"))
    if (nm == "PP") {
      expect_false(anyNA(res$Converged), info = nm)
    } else {
      expect_true(all(is.na(res$Converged)), info = nm)
    }
  }
})

test_that("calc_dte_assurance_adaptive validates MatchedZ inputs", {
  s <- futility_setup()
  G <- s$GSD_models$MatchedZ
  G$futility_boundary_Z <- NULL
  expect_error(
    calc_dte_assurance_adaptive(75, 75, s$control_model, s$effect_model,
                                s$recruitment_model, G, n_sims = 1),
    "futility_boundary_Z"
  )
})
