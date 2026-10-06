control_model = list(dist = "Exponential",
                     parameter_mode = "Distribution",
                     t1 = 12,
                     t1_Beta_a = 20,
                     t1_Beta_b = 32)
effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                    delay_dist = "gamma",
                    HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                    HR_dist = "gamma",
                    P_S = 1,
                    P_DTE = 0)

recruitment_model <- list(method = "power", period = 12, power = 1)

IA_model = list(events = 5, IF = 0.5)

analysis_model = list(method = "LRT",
                      alpha = 0.025,
                      alternative_hypothesis = "one.sided")


data_generating_model = list(lambda_c = log(2)/12,
                             delay_time = 3,
                             post_delay_HR = 0.75)


test_that("calibrate_PP_threshold runs end-to-end with real simulations", {
  set.seed(123)


  out <- calibrate_PP_threshold(
    n_c = 5,
    n_t = 5,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model,
    IA_model = IA_model,
    analysis_model = analysis_model,
    data_generating_model = data_generating_model,
    n_df_sims = 3
  )

  expect_type(out, "list")
  expect_true("PP_vec" %in% names(out))
  expect_length(out$PP_vec, 3)
  expect_false(any(is.na(out$PP_vec)))
})


test_that("calibrate_PP_threshold loop logic works via light mocking", {

  local_mocked_bindings(
    sim_dte = function(...) data.frame(time = 1:2, status = c(1,1), arm = c(0,1)),
    add_recruitment_time = function(data, ...) cbind(data, rec_time = 0),
    cens_data = function(data, ...) list(data = data, cens_time = 1),
    update_priors = function(...) list(dummy = "posterior"),
    PP_func = function(...) {
      list(PP_df = data.frame(success = c(TRUE, FALSE)))
    }
  )

  out <- calibrate_PP_threshold(
    n_c = 2, n_t = 2,
    control_model = control_model,
    effect_model = effect_model,
    recruitment_model = recruitment_model,
    IA_model = IA_model,
    analysis_model = analysis_model,
    data_generating_model = data_generating_model,
    n_df_sims = 4
  )

  expect_equal(out$PP_vec, rep(0.5, 4))
})
