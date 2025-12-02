test_that("BPP_func runs with Exponential control and returns correct structure", {

  # ---- Minimal interim dataset ----
  df_interim <- data.frame(
    time          = c(1, 2, 3, 1.5),
    group         = c("Control", "Control", "Treatment", "Treatment"),
    rec_time      = c(0.5, 1, 0.7, 0.2),
    pseudo_time   = c(1.5, 3, 3.7, 1.7),
    status        = c(1, 0, 1, 0),
    survival_time = c(1, 2, 3, 1.5)
  )

  # ---- Minimal posterior draws ----
  posterior_expon <- data.frame(
    lambda_c   = c(0.1, 0.12),
    delay_time = c(2, 2.5),
    HR         = c(0.8, 0.7)
  )

  # ---- Mock censoring & survival functions INSIDE DTEAssurance namespace ----
  testthat::with_mocked_bindings(
    cens_data = function(df, cens_method, cens_time = NULL, cens_events = NULL, cens_IF = NULL) {
      # Always return the data untouched
      list(data = df)
    },

    survival_test = function(df, analysis_method, alpha, alternative, rho, gamma, t_star, s_star) {
      list(Signif = 1, Z = 2.0)
    },

    {

      out <- BPP_func(
        data = df_interim,
        posterior_df = posterior_expon,
        control_distribution = "Exponential",
        n_c_planned = 10,
        n_t_planned = 10,
        rec_time_planned = 20,
        df_cens_time = 5,
        censoring_model = list(method = "Events", events = 50),
        analysis_model = list(
          method = "LRT",
          alpha  = 0.025,
          alternative_hypothesis = "one.sided"
        ),
        n_sims = 10
      )

      # ---- Structural expectations ----
      expect_true(is.list(out))
      expect_true(is.data.frame(out$BPP_df))
      expect_equal(nrow(out$BPP_df), 10)
      expect_true(all(c("success", "Z_val") %in% names(out$BPP_df)))

      # Mock outputs
      expect_true(all(out$BPP_df$success == 1))
      expect_true(all(out$BPP_df$Z_val == 2.0))
    }
  )
})

test_that("BPP_func runs end-to-end under Weibull control model", {

  set.seed(123)

  delay <- 5

  # Construct dataset that triggers ALL branches
  df <- data.frame(
    time        = c(1, 4, 3, 9),          # 3 < delay -> before-delay segment
    group       = c("Control", "Treatment", "Treatment", "Control"),
    rec_time    = c(0, 0, 0, 0)
  )
  df$pseudo_time <- df$time
  df$status <- df$time < 7               # event if < 7
  df$survival_time <- ifelse(df$status, df$time, 7)

  posterior_df <- data.frame(
    lambda_c   = rep(0.1, 6),
    gamma_c    = rep(1.5, 6),             # ensures nonlinear hazard
    delay_time = rep(delay, 6),
    HR         = rep(0.6, 6)
  )

  censoring_model <- list(method = "Time", time = 12)

  analysis_model <- list(
    method = "LRT",
    alpha = 0.025,
    alternative_hypothesis = "one.sided",
    alternative = "one.sided"
  )

  out <- BPP_func(
    data = df,
    posterior_df = posterior_df,
    control_distribution = "Weibull",
    n_c_planned = 4, n_t_planned = 4,
    rec_time_planned = 10,
    df_cens_time = 7,
    censoring_model = censoring_model,
    analysis_model = analysis_model,
    n_sims = 2
  )

  expect_type(out, "list")
  expect_true("BPP_df" %in% names(out))
  expect_equal(nrow(out$BPP_df), 2)
  expect_false(any(is.na(out$BPP_df$success)))
})

test_that("BPP_func hits Weibull branch with n_before > 0", {
  skip_if_not_installed("survival")

  set.seed(123)

  # --- Construct a dataset that forces n_before > 0 ---
  # One treatment subject is censored and has survival_time < delay_time
  data <- data.frame(
    time = c(2, 3, 1.0, 0.5),                # raw event times
    group = c("Control", "Control", "Treatment", "Treatment"),
    rec_time = c(0, 1, 0.3, 0.4)
  )

  data$pseudo_time <- data$time + data$rec_time

  # Status: second treatment subject censored
  data$status <- c(1, 1, 1, 0)

  # survival_time for censored subject must be < delay_time
  data$survival_time <- ifelse(data$status == 1, data$time, 0.5)

  # posterior sampled delay_time must exceed that censored survival_time
  posterior_df <- data.frame(
    lambda_c = rep(0.1, 20),
    gamma_c = rep(1.2, 20),         # Weibull shape
    delay_time = rep(2.0, 20),      # > 0.5 → triggers n_before > 0
    HR = rep(0.7, 20)
  )

  censoring_model <- list(method = "Time", time = 20)

  analysis_model <- list(
    method = "LRT",
    alpha = 0.05,
    alternative_hypothesis = "one.sided",
    rho = NULL, gamma = NULL,
    t_star = NULL, s_star = NULL
  )

  result <- BPP_func(
    data = data,
    posterior_df = posterior_df,
    control_distribution = "Weibull",
    n_c_planned = 3,
    n_t_planned = 3,
    rec_time_planned = 10,
    df_cens_time = 5,
    censoring_model = censoring_model,
    analysis_model = analysis_model,
    n_sims = 3
  )

  # structure checks
  expect_type(result, "list")
  expect_true("BPP_df" %in% names(result))
  expect_equal(nrow(result$BPP_df), 3)

  expect_true(all(c("success", "Z_val") %in% names(result$BPP_df)))
  expect_true(all(is.finite(result$BPP_df$Z_val)))
})

