make_raw <- function(n = 400, seed = 1) {
  set.seed(seed)
  data.frame(
    PP_val = runif(n),
    continuation_success = rbinom(n, 1, 0.8),
    t_interim = 10,
    sample_size_interim = 300L,
    continuation_stop_time = sample(c(15, 20), n, replace = TRUE),
    continuation_sample_size = 400L,
    converged = TRUE
  )
}

test_that("summarize_grid_by_kappa: power and ESS non-increasing in kappa (spec test 9)", {
  grid <- seq(0, 0.5, by = 0.05)
  sm <- summarize_grid_by_kappa(make_raw(), grid)
  expect_equal(sm$kappa, grid)
  expect_true(all(diff(sm$power_or_typeI) <= 0))
  expect_true(all(diff(sm$ESS) <= 0))
  expect_true(all(diff(sm$power_LCB) <= 0))
  expect_true(all(c("n", "n_NA_dropped", "power_SE", "ESS_SE") %in% names(sm)))

  # one-sided 95% exact lower bound
  k <- sum(make_raw()$continuation_success)
  expect_equal(sm$power_LCB[1],
               stats::binom.test(k, 400, alternative = "greater", conf.level = 0.95)$conf.int[1])

  # the feasible set is a prefix of the grid
  sel <- select_kappa_star(list(null = sm, alt = sm), power_floor = 0.6,
                           null_scenario = "null", alt_scenarios = "alt")
  f <- sel$feasibility_table$feasible
  expect_true(any(f) && !all(f))
  expect_equal(f, seq_along(f) <= sum(f))
})

test_that("summarize_grid_by_kappa drops NA PP_val with a warning and can exclude non-converged", {
  raw <- make_raw()
  raw$PP_val[1:3] <- NA
  expect_warning(sm <- summarize_grid_by_kappa(raw, c(0, 0.2)), "3 replicates have NA PP_val")
  expect_equal(sm$n, c(397, 397))
  expect_equal(sm$n_NA_dropped, c(3, 3))
  raw <- make_raw(); raw$converged[1:10] <- FALSE; raw$converged[11] <- NA
  expect_equal(summarize_grid_by_kappa(raw, 0, exclude_nonconverged = TRUE)$n, 389)
})

test_that("select_kappa_star: ties go to the larger kappa and NA LCB is infeasible (spec test 10)", {
  mk <- function(lcb, ess) data.frame(kappa = c(0, 0.1, 0.2, 0.3), power_or_typeI = lcb,
                                      power_LCB = lcb, ESS = ess)
  null <- mk(c(0.02, 0.02, 0.02, 0.02), c(500, 450, 450, 400))
  alt  <- mk(c(0.9, 0.85, 0.85, 0.5), c(500, 480, 470, 420))
  sel <- select_kappa_star(list(null = null, alt = alt), 0.8, "null", "alt")
  expect_equal(sel$kappa_star, 0.2)   # 0.1 and 0.2 tie on null ESS

  alt_na <- alt; alt_na$power_LCB[3] <- NA
  sel <- select_kappa_star(list(null = null, alt = alt_na), 0.8, "null", "alt")
  expect_false(sel$feasibility_table$feasible[3])
  expect_equal(sel$kappa_star, 0.1)

  # per-scenario floors, and the infeasible case
  alt2 <- mk(c(0.7, 0.7, 0.6, 0.6), c(1, 1, 1, 1))
  sel <- select_kappa_star(list(null = null, alt = alt, alt2 = alt2),
                           c(alt = 0.8, alt2 = 0.65), "null", c("alt", "alt2"))
  expect_equal(sel$kappa_star, 0.1)
  expect_equal(sel$feasibility_table$floor_alt2, rep(0.65, 4))
  expect_warning(sel <- select_kappa_star(list(null = null, alt = alt), 0.99, "null", "alt"),
                 "no feasible kappa")
  expect_true(is.na(sel$kappa_star))
})

test_that("compute_relative_floors is D2 power minus the margin", {
  raws <- list(a = make_raw(seed = 1), b = make_raw(seed = 2))
  fl <- compute_relative_floors(raws, c("a", "b"), margin = 0.05)
  expect_equal(names(fl), c("a", "b"))
  expect_equal(unname(fl["a"]), mean(raws$a$continuation_success) - 0.05)
})
