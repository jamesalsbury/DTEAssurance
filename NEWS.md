# DTEAssurance 1.3.0

Results from 1.2.0 are not reproduced exactly: the event-cut convention
(below) and the exponential predictive simulation changed.

## Changes in behaviour

* One event-cut convention everywhere. `cens_data(cens_method = "Events",
  cens_events = k)` is now the only routine that cuts a trial at an analysis
  (used by `apply_GSD_to_trial()`, `single_grid_rep()` and the new paired
  runner). Cutting at `k` events leaves exactly `k` events: previously the
  group-sequential code censored the `k`-th event. Event counts at an
  information fraction are `floor(IF * total_events)` (with a small
  tolerance), replacing `ceiling()`; information fractions are rounded to
  6 dp wherever they are compared.
* `PP_func()`: an exponential control arm now runs the Weibull code path with
  `gamma_c = 1`. This fixes the exponential branch for treated patients
  censored before the delay, where one uniform draw both chose the branch and
  placed the event time, so the late-branch time could fall below the delay.
  Weibull results are unchanged.
* `make_rpact_design_from_GSD_model()`: for `"PP"` and `"MatchedZ"`
  futility, the efficacy critical values are taken from the efficacy-only
  design. A zero-alpha futility look cannot change them, but rpact's
  numerical integration moved them by about 8e-5 when the futility look was
  close to an efficacy look (e.g. 0.7 and 0.75).
* `update_priors()`: `converged` is now computed over finite R-hat values
  only and is `NA` (not `TRUE`) when none are finite.
* `summarize_grid_by_kappa()` and `select_kappa_star()` are replaced.
  `power_LCB` is now a one-sided 95% exact (Clopper-Pearson) lower bound
  (argument `lcb_level`, replacing `conf_level`); new columns `n`,
  `n_NA_dropped`, `power_SE`, `ESS_SE`; replicates with `NA` `PP_val` are
  dropped with a warning; `exclude_nonconverged` option. `select_kappa_star()`
  accepts per-scenario floors, treats an `NA` lower bound as infeasible,
  breaks ties in null ESS towards the larger kappa, and reports power and
  floors in the feasibility table.

## New features

* `single_paired_rep()` and `run_paired_scenario()`: one simulated trial per
  replicate with one PP evaluation, recording interim, efficacy-look and
  final statistics (log-rank and modestly weighted log-rank) so that every
  design (D1-D5) and kappa is a post hoc summary. `run_paired_scenario()`
  runs in parallel chunks with an RDS checkpoint and resumes from it.
* `compute_relative_floors()`: D2 power minus a margin, per alternative
  scenario, for use as `power_floor`.
* `update_priors(jags_seed = )` makes the MCMC reproducible, and new
  attributes `rhat_max`, `n_nonfinite_rhat` and `n_retained_total`.
* `survival_test(return_HR = )`: `FALSE` skips the Cox fit (used by the PP
  and group-sequential code, where the hazard ratio is not needed).
* `calc_dte_assurance_adaptive()` returns `PP_val`, `P_Z1`, `P_Z2`, `P_Z3`
  per trial and a `settings` attribute (arguments, package and R versions,
  git commit if available, timestamp).
* Internal `simulate_trial_with_recruitment(force_state = )` fixes the
  latent state, and the simulated data carry a `truth` attribute.
* `inst/benchmark/time_one_rep.R` (in the source repository only) times one
  paired replicate at the manuscript settings.

## Stricter inputs

* `PP_func()`: `n_sims` is required.
* `calc_dte_assurance_adaptive()`: `update_priors_sims` and `PP_sims` are
  required when `futility_type = "PP"`.
* The PP code paths stop unless recruitment is uniform
  (`recruitment_model$method = "power"`, `power = 1`), the assumption under
  which `PP_func()` draws future recruitment times.

## Documentation

* `update_priors()`: `n_samples` is per chain (total `n.chains * n_samples`).
* `"MatchedZ"` futility is applied as binding (the trial stops).
* `GSD_model$alpha_spending` is user-specified cumulative alpha
  (`typeOfDesign = "asUser"`).

## Renamed to match the manuscript notation (no behaviour change)

* The predictive probability is now called PP and its futility threshold
  kappa, as in the manuscript:
  * `BPP_func()` -> `PP_func()`; its return element `BPP_df` -> `PP_df`.
  * `calibrate_BPP_threshold()` -> `calibrate_PP_threshold()` (returns
    `PP_vec`) and `calibrate_BPP_timing()` -> `calibrate_PP_timing()`
    (returns `PP_values`).
  * `summarize_grid_by_lambda()` -> `summarize_grid_by_kappa()` (argument
    `kappa_grid`, column `kappa`) and `select_lambda_star()` ->
    `select_kappa_star()` (returns `kappa_star`).
  * `GSD_model$futility_type = "BPP"` -> `"PP"` and
    `GSD_model$BPP_threshold` -> `GSD_model$kappa`.
  * `n_BPP_sims` -> `PP_sims` in `calc_dte_assurance_adaptive()`, so the
    argument has the same name everywhere.
  * The `BPP_val` column of the calibration output is now `PP_val`.
* Internally, the treatment-arm post-delay rate is now named `lambda_e`
  (in `PP_func()` and in the `update_priors()` JAGS models); the models are
  otherwise unchanged.
* A "Notation" section in the README maps manuscript symbols to package
  identifiers.

## Deprecated (to be removed in the next release)

* `BPP_func()`, `calibrate_BPP_threshold()`, `calibrate_BPP_timing()`,
  `summarize_grid_by_lambda()` and `select_lambda_star()` still work but warn
  and forward to their replacements. Returned element and column names are
  not translated back to the old names.
* `futility_type = "BPP"` and `GSD_model$BPP_threshold` are accepted by
  `calc_dte_assurance_adaptive()` with a warning and mapped to `"PP"` and
  `GSD_model$kappa`.

# DTEAssurance 1.2.0

## Breaking changes

* `calc_dte_assurance_adaptive()` no longer returns a `Final_Decision` column.
  That column was recomputed independently from a Cox Wald statistic at a flat
  threshold, ignoring `analysis_model$method` and the design's boundaries, and
  could disagree with `Decision`. Use the new logical `Success` column, which is
  derived only from `Decision`. A `Converged` column (MCMC convergence at the
  BPP interim; `NA` for other futility types) is also returned.

## New features

* New `"MatchedZ"` futility type: stop for futility at `futility_IF` if the
  interim Z-statistic falls below `GSD_model$futility_boundary_Z`.
* New exported functions: `calibrate_matched_futility_boundary()` and
  `single_matched_futility_rep()` (calibrate a MatchedZ boundary), and
  `run_calibration_grid()`, `summarize_grid_by_lambda()` and
  `select_lambda_star()` (calibrate a BPP futility threshold over a grid).
* `update_priors()` returns Gelman-Rubin diagnostics and posterior latent-state
  probabilities as attributes (`rhat`, `converged`, `Z_probs`).
* `BPP_func()` accepts `future_boundaries`, evaluating each predictive draw
  against the design's remaining efficacy boundaries rather than a single
  final test.

## Bug fixes

* `survival_test()`: `"MW"` now returns Z with the same sign convention as
  `"LRT"`/`"WLRT"` (positive Z favours treatment); `"WLRT"` uses the signed
  statistic from `nph::logrank.test()` directly.
* `survival_test()` no longer fails with "could not find function Surv" when
  the survival package is not attached.
* `apply_GSD_to_trial()` computes interim and final statistics with
  `analysis_model$method` via `survival_test()` instead of a hard-coded Cox
  Wald statistic, and passes `update_priors_sims`/`n_BPP_sims` through.
* `calc_dte_assurance_adaptive()` and `make_rpact_design_from_GSD_model()`
  recognise the `"MatchedZ"` futility type.

# DTEAssurance 1.1.1

* Added `update_priors()` to update elicited prior distributions using interim data.
* Added `BPP_func()` for computing the Bayesian predictive probability (BPP) from posterior samples.
* Added `calibrate_BPP_timing()` to determine the optimal timing for BPP-based futility looks.
* Added `calibrate_BPP_threshold()` to calibrate the BPP threshold for futility decisions.
* Renamed `calc_dte_assurance_interim()` to `calc_dte_assurance_adaptive()` for consistency in adaptive design terminology.
* Renamed `assurance_interim_shiny_app()` to `assurance_adaptive_shiny_app()`.
* Expanded functionality and UI elements in `assurance_adaptive_shiny_app()`.

# DTEAssurance 1.0.1

* Fixed a bug in `calc_dte_assurance()` affecting certain parameter configurations.

# DTEAssurance 1.0.0

* Initial CRAN release.
* Added support for delayed treatment effects using elicited prior distributions.
* Introduced `calc_dte_assurance()` for fixed designs.
* Introduced `calc_dte_assurance_interim()` for group sequential designs.
* Added interactive Shiny applications for both design types.
* Included vignettes and a pkgdown site for documentation.
