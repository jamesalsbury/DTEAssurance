# Changelog

## DTEAssurance 1.3.0

Results from 1.2.0 are not reproduced exactly: the event-cut convention
(below) and the exponential predictive simulation changed.

### Changes in behaviour

- One event-cut convention everywhere.
  `cens_data(cens_method = "Events", cens_events = k)` is now the only
  routine that cuts a trial at an analysis (used by
  [`apply_GSD_to_trial()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md),
  `single_grid_rep()` and the new paired runner). Cutting at `k` events
  leaves exactly `k` events: previously the group-sequential code
  censored the `k`-th event. Event counts at an information fraction are
  `floor(IF * total_events)` (with a small tolerance), replacing
  [`ceiling()`](https://rdrr.io/r/base/Round.html); information
  fractions are rounded to 6 dp wherever they are compared.
- [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md):
  an exponential control arm now runs the Weibull code path with
  `gamma_c = 1`. This fixes the exponential branch for treated patients
  censored before the delay, where one uniform draw both chose the
  branch and placed the event time, so the late-branch time could fall
  below the delay. Weibull results are unchanged.
- [`make_rpact_design_from_GSD_model()`](https://jamesalsbury.github.io/DTEAssurance/reference/make_gsd_design.md):
  for `"PP"` and `"MatchedZ"` futility, the efficacy critical values are
  taken from the efficacy-only design. A zero-alpha futility look cannot
  change them, but rpact’s numerical integration moved them by about
  8e-5 when the futility look was close to an efficacy look (e.g. 0.7
  and 0.75).
- [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md):
  `converged` is now computed over finite R-hat values only and is `NA`
  (not `TRUE`) when none are finite.
- [`summarize_grid_by_kappa()`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
  and
  [`select_kappa_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md)
  are replaced. `power_LCB` is now a one-sided 95% exact
  (Clopper-Pearson) lower bound (argument `lcb_level`, replacing
  `conf_level`); new columns `n`, `n_NA_dropped`, `power_SE`, `ESS_SE`;
  replicates with `NA` `PP_val` are dropped with a warning;
  `exclude_nonconverged` option.
  [`select_kappa_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md)
  accepts per-scenario floors, treats an `NA` lower bound as infeasible,
  breaks ties in null ESS towards the larger kappa, and reports power
  and floors in the feasibility table.

### New features

- [`single_paired_rep()`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md)
  and
  [`run_paired_scenario()`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md):
  one simulated trial per replicate with one PP evaluation, recording
  interim, efficacy-look and final statistics (log-rank and modestly
  weighted log-rank) so that every design (D1-D5) and kappa is a post
  hoc summary.
  [`run_paired_scenario()`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
  runs in parallel chunks with an RDS checkpoint and resumes from it.

- [`compute_relative_floors()`](https://jamesalsbury.github.io/DTEAssurance/reference/compute_relative_floors.md):
  D2 power minus a margin, per alternative scenario, for use as
  `power_floor`.

- `update_priors(jags_seed = )` makes the MCMC reproducible, and new
  attributes `rhat_max`, `n_nonfinite_rhat` and `n_retained_total`.

- `survival_test(return_HR = )`: `FALSE` skips the Cox fit (used by the
  PP and group-sequential code, where the hazard ratio is not needed).

- [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  returns `PP_val`, `P_Z1`, `P_Z2`, `P_Z3` per trial and a `settings`
  attribute (arguments, package and R versions, git commit if available,
  timestamp).

- Internal `simulate_trial_with_recruitment(force_state = )` fixes the
  latent state, and the simulated data carry a `truth` attribute.

- `inst/benchmark/time_one_rep.R` (in the source repository only) times
  one paired replicate at the manuscript settings.

- [`make_gsd_design()`](https://jamesalsbury.github.io/DTEAssurance/reference/make_gsd_design.md)
  (exported; previously the internal
  [`make_rpact_design_from_GSD_model()`](https://jamesalsbury.github.io/DTEAssurance/reference/make_gsd_design.md),
  kept as an alias for one release) and
  [`apply_GSD_to_trial()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md)
  are now exported and documented.

- [`apply_design_rule()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_design_rule.md):
  D1-D5 decisions post hoc from
  [`run_paired_scenario()`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
  output, using the same decision labels as
  [`apply_GSD_to_trial()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md).
  A test checks that both give identical decisions, stop times and
  sample sizes on the same simulated trials.

- [`run_paired_scenario()`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
  is the single entry point for the paper’s simulation tables.

- `settings` returned by the simulation functions now record all
  arguments plus the package, R, rjags and JAGS versions, the git commit
  of the working directory, the hostname and a timestamp.

### Internal tidy-up (no change in results unless stated)

- One test wrapper (`run_test()`) and one truth simulator
  (`simulate_trial_from_truth()`) replace repeated code; outputs are
  identical under a fixed seed.
- Weibull control parameters from two landmark survival probabilities
  are computed in closed form (as in the
  [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  prior) instead of with `nleqslv`, which could fail silently. Results
  agree to about 1e-8 relative; invalid landmarks (S(t2) \<= 0 or S(t2)
  \>= S(t1)) now give an informative error. `nleqslv` moves from Imports
  to Suggests. The assurance Shiny app uses the same closed form.
- [`survival_test()`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md):
  `alpha` has no default (it previously defaulted to 0.05;
  `alpha = NULL` returns `Signif = NA` when only `Z` is needed), the
  documented default `alternative` is now the actual default
  (`"one.sided"`), and `Signif` is always logical (`FALSE` if `Z` is
  `NA`).

### History of earlier fixes (now removed from the code comments)

- The PP calculation evaluates the full group-sequential success event
  (crossing any remaining efficacy boundary, or success at the final
  analysis), not a single final test at a flat alpha.
- The trial evaluation in
  [`apply_GSD_to_trial()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md)
  and
  [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  uses the specified test statistic (`analysis_model$method`) and the
  design’s boundaries, replacing a hard-coded Cox Wald statistic at a
  flat `qnorm(0.975)`; `Success` is derived only from `Decision`.
- `"MatchedZ"` futility looks are included in the design’s
  information-rate grid with zero alpha spent.
- [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  monitors the latent state `Z` and returns R-hat and state-probability
  diagnostics; calibration output records them.
- The MW statistic from
  [`nphRCT::wlrt()`](https://rdrr.io/pkg/nphRCT/man/wlrt.html) is
  negated so that positive `Z` means benefit for all three methods.
- [`calibrate_PP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_timing.md)
  defaults for `update_priors_sims` and `PP_sims` are 1000 and 2000
  (previously hard-coded to 100 and 50).

### Deprecated (kept, to be removed in a future release)

- The exponential control arm in
  [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  and
  [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md),
  the legacy `censoring_model` fallback of
  [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md),
  [`calibrate_PP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_threshold.md)
  (and its internal `single_calibration_rep()`), and the internal
  `summarize_gsd_results()` and `summarize_convergence()`.

### Stricter inputs

- [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md):
  `n_sims` is required.
- [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md):
  `update_priors_sims` and `PP_sims` are required when
  `futility_type = "PP"`.
- The PP code paths stop unless recruitment is uniform
  (`recruitment_model$method = "power"`, `power = 1`), the assumption
  under which
  [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  draws future recruitment times.

### Documentation

- [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md):
  `n_samples` is per chain (total `n.chains * n_samples`).
- `"MatchedZ"` futility is applied as binding (the trial stops).
- `GSD_model$alpha_spending` is user-specified cumulative alpha
  (`typeOfDesign = "asUser"`).

### Renamed to match the manuscript notation (no behaviour change)

- The predictive probability is now called PP and its futility threshold
  kappa, as in the manuscript:
  - [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
    -\>
    [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md);
    its return element `BPP_df` -\> `PP_df`.
  - [`calibrate_BPP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
    -\>
    [`calibrate_PP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_threshold.md)
    (returns `PP_vec`) and
    [`calibrate_BPP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
    -\>
    [`calibrate_PP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_timing.md)
    (returns `PP_values`).
  - [`summarize_grid_by_lambda()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
    -\>
    [`summarize_grid_by_kappa()`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
    (argument `kappa_grid`, column `kappa`) and
    [`select_lambda_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
    -\>
    [`select_kappa_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md)
    (returns `kappa_star`).
  - `GSD_model$futility_type = "BPP"` -\> `"PP"` and
    `GSD_model$BPP_threshold` -\> `GSD_model$kappa`.
  - `n_BPP_sims` -\> `PP_sims` in
    [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md),
    so the argument has the same name everywhere.
  - The `BPP_val` column of the calibration output is now `PP_val`.
- Internally, the treatment-arm post-delay rate is now named `lambda_e`
  (in
  [`PP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  and in the
  [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  JAGS models); the models are otherwise unchanged.
- A “Notation” section in the README maps manuscript symbols to package
  identifiers.

### Deprecated (to be removed in the next release)

- [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md),
  [`calibrate_BPP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md),
  [`calibrate_BPP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md),
  [`summarize_grid_by_lambda()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  and
  [`select_lambda_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  still work but warn and forward to their replacements. Returned
  element and column names are not translated back to the old names.
- `futility_type = "BPP"` and `GSD_model$BPP_threshold` are accepted by
  [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  with a warning and mapped to `"PP"` and `GSD_model$kappa`.

## DTEAssurance 1.2.0

### Breaking changes

- [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  no longer returns a `Final_Decision` column. That column was
  recomputed independently from a Cox Wald statistic at a flat
  threshold, ignoring `analysis_model$method` and the design’s
  boundaries, and could disagree with `Decision`. Use the new logical
  `Success` column, which is derived only from `Decision`. A `Converged`
  column (MCMC convergence at the BPP interim; `NA` for other futility
  types) is also returned.

### New features

- New `"MatchedZ"` futility type: stop for futility at `futility_IF` if
  the interim Z-statistic falls below `GSD_model$futility_boundary_Z`.
- New exported functions:
  [`calibrate_matched_futility_boundary()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_matched_futility_boundary.md)
  and
  [`single_matched_futility_rep()`](https://jamesalsbury.github.io/DTEAssurance/reference/single_matched_futility_rep.md)
  (calibrate a MatchedZ boundary), and
  [`run_calibration_grid()`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md),
  [`summarize_grid_by_lambda()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  and
  [`select_lambda_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  (calibrate a BPP futility threshold over a grid).
- [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  returns Gelman-Rubin diagnostics and posterior latent-state
  probabilities as attributes (`rhat`, `converged`, `Z_probs`).
- [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  accepts `future_boundaries`, evaluating each predictive draw against
  the design’s remaining efficacy boundaries rather than a single final
  test.

### Bug fixes

- [`survival_test()`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md):
  `"MW"` now returns Z with the same sign convention as `"LRT"`/`"WLRT"`
  (positive Z favours treatment); `"WLRT"` uses the signed statistic
  from
  [`nph::logrank.test()`](https://rdrr.io/pkg/nph/man/logrank.test.html)
  directly.
- [`survival_test()`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)
  no longer fails with “could not find function Surv” when the survival
  package is not attached.
- [`apply_GSD_to_trial()`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md)
  computes interim and final statistics with `analysis_model$method` via
  [`survival_test()`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)
  instead of a hard-coded Cox Wald statistic, and passes
  `update_priors_sims`/`n_BPP_sims` through.
- [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  and
  [`make_rpact_design_from_GSD_model()`](https://jamesalsbury.github.io/DTEAssurance/reference/make_gsd_design.md)
  recognise the `"MatchedZ"` futility type.

## DTEAssurance 1.1.1

- Added
  [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  to update elicited prior distributions using interim data.
- Added
  [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  for computing the Bayesian predictive probability (BPP) from posterior
  samples.
- Added
  [`calibrate_BPP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  to determine the optimal timing for BPP-based futility looks.
- Added
  [`calibrate_BPP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/DTEAssurance-deprecated.md)
  to calibrate the BPP threshold for futility decisions.
- Renamed `calc_dte_assurance_interim()` to
  [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  for consistency in adaptive design terminology.
- Renamed `assurance_interim_shiny_app()` to
  [`assurance_adaptive_shiny_app()`](https://jamesalsbury.github.io/DTEAssurance/reference/assurance_adaptive_shiny_app.md).
- Expanded functionality and UI elements in
  [`assurance_adaptive_shiny_app()`](https://jamesalsbury.github.io/DTEAssurance/reference/assurance_adaptive_shiny_app.md).

## DTEAssurance 1.0.1

CRAN release: 2025-10-24

- Fixed a bug in
  [`calc_dte_assurance()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance.md)
  affecting certain parameter configurations.

## DTEAssurance 1.0.0

CRAN release: 2025-10-14

- Initial CRAN release.
- Added support for delayed treatment effects using elicited prior
  distributions.
- Introduced
  [`calc_dte_assurance()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance.md)
  for fixed designs.
- Introduced `calc_dte_assurance_interim()` for group sequential
  designs.
- Added interactive Shiny applications for both design types.
- Included vignettes and a pkgdown site for documentation.
