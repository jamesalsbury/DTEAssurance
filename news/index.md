# Changelog

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
  [`summarize_grid_by_lambda()`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_lambda.md)
  and
  [`select_lambda_star()`](https://jamesalsbury.github.io/DTEAssurance/reference/select_lambda_star.md)
  (calibrate a BPP futility threshold over a grid).
- [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  returns Gelman-Rubin diagnostics and posterior latent-state
  probabilities as attributes (`rhat`, `converged`, `Z_probs`).
- [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/BPP_func.md)
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
- `apply_GSD_to_trial()` computes interim and final statistics with
  `analysis_model$method` via
  [`survival_test()`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)
  instead of a hard-coded Cox Wald statistic, and passes
  `update_priors_sims`/`n_BPP_sims` through.
- [`calc_dte_assurance_adaptive()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)
  and `make_rpact_design_from_GSD_model()` recognise the `"MatchedZ"`
  futility type.

## DTEAssurance 1.1.1

- Added
  [`update_priors()`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  to update elicited prior distributions using interim data.
- Added
  [`BPP_func()`](https://jamesalsbury.github.io/DTEAssurance/reference/BPP_func.md)
  for computing the Bayesian predictive probability (BPP) from posterior
  samples.
- Added
  [`calibrate_BPP_timing()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_BPP_timing.md)
  to determine the optimal timing for BPP-based futility looks.
- Added
  [`calibrate_BPP_threshold()`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_BPP_threshold.md)
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
