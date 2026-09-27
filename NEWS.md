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
