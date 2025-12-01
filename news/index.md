# Changelog

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
