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
