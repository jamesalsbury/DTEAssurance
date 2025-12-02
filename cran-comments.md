## R CMD check results

0 errors | 0 warnings | 0 note

## Comments
This submission updates the package to version 1.1.0
Changes:
* Added `update_priors()` to update elicited prior distributions using interim data.
* Added `BPP_func()` for computing the Bayesian predictive probability (BPP) from posterior samples.
* Added `calibrate_BPP_timing()` to determine the optimal timing for BPP-based futility looks.
* Added `calibrate_BPP_threshold()` to calibrate the BPP threshold for futility decisions.
* Renamed `calc_dte_assurance_interim()` to `calc_dte_assurance_adaptive()` for consistency in adaptive design terminology.
* Renamed `assurance_interim_shiny_app()` to `assurance_adaptive_shiny_app()`.
* Expanded functionality and UI elements in `assurance_adaptive_shiny_app()`.

