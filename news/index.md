# Changelog

## DTEAssurance 1.0.1

CRAN release: 2025-10-24

- Fixed bug in
  [`calc_dte_assurance()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance.md)

## DTEAssurance 1.0.0

CRAN release: 2025-10-14

- Initial CRAN release
- Added support for delayed treatment effects using elicited priors
- Introduced
  [`calc_dte_assurance()`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance.md)
  for fixed designs
- Introduced `calc_dte_assurance_interim()` for group sequential designs
- Included interactive Shiny apps for both design types
- Added vignettes and pkgdown site for documentation

#### Notes

- All functions pass `R CMD check` with 0 errors, 0 warnings, and 0
  notes.
