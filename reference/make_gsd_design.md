# Build the group-sequential design for a GSD_model

Builds the `rpact` group-sequential design used by
[`calc_dte_assurance_adaptive`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md),
[`apply_GSD_to_trial`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md)
and
[`single_paired_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_paired_rep.md).
`GSD_model$alpha_spending` is the user-specified *cumulative* alpha
spent at each efficacy look `GSD_model$alpha_IF` (`rpact`'s
`typeOfDesign = "asUser"`). For `"PP"` and `"MatchedZ"` futility, the
futility look (`futility_IF`) is added to the information-rate grid with
zero alpha spent, so its critical value is `Inf` and the efficacy
critical values are exactly those of the efficacy-only design. For
`"Beta"` futility, `GSD_model$beta_spending` is the cumulative beta
spent at each `futility_IF` (`typeBetaSpending = "bsUser"`). Information
fractions are rounded to 6 dp before they are compared.

## Usage

``` r
make_gsd_design(GSD_model)

make_rpact_design_from_GSD_model(GSD_model)
```

## Arguments

- GSD_model:

  A named list with `alpha_IF`, `alpha_spending`, `futility_type`
  (`"none"`, `"Beta"`, `"PP"` or `"MatchedZ"`) and, for futility
  designs, `futility_IF` (and `beta_spending` for `"Beta"`). See
  [`calc_dte_assurance_adaptive`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md).

## Value

A list with `design` (the `rpact` design object;
`design$informationRates` and `design$criticalValues` give the looks and
their Z critical values), `IF_all`, `alpha_spending_full` and
`beta_spending_full`.

## Details

`make_rpact_design_from_GSD_model()` is the previous name, kept as an
alias for one release.

## Examples

``` r
# Efficacy look at 75% information and final analysis, with a PP futility
# look at 50% information (zero alpha spent there)
gsd <- make_gsd_design(list(alpha_IF = c(0.75, 1),
                            alpha_spending = c(0.0125, 0.025),
                            futility_type = "PP", futility_IF = 0.5))
gsd$design$informationRates
#> [1] 0.50 0.75 1.00
gsd$design$criticalValues
#> [1]      Inf 2.241403 2.046965
```
