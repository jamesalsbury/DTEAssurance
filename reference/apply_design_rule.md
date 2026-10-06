# Apply a design rule to paired replicate output

Turns the output of
[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
into the decisions of one design, post hoc. The efficacy look is only
checked if the trial was not stopped at the futility look. A missing
test statistic never crosses a boundary (as in
[`apply_GSD_to_trial`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_GSD_to_trial.md)).

## Usage

``` r
apply_design_rule(raw, rule)
```

## Arguments

- raw:

  The `raw` element returned by
  [`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md).

- rule:

  A list with `type` (`"D1"` to `"D5"`) and the rule's parameters:
  `crit_final` (D1); `crit_eff`, `crit_fin` (D2-D5); `kappa` (D3);
  `z_fut` (D4, D5); `mw_t_star` (D5).

## Value

A data frame with one row per replicate and columns `decision`
(`"Stop for efficacy"`, `"Stop for futility"`, `"Successful at final"`
or `"Unsuccessful at final"`), `success`, `early_fut`, `early_eff`,
`sample_size` and `duration`. Rows for failed replicates (non-missing
`error`) are `NA`.

## Details

- D1:

  Fixed design: success iff `Z_fin_LRT > crit_final`.

- D2:

  Efficacy look then final: `"Stop for efficacy"` if
  `Z_eff_LRT > crit_eff`, otherwise the final analysis with
  `Z_fin_LRT > crit_fin`.

- D3:

  D2 plus PP futility: `"Stop for futility"` if `PP_val < kappa`.

- D4:

  D2 plus Z futility: `"Stop for futility"` if `Z_int_LRT < z_fut`.

- D5:

  As D4 with the modestly weighted statistics `Z_int_MW_t<t>`,
  `Z_eff_MW_t<t>`, `Z_fin_MW_t<t>` for `t = mw_t_star`.

## See also

[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md),
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
