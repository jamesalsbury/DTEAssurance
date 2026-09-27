# Summarise calibration-grid output over a grid of BPP futility thresholds

Applies each candidate BPP futility threshold \\\lambda\\ to the raw
output of
[`run_calibration_grid`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md):
a trial stops for futility if its interim BPP is below \\\lambda\\,
otherwise it takes its simulated continuation outcome.

## Usage

``` r
summarize_grid_by_lambda(raw, lambda_grid, conf_level = 0.9)
```

## Arguments

- raw:

  The `raw` element returned by
  [`run_calibration_grid`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md).

- lambda_grid:

  Numeric vector of candidate BPP thresholds.

- conf_level:

  Confidence level for the one-sided lower confidence bound on power
  (Clopper-Pearson; default 0.90).

## Value

A data frame with one row per \\\lambda\\ and columns `lambda`,
`P_early_fut`, `power_or_typeI`, `power_LCB`, `ESS` (expected sample
size) and `duration` (expected trial duration).

## See also

[`run_calibration_grid`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md),
[`select_lambda_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_lambda_star.md)
