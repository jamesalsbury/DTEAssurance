# Summarise calibration output over a grid of PP futility thresholds

Applies each candidate PP futility threshold \\\kappa\\ to raw replicate
output (from
[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
or
[`run_calibration_grid`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md)):
a trial stops for futility if its interim PP is below \\\kappa\\,
otherwise it takes its continuation (D2) outcome. At `kappa = 0` no
trial stops, so the result is the D2 operating characteristics.

## Usage

``` r
summarize_grid_by_kappa(
  raw,
  kappa_grid,
  lcb_level = 0.95,
  exclude_nonconverged = FALSE
)
```

## Arguments

- raw:

  A data frame with columns `PP_val`, `continuation_success`,
  `continuation_sample_size`, `continuation_stop_time`,
  `sample_size_interim`, `t_interim` and (if
  `exclude_nonconverged = TRUE`) `converged`.

- kappa_grid:

  Numeric vector of candidate PP thresholds.

- lcb_level:

  Confidence level of the one-sided exact (Clopper-Pearson) lower bound
  on power (default 0.95).

- exclude_nonconverged:

  If `TRUE`, drop replicates whose interim MCMC did not converge
  (`converged` `FALSE` or `NA`). Default `FALSE`.

## Value

A data frame with one row per \\\kappa\\ and columns `kappa`, `n`
(replicates used), `n_NA_dropped` (replicates dropped because `PP_val`
was `NA`, with a warning), `P_early_fut`, `power_or_typeI`, `power_SE`,
`power_LCB` (one-sided exact lower bound at level `lcb_level`, 95\\
size), `ESS_SE` and `duration` (expected trial duration).

## See also

[`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md),
[`compute_relative_floors`](https://jamesalsbury.github.io/DTEAssurance/reference/compute_relative_floors.md)
