# Select the BPP futility threshold minimising null expected sample size

Among candidate thresholds for which the lower confidence bound on power
is at least `power_floor` under every alternative scenario, selects the
one with the smallest expected sample size under the null scenario.

## Usage

``` r
select_lambda_star(
  summary_by_scenario,
  power_floor,
  null_scenario,
  alt_scenarios
)
```

## Arguments

- summary_by_scenario:

  A named list of data frames, one per scenario, each as returned by
  [`summarize_grid_by_lambda`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_lambda.md)
  over the same `lambda_grid`.

- power_floor:

  Minimum acceptable lower confidence bound on power.

- null_scenario:

  Name of the null scenario in `summary_by_scenario`.

- alt_scenarios:

  Character vector of alternative scenario names.

## Value

A list with `lambda_star` (the selected threshold, or `NA` with a
warning if none is feasible) and `feasibility_table`.

## See also

[`run_calibration_grid`](https://jamesalsbury.github.io/DTEAssurance/reference/run_calibration_grid.md),
[`summarize_grid_by_lambda`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_lambda.md)
