# Select the PP futility threshold minimising null expected sample size

Among candidate thresholds for which the one-sided 95\\ confidence bound
on power (`power_LCB` from
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md))
is at least the floor under every alternative scenario, selects the one
with the smallest expected sample size under the null scenario. A
threshold with an `NA` lower bound in any alternative scenario is
infeasible. Ties in null expected sample size (within 1e-8) are broken
in favour of the *larger* kappa.

## Usage

``` r
select_kappa_star(
  summary_by_scenario,
  power_floor,
  null_scenario,
  alt_scenarios
)
```

## Arguments

- summary_by_scenario:

  A named list of data frames, one per scenario, each as returned by
  [`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
  over the same `kappa_grid`.

- power_floor:

  Minimum acceptable lower confidence bound on power: a scalar applied
  to every alternative scenario, or a named numeric vector with one
  entry per alternative scenario (e.g. from
  [`compute_relative_floors`](https://jamesalsbury.github.io/DTEAssurance/reference/compute_relative_floors.md)).

- null_scenario:

  Name of the null scenario in `summary_by_scenario`.

- alt_scenarios:

  Character vector of alternative scenario names.

## Value

A list with `kappa_star` (the selected threshold, or `NA` with a warning
if none is feasible) and `feasibility_table` (`kappa`, `null_ESS`,
`feasible` and, per alternative scenario, `power_<s>`, `power_LCB_<s>`
and `floor_<s>`).

## See also

[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md),
[`compute_relative_floors`](https://jamesalsbury.github.io/DTEAssurance/reference/compute_relative_floors.md)
