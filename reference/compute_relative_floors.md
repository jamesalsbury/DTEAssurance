# Relative power floors for kappa selection

For each alternative scenario, the D2 power (the power with no futility
stopping, i.e. at `kappa = 0`) minus `margin`. Pass the result as
`power_floor` to
[`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md).

## Usage

``` r
compute_relative_floors(raw_by_scenario, alt_scenarios, margin = 0.05)
```

## Arguments

- raw_by_scenario:

  A named list of raw replicate data frames (each with a
  `continuation_success` column).

- alt_scenarios:

  Character vector of alternative scenario names.

- margin:

  Allowed loss of power relative to D2 (default 0.05).

## Value

A named numeric vector with one floor per alternative scenario.

## See also

[`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md)
