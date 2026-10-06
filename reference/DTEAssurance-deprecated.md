# Deprecated functions in DTEAssurance

These functions were renamed to match the manuscript notation (PP for
the predictive probability, kappa for the futility threshold) and will
be removed in the next release. Each one forwards its arguments to its
replacement.

## Usage

``` r
BPP_func(...)

calibrate_BPP_threshold(...)

calibrate_BPP_timing(...)

summarize_grid_by_lambda(raw, lambda_grid, conf_level = 0.9)

select_lambda_star(...)
```

## Arguments

- ...:

  Arguments passed to the replacement function.

- raw, lambda_grid, conf_level:

  See
  [`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
  (`lambda_grid` is passed as `kappa_grid`; the two-sided `conf_level`
  is converted to the equivalent one-sided `lcb_level`).

## Value

The return value of the replacement function.

## Details

Only the function names are translated. The elements and columns they
return use the new names (`PP_df`, `PP_vec`, `PP_values`, `PP_val`,
`kappa`, `kappa_star`), so code that reads `BPP_df`, `BPP_vec`,
`lambda_star`, etc. must be updated.

- `BPP_func()`: use
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md).

- `calibrate_BPP_threshold()`: use
  [`calibrate_PP_threshold`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_threshold.md).

- `calibrate_BPP_timing()`: use
  [`calibrate_PP_timing`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_PP_timing.md).

- `summarize_grid_by_lambda()`: use
  [`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md).

- `select_lambda_star()`: use
  [`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md).
