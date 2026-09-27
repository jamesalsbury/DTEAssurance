# Calibrate a fixed Z-statistic futility boundary

Finds the Z-statistic futility boundary at `futility_IF` such that the
probability of stopping for futility under the `"null"` scenario equals
`target_null_futility_rate` (the corresponding empirical quantile of the
simulated null Z distribution), and reports the resulting
futility-stopping rate under every supplied scenario. The result is used
as `GSD_model$futility_boundary_Z` with
`GSD_model$futility_type = "MatchedZ"` in
[`calc_dte_assurance_adaptive`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md).

## Usage

``` r
calibrate_matched_futility_boundary(
  n_c,
  n_t,
  recruitment_model,
  futility_IF,
  total_events,
  analysis_model,
  target_null_futility_rate,
  scenarios,
  n_sims = 2000,
  n_cores = 1,
  seed = NULL
)
```

## Arguments

- n_c, n_t:

  Number of patients in the control / treatment group.

- recruitment_model:

  Recruitment specification (see
  [`add_recruitment_time`](https://jamesalsbury.github.io/DTEAssurance/reference/add_recruitment_time.md)).

- futility_IF:

  Information fraction of the futility look.

- total_events:

  Maximum planned number of events.

- analysis_model:

  Analysis specification (see
  [`survival_test`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)).

- target_null_futility_rate:

  Target probability of stopping for futility under the null scenario.

- scenarios:

  Named list of data-generating scenarios (see
  [`single_matched_futility_rep`](https://jamesalsbury.github.io/DTEAssurance/reference/single_matched_futility_rep.md));
  must include one named `"null"`.

- n_sims:

  Number of simulated trials per scenario (default 2000).

- n_cores:

  Number of cores (uses
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) when
  \> 1).

- seed:

  Optional integer seed.

## Value

A list with `boundary`, `scenario_futility_rates`, `raw_Z_by_scenario`
and `settings`.

## Examples

``` r
scenarios <- list(
  null = list(lambda_c = log(2) / 12, delay_time = 0, post_delay_HR = 1),
  alt  = list(lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.6)
)
cal <- calibrate_matched_futility_boundary(
  n_c = 100, n_t = 100,
  recruitment_model = list(method = "power", period = 12, power = 1),
  futility_IF = 0.5, total_events = 120,
  analysis_model = list(method = "LRT", alpha = 0.025,
                        alternative_hypothesis = "one.sided"),
  target_null_futility_rate = 0.5,
  scenarios = scenarios, n_sims = 20, seed = 1)
cal$boundary
#> [1] 0.1815397
cal$scenario_futility_rates
#> null  alt 
#> 0.50 0.15 
```
