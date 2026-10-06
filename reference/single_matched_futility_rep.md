# Simulate one interim dataset and compute Z at the futility look

Single replicate used by
[`calibrate_matched_futility_boundary`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_matched_futility_boundary.md):
simulates one trial under a fixed data-generating scenario, censors it
at `floor(futility_IF * total_events)` events, and returns the test
statistic from
[`survival_test`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)
(positive Z favours treatment).

## Usage

``` r
single_matched_futility_rep(
  i,
  n_c,
  n_t,
  data_generating_model,
  recruitment_model,
  futility_IF,
  total_events,
  analysis_model,
  seed = NULL
)
```

## Arguments

- i:

  Replicate index (used with `seed`).

- n_c, n_t:

  Number of patients in the control / treatment group.

- data_generating_model:

  True scenario: `lambda_c`, `delay_time`, `post_delay_HR`, optionally
  `gamma_c`.

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

- seed:

  Optional integer seed; replicate `i` uses `seed * 10000 + i`.

## Value

A one-row data frame with column `Z`.
