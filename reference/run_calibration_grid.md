# Simulate PP values and true trial outcomes for PP-threshold calibration

For a single fixed data-generating scenario, simulates `n_sims` trials,
computes the predictive probability (PP) at the futility look, and
records what actually happens to each trial if it is allowed to continue
through the remaining fixed decision points in `future_boundaries`.
Because the PP value does not depend on the futility threshold
\\\kappa\\, the output can be summarised over a whole grid of thresholds
afterwards via
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
without re-simulating.

## Usage

``` r
run_calibration_grid(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  data_generating_model,
  futility_IF,
  total_events,
  future_boundaries,
  analysis_model,
  update_priors_sims = 1000,
  PP_sims = 2000,
  n_sims = 1000,
  n_cores = 1,
  seed = NULL
)
```

## Arguments

- n_c, n_t:

  Planned number of patients in the control / treatment group.

- control_model, effect_model:

  Prior specification used for the interim posterior update (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)).
  `control_model$parameter_mode` must be `"Distribution"`.

- recruitment_model:

  Recruitment specification (see
  [`add_recruitment_time`](https://jamesalsbury.github.io/DTEAssurance/reference/add_recruitment_time.md)):
  `method`, `period`, `power`, `rate`, `duration`.

- data_generating_model:

  A named list giving the true scenario used to simulate data:
  `lambda_c`, `delay_time`, `post_delay_HR`, and optionally `gamma_c`
  (Weibull control arm if supplied, exponential otherwise).

- futility_IF:

  Information fraction of the PP futility look.

- total_events:

  Maximum planned number of events.

- future_boundaries:

  List of future fixed decision points, each a list with `events` and
  `crit` (see
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)).

- analysis_model:

  Analysis specification (see
  [`survival_test`](https://jamesalsbury.github.io/DTEAssurance/reference/survival_test.md)).

- update_priors_sims:

  Number of posterior samples per interim dataset.

- PP_sims:

  Number of posterior-predictive simulations per interim dataset.

- n_sims:

  Number of simulated trials.

- n_cores:

  Number of cores (uses
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html) when
  \> 1).

- seed:

  Optional integer seed; replicate `i` uses `seed * 10000 + i`.

## Value

A list with elements

- `raw`:

  A data frame with one row per simulated trial and columns `PP_val`,
  `t_interim`, `sample_size_interim`, `continuation_success`,
  `continuation_stop_time`, `continuation_sample_size`, `converged`,
  `P_Z1`, `P_Z2`, `P_Z3`.

- `settings`:

  All arguments, plus the package, R, rjags and JAGS versions, the git
  commit of the working directory, the hostname and a timestamp.

## See also

[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md),
[`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md)
