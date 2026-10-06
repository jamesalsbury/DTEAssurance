# Function to calculate the 'optimal' PP threshold value

Function to calculate the 'optimal' PP threshold value

## Usage

``` r
calibrate_PP_threshold(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  IA_model,
  analysis_model,
  data_generating_model,
  future_boundaries = NULL,
  n_df_sims = 100,
  update_priors_sims = 1000,
  PP_sims = 2000,
  n_cores = 1,
  seed = NULL
)
```

## Arguments

- n_c:

  Number of control patients

- n_t:

  Number of treatment patients

- control_model:

  A named list specifying the control arm survival distribution (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  for details).

- effect_model:

  A named list specifying beliefs about the treatment effect (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  for details).

- recruitment_model:

  A named list specifying the recruitment process (see
  [`add_recruitment_time`](https://jamesalsbury.github.io/DTEAssurance/reference/add_recruitment_time.md)
  for details).

- IA_model:

  A named list specifying the interim analysis timing:

  - `events`: total planned event count (100% information fraction).

  - `IF`: the information fraction at which the interim futility look
    occurs (i.e. where the data are censored to compute PP).

- analysis_model:

  A named list specifying the analysis method (see
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  for details).

- data_generating_model:

  A named list specifying the true data-generating parameters used to
  simulate trials for calibration:

  - `lambda_c`: hazard rate for the control group.

  - `gamma_c`: Weibull shape parameter (`NULL` for Exponential).

  - `delay_time`: true delay before the treatment effect begins.

  - `post_delay_HR`: true post-delay hazard ratio.

- future_boundaries:

  **Recommended.** A list of future, pre-specified, fixed decision
  points (efficacy look(s) and the final analysis) to check on each
  posterior-predictive draw, matching the true group-sequential design
  under which this PP threshold will actually be used – see
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  for the exact structure. If `NULL` (not recommended, kept only for
  backward compatibility), PP is computed via the legacy single-stage
  fallback: censoring directly to `IA_model$events` and testing at a
  flat `analysis_model$alpha`, which does NOT account for any future
  efficacy boundary the real design may have. A message is emitted if
  this fallback is used.

- n_df_sims:

  Number of interim datasets to simulate (default is 100). For
  calibration decisions near a specific power/Type I error target,
  consider a substantially larger number and report Monte Carlo standard
  errors alongside the result.

- update_priors_sims:

  Number of posterior samples to generate per interim dataset via
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  (default is 1000).

- PP_sims:

  Number of predictive simulations used to estimate PP for each interim
  dataset, passed to
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)
  (default is 2000).

- n_cores:

  Number of cores to parallelise over via
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html)
  (default is 1, i.e. sequential `lapply`; not supported on Windows for
  `n_cores > 1`, per base R's `mclapply` limitations).

- seed:

  Optional integer seed. If supplied, each simulated interim dataset `i`
  is seeded reproducibly as `seed * 10000 + i`. If `NULL` (default), no
  seed is set internally – set one in the calling script if
  reproducibility across runs is required.

## Value

A list with:

- PP_vec:

  A numeric vector of length `n_df_sims`, the estimated PP for each
  simulated interim dataset.

- settings:

  A list of all arguments, plus the package, R, rjags and JAGS versions,
  the git commit of the working directory, the hostname and a timestamp.

## Deprecated

`calibrate_PP_threshold()` is deprecated and will be removed in a future
release. Use
[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
with
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md)
and
[`select_kappa_star`](https://jamesalsbury.github.io/DTEAssurance/reference/select_kappa_star.md).

## Examples

``` r
set.seed(123)
control_model <- list(dist = "Exponential", parameter_mode = "Distribution",
                      t1 = 12, t1_Beta_a = 20, t1_Beta_b = 32)

effect_model <- list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5),
                    probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                    delay_dist = "gamma",
                    HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7),
                    probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                    HR_dist = "gamma",
                    P_S = 1, P_DTE = 0)

recruitment_model <- list(method = "power", period = 12, power = 1)

IA_model <- list(events = 40, IF = 0.5)

analysis_model <- list(method = "LRT", alpha = 0.025,
                       alternative_hypothesis = "one.sided")

data_generating_model <- list(lambda_c = log(2)/12, gamma_c = NULL,
                              delay_time = 3, post_delay_HR = 0.75)

# A single future efficacy look at 30 events (Z > 2.24), then final at 40
# events (Z > 2.00) -- match this to the true design's own boundaries.
future_boundaries <- list(
  list(events = 30, crit = 2.24),
  list(events = 40, crit = 2.00)
)

threshold <- calibrate_PP_threshold(n_c = 25, n_t = 25,
                     control_model = control_model,
                     effect_model = effect_model,
                     recruitment_model = recruitment_model,
                     IA_model = IA_model,
                     analysis_model = analysis_model,
                     data_generating_model = data_generating_model,
                     future_boundaries = future_boundaries,
                     n_df_sims = 2)
```
