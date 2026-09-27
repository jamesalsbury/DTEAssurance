# Function to calculate the 'optimal' information fraction to calculate BPP

Function to calculate the 'optimal' information fraction to calculate
BPP

## Usage

``` r
calibrate_BPP_timing(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  IA_model,
  analysis_model,
  future_boundaries = NULL,
  update_priors_sims = 1000,
  PP_sims = 2000,
  n_sims = 100,
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

  A named list specifying the censoring mechanism for the future data:

  - `events`: Number of events which is 100% information fraction

  - `IF`: A vector of candidate information fractions to evaluate.
    **Every candidate must genuinely precede `future_boundaries`** (see
    below) – e.g. if the true design has an efficacy look at IF = 0.75,
    do not include candidates at or beyond 0.75 in this sweep; a
    futility look scheduled at or after the design's own efficacy look
    is not a well-posed question about that design.

- analysis_model:

  A named list specifying the final analysis and decision rule (see
  [`BPP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/BPP_func.md)
  for details).

- future_boundaries:

  **Recommended.** The chain of fixed future decision points (efficacy
  look(s) and final analysis) of the true group-sequential design under
  consideration – see
  [`BPP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/BPP_func.md).
  The same chain is used for every candidate in `IA_model$IF` (each
  candidate is checked to ensure it genuinely precedes every boundary in
  the chain; see `single_calibration_rep()`). If `NULL` (not
  recommended), falls back to the legacy single-stage calculation, and a
  message is emitted.

- update_priors_sims:

  Number of posterior samples per interim dataset (default 1000; was
  previously hardcoded to 100).

- PP_sims:

  Number of predictive simulations per interim dataset (default 2000;
  was previously hardcoded to 50).

- n_sims:

  Number of interim datasets to simulate per candidate timing (default
  is 100).

- n_cores:

  Number of cores to parallelise over via
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html)
  (default is 1, i.e. sequential `lapply`).

- seed:

  Optional integer seed; if supplied, replicate `i` under candidate
  timing `k` is seeded as `seed * 10000 + i` (note: replicates are
  re-seeded identically across candidate timings, so the same underlying
  trial trajectories are reused – i.e. paired comparison across timings
  – unless you deliberately want independent draws per timing, in which
  case pass a different `seed` per call).

## Value

A list with:

- outcome_list:

  A list, one element per candidate information fraction, each
  containing `BPP_values` (a vector of estimated BPP, one per simulated
  interim dataset) and `cens_time` (the corresponding calendar times).

- settings:

  A list recording the exact settings used, plus the installed
  `DTEAssurance` package version, for provenance.

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

# Sweep candidates from 0.2 to 0.7 only -- all genuinely precede the
# design's own efficacy look at IF = 0.75.
IA_model <- list(events = 40, IF = seq(0.2, 0.7, by = 0.1))

analysis_model <- list(method = "LRT", alpha = 0.025,
                      alternative_hypothesis = "one.sided")

future_boundaries <- list(
  list(events = 30, crit = 2.24),   # efficacy look at IF = 0.75
  list(events = 40, crit = 2.00)    # final analysis
)

timing <- calibrate_BPP_timing(n_c = 25, n_t = 25,
                     control_model = control_model,
                     effect_model = effect_model,
                     recruitment_model = recruitment_model,
                     IA_model = IA_model,
                     analysis_model = analysis_model,
                     future_boundaries = future_boundaries,
                     n_sims = 2)
```
