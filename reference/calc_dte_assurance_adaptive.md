# Calculates operating characteristics for a Group Sequential Trial with a Delayed Treatment Effect

Simulates assurance and operating characteristics for a group sequential
trial under prior uncertainty about a delayed treatment effect. The
function integrates beliefs about control survival, treatment delay,
post-delay hazard ratio, recruitment, and group sequential design (GSD)
parameters.

## Usage

``` r
calc_dte_assurance_adaptive(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  GSD_model,
  analysis_model = NULL,
  update_priors_sims = 1000,
  n_BPP_sims = 1000,
  n_sims = 1000
)
```

## Arguments

- n_c:

  Control group sample size

- n_t:

  Treatment group sample size

- control_model:

  A named list specifying the control arm survival distribution:

  - `dist`: Distribution type ("Exponential" or "Weibull")

  - `parameter_mode`: Either "Fixed" or "Distribution"

  - `fixed_type`: If "Fixed", specify as "Parameters" or "Landmark"

  - `lambda`, `gamma`: Scale and shape parameters

  - `t1`, `t2`: Landmark times

  - `surv_t1`, `surv_t2`: Survival probabilities at landmarks

  - `t1_Beta_a`, `t1_Beta_b`, `diff_Beta_a`, `diff_Beta_b`: Beta prior
    parameters

- effect_model:

  A named list specifying beliefs about the treatment effect:

  - `delay_SHELF`, `HR_SHELF`: SHELF objects encoding beliefs

  - `delay_dist`, `HR_dist`: Distribution types ("hist" by default)

  - `P_S`: Probability that survival curves separate

  - `P_DTE`: Probability of delayed separation, conditional on
    separation

- recruitment_model:

  A named list specifying the recruitment process:

  - `method`: "power" or "PWC"

  - `period`, `power`: Parameters for power model

  - `rate`, `duration`: Comma-separated strings for PWC model

- GSD_model:

  A named list specifying the group sequential design:

  - `events`: Total number of events

  - `alpha_spending`: Cumulative alpha spending vector

  - `alpha_IF`: Information Fraction(s) at which we look for efficacy

  - `futility_type`: One of `"none"`, `"Beta"` (pre-specified
    beta-spending, via `rpact`), `"BPP"` (Bayesian Predictive
    Probability futility, D3-style), or `"MatchedZ"` (a fixed,
    externally-calibrated Z-statistic cutoff, non-binding – D4/D5-style;
    see
    [`calibrate_matched_futility_boundary`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_matched_futility_boundary.md)
    for how to obtain `futility_boundary_Z`).

  - `futility_IF`: Information Fraction at which we look for futility
    (required for `"BPP"` and `"MatchedZ"`).

  - `beta_spending`: Cumulative beta spending vector (`"Beta"` only).

  - `BPP_threshold`: BPP value below which we stop for futility (`"BPP"`
    only).

  - `futility_boundary_Z`: Z-statistic value below which we stop for
    futility (`"MatchedZ"` only).

- analysis_model:

  A named list specifying the final analysis and decision rule:

  - `method`: e.g. `"LRT"`, `"WLRT"`, or `"MW"`.

  - `alpha`: one-sided type I error level.

  - `alternative_hypothesis`: direction of the alternative (e.g.
    `"one.sided"`).

  - `rho`, `gamma`, `t_star`, `s_star`: additional parameters for WLRT
    or MW (if applicable).

- update_priors_sims:

  Number of posterior samples per interim dataset, passed to
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)
  (default 1000). Only used when `GSD_model$futility_type == "BPP"`;
  harmless (ignored) otherwise.

- n_BPP_sims:

  Number of predictive simulations per interim dataset, passed to
  [`BPP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/BPP_func.md)
  (default 1000). Only used when `GSD_model$futility_type == "BPP"`;
  harmless (ignored) otherwise.

- n_sims:

  Number of simulations to run (default = 1000)

## Value

A data frame with one row per simulated trial and the following columns:

- Trial:

  Simulation index

- Decision:

  Final interim/final decision outcome – one of `"Stop for efficacy"`,
  `"Stop for futility"`, `"Successful at final"`, or
  `"Unsuccessful at final"`. This is the single source of truth for
  trial outcome; use it directly rather than deriving success/failure
  independently.

- StopTime:

  Time at which the trial stopped or completed

- SampleSize:

  Total sample size at the time of decision

- Success:

  Logical recode of `Decision` for convenience: `TRUE` if
  `Decision %in% c("Stop for efficacy", "Successful at final")`, `FALSE`
  otherwise. Derived directly and only from `Decision` – see "Bug fix"
  below.

- Converged:

  For `"BPP"` designs, whether the interim MCMC fit converged (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md));
  `NA` for other futility types, which involve no MCMC step.

Class: `data.frame`

## Bug fix (this version)

previous versions of this function independently recomputed a separate
`Final_Decision` field from a hardcoded Cox proportional-hazards Wald
statistic at a flat `qnorm(0.975)` threshold, regardless of
`analysis_model$method` or the design's actual group-sequential
boundaries. This was a second, separate copy of the same bug fixed in
`apply_GSD_to_trial()` (see its documentation), and could silently
disagree with the trial's own `Decision`. This version removes that
duplicate computation entirely: `Success` is now derived only from
`Decision`, which is itself computed once, correctly, inside
`apply_GSD_to_trial()`, via `analysis_model$method` and the design's
real boundaries. This also collapses what were previously three
near-duplicate branches (one per futility type) into a single call path,
since `apply_GSD_to_trial()` already dispatches correctly on
`GSD_model$futility_type` – removing the code duplication that allowed
the two copies of the bug to drift apart in the first place.

## Examples

``` r
set.seed(123)
control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
fixed_type = "Parameters", lambda = 0.1)
effect_model <- list(P_S = 1, P_DTE = 0,
HR_SHELF = SHELF::fitdist(c(0.6, 0.65, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 2),
HR_dist = "gamma",
delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
delay_dist = "gamma"
)
recruitment_model <- list(method = "power", period = 12, power = 1)
GSD_model <- list(events = 300, alpha_spending = c(0.0125, 0.025),
                  alpha_IF = c(0.75, 1), futility_type = "none")
result <- calc_dte_assurance_adaptive(n_c = 300, n_t = 300,
                        control_model = control_model,
                        effect_model = effect_model,
                        recruitment_model = recruitment_model,
                        GSD_model = GSD_model,
                        n_sims = 10)
str(result)
#> 'data.frame':    10 obs. of  6 variables:
#>  $ Trial     : int  1 2 3 4 5 6 7 8 9 10
#>  $ Decision  : chr  "Stop for efficacy" "Successful at final" "Stop for efficacy" "Stop for efficacy" ...
#>  $ StopTime  : num  12.5 13.6 12 12.4 12 ...
#>  $ SampleSize: int  600 600 600 600 600 600 600 600 600 600
#>  $ Success   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ Converged : logi  NA NA NA NA NA NA ...
```
