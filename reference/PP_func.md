# Calculate the predictive probability (PP) given interim data and posterior samples

Calculate the predictive probability (PP) given interim data and
posterior samples

## Usage

``` r
PP_func(
  data,
  posterior_df,
  control_distribution = "Exponential",
  n_c_planned,
  n_t_planned,
  rec_time_planned,
  df_cens_time,
  analysis_model,
  censoring_model = NULL,
  future_boundaries = NULL,
  n_sims
)
```

## Arguments

- data:

  A data frame containing interim survival data, censored at
  `df_cens_time`, with columns:

  - `time` Final observed/event time at the interim (on the analysis
    time scale).

  - `group` Treatment group indicator (e.g. "Control", "Treatment").

  - `rec_time` Recruitment (calendar) time.

  - `pseudo_time` `time + rec_time` (calendar time at event/censoring).

  - `status` Event indicator at the interim (1 = event, 0 = censored).

  - `survival_time` Observed follow-up time from randomisation to
    event/censoring at the interim.

- posterior_df:

  A data frame of posterior samples with columns: `lambda_c`,
  `delay_time` and `HR`, corresponding to the control hazard, the delay
  (changepoint) time and the post-delay hazard ratio, respectively, plus
  `gamma_c` (the Weibull shape) when `control_distribution = "Weibull"`.

- control_distribution:

  Distributional form assumed for the control arm: either
  `"Exponential"` (default) or `"Weibull"`.

- n_c_planned:

  Planned maximum number of patients in the control group.

- n_t_planned:

  Planned maximum number of patients in the treatment group.

- rec_time_planned:

  Planned maximum recruitment calendar time for the full trial.

- df_cens_time:

  Calendar time at which `df` has been censored (interim analysis time).

- analysis_model:

  A named list specifying the analysis method and decision rule:

  - `method`: e.g. `"LRT"`, `"WLRT"`, or `"MW"`.

  - `alpha`: one-sided type I error level (only used in the legacy
    fallback path, see `censoring_model` below).

  - `alternative_hypothesis`: direction of the alternative (e.g.
    `"one.sided"`).

  - `rho`, `gamma`, `t_star`, `s_star`: additional parameters for WLRT
    or MW (if applicable).

- censoring_model:

  **Legacy fallback, deprecated** (see the Deprecated section). Used
  only if `future_boundaries` is `NULL`. A named list specifying a
  single censoring mechanism for the future data (`method`: one of
  `"Time"`, `"Events"`, or `"IF"`; plus the corresponding
  `time`/`events`/`IF` parameter), with success determined by
  `analysis_model$alpha` rather than a design-specific critical value.

- future_boundaries:

  **Recommended.** A list of future, pre-specified, fixed decision
  points to evaluate on each posterior-predictive draw, in ascending
  chronological order, matching the trial's own group-sequential design.
  Each element is a list with:

  - `events`: the cumulative event count at that analysis (the final
    element should be the maximum planned event count).

  - `crit`: the pre-specified critical value for the test statistic `Z`
    at that analysis (e.g. from `design$criticalValues` of an `rpact`
    group-sequential design).

  On each predictive draw, boundaries are checked in order: if `Z`
  crosses a boundary, the draw is recorded as successful and evaluation
  stops (mirroring the group-sequential design's own early-stopping
  logic); if the final boundary in the chain is reached without
  crossing, the draw is recorded as unsuccessful. This evaluates the
  trial-success event: crossing any future efficacy boundary, or
  rejecting \\H_0\\ at the final analysis.

  **Scope:** `future_boundaries` must contain only fixed (non-Bayesian)
  decision points. A design with more than one future
  Bayesian-predictive-probability futility look is not supported here,
  as it would require nested posterior-predictive simulation at each
  look; see the package/manuscript Limitations.

- n_sims:

  Number of posterior-predictive draws (required, no default).

## Value

A list with a single element `PP_df`, a data frame with columns
`success` (0/1, whether this predictive draw ultimately rejects \\H_0\\,
accounting for any future fixed efficacy boundaries) and `Z_val` (the
test statistic at the analysis where the draw was decided). The
predictive probability is `mean(PP_df$success)`.

## Details

**Model.** The control arm is Weibull with rate `lambda_c` and shape
`gamma_c`; an exponential control arm
(`control_distribution = "Exponential"`) is handled as the Weibull case
with `gamma_c = 1`, using the same code path. The treatment arm follows
the control hazard up to `delay_time` and the control hazard multiplied
by `HR` afterwards. Each predictive draw samples one row of
`posterior_df`, simulates event times for patients not yet recruited,
and simulates residual event times for patients censored at the interim,
conditional on their follow-up so far.

**Recruitment assumption.** Recruitment times for patients not yet
recruited at the interim are drawn from
`Uniform(df_cens_time, rec_time_planned)`. This is correct for uniform
recruitment (the `"power"` recruitment model with `power = 1`) only; the
simulation functions that call `PP_func()` stop if another recruitment
model is supplied.

## Deprecated

The exponential control arm (`control_distribution = "Exponential"`) and
the legacy single-stage `censoring_model` fallback are deprecated and
will be removed in a future release. Use
`control_distribution = "Weibull"` (an exponential control arm is the
Weibull case with `gamma_c = 1`) and `future_boundaries`.

## Examples

``` r
set.seed(123)
n <- 30
cens_time <- 15

time <- runif(n, 0, 12)
rec_time <- runif(n, 0, 12)

df <- data.frame(
  time = time,
  group = c(rep("Control", n/2), rep("Treatment", n/2)),
  rec_time = rec_time
)

df$pseudo_time <- df$time + df$rec_time
df$status <- df$pseudo_time < cens_time
df$survival_time <- ifelse(df$status == TRUE, df$time, cens_time - df$rec_time)

posterior_df <- data.frame(HR = rnorm(20, mean = 0.75, sd = 0.05),
                           delay_time = rep(0, 20),
                           lambda_c = rnorm(20, log(2)/9, sd = 0.01))

analysis_model <- list(method = "LRT", alpha = 0.025,
                       alternative_hypothesis = "one.sided")

# Recommended usage: pass the design's own future boundaries, e.g. a single
# efficacy look at 20 events (Z > 2.24) followed by a final analysis at
# 28 events (Z > 2.00):
future_boundaries <- list(
  list(events = 20, crit = 2.24),
  list(events = 28, crit = 2.00)
)

PP_outcome <- PP_func(df, posterior_df,
           control_distribution = "Exponential",
           n_c_planned = n/2, n_t_planned = n/2,
           rec_time_planned = 12, df_cens_time = 15,
           analysis_model = analysis_model,
           future_boundaries = future_boundaries,
           n_sims = 10)
```
