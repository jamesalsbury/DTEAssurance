# Calculate Bayesian Predictive Probability given interim data and posterior samples

Calculate Bayesian Predictive Probability given interim data and
posterior samples

## Usage

``` r
BPP_func(
  data,
  posterior_df,
  control_distribution = "Exponential",
  n_c_planned,
  n_t_planned,
  rec_time_planned,
  df_cens_time,
  censoring_model,
  analysis_model,
  n_sims = 500
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
  (changepoint) time and the post-delay hazard ratio, respectively.

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

- censoring_model:

  A named list specifying the censoring mechanism for the future data:

  - `method`: one of `"Time"`, `"Events"`, or `"IF"`.

  - `time`, `events`, `IF`: parameters for the corresponding method.

- analysis_model:

  A named list specifying the final analysis and decision rule:

  - `method`: e.g. `"LRT"`, `"WLRT"`, or `"MW"`.

  - `alpha`: one-sided type I error level.

  - `alternative_hypothesis`: direction of the alternative (e.g.
    `"one.sided"`).

  - `rho`, `gamma`, `t_star`, `s_star`: additional parameters for WLRT
    or MW (if applicable).

- n_sims:

  Number of predictive simulations to run (default is 1000).

## Value

A single numeric value giving the Bayesian predictive probability of
success at the final analysis under the specified design, censoring
model and analysis model.

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


censoring_model = list(method = "Time", time = 25)
analysis_model = list(method = "LRT",
                      alpha = 0.025,
                      alternative_hypothesis = "one.sided")

BPP_outcome <- BPP_func(df,
           posterior_df,
           control_distribution = "Exponential",
           n_c_planned = n/2,
           n_t_planned = n/2,
           rec_time_planned = 12, df_cens_time = 15,
           censoring_model = censoring_model,
           analysis_model = analysis_model,
           n_sims = 10)
```
