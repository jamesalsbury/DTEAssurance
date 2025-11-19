# Calculate Bayesian Predictive Probability given interim data and posterior samples

Calculate Bayesian Predictive Probability given interim data and
posterior samples

## Usage

``` r
BPP_func(
  df,
  posterior_df,
  n_c_planned,
  n_t_planned,
  rec_time_planned,
  df_cens_time,
  censoring_model,
  analysis_model,
  n_sims = 1000
)
```

## Arguments

- df:

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
if (FALSE) { # \dontrun{
  BPP_func(df, posterior_df, n_c_planned = 500, n_t_planned = 500,
           rec_time_planned = 34, df_cens_time = 20,
           censoring_model = list(method = "Events", events = 1200),
           analysis_model = list(method = "LRT",
                                 alpha = 0.025,
                                 alternative_hypothesis = "one.sided"))
} # }
```
