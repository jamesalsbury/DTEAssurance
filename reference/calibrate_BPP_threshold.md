# Function to calculate the 'optimal' BPP threshold value

Function to calculate the 'optimal' BPP threshold value

## Usage

``` r
calibrate_BPP_threshold(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  IA_model,
  analysis_model,
  data_generating_model,
  n_sims = 100
)
```

## Arguments

- n_c:

  Number of control patients

- n_t:

  Number of treatment patients

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

- IA_model:

  A named list specifying the censoring mechanism for the future data:

  - `events`: Number of events which is 100% information fraction

  - `IF`: The information fraction at which to censor and calculate BPP

- analysis_model:

  A named list specifying the final analysis and decision rule:

  - `method`: e.g. `"LRT"`, `"WLRT"`, or `"MW"`.

  - `alpha`: one-sided type I error level.

  - `alternative_hypothesis`: direction of the alternative (e.g.
    `"one.sided"`).

  - `rho`, `gamma`, `t_star`, `s_star`: additional parameters for WLRT
    or MW (if applicable).

- data_generating_model:

  A named list specifying the parameters for the data-generating
  mechanism

  - `lambda_c`: hazard rate for the control group

  - `delay_time`: time at which the treatment starts to take effect

  - `post_delay_HR`: hazard ratio, after `delay_time`

- n_sims:

  Number of data sets to simulate (default is 100).

## Value

A vector of length `n_sims` corresponding to the value of BPP for each
simulated trial

## Examples

``` r
set.seed(123)
control_model = list(dist = "Exponential",
                     parameter_mode = "Distribution",
                     t1 = 12,
                     t1_Beta_a = 20,
                     t1_Beta_b = 32)

effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5),
                    probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                    delay_dist = "gamma",
                    HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7),
                    probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                    HR_dist = "gamma",
                    P_S = 1,
                    P_DTE = 0)

recruitment_model <- list(method = "power", period = 12, power = 1)

IA_model = list(events = 20, IF = 0.5)

analysis_model = list(method = "LRT",
                      alpha = 0.025,
                      alternative_hypothesis = "one.sided")


data_generating_model = list(lambda_c = log(2)/12,
                             delay_time = 3,
                             post_delay_HR = 0.75)


threshold <- calibrate_BPP_threshold(n_c = 15, n_t = 15,
                     control_model = control_model,
                     effect_model = effect_model,
                     recruitment_model = recruitment_model,
                     IA_model = IA_model,
                     analysis_model = analysis_model,
                     data_generating_model = data_generating_model,
                     n_sims = 2)
```
