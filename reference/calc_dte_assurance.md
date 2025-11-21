# Calculate Assurance for a Trial with a Delayed Treatment Effect

Simulates operating characteristics for a clinical trial under prior
uncertainty about a delayed treatment effect. The function integrates
beliefs about control survival, treatment delay, post-delay hazard
ratio, recruitment, censoring, and analysis method to estimate assurance
and other trial metrics.

## Usage

``` r
calc_dte_assurance(
  n_c,
  n_t,
  control_model,
  effect_model,
  censoring_model,
  recruitment_model,
  analysis_model,
  n_sims = 1000
)
```

## Arguments

- n_c:

  Vector of control group sample sizes

- n_t:

  Vector of treatment group sample sizes

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

- censoring_model:

  A named list specifying the censoring mechanism:

  - `method`: "Time", "Events", or "IF"

  - `time`, `events`, `IF`: Parameters for each method

- recruitment_model:

  A named list specifying the recruitment process:

  - `method`: "power" or "PWC"

  - `period`, `power`: Parameters for power model

  - `rate`, `duration`: Comma-separated strings for PWC model

- analysis_model:

  A named list specifying the statistical test and decision rule:

  - `method`: "LRT", "WLRT", or "MW"

  - `alpha`, `alternative_hypothesis`: Type I error and hypothesis
    direction

  - `rho`, `gamma`, `t_star`, `s_star`: Parameters for WLRT or MW

  - `success_threshold_HR`: Optional threshold for declaring success

- n_sims:

  Number of simulations to run (default = 1000)

## Value

A named list containing:

- assurance:

  Estimated assurance (probability of success under prior uncertainty)

- CI:

  95% confidence interval for assurance

- duration:

  Mean trial duration across simulations

- sample_size:

  Mean sample size across simulations

- diagnostics:

  Additional diagnostics if `success_threshold_HR` is specified

Class: `list`

## Examples

``` r
# Minimal example with placeholder inputs
control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
fixed_type = "Parameters", lambda = 0.1)
effect_model <- list(delay_SHELF = SHELF::fitdist(c(3, 4, 5),
probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
delay_dist = "gamma",
HR_SHELF = SHELF::fitdist(c(0.55, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1.5),
HR_dist = "gamma",
P_S = 1, P_DTE = 0)
censoring_model <- list(method = "Time", time = 12)
recruitment_model <- list(method = "power", period = 12, power = 1)
analysis_model <- list(method = "LRT", alpha = 0.025, alternative_hypothesis = "two.sided")
result <- calc_dte_assurance(n_c = 300, n_t = 300,
                                     control_model = control_model,
                                     effect_model = effect_model,
                                     censoring_model = censoring_model,
                                     recruitment_model = recruitment_model,
                                     analysis_model = analysis_model,
                                     n_sims = 10)
#> Warning: Caught simpleError. Canceling all iterations ...
#> Error in apply_censoring(data, censoring_model): could not find function "apply_censoring"
str(result)
#> function (future, ...)  
```
