# Update prior distributions using interim survival data

This function updates elicited priors (defined through SHELF objects and
parametric prior distributions) using interim survival data under a
delayed-effect, piecewise-exponential model for the treatment arm and an
exponential or Weibull model for the control arm.

## Usage

``` r
update_priors(data, control_model, effect_model, n_samples = 1000)
```

## Arguments

- data:

  A data frame containing interim survival data with columns:

  - `survival_time` Observed time from randomisation to event/censoring.

  - `status` Event indicator (1 = event, 0 = censored).

  - `group` Group identifier (e.g., "Control", "Treatment").

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

- n_samples:

  Number of posterior samples to generate (default: 1000).

## Value

A data frame containing Monte Carlo samples from the updated (posterior)
distribution of the model parameters. Columns normally include:

- `lambda_c` Posterior samples for the control hazard parameter.

- `delay_time` Posterior samples for the delay/changepoint time \\T\\.

- `HR` Posterior samples for the post-delay hazard ratio.

- `gamma_c` (only if `control_distribution = "Weibull"`) Posterior
  samples for the Weibull shape parameter.

Priors for `lambda_c`, `T`, and `HR` are constructed from elicited
distributions using the SHELF framework, then updated through
sampling-based posterior inference.

## Examples

``` r
interim_data = data.frame(survival_time = runif(10, min = 0, max = 1),
status = rbinom(10, size = 1, prob = 0.5),
group = c(rep("Control", 5), rep("Treatment", 5)))
control_model = list(dist = "Exponential",
                     parameter_mode = "Distribution",
                     t1 = 12,
                     t1_Beta_a = 20,
                     t1_Beta_b = 32)
effect_model = list(delay_SHELF = SHELF::fitdist(c(5.5, 6, 6.5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 12),
                    delay_dist = "gamma",
                    HR_SHELF = SHELF::fitdist(c(0.5, 0.6, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 1),
                    HR_dist = "gamma",
                    P_S = 1,
                    P_DTE = 0)

posterior_df <- update_priors(
  data = interim_data,
  control_model = control_prior_list,
  effect_model = effect_model,
  n_samples = 10)
#> Error: object 'control_prior_list' not found

```
