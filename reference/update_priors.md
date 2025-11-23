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

  @param effect_model A named list specifying beliefs about the
  treatment effect:

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

## Details

The function performs Bayesian updating under a delayed-effect model:
\$\$ h_T(t) = \begin{cases} \lambda_c, & t \le T \\ \lambda_c \times HR,
& t \> T \end{cases} \$\$

Priors for `lambda_c`, `T`, and `HR` are constructed from elicited
distributions using the SHELF framework, then updated through
sampling-based posterior inference.

## Examples

``` r
if (FALSE) { # \dontrun{
posterior_df <- update_priors(
  data = interim_data,
  control_distribution = "Exponential",
  control_model = control_prior_list,
  delay_SHELF = delay_shelf_obj,
  HR_SHELF = hr_shelf_obj,
  delay_param_dist = "lognormal",
  HR_param_dist = "lognormal",
  n_samples = 2000
)
} # }


```
