# Update prior distributions using interim survival data

This function updates elicited priors (defined through SHELF objects and
parametric prior distributions) using interim survival data under a
delayed-effect, piecewise-exponential model for the treatment arm and an
exponential or Weibull model for the control arm.

## Usage

``` r
update_priors(
  data,
  control_distribution = "Exponential",
  control_model,
  delay_SHELF,
  HR_SHELF,
  delay_param_dist,
  HR_param_dist,
  n_samples = 1000
)
```

## Arguments

- data:

  A data frame containing interim survival data with columns:

  - `survival_time` Observed time from randomisation to event/censoring.

  - `status` Event indicator (1 = event, 0 = censored).

  - `group` Group identifier (e.g., "Control", "Treatment").

- control_distribution:

  Distributional form assumed for the control arm: either
  `"Exponential"` (default) or `"Weibull"`.

- control_model:

  A named list specifying the elicited prior for the control arm.
  Expected elements depend on the distribution:

  - `lambda_c_SHELF`, `lambda_c_dist` Prior for baseline hazard
    (Exponential).

  - `gamma_c_SHELF`, `gamma_c_dist` Prior for Weibull shape (if
    applicable).

  - `s1_SHELF`, `s1_dist` Prior for survival at time `t_1`.

  - `parameter_mode` Character string indicating which parameterisation
    is used when eliciting Weibull priors.

  - `t_1` Time point at which survival probability was elicited.

- delay_SHELF:

  A SHELF elicitation object encoding prior belief about the length of
  the delay period \\\tau\\ in the treatment arm.

- HR_SHELF:

  A SHELF elicitation object encoding prior belief about the post-delay
  hazard ratio.

- delay_param_dist:

  Parametric form chosen to represent the prior for the delay time.

- HR_param_dist:

  Parametric form chosen to represent the prior for the post-delay HR.

- n_samples:

  Number of posterior samples to generate (default: 1000).

## Value

A data frame containing Monte Carlo samples from the updated (posterior)
distribution of the model parameters. Columns normally include:

- `lambda_c` Posterior samples for the control hazard parameter.

- `delay_time` Posterior samples for the delay/changepoint time
  \\\tau\\.

- `HR` Posterior samples for the post-delay hazard ratio.

- `gamma_c` (only if `control_distribution = "Weibull"`) Posterior
  samples for the Weibull shape parameter.

## Details

The function performs Bayesian updating under a delayed-effect model:
\$\$ h_T(t) = \begin{cases} \lambda_c, & t \le \tau \\ \lambda_c \times
HR, & t \> \tau \end{cases} \$\$

Priors for `lambda_c`, `tau`, and `HR` are constructed from elicited
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
