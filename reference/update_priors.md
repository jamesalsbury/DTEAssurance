# Update prior distributions using interim survival data

This function updates elicited priors (defined through SHELF objects and
parametric prior distributions) using interim survival data under a
delayed-effect, piecewise-exponential model for the treatment arm and an
exponential or Weibull model for the control arm.

## Usage

``` r
update_priors(
  data,
  control_model,
  effect_model,
  n.chains = 2,
  n_burnin = 500,
  n_samples = 1000,
  rhat_threshold = 1.1
)
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

- n.chains:

  Number of MCMC chains to run (default is 2)

- n_burnin:

  Number of burn-in samples for the MCMC chain(s) (default is 500)

- n_samples:

  Number of posterior samples to generate (default is 1000)

- rhat_threshold:

  Convergence threshold on the Gelman-Rubin potential scale reduction
  factor (default 1.1). Used only to compute the `"converged"` attribute
  on the return value; does not affect sampling.

## Value

A data frame containing Monte Carlo samples from the updated (posterior)
distribution of the model parameters, with columns `lambda_c`,
`delay_time`, `HR`, `gamma_c` (Weibull only), and `Z` (the latent
scenario indicator: 1 = no separation, 2 = immediate separation, 3 =
delayed separation). Column access (`posterior_df$lambda_c`, etc.) is
unchanged from previous versions of this function – existing calling
code does not need to be modified. In addition, three attributes are
attached to the returned data frame for diagnostic purposes:

- `rhat`:

  A named numeric vector of per-parameter Gelman-Rubin point estimates
  (accessed via `attr(posterior_df, "rhat")`).

- `converged`:

  `TRUE` if every monitored parameter's Rhat is below `rhat_threshold`,
  `FALSE` if not, or `NA` if the diagnostic could not be computed
  (accessed via `attr(posterior_df, "converged")`).

- `Z_probs`:

  A named numeric vector `c(P_Z1=, P_Z2=, P_Z3=)`, the posterior
  probability of each latent state, i.e.
  `table(posterior_df$Z) / nrow(posterior_df)` (accessed via
  `attr(posterior_df, "Z_probs")`).

Priors for `lambda_c`, `T`, and `HR` are constructed from elicited
distributions using the SHELF framework, then updated through
sampling-based posterior inference.

## Examples

``` r
set.seed(123)
interim_data = data.frame(survival_time = runif(10, min = 0, max = 10),
status = rbinom(10, size = 1, prob = 0.5),
group = c(rep("Control", 5), rep("Treatment", 5)))
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

posterior_df <- update_priors(
  data = interim_data,
  control_model = control_model,
  effect_model = effect_model,
  n_samples = 10)

# Diagnostics, e.g.:
attr(posterior_df, "rhat")
#>         HR          Z delay_time   lambda_c 
#>  0.9935484        NaN        NaN  1.0234645 
attr(posterior_df, "converged")
#> [1] TRUE
attr(posterior_df, "Z_probs")
#> P_Z1 P_Z2 P_Z3 
#>    0    1    0 
```
