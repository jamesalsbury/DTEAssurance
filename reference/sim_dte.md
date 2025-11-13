# Simulates survival times for a delayed treatment effect (DTE) scenario, where the treatment group experiences a delayed onset of benefit. Control and treatment groups are generated under exponential or Weibull distributions.

Simulates survival times for a delayed treatment effect (DTE) scenario,
where the treatment group experiences a delayed onset of benefit.
Control and treatment groups are generated under exponential or Weibull
distributions.

## Usage

``` r
sim_dte(
  n_c,
  n_t,
  lambda_c,
  delay_time,
  post_delay_HR,
  dist = "Exponential",
  gamma_c = NULL
)
```

## Arguments

- n_c:

  The number of patients in the control group

- n_t:

  The number of patients in the treatment group

- lambda_c:

  The baseline hazard rate for the control group

- delay_time:

  The length of delay before treatment effect begins

- post_delay_HR:

  The hazard ratio after the delay period

- dist:

  The distribution for the control group; must be one of "Exponential"
  (default) or "Weibull"

- gamma_c:

  The shape parameter for the Weibull distribution (only used if
  `dist = "Weibull"`)

## Value

A data frame with two columns:

- time:

  Simulated survival times

- group:

  Group assignment: "Control" or "Treatment"

Class: `data.frame`

## Examples

``` r
set.seed(123)
sim_data <- sim_dte(n_c = 10, n_t = 10, lambda_c = 0.1,
                    delay_time = 6, post_delay_HR = 0.6)
head(sim_data)
#>         time   group
#> 1 12.4626282 Control
#> 2  2.3787004 Control
#> 3  8.9409655 Control
#> 4  1.2441037 Control
#> 5  0.6137842 Control
#> 6 30.8880198 Control
```
