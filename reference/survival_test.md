# Calculate statistical significance on a survival dataset

Performs a survival analysis using the standard log-rank test (LRT), a
weighted log-rank test in the Fleming-Harrington family (WLRT), or the
modestly-weighted log-rank test of Magirr and Burman (MW), and
determines whether the result is statistically significant at the
specified alpha level and alternative hypothesis.

## Usage

``` r
survival_test(
  data,
  analysis_method = "LRT",
  alternative = "one.sided",
  alpha,
  rho = 0,
  gamma = 0,
  t_star = NULL,
  s_star = NULL,
  return_HR = TRUE
)
```

## Arguments

- data:

  A dataframe containing survival data. Must include columns for
  survival time, event status, and treatment group.

- analysis_method:

  Method of analysis: `"LRT"` (default) for standard log-rank test,
  `"WLRT"` for a Fleming-Harrington-family weighted log-rank test, or
  `"MW"` for the modestly-weighted log-rank test.

- alternative:

  String specifying the alternative hypothesis: `"one.sided"` (default)
  or `"two.sided"`.

- alpha:

  Type I error level for the significance test (required, no default).
  Pass `NULL` if only `Z` is needed; `Signif` is then `NA`.

- rho:

  Rho parameter for the Fleming-Harrington weighted log-rank test.

- gamma:

  Gamma parameter for the Fleming-Harrington weighted log-rank test.

- t_star:

  Parameter \\t^\*\\ used in the modestly weighted test.

- s_star:

  Parameter \\s^\*\\ used in the modestly weighted test.

- return_HR:

  If `TRUE` (default), fit a Cox model and return the estimated hazard
  ratio as `observed_HR`. If `FALSE`, the Cox fit is skipped (it does
  not affect `Signif` or `Z`) and `observed_HR` is `NA`. The
  group-sequential and predictive probability code uses `FALSE`.

## Value

A list containing:

- Signif:

  Logical: whether the test is significant at level `alpha`. `FALSE` if
  `Z` is `NA`; `NA` if `alpha` is `NULL`.

- observed_HR:

  Estimated hazard ratio from a Cox proportional hazards model, or `NA`
  if `return_HR = FALSE`.

- Z:

  Signed test statistic. For all three methods, positive values favour
  the arm coded as "Treatment" (the second factor level of `group`),
  i.e. positive Z corresponds to a hazard ratio below 1.

## Examples

``` r
set.seed(123)
df <- data.frame(
  survival_time = rexp(40, rate = 0.1),
  status = rbinom(40, 1, 0.8),
  group = rep(c("Control", "Treatment"), each = 20)
)
result <- survival_test(df, analysis_method = "LRT", alpha = 0.05)
str(result)
#> List of 3
#>  $ Signif     : logi FALSE
#>  $ observed_HR: num 0.646
#>  $ Z          : num 1.21
```
