# Calculate statistical significance on a survival dataset

Performs a survival analysis using either the standard log-rank test
(LRT) or a weighted log-rank test (WLRT). The function estimates the
hazard ratio and determines whether the result is statistically
significant based on the specified alpha level and alternative
hypothesis.

## Usage

``` r
survival_test(
  data,
  analysis_method = "LRT",
  alternative = "one.sided",
  alpha = 0.05,
  rho = 0,
  gamma = 0,
  t_star = NULL,
  s_star = NULL
)
```

## Arguments

- data:

  A dataframe containing survival data. Must include columns for
  survival time, event status, and treatment group.

- analysis_method:

  Method of analysis: `"LRT"` (default) for standard log-rank test, or
  `"WLRT"` for weighted log-rank test.

- alternative:

  String specifying the alternative hypothesis. Must be one of
  `"one.sided"` or `"two.sided"` (default).

- alpha:

  Type I error threshold for significance testing.

- rho:

  Rho parameter for the Fleming-Harrington weighted log-rank test.

- gamma:

  Gamma parameter for the Fleming-Harrington weighted log-rank test.

- t_star:

  Parameter \\t^\*\\ used in modestly weighted tests.

- s_star:

  Parameter \\s^\*\\ used in modestly weighted tests.

## Value

A list containing:

- Signif:

  Logical indicator of statistical significance based on the chosen test
  and alpha level.

- observed_HR:

  Estimated hazard ratio from a Cox proportional hazards model.

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
