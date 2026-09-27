# Calculate statistical significance on a survival dataset

Performs a survival analysis using the standard log-rank test (LRT), a
weighted log-rank test in the Fleming-Harrington family (WLRT), or the
modestly-weighted log-rank test of Magirr and Burman (MW). The function
estimates the hazard ratio and determines whether the result is
statistically significant based on the specified alpha level and
alternative hypothesis.

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

  Method of analysis: `"LRT"` (default) for standard log-rank test,
  `"WLRT"` for a Fleming-Harrington-family weighted log-rank test, or
  `"MW"` for the modestly-weighted log-rank test.

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

  Parameter \\t^\*\\ used in the modestly weighted test.

- s_star:

  Parameter \\s^\*\\ used in the modestly weighted test.

## Value

A list containing:

- Signif:

  Logical indicator of statistical significance based on the chosen test
  and alpha level.

- observed_HR:

  Estimated hazard ratio from a Cox proportional hazards model.

- Z:

  Signed test statistic, oriented so that positive values favour the arm
  coded as "Treatment" (or the second factor level of `group`,
  alphabetically, if levels are unlabelled) – i.e. positive Z
  corresponds to a hazard ratio below 1 (benefit). This convention is
  consistent across all three methods (verified by diagnostic: see notes
  below) and is what group-sequential boundary comparisons in
  `apply_GSD_to_trial`/`BPP_func` rely on.

## Sign convention notes (verified by diagnostic, 2026)

- **LRT**: uses
  [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)'s
  own `(exp[2] - obs[2])` construction, correctly signed by construction
  (positive = benefit).

- **WLRT**: uses
  [`nph::logrank.test()`](https://rdrr.io/pkg/nph/man/logrank.test.html)'s
  native `$test$z` directly. Confirmed correctly signed against LRT on
  an unambiguous large-benefit case (HR = 0.21: LRT Z = 13.20,
  WLRT(rho=0,gamma=1) Z = 13.96 – same sign, comparable magnitude).

- **MW**: [`nphRCT::wlrt()`](https://rdrr.io/pkg/nphRCT/man/wlrt.html)'s
  `$z` uses the OPPOSITE sign convention to `survdiff()` (confirmed by
  diagnostic: on the same unambiguous large-benefit case, HR = 0.245,
  LRT gave Z = +12.27 while raw `wlrt()$z` gave -12.30 – same magnitude,
  flipped sign). The sign is therefore negated below (`Z <- -test$z`) so
  that positive Z consistently means benefit across all three methods.

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
