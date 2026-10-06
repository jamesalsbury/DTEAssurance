# Calculates operating characteristics for a Group Sequential Trial with a Delayed Treatment Effect

Simulates assurance and operating characteristics for a group sequential
trial under prior uncertainty about a delayed treatment effect. The
function integrates beliefs about control survival, treatment delay,
post-delay hazard ratio, recruitment, and group sequential design (GSD)
parameters.

## Usage

``` r
calc_dte_assurance_adaptive(
  n_c,
  n_t,
  control_model,
  effect_model,
  recruitment_model,
  GSD_model,
  analysis_model = NULL,
  update_priors_sims = NULL,
  PP_sims = NULL,
  n_sims = 1000
)
```

## Arguments

- n_c:

  Control group sample size

- n_t:

  Treatment group sample size

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

- GSD_model:

  A named list specifying the group sequential design:

  - `events`: Total number of events

  - `alpha_spending`: User-specified *cumulative* alpha spent by each
    efficacy look (one value per element of `alpha_IF`; the design uses
    `rpact`'s `typeOfDesign = "asUser"`). Futility looks are added to
    the design with zero alpha spent, so they do not change the efficacy
    critical values.

  - `alpha_IF`: Information Fraction(s) at which we look for efficacy

  - `futility_type`: One of `"none"`, `"Beta"` (pre-specified
    beta-spending, via `rpact`), `"PP"` (predictive probability
    futility, D3-style), or `"MatchedZ"` (a fixed, externally-calibrated
    Z-statistic cutoff, D4/D5-style; the rule is applied as binding,
    i.e. the simulated trial stops when Z falls below the cutoff; see
    [`calibrate_matched_futility_boundary`](https://jamesalsbury.github.io/DTEAssurance/reference/calibrate_matched_futility_boundary.md)
    for how to obtain `futility_boundary_Z`).

  - `futility_IF`: Information Fraction at which we look for futility
    (required for `"PP"` and `"MatchedZ"`).

  - `beta_spending`: Cumulative beta spending vector (`"Beta"` only).

  - `kappa`: PP value below which we stop for futility (`"PP"` only).

  - `futility_boundary_Z`: Z-statistic value below which we stop for
    futility (`"MatchedZ"` only).

- analysis_model:

  A named list specifying the final analysis and decision rule:

  - `method`: e.g. `"LRT"`, `"WLRT"`, or `"MW"`.

  - `alpha`: one-sided type I error level.

  - `alternative_hypothesis`: direction of the alternative (e.g.
    `"one.sided"`).

  - `rho`, `gamma`, `t_star`, `s_star`: additional parameters for WLRT
    or MW (if applicable).

- update_priors_sims:

  Number of posterior samples per chain for each interim dataset, passed
  to
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md).
  Required when `GSD_model$futility_type == "PP"`; ignored otherwise.

- PP_sims:

  Number of predictive simulations per interim dataset, passed to
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md).
  Required when `GSD_model$futility_type == "PP"`; ignored otherwise.

- n_sims:

  Number of simulations to run (default = 1000)

## Value

A data frame with one row per simulated trial and the following columns:

- Trial:

  Simulation index

- Decision:

  Final interim/final decision outcome – one of `"Stop for efficacy"`,
  `"Stop for futility"`, `"Successful at final"`, or
  `"Unsuccessful at final"`. This is the single source of truth for
  trial outcome; use it directly rather than deriving success/failure
  independently.

- StopTime:

  Time at which the trial stopped or completed

- SampleSize:

  Total sample size at the time of decision

- Success:

  Logical recode of `Decision` for convenience: `TRUE` if
  `Decision %in% c("Stop for efficacy", "Successful at final")`, `FALSE`
  otherwise.

- Converged:

  For `"PP"` designs, whether the interim MCMC fit converged (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md));
  `NA` for other futility types, which involve no MCMC step.

- PP_val:

  For `"PP"` designs, the predictive probability at the futility look;
  `NA` otherwise (or if the trial stopped for efficacy before the
  futility look).

- P_Z1, P_Z2, P_Z3:

  For `"PP"` designs, the posterior probabilities of the three latent
  states at the futility look; `NA` otherwise.

Class: `data.frame`, with attribute `settings`: a list of all arguments,
plus the package, R, rjags and JAGS versions, the git commit of the
working directory, the hostname and a timestamp.

## Examples

``` r
set.seed(123)
control_model <- list(dist = "Exponential", parameter_mode = "Fixed",
fixed_type = "Parameters", lambda = 0.1)
effect_model <- list(P_S = 1, P_DTE = 0,
HR_SHELF = SHELF::fitdist(c(0.6, 0.65, 0.7), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 2),
HR_dist = "gamma",
delay_SHELF = SHELF::fitdist(c(3, 4, 5), probs = c(0.25, 0.5, 0.75), lower = 0, upper = 10),
delay_dist = "gamma"
)
recruitment_model <- list(method = "power", period = 12, power = 1)
GSD_model <- list(events = 300, alpha_spending = c(0.0125, 0.025),
                  alpha_IF = c(0.75, 1), futility_type = "none")
result <- calc_dte_assurance_adaptive(n_c = 300, n_t = 300,
                        control_model = control_model,
                        effect_model = effect_model,
                        recruitment_model = recruitment_model,
                        GSD_model = GSD_model,
                        n_sims = 10)
str(result)
#> 'data.frame':    10 obs. of  10 variables:
#>  $ Trial     : int  1 2 3 4 5 6 7 8 9 10
#>  $ Decision  : chr  "Stop for efficacy" "Successful at final" "Stop for efficacy" "Stop for efficacy" ...
#>  $ StopTime  : num  12.5 13.6 12 12.4 12 ...
#>  $ SampleSize: int  600 600 600 600 600 600 600 600 600 600
#>  $ Success   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ Converged : logi  NA NA NA NA NA NA ...
#>  $ PP_val    : logi  NA NA NA NA NA NA ...
#>  $ P_Z1      : num  NA NA NA NA NA NA NA NA NA NA
#>  $ P_Z2      : num  NA NA NA NA NA NA NA NA NA NA
#>  $ P_Z3      : num  NA NA NA NA NA NA NA NA NA NA
#>  - attr(*, "settings")=List of 17
#>   ..$ n_c               : num 300
#>   ..$ n_t               : num 300
#>   ..$ control_model     :List of 4
#>   .. ..$ dist          : chr "Exponential"
#>   .. ..$ parameter_mode: chr "Fixed"
#>   .. ..$ fixed_type    : chr "Parameters"
#>   .. ..$ lambda        : num 0.1
#>   ..$ effect_model      :List of 6
#>   .. ..$ P_S        : num 1
#>   .. ..$ P_DTE      : num 0
#>   .. ..$ HR_SHELF   :List of 16
#>   .. .. ..$ Normal         :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean: num 0.65
#>   .. .. .. ..$ sd  : num 0.0741
#>   .. .. ..$ Student.t      :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location: num 0.65
#>   .. .. .. ..$ scale   : num 0.0654
#>   .. .. .. ..$ df      : num 3
#>   .. .. ..$ Skewnormal     :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location: num 0.65
#>   .. .. .. ..$ scale   : num 0.0741
#>   .. .. .. ..$ slant   : num 0
#>   .. .. ..$ Gamma          :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape: num 76.9
#>   .. .. .. ..$ rate : num 118
#>   .. .. ..$ Log.normal     :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean.log.X: num -0.432
#>   .. .. .. ..$ sd.log.X  : num 0.114
#>   .. .. ..$ Log.Student.t  :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location.log.X: num -0.432
#>   .. .. .. ..$ scale.log.X   : num 0.101
#>   .. .. .. ..$ df.log.X      : num 3
#>   .. .. ..$ Beta           :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape1: num 52
#>   .. .. .. ..$ shape2: num 108
#>   .. .. ..$ mirrorgamma    :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape: num 332
#>   .. .. .. ..$ rate : num 245
#>   .. .. ..$ mirrorlognormal:'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean.log.X: num 0.3
#>   .. .. .. ..$ sd.log.X  : num 0.0549
#>   .. .. ..$ mirrorlogt     :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location.log.X: num 0.3
#>   .. .. .. ..$ scale.log.X   : num 0.0484
#>   .. .. .. ..$ df.log.X      : num 3
#>   .. .. ..$ ssq            :'data.frame':    1 obs. of  10 variables:
#>   .. .. .. ..$ normal         : num 4.07e-31
#>   .. .. .. ..$ t              : num 5.77e-12
#>   .. .. .. ..$ skewnormal     : num 2.96e-31
#>   .. .. .. ..$ gamma          : num 2.67e-05
#>   .. .. .. ..$ lognormal      : num 6e-05
#>   .. .. .. ..$ logt           : num 5.8e-05
#>   .. .. .. ..$ beta           : num 7.2e-06
#>   .. .. .. ..$ mirrorgamma    : num 6.18e-06
#>   .. .. .. ..$ mirrorlognormal: num 1.39e-05
#>   .. .. .. ..$ mirrorlogt     : num 1.34e-05
#>   .. .. ..$ best.fitting   :'data.frame':    1 obs. of  1 variable:
#>   .. .. .. ..$ best.fit: chr "skewnormal"
#>   .. .. ..$ vals           : num [1, 1:3] 0.6 0.65 0.7
#>   .. .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. .. ..$ : NULL
#>   .. .. .. .. ..$ : NULL
#>   .. .. ..$ probs          : num [1, 1:3] 0.25 0.5 0.75
#>   .. .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. .. ..$ : NULL
#>   .. .. .. .. ..$ : NULL
#>   .. .. ..$ limits         :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ lower: num 0
#>   .. .. .. ..$ upper: num 2
#>   .. .. ..$ notes          : NULL
#>   .. .. ..- attr(*, "class")= chr "elicitation"
#>   .. ..$ HR_dist    : chr "gamma"
#>   .. ..$ delay_SHELF:List of 16
#>   .. .. ..$ Normal         :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean: num 4
#>   .. .. .. ..$ sd  : num 1.48
#>   .. .. ..$ Student.t      :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location: num 4
#>   .. .. .. ..$ scale   : num 1.31
#>   .. .. .. ..$ df      : num 3
#>   .. .. ..$ Skewnormal     :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location: num 4
#>   .. .. .. ..$ scale   : num 1.48
#>   .. .. .. ..$ slant   : num 0
#>   .. .. ..$ Gamma          :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape: num 7.29
#>   .. .. .. ..$ rate : num 1.76
#>   .. .. ..$ Log.normal     :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean.log.X: num 1.37
#>   .. .. .. ..$ sd.log.X  : num 0.381
#>   .. .. ..$ Log.Student.t  :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location.log.X: num 1.37
#>   .. .. .. ..$ scale.log.X   : num 0.335
#>   .. .. .. ..$ df.log.X      : num 3
#>   .. .. ..$ Beta           :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape1: num 4.5
#>   .. .. .. ..$ shape2: num 6.63
#>   .. .. ..$ mirrorgamma    :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ shape: num 16.4
#>   .. .. .. ..$ rate : num 2.69
#>   .. .. ..$ mirrorlognormal:'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ mean.log.X: num 1.78
#>   .. .. .. ..$ sd.log.X  : num 0.25
#>   .. .. ..$ mirrorlogt     :'data.frame':    1 obs. of  3 variables:
#>   .. .. .. ..$ location.log.X: num 1.78
#>   .. .. .. ..$ scale.log.X   : num 0.22
#>   .. .. .. ..$ df.log.X      : num 3
#>   .. .. ..$ ssq            :'data.frame':    1 obs. of  10 variables:
#>   .. .. .. ..$ normal         : num 7.7e-34
#>   .. .. .. ..$ t              : num 6.35e-12
#>   .. .. .. ..$ skewnormal     : num 3.08e-33
#>   .. .. .. ..$ gamma          : num 0.00029
#>   .. .. .. ..$ lognormal      : num 0.000644
#>   .. .. .. ..$ logt           : num 0.000623
#>   .. .. .. ..$ beta           : num 3.4e-05
#>   .. .. .. ..$ mirrorgamma    : num 0.000127
#>   .. .. .. ..$ mirrorlognormal: num 0.000283
#>   .. .. .. ..$ mirrorlogt     : num 0.000274
#>   .. .. ..$ best.fitting   :'data.frame':    1 obs. of  1 variable:
#>   .. .. .. ..$ best.fit: chr "normal"
#>   .. .. ..$ vals           : num [1, 1:3] 3 4 5
#>   .. .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. .. ..$ : NULL
#>   .. .. .. .. ..$ : NULL
#>   .. .. ..$ probs          : num [1, 1:3] 0.25 0.5 0.75
#>   .. .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. .. ..$ : NULL
#>   .. .. .. .. ..$ : NULL
#>   .. .. ..$ limits         :'data.frame':    1 obs. of  2 variables:
#>   .. .. .. ..$ lower: num 0
#>   .. .. .. ..$ upper: num 10
#>   .. .. ..$ notes          : NULL
#>   .. .. ..- attr(*, "class")= chr "elicitation"
#>   .. ..$ delay_dist : chr "gamma"
#>   ..$ recruitment_model :List of 3
#>   .. ..$ method: chr "power"
#>   .. ..$ period: num 12
#>   .. ..$ power : num 1
#>   ..$ GSD_model         :List of 4
#>   .. ..$ events        : num 300
#>   .. ..$ alpha_spending: num [1:2] 0.0125 0.025
#>   .. ..$ alpha_IF      : num [1:2] 0.75 1
#>   .. ..$ futility_type : chr "none"
#>   ..$ analysis_model    :List of 3
#>   .. ..$ method                : chr "LRT"
#>   .. ..$ alpha                 : num 0.025
#>   .. ..$ alternative_hypothesis: chr "one.sided"
#>   ..$ update_priors_sims: NULL
#>   ..$ PP_sims           : NULL
#>   ..$ n_sims            : num 10
#>   ..$ package_version   : chr "1.3.0"
#>   ..$ r_version         : chr "R version 4.6.1 (2026-06-24)"
#>   ..$ rjags_version     : chr "4.17"
#>   ..$ jags_version      :Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:3] 4 3 2
#>   ..$ git_commit        : chr "c6f664163295cd45631b96bcff1355f66501572d"
#>   ..$ hostname          : chr "runnervm8df0l"
#>   ..$ timestamp         : chr "2026-10-06 09:58:23.551781"
```
