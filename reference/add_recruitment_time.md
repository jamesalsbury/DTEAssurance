# Add recruitment time to a survival dataset

Simulates recruitment timing for each subject in a survival dataset
using either a power model or a piecewise constant (PWC) model. The
function appends recruitment times and pseudo survival times (time from
recruitment to event or censoring).

## Usage

``` r
add_recruitment_time(
  data,
  rec_method,
  rec_period = NULL,
  rec_power = NULL,
  rec_rate = NULL,
  rec_duration = NULL
)
```

## Arguments

- data:

  A dataframe containing survival data with columns: `time`, `status`,
  and `group`

- rec_method:

  Recruitment method: `"power"` for power model or `"PWC"` for piecewise
  constant model

- rec_period:

  Period length for the power model

- rec_power:

  Power parameter for the power model

- rec_rate:

  Comma-separated string of recruitment rates for the PWC model

- rec_duration:

  Comma-separated string of durations corresponding to each rate in the
  PWC model

## Value

A dataframe with two additional columns:

- rec_time:

  Simulated recruitment time for each subject

- pseudo_time:

  Time from recruitment to event or censoring

Class: `data.frame`

## Examples

``` r
set.seed(123)
df <- data.frame(
  time = rexp(20, rate = 0.1),
  status = rbinom(20, 1, 0.8),
  group = rep(c("Control", "Treatment"), each = 10)
)
recruited <- add_recruitment_time(df, rec_method = "power", rec_period = 12, rec_power = 1)
head(recruited)
#>         time status   group  rec_time pseudo_time
#> 1  8.4345726      1 Control  2.478377   10.912949
#> 2  5.7661027      1 Control  1.530380    7.296483
#> 3 13.2905487      1 Control  9.039694   22.330243
#> 4  0.3157736      1 Control 10.740544   11.056318
#> 5  0.5621098      1 Control  4.493553    5.055663
#> 6  3.1650122      1 Control  7.981382   11.146394
```
