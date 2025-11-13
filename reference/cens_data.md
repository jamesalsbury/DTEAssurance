# Censor a survival dataset

Applies administrative censoring to a survival dataset using one of
three methods: fixed time, fixed number of events, or fixed information
fraction. The input data must contain columns for pseudo survival time,
recruitment time, and observed time.

## Usage

``` r
cens_data(
  data,
  cens_method = "Time",
  cens_time = NULL,
  cens_IF = NULL,
  cens_events = NULL
)
```

## Arguments

- data:

  A dataframe containing uncensored survival data with columns:
  `pseudo_time`, `rec_time`, and `time`

- cens_method:

  Censoring method: `"Time"` (default), `"Events"`, or `"IF"`

- cens_time:

  Time point for censoring (required if `cens_method = "Time"`)

- cens_IF:

  Information fraction for censoring (required if `cens_method = "IF"`)

- cens_events:

  Number of events for censoring (required if `cens_method = "Events"`)

## Value

A list containing:

- data:

  Censored dataframe with updated `status` and filtered rows

- cens_events:

  Number of events used for censoring (if applicable)

- cens_time:

  Time point used for censoring

- sample_size:

  Number of subjects remaining after censoring

## Examples

``` r
set.seed(123)
df <- data.frame(
  pseudo_time = rexp(20, rate = 0.1),
  rec_time = runif(20, 0, 12),
  time = rexp(20, rate = 0.1)
)
censored <- cens_data(df, cens_method = "Time", cens_time = 10)
str(censored)
#> List of 4
#>  $ data       :'data.frame': 19 obs. of  6 variables:
#>   ..$ pseudo_time  : num [1:19] 0.292 0.316 0.562 1.453 2.81 ...
#>   ..$ rec_time     : num [1:19] 1.83 3.82 2.78 4.96 5.59 ...
#>   ..$ time         : num [1:19] 5.89 2.6 25.97 6.29 72.11 ...
#>   ..$ status       : int [1:19] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ enrolled     : logi [1:19] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ survival_time: num [1:19] 5.89 2.6 25.97 6.29 72.11 ...
#>  $ cens_events: NULL
#>  $ cens_time  : num 10
#>  $ sample_size: int 19
```
