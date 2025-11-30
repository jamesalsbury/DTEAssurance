# Launch the 'shiny' adaptive assurance app

Launches a 'shiny' application to simulate group sequential trials with
delayed treatment effects (DTE) using elicited prior distributions. The
app allows interactive exploration of trial designs and assurance
calculations.

## Usage

``` r
assurance_adaptive_shiny_app()
```

## Value

No return value, called for side effects (invisibly returns NULL). The
function launches an interactive 'shiny' application.

## Examples

``` r
if (interactive()) {
  # Launch the interactive app in an R session
  assurance_adaptive_shiny_app()
}
```
