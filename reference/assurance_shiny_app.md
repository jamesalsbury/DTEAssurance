# Launch the 'shiny' Assurance app

Launches a 'shiny' application to calculate assurance for clinical
trials where delayed treatment effects (DTE) may be present. The app
allows elicitation of prior distributions and calculates assurance
metrics.

## Usage

``` r
assurance_shiny_app()
```

## Value

No return value, called for side effects (invisibly returns NULL). The
function launches an interactive 'shiny' application.

## Examples

``` r
if (interactive()) {
  # Launch the interactive app in an R session
  assurance_shiny_app()
}
```
