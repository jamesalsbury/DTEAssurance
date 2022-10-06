
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DTEAssurance

<!-- badges: start -->
<!-- badges: end -->

DTE is an R package which elicits two distributions and then calculates
assurance using these elicited prior distributions. The package
implements the methods as described in

-   [this paper](https://jamesalsbury.github.io/)

## Installation

You can install the development version of DTEAssurance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jamesalsbury/DTEAssurance")
```

## shiny app

The only function contained within the package is one which launches a
`shiny` app. The app allows you to implement all of the methods outlined
in [this paper](https://jamesalsbury.github.io/). To launch the app, run

``` r
DTEAssurance::DTEAssuranceApp()
```
