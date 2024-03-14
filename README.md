
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

## Example (from the paper)

First, we use SHELF to elicit beliefs about T, the length of the delay

``` r
DTEAssurance::DTEAssuranceApp()
```
