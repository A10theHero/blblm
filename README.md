
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blblm

The goal of blblm is to apply the Bag of Little Bootstraps method to
Linear Regression. This package draws inspiration from `stats::lm()` and
its related functions. It also incorporates `Rcpp` and `furrr` to
improve performance as well as some basic `Tidyverse` principles to
improve the elegance of the code.

For full documentation, please see the vignette.

## Installation

You can install `blblm` from here using:

``` r
devtools::devtools::install_github("a10thehero/blblm")
```

## Example

The vignette should be consulted to get a better understanding of the
package, but here are some usage examples anyways.

``` r
library(blblm)


future::plan(future::multiprocess, workers = 2)
mtcars %>% blblm(mpg ~ cyl, m = 2, B = 500, useParallel = TRUE) %>% Adj.Rsq.blblm(confidence = TRUE, level = 0.95)
#>   Adj.Rsq       lwr       upr 
#> 0.6939897 0.6360200 0.7139894


mtcars %>% blblm(mpg ~ 0 + hp * cyl, m = 2, B = 101) %>% coef()
#>          hp         cyl      hp:cyl 
#>  0.30327910  3.56710201 -0.04679175


mtcars %>% blblm(wt ~ mpg, m = 3, B = 200) %>% confint(level = 0.99)
#>                   0.5%       99.5%
#> (Intercept)  5.0760567  6.70703551
#> mpg         -0.1744034 -0.09705388
```
