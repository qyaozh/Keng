
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Keng

<!-- badges: start -->
<!-- badges: end -->

The goal of `Keng` is to automate the computations Qingyao repeat in his
research and teaching. Hope these functions ease your life too.

## Installation

You can install the development version of `Keng` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("qyaozh/Keng")
```

## Example

Before using these functions `Keng` gathered, load `Keng` using
`library()` function.

``` r
library(Keng)
```

`cut_r()` give you the cut-off values of Pearson’s *r* at the
significance levels of *p* = 0.1, 0.05, 0.01, 0.001 When sample size *n*
is known.

``` r
cut_r(193)
#>             p         r
#> Result1 0.100 0.1187512
#> Result2 0.050 0.1412906
#> Result3 0.010 0.1850106
#> Result4 0.001 0.2350568
```

`r` with the absolute value larger than the cut-off value is significant
at the corresponding significance level. As the results above suggests,
when *n* = 193, \|r\| \> 0.1412906 is significant (e.g., ±0.15).

``` r
test_r(0.2, 193)
#>           r         se        t           p
#> Result1 0.2 0.07089554 2.821052 0.005292479
```

`test_r()` test the significance of *r* using *t*-test when *r* and *n*
is known.
