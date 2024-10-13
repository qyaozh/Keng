
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Keng

<!-- badges: start -->
<!-- badges: end -->

The goal of Keng is to …

## Installation

You can install the development version of Keng from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("qyaozh/Keng")
```

## Example

``` r
library(Keng)
```

``` r
cut_r(193)
#>             p         r
#> Result1 0.100 0.1187512
#> Result2 0.050 0.1412906
#> Result3 0.010 0.1850106
#> Result4 0.001 0.2350568
```

``` r
test_r(0.2, 193)
#>           r         se        t           p
#> Result1 0.2 0.07089554 2.821052 0.005292479
```
