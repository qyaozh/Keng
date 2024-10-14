
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Keng (庚)

<!-- badges: start -->
<!-- badges: end -->

The `Keng` package is named after Loo-Keng Hua, who made great
achievements in mathematics mainly through self-study. Hua’s spirit
encourages Qingyao, and inspires the `Keng` package.

The `Keng` package aims to automate the computations Qingyao repeat in
his research and teaching. Hope the functions and data gathered in this
package help to ease your life.

## Installation

You can install the development version of `Keng` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("qyaozh/Keng")
```

## Load

Before using the `Keng` package, load `Keng` using `library()` function.

``` r
library(Keng)
```

## List of contents

Here is a list of the functions and data gathered in the `Keng` package.
Their Useges are detailed in the documentation.

`cut_r()` give you the cut-off values of Pearson’s *r* at the
significance levels of *p* = 0.1, 0.05, 0.01, 0.001 when the sample size
*n* is known.

`test_r()` tests the significance of *r* using *t*-test when *r* and *n*
is known.

`PRE` computes *PRE* and test it using *F* test.