
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Keng <img src="man/figures/logo.png" align="right" height="139" alt="Keng" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/Keng)](https://CRAN.R-project.org/package=Keng)
<!-- badges: end -->

`Keng` is the abbreviation of “Knock Errors off Nice Guesses.” Hope the
functions and data gathered in the `Keng` package help to ease your
life.

## Installation

You can install the development version of `Keng` from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("qyaozh/Keng", dependencies = TRUE, build_vignettes = TRUE)
```

## Load

Before using the `Keng` package, load it using the `library()` function.

``` r
library(Keng)
```

## List of contents

Here is a list of the data and functions gathered in the `Keng` package.
Their usages are detailed in the documentation.

### Data

`depress` is a subset of data from a research about depression and
coping.

### Variable transformation

`Scale()` could change the origin of a numeric vector `x` (including
mean-centering it), or standardize the mean and standard deviation of
`x` (including transforming it to its z-score).

### Pearson’s r

`cut_r()` gives you the cut-off values of Pearson’s r at the
significance levels of p = 0.1, 0.05, 0.01, and 0.001 with known sample
size n.

`test_r()` tests the significance and compute the post-hoc power of r
with known sample size n.

`power_r()` conducts a priori power analysis and plan the sample size
for r; post-hoc power analysis would also be conducted with known sample
size n. 

### The linear model

`compare_lm()` compares `lm()`’s fitted outputs using PRE,
R<sup>2</sup>, f<sup>2</sup>, and post-hoc power.

`calc_PRE()` calculates PRE from partial correlation, Cohen’s f, or
f_squared.

`power_lm()` conducts a priori power analysis and plans the sample size
for one or a set of predictors in regression analysis; post-hoc power
analysis would also be conducted with known sample size n.

### The `Keng_power` class

`power_r()` and `power_lm()` return the `Keng_power` class, which has
`print()` and `plot()` methods.

`print()` prints primary but not all contents of the `Keng_power` class.

`plot()` plots the power against sample size for the `Keng_power` class.

### pick\_\* tools

`pick_sl()` and `pick_dcb()` tools have been added to randomly pick
numbers for Chinese Super Lotto and Double Color Balls.
