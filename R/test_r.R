#' Test the significance, analyze the power, and plan the sample size for r.
#'
#' @param r Pearson's correlation.
#' @param sig.level Expected significance level.
#' @param power Expected statistical power.
#' @param n Sample size of *r*. If *n* is given, the t-test, Fisher's z transformation,
#' post-hoc power analysis would be conducted.
#'
#' @details To test the significance of the *r* using one-sample *t*-test,
#' the *SE* of the *r* is determined by the following formula: `SE = sqrt((1 - r^2)/(n - 2))`.
#' Another way is transforming *r* to Fisher's z using the following formula:
#' `fz = atanh(r)` with the *SE* of `fz` being `sqrt(n - 3)`.
#' Note that Fisher's z is commonly used to compare two Pearson's correlations from independent samples.
#' Fisher's transformation is presented here only for satisfying the curiosity of users
#' interested in the difference of *t* -test and Fisher's transformation.
#'
#' @return A list with the following results: would also be returned:
#' `[[1]]` `r`, the given *r*;
#' `[[2]]` `d`, Cohen's *d* derived from `r`;  Cohen (1988) suggested >=0.2, >=0.5, and >=0.8
#' as cut-off values of `d` for small, medium, and large effect sizes, respectively.
#' `[[3]]` `minimum`, the minimum planned sample size `n_i` and corresponding
#' `df_i` (the `df` of *t*-test at the sample size `n_i`, `df_i` = `n_i` - 2),
#' `SE_r_i` (the *SE* of `r` at the sample size `n_i`),
#' `t_i` (the *t*-test of `r` at the sample size `n_i`),
#' `p_r_i` (the *p*-value of `t_i`),
#' `delta_i` (the non-centrality parameter of `t_i`),
#' `power_i` (the actual power at the sample size `n_i`).
#' `[[4]]` `prior`, a power table with increasing sample sizes and power;
#' `[[5]]`  A plot of power against sample size *n*.
#' The minimum *n* satisfying the expected statistical power and
#' significance level is annotated on the plot.
#'
#' If sample size *n* is given, the following results would also be returned:
#' `[[1]]` Integer `n`;
#' `[[2]]` *t* -test of *r* (`r`, `df` of *r*, `SE_r`, `t`, `p_r`),
#' 95% *CI* of *r* based on *t* -test (`LLCI_r_t`, `ULCI_r_t`),
#' and post-hoc power of *r* (Cohen's `d`, `delta_post`, `power_post`);
#' `[[3]]` Fisher's z transformation (`fz` of *r*, *z* -test of `fz` \[`SE_fz`, `z`, `p_fz`\], and 95% *CI* of *r* derived from `fz`.
#'
#' Note that the returned *CI* of *r* may be out of *r*'s valid range \[-1, 1\].
#' This "error" is deliberately left to users, who should correct the CI manually when reporting.
#'
#' @references Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples test_r(0.2, 193)
#'
#' # compare the p-values of t-test and Fisher's transformation
#' for (i in seq(30, 200, 10)) {
#' cat(c(
#'       "n =", i, ",",
#'        format(
#'         abs(test_r(0.2, i)[[1]][4] - test_r(0.2, i)[[2]][4]),
#'         nsmall = 12, scientific = FALSE)),
#'     fill = TRUE)
#' }

test_r <- function(r = NULL,
                   n = NULL,
                   sig.level = 0.05,
                   power = 0.80)
  {
  n <- as.integer(n)

  stopifnot(r > -1,
            r < 1,
            r != 0,
            power > 0,
            power <= 1,
            sig.level > 0,
            sig.level <= 1,
            !identical(n, integer(0)),
            n > 2)

  d <- 2 * r / sqrt(1 - r ^ 2) #approximate estimate of d

  # t-test of r
  df <- n - 2
  SE_r <- sqrt((1 - r ^ 2) / df)
  t <- r / SE_r
  p_r <- stats::pt(abs(t), df, lower.tail = FALSE) * 2
  CI_t_half <- stats::pt(q = 1 - sig.level / 2, df = df) * SE_r
  LLCI_r_t <- r - CI_t_half
  ULCI_r_t <- r + CI_t_half

  # power of r based on delta
  delta_post <- d * sqrt(df) / 2
  power_post <- stats::pt(stats::qt(1 - sig.level / 2, df), df, abs(delta_post), lower.tail = FALSE)

  # CI_z, CI based on Fisher's z
  fz <- atanh(r) + r / (2 * (n - 1))# Adjustment (r/(2*(n - 1))) is ignored to simplify the computation.
  SE_fz <- 1 / sqrt(n - 3)
  z <- fz / SE_fz
  p_fz <- stats::pnorm(abs(z), lower.tail = FALSE) * 2
  CI_z_half <- stats::qnorm(1 - sig.level / 2) * SE_fz
  LLCI_r_fz <- tanh(fz - CI_z_half)
  ULCI_r_fz <- tanh(fz + CI_z_half)

list(
  r = r,
  d = d,
  n = n,
  t_test = c(
    r = r,
    df = df,
    SE_r = SE_r,
    t = t,
    p_r = p_r,
    LLCI_r_t = LLCI_r_t,
    ULCI_r_t = ULCI_r_t,
    delta_post = delta_post,
    power_post = power_post
  ),
  Fisher_z = c(
    fz = fz,
    SE_fz = SE_fz,
    z = z,
    p_fz = p_fz,
    LLCI_r_fz = LLCI_r_fz,
    ULCI_r_fz = ULCI_r_fz
  )
)
}

