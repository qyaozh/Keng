#' Test the significance, analyze the power, and plan the sample size for r.
#'
#' @param r Pearson's correlation. Cohen(1988) suggested >=0.1, >=0.3, and >=0.5 as cut-off values
#' of Pearson's correlation r for small, medium, and large effect sizes, respectively.
#' @param sig_level Expected significance level.
#' @param power Expected statistical power.
#' @param n Sample size of r. Non-integer `n` would be converted to be a integer using `as.integer()`.
#' `n` should be at least 3.
#'
#' @details To test the significance of the r using one-sample t-test,
#' the SE of `r` is determined by the following formula: `SE = sqrt((1 - r^2)/(n - 2))`.
#' Another way is transforming `r` to Fisher's z using the following formula:
#' `fz = atanh(r)` with the SE of `fz` being `sqrt(n - 3)`.
#' Fisher's z is commonly used to compare two Pearson's correlations from independent samples.
#' Fisher's transformation is presented here only for satisfying the curiosity of users who are
#' interested in the difference of t-test and Fisher's transformation.
#'
#' The post-hoc power of `r`'s t-test is computed through the way of Aberson (2019).
#' Other softwares and R packages like SPSS and `pwr` give different power estimates due to
#' underlying different formulas. `Keng` adopts Aberson's approach because this approach guarantees
#' the equivalence of r and PRE.
#'
#' @return A list with the following results:
#' `[[1]]` `r`, the given r;
#' `[[2]]` `d`, Cohen's d derived from `r`; Cohen (1988) suggested >=0.2, >=0.5, and >=0.8
#' as cut-off values of `d` for small, medium, and large effect sizes, respectively.
#' `[[3]]` Integer `n`;
#' `[[4]]` t-test of `r` (incl., `r`, `df` of r, `SE_r`, `t`, `p_r`),
#' 95% CI of `r` based on t -test (`LLCI_r_t`, `ULCI_r_t`),
#' and post-hoc power of `r` (incl., `delta_post`, `power_post`);
#' `[[5]]` Fisher's z transformation (incl., `fz` of `r`, z-test of `fz` \[`SE_fz`, `z`, `p_fz`\],
#' and 95% CI of `r` derived from `fz`.
#'
#' Note that the returned CI of `r` may be out of r's valid range \[-1, 1\].
#' This "error" is deliberately left to users, who should correct the CI manually in reports.
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
                   sig_level = 0.05,
                   power = 0.80){
  n <- as.integer(n)

  stopifnot(r > -1,
            r < 1,
            r != 0,
            power > 0,
            power <= 1,
            sig_level > 0,
            sig_level <= 1,
            !identical(n, integer(0)),
            n >= 3)

  d <- 2 * r / sqrt(1 - r ^ 2) # approximate estimate of d

  # t-test of r
  df <- n - 2
  SE_r <- sqrt((1 - r ^ 2) / df)
  t <- r / SE_r
  p_r <- stats::pt(abs(t), df, lower.tail = FALSE) * 2
  CI_t_half <- stats::pt(q = 1 - sig_level / 2, df = df) * SE_r
  LLCI_r_t <- r - CI_t_half
  ULCI_r_t <- r + CI_t_half

  # power of r based on delta
  delta_post <- d * sqrt(df) / 2
  power_post <- stats::pt(stats::qt(1 - sig_level / 2, df), df, abs(delta_post), lower.tail = FALSE)

  # CI_z, CI based on Fisher's z
  fz <- atanh(r) + r / (2 * (n - 1))# Adjustment (r/(2*(n - 1))) is ignored to simplify the computation.
  SE_fz <- 1 / sqrt(n - 3)
  z <- fz / SE_fz
  p_fz <- stats::pnorm(abs(z), lower.tail = FALSE) * 2
  CI_z_half <- stats::qnorm(1 - sig_level / 2) * SE_fz
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

