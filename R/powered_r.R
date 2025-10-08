#' Compute r's post-hoc power
#'
#' @param r Pearson's correlation. Cohen(1988) suggested >=0.1, >=0.3, and >=0.5 as cut-off values
#' of Pearson's correlation r for small, medium, and large effect sizes, respectively.
#' @param sig_level Expected significance level.
#' @param n The current sample size. Non-integer `n` would be converted to be an integer using `as.integer()`.
#' `n` should be at least 3.
#'
#' @returns Integer `n`, the t_test of `r` at the sample size `n` with `df`, `SE` of `r`,
#' `p` (the p-value of t-test), and the post-hoc power analysis with `delta`
#' (the non-centrality parameter of the t-distribution for the alternative hypothesis),
#' and `power` (the post-hoc power of `r` at the sample size `n`).
#'
#' @references Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples powered_r()
powered_r <- function(r = 0.2,
                      n = 200L,
                      sig_level = 0.05) {
  # coerce n to be a integer
  n <- as.integer(n)

  # check arguments
  stopifnot(r > -1,
            r < 1,
            r != 0,
            sig_level > 0,
            sig_level <= 1,
            identical(n, integer(0)) || n >= 3)

  # computation
  df <- n - 2L
  SE <- sqrt((1 - r^2) / df)
  t <- r / SE
  p <- stats::pt(abs(t), df, lower.tail = FALSE) * 2
  d <- 2 * r / sqrt(1 - r^2) #approximate estimate of d
  delta <- d * sqrt(df) / 2
  power <- stats::pt(stats::qt(1 - sig_level / 2, df),
                          df,
                          abs(delta),
                          lower.tail = FALSE)
  # return
  t_test <- list(
    n = n,
    df = df,
    SE = SE,
    t = t,
    p = p,
    delta = delta,
    power = power
  )
  t_test
}
