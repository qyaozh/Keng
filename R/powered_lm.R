#' Compute lm's post-hoc power
#'
#' @param PRE Proportional Reduction in Error.
#' PRE = The square of partial correlation.
#' Cohen (1988) suggested >=0.02, >=0.13, and >=0.26 as cut-off values of PRE for small,
#' medium, and large effect sizes, respectively.
#' @param PC Number of parameters of model C (compact model) without focal predictors of interest.
#' Non-integer `PC` would be converted to be an integer using `as.integer()`.
#' @param PA Number of parameters of model A (augmented model) with focal predictors of interest.
#' Non-integer `PA` would be converted to be an integer using `as.integer()`.
#' `as.integer(PA)` should be larger than `as.integer(PC)`.
#' @param n The current sample size. Non-integer `n` would be converted to be an integer using `as.integer()`.
#' Non-NULL `as.integer(n)` should be at least `as.integer(PA) + 1`.
#' @param sig_level Expected significance level for effects of focal predictors.
#'
#' @returns Integer `n`, the F_test of `PRE` at the sample size `n` with
#' `df_A_C`,
#' `df_A` (the df of the model A at the sample size `n`),
#' `F` (the F-test of `PRE` at the sample size `n`),
#' `p` (the p-value of F-test at the sample size `n`), and the post-hoc power analysis with
#' `lambda` (the non-centrality parameter of `F` at the sample size `n`),
#' and `power` (the post-hoc power at the sample size `n`).

#' @export
#'
#' @examples powered_lm()
powered_lm <- function(PRE = 0.04,
         PC = 1L,
         PA = 2L,
         n = 200L,
         sig_level = 0.05) {

  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n <- as.integer(n)

  stopifnot(
    sum(sapply(list(
      PRE, PC, PA, n, sig_level
    ), is.null)) == 0,
    PRE > 0,
    # If PRE = 0, the computation would be endless.
    PRE < 1,
    # PRE would be subjected to the F-test, hence should be smaller than 1.
    PC >= 0,
    PC < PA,
    # power is only used to filter the minimum sample size, and it does not influence the loop.
    # Actually, power coule be set be negative.
    sig_level > 0,
    # if sig_level = 0, the computation would be endless.
    sig_level <= 1,
    # Use ||, so if n is integer(0), (n > PA) would not be evaluated.
    identical(n, integer(0)) || n >= PA + 1
  )

  f_squared <- PRE / (1 - PRE)
  df_A_C <- PA - PC

  # post-hoc power
  df_A <- n - PA
  F <- (PRE / df_A_C) / ((1 - PRE) / df_A)
  p <- stats::pf(F, df_A_C , df_A, lower.tail = FALSE)
  lambda <- f_squared * df_A
  power <- stats::pf(stats::qf((1 - sig_level), df_A_C , df_A),
                          df_A_C,
                          df_A,
                          lambda,
                          lower.tail = FALSE)
  F_test <- list(
    n = n,
    df_A_C = df_A_C,
    df_A = df_A,
    F = F,
    p = p,
    lambda = lambda,
    power = power
  )
  F_test
}
