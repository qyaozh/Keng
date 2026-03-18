#' Conduct a priori power analysis, and plan the sample size for regression analysis
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
#' @param sig_level Expected significance level for effects of focal predictors.
#' @param power Expected statistical power for effects of focal predictors.
#' @param n_ul The upper limit of sample size below which the minimum required sample size is searched.
#' Non-integer `n_ul` would be converted to be an integer using `as.integer()`.
#' `as.integer(n_ul)` should be at least `as.integer(PA) + 1`.
#'
#' @details `n_ul` determine the total times of power_lm()'s attempts searching for the minimum required sample size,
#' hence the number of rows of the returned power table `priori` and the right limit of the horizontal axis of the returned power plot.
#' `power_lm()` will keep searching for the minimum required sample size that pushes the power level to `power`.
#' When PRE is very small (e.g., less than 0.001) and power is larger than 0.8,
#' a huge increase in sample size only brings about a trivial increase in power, which is cost-ineffective.
#' To make `power_lm()` omit unnecessary attempts, you could set `n_ul` to be a value less than 1.45e+09 (e.g., 10000).
#'
#' @return A Keng_power class, also a list. If sample size `n` is not given, the following results would be returned:
#' `[[1]]` `PRE`;
#' `[[2]]` `f_squared`, Cohen's f_squared derived from PRE;
#' `[[3]]` `PC`;
#' `[[4]]` `PA`;
#' `[[5]]` `sig_level`, expected significance level for effects of focal predictors;
#' `[[6]]` `power`, expected statistical power for effects of focal predictors;
#' `[[7]]` `n_ul`, the upper limit of sample size;
#' `[[8]]` `minimum`, the minimum sample size `n_i` required for focal predictors to reach the
#' expected statistical power and significance level, and corresponding
#' `df_A_C`(the df of the numerator of the F-test, i.e., the difference of the dfs between model C and model A),
#' `df_A_i`(the df of the denominator of the F-test, i.e., the df of the model A at the sample size `n_i`),
#' `F_i`(the *F*-test of `PRE` at the sample size `n_i`),
#' `p_i`(the p-value of `F_i`),
#' `lambda_i`(the non-centrality parameter of the F-distribution for the alternative hypothesis, given `PRE` and `n_i`),
#' `power_i`(the actual power of `PRE` at the sample size `n_i`);
#' `[[9]]` `priori`, a priori power table with increasing sample sizes (`n_i`) and power(`power_i`).
#'
#' By default, `print()` prints the primary but not all contents of the `Keng_power` class.
#' To inspect more contents, use `print.AsIs()` or list extracting.
#'
#' @references Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples power_lm()
#' print(power_lm())
#' plot(power_lm())
power_lm <- function(PRE = 0.02,
                     PC = 1L,
                     PA = 2L,
                     sig_level = 0.05,
                     power = 0.8,
                     n_ul = 1.45e+09L) {
  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n_ul <- as.integer(n_ul)

  stopifnot(
    sum(sapply(
      list(PRE, PC, PA, sig_level, power, n_ul), is.null
    )) == 0,
    PRE > 0,
    # If PRE = 0, the computation would be endless.
    PRE < 1,
    # PRE would be subjected to the F-test, hence should be smaller than 1.
    PC >= 0,
    PC < PA,
    # power is only used to filter the minimum sample size, and it does not influence the loop.
    # Actually, power coule be set be negative.
    power >= 0,
    # power could be 1.
    # If PRE is very very small, n_i will grow towards Inf. Limit the maximum n_i to be 100,000.
    power <= 1,
    sig_level > 0,
    # if sig_level = 0, the computation would be endless.
    sig_level <= 1,
    n_ul >= PA + 1 # uniroot search begin from n_i = PA + 1
  )

  f_squared <- PRE / (1 - PRE)
  df_A_C <- PA - PC

  # a priori power
  root <- tryCatch({
    # code may return error
    uniroot(
      function(n) powered_lm(PRE = PRE, PC = PC, PA = PA, n = n, sig_level = sig_level)$power - power,
      interval = c(PA + 1L, n_ul),
      tol = .Machine$double.eps
    )$root
  }, error = function(e) {
    # deal with error
    message(
      paste0("The minimum required sample size does not exist within the range of 3-",n_ul,"."))
    # minimum <- NA
    return(NA)
  })

  # minimum sample size
  minimum <- ceiling(root)

  # power plot data
  if (is.na(root)) {
    n_i <- seq(PA + 1L, n_ul)
  } else {
    n_i <- seq(PA + 1L, minimum)
  }
  priori <- powered_lm(PRE = PRE, PC = PC, PA = PA, n = n_i, sig_level = sig_level)
  priori <- data.frame(priori)
  names(priori) <- c("n_i", "df_A_C", "df_A_i", "F_i", "p_i", "lambda_i", "power_i")

  method <- "power_lm"

  out <- structure(
    list(
      PRE = PRE,
      f_squared = f_squared,
      PC = PC,
      PA = PA,
      sig_level = sig_level,
      power = power,
      n_ul = n_ul,
      root = root,
      minimum = minimum,
      priori = priori,
      method = method
    ),
    class = c("Keng_power", "list")
  )

  out
}
