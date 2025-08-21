#' Conduct post hoc and a priori power analysis, and plan the sample size for regression analysis
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
#' @param power_ul The upper limit of power below which the minimum sample size is searched.
#' `power_ul` should be larger than `power`, and the maximum `power_ul` is 1.
#' @param n_ul The upper limit of sample size below which the minimum required sample size is searched.
#' Non-integer `n_ul` would be converted to be an integer using `as.integer()`.
#' `as.integer(n_ul)` should be at least `as.integer(PA) + 1`.
#' @param n The current sample size. Non-integer `n` would be converted to be an integer using `as.integer()`.
#' Non-NULL `as.integer(n)` should be at least `as.integer(PA) + 1`.
#'
#' @details `power_ul` and `n_ul` determine the total times of power_lm()'s attempts searching for the minimum required sample size,
#' hence the number of rows of the returned power table `priori` and the right limit of the horizontal axis of the returned power plot.
#' `power_lm()` will keep running and gradually raise the sample size to `n_ul` until the sample size pushes the power level to `power_ul`.
#' When PRE is very small (e.g., less than 0.001) and power is larger than 0.8,
#' a huge increase in sample size only brings about a trivial increase in power, which is cost-ineffective.
#' To make `power_lm()` omit unnecessary attempts, you could set `power_ul` to be a value less than 1 (e.g., 0.90),
#' and/or set `n_ul` to be a value less than 1.45e+09 (e.g., 10000).
#'
#' @return A Keng_power class, also a list. If sample size `n` is not given, the following results would be returned:
#' `[[1]]` `PRE`;
#' `[[2]]` `f_squared`, Cohen's f_squared derived from PRE;
#' `[[3]]` `PC`;
#' `[[4]]` `PA`;
#' `[[5]]` `sig_level`, expected significance level for effects of focal predictors;
#' `[[6]]` `power`, expected statistical power for effects of focal predictors;
#' `[[7]]` `power_ul`, the upper limit of power;
#' `[[8]]` `n_ul`, the upper limit of sample size;
#' `[[9]]` `minimum`, the minimum sample size `n_i` required for focal predictors to reach the
#' expected statistical power and significance level, and corresponding
#' `df_A_C`(the df of the numerator of the F-test, i.e., the difference of the dfs between model C and model A),
#' `df_A_i`(the df of the denominator of the F-test, i.e., the df of the model A at the sample size `n_i`),
#' `F_i`(the *F*-test of `PRE` at the sample size `n_i`),
#' `p_i`(the p-value of `F_i`),
#' `lambda_i`(the non-centrality parameter of the F-distribution for the alternative hypothesis, given `PRE` and `n_i`),
#' `power_i`(the actual power of `PRE` at the sample size `n_i`);
#' `[[10]]` `priori`, a priori power table with increasing sample sizes (`n_i`) and power(`power_i`).
#'
#' If sample size `n` is given, the following results would also be returned:
#' Integer `n`, the F_test of `PRE` at the sample size `n` with
#' `df_A_C`,
#' `df_A` (the df of the model A at the sample size `n`),
#' `F` (the F-test of `PRE` at the sample size `n`),
#' `p` (the p-value of F-test at the sample size `n`), and the post-hoc power analysis with
#' `lambda_post` (the non-centrality parameter of `F` at the sample size `n`),
#' and `power_post` (the post-hoc power at the sample size `n`).
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
                     PC = 1,
                     PA = 2,
                     sig_level = 0.05,
                     power = 0.8,
                     power_ul = 1,
                     n_ul = 1.45e+09,
                     n = NULL) {

  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n_ul <- as.integer(n_ul)
  n <- as.integer(n)

  stopifnot(
    sum(sapply(list(PRE, PC, PA, sig_level, power, power_ul, n_ul), is.null)) == 0,
    PRE > 0, # If PRE = 0, the computation would be endless.
    PRE < 1, # PRE would be subjected to the F-test, hence should be smaller than 1.
    PC >= 0,
    PC < PA,
    # power is only used to filter the minimum sample size, and it does not influence the loop.
    # Actually, power coule be set be negative.
    power >= 0,
    # power could be 1.
    # If PRE is very very small, n_i will grow towards Inf. Limit the maximum n_i to be 100,000.
    power <= 1,
    power_ul >= power,
    power_ul <= 1, # power_ul must <= 1, otherwise the loop will be endless because power_i cannot > 1.
    sig_level > 0, # if sig_level = 0, the computation would be endless.
    sig_level <= 1,
    identical(n, integer(0)) || n >= PA + 1, # Use ||, so if n is integer(0), (n > PA) would not be evaluated.
    n_ul >= PA + 1 # the loop begin from n_i = PA + 1
  )

  f_squared <- PRE / (1 - PRE)
  df_A_C <- PA - PC

  # post-hoc power
  if (!identical(n, integer(0))) {
    df_A <- n - PA
    F <- (PRE / df_A_C) / ((1 - PRE) / df_A)
    p <- stats::pf(F, df_A_C , df_A, lower.tail = FALSE)
    lambda_post <- f_squared * df_A
    power_post <- stats::pf(stats::qf((1 - sig_level), df_A_C , df_A),
                            df_A_C,
                            df_A,
                            lambda_post,
                            lower.tail = FALSE)
    F_test = c(n = n, df_A_C = df_A_C, df_A = df_A, F = F, p = p, lambda_post = lambda_post, power_post = power_post)
  }

  # a priori power
  n_i <- PA + 1
  power_i <- 0
  priori <- list()
  index <- 1

  # The maximum power_i is 1, and the maximum power_ul is 1.
  # Do not use "power_i <= power_ul" to keep the while alive,
  # when power_ul = 1, the loop will be endless.

  while ((power_i < power_ul) & (n_i <= n_ul)) {
    df_A_i <- n_i - PA
    F_i <- (PRE / df_A_C) / ((1 - PRE) / df_A_i)
    p_i <- stats::pf(F_i, df_A_C , df_A_i, lower.tail = FALSE)
    lambda_i <- f_squared * df_A_i
    power_i <- stats::pf(stats::qf((1 - sig_level), df_A_C , df_A_i),
                         df_A_C,
                         df_A_i,
                         lambda_i,
                         lower.tail = FALSE)
    priori[[index]] <- c(n_i, df_A_C, df_A_i, F_i, p_i, lambda_i, power_i)
    n_i <- n_i + 1
    index <- index + 1
  }

  priori <- data.frame(matrix(unlist(priori), ncol = 7, byrow = TRUE))
  names(priori) <- c("n_i", "df_A_C", "df_A_i", "F_i", "p_i", "lambda_i", "power_i")

  minimum <- priori[priori$power_i >= power, ][1, ]

  method <- "power_lm"

  if (identical(n, integer(0))) {
    out <- structure(
      list(
        PRE = PRE,
        f_squared = f_squared,
        PC = PC,
        PA = PA,
        sig_level = sig_level,
        power = power,
        power_ul = power_ul,
        n_ul = n_ul,
        minimum = minimum,
        priori = priori,
        method = method
      ),
      class = c("Keng_power", "list")
    )
  } else {
    out <- structure(
      list(
        PRE = PRE,
        f_squared = f_squared,
        PC = PC,
        PA = PA,
        sig_level = sig_level,
        n = n,
        F_test = F_test,
        power = power,
        power_ul = power_ul,
        n_ul = n_ul,
        minimum = minimum,
        priori = priori,
        method = method
      ),
      class = c("Keng_power", "list")
    )
  }
  out
}
