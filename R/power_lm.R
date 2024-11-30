#' Conduct post-hoc and prior power analysis, and plan the sample size for regression analysis
#'
#' @param PRE Proportional Reduction in Error.
#' PRE = The square of partial correlation.
#' Cohen (1988) suggested >=0.02, >=0.13, and >=0.26 as cut-off values of PRE for small,
#' medium, and large effect sizes, respectively.
#' @param PC Number of parameters of model C (compact model) without focal predictors of interest.
#' Non-integer `PC` would be converted to be a integer using `as.integer()`.
#' @param PA Number of parameters of model A (augmented model) with focal predictors of interest.
#' Non-integer `PA` would be converted to be a integer using `as.integer()`.
#' `as.integer(PA)` should be larger than `as.integer(PC)`.
#' @param sig.level Expected significance level for effects of focal predictors.
#' @param power Expected statistical power for effects of focal predictors.
#' @param power.ul The upper limit of power. `power.ul` should be larger than `power`, and the maximum `power.ul` is 1.
#' `power.ul` determines the number of rows of the returned power table `prior`,
#' and the right limit of the horizontal axis of the returned power plot.
#' `power_lm` will keep running and gradually raise the sample size until the sample size reaches the power level `power.ul`.
#' When PRE is very small (e.g., less than 0.001) and power is larger than 0.8,
#' a huge increase of sample size only bring about a trivial increase of power, which is cost-ineffective.
#' You could set `power.ul` to be a value less than 1 (e.g., 0.90) to make `power_lm` omit unnecessary attempts.
#' @param n The current sample size. Non-integer `n` would be converted to be a integer using `as.integer()`.
#'
#' @return A Keng_power class, also a list. If sample size `n` is not given, the following results would be returned:
#' `[[1]]` `PRE`;
#' `[[2]]` `f_squared`, Cohen's f_squared derived from PRE;
#' `[[3]]` `PC`;
#' `[[4]]` `PA`;
#' `[[5]]` `sig.level`, expected significance level for effects of focal predictors;
#' `[[6]]` `power`, expected statistical power for effects of focal predictors;
#' `[[6]]` `minimum`, the minimum sample size `n_i` required for focal predictors to reach the
#' expected statistical power and significance level, and corresponding
#' `df_A_C`(the df of the numerator of the F-test, i.e., the difference of the dfs between model C and model A),
#' `df_A_i`(the df of the denominator of the F-test, i.e., the df of the model A at the sample size `n_i`),
#' `F_i`(the *F*-test of `PRE` at the sample size `n_i`),
#' `p_i`(the p-value of `F_i`),
#' `lambda_i`(the non-centrality parameter of `F_i` at the sample size `n_i`),
#' `power_i`(the actual power at the sample size `n_i`),
#' `[[7]]` `prior`, a prior power table including `n_i` and corresponding `df_A_C`, `df_A_i`, `F_i`, `p_i`, `lambda_i`, `power_i`.
#' `_i` indicates these statistics are the intermediate iterative results.
#'
#' Note that the maximum `n_i` is limited to be 100,000.
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
#' To inspect more contents, use list extracting.
#'
#' @references Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples print(power_lm())
#'
#' plot(power_lm())
power_lm <- function(PRE = 0.02,
                     PC = 1,
                     PA = 2,
                     sig.level = 0.05,
                     power = 0.8,
                     power.ul = 0.9,
                     n = NULL) {

  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n <- as.integer(n)

  stopifnot(
    sum(sapply(
      list(PRE, PC, PA, power, sig.level), is.null
    )) == 0,
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
    power.ul >= power,
    power.ul <= 1, # power.ul must <= 1, otherwise the loop will be endless because power cannot > 1.
    sig.level > 0, # if sig.level = 0, the computation would be endless.
    sig.level <= 1,
    identical(n, integer(0)) || n > PA # Use ||, so if n is integer(0), (n > PA) would not be evaluated.
  )

  f_squared <- PRE / (1 - PRE)
  df_A_C <- PA - PC

  # post-hoc power
  if (!identical(n, integer(0))) {
    df_A <- n - PA
    F <- (PRE / df_A_C) / ((1 - PRE) / df_A)
    p <- stats::pf(F, df_A_C , df_A, lower.tail = FALSE)
    lambda_post <- f_squared * df_A
    power_post <- stats::pf(stats::qf((1 - sig.level), df_A_C , df_A),
                            df_A_C,
                            df_A,
                            lambda_post,
                            lower.tail = FALSE)
    F_test = c(n = n, df_A_C = df_A_C, df_A = df_A, F = F, p = p, lambda_post = lambda_post, power_post = power_post)
  }

  # prior power
  n_i <- PA + 1
  power_i <- 0
  prior <- list()
  index <- 1

  while (power_i <= power.ul & n_i <= 100000) {
    df_A_i <- n_i - PA
    F_i <- (PRE / df_A_C) / ((1 - PRE) / df_A_i)
    p_i <- stats::pf(F_i, df_A_C , df_A_i, lower.tail = FALSE)
    lambda_i <- f_squared * df_A_i
    power_i <- stats::pf(stats::qf((1 - sig.level), df_A_C , df_A_i),
                         df_A_C,
                         df_A_i,
                         lambda_i,
                         lower.tail = FALSE)
    prior[[index]] <- c(n_i, df_A_C, df_A_i, F_i, p_i, lambda_i, power_i)
    n_i <- n_i + 1
    index <- index + 1
  }

  prior <- data.frame(matrix(unlist(prior), ncol = 7, byrow = TRUE))
  names(prior) <- c("n_i", "df_A_C", "df_A_i", "F_i", "p_i", "lambda_i", "power_i")

  minimum <- prior[prior$power_i >= power, ][1, ]

  method <- "power_lm"

  if (identical(n, integer(0))) {
    out <- structure(
      list(
        PRE = PRE,
        f_squared = f_squared,
        PC = PC,
        PA = PA,
        sig.level = sig.level,
        power = power,
        minimum = minimum,
        prior = prior,
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
        sig.level = sig.level,
        n = n,
        F_test = F_test,
        power = power,
        minimum = minimum,
        prior = prior,
        method = method
      ),
      class = c("Keng_power", "list")
    )
  }
  out
}
