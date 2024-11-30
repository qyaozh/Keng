#' Conduct post-hoc and prior power analysis, and plan the sample size for regression analysis
#'
#' @param PRE Proportional Reduction in Error.
#' PRE = The square of partial correlation.
#' Cohen (1988) suggested >=0.02, >=0.13, and >=0.26 as cut-off values of PRE for small,
#' medium, and large effect sizes, respectively.
#' @param PC Number of parameters of model C (compact model) without focal predictors of interest.
#' @param PA Number of parameters of model A (augmented model) with focal predictors of interest.
#' @param sig.level Expected significance level for effects of focal predictors.
#' @param power Expected statistical power for effects of focal predictors.
#' @param n The current sample size.#'
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
#' If sample size `n` is given, the following results would also be returned:
#'
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
                     n = NULL) {

  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n <- as.integer(n)

  stopifnot(
    sum(sapply(
      list(PRE, PC, PA, power, sig.level), is.null
    )) == 0,
    PRE > 0,
    PRE < 1,
    PC >= 0,
    PA >= 1,
    PC < PA,
    power > 0,
    power <= 1,
    sig.level > 0,
    sig.level <= 1,
    identical(n, integer(0)) | n > PA
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

  while (power_i < 1) {
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
}
