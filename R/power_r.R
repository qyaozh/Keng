#' Conduct post-hoc and prior power analysis, and plan the sample size for r.
#'
#' @param r Pearson's correlation.
#' @param sig.level Expected significance level.
#' @param power Expected statistical power.
#' @param n The current sample size.
#'
#' @details `Power_r()` follows Aberson (2019) approach to conduct power analysis.
#'
#' @return A Keng_power class, also a list. If `n` is not given, the following results would be returned:
#' `[[1]]` `r`, the given *r*;
#' `[[2]]` `d`, Cohen's *d* derived from `r`;  Cohen (1988) suggested >=0.2, >=0.5, and >=0.8
#' as cut-off values of `d` for small, medium, and large effect sizes, respectively;
#' `[[3]]` `power`, the expected power;
#' `[[4]]` `minimum`, the minimum planned sample size `n_i` and corresponding
#' `df_i` (the `df` of *t*-test at the sample size `n_i`, `df_i` = `n_i` - 2),
#' `SE_i` (the *SE* of `r` at the sample size `n_i`),
#' `t_i` (the *t*-test of `r`),
#' `p_i` (the *p*-value of `t_i`),
#' `delta_i` (the non-centrality parameter of *t*-test),
#' `power_i` (the actual power at the sample size `n_i`);
#' `[[5]]` `prior`, a power table with increasing sample sizes and power;
#' `[[6]]`  A plot of power against sample size *n*.
#'
#' If sample size `n` is given, the following results would also be returned:
#'
#' Integer `n`, the t_test of `r` at the sample size `n` with
#' `df_A_C`,
#' `df_A` (the *df* of the model A at the sample size `n`),
#' `F` (the *F*-test of `PRE` at the sample size `n`),
#' `p` (the p-value of *F*-test at the sample size `n`), and the post-hoc power analysis with
#' `lambda_post` (the non-centrality parameter of `F` at the sample size `n`),
#' and `power_post` (the post-hoc power at the sample size `n`).
#'
#' By default, `print()` prints the primary but not all contents of the `Keng_power` class.
#' To inspect more contents, use list extracting.
#'
#' @references Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples print(power_r(0.04))
#'
#' plot(power_r(0.04))

power_r <- function(r = 0.2,
                    sig.level = 0.05,
                    power = 0.80,
                    n = NULL) {
  n <- as.integer(n)

  stopifnot(r > -1,
            r < 1,
            r != 0,
            power > 0,
            power <= 1,
            sig.level > 0,
            sig.level <= 1,
            identical(n, integer(0)) | n > 2)

  d <- 2 * r / sqrt(1 - r ^ 2) #approximate estimate of d

  #post-hoc power
  if (!identical(n, integer(0))) {
    df <- n - 2
    SE <- sqrt((1 - r ^ 2) / df)
    t <- r / SE
    p <- stats::pt(abs(t), df, lower.tail = FALSE) * 2
    delta_post <- d * sqrt(df) / 2
    power_post <- stats::pt(stats::qt(1 - sig.level / 2, df),
                         df,
                         abs(delta_post),
                         lower.tail = FALSE)
    t_test <- c(n = n, df = df, SE = SE, t = t, p = p, delta_post = delta_post, power_post = power_post)
  }


  # prior power
  n_i <- 3
  power_i <- 0
  prior <- list()
  index <- 1

  while (power_i < 1) {
    df_i <- n_i - 2
    SE_i <- sqrt((1 - r ^ 2) / df_i)
    t_i <- r / SE_i
    p_i <- stats::pt(abs(t_i), df_i, lower.tail = FALSE) * 2
    delta_i <- d * sqrt(df_i) / 2
    power_i <- stats::pt(stats::qt(1 - sig.level / 2, n_i - 2),
                         n_i - 2,
                         abs(delta_i),
                         lower.tail = FALSE)
    prior[[index]] <- c(n_i, df_i, SE_i, t_i, p_i, delta_i, power_i)
    n_i <- n_i + 1
    index <- index + 1
  }

  prior <- data.frame(matrix(unlist(prior), ncol = 7, byrow = TRUE))
  names(prior) <- c("n_i", "df_i", "SE_i", "t_i", "p_r_i", "delta_i", "power_i")

  minimum <- prior[prior$power_i >= power, ][1, ]

  method <- "power_r"

  if (identical(n, integer(0))) {
    out <- structure(
      list(
        r = r,
        d = d,
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
        r = r,
        d = d,
        sig.level = sig.level,
        n = n,
        t_test = t_test,
        power = power,
        minimum = minimum,
        prior = prior,
        method = method
      ),
      class = c("Keng_power", "list")
    )
  }
}

