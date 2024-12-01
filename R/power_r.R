#' Conduct post-hoc and prior power analysis, and plan the sample size for r.
#'
#' @param r Pearson's correlation. Cohen(1988) suggested >=0.1, >=0.3, and >=0.5 as cut-off values
#' of Pearson's correlation r for small, medium, and large effect sizes, respectively.
#' @param sig_level Expected significance level.
#' @param power Expected statistical power.
#' @param n The current sample size. Non-integer `n` would be converted to be a integer using `as.integer()`.
#' `n` should be at least 3.
#' @param power_ul The upper limit of power. `power_ul` should be larger than `power`, and the maximum `power_ul` is 1.
#' @param n_ul The upper limit of sample size below which the minimum required sample size is searched.
#' Non-integer `n_ul` would be converted to be a integer using `as.integer()`.
#' `n_ul` should be at least 3.
#'
#' @details `Power_r()` follows Aberson (2019) approach to conduct power analysis. `power_ul` and `n_ul` determine the total times of power_r()'s attempts searching for the minimum required sample size,
#' hence the number of rows of the returned power table `prior` and the right limit of the horizontal axis of the returned power plot.
#' `power_r()` will keep running and gradually raise the sample size to `n_ul` until the sample size pushes the power level to `power_ul`.
#' When `r` is very small and power is larger than 0.8, a huge increase of sample size only bring about a trivial increase of power,
#' which is cost-ineffective. To make `power_r()` omit unnecessary attempts, you could set `power_ul` to be a value less than 1 (e.g., 0.90),
#' and/or set `n_ul` to be a value less than 1.45e+09 (e.g., 10000).
#'
#' @return A Keng_power class, also a list. If `n` is not given, the following results would be returned:
#' `[[1]]` `r`, the given r;
#' `[[2]]` `d`, Cohen's d derived from `r`;  Cohen (1988) suggested >=0.2, >=0.5, and >=0.8
#' as cut-off values of `d` for small, medium, and large effect sizes, respectively;
#' `[[3]]` `sig_level`, the expected significance level;
#' `[[4]]` `power`, the expected power;
#' `[[5]]` `power_ul`, The upper limit of power;
#' `[[6]]` `n_ul`, the upper limit of sample size;
#' `[[7]]` `minimum`, the minimum planned sample size `n_i` and corresponding
#' `df_i` (the `df` of t-test at the sample size `n_i`, `df_i` = `n_i` - 2),
#' `SE_i` (the SE of `r` at the sample size `n_i`),
#' `t_i` (the t-test of `r`),
#' `p_i` (the p-value of `t_i`),
#' `delta_i` (the non-centrality parameter of the t-distribution for the alternative hypothesis, given `r` and `n_i`),
#' `power_i` (the actual power of `r` at the sample size `n_i`);
#' `[[8]]` `prior`, a prior power table with increasing sample sizes (`n_i`) and power(`power_i`).
#' `[[9]]`  A plot of power against sample size n.
#'
#' If sample size `n` is given, the following results would also be returned:
#' Integer `n`, the t_test of `r` at the sample size `n` with
#' `df`, `SE` of `r`, `p` (the p-value of t-test), and the post-hoc power analysis with
#' `delta_post` (the non-centrality parameter of the t-distribution for the alternative hypothesis),
#' and `power_post` (the post-hoc power of `r` at the sample size `n`).
#'
#' By default, `print()` prints the primary but not all contents of the `Keng_power` class.
#' To inspect more contents, use `print.AsIs()` or list extracting.
#'
#' @references Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples power_r(0.2)
#' print(power_r(0.04))
#' plot(power_r(0.04))

power_r <- function(r = 0.2,
                    sig_level = 0.05,
                    power = 0.80,
                    power_ul = 1,
                    n_ul = 1.45e+09,
                    n = NULL) {
  n <- as.integer(n)
  n_ul <- as.integer(n_ul)

  stopifnot(r > -1,
            r < 1,
            r != 0,
            power >= 0,
            power <= 1,
            power_ul >= power,
            power_ul <= 1,
            sig_level > 0,
            sig_level <= 1,
            identical(n, integer(0)) || n >= 3,
            n_ul >= 3)

  d <- 2 * r / sqrt(1 - r ^ 2) #approximate estimate of d

  #post-hoc power
  if (!identical(n, integer(0))) {
    df <- n - 2
    SE <- sqrt((1 - r ^ 2) / df)
    t <- r / SE
    p <- stats::pt(abs(t), df, lower.tail = FALSE) * 2
    delta_post <- d * sqrt(df) / 2
    power_post <- stats::pt(stats::qt(1 - sig_level / 2, df),
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

  while (power_i < power_ul & n_i <= n_ul) {
    df_i <- n_i - 2
    SE_i <- sqrt((1 - r ^ 2) / df_i)
    t_i <- r / SE_i
    p_i <- stats::pt(abs(t_i), df_i, lower.tail = FALSE) * 2
    delta_i <- d * sqrt(df_i) / 2
    power_i <- stats::pt(stats::qt(1 - sig_level / 2, n_i - 2),
                         n_i - 2,
                         abs(delta_i),
                         lower.tail = FALSE)
    prior[[index]] <- c(n_i, df_i, SE_i, t_i, p_i, delta_i, power_i)
    n_i <- n_i + 1
    index <- index + 1
  }

  prior <- data.frame(matrix(unlist(prior), ncol = 7, byrow = TRUE))
  names(prior) <- c("n_i", "df_i", "SE_i", "t_i", "p_i", "delta_i", "power_i")

  minimum <- prior[prior$power_i >= power, ][1, ]

  method <- "power_r"

  if (identical(n, integer(0))) {
    out <- structure(
      list(
        r = r,
        d = d,
        sig_level = sig_level,
        power = power,
        power_ul = power_ul,
        n_ul = n_ul,
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
        sig_level = sig_level,
        n = n,
        t_test = t_test,
        power = power,
        power_ul = power_ul,
        n_ul = n_ul,
        minimum = minimum,
        prior = prior,
        method = method
      ),
      class = c("Keng_power", "list")
    )
  }
  out
}

