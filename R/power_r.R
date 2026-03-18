#' Conduct a priori power analysis, and plan the sample size for r.
#'
#' @param r Pearson's correlation. Cohen(1988) suggested >=0.1, >=0.3, and >=0.5 as cut-off values
#' of Pearson's correlation r for small, medium, and large effect sizes, respectively.
#' @param sig_level Expected significance level.
#' @param power Expected statistical power.
#' @param n_ul The upper limit of sample size below which the minimum required sample size is searched.
#' Non-integer `n_ul` would be converted to be an integer using `as.integer()`.
#' `n_ul` should be at least 3.
#'
#' @details `Power_r()` follows Aberson (2019) approach to conduct power analysis.
#' `n_ul` determine the maximum sample size below which power_r() attempts searching
#' for the minimum required sample size, hence the number of rows of the returned
#' power table `priori` and the right limit of the horizontal axis of the returned power plot.
#' `power_r()` will keep searching for the sample size that pushes the power level to `power`.
#' When `r` is very small and power is larger than 0.8, a huge increase of
#' sample size only brings about a trivial increase in power,
#' which is cost-ineffective. To make `power_r()` omit unnecessary attempts,
#' you could set `n_ul` to be a value less than 1.45e+09 (e.g., 10000).
#'
#' @return A Keng_power class, also a list. If `n` is not given, the following results would be returned:
#' `[[1]]` `r`, the given r;
#' `[[2]]` `d`, Cohen's d derived from `r`;  Cohen (1988) suggested >=0.2, >=0.5, and >=0.8
#' as cut-off values of `d` for small, medium, and large effect sizes, respectively;
#' `[[3]]` `sig_level`, the expected significance level;
#' `[[4]]` `power`, the expected power;
#' `[[5]]` `n_ul`, the upper limit of sample size;
#' `[[6]]` `minimum`, the minimum planned sample size `n_i` and corresponding
#' `df_i` (the `df` of t-test at the sample size `n_i`, `df_i` = `n_i` - 2),
#' `SE_i` (the SE of `r` at the sample size `n_i`),
#' `t_i` (the t-test of `r`),
#' `p_i` (the p-value of `t_i`),
#' `delta_i` (the non-centrality parameter of the t-distribution for the alternative hypothesis, given `r` and `n_i`),
#' `power_i` (the actual power of `r` at the sample size `n_i`);
#' `[[7]]` `priori`, a priori power table with increasing sample sizes (`n_i`) and power(`power_i`).
#' `[[8]]`  A plot of power against sample size n.
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
                    n_ul = 1.45e+09L) {
  n_ul <- as.integer(n_ul)

  stopifnot(
    r > -1,
    r < 1,
    r != 0,
    power >= 0,
    power <= 1,
    sig_level > 0,
    sig_level <= 1,
    n_ul >= 3L
  )

  d <- 2 * r / sqrt(1 - r^2) #approximate estimate of d

  # a priori power
  root <- tryCatch({
    # code may return error
    stats::uniroot(
      function(n) powered_r(r = r, n = n, sig_level = sig_level)$power - power,
      interval = c(3, n_ul),
      tol = .Machine$double.eps
    )$root
  }, error = function(e) {
    # deal with error
    message(
      paste0(
             "The minimum required sample size does not exist within the range of 3-",
             n_ul,"."))
    # minimum <- n_ul
    return(NA)
  })

  # minimal sample size
  minimum <- ceiling(root)

  # power plot data
  if (is.na(root)) {
    n_i <- seq(3L, n_ul)
  } else {
    n_i <- seq(3L, minimum)
  }
  priori <- powered_r(r = r, n = n_i, sig_level = sig_level)
  priori <- data.frame(priori)
  names(priori) <- c("n_i", "df_i", "SE_i", "t_i", "p_i", "delta_i", "power_i")

  method <- "power_r"

  out <- structure(
    list(
      r = r,
      d = d,
      sig_level = sig_level,
      power = power,
      n_ul = n_ul,
      minimum = minimum,
      priori = priori,
      method = method
    ),
    class = c("Keng_power", "list")
  )

  out
}
