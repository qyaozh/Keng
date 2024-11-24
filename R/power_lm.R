#' Post-hoc and/or prior power of regression analysis
#'
#' @param PRE Proportional Reduction in Error.
#' *PRE* = The square of partial correlation.
#' Cohen (1988) suggested >=0.02, >=0.13, and >=0.26 as cut-off values of *PRE* for small,
#' medium, and large effect sizes, respectively.
#' @param PC Number of parameters of model C (compact model) without focal predictors of interest.
#' @param PA Number of parameters of model A (augmented model) with focal predictors of interest.
#' @param power Expected statistical power for effects of focal predictors.
#' @param sig.level Expected significance level for effects of focal predictors.
#' @param n The current sample size. If *n* is given, the post-hoc power would be computed.
#'
#' @return A list with 4 items: (1) `post`, the post-hoc *F* -test, `lambda` (non-centrality parameter),
#' and `power` for sample size `n`;
#' (2)`minimum`, the minimum sample size required for focal predictors to reach the
#' expected statistical power and significance level;
#' (3) `prior`, a data.frame including `n_i`, `PC`, `PA`,`df_A_i`, `F_i`, `p_i`, `lambda_i`, `power_i`.
#' `_i` indicates these statistics are the intermediate iterative results.
#' Each row of `prior` presents results for one possible sample size `n_i`.
#' Given `n_i`, `df_A_i`, `F_i`, `p_i`, `lambda_i` and `power_i` would be computed accordingly.
#' (4) A plot of power against sample size *n*.
#' The cut-off value of *n* for expected statistical power `power` and
#' expected significance level `sig.level` is annotated on the plot.
#'
#' @references Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples power_lm()
power_lm <- function(PRE = 0.02, PC = 0, PA = 1, power = 0.8, sig.level = 0.05, n = NULL){
  stopifnot(
    sum(sapply(list(PRE, PC, PA, power, sig.level), is.null)) == 0,
    PC >= 0,
    PA >= 1,
    PC < PA,
    PA < n,
    power > 0,
    power <= 1,
    sig.level > 0,
    sig.level <= 1
    )

  PC <- as.integer(PC)
  PA <- as.integer(PA)
  n <- as.integer(n)

  f_squared <- PRE/(1 - PRE)
  df_A_C <- PA - PC

  if (!is.null(n)) {
    df_A <- n - PA
    F <- (PRE/df_A_C)/((1 - PRE)/df_A)
    p <- stats::pf(F, df_A_C , df_A, lower.tail = FALSE)
    lambda <- f_squared*df_A
    power_post <- stats::pf(stats::qf((1 - sig.level), df_A_C , df_A), df_A_C, df_A, lambda, lower.tail = FALSE)
  }
  else {
    power_post <- NA
  }

  n_i <- PA + 1
  power_i <- 0
  prior <- list()
  index <- 1

  while (power_i < 1) {
    df_A_i <- n_i - PA
    F_i <- (PRE/df_A_C)/((1 - PRE)/df_A_i)
    p_i <- stats::pf(F_i, df_A_C , df_A_i, lower.tail = FALSE)
    lambda_i <- f_squared*df_A_i
    power_i <- stats::pf(stats::qf((1 - sig.level), df_A_C , df_A_i), df_A_C, df_A_i, lambda_i, lower.tail = FALSE)
    prior[[index]] <- c(n_i, PC, PA, df_A_i, F_i, p_i, lambda_i, power_i)
    n_i <- n_i + 1
    index <- index + 1
  }

  prior <- data.frame(matrix(unlist(prior), ncol = 8,  byrow = TRUE))
  names(prior) <- c("n_i", "PC", "PA", "df_A_i", "F_i", "p_i", "lambda_i", "power_i")

  minimum <- prior[prior$power_i >= power,][1,]

  plot_power_n <- graphics::plot(
    power_i ~ n_i,
    data = prior,
    pch = 21,
    col = "transparent",
    bg = "#1661AB",
    main = "Power ~ Sample Size",
    xlab = "Sample Size n",
    ylim = c(0, 1),
    ylab = "Power"
  ) +
    graphics::abline(
      h = power,
      v = minimum$n_i,
      lwd = 2,
      lty = 3,
      col = "#005541"
    ) +
    graphics::text(
      x = minimum$n_i,
      y = 0.01,
      paste(minimum$n_i, "= Planned Sample Size"),
      pos = 4,
      col = "#005541"
    )

  if (!is.null(n)) {
    list(
      post = c(PRE = PRE, f_squared = f_squared, n = n, PC = PC, PA = PA, df_A = df_A, F = F, p = p, lambda = lambda, power_post = power_post),
      minimum = minimum,
      prior = prior,
      plot_power_n = plot_power_n)
  }
  else {
    list(post = c(PRE = PRE, f_squared = f_squared, n = NA, PC = PC, PA = PA, df_A = NA, F = NA, p = NA, lambda = NA, power_post = NA),
    minimum = minimum,
    prior = prior,
    plot_power_n = plot_power_n)
  }
}
