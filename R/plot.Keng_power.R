#' Plot the power against sample size for the Keng_power class
#'
#' @param x The output object of `power_r()` or `power_lm()`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A plot of power against sample size.
#' @export
#'
#' @examples plot(power_lm())
#'
#' out <- power_r(0.2, n = 193)
#'
#' plot(out)
plot.Keng_power <- function(x, ...) {
  stopifnot("Keng_power" %in% class(x))

  # select 200 rows
  if (nrow(x$prior) > 100) {
    x$prior <- x$prior[quantile(0:nrow(x$prior), seq(0, 1, 0.01)), ]
  }

  labels <- switch(x$method,
                   power_r = paste0("Note. ",
                                    "r = ", x$r, ", ",
                                    "sig.level = ", x$sig.level, ", ",
                                    "planned sample size = ", x$minimum$n_i, "."),
                   power_lm = paste0("Note. ",
                                     "PRE = ", x$PRE, ", ",
                                     "PC = ", x$PC, ", ", "PA = ", x$PA, ", ",
                                     "sig.level = ", x$sig.level, ", ",
                                     "planned sample size = ", x$minimum$n_i, "."))
  plot_power_n <-
    graphics::plot(
      formula = power_i ~ n_i,
      data = x$prior,
      pch = 20,
      col = "#1661AB",
      xlab = "Sample Size",
      ylim = c(0, 1),
      ylab = "Power"
    ) + graphics::abline(
    h = x$power,
    v = x$minimum$n_i,
    lwd = 2,
    lty = 3,
    col = "#FCC307"
  ) +
    title(
      main = "Power ~ Sample Size",
      sub = labels,
      col.sub = "#A7535A")
}
