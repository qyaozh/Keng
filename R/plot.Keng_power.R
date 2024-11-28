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
  labels <- switch(x$method,
                   power_r = paste0("r = ", x$r, ",\n",
                                    "sig.level = ", x$sig.level, ",\n",
                                    "Planned sample size = ", x$minimum$n_i),
                   power_lm = paste0("PRE = ", x$PRE, ",\n",
                                     "PC = ", x$PC, ",\n",
                                     "PA = ", x$PA, ",\n",
                                     "sig.level = ", x$sig.level, ",\n",
                                     "Planned sample size = ", x$minimum$n_i))
  plot_power_n <-
    graphics::plot(
      formula = power_i ~ n_i,
      data = x$prior,
      pch = 20,
      col = "#1661AB",
      main = "Power ~ Sample Size",
      xlab = "Sample Size n",
      ylim = c(0, 1),
      ylab = "Power"
    ) + graphics::abline(
    h = x$power,
    v = x$minimum$n_i,
    lwd = 2,
    lty = 3,
    col = "#FCC307"
  ) + graphics::text(
    x = x$minimum$n_i,
    y = 0.2,
    labels = labels,
    pos = 4,
    col = "#A7535A"
  )
}
