#' Print primary but not all contents of the Keng_power class
#'
#' @param x The output object of `power_r()` or `power_lm()`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return None (invisible NULL).
#' @export
#'
#' @examples print(power_lm(n = 200))
#'
#' out <- power_r(0.2, n = 193)
#'
#' out
print.Keng_power <- function(x, ...) {
  stopifnot("Keng_power" %in% class(x))

  switch(
    x$method,
    power_r =  cat("r = ", x$r, "\n",
                   "d = ", x$d, "\n",
                   "sig.level = ", x$sig.level, "\n",
                   "----- Post-hoc power analysis -----", "\n",
                   "n = ", ifelse(is.null(x$n), "NULL", x$n), "\n",
                   "power_post = ", ifelse(is.null(x$n), "NULL", format(x$t_test["power_post"])), "\n",
                   "------ Sample size planning -------", "\n",
                   "power = ", x$power, "\n",
                   "Minimum sample size = ", x$minimum$n_i, "\n",

                   sep = ""),
    power_lm = cat("PRE = ", x$PRE, "\n",
                   "f_squared = ", x$f_squared, "\n",
                   "PC = ", x$PC, "\n",
                   "PA = ", x$PA, "\n",
                   "sig.level = ", x$sig.level, "\n",
                   "----- Post-hoc power analysis -----", "\n",
                   "n = ", ifelse(is.null(x$n), "NULL", x$n), "\n",
                   "power_post = ", ifelse(is.null(x$n), "NULL", format(x$F_test["power_post"])), "\n",
                   "------ Sample size planning -------", "\n",
                   "Expected power = ", x$power, "\n",
                   "Minimum sample size = ", x$minimum$n_i,
                   sep = ""),
  )
}
