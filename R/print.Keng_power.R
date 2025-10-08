#' Print primary but not all contents of the Keng_power class
#'
#' @param x The output object of `power_r()` or `power_lm()`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return None (invisible NULL).
#' @export
#'
#' @examples power_lm()
#' power_lm(n = 200)
#' print(power_lm(n = 200))
#' x <- power_r(0.2, n = 193)
#' x
print.Keng_power <- function(x, ...) {

  stopifnot("Keng_power" %in% class(x))

  switch(
    x$method,
    power_r =  cat("-- Given ---------------------------", "\n",
                   "r = ", x$r, "\n",
                   "d = ", x$d, "\n",
                   "sig_level = ", x$sig_level, "\n",
                   "-- Sample size planning -----------", "\n",
                   "power = ", x$power, "\n",
                   "Minimum sample size = ", x$minimum$n_i, "\n",
                   if (is.na(x$minimum$n_i))
                     paste0("Note. Sample size has reached ", x$n_ul, ", but power failed to reach ",  x$power, "."),
                   sep = ""),
    power_lm = cat("-- Given ---------------------------", "\n",
                   "PRE = ", x$PRE, "\n",
                   "f_squared = ", x$f_squared, "\n",
                   "PC = ", x$PC, "\n",
                   "PA = ", x$PA, "\n",
                   "sig_level = ", x$sig_level, "\n",
                   "-- Sample size planning -----------", "\n",
                   "Expected power = ", x$power, "\n",
                   "Minimum sample size = ", x$minimum$n_i, "\n",
                   if (is.na(x$minimum$n_i))
                       paste0("Note. Sample size has reached ", x$n_ul, ", but power failed to reach ",  x$power, "."),
                   sep = "")
  )
}
