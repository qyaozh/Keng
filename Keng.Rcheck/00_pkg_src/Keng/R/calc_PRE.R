#' Calculate PRE from Cohen's f, f_squared, or partial correlation
#'
#' @param f Cohen's f. Cohen (1988) suggested >=0.1, >=0.25, and >=0.40 as cut-off values of f for small,
#' medium, and large effect sizes, respectively.
#' @param f_squared Cohen's f_squared. Cohen (1988) suggested >=0.02, >=0.15, and >=0.35 as cut-off values of f for small,
#' medium, and large effect sizes, respectively.
#' @param r_p Partial correlation.
#'
#' @return A list including PRE, the absolute value of r_p (partial correlation), Cohen's f_squared, and f.
#'
#' @references Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Routledge.
#'
#' @export
#'
#' @examples calc_PRE(f = 0.1)
#' calc_PRE(f_squared = 0.02)
#' calc_PRE(r_p = 0.2)
calc_PRE <- function(f = NULL, f_squared = NULL, r_p = NULL) {

  stopifnot(
    sum(sapply(list(f, f_squared, r_p), is.null)) == 2,
    is.null(f) || f >= 0,
    is.null(f_squared) || f_squared >= 0,
    is.null(r_p) || (abs(r_p) >= 0 & abs(r_p) <= 1))

  # use || rather than |, then when r_p is NULL, abs() would not be evaluated.
  # if use |, abs() would always be evaluated and then give an Error.

  if (!is.null(r_p)) {
    PRE <- r_p^2 # r_p would not be influenced by the positive or negative sign
    f_squared <- PRE/(1 - PRE) # PRE must >=0 and < 1, hence f_squared > 0 and could be Inf
    f <- sqrt(f_squared)
  }
  else if (!is.null(f_squared)) {
    PRE <- f_squared/(1 + f_squared)
    r_p <- sqrt(PRE)
    f <- sqrt(f_squared)
  }
  else if ((!is.null(f))) {
    f_squared <- f^2
    PRE <- f_squared/(1 + f_squared)
    r_p <- sqrt(PRE)
  }

  list(PRE = PRE, "abs(r_p)" = r_p, f_squared = f_squared, f = f)
}
