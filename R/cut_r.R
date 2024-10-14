#' Cut-off values of *r* given sample size *n*.
#'
#' @param n Sample size of the *r*.
#'
#' @return Cut-off values of *r* at the significance levels of *p* = 0.1, 0.05, 0.01, 0.001.  *r* with the absolute value larger than the cut-off value is significant at the corresponding significance level.
#' @export
#'
#' @examples cut_r(193)
cut_r <- function(n) {
  p <- c(0.1, 0.05, 0.01, 0.001)
  t <- stats::qt(p = (1 - p/2), df = (n - 2))
  r <- t/sqrt(t^2 + n - 2)
  out <- data.frame(p, r)
  row.names(out) <- paste0("Result", row.names(out))
  return(out)
}
