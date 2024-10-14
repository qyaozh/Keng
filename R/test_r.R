#' Test *r* using t-test given *r* and *n*.
#'
#' @param r Pearson correlation.
#' @param n sample size of *r*.
#'
#' @return *r*, *se* of *r*, *t*, and *p*.
#' @export
#'
#' @examples test_r(0.2, 193)
test_r <- function(r, n) {
  r = r
  se = sqrt((1 - r^2)/(n - 2))
  t = r/se
  p <- -2 * (stats::pt(abs(t), (n - 2)) - 1)
  out <- data.frame(r, se, t, p)
  row.names(out) <- paste0("Result", row.names(out))
  return(out)
}
