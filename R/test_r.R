#' Test r using the t-test given r and n.
#'
#' @param r Pearson correlation.
#' @param n Sample size of *r*.
#' @details To test the significance of the *r* using one-sample *t*-test, the *SE* of the r is determined by the following formula: `SE = sqrt((1 - r^2)/(n - 2))`.
#'
#' @return A data.frame including *r*, *se* of *r*, *t*, and *p*.
#' @export
#'
#' @examples test_r(0.2, 193)
test_r <- function(r, n) {
  r = r
  se = sqrt((1 - r^2)/(n - 2))
  t = r/se
  p <- -2 * (stats::pt(abs(t), (n - 2)) - 1)
  out <- data.frame(r, se, t, p)
  row.names(out) <- paste0("Row_", row.names(out))
  return(out)
}
