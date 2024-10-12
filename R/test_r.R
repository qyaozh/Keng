test_r <- function(r, n) {
  r = r
  se = sqrt((1 - r^2)/(n - 2))
  t = r/se
  p <- -2 * (pt(abs(t), (n - 2)) - 1)
  names(p) <- "p"
  return(p)
}
