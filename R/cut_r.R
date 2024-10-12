cut_r <- function(n) {
  p <- c(0.1, 0.05, 0.01, 0.001)
  t <- qt(p = (1 - p/2), df = (n - 2))
  r <- t/sqrt(t^2 + n - 2)
  out <- data.frame(p, r)
  return(out)
}
