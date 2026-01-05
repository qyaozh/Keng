#' Divide a vector into three groups
#'
#' @param x A numeric vector.
#' @param criterion A character value `"1sd"` or a numeric value between 0 and 0.5.
#'
#' @details `divide()` divides a vector into three groups using the criterion of 1 SD,
#' or proportions like 0.27. Differing from `cut()`, the medium interval of `divide()`
#' does not include the left and right limits.
#'
#' @returns An ordered factor with levels "Low < Medium < High".
#' @export
#'
#' @examples x <- 1:100
#' divide(x)
#' divide(x, 0.27)
divide <- function(x, criterion = "1sd") {
  stopifnot(is.numeric(x))
  if (criterion == "1sd") {
    ll <- mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE)
    ul <- mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)
  }
  else if (criterion > 0 && criterion < 0.5) {
    ll <- quantile(x, criterion)
    ul <- quantile(x, 1 - criterion)
  }
  c <- as.character(x)
  c[x <= ll] <- "Low"
  c[x >= ul] <- "High"
  c[x > ll & x < ul] <- "Medium"
  ordered(c, levels = c("Low", "Medium", "High"))
}
