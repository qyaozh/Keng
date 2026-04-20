#' Transform CIE xyY c(x, y, Y) to matrix(c(X, Y, Z))
#'
#' @param x CIE coordinate. An atomic numeric value or a numeric vector.
#' @param y CIE coordinate. An atomic numeric value or a numeric vector.
#' @param Y CIE coordinate. An atomic numeric value or a numeric vector.
#'
#' @returns A 3-column matrix whose rows specify the colors, and cols specify the XYZs.
#' @export
#'
#' @examples XYZ <- xyY2XYZ(0.555, 0.328, 9.4)
#' RGB <- convertColor(XYZ, "XYZ", "sRGB")
#' rgb(RGB[,1], RGB[,2], RGB[,3])

xyY2XYZ <- function(x, y, Y) {
  # check args
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(Y))
  stopifnot(length(x) == length(y), length(y) == length(Y))
  stopifnot(all(x >= 0 & x <= 1))
  stopifnot(all(y >= 0 & y <= 1))
  stopifnot(all(Y >= 0 & Y <= 100))

  # compute
  n <- length(x)
  Y_norm <- Y / 100

  X <- Y_norm * x / y
  Y <- Y_norm
  Z <- Y_norm * (1 - x - y) / y

  # matrix
  XYZ <- matrix(
    c(X, Y, Z),
    nrow = n,
    ncol = 3,
    byrow = FALSE,
    dimnames = list(NULL, c("X", "Y", "Z"))
  )

  return(XYZ)
}
