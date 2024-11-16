#' Move the origin of x vector forward by `coord` distance.
#'
#' @param x The original vector.
#' @param coord Move origin of x vector forward by `coord` distance.
#' If not specified, `coord` is set be the mean of x by default.
#'
#' @returns A vector whose coordinate centered on x's mean, which is computed with NA values stripped.
#'
#' @export
#'
#' @details
#' O in the `move_O` is capital.
#' The basic use of `move_O` is to mean-center a vector, with the argument `coord` set to be NULL by default.
#' If `coord` is not NULL, `move_O` will move origin of x by coord distance; in this case,
#' `move_O` could be used to pick points in simple-slope analysis for moderation models.
#'
#' @examples
#' x <- rnorm(193, 100, 15)
#' mean(x)
#' head(move_O(x)) - head(x)
#'
#' x <- rnorm(193, 4, 1)
#' w <- rnorm(193, 3, 2)
#' y <- 0.2*x + 0.2*w + 0.2*x*w + rnorm(193)
#' dat <- data.frame(y, x, w)
#' fit <- lm(y ~ x*w, dat)
#' coefs <- coef(fit)
#' # simple slope of x at w = 3
#' coefs[2] + coefs[4]*3
#' w_3 <- move_O(w, 3)
#' dat$w_3 <- w_3
#' fit <- lm(y ~ x*w_3, dat)
#' coef(fit)[2]
#' fit <- lm(y ~ x*I(move_O(w, 3)), dat)
#' coef(fit)[2]

move_O <- function(x, coord = NULL){
  if (is.null(coord)) {
    stopifnot(is.numeric(x))
    coord = mean(x, na.rm = TRUE)
  }
  else {
    stopifnot(is.numeric(x), is.numeric(coord))
  }
  x - coord
  }
