#' Scale a vector
#'
#' @param x The original vector.
#' @param m The expected Mean of the scaled vector.
#' @param sd The expected Standard Deviation (unit) of the scaled vector.
#' @param oadvances The distance the Origin of x advances by.
#'
#' @return The scaled vector.
#'
#' @details
#' To scale `x`, its origin, or unit (sd), or both, could be changed.
#'
#' If `m` = 0 or `NULL`, and `sd` = `NULL`, `x` would be mean-centered.
#'
#' If `m` is a non-zero number, and `sd` = `NULL`, the mean of `x` would be transformed to m.
#'
#' If `m` = 0 or `NULL`, and `sd` = 1, `x` would be standardized to be its z-score with m = 0 and m = 1.
#'
#' The standardized score is not necessarily the z-score. If neither `m` nor `sd` is `NULL`,
#' `x` would be standardized to be a vector whose mean and standard deviation would be `m` and `sd`, respectively.
#' To standardize `x`, the mean and standard deviation of `x` are needed and computed,
#' for which the missing values of `x` are removed if any.
#'
#' If `oadvances` is not `NULL`,  the origin of `x` will advance with the standard deviation being unchanged.
#' In this case, `Scale()` could be used to pick points in simple slope analysis for moderation models.
#' Note that when `oadvances` is not `NULL`, `m` and `sd` must be NULL.
#'
#' @export
#'
#' @examples
#' (x <- rnorm(10, 5, 2))
#' # Mean-center x.
#' Scale(x)
#' # Transform the mean of x to 3.
#' Scale(x, m = 3)
#' # Transform x to its z-score.
#' Scale(x, sd = 1)
#' # Standardize x with m = 100 and sd = 15.
#' Scale(x, m = 100, sd = 15)
#' # The origin of x advances by 3.
#' Scale(x, oadvances = 3)

Scale <- function(x, m = NULL, sd = NULL, oadvances = NULL) {

  stopifnot(is.numeric(x))

  if (!is.null(oadvances)) {
    stopifnot(is.numeric(x), is.null(m), is.null(sd))
    x - oadvances
  }
  else {
    if (is.null(m)) m <- 0
    else stopifnot(is.numeric(m))
    if (is.null(sd)) sd <- stats::sd(x, na.rm = TRUE)
    else stopifnot(is.numeric(sd))
    m + (x - mean(x, na.rm = TRUE))/stats::sd(x, na.rm = FALSE)*sd
  }
}
