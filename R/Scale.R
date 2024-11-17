#' Scale a vector
#'
#' @param x The original vector.
#' @param expected_M The expected Mean of the scaled vector.
#' @param expected_SD The expected Standard Deviation (unit) of the scaled vector.
#' @param oadvances The distance the Origin of x advances by.
#'
#' @return The scaled vector.
#'
#' @details
#' To scale `x`, its origin, or unit (*expected_SD*), or both, could be changed.
#'
#' If `expected_M` = 0 or `NULL`, and `expected_SD` = `NULL`, `x` would be mean-centered.
#'
#' If `expected_M` is a non-zero number, and `expected_SD` = `NULL`, the mean of `x` would be transformed to expected_M.
#'
#' If `expected_M` = 0 or `NULL`, and `expected_SD` = 1, `x` would be standardized to be its z-score with *M* = 0 and *SD* = 1.
#'
#' The standardized score is not necessarily the z-score. If neither `expected_M` nor `expected_SD` is `NULL`,
#' `x` would be standardized to be a vector whose mean and standard deviation would be `expected_M` and `expected_SD`, respectively.
#' To standardize `x`, the mean and standard deviation of `x` are needed and computed,
#' for which the missing values of `x` are removed if any.
#'
#' If `oadvances` is not `NULL`,  the origin of `x` will advance with the standard deviation being unchanged.
#' In this case, `Scale()` could be used to pick points in simple slope analysis for moderation models.
#' Note that when `oadvances` is not `NULL`, `expected_M` and `expected_SD` must be NULL.
#'
#' @export
#'
#' @examples
#' (x <- rnorm(10, 5, 2))
#' # Mean-center x.
#' Scale(x)
#' # Transform the mean of x to 3.
#' Scale(x, expected_M = 3)
#' # Transform x to its z-score.
#' Scale(x, expected_SD = 1)
#' # Standardize x with M = 100 and SD = 15.
#' Scale(x, expected_M = 100, expected_SD = 15)
#' # The origin of x advances by 3.
#' Scale(x, oadvances = 3)

Scale <- function(x, expected_M = NULL, expected_SD = NULL, oadvances = NULL) {

  stopifnot(is.numeric(x))

  if (!is.null(oadvances)) {
    stopifnot(is.numeric(x), is.null(expected_M), is.null(expected_SD))
    x - oadvances
  }
  else {
    if (is.null(expected_M)) expected_M <- 0
    else stopifnot(is.numeric(expected_M))
    if (is.null(expected_SD)) expected_SD <- stats::sd(x, na.rm = TRUE)
    else stopifnot(is.numeric(expected_SD))
    expected_M + (x - mean(x, na.rm = TRUE))/stats::sd(x, na.rm = FALSE)*expected_SD
  }
}
