#' Scale a vector
#'
#' @param x The original vector.
#' @param m The expected Mean of the scaled vector.
#' @param sd The expected Standard Deviation (unit) of the scaled vector.
#'
#' @return The scaled vector.
#'
#' @details
#' To scale `x`, its mean, or standard deviation (unit), or both, could be changed.
#' The missing values of `x` are removed if any.
#'
#' If `m` = 0 (default), and `sd` = `NULL`(default), `x` would be mean-centered (default).
#'
#' If `m` = `NULL`, the mean of `x` would not changed.
#' If `m` is not `NULL`, the mean of `x` would be transformed to `sd`.
#'
#' If `m` = `NULL` or 0, and `sd` = 1, `x` would be standardized to be its z-score with m = 0 and sd = 1.
#' The mean and standardized score are not necessarily 0 and 1.
#'
#' @export
#'
#' @examples
#' (x <- rnorm(10, 5, 2))
#' # Mean-center x.
#' Scale(x)
#' Scale(x, m = 0)
#' # Transform the mean of x to 3.
#' Scale(x, m = 3)
#' # Transform x to its z-score.
#' Scale(x, sd = 1)
#' # Standardize x with m = 100 and sd = 15.
#' Scale(x, m = 100, sd = 15)

Scale <- function(x, m = 0, sd = NULL) {
  stopifnot(is.numeric(x))
  if (is.null(m))
    m <- mean(x, na.rm = TRUE)
  else
    stopifnot(is.numeric(m))
  if (is.null(sd))
    sd <- stats::sd(x, na.rm = TRUE)
  else
    stopifnot(is.numeric(sd))
  m + (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE) * sd
}
