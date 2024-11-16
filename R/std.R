#' Standardize a vector
#'
#' @param x The original vector.
#' @param M The Mean of the standardized vector.
#' @param SD The Standard Deviation of the standardized vector.
#'
#' @return The standardized vector.
#'
#' @details
#' The standardized score is not necessarily the z-score.
#' The *M* and *SD* of the standardized vector should be set according to your need.
#' For example, the *M* and *SD* of IQ are 100 and 15, respectively.
#' To standardization x, the *M* and *SD* of x are needed and computed,
#' for which the missing values of x are removed if any.
#'
#' @export
#'
#' @examples
#' x <- rnorm(193)
#' IQ <- std(x, 100, 15)
std <- function(x, M = 0, SD = 1){
  stopifnot(is.numeric(x), is.numeric(M), is.numeric(SD), SD > 0)
  M + (x - mean(x, na.rm = TRUE))/stats::sd(x, na.rm = FALSE)*SD
}
