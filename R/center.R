#' Mean-center a vector
#'
#' @param x A vector.
#'
#' @returns A vector centered on x's mean that is computed with NA values stripped.
#'
#' @export
#'
#' @examples
#' x <- rnorm(193, 100, 15)
#' center(x)

center <- function(x){
  x - mean(x, na.rm = TRUE)
  }
