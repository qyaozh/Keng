#' Pick Super Lotto numbers
#'
#' @param size An integer. The size of sets of Super Lotto numbers to pick.
#' @param verbose A logical value. Print the numbers picked or not.
#'
#' @returns Print the numbers picked, and return the invisible `balls` list that stored these numbers.
#' @export
#'
#' @examples pick_sl(10)
#' out <- pick_sl(10, verbose = FALSE)
#' out
pick_sl <- function(size = 1L, verbose = TRUE) {
  stopifnot(
    is.numeric(size) && size == round(size),
    is.logical(suppressWarnings(as.logical(verbose))) &&
      !is.na(suppressWarnings(as.logical(verbose)))
  )
  set.seed(NULL)
  i = 1
  balls <- list()
  for (i in seq(i, size, 1)) {
    balls[[i]] <- c(red = sort(sample.int(35, 5)),
                    blue = sort(sample.int(12, 2)))
    if (verbose)
      cat(balls[[i]][1:5], "|\033[32m", balls[[i]][6:7], "\033[0m\n")
    i = i + 1
  }
  invisible(balls)
}
