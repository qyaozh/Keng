#' Pick Double Color Balls
#'
#' @param size The size of sets of Super Lotto numbers to pick.
#' @param verbose A logical value. Print the numbers picked or not.
#'
#' @returns Print the numbers picked, and return the invisible `balls` list that stored these numbers.
#' @export
#'
#' @examples pick_dcb(10)
#' out <- pick_dcb(10, verbose = FALSE)
#' out
pick_dcb <- function(size = 1L, verbose = TRUE) {
  stopifnot(
    is.numeric(size) && size == round(size),
    is.logical(suppressWarnings(as.logical(verbose))) &&
      !is.na(suppressWarnings(as.logical(verbose)))
  )
  set.seed(NULL)
  balls <- list()
  for (i in 1:size) {
    balls[[i]] <- c(red = sort(sample.int(33, 6)),
                    blue = sort(sample.int(16, 1)))
    if (verbose)
      cat(
        "\033[31m",
        paste(balls[[i]][1:6], collapse = ", "),
        "\033[0m&\033[34m",
        balls[[i]][7],
        "\033[0m\n"
      )
  }
  invisible(balls)
}
