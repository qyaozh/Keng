#' Pick Super Lotto numbers
#'
#' @param size An integer. The size of sets of Super Lotto numbers to pick.
#' @param verbose A logical value. Print the numbers picked or not.
#'
#' @returns Print the numbers picked, and return the invisible `balls` list that stored these numbers.
#' @export
#'
#' @examples # Example 1
#' pick_sl(10)
#' # Example 2
#' out <- pick_sl(10, verbose = FALSE)
#' out
#' # Example 3
#' # create an empty list
#' balls <- list(c(front = rep(NA, 5),
#'                 back = rep(NA, 2))
#' )
#  specify the lucky numbers
#' luck <- list(c(front = c(10L, 13L, 14L, 19L, 27L),
#'                back = c(6L, 10L)))
#' # limit the max number of draws
#' max <- 9999
#' # try
#' # count the number of draws
#' i <- 0
#' while (!identical(balls, luck, max)) {
#'   i = i + 1
#'   balls = pick_sl(verbose = FALSE)
#'   if (identical(balls, luck)) {
#'     print(i)
#'     print(balls)
#'   }
#'   else
#'     if (i == max) {
#'       cat(i, "failed\n")
#'       break}
#' }
pick_sl <- function(size = 1L, verbose = TRUE) {
  stopifnot(
    is.numeric(size) && size == round(size),
    is.logical(suppressWarnings(as.logical(verbose))) &&
      !is.na(suppressWarnings(as.logical(verbose)))
  )
  set.seed(NULL)
  balls <- list()
  for (i in 1:size) {
    balls[[i]] <- c(front = sort(sample.int(35, 5)),
                    back = sort(sample.int(12, 2)))
    if (verbose)
      cat(
        paste(balls[[i]][1:5], collapse = ", "),
        "&\033[32m",
        paste(balls[[i]][6:7], collapse = ", "),
        "\033[0m\n"
      )
  }
  invisible(balls)
}

