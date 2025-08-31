
#' Assess course objective achievement
#'
#' @param data A wide-format data.frame that only contains student's grades of each session.
#' `assess_coa` assumes the highest score for each session and the final grade is 100,
#' given that weights are assigned by users.
#' @param session_weights A vector that Weights sessions for the final grade.
#' The length of `session_weights` is the number of sessions. Typically,
#' the final grade is based on the regular grade and final exam grade,
#' in which condition the length of `session_weights` is 2. The range of each weight should be 0-1.
#' The sum of `session_weights` should be 1.
#' @param objective_weights1 A vector that Weights course objectives for session 1.
#' The length of objective_weights1 is the number of course objectives.
#' The range of each weight should be 0-1. The sum of `objective_weights1` should be 1.
#' @param ... objective_weights2, objective_weights3, ...
#' Other vectors that Weight course objectives for session1, session2, ...
#' The number of objective_weights* arguments should be equal to the length of `session_weights`.
#'
#' @returns A data.frame containing grades of each session, final grades, and achievements of each
#' objective. This data.frame also has an attribute named "weights" that contains a list of
#' session_weights, objective_weights_matrix, and weighted_objective_weights_matrix
#' @export
#'
#' @examples data <- data.frame(session1 = 60 + sample.int(40, 100, 1), session2 = 60 + sample.int(40, 100, 1))
#' session_weights <- c(0.4, 0.6)
#' objective_weights1 <- c(0.5, 0.3, 0.2)
#' objective_weights2 <- c(0.1, 0.3, 0.6)
#' coa <- assess_coa(data, session_weights, objective_weights1, objective_weights2)
#' head(coa)
#' attr(coa, "weights")
#' colMeans(coa[row.names(attr(coa, "weights")[[2]])])

assess_coa <- function(data,
                       session_weights,
                       objective_weights1,
                       ...) {
  # check args
  if (!is.data.frame(data))
    stop("data is not a data.frame")
  if (!is.vector(session_weights))
    stop("session_weights is not vector")
  if (length(session_weights) !=  ncol(data))
    stop(
      "The length of session_weights is not equal to the number of columns in data"
    )
  if (length(session_weights) !=  (...length() + 1))
    stop(
      "The length of session_weights is not equal to the number of objective_weights* arguments"
    )
  if (any(length(objective_weights1) != length(...)))
    stop("The lengths of all objective_weights arguments are not identical")
  if (abs(sum(session_weights) - 1) > 1e-8)
    stop("The sum of session_weightss is not 1")
  if (abs(sum(objective_weights1) - 1) > 1e-8)
    stop("The sum of objective_weights1 is not 1")
  if (any(abs(colSums(cbind(...)) - 1) > 1e-8))
    stop("Not all the sum of objective_weights arguments are 1")
  if (any(c(session_weights, objective_weights1, ...) < 0))
    stop("Not all weights are equal to or larger than 0")
  if (any(c(session_weights, objective_weights1, ...) > 1))
    stop("Not all weights are equal to or smaller than 1")

  # objective_weights_matrix
  objective_weights_matrix <- matrix(
    c(objective_weights1, ...),
    ncol = length(session_weights),
    dimnames = (list(
      paste0("objective", 1:length(objective_weights1)),
      paste0("session", 1:length(session_weights))
    ))
  )

  # weighted weight
  weighted_objective_weights_matrix <- t(apply(
    objective_weights_matrix,
    1,
    FUN = function(x)
      x * session_weights
  ))

  if (abs(sum(weighted_objective_weights_matrix) - 1) > 1e-8)
    stop("the sum of weighted weight_objective is not 1")

  # objective achievement
  oa <- apply(
    weighted_objective_weights_matrix %*% t(as.matrix(data)),
    2,
    FUN = function(x)
      x / (rowSums(weighted_objective_weights_matrix) * 100)
  )

  # cbind
  coa <- cbind(
    data,
    final = as.matrix(data) %*% matrix(session_weights, ncol = 1)[, 1],
    t(oa)
  )
  attr(coa, "weights") <- list(session_weights = session_weights,
                               objective_weights_matrix = objective_weights_matrix,
                               weighted_objective_weights_matrix = weighted_objective_weights_matrix)
  coa
}
