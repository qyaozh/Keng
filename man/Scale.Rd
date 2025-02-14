% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scale.R
\name{Scale}
\alias{Scale}
\title{Scale a vector}
\usage{
Scale(x, m = NULL, sd = NULL, oadvances = NULL)
}
\arguments{
\item{x}{The original vector.}

\item{m}{The expected Mean of the scaled vector.}

\item{sd}{The expected Standard Deviation (unit) of the scaled vector.}

\item{oadvances}{The distance the Origin of x advances by.}
}
\value{
The scaled vector.
}
\description{
Scale a vector
}
\details{
To scale \code{x}, its origin, or unit (sd), or both, could be changed.

If \code{m} = 0 or \code{NULL}, and \code{sd} = \code{NULL}, \code{x} would be mean-centered.

If \code{m} is a non-zero number, and \code{sd} = \code{NULL}, the mean of \code{x} would be transformed to m.

If \code{m} = 0 or \code{NULL}, and \code{sd} = 1, \code{x} would be standardized to be its z-score with m = 0 and m = 1.

The standardized score is not necessarily the z-score. If neither \code{m} nor \code{sd} is \code{NULL},
\code{x} would be standardized to be a vector whose mean and standard deviation would be \code{m} and \code{sd}, respectively.
To standardize \code{x}, the mean and standard deviation of \code{x} are needed and computed,
for which the missing values of \code{x} are removed if any.

If \code{oadvances} is not \code{NULL},  the origin of \code{x} will advance with the standard deviation being unchanged.
In this case, \code{Scale()} could be used to pick points in simple slope analysis for moderation models.
Note that when \code{oadvances} is not \code{NULL}, \code{m} and \code{sd} must be NULL.
}
\examples{
(x <- rnorm(10, 5, 2))
# Mean-center x.
Scale(x)
# Transform the mean of x to 3.
Scale(x, m = 3)
# Transform x to its z-score.
Scale(x, sd = 1)
# Standardize x with m = 100 and sd = 15.
Scale(x, m = 100, sd = 15)
# The origin of x advances by 3.
Scale(x, oadvances = 3)
}
