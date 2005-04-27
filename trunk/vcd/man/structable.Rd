\name{structable}
\alias{structable.default}
\alias{structable.formula}
\alias{structable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{structable}
\description{
This function produces a \sQuote{flat} representation of a high-dimensional
contingency table constructed in a similar way than mosaic displays.
}
\usage{
\method{structable}{formula}(formula, data, direction = 'h', ...,
subset, na.action)
\method{structable}{default}(..., split_vertical = FALSE, direction = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula object with possibly both left and right hand sides
          specifying the column and row variables of the flat table.}
  \item{data}{a data frame, list or environment containing the variables to
          be cross-tabulated, or a contingency table.}
  \item{subset}{an optional vector specifying a subset of observations to be
          used. Ignored if 'data' is a contingency table}
  \item{na.action}{a function which indicates what should happen when the data
          contain \code{NA}s. Ignored if \code{data} is a contingency table}
  \item{\dots}{R objects which can be interpreted as factors (including
          character strings), or a list (or data frame) whose
          components can be so interpreted, or a contingency table
          object of class \code{"table"} or \code{"ftable"}.}
  \item{split_vertical}{logical vector indicating, for each dimension,
    whether it should be splitted vertically or not. Values are recycled
    as needed. If the argument is of length 1, the value is alternated for all
    dimensions.}
  \item{direction}{character vector alternatively specifying the
    splitting direction (\code{"h"} for horizontal and \code{"v"} for
    vertical splits). Values are recycled
    as needed. If the argument is of length 1, the value is alternated for all
    dimensions.}
}
\details{
  This function produces textual representations of mosaic displays, and
  thus \sQuote{flat} contingency tables. The formula interface is quite
  similar to the one of \code{\link{ftable}}, but also accepts the
  \code{\link{mosaic}}-like formula interface (empty left-hand
  side). Note that even if the \code{\link{ftable}} interface is used,
  the \code{split_vertical} or \code{direction} argument is needed to
  specify the \emph{order} of the splits.
}
\value{
An object inheriting from class \code{"ftable"} with the
splitting information (\code{"split_vertical"}) as additional attribute.
}
\author{David Meyer (\email{David.Meyer@R-project.org})}

\seealso{
  \code{\link{strucplot}},
  \code{\link{mosaic}}
  \code{\link[stats]{ftable}}
}

\examples{
  structable(Titanic)
  structable(Titanic, split_vertical = c(TRUE, TRUE, FALSE, FALSE))
  structable(Titanic, direction = c("h","h","v","v"))
  structable(Sex + Class ~ Survived + Age, data = Titanic)
}
\keyword{hplot}% at least one, from doc/KEYWORDS

