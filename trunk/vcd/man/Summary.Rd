\name{Summary}
\alias{Summary}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Summary of a m x n-table}
\description{
Prints a m x n-contingency table along with percentages, marginal and conditional distributions.
}
\usage{
Summary(x, totals = TRUE, percentages = TRUE,
        conditionals = c("row", "column", "none"), digits = 4)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a m x n-contingency table}
  \item{totals}{if \code{TRUE}, marginal distributions are computed.}
  \item{percentages}{if \code{TRUE}, relative frequencies are computed.}
  \item{conditionals}{if not \code{"none"}, the conditioinal
    distributions, given the row/column factor, are computed.}
  \item{digits}{a non-null value for \code{digits} specifies the number of
          significant digits to be printed in values.  If \code{digits} is
          \code{NULL}, the value of \code{digits} set by \code{options} is used.}
}
\value{
Returns invisibly a m x n x o - table, o depending on the amount of
choices (max 3).
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{\code{\link{mar.table}}, \code{\link{prop.table}}}

\examples{
data(UCBAdmissions)
Summary(margin.table(UCBAdmissions, c(1,2)))
}
\keyword{category}% at least one, from doc/KEYWORDS
