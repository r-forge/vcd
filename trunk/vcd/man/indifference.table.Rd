\name{indifference.table}
\alias{indifference.table}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Indifference Table}
\description{
  Computes table of expected frequencies (under the null hypotheses of
  independence) from a n-way table.
}
\usage{
indifference.table(x, frequency = c("absolute", "relative"))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a table.}
  \item{frequency}{indicates, if absolute or relative frequencies should
  be computed.}
}
\value{
  A table with either absolute or relative frequencies.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}
\examples{
data(MSPatients)
indifference.table(MSPatients)
indifference.table(MSPatients, frequency = "relative")
}
\keyword{category}% at least one, from doc/KEYWORDS
\keyword{array}% __ONLY ONE__ keyword per line
