\name{assocstats}
\alias{assocstats}
\alias{summary.assocstats}
\alias{print.assocstats}
\alias{print.summary.assocstats}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Association Statistics}
\description{
  Computes the Chi-Square test, the Likelihood Ratio Chi-Square test,
  the phi-coefficient, the the contingency coefficient and Cramer's V.
}
\usage{
assocstats(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a r x c-table.}
}
\value{
  A list with components:
  \item{chisq.tests}{a 2x3 table with the chisquare statistics.}
  \item{phi}{The phi-coefficient.}
  \item{cont}{The  contingency coefficient.}
  \item{cramer}{Cramer's V.}
}
\references{
Michael Friendly (2000), \emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}
\examples{
data(Arthritis)
tab <- xtabs(~Improved + Treatment, data = Arthritis)
summary(assocstats(tab))
}
\keyword{category}% at least one, from doc/KEYWORDS
