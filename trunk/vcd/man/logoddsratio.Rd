\name{logoddsratio}
\alias{logoddsratio}
\alias{print.logoddsratio}
\alias{summary.logoddsratio}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Log Odds Ratios}
\description{
  Computes (log) odds ratios and their asymtotic standard errors
  for (possibly) stratified data.
}
\usage{
logoddsratio(x, stratum = NULL, log = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a 2 x 2 x ... table.}
 \item{stratum}{vector of strata dimensions.}
  \item{log}{if \code{FALSE}, ordinary odds ratios are computed (the
    standard errors are still computed for log odds ratios!)}
}
\value{
  An object of class \code{logoddratio}, which is simply a
  vector of (log) odds ratios with dimensionality depending on \code{stratum}, along with an
  attribute \code{ASE} containing a similar object with the asymptotic
  standard errors.
}
\note{The \code{summary} method prints the standard errors.}
\references{Friendly, Michael (2000): Visualizing Categorical Data, SAS
  Publishing.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}

\seealso{\code{\link{plot.logoddsratio}}}

\examples{
data(CoalMiners)
summary(logoddsratio(CoalMiners))
}
\keyword{category}% __ONLY ONE__ keyword per line
