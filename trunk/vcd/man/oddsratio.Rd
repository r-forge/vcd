\name{oddsratio}
\alias{oddsratio}
\alias{print.oddsratio}
\alias{summary.oddsratio}
\alias{print.summary.oddsratio}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Odds Ratios}
\description{
  Computes (log) odds ratios and their asymtotic standard errors
  for (possibly) stratified data.
}
\usage{
oddsratio(x, stratum = NULL, log = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a 2 x 2 x ... table.}
 \item{stratum}{vector of strata dimensions.}
 \item{log}{if \code{FALSE}, ordinary odds ratios are computed}
}
\value{
  An object of class \code{logoddratio}, which is simply a
  vector of (log) odds ratios with dimensionality depending on \code{stratum}, along with
  the following attributes:
  \item{ASE}{a vector with the asymptotic standard errors}
  \item{log}{logical value indicating if log odds ratios or common odds
    ratios are computed.}
}
\note{The \code{summary} method prints the standard errors, and---for
  log odds ratios---also computes and prints the standardized
  log odds ratios, which follows an asymptotic normal distribution, and
  the correpsonding P values. \code{confint.oddsratio} computes
  confidence intervals for the (log) odds ratios.
}

\references{
M. Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}

\seealso{\code{\link{plot.oddsratio}}, \code{\link{confint}}}

\examples{
data(CoalMiners)
summary(oddsratio(CoalMiners))
}
\keyword{category}% __ONLY ONE__ keyword per line
