\name{oddsratio}
\alias{oddsratio}
\alias{print.oddsratio}
\alias{summary.oddsratio}
\alias{confint.oddsratio}
\alias{print.summary.oddsratio}
\title{Odds Ratios}
\description{
  Computes (log) odds ratios and their asymptotic standard errors for
  (possibly) stratified data.
}
\usage{
oddsratio(x, stratum = NULL, log = TRUE)
}
\arguments{
  \item{x}{a 2 by 2 by \dots table.}
  \item{stratum}{vector of strata dimensions.}
  \item{log}{if \code{FALSE}, ordinary odds ratios are computed.}
}
\value{
  An object of class \code{"logoddsratio"}, which is simply a vector of
  (log) odds ratios with dimensionality depending on \code{stratum},
  along with the following attributes:
  \item{ASE}{a numeric vector with the asymptotic standard errors.}
  \item{log}{logical indicating whether log odds ratios or common odds
    ratios are computed.}
}
\note{
  The \code{summary} method prints the standard errors and---for log
  odds ratios---also computes and prints the standardized log odds
  ratios (which follow an asymptotic normal distribution) and the
  corresponding \eqn{p}-values.
  FIXME
  \code{confint.oddsratio} computes confidence intervals for the (log)
  odds ratios.
}

\references{
  M. Friendly (2000),
  \emph{Visualizing Categorical Data}.
  SAS Institute, Cary, NC.
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}

\seealso{
  \code{\link{plot.oddsratio}},
  \code{\link{confint}}
}
\examples{
data(CoalMiners)
summary(oddsratio(CoalMiners))
}
\keyword{category}
