\name{confint}
\alias{confint}
\alias{confint.oddsratio}
\alias{confint.Kappa}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Confidence Intervals}
\description{Computes confidence intervals}
\usage{
confint(object, parm, level = 0.95, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{object}{a fitted model object. In \code{vcd}, methods currently
    exist for the classes \code{Kappa} and \code{oddsratio}}
  \item{parm}{currently not used.}
  \item{level}{the confidence level required.}
  \item{\dots}{currently not used.}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for each parameter. These will be labelled as
  (1-level)/2 and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\references{
Michael Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}

\examples{
data(SexualFun)
confint(Kappa(SexualFun))

data(CoalMiners)
confint(oddsratio(CoalMiners))
}
\keyword{category}% at least one, from doc/KEYWORDS
