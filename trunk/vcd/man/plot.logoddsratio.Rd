\name{plot.logoddsratio}
\alias{plot.logoddsratio}
\title{Log Odds Ratios Plots}
\description{
Plots log odds ratios, computed by \code{logoddsratio} for 2 x 2 x
k-tables, along with confidence intervals.
}
\usage{
plot.logoddsratio(x, quantile = qnorm(0.975), confidence = TRUE,
                  type ="o", ylab = "Log Odds Ratio", xlab = "Strata",
                  whiskers = 0.1, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{logoddsratio}, computed for a 2 x 2
    x k-table.}
  \item{quantile}{(one-sided) quantile used for the confidence intervals.}
  \item{confidence}{if \code{TRUE}, confidence intervals are plotted for
  each data point.}
  \item{type}{plot type.}
  \item{ylab}{label for the y-axis.}
  \item{xlab}{label for the x-axis.}
  \item{whiskers}{Width of the confidence interval whiskers.}
  \item{\dots}{Other graphics parameters (see \code{par}).}
}
\references{Friendly, Michael (2000): Visualizing Categorical Data, SAS
  Publishing.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}

\seealso{\code{\link{logoddsratio}}}

\examples{
data(CoalMiners)
plot(logoddsratio(CoalMiners),
     xlab = "Age Group",
     ylab = "Breathelessness and Wheeze in Coal Miners")
}
\keyword{hplot}% at least one, from doc/KEYWORDS
