\name{plot.oddsratio}
\alias{plot.oddsratio}
\title{(Log) Odds Ratios Plots}
\description{
  Plots (log) odds ratios, computed by \code{oddsratio} for 2 x 2 x
  \eqn{k}-tables, along with confidence intervals.
}
\usage{
\method{plot}{oddsratio}(x, conf_level = 0.95,
                  type ="o", ylab = NULL, xlab = "Strata",
                  whiskers = 0.1, ...)
}
\arguments{
  \item{x}{an object of class \code{logoddsratio}, computed for a 2 x 2
    x \eqn{k}-table.}
  \item{conf_level}{if not \code{NULL} or \code{FALSE},
    \code{conf_level}-\% confidence intervals are plotted for each data point.}
  \item{type}{plot type.}
  \item{ylab}{label for the y-axis.}
  \item{xlab}{label for the x-axis.}
  \item{whiskers}{width of the confidence interval whiskers.}
  \item{\dots}{other graphics parameters (see \code{par}).}
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
  \code{\link{oddsratio}}
}
\examples{
data(CoalMiners)
l <- oddsratio(CoalMiners)
g <- seq(25, 60, by = 5)
plot(l,
     xlab = "Age Group",
     main = "Breathelessness and Wheeze in Coal Miners")
m <- lm(l ~ g + I(g^2))
lines(fitted(m), col = "red")
}

\keyword{hplot}
