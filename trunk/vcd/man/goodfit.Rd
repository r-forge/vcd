\name{goodfit}
\alias{goodfit}
\alias{summary.goodfit}
\alias{plot.goodfit}
\title{Goodness-of-fit tests for discrete data}
\description{
Fits a discrete (count data) distribution for goodness-of-fit tests.
}
\usage{
goodfit(obj, type = c("poisson", "binomial", "nbinomial"),
        par = NULL, size = NULL)
}
\arguments{
  \item{obj}{either a vector of counts, a 1-way table of frequencies
    of counts or a data frame or matrix with frequencies
    in the first column and the corresponding counts in the
    second column.}
  \item{type}{a character string indicating which distribution should be fit.}
  \item{par}{a named list giving the distribution parameters (named as in the
    corresponding density function), if set to \code{NULL}, the default,
    the parameters are estimated.}
  \item{size}{the size argument for the binomial distribution, only
    required if \code{type} is \code{"binomial"}. If set to \code{NULL},
    \code{size} is taken to be the maximum count.}
}
\details{\code{goodfit} essentially computes the fitted values of a
discrete distribution (either poisson, binomial or negative binomial)
to the count data given in \code{obj}. If the parameters are not specified
they are estimated by maximum likelihood. Furthermore the parameters are
estimated by minimum Chisquare and method of moments for \code{"nbinomial"}
(MM is equivalent to ML for the other distributions).
\code{par} should a named list specifying the parameters \code{lambda}
for \code{"poisson"}, \code{prob} for \code{"binomial"} and \code{prob} and
\code{size} for \code{"nbinomial"}.

The corresponding Pearson Chisquare and likelihood
ratio statistic are also computed and given with their $p$ values by the
\code{summary} method. The
\code{plot} method produces a \code{link{rootogram}} of the
observed and fitted values.
}
\value{
  A list of class \code{"goodfit"} with elements:
  \item{observed}{observed frequencies,}
  \item{count}{corresponding counts,}
  \item{fitted}{expected frequencies (fitted by ML),}
  \item{type}{a character string indicating the distribution fitted,}
  \item{df}{degrees of freedom,}
  \item{estimate}{a matrix of estimates of the distribution parameters,}
  \item{statistic}{the corresponding Pearson Chisquare and likelihood ratio statistics.}
}
\author{
  Achim Zeileis
}
\references{
Michael Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Publishing.
}
\examples{
## Simulated data examples:
dummy <- rnbinom(200, size = 1.5, prob = 0.8)
gf <- goodfit(dummy, type = "nbinomial")
summary(gf)
plot(gf)

dummy <- rbinom(100, size = 6, prob = 0.5)
gf1 <- goodfit(dummy, type = "binomial", size = 6)
gf2 <- goodfit(dummy, type = "binomial", par = list(prob = 0.6), size = 6)
summary(gf1)
plot(gf1)
summary(gf2)
plot(gf2)

## Real data examples:
data(HorseKicks)
HK.fit <- goodfit(HorseKicks)
summary(HK.fit)
plot(HK.fit)

data(Federalist)
F.fit <- goodfit(Federalist, type = "nbinomial")
summary(F.fit)
plot(F.fit)
}
\keyword{category}
