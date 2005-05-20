\name{pearson_test}
\alias{pearson_test}
\title{Conditional Independence Test}
\description{
  Performs a test of independence on a 2-dimensional contingency table by
  simulating the Pearson residuals under the hypotheses of independence,
  conditional the marginals of the input table.
}
\usage{
pearson_test(x, n = 1000, FUN = function(x) max(abs(x)),
             alternative = c("greater","less"), conf_level = 0.95,
             return_distribution = TRUE)
}
\arguments{
  \item{x}{a two-way contingency table.}
  \item{n}{number of independence tables to be drawn.}
  \item{FUN}{test statistic to be applied to all tables.}
  \item{alternative}{a character string specifying the alternative
    hypothesis; must be either \code{"greater"} (default) or
    \code{"less"} (and may be abbreviated.)}
  \item{conf_level}{currently not used.}
  \item{return_distribution}{if \code{TRUE}, the simulated distribution
    of \code{FUN} under the null is returned.}
}
\details{
  The function uses \code{\link{r2dtable}} to simulate the distribution
  under the null.
}
\value{
  A list of class \code{"htest"} with following components: 
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the \eqn{p}-value for the test.}
  \item{estimate}{the estimated value of \code{FUN(x)}.}
  \item{method}{a character string indicating the type of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed table.}
  \item{expected}{the independence table.}
  \item{residuals}{the Pearson residuals (standardized deviations of
    observed and expected table).}
  \item{dist}{if \code{return_distribution} is \code{TRUE}, a vector of
    size \code{n} with simulated values of the distribution of
    \code{FUN} under the null.}
}
\references{
  FIXME
  (strasser)
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}
\seealso{
  \code{\link{chisq.test}},
  \code{\link{fisher.test}},
  \code{\link{r2dtable}}
}

\examples{
TeaTasting <- matrix(c(3, 1, 1, 3), nr = 2,
                     dimnames = list(Guess = c("Milk", "Tea"),
                                     Truth = c("Milk", "Tea"))
)
pearson_test(TeaTasting)
pearson_test(TeaTasting, FUN = function(x) sum(x * x))
chisq.test(TeaTasting)
}
\keyword{htest}
