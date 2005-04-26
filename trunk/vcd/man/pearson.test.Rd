\name{pearson_test}
\alias{pearson_test}

\title{Conditional independence test}
\description{
  Performs a test of independence on a 2d-contigency table by simulating
  the Pearson residuals under the hypotheses of independence,
  conditional the marginals of the input table.
}
\usage{
pearson_test(x, n = 1000, FUN = function(x) max(abs(x)), alternative =
c("greater","less"), conf_level = 0.95, return_distribution = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{a 2d-contingency table}
  \item{n}{Number of independence tables to be drawn}
  \item{FUN}{Test statistic to be applied to all tables}
  \item{alternative}{a character string specifying the alternative hypothesis,
    must be one \code{"greater"} (default) or \code{"less"}.
    You can specify just the initial letter.}
  \item{conf_level}{currently not used}
  \item{return_distribution}{If \code{TRUE}, the simulated distribution
    of \code{FUN} under the null is returned}
}
\details{
  The function uses \code{r2dtable} to simulate the distribution under
  the null.
}
\value{
A list with class `"htest"' containing the following components: 

\item{statistic}{the value of the test statistic.}

\item{p.value}{the p-value for the test.}

\item{estimate}{the estimated value of \code{FUN(x)}}

\item{method}{a character string indicating the type of the test}

\item{data.name}{a character string giving the name(s) of the data}

\item{observed}{the observed table}
\item{expected}{the independence table}
\item{residuals}{the Pearson residuals (standardized deviations of
  observed and expected table)}
\item{dist}{if \code{return_distribution} is \code{TRUE}, a vector of
  size \code{n} with simulated values of the distributiion of \code{FUN}
under the null.}
}
\references{(strasser)}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{code{\link{chisq.test}}, code{\link{fisher.test}}}

\examples{
TeaTasting <- matrix(c(3, 1, 1, 3), nr = 2, dimnames = list(Guess = c("Milk", 
    "Tea"), Truth = c("Milk", "Tea")))
pearson_test(TeaTasting)
pearson_test(TeaTasting, FUN = function(x) sum(x * x))
chisq.test(TeaTasting)
}
\keyword{htest}% at least one, from doc/KEYWORDS
