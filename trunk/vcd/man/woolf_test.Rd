\name{woolf_test}
\alias{woolf_test}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Woolf test}
\description{
  Test for homogeneity on \eqn{2 \times 2 \times k}{2 times 2 times k}
  tables over strata (i.e., if the log odds ratios are the same in all strata).
}
\usage{
woolf_test(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{A \eqn{2 \times 2 \times k}{2 times 2 times k} table.}
}
\value{
     A list of class \code{"htest"} containing the following components: 
\item{statistic}{the chi-squared test statistic.}
\item{parameter}{degrees of freedom of the approximate chi-squared
          distribution of the test statistic.}
\item{p.value}{\eqn{p}-value for the test.}
\item{method}{a character string indicating the type of test performed.}
\item{data.name}{a character string giving the name(s) of the data.}
\item{observed}{the observed counts.}
\item{expected}{the expected counts under the null hypothesis.}
}

\seealso{\code{\link{mantelhaen.test}}}

\examples{
data(CoalMiners)
woolf_test(CoalMiners)
}
\keyword{htest}% at least one, from doc/KEYWORDS

