\name{SpaceShuttle}
\alias{SpaceShuttle}
\non_function{}
\title{SpaceShuttle}
\description{
Data set about NASA space shuttle O-ring failures.
}

\usage{data(SpaceShuttle)}
\format{A data frame with 24 observations and 6 variables.
  \describe{
    \item{FlightNumber}{Number of space shuttle flight,}
    \item{Temperature}{temperature during start (in degrees F),}
    \item{Pressure}{pressure,}
    \item{Fail}{did any O-ring failures occur? (no, yes),}
    \item{nFailures}{how many (of six) 0-rings faild?,}
    \item{Damage}{damage index.}
  }
}

\references{
Dalal et al. (1989), \emph{JASA}, \bold{84}, 945-957.
}

\source{Friendly, Visualizing Categorical Data:
\url{http://hotspur.psych.yorku.ca/ftp/sas/vcd/catdata/orings.sas}}

\examples{
data(SpaceShuttle)
plot(nFailures/6 ~ Temperature, xlim = c(30, 81), data = SpaceShuttle,
     ylim = c(0,1), main = "NASA Space Shuttle O-Ring Failures",
     ylab = "Estimated failure probability", pch = 19, col = 4)
fm <- glm(cbind(nFailures, 6 - nFailures) ~ Temperature, data = SpaceShuttle,
          family = binomial)
lines(30:81, predict(fm, data.frame(Temperature = 30:81), type = "re"), lwd = 2)
abline(v = 31, lty = 3)
}

\keyword{datasets}
