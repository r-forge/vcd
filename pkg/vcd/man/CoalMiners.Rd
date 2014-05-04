\name{CoalMiners}
\alias{CoalMiners}
\docType{data}
\title{Breathlessness and Wheeze in Coal Miners}
\description{
  Data from Ashford & Sowden (1970) given by Agresti (1990) on the
  association between two pulmonary conditions, breathlessness and
  wheeze, in a large sample of coal miners.
  This data is frequently used as an example of fitting models for
  bivariate, binary responses.
}
\usage{
data("CoalMiners")
}
\format{
  A 3-dimensional array resulting from cross-tabulating variables for
  16,330 coal miners.  The variables and their levels are as follows:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1 \tab Wheeze \tab W, NoW \cr
    2 \tab Breathlessness \tab B, NoB \cr
    3 \tab Age \tab 25-29, 30-34, \dots, 60-64
  }
}

\details{
In Ashford & Sowden (1970) and Agresti (1990), there is also a ninth group, aged 20-24,
inadvertently omitted from this data table.
}

\references{
  A. Agresti (1990),
  \emph{Categorical Data Analysis}.
  Wiley-Interscience, New York, Table 7.11

  J. R. Ashford and R. D. Sowdon (1970),
  Multivariate probit analysis,
  \emph{Biometrics}, \bold{26}, 535--546.

  M. Friendly (2000),
  \emph{Visualizing Categorical Data}.
  SAS Institute, Cary, NC.
}

\source{
  Michael Friendly (2000),
  Visualizing Categorical Data, pages 82--83, 319--322.
}

\examples{
data("CoalMiners")

## Fourfold display, both margins equated
fourfold(CoalMiners, mfcol = c(2,4))

## Log Odds Ratio Plot
summary(l <- oddsratio(CoalMiners))
age <- seq(25, 60, by = 5)
plot(l,
     xlab = "Age Group",
     main = "Breathlessness and Wheeze in Coal Miners")
m <- lm(l ~ age + I(age^2))
lines(fitted(m), col = "red")

## Fourfold display, strata equated
fourfold(CoalMiners, std = "ind.max", mfcol = c(2,4))

## add back the deleted age group (could use abind::abind() here)
CoalMiners1 <- array(0, dim=c(2,2,9))
CoalMiners1[,,1] <- matrix( c(9, 95, 7, 1841), 2, 2)
CoalMiners1[,,2:9] <- CoalMiners
dn <- dimnames(CoalMiners)
dn[[3]] <- c("20-24", dn[[3]])
dimnames(CoalMiners1) <- dn
class(CoalMiners1) <- "table"

}

\keyword{datasets}
