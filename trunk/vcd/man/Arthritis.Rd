\name{Arthritis}
\alias{Arthritis}
\non_function{}
\title{Arthritis}
\description{
Arthritis treatment data.
}

\usage{data(Arthritis)}
\format{A data frame with 84 observations and 5 variables.
  \describe{
    \item{ID}{patient ID,}
    \item{Treatment}{factor indicating treatment (Placebo, Treated),}
    \item{Sex}{factor indicating sex (Female, Male),}
    \item{Age}{age of patient,}
    \item{Improved}{ordered factor indicating treatment outcome (None, Some, Marked).}
  }
}

\references{
}

\source{Friendly, Visualizing Categorical Data:
\url{http://hotspur.psych.yorku.ca/ftp/sas/vcd/catdata/arthrit.sas}}

\examples{
data(Arthritis)
attach(Arthritis)
ftable(table(Treatment, Sex, Improved))
}

\keyword{datasets}
