\name{Hitters}
\alias{Hitters}
\non_function{}
\title{Hitters}
\description{
Hitters data.
}

\usage{data(Hitters)}
\format{A data frame with 154 observations and 4 variables.
  \describe{
    \item{Positions}{factor indicating ,}
    \item{Putouts}{,}
    \item{Assists}{,}
    \item{Erros}{.}
  }
}

\references{
M. Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Publishing.
}

\source{Michael Friendly (2000), Visualizing Categorical Data, p. ??.}

\examples{
data(Hitters)
attach(Hitters)

colors <- c("black","red","green","blue","red","black","blue")
pch <- substr(levels(Positions), 1, 1)
ternaryplot(
            Hitters[,2:4],
            pch = as.character(Positions),
            col = colors[codes(Positions)],
            main = "Baseball Hitters Data"
            )
legend(
       0.7, 0.85,
       legend = c("POSITION(S)", levels(Positions)),
       pch = c("", pch),
       col = c(NA, colors)
       )

  detach(Hitters)
}

\keyword{datasets}
