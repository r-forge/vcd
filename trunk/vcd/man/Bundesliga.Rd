\name{Bundesliga}
\alias{Bundesliga}
\non_function{}
\title{Ergebnisse der Fussball-Bundesliga}
\description{
Results from the first German soccer league in the years
1995/6 and 2001/2.
}

\usage{data(Bundesliga)}
\format{A data frame with 612 observations and 6 variables.
  \describe{
    \item{Heimteam}{character. Name of the home team,}
    \item{Gastteam}{character. Name of the away team,}
    \item{Heimtore}{number of goals scored by the home team,}
    \item{Gasttore}{number of goals scored by the away team,}
    \item{Spieltag}{day of the game,}
    \item{Jahr}{year in which the season started.}
  }
}

\details{The data for the season 1995/6 is taken from Knorr-Held (1999),
the data for the season 2001/2 was collected by Achim Zeileis.
}

\references{
Leonhard Knorr-Held (1999), Dynamic rating of sports teams.
SFB 386 ``Statistical Analysis of Discrete Structures'',
Discussion paper \bold{98}.
}

\source{SFB 386 ``Statistical Analysis of Discrete Structures''
\url{http://www.stat.uni-muenchen.de/service/datenarchiv/bundesliga/bundesliga.html}}

\seealso{\code{\link{UKSoccer}}}

\examples{
data(Bundesliga)

## independence of home and away goals
tore95 <- xtabs(~ Heimtore + Gasttore, data = Bundesliga, subset = Jahr == 1995)
tore95
mosaicplot(tore95, shade = TRUE)

## number of goals per game poisson distributed?
ntore1 <- xtabs(~ Heimtore, data = Bundesliga, subset = Jahr == 1995)
ntore2 <- xtabs(~ Gasttore, data = Bundesliga, subset = Jahr == 1995)
ntore3 <- table(apply(subset(Bundesliga, Jahr == 1995)[,3:4], 1, sum))

gf1 <- goodfit(ntore1)
gf2 <- goodfit(ntore2)
gf3 <- goodfit(ntore3)

summary(gf1)
summary(gf2)
summary(gf3)
plot(gf1)
plot(gf2)
plot(gf3)

Ord.plot(ntore1)
distplot(ntore1)
}

\keyword{datasets}
