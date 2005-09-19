\name{mondrian}
\alias{mondrian}
\title{Mondrian Mosaic}
\description{
Draws a painting from Piet Mondrian.
}
\usage{
mondrian()
}
\value{
  Invisibly, a list with components:
  \item{table}{table used for creating the painting.}
  \item{fill}{fill color used for the shading.}
}
\details{
  This function tries to reproduce the painting: \dQuote{Composition
    with Red Yellow and Blue - 1927} from Piet Mondrian (1872--1944).
}
\seealso{
  \code{\link{mosaic}},
  \code{\link{shadings}},
  \code{\link{spacings}}.
}
\examples{
mondrian()
}
\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}\cr
  David Meyer \email{David.Meyer@R-project.org}
}
\keyword{hplot}
