\name{sieveplot}
\alias{sieveplot}
\title{Sieve Plots}
\description{
Visualization of two-way contingency tables: plots rectangles with surfaces
proportional to the expected cell frequencies and filled with a number of
squares equal to the expected frequencies. Thus, the densities visualize
the deviations of the observed from the expected values.
}
\usage{
sieveplot(x, reverse.y = TRUE, type = c("sieve", "expected"), title = NULL, values = c("none", "cells", "margins", "both"), frequencies = c("absolute", "relative"), cex.main = 3, cex.lab = 2, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{A two-way contingency table}
  \item{reverse.y}{If \code{TRUE}, the y axis is reversed (i.e., the
    rectangles' positions correspond to the contingency table)}
  \item{type}{\code{expected} fills the rectangles 
    according to the \emph{expected} frequencies.}
  \item{title}{User specified title}
  \item{values}{Optionally, the frequencies of the \code{cells} or
    \code{margins} or of \code{both} can be plotted.}
  \item{frequencies}{Chooses the type of these frequencies:
    \code{relative} or \code{absolute}}
  \item{cex.main}{Font size of title}
  \item{cex.lab}{Font size of labels}
  \item{\dots}{Other graphics parameters (see \code{par})}
}
\references{Friendly, Michael (2000): Visualizing Categorical Data, SAS
  Publishing.
}
\author{
  David Meyer\cr
  \email{david.meyer@ci.tuwien.ac.at}
}
\seealso{\code{\link{mosaicplot}}}

\examples{
data(HairEyeColor)
(tab <- apply(HairEyeColor, 1:2, sum))
sieveplot(t(tab))
sieveplot(t(tab), type = "expected", values = "both")
}
\keyword{category}% at least one, from doc/KEYWORDS
\keyword{hplot}% __ONLY ONE__ keyword per line
