\name{panel.assoc}
\alias{panel.assoc}
\alias{panel.mosaic}
\title{Panel Functions for Pairs Plot}
\description{
  Panel-generating functions wrapping \code{assoc} and \code{mosaic} for the
  \code{pairs.table} function.
}
\usage{
panel.assoc(type = NULL, legend = FALSE, margins = c(0, 0, 0, 0),
            labeling = NULL, shade = TRUE, ...)
panel.mosaic(type = c("pairwise", "total", "conditional", "joint"),
             legend = FALSE, margins = c(0, 0, 0, 0),
             labeling = NULL, shade = TRUE, ...)
}
\arguments{
  \item{type}{type of the independence model to be visualized (not used by \code{panel.assocplot})}
  \item{legend}{legend-function}
  \item{margins}{vector of margins (default: no margins)}
  \item{labeling}{labeling function (default: no labeling)}
  \item{shade}{logical or shading function (default: use default shading)}
  \item{\dots}{Other arguments passed to plotting functions.}
}
\details{
These functions are wrappers to make \code{assoc} and \code{mosaic}
usable by \code{pairs.table}.
}
\value{
  A panel function to be called by \code{pairs.table} with arguments:
  \item{x}{a contingency table}
  \item{i}{first dimension used to specify the visualized independence model}
  \item{j}{second dimension used to specify the visualized independence model}
}

\references{
  Cohen, A. (1980),
  On the graphical display of the significant components in a two-way
  contingency table.
  \emph{Communications in Statistics---Theory and Methods}, \bold{A9},
  1025--1041.

  Friendly, M. (1992),
  Graphical methods for categorical data.
  \emph{SAS User Group International Conference Proceedings}, \bold{17},
  190--200.
  \url{http://www.math.yorku.ca/SCS/sugi/sugi17-paper.html}
}
\author{David Meyer (\email{david.meyer@R-project.org})}
\seealso{\code{\link{pairs.table}}, \code{\link{assoc}}, \code{\link{mosaic}}}
\examples{
pairs(Titanic, upper.panel = panel.assoc())
}
\keyword{hplot}
