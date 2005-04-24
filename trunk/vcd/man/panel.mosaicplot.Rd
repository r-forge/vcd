\name{panel.mosaicplot}
\alias{panel_mosaicplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Panel-generating function for mosaic plots}
\description{
  Panel-generating function for \code{strucplot} returning a function
  producing mosaic plots.
}
\usage{
panel_mosaicplot(visZero = TRUE, zeroSize = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{visZero}{logical; shall zero entries in the contingency table be
  visualized using a bullet?}
  \item{zeroSize}{size of the zero-entries bullet (ignored if \code{visZero} is \code{TRUE})}
}
\details{
  This function is usually called by \code{strucplot} (typically when called
  by \code{mosaic}) and returns a function used by \code{strucplot} to
  produce mosaic plots. 
}
\value{
  A function with arguments:
  \item{residuals}{table of residuals}
  \item{observed}{table of observed residuals}
  \item{expected}{not used by \code{panel.mosaicplot}}
  \item{spacing}{object of class \code{"unit"} specifying the space
    between the tiles}
  \item{gp}{list of \code{gpar} objects used for the drawing the tiles}
  \item{split_vertical}{vector of logicals indicating, for each
    dimension of the table, the split direction}
}
\author{David Meyer (\email{david.meyer@R-project.org})}
\seealso{
  \code{\link{mosaic}},
  \code{\link{strucplot}},
  \code{\link{structable}},
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
\keyword{hplot}% at least one, from doc/KEYWORDS
