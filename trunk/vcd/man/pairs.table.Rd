\name{pairs.table}
\alias{pairs.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pairs plot for contingency tables}
\description{
Priduces a matrix of association of mosaic plots.
}
\usage{
pairs.table(x, upper_panel = panel_mosaic(), lower_panel = panel_mosaic(), diag_panel = panel_barplot(), main = NULL, title_gp = gpar(fontsize = 20), space = 0.1, newpage = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A contingency table}
  \item{upper_panel}{Panel function for the upper triangle of the matrix}
  \item{lower_panel}{Panel function for the lower triangle of the matrix}
  \item{diag_panel}{Panel function for the diagonal of the matrix}
  \item{main}{Main title}
  \item{title_gp}{Object of class \code{"gpar"} used for the title}
  \item{space}{double specifying the distance between the cells}
  \item{newpage}{logical controlling whether a new grid page should be created}
  \item{\dots}{currently not used}
}
\details{
  \code{mosaicpairs()} plots a matrix of pairwise mosaic plots, four
  independence types are distinguished:
  \code{"pairwise"}, \code{"total"}, \code{"conditional"} and \code{"joint"}.
  The pairwise mosaic matrix shows bivariate marginal relations, collapsed over
  all other variables. The total independence mosaic matrix shows mosaic plots
  for mutual independence, i.e. for marginal and conditional independence
  among all pairs of variables. The conditional independence mosaic matrix shows
  mosaic plots for marginal independence given all other variables. The
  joint independence mosaic matrix shows mosaic plots for joint independence of all pairs of
  variables from the others.
}
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
\author{David Meyer (\email{david.meyer@R-project.org})}
\examples{
data(UCBAdmissions)
data(PreSex)

pairs(PreSex)
pairs(UCBAdmissions)
pairs(UCBAdmissions, lower_panel = panel_mosaic(type="conditional"))
pairs(UCBAdmissions, upper_panel = panel_assoc())
}
\keyword{hplot}
