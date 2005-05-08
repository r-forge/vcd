\name{Pairsplot panel functions for off-diagonal cells}
\alias{panel_mosaic}
\alias{panel_assoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Off-diagonal panel functions for pairs.table}
\description{
Off-diagonal panel functions for \code{\link{pairs.table}}.
}
\usage{
panel_mosaic(type = c("pairwise", "total", "conditional", "joint"),
             legend = FALSE, margins = c(0, 0, 0, 0), labeling = NULL,
             shade = TRUE, ...)
panel_assoc(legend = FALSE, margins = c(0, 0, 0, 0), labeling = NULL,
            shade = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{character string specifying the type of independence model
    visualized in the cells.}
  \item{legend}{logical specifying whether a legend shall be displayed
    in the cells or not.}
  \item{margins}{margins inside each cell (see \code{\link{strucplot}}).}
  \item{labeling}{labeling function or labeling-generating function (see
  \code{\link{strucplot}}).}
  \item{shade}{shading function or shading-generating function (see
  \code{\link{strucplot}}).}
  \item{\dots}{other parameters passed to panel function.}
}
\details{
These functions really just wrap \code{\link{assoc}} and
\code{\link{mosaic}} by basically inhibiting labeling and
legend-drawing and setting the margins to 0.
}
\value{
  A function with arguments:
  \item{x}{contingency table.}
  \item{i,j}{cell coordiantes.}
}
\seealso{
  \code{\link{pairs.table}},
  \code{\link{panel_text}},
  \code{\link{panel_barplot}},
  \code{\link{assoc}},
  \code{\link{mosaic}}
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
\author{David Meyer \email{david.meyer@R-project.org}}
\examples{
data(UCBAdmissions)
data(PreSex)

pairs(PreSex)
pairs(UCBAdmissions)
pairs(UCBAdmissions, upper_panel_args = list(shade = FALSE))
pairs(UCBAdmissions, lower_panel = panel_mosaic(type = "conditional"))
pairs(UCBAdmissions, upper_panel = panel_assoc)
}
\keyword{hplot}
