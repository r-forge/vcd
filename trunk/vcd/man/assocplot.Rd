\name{assocplot}
\alias{assocplot}
\title{Extended Association Plots}
\description{
  Produce a Cohen-Friendly association plot indicating deviations from
  independence of rows and columns in a 2-dimensional contingency
  table.
}
\usage{
assocplot(x, color = NULL, shade = TRUE, test = c("Chisq","none"),
          conf.level = 0.95, density = 20,
          main = NULL, space = 0.3, xlab = NULL, ylab = NULL,
          cex.axis = 0.66, ...)
}
\arguments{
  \item{x}{a two-dimensional contingency table in matrix form.}
  \item{color}{a character vector of length two defining the colors used for
    drawing positive and negative Pearson residuals, respectively
    (defaults are blue and red). Note that if \code{shade} is
    \code{TRUE}, \code{color} specifies hue
    values (default: 0 and 2/3 for blue/red).}
  \item{shade}{a logical indicating whether to produce extended association
    plots, or a numeric vector of at most 5 distinct positive numbers
    giving the absolute values of the cut points for the residuals. 
    Using \code{shade = TRUE} cuts absolute values at 2 and 4.}
  \item{test}{statistic to be used for the test of
    independence. Currently, only the Chi-squared test is
    implemented. If the hypothesis cannot be rejected and \code{shade} is \code{TRUE},
    the boxes visualizing the residuals are striped. If \code{test} is
    \code{"none"} or \code{FALSE}, no test is performed and no stripes
    are produced. Ignored if \code{shade = FALSE}.}
  \item{conf.level}{confidence level for the test of independence.}
  \item{density}{density for the stripes in case of visualized test of independence.}
  \item{space}{the amount of space (as a fraction of the average
    rectangle width and height) left between each rectange.}
  \item{main}{overall title for the plot.}
  \item{xlab}{a label for the x axis.  Defaults to the name of the row
    variable in \code{x} if non-\code{NULL}.}
  \item{ylab}{a label for the y axis.  Defaults to the column names of
    the column variable in \code{x} if non-\code{NULL}.}
  \item{cex.axis}{The magnification to be used for axis annotation,
    as a multiple of \code{par("cex")}.}
  \item{\dots}{Further graphics parameters}
}
\details{
  For a two-way contingency table, the signed contribution to Pearson's
  \eqn{\chi^2}{chi^2} for cell \eqn{i, j} is \eqn{d_{ij} = (f_{ij} -
    e_{ij}) / \sqrt{e_{ij}}}{d_{ij} = (f_{ij} - e_{ij}) / sqrt(e_{ij})},
  where \eqn{f_{ij}} and \eqn{e_{ij}} are the observed and expected
  counts corresponding to the cell.  In the Cohen-Friendly association
  plot, each cell is represented by a rectangle that has (signed) height
  proportional to \eqn{d_{ij}} and width proportional to
  \eqn{\sqrt{e_{ij}}}{sqrt(e_{ij})}, so that the area of the box is
  proportional to the difference in observed and expected frequencies.
  The rectangles in each row are positioned relative to a baseline
  indicating independence (\eqn{d_{ij} = 0}).  If the observed frequency
  of a cell is greater than the expected one, the box rises above the
  baseline, and falls below otherwise. In addition, the boxes are shaded
  depending on \code{shade} and \code{chisq.test}. If \code{shade = FALSE},
  positive/negative residuals are shaded in the color specified by the
  first/second element of \code{color} (default: black and red). If
  \code{shade = TRUE}, the residuals are shaded according to their size:
  the elements of \code{color} specify the hue value for the
  color shades of the positive/negative residuals (default: blue and
  red, respectively). If \code{test} is not \code{FALSE} or \code{"none"},
  the specified test of independence is done, and the boxes are striped
  if the hypothesis cannot be rejected.
}
\note{The code for extended shading is taken from \code{mosaicplot}.}
\seealso{
  \code{\link{mosaicplot}};
  \code{\link[ctest]{chisq.test}}.
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
\examples{
data(HairEyeColor)
## Aggregate over sex:
(x <- margin.table(HairEyeColor, c(1, 2)))
assocplot(x, main = "Relation between hair and eye color")

## Ordinary assocplot:
assocplot(x, shade = FALSE)

## Aggregate over Eye color:
(x <- margin.table(HairEyeColor, c(1, 3)))
chisq.test(x)
assocplot(x, main = "Relation between hair color and sex")
}
\keyword{hplot}






