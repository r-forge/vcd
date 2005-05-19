\name{legends}
\alias{legends}
\alias{legend_resbased}
\alias{legend_fixed}
\title{Legend Functions for Strucplots}
\description{
  These functions generate legend functions for residual-based shadings.
}
\usage{
legend_resbased(fontsize = 12, x = unit(1, "lines"), y = unit(0.1,"npc"),
                height = unit(0.8, "npc"), width = unit(0.7, "lines"),
                digits = 3, check_overlap = TRUE, text = NULL, steps = 200,
                ticks = 10, pvalue = TRUE)
legend_fixed(fontsize = 12, x = unit(1, "lines"), y = unit(0.2, "npc"),
             height = unit(0.8, "npc"), width = unit(1.5, "lines"), digits = 3, 
             check_overlap = TRUE, text = NULL)
}
\arguments{
  \item{fontsize}{fontsize of title and p-value text.}
  \item{x, y}{objects of class \code{"unit"} indicating the coordinates of the title.}
  \item{height, width}{object of class \code{"unit"} indicating the height/width of the legend.}
  \item{digits}{number of digits for the scale labels.}
  \item{check_overlap}{logical indicating whether overlap of scale
    labels should be inhibited or not.}
  \item{text}{character string indicating the title of the legend.}
  \item{steps}{granularity of the color gradient.}
  \item{ticks}{number of scale ticks.}
  \item{pvalue}{logical indicating whether the \eqn{p}-value should be
    visualized under the scale or not.}
}
\value{
  A function with arguments:
  \item{residuals}{residuals from the fitted independence model to be visualized.}
  \item{shading}{shading-function computing colors from residuals (see details).}
  \item{autotext}{character vector indicating the title to be used when
    no \code{text} argument is specified. Allows strucplot to generate
    sensible defaults depending on the residuals type.}
}
\details{
  These functions generate legend functions for residual-based
  shadings only, visualizing deviations from expected values of an hypothesized
  independence model. The legend, therefore, uses a supplied shading
  function to visualize the color gradient for the residuals range. For
  more details on the shading functions and their return values, see \code{\link{shadings}}.
}
\references{
  David Meyer, Achim Zeileis, and Kurt Hornik.
  Visualizing independence using extended association plots. In: Kurt
  Hornik, Friedrich Leisch, Achim Zeileis, editors, Proceedings of the
  3rd International Workshop on Distributed Statistical Computing,
  Vienna, Austria.
}

\author{
  David Meyer \email{David.Meyer@R-project.org}
}
\seealso{
  \code{\link{structable}},
  \code{\link{shadings}}
}

\examples{
mosaic(Titanic, legend = legend_resbased)
mosaic(Titanic, legend = legend_fixed)
}
\keyword{hplot}

