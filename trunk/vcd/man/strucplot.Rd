\name{strucplot}
\alias{strucplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a strucplot object.}
%Z% really a strucplot object?
\description{
  This modular function visualizes certain
  aspects of high-dimensional contingency tables in a hierarchical way. 
}
\usage{
strucplot(x, residuals = NULL, expected = NULL, df = NULL,
  condvars = NULL, shade = NULL, type = c("observed", "expected"),
  residuals.type = c("Pearson", "deviance", "FT"), split.vertical = TRUE, 
  spacing = spacing.equal, spacing.args = list(),
  gp = NULL, gp.args = list(),
  labeling = labeling.text, labeling.args = list(),
  panel = panel.mosaicplot, panel.args = list(),
  legend = legend.resbased, legend.args = list(),
  main = NULL, sub = NULL, margin = rep.int(2.5, 4), legend.width = unit(0.15, "npc"),
  pop = FALSE, title.gp = gpar(fontsize = 20), newpage = TRUE, keepAR = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a contingency table in array form, with optional category
    labels specified in the 'dimnames(x)' attribute.}
  \item{residuals}{Optionally, an array of residuals in the same shape
    than \code{x} (see details).}
  \item{expected}{Optionally, an array of expected values in the same shape
    than \code{x}, or alternatively a formula object to create such a table as used by
    \code{\link[stats]{loglin}} or \code{\link[MASS]{loglm}} (see details).}
  \item{df}{degrees of freedom of the independence model --- TO BE REMOVED ---}
  \item{condvars}{vector of integers decribing the conditioning
    variables for the visualization of conditional independence
    models. This information is used for computing the expected values,
    and is also passed to the spacing functions (see
    \code{\link{spacings}}). Note that the user is responsible for
    permuting \code{x} accordingly to have the conditioning
    variables come first. Then usually, \code{condvars} will be
    \code{1:k}, \code{k} number of conditioning variables. -- FIXME: so
    just expect the numbers? --}
  \item{shade}{logical specifying whether shading should be used or not
    (see \code{gp})}
  \item{residuals.type}{a character string indicating the type of residuals to be
    computed when none are supplied.
    Must be one of \code{"pearson"} (giving components
    of Pearson's chi-squared), \code{"deviance"} (giving components of
    the likelihood ratio chi-squared), or \code{"FT"} for the
    Freeman-Tukey residuals.  The value of this argument can be
    abbreviated.}
  \item{type}{a character string indicating whether the
    \code{"observed"} or the \code{"expected"} values of the table shall be
    visualized.}
  \item{split.vertical}{logical vector of length \eqn{k},
    \eqn{k} dimensionality of \code{x} (values are recycled as needed).
    A \code{TRUE} component indicates that the
    tile(s) of the corresponding dimension 
    shall be split vertically, \code{FALSE} means
    horizontal splits.
    Ignored if \code{direction} is not \code{NULL}.}
  \item{spacing}{either a spacing-generating function, or a spacing
    function (see details and \code{\link{spacings}}.}
  \item{spacing.args}{list of arguments for the
    spacing-generating function, if specified.}
  \item{gp}{either a shading-generating function, or a shading
    function (see details and \code{\link{shadings}}.}
  \item{gp.args}{list of arguments for the
    shading-generating function, if specified.}
  \item{labeling}{either a labeling-generating function, or a labeling
    function (see details and \code{\link{labelings}}.}
  \item{labeling.args}{list of arguments for the
    labeling-generating function, if specified.}
  \item{panel}{either a panel-generating function, or a panel
    function (see details). Currently, panel-generating functions for
    mosaic plots (\code{\link{panel.mosaic}}) and association plots
    (\code{\link{panel.assoc}}) are provided.}
  \item{panel.args}{list of arguments for the
    panel-generating function, if specified.}
  \item{legend}{either a legend-generating function, or a legend
    function (see details and \code{\link{legends}}.}
  \item{legend.args}{list of arguments for the
    legend-generating function, if specified.}
  \item{main}{either a logical, or a character string used for
    plotting the main title. If \code{main} is a logical and
    \code{TRUE}, the name of the object supplied as \code{x} is used.}
  \item{sub}{either a logical, or a character string used for
    plotting the subtitle. If \code{sub} is a logical and
    \code{TRUE}, the name of the object supplied as \code{x} is used.}
  \item{margin}{either an object of class \code{"unit"} of length 4, or
    a numeric vector of length 4. The elements are recycled as
    needed. The four components specify the top, right, bottom, and left
    margin of the plot, respectively. When a numeric vector is supplied,
    the numbers are interpreted as \code{"lines"} units. In addition,
    it may have named arguments ("top", "right", "bottom", and "left"),
    in which case the non-named arguments specify the default values
    (recycled as needed), overloaded by the named arguments.}
  \item{legend.width}{either an object of class "unit" of length 1, or a
  numeric value interpreted as proportion of the available horizontal
  space, specifying the width of the legend (if any).}
  \item{pop}{logical indicating whether the generated viewport-tree
    shall be removed at the end of the drawing or not.}
  \item{title.gp}{object of class \code{"gpar"} containing the graphical
  parameters used for the main and sub titles.}
  \item{newpage}{logical indicating whether a new page should be created
  for the plot or not.}
  \item{keepAR}{logical indicating whether the aspect ration should be
    fixed or not.}
}
\details{
--- many details ---
}
\value{
Invisibly, an object of class "structable".
}
\author{David Meyer (\email{david.meyer@R-project.org})}

\seealso{
  \code{\link{assoc}},
  \code{\link{mosaic}},
  \code{\link{panel.assoc}},
  \code{\link{panel.mosaic}},
  \code{\link{structable}},
  \code{\link{doubledeckerplot}}
  \code{\link{labelings}}
  \code{\link{shadings}}
  \code{\link{legends}}
  \code{\link{spacings}}
}

\examples{

}
\keyword{hplot}% at least one, from doc/KEYWORDS

