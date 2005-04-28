\name{labeling_cells}
\alias{labeling_list}
\alias{labeling_cells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Labeling functions for cells and simple label lists}
\description{
These functions generate labeling functions that produce labels for strucplots.
}
\usage{
labeling_cells(labels = TRUE, varnames = TRUE, abbreviate_labels = FALSE,
               abbreviate_varnames = FALSE, gp = gpar(), lsep = ": ",
               lcollapse = "\n", just = "center", pos = "center", rot = 0,
               margin = unit(0.5, "lines"), clip_cells = TRUE, text = NULL, ...)
labeling_list(gp = gpar(), just = "left", pos = "left", lsep = ": ",
              sep = " ", offset = unit(c(2, 2), "lines"),
              varnames = TRUE, cols = 2, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{logical vector indicating, for each dimension, whether
    labels for the factor levels should be drawn. Values are recycled as needed.}
  \item{varnames}{logical vector indicating, for each dimension, whether variable names
    should be drawn. Values are recycled as needed.}
  \item{abbreviate_labels}{logical vector indicating, for each
    dimension, the number of characters to which the labels should be
    abbreviated. \code{TRUE} means 1 character, \code{FALSE} causes no
    abbreviation. Values are recycled as needed.}
  \item{abbreviate_varnames}{logical vector indicating, for each
    dimension, the number of characters to which the variable (=dimension) names should be
    abbreviated. \code{TRUE} means 1 character, \code{FALSE} causes no
    abbreviation. Values are recycled as needed.}
  \item{gp}{object of class \code{"gpar"} used for the drawn text.}
  \item{lsep}{character that separates variable names from the factor levels.}
  \item{sep}{character that separates the factor levels (only used for \code{labeling_list}).}
  \item{offset}{object of class \code{"unit"} of length 2 specifying the
    offset in x- and y-direction of the text block drawn under the
    strucplot (only used for \code{labeling_list}).}
  \item{cols}{Number of text columns (only used for \code{labeling_list}).}
  \item{lcollapse}{character that separates several variable name/factor
  level-combinations, typically a line break (only used for \code{labeling_cells}).}
  \item{just}{character vector of length 1 (\code{labeling_list}) or at
    most 2 (\code{labeling_cells}) specifying the horizontal and
    vertical justification of the text drawn.}
  \item{pos}{character string of length 1 (\code{labeling_list}) or at
    most 2 (\code{labeling_cells}) specifying the labels' horizontal position
    (horizontal and vertical position for \code{labeling_cells}).}
  \item{rot}{rotation angle in degrees, used for all labels (only used for \code{labeling_cells}).}
  \item{margin}{object of class \code{"unit"} (a numeric value is
    converted to \code{"lines"}) specifying an offset from
    the cell borders (only used for \code{labeling_cells}).}
  \item{clip_cells}{logical indicating whether text should be clipped at
  the cell borders (only used for \code{labeling_cells}).}
  \item{text}{Optionally, a character table of the same dimension than
    the contingency table whose entries will then be used instead of
    the labels. \code{NA} entries are not drawn.
    This allows for custom cell annotations (see examples). Only used for \code{labeling_cells}.}
  \item{\dots}{Currently not used.}
}
\details{
  These functions generate labeling functions called by
  \code{\link{strucplot}} for their side-effect of adding labels to the
  plot. They suppose that a strucplot has been drawn and the
  corresponding viewport structure is pushed, since the positions of the
  viewports are used for the label positioning.

  This help page only documents \code{labeling_list} and
  \code{labeling_cells}, (many) more functions are described on the help page
  for \code{\link{labeling_text}}.
  
  Using \code{labeling_list} will typically necessitate a bottom margin
  adjustment.

  The functions can also be used sQuote{stand-alone} as shown in the examples.
}
\value{
  A function with arguments:
  \item{d}{\code{"dimnames"} attribute from the visualized contingency
    table, or the visualized table itself from which the
    \code{"dimnames"} attributes will then be extracted.}
  \item{split_vertical}{logical vector indicating the split directions.}
  \item{condvars}{integer vector of conditioning dimensions}
}
\author{David Meyer (\email{David.Meyer@R-project.org})}

\seealso{
  \code{\link{labeling_text}}, 
  \code{\link{structable}}, 
  \code{\link[grid]{grid.text}}
}

\examples{
mosaic(Titanic, labeling = labeling_cells)
mosaic(Titanic, labeling = labeling_list)

## A more comlex example, adding the observed frequencies to a mosaicplot:
tab <- ifelse(Titanic < 6, NA, Titanic)
mosaic(Titanic)
labeling_cells(text = tab, margin = 0)(Titanic)
}
\keyword{hplot}% at least one, from doc/KEYWORDS

