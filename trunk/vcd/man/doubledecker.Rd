\name{doubledecker}
\alias{doubledecker.default}
\alias{doubledecker.formula}
\alias{doubledecker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{doubledecker plot}
\description{
This function creates a doubledecker plot visualizing a classification rule.
}
\usage{
\method{doubledecker}{formula}(formula, data = NULL, ..., main = NULL)
\method{doubledecker}{default}(x, depvar = length(dim(x)), margins = c(1,4, length(dim(x)) + 1, 1),
                col = hcl(seq(0, 260, length = dim(x)[length(dim(x))]), 50, 70),
                labeling = labeling_doubledecker, spacing = spacing_doubledecker,
                main = NULL, keepAR = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula specifying the variables used to create a 
    contingency table from \code{data}. The dependent variable is used
    last for splitting.}
  \item{data}{either a data frame, or an object of class \code{"table"}
    or \code{"ftable"}.}
  \item{x}{a contingency table in array form, with optional category
    labels specified in the \code{dimnames(x)} attribute.}
  \item{depvar}{dimension index or character string specifying the
    dependent variable. That will be sorted last in the table.}
  \item{margins}{margins of the plot. Note that by default, all factor names (except
    the last one) and their levels are visualized \emph{as a block}
    under the plot.}
  \item{col}{vector of colors used for the tiles of the last dimension.}
  \item{labeling}{labeling function or corresponding generating generating function (see
    \code{\link{strucplot}} for details).}
  \item{spacing}{spacing object, spacing function or corresponding generating function (see
    \code{\link{strucplot}} for details).}
  \item{main}{character vector specifying the main title.}
  \item{keepAR}{logical indicating whether the aspect ratio should be
    maintained or not.}
  \item{\dots}{Further parameters passed to \code{mosaic}.}
}
\details{
Doubledecker plots visualize the causal relationship between one categorical
(typically binary) variable and other categorical variables. Formally,
they are mosaic plots with vertical splits for all dimensions except the
last one (antecedents), which represents the dependent variable
(consequent). The last variable is visualized by horizontal splits,
no space between the tiles, and separate colors for the levels.
}
\value{
An object of class \code{"structable"} corresponding to the plot.
}
\references{
  Hoffmann, Heike (2001),
  Generalized Odds Ratios for Visual Modeling. \emph{Journal of
    Computational and Graphical Statistics}, \bold{10}, 4, 628--640.
}
\author{David Meyer \email{David.Meyer@R-project.org}}

\seealso{
  \code{\link{strucplot}},
  \code{\link{mosaic}}.
}
\examples{
doubledecker(Titanic)
doubledecker(Titanic, depvar = "Survived")
doubledecker(Survived ~ ., data = Titanic)
}
\keyword{hplot}% at least one, from doc/KEYWORDS

