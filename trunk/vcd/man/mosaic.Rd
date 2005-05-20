\name{mosaic}
\alias{mosaic}
\alias{mosaic.default}
\alias{mosaic.formula}
\title{Extended Mosaic Plots}
\description{
  Plots (extended) mosaic displays.
}
\usage{
\method{mosaic}{default}(x, condvars = NULL,
       split_vertical = FALSE, direction = NULL, spacing = NULL,
       spacing_args = list(), visZero = TRUE, zeroSize = 0.5, ...)
\method{mosaic}{formula}(formula, data, ..., main = NULL)
}
\arguments{
  \item{x}{a contingency table in array form, with optional category
    labels specified in the \code{dimnames(x)} attribute.}
  \item{condvars}{vector of integers or character strings indicating
    conditioning variables, if any. The table will be permuted to order
    them first.}
  \item{formula}{a formula specifying the variables used to create a 
    contingency table from \code{data}.  For convenience, conditioning
    formulas can be specified; the conditioning variables will then be
    used first for splitting.  Formulas for mosaic displays (unlike
    those for doubledecker plots) have no response variable.}
  \item{data}{either a data frame, or an object of class \code{"table"}
    or \code{"ftable"}.}
  \item{visZero}{if \code{TRUE}, zero entries are represented by
    a bullet.}
  \item{zeroSize}{size of the bullets used for zero entries (ignored if 
    \code{visZero} is \code{FALSE}).}
  \item{split_vertical}{vector of logicals of length \eqn{k}, \eqn{k}
    the dimensionality of \code{x} (values are recycled as needed). 
    A \code{TRUE} component indicates that the tile(s) of the
    corresponding dimension shall be split vertically, \code{FALSE}
    means horizontal splits.
    Ignored if \code{direction} is not \code{NULL}.}
  \item{direction}{character vector of length \eqn{k}, \eqn{k} the
    dimensionality of \code{x} (values are recycled as needed).
    For each component, a value of \code{"h"} indicates that the tile(s)
    of the corresponding dimension shall be split horizontally, whereas
    \code{"v"} indicates vertical split(s).}
  \item{spacing}{spacing object, spacing function, or corresponding
    generating function (see \code{\link{strucplot}} for more
    information).
    The default is \code{spacing_equal} if \code{x} has two dimensions,
    and  \code{spacing_increase} for more dimensions.}
  \item{spacing_args}{list of arguments for the generating function, if
    specified (see \code{\link{strucplot}} for more information).}
  \item{main}{either a logical, or a character string used for plotting
    the main title.  If \code{main} is a logical and \code{TRUE}, the
    name of the \code{data} object is used.}
  \item{\dots}{Other arguments passed to \code{\link{strucplot}}}
}
\details{
  This is a generic function.  It currently has a default method and a
  formula interface.  Both are high-level interfaces to the
  \code{\link{strucplot}} function, and produce (extended) mosaic
  displays.  Most of the functionality is described there, such as
  specification of the independence model, labeling, legend, spacing,
  shading, and other graphical parameters.

  A mosaic plot is an area proportional visualization of a (possibly
  higher-dimensional) table of expected frequencies.  It is composed of
  tiles (corresponding to the cells) created by recursive vertical and
  horizontal splits of a square.  The area of each tile is proportional
  to the corresponding cell entry, \emph{given} the dimensions of
  previous splits.

  An \emph{extended} mosaic plot, in addition, visualizes the residuals
  of a log-linear model fit to the table by the color and outline of the
  tiles. This package offers a range of \emph{residual-based} shadings
  (see the \link{shadings} help page). Some of them allow, e.g., the
  visualization of test statistics.

  The layout is very flexible: the specification of shading, labeling,
  spacing, and legend is modularized (see \code{\link{strucplot}} for
  details).
}
\note{
  In contrast to the \code{\link[graphics]{mosaicplot}} function in
  \pkg{graphics}, the splits start with the \emph{horizontal} direction
  by default to match the output of \code{\link{structable}}.
}
\value{
  Invisibly, an object of class \code{"structable"}.
}
\references{
  Hartigan, J.A., and Kleiner, B. (1984)
  A mosaic of television ratings.
  \emph{The American Statistician},
  \bold{38}, 32--35.

  Emerson, J. W. (1998)
  Mosaic displays in S-PLUS: a general implementation and a case study.
  \emph{Statistical Computing and Graphics Newsletter (ASA)},
  \bold{9}, 1, 17--23.

  Friendly, M. (1994)
  Mosaic displays for multi-way contingency tables.
  \emph{Journal of the American Statistical Association}, \bold{89},
  190--200.

  The home page of Michael Friendly
  (\url{http://www.math.yorku.ca/SCS/friendly.html}) provides
  information on various aspects of graphical methods for analyzing
  categorical data, including mosaic plots.
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}
\seealso{
  \code{\link{assoc}},
  \code{\link{strucplot}},
  \code{\link{mosaicplot}},
  \code{\link{structable}},
  \code{\link{doubledecker}}
}
\examples{
data(Titanic)
mosaic(Titanic, main = "Survival on the Titanic", shade = TRUE,
legend = TRUE)
## Formula interface for tabulated data:
mosaic(~ Sex + Age + Survived, data = Titanic, shade = TRUE)

data(HairEyeColor)
mosaic(HairEyeColor, shade = TRUE)
## Independence model of hair and eye color and sex.  Indicates that
## there are significantly more blue eyed blond females than expected
## in the case of independence (and too few brown eyed blond females).

mosaic(HairEyeColor, shade = TRUE, expected = list(c(1,2), 3))
## Model of joint independence of sex from hair and eye color.  Males
## are underrepresented among people with brown hair and eyes, and are
## overrepresented among people with brown hair and blue eyes, but not
## "significantly".

## Formula interface for raw data: visualize crosstabulation of numbers
## of gears and carburettors in Motor Trend car data.
data(mtcars)
mosaic(~ gear + carb, data = mtcars, shade = TRUE)

data(PreSex)
mosaic(PreSex, condvars = c(1,4))
mosaic(~ ExtramaritalSex + PremaritalSex | MaritalStatus + Gender,
       data = PreSex)
}
\keyword{hplot}
