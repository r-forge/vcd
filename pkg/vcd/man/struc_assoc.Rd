\name{struc_assoc}
\alias{struc_assoc}
\title{Core-generating Function for Association Plots}
\description{
  Core-generating function for \code{strucplot} returning a function
  producing association plots.
}
\usage{
struc_assoc(compress = TRUE, xlim = NULL, ylim = NULL,
  yspace = unit(0.5, "lines"), xscale = 0.9, gp_axis = gpar(lty = 3))
}
\arguments{
  \item{compress}{logical; if \code{FALSE}, the space between the rows
    (columns) are chosen such that the \emph{total} heights (widths) of
    the rows (column) are all equal.  If \code{TRUE}, the space between
    the rows and columns is fixed and hence the plot is more
    \dQuote{compressed}.} 
  \item{xlim}{either a \eqn{2 \times k}{2 x k} matrix of doubles,
    \eqn{k} the number of total columns of the plot, or a recycled
    vector from which such a matrix will be constructed.
    The columns of \code{xlim} correspond to the columns of the
    association plot, the rows describe the column ranges (minimums in
    the first row, maximums in the second row).
    If \code{xlim} is \code{NULL}, the ranges are determined from the
    residuals according to \code{compress} (if \code{TRUE}: widest range
    from each column, if \code{FALSE}: from the whole association plot
    matrix).}
  \item{ylim}{either a \eqn{2 \times k}{2 x k} matrix of doubles,
    \eqn{k} the number of total rows of the plot, or a recycled vector
    from which such a matrix will be constructed.
    The columns of \code{ylim} correspond to the rows of the association
    plot, the rows describe the column ranges (minimums in the first
    row, maximums in the second row).
    If \code{ylim} is \code{NULL}, the ranges are determined from the
    residuals according to \code{compress} (if \code{TRUE}: widest range
    from each row, if \code{FALSE}: from the whole association plot
    matrix).}
  \item{xscale}{scale factor resizing the tile's width, thus adding
    additional space between the tiles. }
  \item{yspace}{object of class \code{"unit"} specifying additional
    space separating the rows.}
  \item{gp_axis}{object of class \code{"gpar"} specifying the visual
    aspects of the tiles' baseline.}
}
\details{
  This function is usually called by \code{strucplot} (typically when
  called by \code{assoc}) and returns a function used by
  \code{strucplot} to produce association plots. 
}
\value{
  A function with arguments:
  \item{residuals}{table of residuals.}
  \item{observed}{not used by \code{struc_assoc}.}
  \item{expected}{table of expected frequencies.}
  \item{spacing}{object of class \code{"unit"} specifying the space
    between the tiles.}
  \item{gp}{list of \code{gpar} objects used for the drawing the tiles.}
  \item{split_vertical}{vector of logicals indicating, for each
    dimension of the table, the split direction.}
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}
\seealso{
  \code{\link{assoc}},
  \code{\link{strucplot}},
  \code{\link{structable}}
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
  \url{http://datavis.ca/papers/sugi/sugi17.pdf}

  Meyer, D., Zeileis, A., and Hornik, K. (2006),
  The strucplot framework: Visualizing multi-way contingency tables with
  \pkg{vcd}.
  \emph{Journal of Statistical Software}, \bold{17(3)}, 1-48.
  \doi{10.18637/jss.v017.i03} and available as 
  \code{vignette("strucplot")}.
}
\examples{
## UCB Admissions
data("UCBAdmissions")
ucb <- aperm(UCBAdmissions)

## association plot for conditional independence
strucplot(ucb, expected = ~ Dept * (Admit + Gender),
  core = struc_assoc(ylim = c(-4, 4)), labeling_args = list(abbreviate_labs = c(Admit = 3)))
}
\keyword{hplot}
