\name{mplot}
\alias{mplot}
\title{Multiple Grid plots}
\description{
  combines severals grid-based plots in a multi-panel-layout.
}
\usage{
mplot(...,
      layout = NULL, cex = NULL,
      main = NULL, gp_main = gpar(fontsize = 20),
      sub = NULL, gp_sub = gpar(fontsize = 15),
      keep_aspect_ratio = TRUE)
}
\arguments{
  \item{\dots}{A list of objects inheriting from class \code{"gTree"},
    or having a \code{".GTREE"} attribute containing such an object, or
    a \code{".REDRAW"} attribute containing a function that produces some
    grid graphics.}
  \item{layout}{integer vector of length 2 giving the number of rows and
    columns. If \code{NULL}, the values will be
    guessed using some heuristics from the number of objects supplied in
    \dots.}
  \item{cex}{Scaling factor for the fonts in the subplots. If
    \code{NULL}, the value is calculated as the inverse square root of
    the row number.}
  \item{main, sub}{Optional main and sub title, respectively.}
  \item{gp_main, gp_sub}{Optional objects of class \code{"gpar"} specifying
    the graphical parameters for the main and sub title, respectively.}
  \item{keep_aspect_ratio}{logical; should the aspect ratio of the plots
  be fixed?}
}
\value{
  None.
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}
\examples{
mplot(mosaic(Titanic), assoc(Titanic))

A = mosaic(Titanic)
B = mosaic(Titanic, type = "expected")
mplot(A, B)

mplot(sieve(SexualFun), agreementplot(SexualFun), main = "Sexual Fun")

}