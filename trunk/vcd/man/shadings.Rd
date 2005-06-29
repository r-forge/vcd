\name{shadings}
\alias{shadings}
\alias{shading_hsv}
\alias{shading_hcl}
\alias{shading_max}
\alias{shading_Friendly}
\alias{shading_binary}
\title{Shading-generating Functions for Residual-based Shadings}
\description{
  Shading-generating functions for computing residual-based shadings 
  for mosaic and association plots.
}
\usage{
shading_hcl(observed, residuals = NULL, expected = NULL, df = NULL,
  h = NULL, c = NULL, l = NULL, interpolate = c(2, 4), lty = 1,
  p.value = NULL, level = 0.95, \dots)

shading_hsv(observed, residuals = NULL, expected = NULL, df = NULL,
  h = c(2/3, 0), s = c(1, 0), v = c(1, 0.5),
  interpolate = c(2, 4), lty = 1, p.value = NULL, level = 0.95, \dots)


shading_max(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
  h = NULL, c = NULL, l = NULL, lty = 1, level = c(0.9, 0.99), n = 1000, \dots)

shading_Friendly(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
  h = c(2/3, 0), lty = 1:2, interpolate = c(2, 4), \dots)

shading_binary(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
  col = hcl(c(260, 0), 50, 70))
}
\arguments{
  \item{observed}{}
  \item{residuals}{}
  \item{expected}{}
  \item{df}{}
  
  \item{h}{hue value in the HCL or HSV color description, has to be
    in [0, 360] for HCL and in [0, 1] for HSV colors. The default is
    to use blue and red for positive and negative residuals respectively.
    In the HCL specification it is \code{c(260, 0)} by default and for HSV
    \code{c(2/3, 0)}.}
  \item{c}{chroma value in the HCL color description. Defaults to \code{c(100, 20)}
    for significant and non-significant results respectively.}
  \item{l}{luminance value in the HCL color description. Defaults to \code{c(90, 50)}
    for small and large residuals respectively.}
  \item{s}{saturation value in the HSV color description. Defaults to \code{c(1, 0)}
    for large and small residuals respectively.}
  \item{v}{saturation value in the HSV color description. Defaults to \code{c(1, 0.5)}
    for significant and non-significant results respectively.}

  \item{interpolate}{}
  \item{lty}{a vector of two line types for positive and negative residuals respectively.
    Recycled if necessary.}
  \item{p.value}{}
  \item{level}{}
  \item{n}{}
  \item{col}{a vector of two colors for positive and negative residuals respectively}

  \item{\dots}{Other arguments passed to \code{\link[colorspace]{hcl}}
    or \code{\link{hsv}}, respectively.}
}
\details{
}
\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}
}
\seealso{
  \code{\link[colorspace{hcl}},
  \code{\link{hsv}},
  \code{\link{mosaic}},
  \code{\link{assoc}},
  \code{\link{strucplot}}}
}
\examples{
## load Arthritis data
data(Arthritis)
art <- xtabs(~Treatment + Improved, data = Arthritis)

## plain mosaic display without shading
mosaic(art)

## with shading for independence model
mosaic(art, shade = TRUE)
## which uses the HCL shading
mosaic(art, gp = shading_hcl)
## the residuals are two small to have color,
## hence the cut-offs can be modified
mosaic(art, gp = shading_hcl, gp_args = list(interpolate = c(1, 1.8)))
## the same with the Friendly palette 
## (without significance testing)
mosaic(art, gp = shading_Friendly, gp_args = list(interpolate = c(1, 1.8)))

## assess independence using the maximum statistic
## cut-offs are now critical values for the test statistic
mosaic(art, gp = shading_max)

## association plot with shading as in base R
assoc(art, gp = shading_binary(col = c(1, 2)))
}
\keyword{hplot}
