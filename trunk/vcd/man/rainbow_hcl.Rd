\name{rainbow_hcl}
\alias{rainbow_hcl}
\alias{diverge_hcl}
\alias{diverge_hsv}
\alias{heat_hcl}
\alias{sequential_hcl}
\alias{terrain_hcl}
\title{HCL and HSV Color Palettes}
\description{
  Color palettes based on the HCL and HSV color spaces.
}
\usage{
rainbow_hcl(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n, \dots)

diverge_hcl(n, h = c(260, 0), c = 100, l = c(90, 50), \dots)
diverge_hsv(n, h = c(2/3, 0), s = 1, v = 1, \dots)  

heat_hcl(n, h = c(0, 90), c = 100, l = c(50, 90), correct = 5, \dots)
terrain_hcl(n, h = c(130, 0), c = 80, l = c(60, 100), correct = 10, \dots)
sequential_hcl(n, h = 260, c = 100, l = c(50, 90), gamma = 1, \dots)
}
\arguments{
  \item{n}{the number of colors (\deqn{\ge 1}{>= 1}) to be in the palette.}
  \item{c}{chroma value in the HCL color description.}
  \item{l}{luminance value in the HCL color description.}
  \item{start}{the hue at which the rainbow begins.}
  \item{end}{the hue at which the rainbow ends.}
  \item{h}{hue value in the HCL or HSV color description, has to be
    in [0, 360] for HCL and in [0, 1] for HSV colors.}
  \item{s}{saturation value in the HSV color description.}
  \item{v}{value value in the HSV color description.}
  \item{correct}{control parameter for \code{heat_hcl} and \code{terrain_hcl}
    determining how slowly chroma should fade out.}
  \item{gamma}{control parameter for gamma correction in \code{sequential_hcl},
    i.e., this determins how quickly luminance decreases.}
  \item{\dots}{Other arguments passed to \code{\link[colorspace]{hcl}}
    or \code{\link{hsv}}, respectively.}
}
\details{
  All functions compute palettes based on either the HCL or
  the HSV color space. \code{rainbow_hcl} computes a rainbow of colors
  defined by different hues given a single value of each chroma and luminance.
  It corresponds to \code{\link{rainbow}} which computes a rainbow in
  HSV space.
  
  Both functions, \code{diverge_hcl} and \code{diverge_hsv}, compute 
  a set of colors diverging from a neutral center (grey or white, without
  color) to two different extreme colors (blue and red by default). This
  is similar to \code{\link{cm.colors}}. For the diverging HSV colors,
  two hues \code{h} are needed, a maximal saturation \code{s} and a
  fixed value \code{v}. The saturation is then varied to obtain the
  diverging colors. For the diverging HCL colors, again two hues \code{h}
  are needed, a maximal chroma \code{chroma} and two luminances \code{l}.
  The colors are then created by linear interpolation between the 
  neutral color (with zero chroma and luminance \code{l[1]}) and 
  the full color (with chroma \code{c} and luminance \code{l[2]}).

  The palette \code{heat_hcl} gives an implementation of \code{\link{heat.colors}}
  in HCL space. By default, it goes from a red to a yellow hue, while
  simultaneously going to lighter colors (i.e., increasing luminance)
  and reducing the amount of color (i.e., decreasing chroma). Similarly,
  \code{sequential_hcl} gives a palette that goes from a fully saturated
  color (blue by default) to a lighter grey. This is essentially one branch
  of the \code{diverge_hcl} palettes.

  The \code{terrain_hcl} palette simply calls \code{heat_hcl} with different
  parameters, providing colors similar in spirit to \code{terrain.colors}.
  The lighter colors are not strictly HCL colors, though.

  The diverging palettes are used for choosing the colors for mosaic 
  displays in \code{\link{mosaic}}.
}
\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}
}
\seealso{
  \code{\link[colorspace]{hcl}},
  \code{\link{hsv}},
  \code{\link{shadings}}}
}
\examples{
## convenience demo functions
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...) 

pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab="")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## qualitative palette
wheel(rainbow_hcl(12))

## a few useful diverging HCL palettes
pal(diverge_hcl(7))
pal(diverge_hcl(7, h = c(246, 40), c = 96, l = c(90, 65)))
pal(diverge_hcl(7, h = c(130, 43), c = 100, l = c(90, 70)))
pal(diverge_hcl(7, h = c(180, 70), c = 70, l = c(95, 90)))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(95, 75)))
pal(diverge_hcl(7, h = c(128, 330), c = 98, l = c(90, 65)))
pal(diverge_hcl(7, h = c(255, 330)))

## sequential palettes
pal(sequential_hcl(12))
pal(heat_hcl(12))
pal(heat_hcl(12, h = c(0, -100), l = c(50, 80), correct = 3))

## compare base and vcd palettes
par(mfrow = c(2, 1))
wheel(rainbow(12));wheel(rainbow_hcl(12))
pal(diverge_hcl(7));pal(diverge_hsv(7))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(95, 75)));pal(cm.colors(7))
pal(heat_hcl(12));pal(heat.colors(12))
pal(terrain_hcl(12));pal(terrain.colors(12))
}
\keyword{hplot}
