.onLoad <- function(lib, pkg) {
  if(!("package:grid" %in% search() || require(grid))) 
    stop("could not load package grid")
  if(!("package:MASS" %in% search() || require(MASS)))
    stop("could not load package MASS")
  if(!("package:colorspace" %in% search() || require(colorspace)))
    stop("could not load package colorspace")
}
