.onLoad <- function(lib, pkg)
  if(!require(grid))
    stop("Can't load package grid.")
