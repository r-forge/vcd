## changes:
##   - readbndfile() changed to read.bnd()
##     this returns a suitable object (of class "bnd")
##     and does *not* yet assign it to some variable
##   - added quiet argument (as in scan())
##
##   - added print() and range() method
##
##   - added plot() method (not unsimilar to drawmap())
##   - changed interface to be similar to plot()
##     support standard graphical parameters (xlim, xlab, main, ...)
##     removed specialized color code (replaced by col argument)
##     removed specialized x11() and postscript() code

## comments:
##  - better than grey(0:(n-1)/(n-1)) is grey.colors(n)
##    which (by default) stays away from the extreme colors
##    black and white and uses a gamma-correction
##  - T, F should be better TRUE, FALSE
##  - don't use if(foo == TRUE), just say if(foo)
##  - don't use return(invisible()) just invisible() is enough


read.bnd <- function(file, quiet = TRUE)
{
        owarn <- getOption("warn")
	options(warn = -1)

	data.raw <- scan(file, what = list("", ""), sep = ",", quote = "", quiet = quiet)
	data.numeric <- list(as.numeric(data.raw[[1]]), as.numeric(data.raw[[2]]))

	npolygon <- sum(is.na(data.numeric[[1]])) - sum(data.raw[[1]] == "is.in")
	if(!quiet) cat("Note: map consists of", npolygon, "polygons\n")
	if(!quiet) cat("Reading map ...\n")

	map <- list()
	i <- 1

	for(k in 1:npolygon) {
		j <- 1
		npoints <- data.numeric[[2]][i]
		if(is.na(data.numeric[[1]][i + 1]) && is.na(data.numeric[[2]][i + 1])) {
			npoints <- npoints + 1
		}
		elem1 <- data.numeric[[1]][i+1:npoints]
		elem2 <- data.numeric[[2]][i+1:npoints]
		map[[k]] <- matrix(c(elem1, elem2), ncol = 2)
		names(map)[k] <- substring(data.raw[[1]][i], 2, nchar(data.raw[[1]][i]) - 1)
		i <- i + npoints + 1
	}

	if(sum(is.na(as.numeric(names(map)))) == 0) {
		map <- map[order(as.numeric(names(map)))]
		if(!quiet) cat("Note: regions sorted by number\n")
	} else {
		map <- map[order(names(map))]
		if(!quiet) cat("Note: regions sorted by name\n")
	}

 	if(!quiet) cat("Note: map consists of ", length(unique(names(map))),"regions\n")
	options(warn = owarn)

	class(map) <- "bnd"
	return(map)
}

print.bnd <- function(x, ...) {
    cat("A map object of class", dQuote("bnd"), "consisting of", length(x),"regions.\n")
    invisible(x)
}

range.bnd <- function(x, ..., na.rm = FALSE) {
  cbind(range(sapply(x, function(z) range(z[,1], na.rm = na.rm)), na.rm = na.rm),
        range(sapply(x, function(z) range(z[,2], na.rm = na.rm)), na.rm = na.rm))
}

plot.bnd <- function(x, z = NULL,
    xlim = NULL, ylim = NULL, xlab = "", ylab = "", main = "", lwd = 0.3, col = 1, fill = NULL,
    zlim = NULL, legend = NULL, labels = FALSE, cex.text = 0.8,
    ...)
{
    ## compute map ranges
    xrange <- range(x, na.rm = TRUE)
    if(is.null(xlim)) xlim <- xrange[,1]
    if(is.null(ylim)) ylim <- xrange[,2]

    ## set up shading (if any)
    if(!is.null(z)) {
      stopifnot(length(z) == length(x))
      if(is.null(zlim)) zlim <- range(z, na.rm = TRUE)
      
      ## cut for shading
      if(is.null(fill)) fill <- rev(gray.colors(100))
      zbreaks <- seq(from = zlim[1], to = zlim[2], length.out = length(fill))
      zcut <- as.numeric(cut(z, breaks = zbreaks, include.lowest = TRUE))
      zfill <- fill[zcut]
      
      if(is.null(legend)) legend <- TRUE
    } else {
    ## otherwise use simple light gray shading      
      if(is.null(fill)) fill <- "lightgray"
      zfill <- fill[1]
      zcut <- seq(along = x)
      legend <- FALSE
    }
    zfill <- rep(zfill, length.out = length(x))

    ## set up plot
    plot(0, 0, type = "n", axes = FALSE, xlim = xlim, ylim = ylim,
      xlab = xlab, ylab = ylab, main = main)

    ## draw the map (= shaded polygons)
    for(i in seq(along = x))
        polygon(x[[i]][, 1], x[[i]][, 2], lwd = lwd, col = zfill[i], border = col, ...)

    ## add labels
    if(labels) {
        #FIXME# This corresponds to the old approach:
        #FIXME# text(sapply(x, function(z) mean(range(z[,1], na.rm = TRUE))),
	#FIXME#      sapply(x, function(z) mean(range(z[,2], na.rm = TRUE))),
	#FIXME#      names(x), cex = cex.text)
        #FIXME# does not handle non-convex polygons well, maybe try this:
	text(sapply(x, function(z) mean(z[,1], na.rm = TRUE)),
	     sapply(x, function(z) mean(z[,2], na.rm = TRUE)),
	     names(x), cex = cex.text)
	#FIXME# somewhat better, still not perfect, though...
    }

    ## add legend
    if(!identical(legend, FALSE)) {
        ## legend should either be logical
	## or a pair of coordinates
        if(identical(legend, TRUE)) {
	  legend <- cbind(xrange[2,1] - c(0.4 * diff(xrange[,1]), 0),
                          xrange[1,2] + c(0.02, 0.06) * diff(xrange[,2]))
	}
        incr <- seq(0, 1, length.out = length(fill)+1) * diff(legend[,1])
	
	## draw legend	
	for(i in seq(along = fill)) {
	  rect(legend[1,1] + incr[i], legend[1,2], legend[1,1] + incr[i+1], legend[2,2],
	       col = fill[i], border = "transparent")
	}
	## legend axes
        rect(legend[1,1], legend[1,2], legend[2,1], legend[2,2], col = "transparent", border = "black")
	text(legend[1,1], legend[1,2], round(zlim[1], digits = 3), pos = 1, cex = cex.text)
	text(legend[2,1], legend[1,2], round(zlim[2], digits = 3), pos = 1, cex = cex.text)
	if(zlim[1] < 0 && zlim[2] > 0) text(legend[1,1] + diff(legend[,1]) * abs(zlim[1])/diff(zlim),
	    legend[1,2], "0", pos = 1, cex = cex.text)
    }
    
    invisible(x)
}

