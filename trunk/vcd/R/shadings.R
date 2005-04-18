## vcdShading functions should take at least the arguments
##   observed, residuals, expected
## and return a function which takes a single argument (interpreted
## to be a vector of residuals).

gp.HSVshading <- function(observed, residuals = NULL, expected = NULL, df = NULL,
                          hue = c(2/3, 0), saturation = c(1, 0), value = c(1, 0.5),
                          interpolate = c(2, 4), lty = 1,
                          p.value = NULL, level = 0.95)
{
  ## get h/s/v and lty
  my.h <- rep(hue, length.out = 2)	  ## positive and negative hue
  my.s <- rep(saturation, length.out = 2) ## maximum and minimum saturation
  my.v <- rep(value, length.out = 2)	  ## significant and non-significant value
  lty <- rep(lty, length.out = 2)	  ## positive and negative lty

  ## model fitting (if necessary)
  if(is.null(expected) && !is.null(residuals)) stop("residuals without expected values specified")
  if(!is.null(expected) && is.null(df) && is.null(p.value)) {
    warning("no default inference available without degrees of freedom")
    p.value <- NA
  }
  if(is.null(expected) && !is.null(observed)) {
    expected <- loglin(observed, 1:length(dim(observed)), fit = TRUE, print = FALSE)
    df <- expected$df
    expected <- expected$df
  }
  if(is.null(residuals) && !is.null(observed)) residuals <- (observed - expected)/sqrt(expected)
    
  ## conduct significance test (if specified)
  if(is.null(p.value)) p.value <- function(observed, residuals, expected, df)
    pchisq(sum(as.vector(residuals)^2), df, lower.tail = FALSE)
  if(!is.function(p.value) && is.na(p.value)) {
    value <- my.v[1]
    p.value <- NULL
  } else {
    if(is.function(p.value)) p.value <- p.value(observed, residuals, expected, df)
    value <- if(p.value < (1-level)) my.v[1] else my.v[2]
  }

  ## set up function for interpolation of saturation
  if(!is.function(interpolate)) {
    col.bins <- sort(interpolate)
    interpolate <- stepfun(col.bins,  seq(my.s[2], my.s[1], length = length(col.bins) + 1))
    col.bins <- sort(unique(c(col.bins, 0, -col.bins)))
  } else {
    col.bins <- NULL
  }

  ## store color and lty information for legend
  if(!is.null(col.bins)) {
    res2 <- col.bins
    res2 <- c(head(res2, 1) - 1, res2[-1] - diff(res2)/2, tail(res2, 1) + 1)
    legend.col <- hsv(ifelse(res2 > 0, my.h[1], my.h[2]),
                      pmax(pmin(interpolate(abs(res2)), 1), 0),
		      value)
    lty.bins <- 0
    legend.lty <- lty[2:1]
    legend <- list(col = legend.col, col.bins = col.bins,
                   lty = legend.lty, lty.bins = lty.bins)
  }

  ## set up function that computes color/lty from residuals
  rval <- function(x) {
    res <- as.vector(x)

    col <- hsv(ifelse(res > 0, my.h[1], my.h[2]),
               pmax(pmin(interpolate(abs(res)), 1), 0),
	       value)
    dim(col) <- dim(x)
    
    lty <- ifelse(x > 0, lty[1], lty[2])    
    dim(lty) <- dim(x)

    return(structure(list(fill = col, lty = lty), class = "gpar"))
  }
  attr(rval, "legend") <- legend
  attr(rval, "p.value") <- p.value
  return(rval)
}
class(gp.HSVshading) <- "vcdShading"


gp.HCLshading <- function(observed, residuals = NULL, expected = NULL, df = NULL,
                          hue = c(260, 0), chroma = c(100, 20), luminance = c(90, 50),
                          gamma = 2.2, fixup = TRUE, interpolate = c(2, 4), lty = 1,
                          p.value = NULL, level = 0.95)

{
  ## get h/c/l and lty
  my.h <- rep(hue, length.out = 2)       ## positive and negative hue
  my.c <- rep(chroma, length.out = 2)    ## significant and non-significant maximum chroma
  my.l <- rep(luminance, length.out = 2) ## maximum and minimum luminance
  lty <- rep(lty, length.out = 2)        ## positive and negative lty

  ## model fitting (if necessary)
  if(is.null(expected) && !is.null(residuals)) stop("residuals without expected values specified")
  if(!is.null(expected) && is.null(df) && is.null(p.value)) {
    warning("no default inference available without degrees of freedom")
    p.value <- NA
  }
  if(is.null(expected) && !is.null(observed)) {
    expected <- loglin(observed, 1:length(dim(observed)), fit = TRUE, print = FALSE)
    df <- expected$df
    expected <- expected$df
  }
  if(is.null(residuals) && !is.null(observed)) residuals <- (observed - expected)/sqrt(expected)
    
  ## conduct significance test (if specified)
  if(is.null(p.value)) p.value <- function(observed, residuals, expected, df)
    pchisq(sum(as.vector(residuals)^2), df, lower.tail = FALSE)
  if(!is.function(p.value) && is.na(p.value)) {
    max.chroma <- my.c[1]
    p.value <- NULL
  } else {
    if(is.function(p.value)) p.value <- p.value(observed, residuals, expected, df)
    max.chroma <- ifelse(p.value < (1-level), my.c[1], my.c[2])
  }

  ## set up function for interpolation of saturation
  if(!is.function(interpolate)) {
    col.bins <- sort(interpolate)
    interpolate <- stepfun(col.bins,  seq(0, 1, length = length(col.bins) + 1))
    col.bins <- sort(unique(c(col.bins, 0, -col.bins)))
  } else {
    col.bins <- NULL
  }

  ## store color and lty information for legend
  if(!is.null(col.bins)) {
    res2 <- col.bins
    res2 <- c(head(res2, 1) - 1, res2[-1] - diff(res2)/2, tail(res2, 1) + 1)
    legend.col <- hcl(ifelse(res2 > 0, my.h[1], my.h[2]),
                      max.chroma * pmax(pmin(interpolate(abs(res2)), 1), 0),
	              my.l[1] + diff(my.l) * pmax(pmin(interpolate(abs(res2)), 1), 0),
		      gamma = gamma, fixup = fixup)
    lty.bins <- 0
    legend.lty <- lty[2:1]
    legend <- list(col = legend.col, col.bins = col.bins,
                   lty = legend.lty, lty.bins = lty.bins)
  }

  ## set up function that computes color/lty from residuals
  rval <- function(x) {
    res <- as.vector(x)

    col <- hcl(ifelse(res > 0, my.h[1], my.h[2]),
               max.chroma * pmax(pmin(interpolate(abs(res)), 1), 0),
	       my.l[1] + diff(my.l) * pmax(pmin(interpolate(abs(res)), 1), 0),
	       gamma = gamma, fixup = fixup)
    dim(col) <- dim(x)
    
    lty <- ifelse(x > 0, lty[1], lty[2])    
    dim(lty) <- dim(x)

    return(structure(list(fill = col, lty = lty), class = "gpar"))
  }
  attr(rval, "legend") <- legend
  attr(rval, "p.value") <- p.value
  return(rval)
}
class(gp.HCLshading) <- "vcdShading"


gp.Friendly <- function(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
                        hue = c(2/3, 0), lty = 1:2, interpolate = c(2, 4))
  gp.HSVshading(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
                hue = hue, value = 1,
                lty = lty, interpolate = interpolate, p.value = NA)
class(gp.Friendly) <- "vcdShading"

gp.max <- function(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
                   hue = c(260, 0), chroma = c(100, 20), luminance = c(90, 50), 
                   interpolate = c(2, 4), lty = 1, level = c(0.9, 0.99), n = 1000)
{
  stopifnot(length(dim(observed)) == 2)
  obs.test <- pearson.test(observed, n = n, return.distribution = TRUE)
  col.bins <- obs.test$qdist(sort(level))
  rval <- gp.HCLshading(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
                        hue = hue, chroma = chroma, luminance = luminance,
                        interpolate = col.bins, lty = lty,
			p.value = obs.test$p.value)
  return(rval)
}
class(gp.max) <- "vcdShading"

gp.binary <- function(observed = NULL, residuals = NULL, expected = NULL, df = NULL,
                      col = hcl(c(260, 0), 100, 50))

{
  ## get col
  my.col <- rep(col, length.out = 2)
  
  ## store color information for legend
  legend <- list(col = my.col[2:1], col.bins = 0,
                 lty = NULL, lty.bins = NULL)

  ## set up function that computes color/lty from residuals
  rval <- function(x) {
    res <- as.vector(x)
    col <- ifelse(res > 0, my.col[1], my.col[2])
    dim(col) <- dim(x)    
    return(structure(list(fill = col), class = "gpar"))
  }
  attr(rval, "legend") <- legend
  attr(rval, "p.value") <- NULL
  return(rval)
}
class(gp.HCLshading) <- "vcdShading"

## gp.Z <- gp.HCLshading(hue = c(130, 30), chroma = c(80, 20), luminance = c(95, 70), lty = 1)

rainbowHCL <- function(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n,
  gamma = 2.2, fixup = TRUE)
{
  if(n > 0) hcl(seq(start, end, length = n), c = c, l = l, gamma = gamma, fixup = fixup)
    else character(0)
}

cmHCL <- function(n, h = c(260, 0), c = 100, l = c(90, 50), gamma = 2.2, fixup = TRUE)
{
  h <- rep(h, length.out = 2)
  c <- c[1]
  l <- rep(l, length.out = 2)
  rval <- seq(-1, 1, length = n)
  rval <- hcl(h = ifelse(rval > 0, h[1], h[2]),
              c = c * abs(rval),
              l = l[1] + diff(l) * abs(rval),
              gamma = gamma, fixup = fixup)
  if(n > 0) return(rval) else return(character(0))	      
}
