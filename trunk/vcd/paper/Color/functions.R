pal <- function(col, border = "light gray", ...)
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab="", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

hsv.wheel <-
    function(value = c(0:2, 4:6, 8:10)/10, saturation = 0:100/100, hues = 0:300/300, asp = 1,
             p.cex = 0.6, do.label = FALSE, cone = TRUE, cex = 1.5)
{
    stopifnot(is.numeric(saturation), saturation >= 0, saturation <= 1,
              is.numeric(hues), hues >= 0, hues <= 1,
              is.numeric(value), value >= 0, (nch <- length(value)) >= 1)
    if(is.unsorted(hues)) hues <- sort(hues)
    if(nch > 1) {
        op <- par(mfrow= rev(n2mfrow(nch)), mar = c(0,0,0,0))
        on.exit(par(op))
    }
    for(i.c in 1:nch) {
        plot(-1:1,-1:1, type="n", axes = FALSE, xlab="",ylab="", asp = asp)
        text(0.4, 0.99, paste("v =", format(100 * value[i.c])), adj = 0, font = 4, cex = cex)
	l.s <- if(cone) saturation * value[i.c] else saturation
        for(ang in hues) {
            a. <- ang * 2 * pi
            z.a <- exp(1i * a.)
            cols <- hsv(h = ang, s = saturation, v = value[i.c])
            points(l.s * z.a, pch = 16, col = cols, cex = p.cex)
            if(do.label) text(z.a*1.05, labels = ang, col = cols[length(cols)/2], srt = ang)
        }
        lines(exp(1i * hues * 2 * pi))
   }
   invisible()
}

hcl.wheel <-
    function(luminance = 1:9*10, chroma = 0:100, hues = 1:360, asp = 1,
             p.cex = 0.6, do.label = c(TRUE, FALSE), fixup = FALSE, inner.circle = FALSE, cex = 1.5)
{
    do.label <- rep(do.label, length.out = 2)
    stopifnot(is.numeric(chroma), chroma >= 0, chroma <= 100,
              is.numeric(hues), hues >= 0, hues <= 360,
              is.numeric(luminance), luminance >= 0, (nch <- length(luminance)) >= 1)
    if(is.unsorted(hues)) hues <- sort(hues)
    if(nch > 1) {
        op <- par(mfrow= rev(n2mfrow(nch)), mar = c(0,0,0,0))
        on.exit(par(op))
    }
    for(i.c in 1:nch) {
        plot(-1:1,-1:1, type="n", axes = FALSE, xlab="",ylab="", asp = asp)
        if(do.label[1]) text(0.4, 0.99, paste("l =", format(luminance[i.c])), adj = 0, font = 4, cex = cex)
        l.s <- chroma / 100
        for(ang in hues) { # could do all this using outer() instead of for()...
            a. <- ang * pi/180
            z.a <- exp(1i * a.)
            cols <- hcl(h = ang, c = chroma, l = luminance[i.c], fixup = fixup)
            points(l.s * z.a, pch = 16, col = cols, cex = p.cex)
            if(do.label[2]) text(z.a*1.05, labels = ang, col = cols[length(cols)/2], srt = ang)
        }
        if(!fixup) lines(exp(1i * hues * pi/180))
	if(inner.circle) {
	  dummy <- seq(-0.5, 0.5, length = 100)
	  lines(dummy, sqrt(0.25 - dummy^2), lty = 2)
  	  lines(dummy, -sqrt(0.25 - dummy^2), lty = 2)	
	}
   }
   invisible()
}

## show slices for given hues
hue.slice <- function(hue, grid.n = 251, plot = TRUE, fixup = FALSE, main = NULL)
{
  chroma = seq(0, 100, length = grid.n)
  luminance = seq(0, 100, length = grid.n)
  nc <- length(chroma)
  nl <- length(luminance)
  color.slice <- outer(chroma, luminance, function(y, x) hcl(hue, x, y, fixup = fixup))
  xlab <- "chroma"
  ylab <- "luminance"
  if(is.null(main)) main <- paste("hue =", round(hue, digits = 0))
  if(plot) {
    plot(0.5, 0.5, xlim = range(chroma), ylim = range(luminance), type = "n", axes = FALSE,
         xlab = xlab, ylab = ylab, yaxs = "i", xaxs = "i", main = main)
    for(i in 1:(nc-1)) {
      rect(chroma[i], luminance[-nl], chroma[i] + 100/(nc-1), luminance[-1], border = color.slice[,i+1], col = color.slice[,i+1])
    }
    axis(1)
    axis(2)
    box()
  }
  colnames(color.slice) <- chroma
  rownames(color.slice) <- luminance
  class(color.slice) <- "slice"
  invisible(color.slice)
}
