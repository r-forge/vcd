assocplot <- function(x,
                      ## parameters for extended plot
                      color = NULL,
                      shade = TRUE,
                      test  = c("Chisq", "none"),
                      conf.level = 0.95,
                      density = 20,
                      ## layout parameters
                      main = NULL,
                      space = 0.3,
                      xlab = NULL,
                      ylab = NULL,
                      cex.axis = 0.66,
                      ...)
{
    if (length(dim(x)) != 2)
        stop("x must be a 2-d contingency table")
    if (any(x < 0) || any(is.na(x)))
        stop("all entries of x must be nonnegative and finite")
    if ((n <- sum(x)) == 0)
        stop("at least one entry of x must be positive")
    if (is.null(color)) {
      color <- if (is.logical(shade) && !shade)
        c("blue", "red")
      else
        c(0, 2 / 3)
    } else if (length(color) != 2)
      stop("incorrect color specification")

    test <- if (!is.logical(test))
      match.arg(test)
    else
      if (test) "Chisq" else "none"
    if(test == "Chisq") require(ctest)

    f <- x[ , rev(1:NCOL(x))]           # rename for convenience;
                                        # f is observed freqs
                                        # reverse to be consistent with
                                        # mosaicplot().
    e <- outer(rowSums(f), colSums(f), "*") / n
                                        # e is expected freqs
    d <- (f - e) / sqrt(e)              # Pearson residuals
    e <- sqrt(e)
    x.w <- apply(e, 1, max)             # the widths of the x columns
    y.h <- apply(d, 2, max) - apply(d, 2, min)
                                        # the heights of the y rows
    x.delta <- mean(x.w) * space
    y.delta <- mean(y.h) * space
    ylim <- c(0, sum(y.h) + NCOL(f) * y.delta)
    yscale <- sum(y.h) + NCOL(f) * y.delta

    ## Shading (taken from `mosaicplot').
    if (is.logical(shade) && shade) shade <- c(2, 4)
    if (shade) {
      if (any(shade <= 0) || length(shade) > 5)
        stop("invalid shade specification")
      shade <- sort(shade)
      breaks <- c(-Inf, - rev(shade), 0, shade, Inf)
      color <- c(hsv(color[1], 
                     s = seq(1, to = 0, length = length(shade) + 1)),
                 hsv(color[2],   
                     s = seq(0, to = 1, length = length(shade) + 1)))
      if (test == "none" || chisq.test(x)$p.value <= 1 - conf.level)
          density <- NULL
      
      ## This code is extremely ugly, and certainly can be improved.
      ## In the case of extended displays, we also need to provide a
      ## legend for the shading and outline patterns.  The code works
      ## o.k. with integer breaks in `shade'; rounding to two 2 digits
      ## will not be good enough if `shade' has length 5.
      pin <- par("pin")
      rtxt <- "Standardized\nResiduals:"
      ## Compute cex so that the rotated legend text does not take up
      ## more than 1/12 of the of the plot region horizontally and not
      ## more than 1/4 vertically.
      rtxtWidth <- 0.1                # unconditionally ..
      ## We put the legend to the right of the third axis.

      xlim <- c(0, (sum(x.w) + NROW(f) * x.delta) * (1.05 + rtxtWidth))
      offs <- (sum(x.w) + NROW(f) * x.delta) * 1.05
      plot.new()
      plot.window(xlim, ylim, log = "")
      rtxtCex <- min(1,
                     pin[1] / (strheight(rtxt, units = "inches") * 12),
                     pin[2] / (strwidth (rtxt, units = "inches") / 4))
      rtxtHeight <-
        strwidth(rtxt, units = "i", cex = rtxtCex) / pin[2]
      text(offs * (1 + 0.5 * rtxtWidth), 0, labels = rtxt,
           adj = c(0, 0.25), srt = 90, cex = rtxtCex)
      ## `len' is the number of positive or negative intervals of
      ## residuals (so overall, there are `2 * len')
      len <- length(shade) + 1
      ## `bh' is the height of each box in the legend (including the
      ## separating whitespace
      bh <- 0.95 * (0.95 - rtxtHeight) / (2 * len)
      x.l <- offs
      x.r <- offs * (1 + 0.7 * rtxtWidth)
      y.t <- rev(seq(from = 0.95, by = - bh, length = 2 * len))
      y.b <- y.t - 0.8 * bh
      for(i in 1 : (2 * len)) {
        polygon(c(x.l, x.r, x.r, x.l),
                c(y.b[i], y.b[i], y.t[i], y.t[i]) * yscale,
                col = color[i],
                border = "black")
        if (!is.null(density))
          polygon(c(x.l, x.r, x.r, x.l),
                  c(y.b[i], y.b[i], y.t[i], y.t[i]) * yscale,
                  col = "white",
                  border = "black",
                  density = density)
      }
      brks <- round(breaks, 2)
      y.m <- y.b + 0.4 * bh
      text(offs * (1 + rtxtWidth), y.m * yscale,
           c(paste("<", brks[2], sep = ""),
             paste(brks[2 : (2 * len - 1)],
                   brks[3 : (2 * len)],
                   sep = ":"),
             paste(">", brks[2 * len], sep = "")),
           srt = 90, cex = cex.axis)
    } else {
      xlim <- c(0, sum(x.w) + NROW(f) * x.delta)
      plot.new()
      plot.window(xlim, ylim, log = "")
      density <- NULL
    }

    x.r <- cumsum(x.w + x.delta)
    x.m <- (c(0, x.r[-NROW(f)]) + x.r) / 2
    y.u <- cumsum(y.h + y.delta)
    y.m <- y.u - apply(pmax(d, 0), 2, max) - y.delta / 2
    z <- expand.grid(x.m, y.m)
    rect(as.vector(z[, 1] - e / 2), as.vector(z[, 2]),
         as.vector(z[, 1] + e / 2), as.vector(z[, 2] + d),
         col = { if (shade)
             color[as.numeric(cut(d, breaks))]
         else
             color[1 + (d < 0)]},
         border = if (shade && !is.null(density)) "black",
         ...
         )
    if(!is.null(density))
      rect(as.vector(z[, 1] - e / 2), as.vector(z[, 2]),
           as.vector(z[, 1] + e / 2), as.vector(z[, 2] + d),
           col = "white",
           density = density,
           border = "black",
           ...
           )
    
    axis(1, at = x.m, labels = rownames(f), tick = FALSE)
    axis(2, at = y.m, labels = colnames(f), tick = FALSE)
    if (!shade)
      abline(h = y.m, lty = 2)
    else
      for (i in 1:length(y.m))
        lines(c(0, offs / 1.05), c(y.m[i], y.m[i]), lty = 2)
    ndn <- names(dimnames(f))
    if(length(ndn) == 2) {
        if(is.null(xlab))
            xlab <- ndn[1]
        if(is.null(ylab))
            ylab <- ndn[2]
    }
    title(main = main, xlab = xlab, ylab = ylab)
  }
