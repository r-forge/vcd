## HCL

hue_chroma_plane <- function(h = 0:359, c = 0:100, l = 70, fixup = FALSE,
  axes = TRUE, at.c = 0:3 * 25, at.h = 0:6 * 60, main = NULL, inner = NULL, ...)
{
  if(is.null(main)) main <- paste("luminance =", l)
  plot(0, 0, type = "n", axes = FALSE, xlab = "",ylab = "", main = main,
    xlim = c(-100, 100), ylim = c(-100, 100), asp = 1)
  xpos <- function(h, c) cos(h * pi/180) * c
  ypos <- function(h, c) sin(h * pi/180) * c

  for(hue in h) points(xpos(hue, c), ypos(hue, c), 
    col = hcl(h = hue, c = c, l = l, fixup = fixup), pch = 20, ...)
  lines(xpos(0:360, 100), ypos(0:360, 100))
  if(axes) {
    lines(c(0, 100), c(0, 0))
    text(at.c, rep(-7, length(at.c)), at.c)
    text(50, -14, "chroma")
    rect(at.c, 0, at.c, -3)
    if(0 %in% at.h | 360 %in% at.h) {
      lines(xpos(0, c(100, 103)), ypos(0, c(100, 103)))
      text(xpos(0, 107), ypos(0, 107), 0, pos = 3)
      text(xpos(0, 107), ypos(0, 107), 360, pos = 1)
      text(xpos(0, 116), ypos(0, 116), "hue")
    }
    at.h <- at.h[at.h > 0 & at.h < 360]
    for(hue in at.h) {
      text(xpos(hue, 107), ypos(hue, 107), hue)
      lines(xpos(hue, c(100, 103)), ypos(hue, c(100, 103)))
    }
  }
  if(!is.null(inner)) lines(xpos(0:360, inner), ypos(0:360, inner), lty = 2)
}

hue_chroma_point <- function(h = 0, c = 50, l = 70, fixup = FALSE, size = 2)
{
  xpos <- cos(h * pi/180) * c
  ypos <- sin(h * pi/180) * c
  rect(xpos - size, ypos - size, xpos + size, ypos + size,
   col = hcl(h = h, c = c, l = l, fixup = fixup))
}

chroma_luminance_plane <- function(h = 0, c = 0:200/2, l = 0:200/2, fixup = FALSE,
  axes = TRUE, main = NULL, ...)
{
  if(is.null(main)) main <- paste("hue =", h)
  if(axes) {
    xlab <- "chroma"
    ylab <- "luminance"
  } else {
     xlab <- ""
     ylab <- ""
  }
  plot(0, 0, type = "n", xlab = xlab,ylab = ylab, main = main,
    xaxs = "i", yaxs = "i", xlim = range(c), ylim = range(l), axes = axes)
  for(chroma in c) points(rep(chroma, length(l)), l,
    col = hcl(h = h, c = chroma, l = l, fixup = fixup), pch = 20, ...)
  box()
}

chroma_luminance_point <- function(h = 0, c = 50, l = 70, fixup = FALSE, size = 2)
{
  rect(c - size, l - size, c + size, l + size,
    col = hcl(h = h, c = c, l = l, fixup = fixup))
}

## HSV

my_hsv <- function(h = 0, s = 100, v = 100, cylinder = FALSE, ...)
{
  h <- h/360
  s <- s/100
  v <- v/100
  if(cylinder) {
    rval <- hsv(h, s, v, ...)
  } else {
    s <- s1 <- s/v
    s[is.nan(s)] <- s1[is.nan(s)] <- 0
    s1[s > 1] <- 1
    rval <- hsv(h, s1, v, ...)
    rval[s > 1] <- NA
  }
  return(rval)
  
}

hue_saturation_plane <- function(h = 0:359, s = 0:100, v = 100, cylinder = FALSE,
  axes = TRUE, at.s = 0:3 * 25, at.h = 0:6 * 60, main = NULL, inner = NULL, ...)
{
  if(is.null(main)) main <- paste("value =", v)
  plot(0, 0, type = "n", axes = FALSE, xlab = "",ylab = "", main = main,
    xlim = c(-100, 100), ylim = c(-100, 100), asp = 1)
  xpos <- function(h, s) cos(h * pi/180) * s
  ypos <- function(h, s) sin(h * pi/180) * s

  for(hue in h) points(xpos(hue, s), ypos(hue, s), 
    col = my_hsv(h = hue, s = s, v = v, cylinder = cylinder), pch = 20, ...)
  lines(xpos(0:360, 100), ypos(0:360, 100))
  if(axes) {
    lines(c(0, 100), c(0, 0))
    text(at.s, rep(-7, length(at.s)), at.s)
    rect(at.s, 0, at.s, -3)
    text(50, -14, "saturation")
    if(0 %in% at.h | 360 %in% at.h) {
      lines(xpos(0, c(100, 103)), ypos(0, c(100, 103)))
      text(xpos(0, 107), ypos(0, 107), 0, pos = 3)
      text(xpos(0, 107), ypos(0, 107), 360, pos = 1)    
      text(xpos(0, 116), ypos(0, 116), "hue")
    }
    at.h <- at.h[at.h > 0 & at.h < 360]
    for(hue in at.h) {
      text(xpos(hue, 107), ypos(hue, 107), hue)
      lines(xpos(hue, c(100, 103)), ypos(hue, c(100, 103)))
    }
  }
  if(!is.null(inner)) lines(xpos(0:360, inner), ypos(0:360, inner), lty = 2)
}

hue_saturation_point <- function(h = 0, s = 100, v = 100, cylinder = FALSE, size = 2)
{
  xpos <- cos(h * pi/180) * c
  ypos <- sin(h * pi/180) * c
  rect(xpos - size, ypos - size, xpos + size, ypos + size,
   col = my_hsv(h = h, s = s, v = v, cylinder = cylinder))
}

saturation_value_plane <- function(h = 0, s = 0:200/2, v = 0:200/2, cylinder = FALSE,
  axes = TRUE, main = NULL, ...)
{
  if(is.null(main)) main <- paste("hue =", h)
  if(axes) {
    xlab <- "saturation"
    ylab <- "value"
  } else {
     xlab <- ""
     ylab <- ""
  }
  plot(0, 0, type = "n", xlab = xlab,ylab = ylab, main = main,
    xaxs = "i", yaxs = "i", xlim = range(s), ylim = range(v), axes = axes)
  for(saturation in s) points(rep(saturation, length(v)), v,
    col = my_hsv(h = h, s = saturation, v = v, cylinder = cylinder), pch = 20, ...)
  box()
}

saturation_value_point <- function(h = 0, s = 100, v = 100, cylinder = FALSE, size = 2)
{
  rect(s - size, v - size, s + size, v + size,
    col = my_hsv(h = h, s = s, v = v, cylinder = cylinder))
}

