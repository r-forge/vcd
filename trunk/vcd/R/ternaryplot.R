ternaryplot <- function (x,
                         factor = 1,
                         grid = TRUE,
                         labels = c("inside", "outside", "none"),
                         coordinates = FALSE,
                         bg = "white",
                         pch = 19,
                         col = "red",
                         main = "ternary plot",
                         ...) {
  ## some error handling
  if(ncol(x) != 3)
    stop("Need a matrix with 3 columns")
  if(any(x) < 0) stop("X must be non-negative")
  s <- rowSums(x)
  if(any(s <= 0)) stop("each row of X must have a positive sum")

  ## rescaling
  if(max(abs(s - 1)) > 1e-6) {
    warning("row(s) of X will be rescaled")
    x <- x / s
  }
  
  ## prepare plot
  top <- sqrt(3) / 2
  par(plt = c(0.06, 0.94, 0.15, 0.87))
  plot.new()
  par(usr = c(-0.03, 1.03, 0, top),
      oma = c(0, 0, 1, 0)
      )

  labels <- match.arg(labels)

  ## coordinates of point P(a,b,c): xp = b + c/2, yp = c * sqrt(3)/2

  ## triangle
  polygon(c(0, 0.5, 1), c(0, top, 0), col = bg, xpd = NA)

  ## title, labeling
  title(main, outer = TRUE, line = -1)
  axis(1, at = c(-0.03, 1.03), labels = colnames(x)[1:2], tick = FALSE, font = 2)
  axis(3, at = 0.5, labels = colnames(x)[3], tick = FALSE, font = 2)

  ## grid
  if (grid) {
    eps <- 0.01
    for (i in 1:4 * 0.2) {
      ## a - axis
      lines (c(1 - i , (1 - i) / 2), c(0, 1 - i) * top, lty = "dotted", col = "gray")
      ## b - axis
      lines (c(1 - i , 1 - i + i / 2), c(0, i) * top, lty = "dotted", col = "gray")
      ## c - axis
      lines (c(i/2, 1 - i + i/2), c(i, i) * top, lty = "dotted", col = "gray")

      ## labels
      if (labels == "inside") {
        text ((1 - i) * 3 / 4 - eps, (1 - i) / 2 * top, i * factor, col = "darkgray", srt = 120)
        text (1 - i + i / 4 + eps, i / 2 * top - eps, (1 - i) * factor, col = "darkgray", srt = -120)
        text (0.5, i * top + eps, i * factor, col = "darkgray")
      } 
      if (labels == "outside") {
        text ((1 - i) / 2 - 6 * eps, (1 - i) * top, (1 - i) * factor, col = "darkgray")
        text (1 - (1 - i) / 2 + 3 * eps, (1 - i) * top + 5 * eps, i * factor, srt = -120, col = "darkgray")
        text (i + eps, 0, (1 - i) * factor, col = "darkgray", pos = 1, offset = 1.5, srt = 120, xpd = NA)
      }
    }
  }

  ## plot points
  xp <- x[,2] + x[,3] / 2
  yp <- x[,3] * top
  points(xp, yp, pch = pch, col = col, ...)

  ## plot coordinates
  if (coordinates)
      text (xp, yp,
            paste("(",round(x[,1] * factor,1),",",
                      round(x[,2] * factor,1),",",
                      round(x[,3] * factor,1),")", sep=""), pos = 1
            )
}


