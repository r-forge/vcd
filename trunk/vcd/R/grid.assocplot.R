grid.assocplot <- function(x, scale = 0.4, margin = 5)
{
  xlab <- names(dimnames(x))[2]
  ylab <- names(dimnames(x))[1]

  expctd <- expected(x)
  x <- as.matrix(x)
  sexpctd <- sqrt(expctd)
  res <- (x - expctd)/sexpctd

  cols <- res
  cols[cols>0] <- "black" ## or better color schemes
  cols[cols<=0] <- "red"

  x.width <- apply(sexpctd, 2, max)
  y.min <- apply(res, 1, min)
  y.max <- apply(res, 1, max)
  y.height <- y.max - y.min

  x.delta <- mean(x.width) * scale  ## better with grid unit()
  y.delta <- mean(y.height) * scale

  ncols <- ncol(x)
  nrows <- nrow(x)

  acellplot <- function(rw, cl)
  {
    push.viewport(viewport(layout.pos.row = rw, layout.pos.col = cl,
      xscale = c(-x.delta/2, x.width[cl] + x.delta/2),
      yscale = c(y.min[rw]-y.delta/2, y.max[rw]+y.delta/2),
      default.units = "native",
      just = c("left", "bottom")))
    grid.rect(y = 0, width = sexpctd[rw,cl],
              height = res[rw, cl],
              default.units = "native",
  	      just = c("centre", "bottom"),
  	      gp = gpar(fill = cols[rw,cl]))
    grid.move.to(-x.delta/2, 0, default.units = "native")
    grid.line.to(x.width[cl] + x.delta/2, 0,
                 default.units = "native", gp = gpar(lty = "dashed"))
    if(cl == 1) grid.text(names(y.height)[rw], y = unit(0, "native"),
                          x = unit(-x.delta/2, "native") + unit(-1.2, "lines"), rot = 90)
    if(rw == nrows) grid.text(names(x.width)[cl], x = unit(0.5, "npc"),
                          y = unit(y.min[rw] - y.delta/2, "native") + unit(-1.2, "lines"))
    ## grid.rect(gp = gpar(lty = "dashed", col = "light grey"))
    pop.viewport()
  }

  grid.newpage()
  push.viewport(viewport(x = unit(margin, "lines"),
                y = unit(margin, "lines"),
  	        width = unit(1, "npc") - unit(2*margin, "lines"),
  	        height = unit(1, "npc") - unit(2*margin, "lines"),
                xscale = c(0, sum(x.width + x.delta)),
                yscale = c(0, sum(y.height + y.delta)),
  	        just = c("left", "bottom"),
  	        layout = grid.layout(nrows, ncols,
  	          heights = y.height + y.delta,
                  widths =  x.width + x.delta)))

  for(i in 1:nrows) {
    for(j in 1:ncols) {
      acellplot(i,j)
    }
  }
  ## grid.rect(gp = gpar(lty = "dashed", col = "light grey"))

  pop.viewport()
  grid.text(names(dimnames(x))[2], y = unit(1, "lines"))
  grid.text(ylab, x = unit(1, "lines"), rot = 90)
}

