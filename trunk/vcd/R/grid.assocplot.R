grid.assocplot <- function(x, xlab = NULL, ylab = NULL, main = NULL,
                           model.formula = NULL, transpose = TRUE, labels = TRUE,
			   color.type = "Friendly", legend = TRUE,
                           scale = 0.4, margin = 5, rot = 90, check.overlap = TRUE)
{
  require(MASS)

  if(is.null(xlab)) xlab <- names(dimnames(x))[1]
  if(is.null(ylab)) ylab <- names(dimnames(x))[2]

  if(is.null(model.formula))
    model.formula <- as.formula(paste("~ ", paste(names(dimnames(x)), collapse = " + "), sep = " "))

  x.fit <- loglm(model.formula, data = x, fit = TRUE)
  expctd <- as.matrix(fitted(x.fit)) ## Should transposing really be done??
  x <- as.matrix(x)
  sexpctd <- sqrt(expctd)
  res <- as.matrix(residuals(x.fit, type = "pearson"))

  ## Transposing - why??
  if(transpose)
  {
    expctd <- t(expctd)
    x <- t(x)
    sexpctd <- t(sexpctd)
    res <- t(res)
  }

  cols <- colorscheme(res, type = color.type)

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
    if(labels) {
      if(cl == 1) grid.text(names(y.height)[rw], y = unit(0, "native"),
        x = unit(-x.delta/2, "native") + unit(-1.2, "lines"), rot = rot,
	check.overlap = check.overlap)
      if(rw == nrows) grid.text(names(x.width)[cl], x = unit(0.5, "npc"),
        y = unit(y.min[rw] - y.delta/2, "native") + unit(-1.2, "lines"),
	check.overlap = check.overlap)
    }
    ## grid.rect(gp = gpar(lty = "dashed", col = "light grey"))
    pop.viewport()
  }

  grid.newpage()

  if(legend) {
    push.viewport(viewport(layout = grid.layout(1, 2,
                  widths = unit(c(0.85, 0.15), "npc"))))
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  }

  push.viewport(viewport(x = unit(margin, "lines"),
                y = unit(margin, "lines"),
  	        width = unit(1, "npc") - unit(1.5*margin, "lines"),
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

  grid.text(xlab, y = unit(1, "lines"), check.overlap = check.overlap)
  grid.text(ylab, x = unit(1, "lines"), rot = rot, check.overlap = check.overlap)
  grid.text(main, y = unit(1, "npc") - unit(2, "lines"), gp = gpar(fontsize = 20))

  if(legend) {
    pop.viewport()
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    res.range <- range(res)
    push.viewport(viewport(x = 0.1, y = 0.2, just = c("left", "bottom"),
                  yscale = range(res), default.unit = "npc",
                  height = 0.6, width = 0.6))
    colbins <- 100
    for(i in 1:colbins) {
      yval <- res.range[1] + diff(res.range) * ((i-1)/colbins)
      ycol <- colorscheme(yval, type = color.type)
      grid.rect(y = unit(yval, "native"), height = unit(1/colbins, "npc"),
                gp = gpar(fill = ycol, col = ycol),
		just = c("centre", "bottom"))
    }
    grid.rect()
    grid.yaxis()
    grid.text("Pearson\nresiduals:", x = 0.1, y = unit(1, "npc") + unit(0.5, "lines"),
              gp = gpar(fontsize = 10), just = c("left", "bottom"))
    pop.viewport()
    pop.viewport()
  }

  invisible(x)
}

colorscheme <- function(residuals, type = c("Friendly", "Shading", "CI"))
{
    type <- match.arg(type)
    switch(type,

    "Friendly" = {
      color <- residuals
      color[color > 0] <- "black"
      color[color <= 0] <- "red"
    },

    "Shading" = {
      colorvec <- c(hsv(0, s = seq(1, to = 0, length = 3)),
                    hsv(2/3, s = seq(0, to = 1, length = 3)))
      color <- residuals
      color[residuals > 4] <- colorvec[6]
      color[residuals <= -4] <- colorvec[1]
      color[residuals > -4 & residuals <= -2] <- colorvec[2]
      color[residuals > -2 & residuals <= 2] <- colorvec[3]
      color[residuals > 2 & residuals <= 4] <- colorvec[5]
    },

    "CI" = {
      stop("sorry, not yet implemented")
    })

    return(color)
}
