grid.assocplot <- function(x, ...)
{
  UseMethod("grid.assocplot")
}

grid.assocplot.default <- function(x, xlab = NULL, ylab = NULL, main = NULL,
                           model.formula = NULL, transpose = FALSE, labels = TRUE,
			   color.type = "Friendly", legend = TRUE, colbins = NULL,
                           scale = 0.4, margin = 5, rot = 90, check.overlap = TRUE,
			   show.grid = FALSE, panel = FALSE)
{
  require(MASS)
  require(grid)

  if(is.null(dimnames(x))) dimnames(x) <- list(rep("", ncol(x)), rep("", nrow(x)))
  if(is.null(names(dimnames(x)))) {
    names(dimnames(x)) <- c("Y", "X")
    if(is.null(xlab)) xlab <- ""
    if(is.null(ylab)) ylab <- ""
  }

  ## independence model fitting
  if(is.null(model.formula))
    model.formula <- as.formula(paste("~ ", paste(names(dimnames(x)), collapse = " + "), sep = " "))
  x.fit <- loglm(model.formula, data = x, fit = TRUE)
  expctd <- fitted(x.fit)
  x <- x
  sexpctd <- sqrt(expctd)
  res <- residuals(x.fit, type = "pearson")

  ## Transposing - why??
  if(transpose)
  {
    expctd <- t(expctd)
    x <- t(x)
    sexpctd <- t(sexpctd)
    res <- t(res)
  }

  if(is.null(xlab)) xlab <- names(dimnames(x))[2]
  if(is.null(ylab)) ylab <- names(dimnames(x))[1]

  ncols <- ncol(x)
  nrows <- nrow(x)
  x.width <- apply(sexpctd, 2, max)
  y.min <- apply(res, 1, min)
  y.max <- apply(res, 1, max)
  y.height <- y.max - y.min
  x.delta <- mean(x.width) * scale  ## better with grid unit()
  y.delta <- mean(y.height) * scale

  x.pos <- cumsum(x.width + x.delta) - 0.5 * (x.width + x.delta)
  y.pos <- cumsum(rev(y.height) + y.delta) - rev(y.max) - y.delta/2
  x.pos.matrix <- matrix(rep(x.pos, nrows), nrow = nrows, byrow = TRUE)
  y.pos.matrix <- matrix(rep(rev(y.pos), ncols), ncol = ncols)
  cell.height <- res
  cell.width <- sexpctd
  cell.col <- colorscheme(res, type = color.type)

  if(!panel) grid.newpage()

  ## set up if legend is required
  if(legend) {
    push.viewport(viewport(layout = grid.layout(1, 2,
                  widths = unit(c(0.85, 0.15), "npc"))))
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  }

  ## the core assocplot
  push.viewport(viewport(x = unit(margin, "lines"),
                y = unit(margin, "lines"),
  	        width = unit(1, "npc") - unit(1.5*margin, "lines"),
  	        height = unit(1, "npc") - unit(2*margin, "lines"),
                xscale = c(0, sum(x.width + x.delta)),
                yscale = c(0, sum(y.height + y.delta)),
  	        just = c("left", "bottom")))

  grid.rect(x = as.vector(x.pos.matrix),
            y = as.vector(y.pos.matrix),
            width = as.vector(cell.width),
            height = as.vector(cell.height),
            default.units = "native",
            just = c("centre", "bottom"),
            gp = gpar(fill = as.vector(cell.col)))


  grid.segments(0, unit(y.pos, "native"), 1, unit(y.pos, "native"), gp = gpar(lty = "dashed"))
  if(labels) grid.text(rev(rownames(x)), x = unit(-1, "lines"), y = unit(y.pos, "native"), rot = rot,
            check.overlap = check.overlap)
  if(labels) grid.text(colnames(x), y = unit(-1, "lines"), x = unit(x.pos, "native"),
            check.overlap = check.overlap)

  pop.viewport()
  grid.text(xlab, y = unit(1, "lines"), check.overlap = check.overlap)
  grid.text(ylab, x = unit(1, "lines"), rot = rot, check.overlap = check.overlap)
  grid.text(main, y = unit(1, "npc") - unit(2, "lines"), gp = gpar(fontsize = 20))

  ## legend drawing
  if(legend) {
    pop.viewport()
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    push.viewport(viewport(x = 0.1, y = 0.2, just = c("left", "bottom"),
                  yscale = range(res), default.unit = "npc",
                  height = 0.6, width = 0.6))

    if(is.null(colbins)) {
      if(color.type == "Friendly") colbins <- c(min(res), 0, max(res))
      if(color.type == "Shading") {
        colbins <- c(min(res), -4, -2, 2, 4, max(res))
	colbins <- colbins[colbins >= min(res) & colbins <= max(res)]
      }
      if(color.type == "CI") colbins <- 200
    }
    if(length(colbins) == 1) {
      colbins <- floor(colbins) - 1
      colbins <- min(res) + diff(range(res)) * ((0:colbins)/colbins)
    }

    y.pos <- colbins[-length(colbins)]
    y.height <- diff(colbins)
    y.col <- colorscheme(y.pos + 0.5*y.height, type = color.type)

    grid.rect(x = unit(rep(0.5, length(y.pos)), "npc"), y = y.pos,
              height = y.height, default.unit = "native",
              gp = gpar(fill = y.col, col = y.col),
              just = c("centre", "bottom"))

    grid.rect()
    grid.yaxis()
    grid.text("Pearson\nresiduals:", x = 0.1, y = unit(1, "npc") + unit(0.5, "lines"),
              gp = gpar(fontsize = 10), just = c("left", "bottom"))
    pop.viewport(2)
  }

  invisible(x)
}

grid.assocplot.formula <- function(formula, data = list(), ...)
{
  x <- ftable(formula, data = data)
  rownames(x) <- attr(x, "row.vars")[[1]]
  colnames(x) <- attr(x, "col.vars")[[1]]
  names(dimnames(x)) <- c(names(attr(x, "row.vars")), names(attr(x, "col.vars")))
  class(x) <- NULL
  grid.assocplot(t(x), ...)
}

assocpairs <- function(x, margin = 2, ...)
{
  require(grid)
  grid.newpage()
  vars <- names(dimnames(x))
  n <- length(vars)
  push.viewport(plotViewport(rep(margin, 4)))
  push.viewport(viewport(layout = grid.layout(n, n)))
  for(i in 1:n) {
    for(j in 1:n) {
      push.viewport(viewport(layout.pos.row = i, layout.pos.col = j))
      if(i==j) { grid.text(vars[i], gp = gpar(fontsize = 20)) }
      else {
        y <- margin.table(x, c(i, j))
	grid.assocplot(y, panel = TRUE, legend = FALSE, margin = margin, xlab = "", ylab = "", ...)
      }
      grid.rect()
      pop.viewport()
    }
  }
  pop.viewport(2)
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
      color <- residuals
      critval <- 4
      residuals[residuals > critval] <- critval
      residuals[residuals < -critval] <- -critval
      color[residuals > 0] <- hsv(h = 2/3, s = residuals[residuals > 0]/critval, v = 1)
      color[residuals <= 0] <- hsv(h = 0, s = -residuals[residuals <= 0]/critval, v = 1)
    })

    return(color)
}

