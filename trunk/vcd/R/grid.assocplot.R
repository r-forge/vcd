grid.assocplot <- function(x, ...)
{
  UseMethod("grid.assocplot")
}

grid.assocplot.default <- function(x, xlab = NULL, ylab = NULL, main = NULL,
                           model.formula = NULL, labels = TRUE, fontsize = 12,
                           gp = gp.max, legend = TRUE,
                           scale = 0.4, rot = 90, check.overlap = TRUE, axis.labels = TRUE,
                           panel = FALSE, margins = c(1, 1, 1, 1))
{
  require(grid) #Z# require globally?
  if (length(dim(x)) != 2)
      stop("x must be a 2-d contingency table")
  if (any(x < 0) || any(is.na(x)))
      stop("all entries of x must be nonnegative and finite")

  ## independence model fitting
  rowTotals <- rowSums(x)
  colTotals <- colSums(x)
  expctd <- rowTotals %o% colTotals / sum(colTotals)
  sexpctd <- sqrt(expctd)
  res <- (x - expctd) / sexpctd

  if(is.null(ylab)) ylab <- names(dimnames(x))[1]
  if(is.null(xlab)) xlab <- names(dimnames(x))[2]

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

  if(is.function(gp)) {
    gpfun <- gp
    gp <- gpfun(x, res)
  } else {
    if(is.null(gp$legend.col)) legend <- FALSE
  }


  if(!panel) grid.newpage()

  ## compute margins and space for labels
  if(!is.null(main)) margins[3] <- margins[3] + 3

  h <- w <- unit(1, "npc")
  if(labels) {
    h <- h - unit(2 + 2*(1-panel), "lines")
    w <- w - unit(2 + 2*(1-panel), "lines")
  }
  if(axis.labels) {
    h <- h - unit(2, "lines")
    w <- w - unit(2, "lines")
  }

  pushViewport(plotViewport(margins))
  if(!is.null(main))
    grid.text(main, y = unit(1, "npc") + unit(1, "lines"), gp = gpar(fontsize = 1.5 * fontsize))

  ## set up if legend is required
  if(legend) {
    pushViewport(viewport(layout = grid.layout(1, 2,
                  widths = unit(c(0.85, 0.15), "npc"))))
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  }

  ## the core assocplot
  pushViewport(viewport(x = unit(1, "npc") - unit(2*(1-panel), "lines"), y = unit(2*(1-panel), "lines"), xscale = c(0, sum(x.width + x.delta)),
                yscale = c(0, sum(y.height + y.delta)),
                height = h, width = w, just = c("right", "bottom")))

  grid.rect(x = as.vector(x.pos.matrix),
            y = as.vector(y.pos.matrix),
            width = as.vector(cell.width),
            height = as.vector(cell.height),
            default.units = "native",
            just = c("centre", "bottom"),
            gp = gp)


  grid.segments(0, unit(y.pos, "native"), 1, unit(y.pos, "native"), gp = gpar(lty = "dashed"))
  if(labels) {
    grid.text(rev(rownames(x)), x = unit(0, "native") - unit(1, "lines"), y = unit(y.pos, "native"), rot = rot,
              gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
    grid.text(colnames(x), y = unit(sum(y.height + y.delta), "native") + unit(1, "lines"), x = unit(x.pos, "native"),
              gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
  }
  if(axis.labels) {
    grid.text(ylab, x = unit(0, "native") - unit(3, "lines"), rot = 90,
              gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
    grid.text(xlab, y = unit(sum(y.height + y.delta), "native") + unit(3, "lines"),
              gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
  }

  popViewport()

  ## legend drawing
  if(legend) {
    popViewport()
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    pushViewport(viewport(x = unit(1, "npc") - unit(2, "lines"), y = unit(2 + 2*(1-panel), "lines"), just = c("right", "bottom"),
                  yscale = range(res), default.unit = "npc",
                  height = h - unit(4, "lines"),
		  width = unit(0.75, "npc") - unit(2, "lines")))

    if(!is.null(gp$legend.col)) {
      legend.col <- gp$legend.col
    } else {
      col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
      y.pos <- col.bins[-length(col.bins)]
      y.height <- diff(col.bins)
      y.col <- gpfun(x, y.pos + 0.5*y.height)$fill
      legend.col <- list(pos = y.pos, height = y.height, col = y.col)
    }

    grid.rect(x = unit(rep(0.5, length(legend.col$pos)), "npc"), y = legend.col$pos,
              height = legend.col$height, default.unit = "native",
              gp = gpar(fill = legend.col$col, col = NULL),
              just = c("centre", "bottom"))

    grid.yaxis(main = FALSE, gp = gpar(fontsize = fontsize))
    grid.rect()
    #Z# grid.yaxis(main = FALSE, gp = gpar(fontsize = fontsize, lty = 2))
    #Z# if(!is.null(gp$legend.lty)) {
    #Z#   legend.lty <- gp$legend.lty
    #Z#   y.pos <- legend.lty$pos
    #Z#   grid.rect(x = unit(rep(0.5, length(legend.lty$pos)), "npc"), y = legend.lty$pos,
    #Z#             height = legend.lty$height, default.unit = "native",
    #Z#             gp = gpar(lty = legend.lty$lty), just = c("centre", "bottom"))
    #Z# }

    if(!panel) grid.text("Pearson\nresiduals:", x = 0, y = unit(1, "npc") + unit(0.5, "lines"),
                gp = gpar(fontsize = 0.8*fontsize), just = c("left", "bottom"))
    if(!is.null(gp$p.value)) grid.text(paste("p-value:\n", format.pval(gp$p.value), sep = ""),
                x = 0, y = unit(0, "npc") - unit(0.5, "strheight", "A"), gp = gpar(fontsize = 0.8*fontsize),
                just = c("left", "top"))
    popViewport(3)
  }
  popViewport()

  invisible(x)
}

grid.assocplot.formula <- function(formula, data, ...)
{
  tabplot(formula, panel = function(x, ...) grid.assocplot(x, panel = TRUE, legend = FALSE, ...),
          margins = rep(1,4), data, ...)
}

assocpairs <- function(x, margin = 1, omargin = 2, legend = FALSE, ...)
{
  require(grid)
  grid.newpage()
  vars <- names(dimnames(x))
  n <- length(vars)
  pushViewport(plotViewport(rep(omargin, length.out = 4)))
  pushViewport(viewport(layout = grid.layout(n, n)))
  for(i in 1:n) {
    for(j in 1:n) {
      pushViewport(viewport(layout.pos.row = i, layout.pos.col = j))
      if(i==j) { grid.text(vars[i], gp = gpar(fontsize = 20)) }
      else {
        y <- margin.table(x, c(i, j))
	grid.assocplot(y, panel = TRUE, legend = legend, margin = margin, xlab = "", ylab = "", axis.labels = FALSE, ...)
      }
      grid.rect()
      popViewport()
    }
  }
  popViewport(2)
  invisible(x)
}

