grid.assocplot <- function(x, ...)
{
  UseMethod("grid.assocplot")
}

grid.assocplot.default <- function(x, xlab = NULL, ylab = NULL, main = NULL,
                           model.formula = NULL, labels = TRUE, fontsize = 12,
			   gp = gp.signif, legend = TRUE, col.bins = NULL,
                           scale = 0.4, rot = 90, check.overlap = TRUE, axis.labels = TRUE,
			   show.grid = FALSE, panel = FALSE, margins = c(2, 4, 5, 2))
{
  require(grid)

  ## This could be used to fit more complex models:
  ## require(MASS)
  ## if(is.null(model.formula))
  ##   model.formula <- as.formula(paste("~ ", paste(names(dimnames(x)), collapse = " + "), sep = " "))
  ## x.fit <- loglm(model.formula, data = x, fit = TRUE)
  ## expctd <- fitted(x.fit)
  ## res <- residuals(x.fit, type = "pearson")


  ## independence model fitting
  rowTotals <- rowSums(x)
  colTotals <- colSums(x)
  expctd <- rowTotals %o% colTotals / sum(colTotals)
  sexpctd <- sqrt(expctd)
  res <- (x - expctd) / sexpctd
  ##res[is.na(res)] <- 0

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
  } else legend <- FALSE


  if(!panel) grid.newpage()
  if(!is.null(main)) {
    margins[3] <- margins[3] + 2
    grid.text(main, y = unit(1, "npc") - unit(2, "lines"), gp = gpar(fontsize = 1.5*fontsize))
  }
  push.viewport(plotViewport(margins))

  ## set up if legend is required
  if(legend) {
    push.viewport(viewport(layout = grid.layout(1, 2,
                  widths = unit(c(0.8, 0.2), "npc"))))
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  }

  ## the core assocplot
  push.viewport(dataViewport(xscale = c(0, sum(x.width + x.delta)),
                yscale = c(0, sum(y.height + y.delta))))

  grid.rect(x = as.vector(x.pos.matrix),
            y = as.vector(y.pos.matrix),
            width = as.vector(cell.width),
            height = as.vector(cell.height),
            default.units = "native",
            just = c("centre", "bottom"),
            gp = gp)


  grid.segments(0, unit(y.pos, "native"), 1, unit(y.pos, "native"), gp = gpar(lty = "dashed"))
  if(labels) grid.text(rev(rownames(x)), x = unit(-1, "lines"), y = unit(y.pos, "native"), rot = rot,
            gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
  if(labels) grid.text(colnames(x), y = unit(1, "npc") + unit(1, "lines"), x = unit(x.pos, "native"),
            gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
  if(axis.labels) grid.text(xlab, y = unit(1, "npc") + unit(3, "lines"), gp = gpar(fontsize = fontsize), check.overlap = check.overlap)
  if(axis.labels) grid.text(ylab, x = unit(-3, "lines"), rot = 90, gp = gpar(fontsize = fontsize), check.overlap = check.overlap)

  pop.viewport()

  ## legend drawing
  if(legend) {
    pop.viewport()
    push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    push.viewport(viewport(x = 0.6, y = 0.1, just = c("left", "bottom"),
                  yscale = range(res), default.unit = "npc",
                  height = 0.8, width = 0.2))

    if(!is.null(gp$col.legend)) {
      col.legend <- gp$col.legend
    } else {
      col.bins <- gp$col.bins
      if(is.null(col.bins)) col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
      y.pos <- col.bins[-length(col.bins)]
      y.height <- diff(col.bins)
      y.col <- gpfun(x, y.pos + 0.5*y.height)$fill
      col.legend <- list(pos = y.pos, height = y.height, col = y.col)
    }

    grid.rect(x = unit(rep(0.5, length(col.legend$pos)), "npc"), y = col.legend$pos,
              height = col.legend$height, default.unit = "native",
              gp = gpar(fill = col.legend$col, col = NULL),
              just = c("centre", "bottom"))

    #Z# lty.bins <- gp$lty.bins
    #Z# if(is.null(lty.bins))
          grid.rect()
    #Z# else {
    #Z#   y.pos <- lty.bins[-length(lty.bins)]
    #Z#   y.height <- diff(lty.bins)
    #Z#   y.lty <- gpfun(x, y.pos + 0.5*y.height)$lty
    #Z#   if(is.null(y.lty)) y.lty <- 1
    #Z#   grid.rect(x = unit(rep(0.5, length(y.pos)), "npc"), y = y.pos,
    #Z#             height = y.height, default.unit = "native",
    #Z#             gp = gpar(lty = y.lty),
    #Z#             just = c("centre", "bottom"))
    #Z# }

    grid.yaxis(main = FALSE, gp = gpar(fontsize = fontsize))
    if(!panel) grid.text("Pearson\nresiduals:", x = 0, y = unit(1, "npc") + unit(0.5, "lines"),
                gp = gpar(fontsize = 0.8*fontsize), just = c("left", "bottom"))
    if(!is.null(gp$p.value)) grid.text(paste("p-value =\n", format.pval(gp$p.value), sep = ""),
                x = 0, y = unit(0, "npc") - unit(0.5, "strheight", "A"), gp = gpar(fontsize = 0.8*fontsize),
		just = c("left", "top"))
    pop.viewport(3)
  }
  pop.viewport()

  invisible(x)
}

grid.assocplot.formula <- function(formula, data, ...)
{
  tabplot(formula, panel = function(x, ...) grid.assocplot(x, panel = TRUE, legend = FALSE, ...),
          margins = rep(1,4), data, ...)
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

gp.binary <- function(observed, residuals, col = 1:2)
{
  fill <- ifelse(residuals > 0, col[1], col[2])
  col.bins <- sort(c(range(residuals), 0))
  col.bins <- col.bins[col.bins <= max(residuals) & col.bins >= min(residuals)]

  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  y.col <- ifelse(res2 > 0, col[1], col[2])
  col.legend <- list(pos = y.pos, height = y.height, col = y.col)

  rval <- list(fill = fill, col.bins = col.bins, col.legend = col.legend)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.shading <- function(observed, residuals, hue = c(2/3, 0), col.bins = NULL,
                        col.fun = NULL, lty = 1:2, test = NULL, level = 0.95)
{
  res <- as.vector(residuals)

  ## hue and value
  hue.orig <- hue
  hue <- ifelse(res > 0, hue.orig[1], hue.orig[2])
  if(is.null(test)) {
    value <- 1
    p.value <- NULL
  } else {
    p.value <- test(observed)$p.value
    value <- (p.value < (1-level))*0.5 + 0.5
  }

  ## saturation
  if(is.null(col.fun)) {
    if(is.null(col.bins)) col.bins <- c(2, 4)
    discrete <- TRUE
    cb <- sort(col.bins)
    saturation <- rep(0, length(res)) + ifelse(abs(res) > cb[1], 0.5, 0) + ifelse(abs(res) > cb[2], 0.5, 0)
  } else {
    if(!is.null(col.bins)) stop("only one argument of `col.bins' or `col.fun' should be specified")
    discrete <- FALSE
    saturation <- pmax(pmin(col.fun(res) , 1), 0)
    col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
  }

  col <- hsv(hue, saturation, value)
  dim(col) <- dim(residuals)

  ## col.bins
  col.bins <- sort(c(range(res), col.bins, -col.bins, 0))
  col.bins <- col.bins[col.bins <= max(res) & col.bins >= min(res)]

  ## col.legend
  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  hue2 <- ifelse(res2 > 0, hue.orig[1], hue.orig[2])
  if(discrete)
    saturation2 <- rep(0, length(res2)) + ifelse(abs(res2) > cb[1], 0.5, 0) + ifelse(abs(res2) > cb[2], 0.5, 0)
  else
    saturation2 <- pmax(pmin(col.fun(res2) , 1), 0)
  y.col <- hsv(hue2, saturation2, value)
  col.legend <- list(pos = y.pos, height = y.height, col = y.col)

  ## lty and lty.bins
  lty <- ifelse(residuals > 0, lty[1], lty[2])
  lty.bins <- range(res)
  if(diff(sign(lty.bins)) > 0) lty.bins <- sort(c(0, lty.bins))
  rval <- list(fill = col, lty = lty, col.bins = col.bins, lty.bins = lty.bins, col.legend = col.legend, p.value = p.value)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.signif <- function(observed, residuals, hue = c(230, 330), lty = 1:2, level = c(0.9, 0.99))
{
  res <- as.vector(residuals)
  x.test <- pearson.test(observed, return = TRUE)
  col.bins <- cb <- x.test$qdist(sort(level))
  hue.orig <- hue

  ## hue
  hue <- ifelse(res > 0, hue.orig[1], hue.orig[2])
  ## chroma: 0 / 25 / 75
  chroma <- ifelse(abs(res) < cb[1], 0, ifelse(abs(res) <= cb[2], 25, 75))
  ## luminance: 95 / 85 / 70
  luminance <- ifelse(abs(res) < cb[1], 95, ifelse(abs(res) <= cb[2], 85, 70))

  col <- hcl(hue, chroma, luminance)
  dim(col) <- dim(residuals)

  ## col.bins
  col.bins <- sort(c(range(res), col.bins, -col.bins, 0))
  col.bins <- col.bins[col.bins <= max(res) & col.bins >= min(res)]

  ## col.legend
  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  hue2 <- ifelse(res2 > 0, hue.orig[1], hue.orig[2])
  chroma2 <- ifelse(abs(res2) < cb[1], 0, ifelse(abs(res2) <= cb[2], 25, 75))
  luminance2 <- ifelse(abs(res2) < cb[1], 95, ifelse(abs(res2) <= cb[2], 85, 70))
  y.col <- hcl(hue2, chroma2, luminance2)
  col.legend <- list(pos = y.pos, height = y.height, col = y.col)

  ## lty and lty.bins
  lty <- ifelse(residuals > 0, lty[1], lty[2])
  lty.bins <- range(res)
  if(diff(sign(lty.bins)) > 0) lty.bins <- sort(c(0, lty.bins))
  rval <- list(fill = col, lty = lty, col.bins = col.bins, lty.bins = lty.bins, col.legend = col.legend, p.value = x.test$p.value)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.data <- function(observed, residuals, hue = c(2/3, 0), lty = 1:2)
{
  res <- as.vector(residuals)
  x.test <- pearson.test(observed, return = TRUE)

  ## hue and value
  hue.orig <- hue
  hue <- ifelse(res > 0, hue.orig[1], hue.orig[2])
  value <- 1

  ## saturation
  saturation <- x.test$pdist(abs(res))

  col <- hsv(hue, saturation, value)
  dim(col) <- dim(residuals)

  ## col.bins
  col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
  col.bins <- sort(c(range(res), col.bins, -col.bins, 0))
  col.bins <- col.bins[col.bins <= max(res) & col.bins >= min(res)]

  ## col.legend
  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  hue2 <- ifelse(res2 > 0, hue.orig[1], hue.orig[2])
  saturation2 <- x.test$pdist(abs(res2))
  y.col <- hsv(hue2, saturation2, value)
  col.legend <- list(pos = y.pos, height = y.height, col = y.col)

  ## lty and lty.bins
  lty <- ifelse(residuals > 0, lty[1], lty[2])
  lty.bins <- range(res)
  if(diff(sign(lty.bins)) > 0) lty.bins <- sort(c(0, lty.bins))
  rval <- list(fill = col, lty = lty, col.bins = col.bins, lty.bins = lty.bins, col.legend = col.legend, p.value = x.test$p.value)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

tabplot <- function(x, panel = function(x, ...) grid.assocplot(x, panel = TRUE, legend = FALSE, ...),
                    margins = rep(1,4), ...)
{
  UseMethod("tabplot")
}

tabplot.formula <- function(formula, panel = function(x, ...) grid.assocplot(x, panel = TRUE, legend = FALSE, ...),
                    margins = rep(1,4), data, ...)
{
  formula <- as.character(formula)
  formula <- paste(paste(c("Freq", formula[1:2]), collapse = " "), formula[3], sep = " + ")
  formula <- gsub("\\|", "+", formula)
  formula <- gsub("\\*", "+", formula)
  x <- xtabs(as.formula(formula), data = as.data.frame(data))
  tabplot(x, panel = panel, margins = margins, ...)
}


tabplot.default <- function(x, panel = function(x, ...) grid.assocplot(x, panel = TRUE, legend = FALSE, ...),
                    margins = rep(1,4), ...)
{
  grid.newpage()

  if(length(dim(x)) <= 2) panel(x, ...) ## no conditioning variables
  else {

  condition <- dimnames(x)[-(1:2)]
  condvars <- names(condition)
  ncond <- length(condvars)
  nlevels <- sapply(condition, length)
  nplots <- prod(nlevels)
  condition <- as.matrix(sapply(expand.grid(condition), as.character))

  ## compute layout
  layout <- c(1,1,1) ## rows, cols, pages
  if(ncond == 1) {
    layout[2] <- ceiling(sqrt(floor(nlevels)))
    layout[1] <- ceiling(nlevels/layout[2])
    layout <- expand.grid(lapply(layout, function(x) 1:x))[1:nplots,]
  }
  else {
    layout[1] <- nlevels[1]
    layout[2] <- nlevels[2]
    if(ncond > 3) layout[3] <- nplots/prod(nlevels[1:2])
    if(layout[3] > 1) stop("multiple pages not supported yet")
    layout <- expand.grid(lapply(layout, function(x) 1:x))
  }

  nr <- max(layout[,1])
  nc <- max(layout[,2])
  push.viewport(plotViewport(margins))
  push.viewport(viewport(layout = grid.layout(nr, nc, widths = unit(1/nc, "npc"))))

  strUnit <- unit(2 * ncol(condition), "strheight", "A")
  cellport <- viewport(layout = grid.layout(2, 1,
        heights = unit.c(strUnit, unit(1, "npc") - strUnit)))

  for(i in 1:nrow(condition)) {
    condi <- paste(condvars, " = \"", condition[i,], "\"", sep = "", collapse = ", ")
    condistr <- paste(condvars, condition[i,], sep = " = ")

    tabi <- eval(parse(text = paste("x[,,", condi, "]", sep = "")))

    push.viewport(viewport(layout.pos.row = layout[i,1], layout.pos.col = layout[i,2]))
    push.viewport(cellport)
    push.viewport(viewport(layout.pos.row = 1))
    grid.rect(gp = gpar(fill = grey(0.9)))
    grid.text(condistr, y = ncond:1/ncond - 1/(2*ncond))
    grid.segments(0, 0:ncond/ncond, 1, 0:ncond/ncond)
    pop.viewport()

    push.viewport(viewport(layout.pos.row = 2))
    panel(tabi, ...)
    pop.viewport(2)
    grid.rect()
    pop.viewport()
  }
  pop.viewport(2)
  }

  invisible(x)
}

birthwt <- data.frame(BW = factor(c(rep("yes", 8), rep("no", 8)), levels = c("yes", "no")),
 freq = c(10,25,12,15,18,12,42,45,7,5,22,19,10,12,202,205),
 Smoking = factor(rep(c("yes", "no"), 8), levels = c("yes", "no")),
 Cardiac = factor(rep(c(rep("yes", 4), rep("no", 4)), 2), levels = c("yes", "no")),
 Comps = factor(rep(c("yes", "yes", "no", "no"), 4), levels = c("yes", "no")))
birthwt <- xtabs(freq ~ BW + Smoking + Cardiac + Comps, data = birthwt)
