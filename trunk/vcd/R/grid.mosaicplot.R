panel.barplot <- function(x, color = "blue", fontsize = 20, dimnames, ...) {
  push.viewport(viewport(x = 0.3, y = 0.1, width = 0.7, height = 0.7,
                         yscale = c(0,max(x)), just = c("left", "bottom"))
                )
  xpos <- seq(0, 1, length = length(x) + 1)[-1]
  halfstep <- (xpos[2] - xpos[1]) / 2
  grid.rect(xpos - halfstep, rep(0, length(x)), height = x,
            just = c("centre", "bottom"), width = halfstep,
            gp = gpar(fill = color), default = "native", ...)
  grid.yaxis(at = pretty(c(0,max(x))))
  grid.text(names(x), y = unit(-0.15, "npc"),
            x = xpos - halfstep, just = c("center", "bottom"))
  pop.viewport(1)
  grid.text(names(dimnames(x)), y = 1, just = c("center", "top"),
            gp = gpar(fontsize = fontsize))

}

panel.mosaicplot <- function(x, i, j, type, legend = FALSE, axes = TRUE,
                             margins = c(0, 0, 0, 0), abbreviate = FALSE,
                             gp = gp.Friendly, ...) {
  index <- 1:length(dim(x))
  rest <- index[!index %in% c(i, j)]
  grid.mosaicplot(x = margin.table(x,
                    if (type == "pairwise") c(i, j) else c(i, j, rest)),
                  margin = switch(type,
                    pairwise =, total = NULL,
                    conditional = list(c(i, rest), c(j, rest)),
                    joint = list(c(i, j), rest)
                    ),
                  panel = TRUE, main = NULL, labels = FALSE,
                  legend = legend, axes = axes, margins = margins,
                  abbreviate = abbreviate, gp = gp, permute = FALSE, ...)
}

panel.text <- function(x, fontsize = 20, dimnames = TRUE, ...) {
  grid.rect()
  grid.text(names(dimnames(x)), gp = gpar(fontsize = fontsize),
            y = 0.5 + dimnames * 0.05, ...)
  if (dimnames)
    grid.text(paste("(",paste(names(x), collapse = ","), ")", sep = ""), y = 0.4)
}

grid.mosaicpairs <- function(x, main = NULL,
                             panel.upper = panel.mosaicplot,
                             panel.lower = panel.mosaicplot,
                             panel.diag = panel.text,
                             type = c("pairwise", "total", "conditional", "joint"),
                             type.upper = NULL,
                             type.lower = NULL,
                             newpage = TRUE,

                             space = 0.1,
                             legend = FALSE,
                             axes = FALSE,
                             abbreviate = FALSE,
                             margins = c(2, 2, 1, 2),
                             panel.margins = c(0, 0, 0, 0),
                             diag.fontsize = 20,
                             diag.dimnames = TRUE,
                             gp = gp.Friendly,
                             permute = TRUE,
                             ...)
{
  require(grid)
  if (newpage) grid.newpage()
  main
  type.upper <- if (is.null(type.upper))
    match.arg(type)
  else
    match.arg(type.upper, type)
  type.lower <- if (is.null(type.lower))
    match.arg(type)
  else
    match.arg(type.lower, type)
  if (permute) x <- aperm(x)

  d <- length(dim(x))
  l <- grid.layout(d, d)
  push.viewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
  push.viewport(viewport(layout = l,
                         height = if (is.null(main)) 1 else 0.9,
                         y = 0, just = "bottom"))
  grid.text(main, y = unit(1.05, "npc"), gp = gpar(fontsize = 20))


  for (i in 1:d)
    for(j in 1:d) {
      push.viewport(viewport(layout.pos.col = i, layout.pos.row = j))
      push.viewport(viewport(width = 1 - space, height = 1 - space))

      if (i > j)
        panel.upper(x, i, j, type.upper, legend = legend, axes = axes,
                    margins = panel.margins, abbreviate = abbreviate, gp = gp, ...)
      else if (i < j)
        panel.lower(x, i, j, type.lower, legend = legend, axes = axes,
                    margins = panel.margins, abbreviate = abbreviate, gp = gp, ...)
      else
        panel.diag(margin.table(x, i), fontsize = diag.fontsize,
                   dimnames = diag.dimnames, ...)

      pop.viewport(2)
    }
  pop.viewport(2)
  invisible(x)
}

grid.mosaicplot <- function(x, ...)
  UseMethod("grid.mosaicplot")

grid.mosaicplot.formula <-
function(formula, data = NULL, ...,
         main = deparse(substitute(data)), subset)
{
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        dat <- as.table(data)
        varnames <- attr(terms(formula), "term.labels")
        if(all(varnames != ".")) {
            ind <- match(varnames, names(dimnames(dat)))
            if (any(is.na(ind)))
              stop(paste("Can't find", paste(varnames[is.na(ind)], collapse=" / "), "in", deparse(substitute(data))))
            dat <- margin.table(dat, ind)
          }
        grid.mosaicplot(dat, main = main, ...)
    }
    else {
        if(is.matrix(edata))
            m$data <- as.data.frame(data)
        m$... <- NULL
        m[[1]] <- as.name("model.frame")
        mf <- eval(m, parent.frame())
        grid.mosaicplot(table(mf), main = main, ...)
    }
}

legend.block <- function(res,
                         gp = gp.Friendly,
                         fontsize = 12,
                         panel = FALSE,
                         space = 2,        #Z# was: from left in "native", now: from right in "lines"
                         text = "Pearson\nresiduals:") {

  push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  push.viewport(viewport(x = unit(1, "native") - unit(space, "lines"), y = 0.1, just = c("right", "bottom"),
                         yscale = range(res), default.unit = "npc",
                         height = 0.75, width = 0.2))

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

  grid.rect()

  grid.yaxis(main = FALSE, gp = gpar(fontsize = fontsize))
  if (!panel)
    grid.text(text, x = 0, y = unit(1, "npc") + unit(0.5, "lines"),
              gp = gpar(fontsize = 0.8 * fontsize),
              just = c("left", "bottom")
              )
  if(!is.null(gp$p.value))
    grid.text(paste("p-value =\n", format.pval(gp$p.value), sep = ""),
              x = 0, y = unit(0, "npc") - unit(0.5, "strheight", "A"),
              gp = gpar(fontsize = 0.8*fontsize),
              just = c("left", "top"))

 pop.viewport(2)
}

grid.mosaicplot.default <-
  function(x,
           main = NULL,
           labels = TRUE,
           axes = TRUE,
           abbreviate = FALSE,

           direction = c("horizontal", "vertical"),

           margin = NULL,
           residuals = NULL,
           type = c("Pearson", "deviance", "FT"),
           freq.type = c("observed", "expected"),  #Z# only if model...
           shade = TRUE,
           legend = TRUE,
           gp = gp.Friendly,
           fontsize = 12,

           margins = c(1, 1, 1, 1),
           space = 0.1,
           panel = FALSE,
           permute = TRUE
           )
{
  require(grid)

  ## parameter handling
  if (is.null(dim(x)))
    x <- as.array(x)
  else if (is.data.frame(x))
    x <- data.matrix(x)
  if (any(x < 0) || any(is.na(x)))
    stop("all entries of x must be nonnegative and finite")
  if (sum(x) == 0)
    stop("at least one entry of x must be positive")
  direction <- match.arg(direction)
  type <- match.arg(type)
  if (permute) x <- aperm(x)
  if(length(axes) == 1) axes <- rep(axes, 4)
  if(length(labels) == 1) labels <- rep(labels, 4)
  if(length(abbreviate) == 1) abbreviate <- rep(abbreviate, 4)
  maxdim <- length(d <- dim(x))
  for (i in 1:maxdim)
    if (abbreviate[i])
      dimnames(x)[[i]] <- substr(dimnames(x)[[i]], 1, abbreviate[i])


  ## title
  if (!panel) grid.newpage()
  if(!is.null(main)) margins[3] <- margins[3] + 3

  #Z# push.viewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
  #Z# needed for what?

  push.viewport(plotViewport(margins))
  if (!is.null(main))
    grid.text(main,
              y = unit(1, "npc") + unit(1, "lines"),
              gp = gpar(fontsize = 1.5 * fontsize))

  ## shading
  if (shade) {
    ## residuals
    if(is.null(residuals)) {
      if (inherits(margin, "formula")) {
        require(MASS)
        E <- fitted(loglm(margin, x, fitted = TRUE))
      } else {
        if (is.null(margin))
          margin <- as.list(1:maxdim)
        E <- loglin(x, margin, fit = TRUE, print = FALSE)$fit
      }
      residuals <- switch(type,
                          Pearson = (x - E) / sqrt(E),
                          deviance = {
                            tmp <- 2 * (x * log(ifelse(x == 0, 1, x/E)) - (x - E))
                            tmp <- sqrt(pmax(tmp, 0))
                            ifelse(x > E, tmp, -tmp)
                          },
                          FT = sqrt(x) + sqrt(x + 1) - sqrt(4 * E + 1))
      if(freq.type == "expected") x <- E #Z# caution: this is not not very clever
    }

    ## colors and legend
    if(is.function(gp)) {
      gpfun <- gp
      gp <- gpfun(x, residuals)
    } else {
      if(is.null(gp$legend.col)) legend <- FALSE
    }
    #Z# used to be
    #Z# if(is.function(gp))
    #Z#   gp <- gp(x, residuals)

    #Z# duplicated from below
    h <- unit(1, "npc")
    if(any(labels)) {
      h <- h - unit(2 + 2*(1-panel), "lines") #Z# see also below
    }
    if(any(axes)) {
      h <- h - unit(2, "lines")
    }


    if (legend && !is.null(residuals)) {
      push.viewport(viewport(layout = grid.layout(1, 2,
                               widths = unit(c(0.85, 0.15), "npc"))))
      #Z# legend.block(residuals, gp, fontsize, panel, 2, "standardized\nresiduals:")
      #Z# difficult to still use legend.block...
      push.viewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
      push.viewport(viewport(x = unit(1, "npc") - unit(2, "lines"), y = unit(2 + 2*(1-panel), "lines"), just = c("right", "bottom"),
                    yscale = range(residuals), default.unit = "npc",
                    height = h - unit(4, "lines"),
                    width = unit(0.75, "npc") - unit(2, "lines")))

      if(!is.null(gp$legend.col)) {
        legend.col <- gp$legend.col
      } else {
        col.bins <- min(residuals) + diff(range(residuals)) * ((0:200)/200)
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

      if(!panel) grid.text(paste(type, "\nresiduals:", sep = ""), x = 0, y = unit(1, "npc") + unit(0.5, "lines"),
                  gp = gpar(fontsize = 0.8*fontsize), just = c("left", "bottom"))
      if(!is.null(gp$p.value)) grid.text(paste("p-value:\n", format.pval(gp$p.value), sep = ""),
                  x = 0, y = unit(0, "npc") - unit(0.5, "strheight", "A"), gp = gpar(fontsize = 0.8*fontsize),
                  just = c("left", "top"))
      pop.viewport(2)

    }
  } else {
    legend <- FALSE
    gp <- NULL
  }

  ## compute unit shifts for labels
  rsh <- unit(1, "npc") + unit((1 + any(axes)) / 2, "lines")
  lsh <- unit(0, "npc") - unit((1 + any(axes)) / 2, "lines")

  ## draw tiles
  cc <- 0
  lab <- list()
  ## workhorse
  split <- function(table, v, x0, y0, w, h, index = c()) {
    ## compute relative tile sizes and positions
    m <- apply(table, 1, sum)
    m[m == 0] <- 0.000001
    m <- (1 - space) * m / sum(m)
    l <- length(m)
    sp <- space / (l - 1)
    pos <- c(0, cumsum(m + sp)[-l])

    ## compute absolute sizes / coordinates
    coord <- if (v)
      cbind(x0, y0 - pos * h, w, m * h)
    else
      cbind(x0 + pos * w, y0, m * w, h)

    ## labels
    dim <- length(index) + 1

## store all coordinates in the `lab' list and print all at the end
## to prevent overlapping text. Not really a neat solution.

    if (direction == "vertical") {
      if (dim == 1 && axes[1])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[1]]),
                                            x = I(rep(list(lsh), l)),
                                            y = coord[,2] - coord[,4] / 2,
                                            rot = 90)
      else if (dim == 2 && index[1] == 1 && axes[2])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[2]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = I(rep(list(rsh), l)),
                                            rot = 0)
      else if (dim == 3 && index[2] == d[2] && axes[3])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[3]]),
                                            x = I(rep(list(rsh), l)),
                                            y = coord[,2] - coord[,4] / 2,
                                            rot = -90)
      else if (dim == 4 && index[3] == d[3] && index[1] == d[1] && axes[4])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[4]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = I(rep(list(lsh), l)),
                                            rot = 0)
    } else {
      if (dim == 1 && axes[1])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[1]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = I(rep(list(rsh), l)),
                                            rot = 0)
      else if (dim == 2 && index[1] == 1 && axes[2])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[2]]),
                                            x = I(rep(list(lsh), l)),
                                            y = coord[,2] - coord[,4] / 2,
                                            rot = 90)
      else if (dim == 3 && index[2] == d[2] && axes[3])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[3]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = I(rep(list(lsh), l)),
                                            rot = 0)
      else if (dim == 4 && index[3] == d[3] && index[1] == d[1] && axes[4])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[4]]),
                                            x = I(rep(list(rsh), l)),
                                            y = coord[,2] - coord[,4] / 2,
                                            rot = -90)
    }

    ## repeat recursively for all subtiles, and return coordinate matrix
    if (dim < maxdim) {
      ind <- slice.index(table, 1)
      do.call("rbind",
              lapply(1:l, function(i)
                     split(array(table[ind == i], d[-(1:dim)]), !v,
                           coord[i, 1], coord[i, 2], coord[i, 3], coord[i, 4],
                           c(index, i)
                           )
                     )
              )
    } else cbind(coord,
                 if (dim > 1) matrix(index, ncol = dim - 1, nrow = l, byrow = TRUE),
                 1:l)
  }

  ## set margins
  push.viewport(viewport(layout.pos.col = 1))

  w <- unit(1, "npc")
  h <- unit(1, "npc")
  if(any(labels)) {
    w <- w - unit(2 + 2*(1-panel), "lines") #Z# discuss this!
    h <- h - unit(2 + 2*(1-panel), "lines") #Z# see also below
  }
  if(any(axes)) {
    w <- w - unit(2, "lines")
    h <- h - unit(2, "lines")
  }



  #Z# push.viewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
  #Z# better adjustment of viewport instead of
  #Z# push.viewport(viewport(width = w, height = h))
  push.viewport(viewport(x = unit(1, "npc") - unit(2*(1-panel), "lines"), y = unit(2*(1-panel), "lines"),
                height = h, width = w, just = c("right", "bottom")))

  ## compute coordinates
  coordinates <- split(x, v = direction == "vertical", 0, 1, 1, 1)

  ## draw tiles
  grid.rect(coordinates[,1], coordinates[,2], coordinates[,3], coordinates[,4],
            just = c("left", "top"),
            gp = do.call("gpar", lapply(gp, function(x) if(is.array(x)) aperm(x) else x))   #Z# too complicated?
            )

  ## draw labels
  if (any(axes)) {
    lab = do.call("rbind", lab)
    grid.text(lab[,1],
              x = do.call("unit.c", lapply(lab[,2],
                function(x) if(!is.unit(x)) unit(x, "npc") else x)),
              y = do.call("unit.c", lapply(lab[,3],
                function(x) if(!is.unit(x)) unit(x, "npc") else x)),
              rot = lab[,4],
              check.overlap = TRUE,
              gp = gpar(fontsize = 0.8 * fontsize))
  }

  ## draw dim labels
  rsh <- unit(1, "npc") + unit(2 + any(axes), "lines")
  lsh <- unit(-2 - any(axes), "lines")
  if (direction == "vertical") {
    if (labels[1])
      grid.text(names(dimnames(x))[1],
                y = 0.5, x = lsh, gp = gpar(fontsize = fontsize), rot = 90)

    if (maxdim > 1 && labels[2])
      grid.text(names(dimnames(x))[2],
                x = 0.5, y = rsh, gp = gpar(fontsize = fontsize), rot = 0)

    if (maxdim > 2 && labels[3])
      grid.text(names(dimnames(x))[3],
                y = 0.5, x = rsh, gp = gpar(fontsize = fontsize), rot = -90)

    if (maxdim > 3 && labels[4])
      grid.text(names(dimnames(x))[4],
                x = 0.5, y = lsh, gp = gpar(fontsize = fontsize), rot = 0)
  } else {
    if (labels[1])
      grid.text(names(dimnames(x))[1],
                x = 0.5, y = rsh, gp = gpar(fontsize = fontsize))

    if (maxdim > 1 && labels[2])
      grid.text(names(dimnames(x))[2],
                y = 0.5, x = lsh, gp = gpar(fontsize = fontsize), rot = 90)

    if (maxdim > 2 && labels[3])
      grid.text(names(dimnames(x))[3],
                x = 0.5, y = lsh, gp = gpar(fontsize = fontsize))

    if (maxdim > 3 && labels[4])
      grid.text(names(dimnames(x))[4],
                y = 0.5, x = rsh, gp = gpar(fontsize = fontsize), rot = -90)
  }

  ## clean up and return
  #Z# pop.viewport(5 + legend)
      pop.viewport(3 + legend)
  invisible(x)
}

