panel.barplot <- function(x, color = "blue", fontsize = 20, ...) {
  push.viewport(viewport(x = 0.2, y = 0.1, width = 0.7, height = 0.7,
                         yscale = c(0,max(x)), just = c("left", "bottom"))
                )
  xpos <- seq(0, 1, length = length(x) + 1)[-1]
  halfstep <- (xpos[2] - xpos[1]) / 2
  grid.rect(xpos - halfstep, rep(0, length(x)), height = x,
            just = c("centre", "bottom"), width = halfstep,
            gp = gpar(fill = color), default = "native", ...)
  grid.yaxis(at = pretty(c(0,max(x))))
  grid.text(names(x), y = unit(-0.1, "npc"),
            x = xpos - halfstep, just = c("center", "bottom"))
  pop.viewport(1)
  grid.text(names(dimnames(x)), y = 0.95, just = c("center", "top"),
            gp = gpar(fontsize = fontsize))
  
}

panel.mosaicplot <- function(x, i, j, type, legend = FALSE, axes = TRUE,
                             margins = c(0, 0, 0, 0), test = TRUE, level = 0.95,
                             abbreviate = FALSE, ...) {
  index <- 1:length(dim(x))
  rest <- index[!index %in% c(i, j)]
  grid.mosaicplot(x = switch(type,
                         pairwise = margin.table(x, c(i, j)),
                         total = margin.table(x, c(i, j, rest)),
                         conditional = margin.table(x, c(i, j, rest)),
                         joint = margin.table(x, c(i, j, rest))
                         ),
                  margin = switch(type,
                    pairwise = NULL,
                    total = NULL,
                    conditional = list(c(i, rest), c(j, rest)),
                    joint = list(c(i, j), rest)
                    ),
                  panel = TRUE, main = NULL, labels = FALSE,
                  legend = legend, axes = axes, margins = margins,
                  test = test, level = level, abbreviate = abbreviate, ...)
}

panel.text <- function(x, fontsize = 20, dimnames = TRUE, ...) {
  grid.rect()
  grid.text(names(dimnames(x)), gp = gpar(fontsize = fontsize),
            y = 0.5 + dimnames * 0.05, ...)
  if (dimnames)
    grid.text(paste("(",paste(names(x), collapse = ","), ")", sep = ""),
              y = 0.4)
}

grid.mosaicpairs <- function(x, main = deparse(substitute(x)),
                             panel.upper = panel.mosaicplot,
                             panel.lower = panel.mosaicplot,
                             panel.diag = panel.text,
                             type = c("pairwise", "total", "conditional", "joint"),
                             type.upper = NULL,
                             type.lower = NULL,
                             
                             test = TRUE,
                             level = 0.95,
                             space = 0.1,
                             legend = FALSE,
                             axes = FALSE,
                             abbreviate = FALSE,
                             margins = c(2, 2, 1, 2),
                             panel.margins = c(0, 0, 0, 0),
                             diag.fontsize = 20,
                             diag.dimnames = TRUE,
                             ...)
{
  grid.newpage()
  main
  type.upper <- if (is.null(type.upper))
    match.arg(type)
  else
    match.arg(type.upper, type)
  type.lower <- if (is.null(type.lower))
    match.arg(type)
  else
    match.arg(type.lower, type)
  
  d <- length(dim(x))
  l <- grid.layout(d, d)
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
                    margins = panel.margins, test = test, level = level,
                    abbreviate = abbreviate, ...)
      else if (i < j)
        panel.lower(x, i, j, type.lower, legend = legend, axes = axes,
                    margins = panel.margins, test = test, level = level,
                    abbreviate = abbreviate, ...)
      else 
        panel.diag(margin.table(x, i), fontsize = diag.fontsize,
                   dimnames = diag.dimnames, ...)

      pop.viewport(2)
    }
  pop.viewport(1)
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

grid.mosaicplot.default <-
  function(x,
           main = deparse(substitute(x)),
           labels = TRUE,
           axes = TRUE,
           abbreviate = FALSE,
           
           direction = c("horizontal", "vertical"),
           
           margin = NULL,
           residuals = NULL,
           type = c("pearson", "deviance", "FT"),
           shade = TRUE,
           legend = TRUE,
           granularity = 100,
           color = vcd.Friendly,

           margins = c(1, 1, 1, 1),
           space = 0.1,
           panel = FALSE,
           permute = FALSE,
           prange = c(-6,6),
           test = TRUE,
           level = 0.95
           )
{
  require(grid)

  ## parameter handling
  main
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
  if(length(axes) == 1)
    axes <- rep(axes, 4)
  if(length(labels) == 1)
    labels <- rep(labels, 4)
  if(length(abbreviate) == 1)
    abbreviate <- rep(abbreviate, 4)
  maxdim <- length(d <- dim(x))
  for (i in 1:maxdim)
    if (abbreviate[i])
      dimnames(x)[[i]] <- substr(dimnames(x)[[i]], 1, abbreviate[i])
  
  ## title
  if (!panel) grid.newpage()
  if(!is.null(main))
    grid.text(main, y = unit(0.95, "npc"),
              gp = gpar(fontsize = 20))

  if (!is.null(main)) margins[3] <- margins[3] + 3
  push.viewport(plotViewport(margins))

  ## residuals
  if(shade && is.null(residuals)) {
    if (inherits(margin, "formula")) {
      require(MASS)
      E <- fitted(loglm(margin, x, fitted = TRUE))
    } else {
      if (is.null(margin)) 
        margin <- as.list(1:maxdim)
      E <- loglin(x, margin, fit = TRUE, print = FALSE)$fit
    }
    residuals <- switch(type,
                        pearson = (x - E)/sqrt(E), 
                        deviance = {
                          tmp <- 2 * (x * log(ifelse(x == 0, 1, x/E)) - (x - E))
                          tmp <- sqrt(pmax(tmp, 0))
                          ifelse(x > E, tmp, -tmp)
                        },
                        FT = sqrt(x) + sqrt(x + 1) - sqrt(4 * E + 1))
  }

  ## global test
  luminance <- 1
  if (shade && test) {
    chi <- crossprod(as.vector(residuals))
    df <- prod(d) - 1 - sum(d - 1)
    if (pchisq(chi, df) <= level) luminance <- 0.5
  }
  
  ## legend for shading
  if (!is.null(residuals) && legend && shade) {
    if(is.null(prange))
      prange <- range(residuals)
    CINT <- prange
    
    l <- grid.layout(1, 2, widths = c(0.8, 0.2))
    push.viewport(viewport(layout = l))
    push.viewport(viewport(layout.pos.col = 2))
    push.viewport(viewport(width = 0.5, height = 0.8, y = 0.1,
                           just = c("centre","bottom"), yscale = prange))
                           
    yp <- seq(prange[1], prange[2], length = granularity)[-1]
    colors <- color(yp, luminance = luminance, CINT = CINT)
    grid.rect(x = rep(0.5, granularity),
              y = yp,
              height = yp[2] - yp[1],
              just = c("centre", "top"),
              gp = gpar(fill = colors, col = colors),
              default = "native")
    grid.rect()
    grid.yaxis(at = c(0, seq(from = unique(trunc(yp))[1] + 1,
                 to = rev(unique(trunc(yp)))[1], 2),
                 ceiling(prange[1] * 10) / 10,
                 floor(prange[2] * 10) / 10),
               main = FALSE)
    if(!panel)
      grid.text(c("standardized", "residuals:"),
                y = unit(c(1.1, 1.05), "npc"),
                x = 0, just = "left",
                gp = gpar(fontsize = unit(12, "native")))
    pop.viewport(2)
  }

  cc <- 0
  lab <- list()
  ## workhorse for tiles
  split <- function(table, v, x0, y0, w, h, index = c()) {
    ## compute relative tile sizes and positions
    m <- apply(table, 1, sum)
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
    
## store all coorinates in the `lab' list and print all at the end
## to prevent overlapping text. Not really a neat solution.

    if (direction == "vertical") {
      if (dim == 1 && axes[1])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[1]]),
                                            x = -0.05, 
                                            y = coord[,2] - coord[,4] / 2,
                                            rot = 90)
      else if (dim == 2 && index[1] == 1 && axes[2])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[2]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = 1.05, rot = 0)
      else if (dim == 3 && index[2] == d[2] && axes[3])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[3]]),
                                            x = 1.05, rot = -90,
                                            y = coord[,2] - coord[,4] / 2)
      else if (dim == 4 && index[3] == d[3] && index[1] == d[1] && axes[4])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[4]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = -0.05, rot = 0)
    } else {
      if (dim == 1 && axes[1])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[1]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = 1.05, rot = 0)
      else if (dim == 2 && index[1] == 1 && axes[2])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[2]]),
                                            y = coord[,2] - coord[,4] / 2,
                                            x = -0.05, rot = 90)
      else if (dim == 3 && index[2] == d[2] && axes[3])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[3]]),
                                            x = coord[,1] + coord[,3] / 2,
                                            y = -0.05, rot = 0)
      else if (dim == 4 && index[3] == d[3] && index[1] == d[1] && axes[4])
        lab[[cc <<- cc + 1]] <<- data.frame(lab = I(dimnames(x)[[4]]),
                                            y = coord[,2] - coord[,4] / 2,
                                            x = 1.05, rot = -90)
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
  
  w = 1; h = 1;
  if(any(labels)) {
    w <- w - 0.1; h <- h - 0.1
  }
  if(any(axes)) {
    w <- w - 0.1; h <- h - 0.1
  }
  push.viewport(viewport(width = w, height = h))

  ## compute coordinates
  coordinates <- split(x, v = direction == "vertical", 0, 1, 1, 1)

  ## draw tiles
  grid.rect(coordinates[,1], coordinates[,2], coordinates[,3], coordinates[,4],
            just = c("left", "top"),
            gp = gpar(
              fill = if (shade) color(residuals[coordinates[,-(1:4)]],
                luminance = luminance),
              lty = if (shade) ifelse(residuals[coordinates[,-(1:4)]] <= 0,
                "dashed", "solid")
              )
            )

  ## draw labels
  if (any(axes)) {
    lab = do.call("rbind", lab)
    grid.text(lab[,1], x = lab[,2], y = lab[,3], rot = lab[,4], check.overlap = TRUE)
  }
  
  ## draw dim labels
  if (direction == "vertical") {
    if (labels[1])
      grid.text(names(dimnames(x))[1],
                y = 0.5, x = -0.05 - any(axes) * 0.05, gp = gpar(fontface = 2), rot = 90)
    
    if (maxdim > 1 && labels[2])
      grid.text(names(dimnames(x))[2],
                x = 0.5, y = 1.05 + any(axes) * 0.05, gp = gpar(fontface = 2), rot = 0)
    
    if (maxdim > 2 && labels[3])
      grid.text(names(dimnames(x))[3],
                y = 0.5, x = 1.05 + any(axes) * 0.05, gp = gpar(fontface = 2), rot = -90)
    
    if (maxdim > 3 && labels[4])
      grid.text(names(dimnames(x))[4],
                x = 0.5, y = -0.05 - any(axes) * 0.05, gp = gpar(fontface = 2), rot = 0)
  } else {
    if (labels[1])
      grid.text(names(dimnames(x))[1],
                x = 0.5, y = 1.05 + any(axes) * 0.05, gp = gpar(fontface = 2))
    
    if (maxdim > 1 && labels[2])
      grid.text(names(dimnames(x))[2],
                y = 0.5, x = -0.05 - any(axes) * 0.05, gp = gpar(fontface = 2), rot = 90)
    
    if (maxdim > 2 && labels[3])
      grid.text(names(dimnames(x))[3],
                x = 0.5, y = -0.05 - any(axes) * 0.05, gp = gpar(fontface = 2))
    
    if (maxdim > 3 && labels[4])
      grid.text(names(dimnames(x))[4],
                y = 0.5, x = 1.05 + any(axes) * 0.05, gp = gpar(fontface = 2), rot = -90)
  }
  
  ## clean up and return
  pop.viewport(3 + legend)
  invisible(x)
}

## colorschemes

vcd.continous <- function(residuals, CINT = range(residuals),
                                      luminance = 1,
                                      positive = 0, negative = 2 / 3)
  array(hsv(ifelse(residuals < 0, positive, negative),
            pmin(residuals / CINT[1 + (residuals > 0)], 1),
            luminance),
        if (is.null(d <- dim(residuals))) length(residuals) else d
        )

vcd.continous2 <- function(residuals, CINT = range(residuals),
                                       luminance = 1,
                                       positive = 0, negative = 2 / 3)
  array(hsv(ifelse(residuals < 0, positive, negative),
            pmin(residuals / CINT[1 + (residuals > 0)], 1) ^ 2,
            luminance),
        if (is.null(d <- dim(residuals))) length(residuals) else d
        )

vcd.binary <- function(residuals, CINT = range(residuals),
                                   luminance = 1,
                                   positive = 0, negative = 2 / 3)
  array(hsv(ifelse(residuals < 0, positive, negative),
            1, luminance),
        if (is.null(d <- dim(residuals))) length(residuals) else d
        )

vcd.Friendly <- function(residuals, CINT = range(residuals),
                                     luminance = 1,
                                     positive = 0, negative = 2 / 3)
  array(hsv(ifelse(residuals < 0, positive, negative),
            ifelse(abs(residuals) < 2, 0,
                   ifelse(abs(residuals) <= 4, 0.5, 1)),
        luminance),
        if (is.null(d <- dim(residuals))) length(residuals) else d
        )

vcd.significant <- function(residuals, CINT = range(residuals),
                                        luminance = 1,
                                        positive = 0, negative = 2 / 3)
  array(hsv(ifelse(residuals < 0, positive, negative),
            residuals < CINT[1] | residuals > CINT[2],
            luminance),
        if (is.null(d <- dim(residuals))) length(residuals) else d
        )
