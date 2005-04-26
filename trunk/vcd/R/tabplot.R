tabplot <- function(x, panel = function(x, ...) assoc(x, newpage = FALSE, ...),
                    margins = rep(1,4), ...)
{
  UseMethod("tabplot")
}

tabplot.formula <- function(formula, panel = function(x, ...) assoc(x, newpage = FALSE, ...),
                    margins = rep(1,4), data, ...)
{
  formula <- as.character(formula)
  formula <- paste(paste(c("Freq", formula[1:2]), collapse = " "), formula[3], sep = " + ")
  formula <- gsub("\\|", "+", formula)
  formula <- gsub("\\*", "+", formula)
  x <- xtabs(as.formula(formula), data = as.data.frame(data))
  tabplot(x, panel = panel, margins = margins, ...)
}


tabplot.default <- function(x, panel = function(x, ...) assoc(x, newpage = FALSE, labeling = NULL, ...), margins = rep(1, 4), fontsize = 12, ...)
{
  grid.newpage()

  if(length(dim(x)) <= 2) panel(x, ...) ## no conditioning variables
  else {

  axis.names <- names(dimnames(x))[1:2]
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
  if(!is.null(axis.names[2])) margins[1] <- margins[1] + 1
  if(!is.null(axis.names[1])) margins[2] <- margins[2] + 1
  pushViewport(plotViewport(margins))

  grid.text(axis.names[1], x = unit(0, "native") - unit(1, "lines"), rot = 90,
            gp = gpar(fontsize = fontsize))
  grid.text(axis.names[2], y = unit(0, "native") - unit(1, "lines"),
            gp = gpar(fontsize = fontsize))

  pushViewport(viewport(layout = grid.layout(nr, nc, widths = unit(1/nc, "npc"))))

  strUnit <- unit(2 * ncol(condition), "strheight", "A")
  cellport <- viewport(layout = grid.layout(2, 1,
        heights = unit.c(strUnit, unit(1, "npc") - strUnit)))

  for(i in 1:nrow(condition)) {
    condi <- paste(condvars, " = \"", condition[i,], "\"", sep = "", collapse = ", ")
    condistr <- paste(condvars, condition[i,], sep = " = ")

    tabi <- eval(parse(text = paste("x[,,", condi, "]", sep = "")))

    pushViewport(viewport(layout.pos.row = layout[i,1], layout.pos.col = layout[i,2]))
    pushViewport(cellport)
    pushViewport(viewport(layout.pos.row = 1))
    grid.rect(gp = gpar(fill = grey(0.9)))
    grid.text(condistr, y = ncond:1/ncond - 1/(2*ncond)) #Z# , gp = gpar(fontsize = fontsize))
    grid.segments(0, 0:ncond/ncond, 1, 0:ncond/ncond)
    popViewport()

    pushViewport(viewport(layout.pos.row = 2))
    panel(tabi, ...)
    popViewport(2)
    grid.rect()
    popViewport()
  }
  popViewport(2)
  }

  invisible(x)
}

tplot <- function(x,
                  indep = NULL,
		  cond = NULL,
                  panel = function(x, ...) mosaic(x, newpage = FALSE, legend = NULL, gp = NULL, pop = TRUE, ...),
		  margins = rep(1, 4),
		  text.gp = gpar(fontsize = 12),
		  pop = TRUE,
		  ...)
{
  grid.newpage()

  ldx <- length(dim(x))
  if(is.null(indep)) { indep <- if(ldx > 1) 1:2 else 1 }
  if(is.null(cond) && ldx > 2) cond <- (max(indep) + 1):ldx

  if(is.character(indep)) indep <- match(indep, names(dimnames(x)))
  if(is.character(cond)) cond <- match(cond, names(dimnames(x)))
  
  x <- margin.table(x, c(indep, cond))

  if(is.null(cond)) panel(x, ...) ## no conditioning variables
  else {

  ind.n <- length(indep)
  ind.num <- 1:ind.n
  ind.dnam <- dimnames(x)[ind.num]
  ind.char <- names(ind.dnam)
  
  cond.n <- length(cond)
  cond.num <- (ind.n + 1):length(dim(x))
  cond.dnam <- dimnames(x)[cond.num]
  cond.char <- names(cond.dnam)

  ## only for coindep.plot!!
  indep.formula <- as.formula(paste("~ (",
                                    paste(ind.char, collapse = " + "),
				    ") * ",
				    paste(cond.char, collapse = " * "),
				    sep = ""))
  fm <- loglm(indep.formula, data = x, fitted = TRUE)
  ## expected
  xx <- co_table(x, cond.num)

  cond.nlevels <- sapply(cond.dnam, length)
  nplots <- prod(cond.nlevels)
  condition <- as.matrix(sapply(expand.grid(cond.dnam), as.character)) ## FIXME: needed?

  ## compute layout
  layout <- c(1,1,1) ## rows, cols, pages
  if(cond.n == 1) {
    layout[2] <- ceiling(sqrt(floor(cond.nlevels)))
    layout[1] <- ceiling(cond.nlevels/layout[2])
    layout <- expand.grid(lapply(layout, function(x) 1:x))[1:nplots,]
  }
  else {
    layout[1] <- cond.nlevels[1]
    layout[2] <- cond.nlevels[2]
    if(cond.n > 3) layout[3] <- nplots/prod(cond.nlevels[1:2])
    if(layout[3] > 1) stop("multiple pages not supported yet")
    layout <- expand.grid(lapply(layout, function(x) 1:x))
  }

  nr <- max(layout[,1])
  nc <- max(layout[,2])

  pushViewport(plotViewport(margins))
  pushViewport(viewport(layout = grid.layout(nr, nc, widths = unit(1/nc, "npc"))))

  strUnit <- unit(2 * ncol(condition), "strheight", "A")
  cellport <- function(name) viewport(layout = grid.layout(2, 1,
        heights = unit.c(strUnit, unit(1, "npc") - strUnit)),
	name = name)

  for(i in 1:nrow(condition)) {
    tabi <- xx[[i]]
    condistr <- paste(cond.char, condition[i,], sep = " = ")

    pushViewport(viewport(layout.pos.row = layout[i,1], layout.pos.col = layout[i,2]))
    pushViewport(cellport(paste("cell", paste(condition[i,], collapse = "."), sep = ".")))
    pushViewport(viewport(layout.pos.row = 1, name = paste("lab", paste(condition[i,], collapse = "."), sep = ".")))
    grid.rect(gp = gpar(fill = grey(0.9)))
    grid.text(condistr, y = cond.n:1/cond.n - 1/(2*cond.n), gp = text.gp)
    grid.segments(0, 0:cond.n/cond.n, 1, 0:cond.n/cond.n)
    upViewport()

    pushViewport(viewport(layout.pos.row = 2, name = paste("plot", paste(condition[i,], collapse = "."), sep = ".")))
    panel(tabi, ...)
    upViewport(2)
    grid.rect()
    upViewport()
  }
  upViewport()
  if(pop) popViewport() else upViewport()
  }

  invisible(x)
}
