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

