#######################################################################
### strucplot - generic plot framework for mosaic-like layouts
### 2 panel functions are provided: panel.mosaicplot and panel.assocplot
########################################################################

strucplot <- function(## main parameters
                      x,
                      residuals = NULL,
                      expected = NULL, condvars = NULL,
                      shade = FALSE,
                      type = c("observed", "expected"),
                      residuals.type = c("Pearson", "deviance", "FT"),
                      
                      ## layout
                      split.vertical = TRUE, 
                      space = NULL,
                      gp = NULL,
                      labels = labels.text(),
                      panel = panel.mosaic(),
                      
                      main = NULL,
                      sub = NULL,
                      margin = rep.int(2.5, 4),
                      legend = FALSE,
                      
                      ## control parameters
                      pop = FALSE,
                      title.gp = gpar(fontsize = 20),
                      newpage = TRUE,
                      keepAR = TRUE,
                      eDLlimit = 3
                      ) {
  require(grid)
  
  type <- match.arg(type)
  residuals.type = match.arg(residuals.type)

  ## table characteristics
  dl <- length(dim(x))
  dn <- dimnames(x)
  if (is.null(dn))
    dn <- dimnames(x) <- lapply(dim(x), seq)
  dnn <- names(dimnames(x))
  if (is.null(dnn))
    dnn <- names(dn) <- names(dimnames(x)) <- LETTERS[1:dl]

  ## performance hack
  engine.display.list(dl <= eDLlimit)
  
  ## residuals
  expected <- if (inherits(expected, "formula")) {
    require(MASS)
    fitted(loglm(expected, x, fitted = TRUE))
  } else {
    if (is.null(expected))
      expected <- if (is.null(condvars))
        as.list(1:dl)
      else
        lapply((length(condvars) + 1):dl, c, seq(length(condvars)))
    loglin(x, expected, fit = TRUE, print = FALSE)$fit
  }
  if (is.null(residuals))
    residuals <- switch(residuals.type,
                        ## FIXME: expected == 0 ??
                        Pearson = (x - expected) / sqrt(ifelse(expected > 0, expected, 1)),
                        deviance = {
                          tmp <- 2 * (x * log(ifelse(x == 0, 1, x / expected)) - (x - expected))
                          tmp <- sqrt(pmax(tmp, 0))
                          ifelse(x > expected, tmp, -tmp)
                        },
                        FT = sqrt(x) + sqrt(x + 1) - sqrt(4 * expected + 1)
                        )

  ## spacing
  if (is.function(space))
    space <- space(dim(x), condvars)

  ## gp (color, fill, lty, etc.) argument
  if (shade) {
    if (is.null(gp))
      gp <- gp.HCLshading()
    if (is.function(gp)) {
      gpfun <- gp
      gp <- gpfun(x, residuals)
    } else if (legend) stop("gp argument must be a shading function for drawing a legend")
  } else {
    if (!is.null(gp)) {
      warning("gp parameter ignored since shade=FALSE")
      gp <- NULL
    }
  }
  
  if (legend && !shade)
    stop("Legend only sensible for shade=TRUE")

  ## choose gray when no shading is used
  if (is.null(gp))
    gp <- gpar(fill = rep.int(grey(0.8), length(x)))
  
  ## set up page
  if (newpage) grid.newpage()
  pushViewport(vcdViewport(margin, legend = legend,
                           main = !is.null(main), sub = !is.null(sub), keepAR = keepAR))

  ## legend
  if (shade && legend) {
    seekViewport("legend")
    legend.block(obs = x, res = residuals, gp = gpfun)
    if (pop) popViewport()
  }

  ## titles
  if (!is.null(main)) {
    seekViewport("main")
    if (is.logical(main) && main)
      main <- deparse(substitute(x))
    grid.text(main, gp = title.gp)
  }

  if (!is.null(sub)) {
    seekViewport("sub")
    grid.text(sub, gp = title.gp)
  }

  ## make plot
  seekViewport("plot")
  panel(observed = if (type == "observed") x else expected,
        residuals = residuals,
        expected = expected,
        space = space,
        gp = gp,
        split.vertical = split.vertical)

  upViewport(dl)

  ## labels
  if (!is.null(labels)) labels(dn, split.vertical, condvars)

  ## pop/move up viewport
  seekViewport("cell")
  if (pop) popViewport()
}

legend.block <- function(obs, res,
                         gp = gp.Friendly(),
                         fontsize = 12,
                         space = 2,  
                         text = "residuals:") {

  pushViewport(viewport(x = 0.2, y = 0.2, just = c("left", "bottom"),
                         yscale = range(res), default.unit = "native",
                         height = unit(0.6, "npc"), width = 0.1))

  if(!is.null(gp$col.legend)) {
    col.legend <- gp$col.legend
  } else {
    if (is.function(gp)) {
      gpfun <- gp
      gp <- gpfun(obs, res)
    }
    col.bins <- gp$col.bins
    if(is.null(col.bins)) col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
    y.pos <- col.bins[-length(col.bins)]
    y.height <- diff(col.bins)
    y.col <- gpfun(obs, y.pos + 0.5*y.height)$fill
    col.legend <- list(pos = y.pos, height = y.height, col = y.col)
  }

  grid.rect(x = unit(rep.int(0.5, length(col.legend$pos)), "npc"), y = col.legend$pos,
            height = col.legend$height, default.unit = "native",
            gp = gpar(fill = col.legend$col, col = NULL),
            just = c("centre", "bottom"))

  grid.rect()

  at <- seq(from = min(col.legend$pos), to = max(col.legend$pos), length = 10)
  grid.text(format(signif(at, 2)), x = unit(7, "npc"), y = at,
            default.unit = "native", just = c("right", "center"))
  grid.segments(x0 = 1, x1 = 2, y0 = at, y1 = at, default.unit = "native")
  
  popViewport(1)
  grid.text(text, x = 0, y = unit(0.8, "npc") + unit(1, "lines"),
            gp = gpar(fontsize = fontsize),
            just = c("left", "centre")
            )
  if(!is.null(gp$p.value)) {
   grid.text(paste("p-value =\n", format.pval(gp$p.value), sep = ""),
              x = 0, y = unit(0.15, "npc") - unit(0.5, "strheight", "A"),
              gp = gpar(fontsize = fontsize),
              just = c("left", "top"))
  }
}

vcdViewport <- function(mar = unit(c(4, 4, 5, 2), "lines"),
                        legend = FALSE, main = FALSE, sub = FALSE,
                        keepAR = TRUE)
{
  if (!is.unit(mar))
    mar <- unit(mar, "lines")
  if (length(mar) == 1)
    mar <- unit.rep(mar, 4)
  vpPlot <- viewport(layout.pos.col = 2, layout.pos.row = 2, name = "plot")
  vpMarginBottom <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = "marginBottom")
  vpMarginLeft <- viewport(layout.pos.col = 1, layout.pos.row = 2, name = "marginLeft")
  vpMarginTop <- viewport(layout.pos.col = 2, layout.pos.row = 1, name = "marginTop")
  vpMarginRight <- viewport(layout.pos.col = 3, layout.pos.row = 2, name = "marginRight")
  vpCornerTL <- viewport(layout.pos.col = 1, layout.pos.row = 1, name = "cornerTL")
  vpCornerTR <- viewport(layout.pos.col = 3, layout.pos.row = 1, name = "cornerTR")
  vpCornerBL <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = "cornerBL")
  vpCornerBR <- viewport(layout.pos.col = 3, layout.pos.row = 3, name = "cornerBR")

  if(legend) {
    vpLegend <- viewport(layout.pos.col = 4,
      layout.pos.row = 2, name = "legend")
    vpPval <- viewport(layout.pos.col = 4,
      layout.pos.row = 3, name = "pval")
    vpBase <- viewport(layout.pos.row = 1 + (legend || main),
                       layout = grid.layout(3, 4,
                         widths = unit.c(mar[2],
                           unit(1, if (keepAR) "snpc" else "npc") -
                           (mar[2] + mar[4] + (1 * !keepAR) * unit(4, "lines")),
                           mar[4], unit(4, "lines")),
                         heights = unit.c(mar[1], unit(1, if (keepAR) "snpc" else "npc") -
                           (mar[1] + mar[3]), mar[3])),
                       name = "base")
    vpPlotregion <- vpTree(vpBase, vpList(vpMarginBottom, vpMarginLeft, vpMarginTop,
                                          vpMarginRight, vpPval, vpLegend,
                                          vpCornerTL, vpCornerTR, vpCornerBL,
                                          vpCornerBR, vpPlot))
  } else {
    vpBase <- viewport(layout.pos.row = 1 + (legend || main),
                       layout = grid.layout(3, 3,
                         widths = unit.c(mar[2], unit(1, if (keepAR) "snpc" else "npc") -
                           mar[2] - mar[4], mar[4]),
                         heights = unit.c(mar[1], unit(1, if (keepAR) "snpc" else "npc") -
                           mar[1] - mar[3], mar[3])
                         ),
                       name = "base")
    vpPlotregion <- vpTree(vpBase,
                           vpList(vpMarginBottom, vpMarginLeft, vpMarginTop, vpMarginRight,
                                  vpCornerTL, vpCornerTR, vpCornerBL, vpCornerBR, vpPlot))
  }

  ## main/sub-title, margins for legend layout
  if (main || sub || legend) {
    vpTop <- viewport(layout.pos.row = 1, name = "main")
    vpSub <- viewport(layout.pos.row = 2 + main, name = "sub")
    
    space <- unit(4, "lines") + mar[2] + mar[4] - mar[1] - mar[3]
    sandwich <- if (legend) {
      vplist <- vpList(vpTop, vpPlotregion, vpSub)
      viewport(layout = grid.layout(3, 1, height = unit.c(0.5 * space, unit(1, "npc") -
                                            space, 0.5 * space)))
    } else if (main && sub) {
      vplist <- vpList(vpTop, vpPlotregion, vpSub)
      viewport(layout = grid.layout(3, 1,
                 height = unit.c(unit(2, "lines"),
                   unit(1, "npc") - unit(4, "lines"), unit(2, "lines"))))
    } else if (main) {
      vplist <- vpList(vpTop, vpPlotregion)
      viewport(layout = grid.layout(2, 1,
                 height = unit.c(unit(2, "lines"), unit(1, "npc") - unit(2, "lines"))))
    } else {
      vplist <- vpList(vpPlotregion, vpSub)
      viewport(layout = grid.layout(2, 1, height = unit.c(unit(1, "npc") - unit(2, "lines"), unit(2, "lines"))))
    }

    vpTree(sandwich, vplist)
  } else vpPlotregion
}
