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
                      spacing = NULL,
                      gp = NULL,
                      labeling = labeling.text(),
                      panel = panel.mosaic(),
                      legend = legend.resbased(),
                      
                      main = NULL,
                      sub = NULL,
                      margin = rep.int(2.5, 4),
                      legend.width = unit(0.15, "npc"),
                      
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
  if (is.function(spacing))
    spacing <- spacing(dim(x), condvars)

  ## gp (color, fill, lty, etc.) argument
  if (shade) {
    if (is.null(gp))
      gp <- gp.HCLshading()
    if (is.function(gp)) {
      gpfun <- gp
      gp <- gpfun(residuals = residuals, observed = x, expected = expected)
    } else if (!is.null(legend))
      stop("gp argument must be a shading function for drawing a legend")
  } else {
    if (!is.null(gp)) {
      warning("gp parameter ignored since shade=FALSE")
      gp <- NULL
    }
  }
  
  ## choose gray when no shading is used
  if (is.null(gp))
    gp <- gpar(fill = rep.int(grey(0.8), length(x)))
  
  ## set up page
  if (newpage) grid.newpage()
  pushViewport(vcdViewport(mar = margin, legend = shade && !is.null(legend),
                           main = !is.null(main), sub = !is.null(sub), keepAR = keepAR,
                           legend.width = legend.width))

  ## legend
  if (is.logical(legend))
    legend <- if (legend) legend.resbased() else NULL
  if (shade && !is.null(legend)) {
    seekViewport("legend")
    legend(obs = x, res = residuals, gp = gpfun,
           autotext = paste(residuals.type, "residuals:", sep = "\n"))
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
  panel(residuals = residuals,
        observed = if (type == "observed") x else expected,
        expected = expected,
        spacing = spacing,
        gp = gp,
        split.vertical = split.vertical)

  upViewport(dl)

  ## labels
  if (!is.null(labeling)) labeling(dn, split.vertical, condvars)

  ## pop/move up viewport
  seekViewport("cell")
  if (pop) popViewport()
}

vcdViewport <- function(mar = rep.int(2.5, 4),
                        legend.width = unit(0.15, "npc"),
                        legend = FALSE, main = FALSE, sub = FALSE,
                        keepAR = TRUE)
{
  mar <- if (!is.unit(mar))
    unit(pexpand(mar, 4, rep.int(2.5, 4), c("top","right","bottom","left")), "lines")
  else
    unit.rep(mar, length.out = 4)
  if (!is.unit(legend.width))
    legend.width <- unit(legend.width, "npc")
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
                         widths = unit.c(mar[4],
                           unit(1, if (keepAR) "snpc" else "npc") -
                           (mar[2] + mar[4] + (1 * !keepAR) * legend.width),
                           mar[2], legend.width),
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
                         widths = unit.c(mar[4], unit(1, if (keepAR) "snpc" else "npc") -
                           mar[2] - mar[4], mar[2]),
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
    
    space <- legend.width + mar[2] + mar[4] - mar[1] - mar[3]
    sandwich <- if (legend) {
      vplist <- vpList(vpTop, vpPlotregion, vpSub)
      viewport(layout = grid.layout(3, 1, height = unit.c(0.5 * space, unit(1, "npc") -
                                            space, 0.5 * space)))
    } else if (main && sub) {
      vplist <- vpList(vpTop, vpPlotregion, vpSub)
      viewport(layout = grid.layout(3, 1,
                 height = unit.c(unit(2, "lines"),
                   unit(1, "npc") - legend.width, unit(2, "lines"))))
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
