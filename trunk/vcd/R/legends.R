legend.resbased <- function(fontsize = 12,
                            x = unit(1, "lines"),
                            y = unit(0.1, "npc"),
                            height = unit(0.8, "npc"),
                            width = unit(0.7, "lines"),
                            text = NULL,
                            steps = 200,
                            ticks = 10,
                            pvalue = TRUE) {

  if (!is.unit(x))
    x <- unit(x, "native")
  if (!is.unit(y))
    y <- unit(y, "npc")
  if (!is.unit(width))
    width <- unit(width, "lines")
  if (!is.unit(height))
    height <- unit(height, "npc")
  
  function(obs, res, gp, autotext) {
    if (is.null(text))
      text <- autotext
    
    pushViewport(viewport(x = x, y = y, just = c("left", "bottom"),
                          yscale = range(res), default.unit = "native",
                          height = height, width = width))

    if(!is.null(gp$legend.col)) {
      ## fixme: gp was expected to contain a col.legend.col slot, but actually has legend.col?
      col.legend <- gp$legend.col
    } else {
      if (is.function(gp)) {
        gpfun <- gp
        gp <- gpfun(res, obs)
      }
      col.bins <- gp$col.bins
      if(is.null(col.bins)) col.bins <- min(res) + diff(range(res)) * ((0:steps) / steps)
      y.pos <- col.bins[-length(col.bins)]
      y.height <- diff(col.bins)
      y.col <- gpfun(y.pos + 0.5 * y.height, obs)$fill
      col.legend <- list(pos = y.pos, height = y.height, col = y.col)
    }

    grid.rect(x = unit(rep.int(0, length(col.legend$pos)), "npc"),
              y = col.legend$pos,
              height = col.legend$height, default.unit = "native",
              gp = gpar(fill = col.legend$col, col = NULL),
              just = c("left", "bottom"))

    grid.rect()

    l <- length(col.legend$pos)
    at <- seq(from = col.legend$pos[1],
              to = col.legend$pos[l] + col.legend$height[l],
              length = ticks)
    grid.text(format(signif(at, 2)),
              x = unit(1, "npc") + unit(0.8, "lines") + unit(1, "strwidth", "-4.44"),
              y = at,
              default.unit = "native", just = c("right", "center"))
    grid.segments(x0 = unit(1, "npc"), x1 = unit(1,"npc") + unit(0.5, "lines"),
                  y0 = at, y1 = at, default.unit = "native")

    popViewport(1)
    grid.text(text, x = x, y = unit(1, "npc") - y + unit(1, "lines"),
              gp = gpar(fontsize = fontsize),
              just = c("left", "bottom")
              )
    if(!is.null(gp$p.value) && pvalue) {
      grid.text(paste("p-value =\n", format.pval(gp$p.value), sep = ""),
                x = x,
                y = y - unit(1, "lines"),
                gp = gpar(fontsize = fontsize),
                just = c("left", "top"))
    }
  }
}

legend.fixed <- function(fontsize = 12,
                         x = unit(1, "lines"),
                         y = unit(0.2, "npc"),
                         height = unit(0.8, "npc"),
                         width = unit(1.5, "lines"),
                         text = NULL) {
  
  if (!is.unit(x))
    x <- unit(x, "native")
  if (!is.unit(y))
    y <- unit(y, "npc")
  if (!is.unit(width))
    width <- unit(width, "lines")
  if (!is.unit(height))
    height <- unit(height, "npc")

  function(obs, res, gp, autotext) {

    if (is.null(text))
      text <- autotext
    
    pushViewport(viewport(x = x, y = y, just = c("left", "bottom"),
                          yscale = range(res), default.unit = "native",
                          height = height, width = width))

    if (is.function(gp))
      gp <- gp(obs, res)
    col.legend <- gp$legend.col

    grid.rect(x = unit(rep.int(0, length(col.legend$pos)), "npc"),
              y = col.legend$pos,
              height = col.legend$height, default.unit = "native",
              gp = gpar(fill = col.legend$col, col = NULL),
              just = c("left", "bottom"))

    grid.rect()

    ## labeling for fixed intervals as returned in gp.objects
    l <- length(col.legend$pos)
    at <- c(col.legend$pos, col.legend$pos[l] + col.legend$height[l])
    grid.text(format(signif(at, 2)),
              x = unit(1, "npc") + unit(0.8, "lines") + unit(1, "strwidth", "-4.44"),
              y = at,
              default.unit = "native", just = c("right", "center"))
    grid.segments(x0 = unit(0, "npc"), x1 = unit(1,"npc") + unit(0.5, "lines"),
                  y0 = at, y1 = at, default.unit = "native")

    popViewport(1)
    grid.text(text, x = x + 0.5 * width, y = 0,
              gp = gpar(fontsize = fontsize),
              just = c("left", "centre"),
              rot = 90
              )
  }
}
