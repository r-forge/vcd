#################################################################
### pairsplot

pairs <- function(x,
                  panel.upper = panel.mosaic,
                  panel.lower = panel.mosaic,
                  panel.diag = panel.barplot,
                  type = c("pairwise", "total", "conditional", "joint"),
                  type.upper = NULL,
                  type.lower = NULL,
                  newpage = TRUE,
                  
                  main = NULL,
                  title.gp = gpar(fontsize = 20),
                  
                  space = 0.1,
                  legend = NULL,
                  axes = FALSE,
                  abbreviate = FALSE,
                  margins = c(2, 2, 1, 2),
                  panel.margins = c(0, 0, 0, 0),
                  diag.fontsize = 20,
                  diag.dimnames = TRUE,
                  shade = FALSE,
                  gp = NULL,
                  labeling = NULL,
                  ...)
{
  require(grid)
  if (newpage) grid.newpage()

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
  pushViewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
  pushViewport(viewport(layout = l,
                         height = if (is.null(main)) 1 else 0.9,
                         y = 0, just = "bottom"))

  if (is.logical(main) && main)
    main <- deparse(substitute(x))
  grid.text(main, y = unit(1.05, "npc"), gp = title.gp)


  for (i in 1:d)
    for(j in 1:d) {
      pushViewport(viewport(layout.pos.col = i, layout.pos.row = j))
      pushViewport(viewport(width = 1 - space, height = 1 - space))

      if (i > j)
        panel.upper(x, j, i, type.upper, legend = legend, axes = axes, labeling = labeling,
                    margins = panel.margins, abbreviate = abbreviate,
                    gp = gp, shade = shade, ...)
      else if (i < j)
        panel.lower(x, j, i, type.lower, legend = legend, axes = axes, labeling = labeling,
                    margins = panel.margins, abbreviate = abbreviate, gp = gp,
                    shade = shade, ...)
      else
        panel.diag(margin.table(x, i), fontsize = diag.fontsize,
                   dimnames = diag.dimnames, ...)

      popViewport(2)
    }
  popViewport(2)
  invisible(x)
}

panel.barplot <- function(x, color = "gray", fontsize = 20, dimnames, ...) {
  pushViewport(viewport(x = 0.3, y = 0.1, width = 0.7, height = 0.7,
                         yscale = c(0,max(x)), just = c("left", "bottom"))
                )
  xpos <- seq(0, 1, length = length(x) + 1)[-1]
  halfstep <- (xpos[2] - xpos[1]) / 2
  grid.rect(xpos - halfstep, rep.int(0, length(x)), height = x,
            just = c("centre", "bottom"), width = halfstep,
            gp = gpar(fill = color), default = "native", ...)
  grid.yaxis(at = pretty(c(0,max(x))))
  grid.text(names(x), y = unit(-0.15, "npc"),
            x = xpos - halfstep, just = c("center", "bottom"))
  popViewport(1)
  grid.text(names(dimnames(x)), y = 1, just = c("center", "top"),
            gp = gpar(fontsize = fontsize))

}

panel.assoc <- function(x, i, j, type, legend = FALSE, axes = TRUE,
                        margins = c(0, 0, 0, 0), abbreviate = FALSE,
                        gp = NULL, labeling = NULL, shade = FALSE, ...) {
  assoc(x = margin.table(x, c(i, j)),
        gp = gp,
        shade = shade,
        labeling = labeling,
        margin = margins,
        newpage = FALSE,
        legend = legend,
        main = NULL,
        pop = TRUE,
        ...)
  seekViewport("base")
  popViewport()
}

panel.mosaic <- function(x, i, j, type, legend = NULL, axes = TRUE,
                         margins = c(0, 0, 0, 0), abbreviate = FALSE,
                         gp = NULL, labeling = NULL, shade = FALSE, ...) {
  index <- 1:length(dim(x))
  rest <- index[!index %in% c(i, j)]
  mosaic(x = margin.table(x, if (type == "pairwise") c(i, j) else c(i, j, rest)),
         expected = switch(type,
           pairwise =, total = NULL,
           conditional = list(c(i, rest), c(j, rest)),
           joint = list(c(i, j), rest)
           ),
         gp = gp,
         shade = shade,
         labeling = labeling,
         margin = margins,
         newpage = FALSE,
         main = NULL,
         pop = TRUE,
         condvars = if (type == "conditional") rest else NULL,
         ...)
  seekViewport("base")
  popViewport()
}

panel.text <- function(x, fontsize = 20, dimnames = TRUE, ...) {
  grid.rect()
  grid.text(names(dimnames(x)), gp = gpar(fontsize = fontsize),
            y = 0.5 + dimnames * 0.05, ...)
  if (dimnames)
    grid.text(paste("(",paste(names(x), collapse = ","), ")", sep = ""), y = 0.4)
}

