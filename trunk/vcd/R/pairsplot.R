#################################################################
### pairsplot

pairs.table <- function(x,
                  upper_panel = panel_mosaic(),
                  lower_panel = panel_mosaic(),
                  diag_panel = panel_barplot(),
                  
                  main = NULL,
                  title_gp = gpar(fontsize = 20),
                  
                  space = 0.1,
                  newpage = TRUE,
                  ...)
{
  require(grid)
  if (newpage) grid.newpage()

  d <- length(dim(x))
  l <- grid.layout(d, d)
  pushViewport(viewport(width = unit(1, "snpc"), height = unit(1, "snpc")))
  pushViewport(viewport(layout = l,
                         height = if (is.null(main)) 1 else 0.9,
                         y = 0, just = "bottom"))

  if (is.logical(main) && main)
    main <- deparse(substitute(x))
  grid.text(main, y = unit(1.05, "npc"), gp = title_gp)


  for (i in 1:d)
    for(j in 1:d) {
      pushViewport(viewport(layout.pos.col = i, layout.pos.row = j))
      pushViewport(viewport(width = 1 - space, height = 1 - space))

      if (i > j)
        upper_panel(x, j, i)
      else if (i < j)
        lower_panel(x, j, i)
      else
        diag_panel(margin.table(x, i))

      popViewport(2)
    }
  popViewport(2)
  invisible(x)
}

## upper/lower panels

panel_assoc <- function(type = NULL, legend = FALSE, margins = c(0, 0, 0, 0),
                        labeling = NULL, shade = TRUE, ...)
  function(x, i, j) assoc(x = margin.table(x, c(i, j)),
                          
                          labeling = labeling,
                          margins = margins,
                          legend = legend,
                          shade = shade,
                          
                          newpage = FALSE,
                          pop = TRUE,
                          ...)


panel_mosaic <- function(type = c("pairwise", "total", "conditional", "joint"),
                         legend = FALSE, margins = c(0, 0, 0, 0),
                         labeling = NULL, shade = TRUE, ...) {
  type = match.arg(type)
  function(x, i, j) {
    index <- 1:length(dim(x))
    rest <- index[!index %in% c(i, j)]
    mosaic(x = margin.table(x, if (type == "pairwise") c(i, j) else c(i, j, rest)),
           expected = switch(type,
             pairwise =, total = NULL,
             conditional = list(c(i, rest), c(j, rest)),
             joint = list(c(i, j), rest)
             ),
           condvars = if (type == "conditional") rest else NULL,
           
           labeling = labeling,
           margins = margins,
           legend = legend,
           shade = shade,
           
           newpage = FALSE,
           pop = TRUE,
           ...)
  }
}
  
## diagonal panels

panel_text <- function(dimnames = TRUE,
                       gp_vartext = gpar(fontsize = 17),
                       gp_leveltext = gpar(),
                       gp_border = gpar(),
                       ...)

  function(x) {
    grid.rect(gp = gp_border)
    grid.text(names(dimnames(x)), gp = gp_vartext,  y = 0.5 + dimnames * 0.05, ...)
    if (dimnames)
      grid.text(paste("(",paste(names(x), collapse = ","), ")", sep = ""),
                y = 0.4, gp = gp_leveltext)
  }

panel_barplot <- function(dimnames = NULL,
                          gp_bars = gpar(fill = "gray"),
                          gp_vartext = gpar(fontsize = 17),
                          gp_leveltext = gpar(),
                          ...)
  function(x) {
    pushViewport(viewport(x = 0.3, y = 0.1, width = 0.7, height = 0.7,
                          yscale = c(0,max(x)), just = c("left", "bottom"))
                 )
    xpos <- seq(0, 1, length = length(x) + 1)[-1]
    halfstep <- (xpos[2] - xpos[1]) / 2
    grid.rect(xpos - halfstep, rep.int(0, length(x)), height = x,
              just = c("center", "bottom"), width = halfstep,
              gp = gp_bars, default = "native", ...)
    grid.yaxis(at = pretty(c(0,max(x))))
    grid.text(names(x), y = unit(-0.15, "npc"),
              x = xpos - halfstep, just = c("center", "bottom"), gp = gp_leveltext)
    popViewport(1)
    grid.text(names(dimnames(x)), y = 1, just = c("center", "top"), gp = gp_vartext)

  }


