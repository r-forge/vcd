###########################################################
## mosaicplot

mosaic <- function(x, ...)
  UseMethod("mosaic")

mosaic.formula <-
function(formula, data = NULL, ..., main = NULL)
{
    if (is.logical(main) && main)
      main <- deparse(substitute(data))
    
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    
    fstr <- strsplit(paste(deparse(formula), collapse = ""), "~")
    vars <- strsplit(strsplit(gsub(" ", "", fstr[[1]][2]), "\\|")[[1]], "\\+")
    dep <- gsub(" ", "", fstr[[1]][1])
    varnames <- vars[[1]]
    if (dep != "") varnames <- c(varnames, dep)
    condnames <- if (length(vars) > 1) vars[[2]] else NULL

    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        dat <- as.table(data)
        if(all(varnames != ".")) {
          
          ind <- match(varnames, names(dimnames(dat)))
          if (any(is.na(ind)))
            stop(paste("Can't find", paste(varnames[is.na(ind)], collapse=" / "), "in", deparse(substitute(data))))
          
          if (!is.null(condnames)) {
            condind <- match(condnames, names(dimnames(dat)))
            if (any(is.na(condind)))
              stop(paste("Can't find", paste(condnames[is.na(condind)], collapse=" / "), "in", deparse(substitute(data))))
            ind <- c(condind, ind)
          }
          dat <- margin.table(dat, ind)
        }
        if (dep != "")
          doubledecker(dat, main = main, ...)
        else 
          mosaic(dat, main = main, ...)
      } else {
        tab <- if ("Freq" %in% colnames(data))
          xtabs(formula(paste("Freq~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)
        else
          xtabs(formula(paste("~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)

        if (dep != "")
          doubledecker(tab, main = main, ...)
        else
          mosaic(tab, main = main, ...)
      }
  }

mosaic.default <- function(x,
                           split_vertical = FALSE, direction = NULL,
                           spacing = NULL, spacing_args = list(),
                           visZero = TRUE, zeroSize = 0.5, ...) {
  dl <- length(dim(x))
  
  ## splitting argument
  if (!is.null(direction))
    split_vertical <- direction == "v"
  if (length(split_vertical) == 1)
    split_vertical <- rep(c(split_vertical, !split_vertical), length.out = dl)
  if (length(split_vertical) < dl)
    split_vertical <- rep(split_vertical, length.out = dl)

  ## spacing argument
  if (is.null(spacing))
    spacing <- if (dl < 3) spacing_equal else spacing_increase

  strucplot(x,
            panel = panel_mosaicplot(visZero = visZero, zeroSize = zeroSize),
            split_vertical = split_vertical,
            spacing = spacing,
            spacing_args = spacing_args,
            ...)
}

panel_mosaicplot <- function(visZero = TRUE, zeroSize = 0.5)
  function(residuals, observed, expected = NULL, spacing, shading, split_vertical) {
    dn <- dimnames(observed)
    dnn <- names(dn)
    dx <- dim(observed)
    dl <- length(dx)

    ## split workhorse
    split <- function(x, i, name, row, col) {
      cotab <- co.table(x, 1)
      margin <- sapply(cotab, sum)
      v <- split_vertical[i]
      d <- dx[i]

      ## compute total cols/rows and build split layout
      dist <- unit.c(unit(margin, "null"), spacing[[i]])
      idx <- matrix(1:(2 * d), nrow = 2, byrow = TRUE)[-2*d]
      layout <- if (v)
        grid.layout(ncol = 2 * d - 1, widths = dist[idx])
      else
        grid.layout(nrow = 2 * d - 1, heights = dist[idx])
      vproot <- viewport(layout.pos.col = col, layout.pos.row = row,
                         layout = layout, name = name)
      
      ## next level: either create further splits, or final viewports
      name <- paste(name, "", dnn[i], dn[[i]], sep = ".")
      row <- col <- rep.int(1, d)
      if (v) col <- 2 * 1:d - 1 else row <- 2 * 1:d - 1
      f <- if (i < dl) 
        function(m) split(cotab[[m]], i + 1, name[m], row[m], col[m])
      else
        function(m) viewport(layout.pos.col = col[m], layout.pos.row = row[m],
                             name = name[m])
      vpleaves <- structure(lapply(1:d, f), class = c("vpList", "viewport"))

      vpTree(vproot, vpleaves)
    }

    ## start spltting on top, creates viewport-tree
    pushViewport(split(observed + .Machine$double.eps,
                       i = 1, name = "cell", row = 1, col = 1))

    ## draw rectangles
    mnames <-  apply(expand.grid(dn), 1,
                     function(i) paste(dnn, i, collapse="..", sep = ".")
                     )
    zeros <- observed <= .Machine$double.eps

    for (i in seq(along = mnames)) {
      seekViewport(paste("cell", mnames[i], sep = ".."))
      gpobj <- structure(lapply(shading, function(x) x[i]), class = "gpar")
      if (!zeros[i]) {
        grid.rect(gp = gpobj, name = paste("rect", mnames[i], sep = ".."))
      } else if (visZero) {
        grid.lines(x = 0.5, gp = gpobj)
        grid.lines(y = 0.5, gp = gpobj)
        grid.points(0.5, 0.5, pch = 19, size = unit(zeroSize, "char"),
                    gp = gpar(col = shading$fill[i]),
                    name = paste("disc", mnames[i], sep = ".."))
        grid.points(0.5, 0.5, pch = 1, size = unit(zeroSize, "char"),
                    name = paste("circle", mnames[i], sep = ".."))
      }
    }

  }
class(panel_mosaicplot) <- "vcdPanel"
