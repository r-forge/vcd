###########################################################
## mosaicplot

mosaic <- function(x, ...)
  UseMethod("mosaic")

mosaic.formula <-
function(formula, data = NULL, space = NULL, labels = NULL,
         ..., main = NULL)
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
          doubledecker(dat, main = main, space = space, labels = labels, ...)
        else if (length(vars) > 1)
          mosaic(dat, main = main,
                 space = if (is.null(space)) spaces.conditional() else space,
                 labels = if (is.null(labels)) labels.conditional() else labels,
                 condvars = 1:length(condind), ...)
        else
          mosaic(dat, main = main, space = space,
                 labels = if (is.null(labels)) labels.text() else labels, ...)
      } else {
        tab <- if ("Freq" %in% colnames(data))
          xtabs(formula(paste("Freq~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)
        else
          xtabs(formula(paste("~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)

        if (dep != "")
          doubledecker(tab, main = main, space = space, labels = labels, ...)
        else if (!is.null(condnames)) {
          condind <- match(condnames, names(dimnames(tab)))
          mosaic(tab, main = main,
                 space = if (is.null(space)) spaces.conditional() else space,
                 labels = if (is.null(labels)) labels.conditional() else labels,
                 condvars = 1:length(condind),
                 ...)
        } else mosaic(tab, main = main, space = space,
                      labels = if (is.null(labels)) labels.text() else labels, ...)
      }
  }

mosaic.default <- function(x, visZero = TRUE, zeroSize = 0.5,
                           split.vertical = FALSE, direction = NULL,
                           space = NULL, ...) {
  dl <- length(dim(x))
  
  ## splitting argument
  if (!is.null(direction))
    split.vertical <- direction == "v"
  if (length(split.vertical) == 1)
    split.vertical <- rep(c(split.vertical, !split.vertical), length.out = dl)
  if (length(split.vertical) < dl)
    split.vertical <- rep(split.vertical, length.out = dl)

  ## spacing argument
  if (is.null(space))
    space <- if (dl < 3) spaces.equal() else spaces.increase()

  strucplot(x,
            panel = panel.mosaicplot(visZero = visZero, zeroSize = zeroSize),
            split.vertical = split.vertical,
            space = space,
            ...)
}

panel.mosaicplot <- function(visZero = TRUE, zeroSize = 0.6)
  function(observed, expected = NULL, residuals,
           space = NULL, gp = NULL, split.vertical = TRUE) {
    dn <- dimnames(observed)
    dnn <- names(dn)
    dx <- dim(observed)
    dl <- length(dx)

    ## split workhorse
    split <- function(x, i, name, row, col) {
      cotab <- co.table(x, 1)
      margin <- sapply(cotab, sum)
      v <- split.vertical[i]
      d <- dx[i]

      ## compute total cols/rows and build split layout
      dist <- unit.c(unit(margin, "null"), space[[i]])
      idx <- matrix(1:(2*d), nrow = 2, byrow = TRUE)[-2*d]
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
    mnames <- paste("cell",
                    apply(expand.grid(dn), 1,
                          function(i) paste(dnn, i, collapse="..", sep = ".")
                          ),
                    sep = "..")
    zeros <- observed <= .Machine$double.eps

    for (i in seq(along = mnames)) {
      seekViewport(mnames[i])
      grid.rect(gp = structure(lapply(gp, function(x) x[i]), class = "gpar"))
      if (visZero && zeros[i]) {
        grid.points(0.5, 0.5, pch = 19, size = unit(zeroSize, "char"),
                    gp = gpar(col = gp$fill[i]))
        grid.points(0.5, 0.5, pch = 1, size = unit(zeroSize, "char"))
      }
    }

  }
