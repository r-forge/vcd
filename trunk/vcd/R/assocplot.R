#################################################################333
## assocplot

assoc <- function(x, ...)
  UseMethod("assoc")

assoc.formula <-
function(formula, data = NULL, subset, na.action, ..., main = NULL)
{
    if (is.logical(main) && main)
      main <- deparse(substitute(data))

    assoc.default(ftable(formula, data, subset, na.action), main = main, ...)
}

assoc.default <- function(x,
                          row.vars = NULL, col.vars = NULL,
                          compress = TRUE, xlim = NULL, ylim = NULL,
                          space = NULL, split.vertical = NULL, ...) {

  if (!inherits(x, "ftable")) {
    if (is.null(row.vars) && is.null(col.vars) && is.table(x))
      row.vars <- names(dimnames(x))[seq(1, length(dim(x)), by = 2)]
    x <- ftable(x, row.vars = row.vars, col.vars = col.vars)
  }

  tab <- as.table(x)
  dl <- length(dim(tab))
  
  ## splitting arguments
  split.vertical <- rep(FALSE, dl)
  names(split.vertical) <- names(dimnames(tab))
  split.vertical[names(attr(x, "col.vars"))] <- TRUE
  
  ## spacing argument
  if (is.null(space))
# FIXME: which spacing?
#    space <- if (dl < 3) spaces.equal(sp = 1) else spaces.increase(rate = 2)
    space = spaces.equal(1)

  strucplot(tab,
            space = space,
            split.vertical = split.vertical,
            panel = panel.assocplot(compress = compress, xlim = xlim, ylim = ylim),
            keepAR = FALSE,
            ...)
}

panel.assocplot <- function(compress = TRUE, xlim = NULL, ylim = NULL, xfactor = 0.8)
  function(observed = NULL, expected, residuals,
           space = NULL, gp = NULL, split.vertical = TRUE) {
    dn <- dimnames(expected)
    dnn <- names(dn)
    dx <- dim(expected)
    dl <- length(dx)

    ## axis limits
    resid <- structable(residuals, split.vertical = split.vertical)
    sexpected <- structable(sqrt(expected), split.vertical = split.vertical)
    if(is.null(ylim))
      ylim <- matrix(if (compress)
                       apply(resid, 1, range)
                     else
                       rep.int(range(resid), nrow(resid)),
                     nrow = 2)
    attr(ylim, "split.vertical") <- rep(T, sum(!split.vertical))
    attr(ylim, "dnames") <- dn[!split.vertical]
    class(ylim) <- "structable"

    if(is.null(xlim))
      xlim <- matrix(if (compress)
                       c(-1,1) %o% apply(sexpected, 2, max)
                     else
                       rep.int(c(-1,1) * max(sexpected), ncol(resid)),
                     nrow = 2) / 2
    attr(xlim, "split.vertical") <- rep(T, sum(split.vertical))
    attr(xlim, "dnames") <- dn[split.vertical]
    class(xlim) <- "structable"

    ## split workhorse
    split <- function(res, sexp, i, name, row, col) {
      v <- split.vertical[i]
      splitbase <- if (v) sexp else res
      splittab <- lapply(seq(dx[i]), function(j) splitbase[[j]])
      len <- sapply(splittab, function(x) sum(x[2,] - x[1,]))
      d <- dx[i]

      ## compute total cols/rows and build split layout
      dist <- unit.c(unit(len, "null"), space[[i]])
      idx <- matrix(1:(2 * d), nrow = 2, byrow = TRUE)[-2 * d]
      layout <- if (v)
        grid.layout(ncol = 2 * d - 1, widths = dist[idx])
      else
        grid.layout(nrow = 2 * d - 1, heights = dist[idx])
      vproot <- viewport(layout.pos.col = col, layout.pos.row = row,
                         layout = layout, name = name, yscale = res[,1])

      ## next level: either create further splits, or final viewports
      name <- paste(name, "", dnn[i], dn[[i]], sep = ".")
      rows <- cols <- rep.int(1, d)
      if (v) cols <- 2 * 1:d - 1 else rows <- 2 * 1:d - 1

      f <- if (i < dl) {
        if (v)
          function(m) split(res, splittab[[m]], i + 1, name[m], rows[m], cols[m])
        else
          function(m) split(splittab[[m]], sexp, i + 1, name[m], rows[m], cols[m])
      } else {
        if (v)
          function(m) viewport(layout.pos.col = cols[m], layout.pos.row = rows[m],
                               name = name[m], yscale = res[,1],
                               xscale = sexp[,m], default.units = "null")
        else
          function(m) viewport(layout.pos.col = cols[m], layout.pos.row = rows[m],
                               name = name[m], yscale = res[,m],
                               xscale = sexp[,1], default.units = "null")
      }
      vpleaves <- structure(lapply(1:d, f), class = c("vpList", "viewport"))

      vpTree(vproot, vpleaves)
    }

    ## start spltting on top, creates viewport-tree
    pushViewport(split(ylim, xlim, i = 1, name = "cell", row = 1, col = 1))

    ## draw baselines (relies on vertical splits coming first!)
    vsplit <- 1:sum(!split.vertical)
    mnames <- paste("cell",
                    apply(expand.grid(dn[vsplit]), 1,
                          function(i) paste(dnn[vsplit], i, collapse="..", sep = ".")
                          ),
                    sep = "..")
    for (i in seq(along = mnames)) {
      seekViewport(mnames[i])
      grid.lines(y = unit(0, "native"), gp = gpar(lty = 5))
    }
    
    ## draw tiles
    mnames <- paste("cell",
                    apply(expand.grid(dn), 1,
                          function(i) paste(dnn, i, collapse="..", sep = ".")
                          ),
                    sep = "..")
    for (i in seq(along = mnames)) {
      seekViewport(mnames[i])
      grid.rect(y = 0, x = 0,
                height = residuals[i], width = sqrt(expected[i]) * xfactor,
                default.units = "native",
                gp = structure(lapply(gp, function(x) x[i]), class = "gpar"),
                just = c("center", "bottom"))
    }

  }

