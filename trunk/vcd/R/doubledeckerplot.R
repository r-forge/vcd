#######################################
### doubledecker plot

doubledecker <- function(x, ...)
  UseMethod("doubledecker")

doubledecker.formula <-
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
    if (dep == "")
      stop("Need a dependent variable!")
    varnames <- c(varnames, dep)

    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        dat <- as.table(data)
        if(all(varnames != ".")) {
          
          ind <- match(varnames, names(dimnames(dat)))
          if (any(is.na(ind)))
            stop(paste("Can't find", paste(varnames[is.na(ind)], collapse=" / "), "in", deparse(substitute(data))))
          
          dat <- margin.table(dat, ind)
        } else {
          ind <- match(dep, names(dimnames(dat)))
          dat <- aperm(dat, c(seq(along = dim(dat))[-ind],ind))
          }
        doubledecker.default(dat, main = main, ...)
      } else {
        tab <- if ("Freq" %in% colnames(data))
          xtabs(formula(paste("Freq~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)
        else
          xtabs(formula(paste("~", paste(c(condnames, varnames), collapse = "+"))),
                data = data)

        doubledecker.default(tab, main = main, ...)
      }
  }

doubledecker.default <- function(x,
                         margin = c(1, 4, length(dim(x)) + 1, 1),
                         col = hcl(seq(0, 260, length = dim(x)[length(dim(x))]), 50, 70),
                         labeling = labeling_doubledecker(),
                         spacing = spacing_doubledecker(),
                         main = NULL, 
                         keepAR = FALSE,
                         ...) {
  d <- dim(x)
  l <- length(d)
  colind = array(rep(1:d[l], each = prod(d[-l])), dim = d)
  mosaic(x, condvars = 1:(l - 1),
         spacing = spacing,
         split = c(rep.int(TRUE, l - 1), FALSE),
         shading = gpar(fill = col[colind]),
         shade = TRUE,
         labeling = labeling,
         main = main,
         margin = margin,
         legend = NULL,
         keepAR = keepAR,
         ...
         )
}
