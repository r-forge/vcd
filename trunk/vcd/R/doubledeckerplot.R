#######################################
### doubledecker plot

doubledecker <- function(x,
                         col = hcl(seq(0, 260, length = dim(x)[length(dim(x))]), 50, 70),
                         main = NULL, 
                         labels = NULL,
                         margin = c(1, 1, length(dim(x)) + 1, 3),
                         space = spaces.doubledecker(),
                         ...) {
  d <- dim(x)
  l <- length(d)
  colind = array(rep(1:d[l], each = prod(d[-l])), dim = d)
  mosaic(x, condvars = 1:(l - 1),
         space = space,
         split = c(rep.int(TRUE, l - 1), FALSE),
         gp = gpar(fill = col[colind]),
         shade = TRUE,
         labels = if (is.null(labels)) labels.doubledecker() else labels,
         main = main,
         margin = margin,
         ...
         )
}
