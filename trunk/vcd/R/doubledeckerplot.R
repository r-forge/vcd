#######################################
### doubledecker plot

doubledecker <- function(x,
                         col = hcl(seq(0, 260, length = dim(x)[length(dim(x))]), 50, 70),
                         main = NULL, 
                         labeling = NULL,
                         margin = c(1, 4, length(dim(x)) + 1, 1),
                         spacing = spacing.doubledecker(),
                         ...) {
  d <- dim(x)
  l <- length(d)
  colind = array(rep(1:d[l], each = prod(d[-l])), dim = d)
  mosaic(x, condvars = 1:(l - 1),
         spacing = spacing,
         split = c(rep.int(TRUE, l - 1), FALSE),
         gp = gpar(fill = col[colind]),
         shade = TRUE,
         labeling = if (is.null(labeling)) labeling.doubledecker() else labeling,
         main = main,
         margin = margin,
         legend = NULL,
         keepAR = FALSE,
         
         ...
         )
}
