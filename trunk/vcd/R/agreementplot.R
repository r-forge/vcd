"agreementplot" <- function (x, ...)
  UseMethod ("agreementplot")

"agreementplot.formula" <-
function (formula, data = NULL, ..., subset) 
{
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    if (inherits(edata, "ftable") || inherits(edata, "table")) {
        data <- as.table(data)
        varnames <- attr(terms(formula), "term.labels")
        if (all(varnames != ".")) 
            data <- margin.table(data, match(varnames, names(dimnames(data))))
        agreementplot(data, ...)
    }
    else {
        if (is.matrix(edata)) 
            m$data <- as.data.frame(data)
        m$... <- NULL
        m[[1]] <- as.name("model.frame")
        mf <- eval(m, parent.frame())
        if (length(formula) == 2) {
          by <- mf
          y <- NULL
        }
        else {
          i <- attr(attr(mf, "terms"), "response")
          by <- mf[-i]
          y <- mf[[i]]
        }
        by <- lapply(by, factor)
        x <- if (is.null(y)) 
          do.call("table", by)
        else if (NCOL(y) == 1) 
          tapply(y, by, sum)
        else {
          z <- lapply(as.data.frame(y), tapply, by, sum)
          array(unlist(z), dim = c(dim(z[[1]]), length(z)), dimnames = c(dimnames(z[[1]]), 
                                                              list(names(z))))
        }
        x[is.na(x)] <- 0
        agreementplot(x, ...)
    }
}

"agreementplot.default" <-
  function(x,
           reverse.y = TRUE,
           main = "Agreement Chart",
           weights = c(1, 1 - 1 / (ncol(x) - 1)^2),
           cex.main = 2,
           cex.lab = 1.5,
           xlab = names(dimnames(x))[2],
           ylab = names(dimnames(x))[1],
           ...)
{
  if (length(dim(x)) > 2)
    stop("Function implemented for two-way tables only!")
  if (ncol(x) != nrow(x))
    stop("Dimensions must have equal length!")

  nc <- ncol(x)
  
  ## compute relative frequencies
  n <- sum(x)
  cols <- colSums(x)/n
  rows <- rowSums(x)/n

  ## set margins
  par(usr = c(0, 1, 0, 1))

  ## title, etc.
  plot.new()
  title(main,
        xlab = xlab,
        ylab = ylab,
        cex.main = cex.main,
        cex.lab = cex.lab,
        ...)

  rect(0, 0, 1, 1)

  xc <- c(0, cumsum(cols))
  yc <- c(0, cumsum(rows))

  my.axis <- function (side, at, ...)
    if (reverse.y)
      axis(side, at, ...)
    else
      axis(side, 1 - at, ...)

  my.rect <- function (xleft, ybottom, xright, ytop, ...)
    if (reverse.y)
      rect(xleft, ybottom, xright, ytop, ...)
    else
      rect(xleft, 1 - ybottom, xright, 1 - ytop, ...)
  
  A <- matrix(0, length(weights), nc)
  for (i in 1:nc) {
    ## x - axis
    axis(1, at = xc[i] + (xc[i+1] - xc[i]) / 2,
         labels = dimnames(x)[[2]][i], tick = FALSE, ...)

    ## y - axis
    my.axis(2, at = yc[i] + (yc[i+1] - yc[i]) / 2,
            labels = dimnames(x)[[1]][i], tick = FALSE, ...)
    
    ## expected rectange
    my.rect(xc[i], yc[i], xc[i+1], yc[i+1])
    
    ## observed rectangle
    y0 <- c(0, cumsum(x[i,])) / sum(x[i,])
    x0 <- c(0, cumsum(x[,i])) / sum(x[,i])

    rec <- function (col, dens)
      my.rect(xc[i] + (xc[i+1] - xc[i]) * x0[lb],
              yc[i] + (yc[i+1] - yc[i]) * y0[lb],
              xc[i] + (xc[i+1] - xc[i]) * x0[tr],
              yc[i] + (yc[i+1] - yc[i]) * y0[tr],
#             col = gray(1-(weights[j])^2)
              col = col,
              density = dens,
              angle = 135
              )

    for (j in length(weights):1) {
      lb <- max(1, i - j + 1)
      tr <- 1 + min(nc, i + j - 1)
      A[j, i] <- sum(x[lb:(tr-1),i]) * sum(x[i, lb:(tr-1)])
      rec("white", NULL) ## erase background
      rec("black", if (weights[j] < 1) weights[j] * 20 else NULL)
    }

    ## correct A[j,i] -> not done by Friendly==Bug?
    for (j in length(weights):1) 
      if (j > 1) A[j, i] <- A[j, i] - A[j - 1, i]
  }
  if (reverse.y)
    lines(c(0, 1), c(0, 1), col = "red", lty = "longdash")
  else
    lines(c(0, 1), c(1, 0), col = "red", lty = "longdash")
  
  ## Statistics - Returned invisibly
  ads <- crossprod(diag(x)) 
  ar  <- n * n * crossprod(cols, rows)
  invisible(list(
                 Bangdiwala = ads / ar,
                 Bangdiwala.Weighted = (sum(weights * A)) /  ar,
                 weights = weights,
                 )
            )
}

Kappa <- function (x, weights = c("Equal-Spacing", "Fleiss-Cohen"))
{
  if (is.character(weights))
      weights = match.arg(weights)

  d  <- diag(x)
  n  <- sum(x)
  nc <- ncol(x)
  cols <- colSums(x)/n
  rows <- rowSums(x)/n
  
  ## Kappa
  kappa <- function (po, pc)
    (po - pc) / (1 - pc)
  std  <- function (po, pc, W = 1)
    sqrt(sum(W * W * po * (1 - po)) / crossprod(1 - pc) / n)
    
  ## unweighted
  po <- sum(d) / n
  pc <- crossprod(cols, rows)
  k <- kappa(po, pc)
  s <- std(po, pc)
  
  ## weighted 
  if (is.matrix(weights))
    W <- weights
  else
    W <- if (weights == "Equal-Spacing")
      outer (1:nc, 1:nc, function(x, y) 1 - abs(x - y) / (nc - 1))
    else
      outer (1:nc, 1:nc, function(x, y) 1 - (abs(x - y) / (nc - 1))^2)
  pow <- sum(W * x) / n
  pcw <- sum(W * cols %o% rows)
  kw <- kappa(pow, pcw)
  sw <- std(x / n, 1 - pcw, W)

  structure(
            list(Kappa = c(
                   value = k,
                   ASE   = s,
                   lwr   = k - s * qnorm(0.975),
                   upr   = k + s * qnorm(0.975) 
                   ),
                 Kappa.Weighted = c(
                   value = kw,
                   ASE   = sw,
                   lwr   = kw - sw * qnorm(0.975),
                   upr   = kw + sw * qnorm(0.975) 
                   ),
                 Weights = W),
            class = "kappa"
       )
}

print.kappa <- function (x, ...) {
  tab <- rbind(x$Kappa, x$Kappa.Weighted)
  rownames(tab) <- names(x)[1:2]
  print(tab)
}

summary.kappa <- function (object, ...) {
  print(object)
  cat("\nWeights:\n")
  print(object$Weights)
}

expected <- function(x, frequency = c("absolute","relative")) {
  if (!is.array(x))
    stop("Need array of absolute frequencies!")
  frequency <- match.arg(frequency)

  n <- sum(x)
  x <- x / n
  d <- length(dim(x))
  tab <- apply(x, 1, sum)
  for (i in 2:d)
    tab <- tab %o% apply(x, i, sum)
  if (frequency == "relative") tab else tab * n
}

mar.table <- function(x) {
  if(!is.matrix(x))
    stop("Function only defined for m x n - tables.")
  tab <- rbind(cbind(x, TOTAL = rowSums(x)), TOTAL = c(colSums(x), sum(x)))
  names(dimnames(tab)) <- names(dimnames(x))
  tab
}

Summary <- function(x,
                    totals = TRUE,
                    percentages = TRUE,
                    conditionals = c("row", "column", "none"),
                    digits = 4
                    )
{
  if(!is.matrix(x))
    stop("Function only defined for m x n - tables.")
  conditionals <- match.arg(conditionals)
  
  tab <- array(0, c(dim(x) + totals, 1 + percentages + (conditionals != "none")))

  ## frequencies
  tab[,,1] <- if(totals) mar.table(x) else x

  ## percentages
  if(percentages) {
    tmp <- prop.table(x)
    tab[,,2] <- 100 * if(totals) mar.table(tmp) else tmp
  }

  ## conditional distributions
  if(conditionals != "none") {
    tmp <- prop.table(x, margin = 1 + (conditionals == "column"))
    tab[,,2 + percentages] <- 100 * if(totals) mar.table(tmp) else tmp
  }

  ## dimnames
  dimnames(tab) <- c(dimnames(if(totals) mar.table(x) else x),
                     list(c("freq",
                            if(percentages) "%",
                            switch(conditionals, row = "row%", column = "col%")
                            )
                          )
                     )

  ## patch row%/col% totals
  if(conditionals == "row") 
    tab[dim(tab)[1],,3] <- NA

  if(conditionals == "column")
    tab[,dim(tab)[2],3] <- NA
                     
  if(dim(tab)[3] == 1)
    print(tab[,,1], digits = digits)
  else
    print(ftable(aperm(tab, c(1,3,2))),2, digits = digits)
  
  invisible(tab)
}




