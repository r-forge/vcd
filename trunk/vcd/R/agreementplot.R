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
        agreementplot(table(mf), ...)
    }
}

"agreementplot.default" <-
  function(x,
           reverse.y = TRUE,
           title = "Agreement Chart",
           cex.main = 2,
           cex.lab = 1.5,
           ...)
{
  if (length(dim(x)) > 2)
    stop("Kappa only implemented for two-way tables")
  if (ncol(x) != nrow(x))
    stop("Dimensions must have equal length!")

  nc <- ncol(x)
  
  ## compute relative frequencies
  n <- sum(x)
  cols <- colSums(x)/n
  rows <- rowSums(x)/n

  ## expected values
  ex <- rows %o% cols * n

  ## set margins
  par(usr = c(0, 1, 0, 1))

  ## title, etc.
  plot.new()
  title(title,
        xlab = names(dimnames(x))[2],
        ylab = names(dimnames(x))[1],
        cex.main = cex.main,
        cex.lab = cex.lab,
        ...)

  rect(0,0,1,1)

  xc <- c(0, cumsum(cols))
  yc <- c(0, cumsum(rows))

  for (i in 1:nc) {
    ## x - axis
    axis(1, at = xc[i] + (xc[i+1] - xc[i]) / 2,
         labels = dimnames(x)[[2]][i], tick = FALSE, ...)

    ## y - axis and expected rectanges
    if (reverse.y) {
      ## y - axis
      axis(2, at = yc[i] + (yc[i+1] - yc[i]) / 2,
           labels = dimnames(x)[[1]][i], tick = FALSE, ...)
      
      ## expected rectange
      rect(xc[i], yc[i], xc[i+1], yc[i+1])

      ## observed rectangle
      y0 <- c(0, cumsum(x[i,])) / sum(x[i,])
      x0 <- c(0, cumsum(x[,i])) / sum(x[,i])

      rect(xc[i] + (xc[i+1] - xc[i]) * x0[i],
           yc[i] + (yc[i+1] - yc[i]) * y0[i],
           xc[i] + (xc[i+1] - xc[i]) * x0[i+1],
           yc[i] + (yc[i+1] - yc[i]) * y0[i+1],
           col = "black"
           )
    } else {
      ## y - axis
      axis(2, at = 1 - yc[i] - (yc[i+1] - yc[i]) / 2,
           labels = dimnames(x)[[1]][i], tick = FALSE, ...)
      
      ## expected rectange
      rect(xc[i], 1 - yc[i], xc[i+1], 1 - yc[i+1])

      ## observed rectangle
      y0 <- c(0, cumsum(x[i,])) / sum(x[i,])
      x0 <- c(0, cumsum(x[,i])) / sum(x[,i])

      rect(xc[i] + (xc[i+1] - xc[i]) * x0[i],
           1 - yc[i] - (yc[i+1] - yc[i]) * y0[i],
           xc[i] + (xc[i+1] - xc[i]) * x0[i+1],
           1 - yc[i] - (yc[i+1] - yc[i]) * y0[i+1],
           col = "black"
           )
    }
  }
  if (reverse.y)
    lines(c(0, 1), c(0, 1), col = "red", lty = "longdash")
  else
    lines(c(0, 1), c(1, 0), col = "red", lty = "longdash")
}




