"sieveplot" <- function (x, ...)
  UseMethod ("sieveplot")

"sieveplot.formula" <-
function (formula, data = NULL, ..., subset) 
{
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    if (inherits(edata, "ftable") || inherits(edata, "table")) {
        data <- as.table(data)
        varnames <- attr(terms(formula), "term.labels")
        if (all(varnames != ".")) 
            data <- margin.table(data, match(varnames, names(dimnames(data))))
        sieveplot(data, ...)
    }
    else {
        if (is.matrix(edata)) 
            m$data <- as.data.frame(data)
        m$... <- NULL
        m[[1]] <- as.name("model.frame")
        mf <- eval(m, parent.frame())
        sieveplot(table(mf), ...)
    }
}

"sieveplot.default" <-
  function(x,
           reverse.y = TRUE,
           type = c("sieve","expected"),
           title = NULL,
           values = c("none", "cells", "margins", "both"),
           frequencies = c("absolute", "relative"),
           sieve.colors = c("red","blue"),
           sieve.lty = c("longdash", "solid"),
           exp.color = "gray",
           exp.lty = "dotted",
           margin = 0.2,
           cex.main = 3,
           cex.lab = 2,
           ...)
{
  if (length(dim(x)) > 2)
    stop ("Function only implemented for two-way tables")
  
  type <- match.arg(type)
  values <- match.arg(values)
  frequencies <- match.arg(frequencies)
  if (is.null(title))
      title <- if (type == "sieve") "Sieve diagram" else "Expected frequencies"

  if (reverse.y) x <- x[nrow(x):1,]

  ## compute relative frequencies
  n <- sum(x)
  cols <- colSums(x)/n
  rows <- rowSums(x)/n

  ## expected values and signs of deviations
  ex <- rows %o% cols * n
  sgn <- ex - x < 0

  ## build layout
  layout(matrix(1:(ncol(x) * nrow(x)), ncol(x), nrow(x), byrow=TRUE),
         widths = cols,
         heights = rows
         )

  ## set margins
  par(oma = c(5, 5, 10, 5),
      mar = rep(margin,4)
      )

  ## title, etc.
  plot.new()
  title(title,
        xlab = names(dimnames(x))[2],
        ylab = names(dimnames(x))[1],
        outer=TRUE,
        cex.main = cex.main,
        cex.lab = cex.lab,
        ...)

  ## boxes
  for (j in 1:nrow(x))
    for (i in 1:ncol(x)) {
      ## write labels
      if (i == 1) {
        axis(2, at = 0.5, labels = dimnames(x)[[1]][j], tick = FALSE, outer = TRUE, ...)
        ## optionally, write marginal frequencies
        if (values %in%  c("margins","both"))
          axis(4, at = 0.5, font = 2,
               labels = if (frequencies == "relative") round(rows[j], 2)
                        else round(rows[j] * n, 1),
               tick = FALSE, outer = TRUE, ...)
      }
      if (j == 1) {
        axis(1 + 2 * values %in% c("margins","both"),
             at = 0.5, labels = dimnames(x)[[2]][i], tick = FALSE, outer = TRUE, ...)
        ## optionally, write marginal frequencies
        if (values %in%  c("margins","both"))
          axis(1, at = 0.5, font = 2,
               labels = if (frequencies == "relative") round(cols[i], 2)
                        else round(cols[i] * n, 1),
               tick = FALSE, outer = TRUE, ...)
      }

      ## draw grid
      square.side <- sqrt(cols[i] * rows[j] / if (type=="sieve") x[j, i] else ex[j, i])
      dev <- sgn[j, i] + 1
      grid(round(cols[i]/square.side), round(rows[j]/square.side),
           col = if (type == "sieve") sieve.colors[dev] else exp.color,
           lty = if (type == "sieve") sieve.lty[dev] else exp.lty
           )

      ## optionally, write cell frequencies
      if (values %in% c("cells","both"))
          text(0.5, 0.5,
               if (frequencies == "relative")
                 round((if (type == "sieve") x[j, i] else ex[j, i]) / n, 2)
               else
                 round((if (type == "sieve") x[j, i] else ex[j, i]), 1),
               font = 2, ...
               )

      ## border
      box()

      ## new frame
      plot.new()
    }

}



