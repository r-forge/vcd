sieveplot <- function(x,
                      reverse.y = TRUE,
                      type = c("sieve","expected"),
                      title = NULL,
                      values = c("none", "cells", "margins", "both"),
                      frequencies = c("absolute", "relative"),
                      cex.main = 3,
                      cex.lab = 2,
                      ...) {
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

  nx <- 2 * ncol(x) - 1
  ny <- 2 * ncol(x) - 1

  ## build index matrix for layout
  ind <- matrix (0, ny, nx)
  count <- 0
  for (j in 1:ny)
    for (i in 1:nx)
      if ((i * j) %% 2 > 0)
        ind[j, i] <- (count <- count + 1)

  ## build layout
  inserts <- function(x, ins) {
    tmp <- c(sapply(x, function(y) c(y, ins)))
    tmp[-length(tmp)]
  }

  layout(ind,
         widths = inserts(cols, 0.01),
         heights = inserts(rows, 0.01)
         )

  ## title, etc.
  par(oma = c(5, 5, 10, 5),
      mar = c(0, 0, 0, 0))
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
           col = if (type == "sieve") c("red", "blue")[dev] else "gray",
           lty = if (type == "sieve") c("longdash", "solid")[dev] else "dotted"
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



