pearson.test <- function (x, n = 1000, FUN = function(x) max(abs(x)),
                          alternative = c("greater", "less"),
                          conf.level = 0.95,
                          return.distribution = TRUE)
{
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  rowTotals <- rowSums(x)
  colTotals <- colSums(x)
  expected <- rowTotals %o% colTotals / sum(colTotals)
  Pearson <- function(x) (x - expected) / sqrt(expected)
  FUNPearson <- function(x) FUN(Pearson(x))
  dist <- sapply(r2dtable(n, rowTotals, colTotals), FUNPearson)
  STATISTIC <- FUNPearson(x)
  pdist <- function(x) sapply(x, function(y) mean(dist <= y))
  qdist <- function(p) quantile(dist, p)
  PVAL <- switch(alternative,
                 greater = mean(dist >= STATISTIC),
                 less = mean(dist <= STATISTIC)
                 )
  ## CINT <- switch(alternative,
  ##                greater = c(0, quantile(dist, conf.level)),
  ##                less = c(quantile(dist, 1 - conf.level), 0),
  ##                )
  ## names(CINT) <- NULL
  ## attr(CINT, "conf.level") <- conf.level
  METHOD <- "Permutation test of independence (based on simulated Pearson residuals)"
  names(STATISTIC) <- "P"
  structure(list(statistic = STATISTIC,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME,
                 observed = x,
                 expected = expected,
                 residuals = Pearson(x),
                 dist = if (return.distribution) dist else NULL,
                 ## conf.int = CINT,
		 qdist = qdist,
		 pdist = pdist),
            class = "htest"
            )
}




