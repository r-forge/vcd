"logoddsratio" <-
function (x, stratum = NULL, log = TRUE) {
  l <- length(dim(x))
  if (l > 2 && is.null(stratum))
    stratum <- 3:l
  if (l - length(stratum) > 2)
    stop("All but 2 dimensions must be specified as strata.")
  if (l == 2 && dim(x) != c(2,2))
    stop("Not a 2x2-table.")
  if (!is.null(stratum) && dim(x)[-stratum] != c(2,2))
    stop("Need strata of 2 x 2 - tables.")
 
  lor <- function (y) {
    or <- y[1,1] * y[2,2] / y[1,2] / y[2,1]
    if (log) log(or) else or
  }

  ase <- function(y) sqrt(sum(1/y))

  if(is.null(stratum))
    structure(lor(x), ASE = ase(x), class = "logoddsratio")
  else
    structure(apply(x, stratum, lor),
              ASE   = apply(x, stratum, ase),
              class = "logoddsratio"
              )
}

"print.logoddsratio" <-
function(x, ...) {
  if (length(dim(x)) > 1)
    print(cbind(unclass(x)))
  else
    print(c(x))
}

"summary.logoddsratio" <-
function(object, ...) {
  cat("\nLog Odd Ratio(s):\n\n")
  print(object)
  cat("\nAsymptotic Standard Error(s):\n\n")
  print(attr(object, "ASE"))
  cat("\n")
}

"plot.logoddsratio" <-
function(x,
         quantile = qnorm(0.975),
         confidence = TRUE,
         type = "o",
         ylab = "Log Odds Ratio",
         xlab = "Strata",
         whiskers = 0.1,
         ...)
{
  if (length(dim(x)) > 1)
    stop ("Plot function works only on vectors.")
  
  yrange <- range(x)
  
  if(confidence) {
    ASE <- attr(x, "ASE")
    lwr <- x - ASE * quantile
    upr <- x + ASE * quantile
    yrange[1] <- trunc(min(yrange[1], min(lwr)))
    yrange[2] <- ceiling(max(yrange[2], max(upr)))
  }

  plot(unclass(x),
       xlab = xlab,
       ylab = ylab,
       type = type,
       xaxt = "n",
       ylim = yrange,
       ...)
  axis (1, at = 1:length(x), names(x))

  if (confidence)
    for (i in 1:length(x)) {
      lines(c(i, i), c(lwr[i], upr[i]))
      lines(c(i - whiskers/2, i + whiskers/2), c(lwr[i], lwr[i]))
      lines(c(i - whiskers/2, i + whiskers/2), c(upr[i], upr[i]))
    }
}



