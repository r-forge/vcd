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

goodfit <- function(freq, count, type = c("poisson", "binomial", "nbinomial"),
                    par = NULL, size = NULL)
{
    df <- length(count) - 1

    type <- match.arg(type)
    switch(type,

    "poisson" = {
      if(is.null(par)) {
        df <- df - 1
	lambda <- weighted.mean(count,freq)
        par <- lambda
      }
        else lambda <- par
      names(par) <- "lambda"
      p.hat <- dpois(count, lambda)
    },

    "binomial" = {
      if(is.null(size)) {
        size <- max(count)
        warning("size was not given, taken as maximum count")
      }
      if(is.null(par)) {
        df <- df - 1
	p <- weighted.mean(count/size, freq)
        par <- p
      }
        else p <- par
      names(par) <- "prob"
      p.hat <- dbinom(count, prob = p, size = size)
    },

    "nbinomial" = {
      if(is.null(par)) {
        df <- df - 2
	xbar <- weighted.mean(count,freq)
	s2 <- var(rep(count,freq))
	p <- xbar / s2
	n <- xbar^2/(s2 - xbar)
        par <- c(p, n)
      } else {
        p <- par[1]
	n <- par[2]
      }
      names(par) <- c("prob", "size")
      p.hat <- dnbinom(count, size = n, prob = p)
    })

    expected <- sum(freq) * p.hat
    chi2 <- sum((freq - expected)^2/expected)
    G2 <- sum(freq*log(freq/expected)) * 2
    RVAL <- c(chi2, G2)
    RVAL <- cbind(RVAL, pchisq(RVAL, df = df, lower = FALSE))
    colnames(RVAL) <- c("statistic", "p value")
    rownames(RVAL) <- c("Pearson chi-square:", "Likelihood ratio:")
    RVAL <- list(tests = RVAL, observed = freq,
                 count = count, fitted = expected,
		 type = type, df = df, estimate = par)
    class(RVAL) <- "goodfit"
    RVAL
}

summary.goodfit <- function(object, ...)
{
    cat(paste("\t Goodness-of-fit test for", object$type, "distribution\n\n"))
    print(object$tests)
    cat(paste("degrees of freedom:", object$df, "\n"))
}

plot.goodfit <- function(x, ...)
{
  rootogram(x, ...)
}

rootogram <- function(x, ...)
{
  UseMethod("rootogram")
}

rootogram.goodfit <- function(x, ...)
{
  rootogram.default(x$observed, x$fitted, names = x$count, ...)
}

rootogram.default <- function(x, fitted, names = NULL, scale = c("sqrt", "raw"),
                              type = c("hanging", "standing", "deviation"),
			      bar.col = grey(0.7), line.col = 2,
			      xlab = NULL, ylab = NULL, ylim = NULL, ...)
{
   obs <- x
   fit <- fitted
   if(is.null(xlab)) {xlab <-  "Number of Occurences"}

   if(match.arg(scale) == "sqrt") {
     obs <- sqrt(obs)
     fit <- sqrt(fit)
     if(is.null(ylab)) {ylab <- "sqrt(Frequency)"}
   } else {
     if(is.null(ylab)) {ylab <- "Frequency"} }


   switch(match.arg(type),

   "hanging" = {
     if(is.null(ylim)) {ylim <- range(-0.1 * c(fit-obs,fit),
                        c(fit-obs,fit)) + c(0, 0.1)}
     dummy <- barplot(obs, names = names, col = bar.col, beside = FALSE,
             xlab = xlab, ylab = ylab, shift = fit - obs, ylim = ylim, ...)
     lines(dummy, fit, col = line.col, type = "b", pch = 19)
     abline(h = 0)
   },

   "standing" = {
     if(is.null(ylim)) {ylim <- range(-0.01 * c(obs,fit), c(obs,fit)) }
     dummy <- barplot(obs, names = names, col = bar.col,
                      xlab = xlab, ylab = ylab, ylim = ylim, ...)
     lines(dummy, fit, col = line.col, type = "b", pch = 19)
   },

   "deviation" = {
     if(is.null(ylim)) {ylim <- range(-0.1 * c(fit-obs,fit),
                        c(fit-obs,fit)) + c(0, 0.1)}
     dummy <- barplot(fit - obs, names = names, col = bar.col,
                      xlab = xlab, ylab = ylab, ylim = ylim, ...)
     lines(dummy, fit, col = line.col, type = "b", pch = 19)
   })
}

Ord.plot <- function(x, count, ylim = NULL, xlab = "Number of occurences",
                     ylab = "Frequency ratio", legend = TRUE,
		     estim = TRUE, tol = 0.1, ...)
{
  y <- count * x/c(NA, x[-length(x)])
  fm <- lm(y ~ count)
  fmw <- lm(y ~ count, weights = sqrt(x - 1))
  fit1 <- predict(fm, data.frame(count))
  fit2 <- predict(fmw, data.frame(count))
  if(is.null(ylim)) ylim <- range(c(y, fit1, fit2), na.rm = TRUE)
  plot(y ~ count, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  lines(count, fit1)
  lines(count, fit2, col = 2)
  RVAL <- coef(fmw)
  names(RVAL) <- c("Intercept", "Slope")
  if(legend)
  {
    legend.text <- c(paste("slope =", round(RVAL[2], digits = 3)),
                     paste("intercept =", round(RVAL[1], digits = 3)))
    if(estim) {
      ordfit <- Ord.estimate(RVAL, tol = tol)
      legend.text <- c(legend.text, "", paste("type:", ordfit$type),
        paste("estimate:", names(ordfit$estimate),"=", round(ordfit$estimate, digits = 3)))
    }
    legend(min(count), ylim[2], legend.text, bty = "n")
  }
  invisible(RVAL)
}

Ord.estimate <- function(x, type = NULL, tol = 0.1)
{
  a <- x[1]
  b <- x[2]
  if(!is.null(type))
    type <- match.arg(type, c("poisson", "binomial", "nbinomial", "log-series"))
  else {
    if(abs(b) < tol) type <- "poisson"
    else if(b < (-1 * tol)) type <- "binomial"
    else if(a > (-1 * tol)) type <- "nbinomial"
    else type <- "log-series"
  }

  switch(type,

  "poisson" = {
    par <- a
    names(par) <- "lambda"
    if(par < 0) warning("lambda not > 0")
  },
  "binomial" = {
    par <- b/(b - 1)
    names(par) <- "prob"
    if(abs(par - 0.5) > 0.5) warning("prob not in (0,1)")
  },
  "nbinomial" = {
    par <- 1 - b
    names(par) <- "prob"
    if(abs(par - 0.5) > 0.5) warning("prob not in (0,1)")
  },

  "log-series" = {
    par <- b
    names(par) <- "theta"
    if(par < 0) warning("theta not > 0")
  })
  list(estimate = par, type = type)
}


distplot <- function(freq, count, type = c("poisson", "binomial", "nbinomial"),
                     size = NULL, lambda = NULL, legend = TRUE, ylim = NULL,
                     line.col = 2, conf.int = TRUE, conf.level = 0.95, main = NULL,
		     xlab = "Number of occurences", ylab = "Distribution metameter", ...)
{
  par.ml <- goodfit(freq, count, type = type, size = size)$estimate

  myindex <- (1:length(freq))[freq > 0]
  mycount <- count[myindex]
  myfreq <- freq[myindex]

  switch(match.arg(type),

  "poisson" = {
    phi <- function(nk, k, N, size = NULL)
      ifelse(nk > 0, lgamma(k + 1) + log(nk/N), NA)
    y <- phi(myfreq, mycount, sum(freq))
    if(!is.null(lambda)) y <- y + lambda - mycount * log(lambda)
    fm <- lm(y ~ mycount)
    par.estim <- exp(coef(fm)[2])
    names(par.estim) <- "lambda"
    if(!is.null(lambda)) par.estim <- par.estim * lambda
    legend.text <- paste("exp(slope) =", round(par.estim, digits = 3))
    if(is.null(main)) main <- "Poissoness plot"
  },

  "binomial" = {
    if(is.null(size)) size <- max(count)
    phi <- function(nk, k, N, size)
      log(nk) - log(N * choose(size, k))
    y <- phi(myfreq, mycount, sum(freq), size = size)
    fm <- lm(y ~ mycount)
    par.estim <- exp(coef(fm)[2])
    par.estim <- par.estim / (1 + par.estim)
    names(par.estim) <- "prob"
    legend.text <- paste("inv.logit(slope) =", round(par.estim, digits = 3))
    if(is.null(main)) main <- "Binomialness plot"
  },

  "nbinomial" = {
    size <- par.ml[2]
    phi <- function(nk, k, N, size)
      log(nk) - log(N * choose(size + k - 1, k))
    y <- phi(myfreq, mycount, sum(freq), size = size)
    fm <- lm(y ~ mycount)
    par.estim <- 1 - exp(coef(fm)[2])
    names(par.estim) <- "prob"
    legend.text <- paste("1-exp(slope) =", round(par.estim, digits = 3))
    if(is.null(main)) main <- "Negative binomialness plot"
  })

  yhat <- ifelse(myfreq > 1.5, myfreq - 0.67, 1/exp(1))
  yhat <- phi(yhat, mycount, sum(freq), size = size)
  if(!is.null(lambda)) yhat <- yhat + lambda - mycount * log(lambda)

  phat <- myfreq / sum(myfreq)
  ci.width <- qnorm(1-(1 - conf.level)/2) *
              sqrt(1-phat)/sqrt(myfreq - (0.25 * phat + 0.47)*sqrt(myfreq))

  RVAL <- cbind(count, freq, NA, NA, NA, NA, NA)
  RVAL[myindex,3:7] <- cbind(y,yhat,ci.width, yhat-ci.width, yhat + ci.width)
  RVAL <- as.data.frame(RVAL)
  names(RVAL) <- c("Counts", "Freq", "Metameter", "CI.center",
                   "CI.width", "CI.lower", "CI.upper")

  if(is.null(ylim)) ylim <- range(RVAL[,c(3,6,7)], na.rm = TRUE)
  plot(Metameter ~ Counts, ylim = ylim, data = RVAL,
       xlab = xlab, ylab = ylab, main = main, ...)
  abline(fm, col = line.col)

  if(conf.int) {
    points(CI.center ~ Counts, data = RVAL, pch = 19, cex = 0.6)
    arrows(RVAL[,1], RVAL[,6], RVAL[,1], RVAL[,7], length = 0, lty = 3)
  }

  if(legend) {
    mymin <- which.min(RVAL[,5])
    leg.x <- RVAL[mymin,1]
    if(RVAL[mymin,6] - ylim[1] > ylim[2] - RVAL[mymin,7])
      leg.y <- ylim[1] + 0.7 * (RVAL[mymin,6] - ylim[1])
    else leg.y <- ylim[2]

    legend.text <- c(paste("slope =", round(coef(fm)[2], digits = 3)),
                     paste("intercept =", round(coef(fm)[1], digits = 3)),
		     "", paste(names(par.estim),": mm =", round(par.ml[1], digits=3)),
		     legend.text)
    legend(leg.x, leg.y, legend.text, bty = "n")
  }
  invisible(RVAL)
}


barplot.default <-
function(height, width = 1, space = NULL, names.arg = NULL,
         legend.text = NULL, beside = FALSE, horiz = FALSE,
         density = NULL, angle = 45,
         col = heat.colors(NR), border = par("fg"),
         main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
         xlim = NULL, ylim = NULL, xpd = TRUE,
         axes = TRUE, axisnames = TRUE,
         cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
         inside = TRUE, plot = TRUE, shift = 0, ...)
{
    if (!missing(inside)) .NotYetUsed("inside", error = FALSE)
    if (!missing(border)) .NotYetUsed("border", error = FALSE)

    if (missing(space))
	space <- if (is.matrix(height) && beside) c(0, 1) else 0.2
    space <- space * mean(width)

    if (plot && axisnames && missing(names.arg))
	names.arg <-
	    if(is.matrix(height)) colnames(height) else names(height)

    if (is.vector(height)) {
	height <- cbind(height)
	beside <- TRUE
    } else if (is.array(height) && (length(dim(height)) == 1)) {
	height <- rbind(height)
	beside <- TRUE
    } else if (!is.matrix(height))
	stop("`height' must be a vector or a matrix")

    if(is.logical(legend.text)) {
        if(legend.text && is.matrix(height))
            legend.text <- rownames(height)
        else
            legend.text <- NULL
    }

    NR <- nrow(height)
    NC <- ncol(height)
    if(length(shift) == 1)
      shift <- rep(shift, NR)
    else if(length(shift) != NR)
      stop("incorrect number of shifts")

    if (beside) {
	if (length(space) == 2)
	    space <- rep(c(space[2], rep(space[1], NR - 1)), NC)
	width <- rep(width, length = NR * NC)
    } else {
	width <- rep(width, length = NC)
	height <- rbind(0, apply(height, 2, cumsum))
    }
    delta <- width / 2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
	if (missing(xlim)) xlim <- range(-0.01 * height + shift, height + shift, na.rm=TRUE)
	if (missing(ylim)) ylim <- c(min(w.l + shift), max(w.r + shift))
    } else {
	if (missing(xlim)) xlim <- c(min(w.l), max(w.r))
	if (missing(ylim)) ylim <- range(-0.01 * height + shift, height + shift, na.rm=TRUE)
    }
    if (beside)
	w.m <- matrix(w.m, nc = NC)
    if(plot) { ##-------- Plotting :
	opar <-
	    if (horiz)	par(xaxs = "i", xpd = xpd)
	    else	par(yaxs = "i", xpd = xpd)
	on.exit(par(opar))

	plot.new()
	plot.window(xlim, ylim, log = "", ...)
        # Beware : angle and density are passed using R scoping rules
	xyrect <- function(x1,y1, x2,y2, horizontal = TRUE, ...) {
	    if(horizontal)
		rect(x1,y1, x2,y2, angle = angle, density = density, ...)
	    else
		rect(y1,x1, y2,x2, angle = angle, density = density, ...)
	}
	if (beside)
          xyrect(0 + shift, w.l, c(height) + shift, w.r, horizontal=horiz, col = col)
	else {
	    for (i in 1:NC) {
		xyrect(height[1:NR, i] + shift[i], w.l[i],
		       height[-1, i] + shift[i], w.r[i],
		       horizontal=horiz, col = col)
	    }
	}
	if (axisnames && !is.null(names.arg)) { # specified or from {col}names
	    at.l <- if (length(names.arg) != length(w.m)) {
		if (length(names.arg) == NC) # i.e. beside (!)
		    colMeans(w.m)
		else
		    stop("incorrect number of names")
	    } else w.m
	    axis(if(horiz) 2 else 1, at = at.l,
                 labels = names.arg, lty = 0, cex.axis = cex.names, ...)
	}
	if(!is.null(legend.text)) {
	    legend.col <- rep(col, length = length(legend.text))
	    if((horiz & beside) || (!horiz & !beside)){
		legend.text <- rev(legend.text)
		legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
	    }
	    xy <- par("usr")
	    legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1),
		   legend = legend.text, angle = angle, density = density,
                   fill = legend.col, xjust = 1, yjust = 1)
	}
	title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	if(axes) axis(if(horiz) 1 else 2, cex.axis = cex.axis, ...)
	invisible(w.m)
    } else w.m
}

CatDat.demo <- function(chapter = 2)
{

if(chapter == 2) {
  par(ask = TRUE)

  data(HorseKicks)
  barplot(HorseKicks$Freq, names = HorseKicks$nDeaths, col = 2,
          xlab = "Number of Deaths", ylab = "Number of Corps-Years",
          main = "Deaths by Horse Kicks")

  data(Federalist)
  barplot(Federalist$Freq, names = Federalist$nMay, col = 2,
          xlab = "Occurences of 'may'", ylab = "Number of Blocks of Text",
          main = "'may' in Federalist papers")

  data(WomenQueue)
  barplot(WomenQueue$Freq, names = WomenQueue$nWomen, col = 2,
          xlab = "Number of women", ylab = "Number of queues",
          main = "Women in queues of length 10")

  data(WeldonDice)
  barplot(WeldonDice$Freq, names = WeldonDice$n56, col = 2,
          xlab = "Number of 5s and 6s", ylab = "Frequency",
	  main = "Weldon's dice data")

  data(Butterfly)
  barplot(Butterfly$nSpecies, names = Butterfly$nTokens, col = 2,
          xlab = "Number of individuals", ylab = "Number of Species",
  	main = "Butterfly species im Malaya")


############################
## Binomial distributions ##
############################

par(mfrow = c(1,2))
barplot(dbinom(0:10, p = 0.15, size = 10), names = 0:10, col = grey(0.7),
        main = "p = 0.15", ylim = c(0,0.35))
barplot(dbinom(0:10, p = 0.35, size = 10), names = 0:10, col = grey(0.7),
        main = "p = 0.35", ylim = c(0,0.35))
par(mfrow = c(1,1))
mtext("Binomial distributions", line = 2, cex = 1.5)

plot(0:10, dbinom(0:10, p = 0.15, size = 10), type = "b", ylab = "Density",
     ylim = c(0, 0.4), main = "Binomial distributions, N = 10", pch = 19)
lines(0:10, dbinom(0:10, p = 0.35, size = 10), type = "b", col = 2, pch = 19)
lines(0:10, dbinom(0:10, p = 0.55, size = 10), type = "b", col = 4, pch = 19)
lines(0:10, dbinom(0:10, p = 0.75, size = 10), type = "b", col = 3, pch = 19)
legend(3, 0.4, c("p", "0.15", "0.35", "0.55", "0.75"), lty = rep(1,5), col =
       c(0,1,2,4,3), bty = "n")

###########################
## Poisson distributions ##
###########################

par(mfrow = c(1,2))
dummy <- barplot(dpois(0:12, 2), names = 0:12, col = grey(0.7), ylim = c(0,0.3),
        main = expression(lambda == 2))
abline(v = dummy[3], col = 2)
diff <- (dummy[3] - dummy[2]) * sqrt(2)/2
lines(c(dummy[3] - diff, dummy[3] + diff), c(0.3, 0.3), col = 2)
dummy <- barplot(dpois(0:12, 5), names = 0:12, col = grey(0.7), ylim = c(0,0.3),
        main = expression(lambda == 5))
abline(v = dummy[6], col = 2)
diff <- (dummy[6] - dummy[5]) * sqrt(5)/2
lines(c(dummy[6] - diff, dummy[6] + diff), c(0.3, 0.3), col = 2)
par(mfrow = c(1,1))
mtext("Poisson distributions", line = 2, cex = 1.5)

#####################################
## Negative binomial distributions ##
#####################################

nbplot <- function(p = 0.2, size = 2, ylim = c(0, 0.2))
{
  plot(0:20, dnbinom(0:20, p = p, size = size), type = "h", col = grey(0.7),
       xlab = "Number of failures (k)", ylab = "Density", ylim = ylim,
       yaxs = "i", bty = "L")
  nb.mean <- size * (1-p)/p
  nb.sd <- sqrt(nb.mean/p)
  abline(v = nb.mean, col = 2)
  lines(nb.mean + c(-nb.sd, nb.sd), c(0.01, 0.01), col = 2)
  legend(14, 0.2, c(paste("p = ", p), paste("n = ", size)), bty = "n")
}
par(mfrow = c(3,2))
nbplot()
nbplot(size = 4)
nbplot(p = 0.3)
nbplot(p = 0.3, size = 4)
nbplot(p = 0.4, size = 2)
nbplot(p = 0.4, size = 4)
par(mfrow = c(1,1))
mtext("Negative binomial distributions for the number of trials to observe n = 2
 or n = 4 successes", line = 3)

#####################
## Goodness of fit ##
#####################

attach(HorseKicks)
p <- weighted.mean(nDeaths, Freq)
p.hat <- dpois(0:4, p)
expected <- sum(Freq) * p.hat
chi2 <- sum((Freq - expected)^2/expected)
pchisq(chi2, df = 3, lower = FALSE)
## or:
HK.fit <- goodfit(Freq, nDeaths)
summary(HK.fit)
detach(HorseKicks)

attach(WeldonDice)
## Are the dice fair?
p.hyp <- 1/3
p.hat <- dbinom(0:10, prob = p.hyp, size = 12)
expected <- sum(Freq) * p.hat
chi2 <- sum((Freq - expected)^2/expected)
G2 <- sum(Freq*log(Freq/expected)) ##???
pchisq(chi2, df = 10, lower = FALSE)
## Are the data from a binomial distribution?
p <- weighted.mean(n56/12, Freq)
p.hat <- dbinom(0:10, prob = p, size = 12)
expected <- sum(Freq) * p.hat
chi2 <- sum((Freq - expected)^2/expected)
G2 <- sum(Freq*log(Freq/expected)) ##???
pchisq(chi2, df = 9, lower = FALSE)
## or:
WD.fit1 <- goodfit(Freq, n56, type = "binomial", par = 1/3, size = 12)
WD.fit2 <- goodfit(Freq, n56, type = "binomial", size = 12)
summary(WD.fit1)
summary(WD.fit2)
detach(WeldonDice)

attach(Federalist)
F.fit1 <- goodfit(Freq, nMay)
F.fit2 <- goodfit(Freq, nMay, type = "nbinomial")
summary(F.fit1)
par(mfrow = c(2,2))
plot(F.fit1, scale = "raw", type = "standing")
plot(F.fit1, type = "standing")
plot(F.fit1)
plot(F.fit1, type = "deviation")
par(mfrow = c(1,1))
plot(F.fit2, type = "deviation")
summary(F.fit2)
detach(Federalist)

data(Saxony)
attach(Saxony)
S.fit <- goodfit(Freq, nMales, type = "binomial", size = 12)
summary(S.fit)
plot(S.fit)
detach(Saxony)

###############
## Ord plots ##
###############

par(mfrow = c(2,2))
Ord.plot(HorseKicks$Freq, HorseKicks$nDeaths, main = "Death by horse kicks")
Ord.plot(Federalist$Freq, Federalist$nMay, main = "Instances of 'may' in
 Federalist papers")
Ord.plot(Butterfly$nSpecies, Butterfly$nTokens, main = "Butterfly species
 collected in Malaya")
Ord.plot(WomenQueue$Freq, WomenQueue$nWomen, main = "Women in queues of length
 10")
par(mfrow = c(1,1))

###############
## Distplots ##
###############

distplot(HorseKicks$Freq, HorseKicks$nDeaths, type = "poisson")
distplot(HorseKicks$Freq, HorseKicks$nDeaths, type = "poisson", lambda = 0.61)
distplot(Federalist$Freq, Federalist$nMay, type = "poisson")
distplot(Federalist$Freq, Federalist$nMay, type = "nbinomial")
distplot(Saxony$Freq, Saxony$nMales, type = "binomial", size = 12)

}
par(ask = FALSE)
}


