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


