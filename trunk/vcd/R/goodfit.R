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

