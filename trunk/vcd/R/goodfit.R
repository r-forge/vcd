goodfit <- function(obj, type = c("poisson", "binomial", "nbinomial"),
                    par = NULL, size = NULL)
{
    if(is.vector(obj)) {
      obj <- table(obj)
    }
    if(is.table(obj)) {
      if(length(dim(obj)) > 1) stop ("obj must be a 1-way table")
      freq <- as.vector(obj)
      count <- as.numeric(names(obj))
    } else {
      if(!(!is.null(ncol(obj)) && ncol(obj) == 2))
        stop("obj must be a 2-column matrix or data.frame")
      freq <- as.vector(obj[,1])
      count <- as.vector(obj[,2])
    }

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
    RVAL <- list(observed = freq,
                 count = count, fitted = expected,
		 type = type, df = df, estimate = par)
    class(RVAL) <- "goodfit"
    RVAL
}

summary.goodfit <- function(object, ...)
{
    freq <- object$observed
    expected <- object$fitted
    df <- object$df
    chi2 <- sum((freq - expected)^2/expected)
    G2 <- sum(freq*log(freq/expected)) * 2
    RVAL <- c(chi2, G2)
    RVAL <- cbind(RVAL, c(df,df), pchisq(RVAL, df = object$df, lower = FALSE))
    colnames(RVAL) <- c("X^2", "df", "P(> X^2)")
    rownames(RVAL) <- c("Pearson", "Likelihood Ratio")

    cat(paste("\n\t Goodness-of-fit tests for", object$type, "distribution\n\n"))
    print(RVAL)
}

plot.goodfit <- function(x, ...)
{
  rootogram(x, ...)
}

