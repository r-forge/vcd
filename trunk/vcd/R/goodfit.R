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

      chi2 <- function(x)
      {
	p.hat <- dpois(count, lambda = x)
        expected <- sum(freq) * p.hat
        sum((freq - expected)^2/expected)
      }

      if(is.null(par)) {
        df <- df - 1
	## ML/MM
	par1 <- weighted.mean(count,freq)
	## minChisq
	chi2opt <- optimize(chi2, range(count))
	par2 <- chi2opt$minimum
	statistic <- chi2opt$objective
        par <- rbind(par1, par2)
        rownames(par) <- c("ML", "minChisq")
      }
      else {
        if(!is.list(par)) stop("`par' must be a named list")
        if(!(names(par) == "lambda"))
	  stop("`par' must specify `lambda'")
	par <- matrix(par$lambda, ncol = 1)
	rownames(par) <- "fixed"
	statistic <- chi2(par[1,1])
      }
      colnames(par) <- "lambda"
      p.hat <- dpois(count, lambda = par[1,1])
    },

    "binomial" = {
      if(is.null(size)) {
        size <- max(count)
        warning("size was not given, taken as maximum count")
      }

      chi2 <- function(x)
      {
	p.hat <- dbinom(count, prob = x, size = size)
        expected <- sum(freq) * p.hat
        sum((freq - expected)^2/expected)
      }

      if(is.null(par)) {
        df <- df - 1
        ## ML/MM
	par1 <- weighted.mean(count/size, freq)
	## minChisq
	chi2opt <- optimize(chi2, c(0,1))
	par2 <- chi2opt$minimum
	statistic <- chi2opt$objective
        par <- rbind(par1, par2)
        rownames(par) <- c("ML", "minChisq")
      }
      else {
        if(!is.list(par)) stop("`par' must be a named list")
        if(!(names(par) == "prob"))
	  stop("`par' must specify `prob'")
	par <- matrix(par$prob, ncol = 1)
	rownames(par) <- "fixed"
	statistic <- chi2(par[1,1])
      }
      colnames(par) <- "prob"
      p.hat <- dbinom(count, prob = par[1,1], size = size)
    },


    "nbinomial" = {

      chi2 <- function(x)
      {
	p.hat <- dnbinom(count, size = x[1], prob = x[2])
        expected <- sum(freq) * p.hat
        sum((freq - expected)^2/expected)
      }

      if(is.null(par)) {
        df <- df - 2

	## MM
	xbar <- weighted.mean(count,freq)
	s2 <- var(rep(count,freq))
	p <- xbar / s2
	n <- xbar^2/(s2 - xbar)
        par1 <- c(n, p)

	## ML
	require(package = MASS)
        par <- fitdistr(rep(count, freq), "negative binomial")$estimate
        par <- par[1]/c(1, sum(par))

	## minChisq
	chi2opt <- optim(par1, chi2)
	par2 <- chi2opt$par
	statistic <- chi2opt$value
        par <- rbind(par, par2, par1)
        rownames(par) <- c("ML", "minChisq", "MM")
      } else {
        if(!is.list(par)) stop("`par' must be a named list")
        if(is.character(all.equal(sum(match(names(par), c("size", "prob"))), 3)))
	  stop("`par' must specify `prob' and `size'")
        par <- matrix(c(par$size, par$prob), nrow = 1)
	rownames(par) <- "fixed"
	statistic <- chi2(par[1,])
      }
      colnames(par) <- c("size", "prob")
      p.hat <- dnbinom(count, size = par[1,1], prob = par[1,2])
    })

    expected <- sum(freq) * p.hat

    statistic <- c(statistic, sum(freq*log(freq/expected)) * 2)
    names(statistic) <- c("Pearson", "Likelihood Ratio")

    RVAL <- list(observed = freq,
                 count = count, fitted = expected,
		 type = type, df = df, estimate = par,
		 statistic = statistic)
    class(RVAL) <- "goodfit"
    RVAL
}

summary.goodfit <- function(object, ...)
{
    df <- object$df
    RVAL <- object$statistic
    RVAL <- cbind(RVAL, c(df,df), pchisq(RVAL, df = df, lower = FALSE))
    colnames(RVAL) <- c("X^2", "df", "P(> X^2)")

    cat(paste("\n\t Goodness-of-fit tests for", object$type, "distribution\n\n"))
    print(RVAL)
}

plot.goodfit <- function(x, ...)
{
  rootogram(x, ...)
}

