gp.HSVshading <- function(observed, residuals,
                          hue = c(2/3, 0), saturation = c(1, 0), value = c(1, 0.5),
                          interpolate = c(2, 4), lty = 1:2,
                          test = chisq.test, level = 0.95)
{
  res <- as.vector(residuals)

  my.h <- rep(hue, length.out = 2)        ## positive and negative hue
  my.s <- rep(saturation, length.out = 2) ## maximum and minimum saturation
  my.v <- rep(value, length.out = 2)      ## significant and non-significant value
  lty <- rep(lty, length.out = 2)         ## positive and negative lty

  if(is.function(interpolate)) {
    col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
  } else {
    cb <- col.bins <- sort(interpolate)
    interpolate <- function(x) stepfun(cb,  seq(my.s[2], my.s[1], length = length(cb) + 1))(abs(x))
  }

  hue <- ifelse(res > 0, my.h[1], my.h[2])
  saturation <- pmax(pmin(interpolate(res), 1), 0)
  if(is.null(test)) {
    value <- my.v[1]
    p.value <- NULL
  } else {
    p.value <- test(observed)$p.value
    value <- ifelse(p.value < (1-level), my.v[1], my.v[2])
  }

  col <- hsv(hue, saturation, value)
  dim(col) <- dim(residuals)

  ## col.bins
  col.bins <- sort(c(range(res), col.bins, -col.bins, 0))
  col.bins <- col.bins[col.bins <= max(res) & col.bins >= min(res)]

  ## legend.col
  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  hue2 <- ifelse(res2 > 0, my.h[1], my.h[2])
  saturation2 <- pmax(pmin(interpolate(res2), 1), 0)
  y.col <- hsv(hue2, saturation2, value)
  legend.col <- list(pos = y.pos, height = y.height, col = y.col)

  ## lty and lty.bins
  lty <- ifelse(residuals > 0, lty[1], lty[2])
  lty.bins <- sort(c(range(res), 0))

  #Z# only needed if *only* positive or negative residuals could occur:
  #Z# lty.bins <- lty.bins[lty.bins <= max(res) & lty.bins >= min(res)]
  #Z# legend.lty <- NULL
  #Z# if(any(lty.bins < 0)) legend.lty <- c(legend.lty, lty[2])
  #Z# if(any(lty.bins > 0)) legend.lty <- c(legend.lty, lty[1])
  legend.lty <- list(pos = lty.bins[1:2], height = diff(lty.bins), lty = lty[2:1])

  rval <- list(fill = col, lty = lty, legend.col = legend.col, legend.lty = legend.lty, p.value = p.value)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.HCLshading <- function(observed, residuals,
                          hue = c(260, 0), chroma = c(100, 20), luminance = c(90, 50),
                          interpolate = c(2, 4), lty = 1:2,
                          test = chisq.test, level = 0.95)
{
  res <- as.vector(residuals)

  my.h <- rep(hue, length.out = 2)       ## positive and negative hue
  my.c <- rep(chroma, length.out = 2)    ## significant and non-significant maximum chroma
  my.l <- rep(luminance, length.out = 2) ## maximum and minimum luminance
  lty <- rep(lty, length.out = 2)        ## positive and negative lty

  if(is.function(interpolate)) {
    col.bins <- min(res) + diff(range(res)) * ((0:200)/200)
  } else {
    cb <- col.bins <- sort(interpolate)
    interpolate <- function(x) stepfun(cb,  seq(0, 1, length = length(cb) + 1))(abs(x))
  }

  hue <- ifelse(res > 0, my.h[1], my.h[2])
  if(is.null(test)) {
    max.chroma <- my.c[1]
    p.value <- NULL
  } else {
    p.value <- test(observed)$p.value
    max.chroma <- ifelse(p.value < (1-level), my.c[1], my.c[2])
  }
  chroma <- max.chroma * pmax(pmin(interpolate(res), 1), 0)
  luminance <- my.l[1] + diff(my.l) * pmax(pmin(interpolate(res), 1), 0)

  col <- hcl(hue, chroma, luminance)
  dim(col) <- dim(residuals)

  ## col.bins
  col.bins <- sort(c(range(res), col.bins, -col.bins, 0))
  col.bins <- col.bins[col.bins <= max(res) & col.bins >= min(res)]

  ## legend.col
  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  hue2 <- ifelse(res2 > 0, my.h[1], my.h[2])
  chroma2 <- max.chroma * pmax(pmin(interpolate(res2), 1), 0)
  luminance2 <- my.l[1] + diff(my.l) * pmax(pmin(interpolate(res2), 1), 0)
  y.col <- hcl(hue2, chroma2, luminance2)
  legend.col <- list(pos = y.pos, height = y.height, col = y.col)

  ## lty and lty.bins
  lty <- ifelse(residuals > 0, lty[1], lty[2])
  lty.bins <- sort(c(range(res), 0))

  #Z# only needed if *only* positive or negative residuals could occur:
  #Z# lty.bins <- lty.bins[lty.bins <= max(res) & lty.bins >= min(res)]
  #Z# legend.lty <- NULL
  #Z# if(any(lty.bins < 0)) legend.lty <- c(legend.lty, lty[2])
  #Z# if(any(lty.bins > 0)) legend.lty <- c(legend.lty, lty[1])
  legend.lty <- list(pos = lty.bins[1:2], height = diff(lty.bins), lty = lty[2:1])

  rval <- list(fill = col, lty = lty, legend.col = legend.col, legend.lty = legend.lty, p.value = p.value)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.Friendly <- function(observed, residuals,
                        hue = c(2/3, 0), lty = 1:2, interpolate = c(2, 4), fuzz = 0.05)
{
  rval <- gp.HSVshading(observed, residuals, hue = hue, value = 1, lty = lty, interpolate = interpolate, test = NULL)
  #Z# rval$col <- ifelse(rval$lty > 1, hsv(hue[2], 1, 1), hsv(hue[1], 1, 1)) ## as in VCD book
  #Z# rval$col <- ifelse(abs(residuals) > fuzz, rval$col, hsv(0, 1, 0))      ## with FUZZ
  #Z# rval$lty <- ifelse(abs(residuals) > fuzz, rval$lty, 1)                 ## with FUZZ
  return(rval)
}

gp.max <- function(observed, residuals,
                   hue = c(260, 0), chroma = c(100, 20), luminance = c(90, 50),
                   interpolate = c(2, 4), lty = 1, level = c(0.9, 0.99), n = 1000)
{
  obs.test <- pearson.test(observed, n = n, return.distribution = TRUE)
  col.bins <- obs.test$qdist(sort(level))
  rval <- gp.HCLshading(observed, residuals, hue = hue, chroma = chroma, luminance = luminance,
                        interpolate = col.bins, lty = lty, test = NULL)
  rval$p.value <- obs.test$p.value
  return(rval)
}

gp.binary <- function(observed, residuals, col = 1:2)
{
  fill <- ifelse(residuals > 0, col[1], col[2])
  col.bins <- sort(c(range(residuals), 0))
  col.bins <- col.bins[col.bins <= max(residuals) & col.bins >= min(residuals)]

  y.pos <- col.bins[-length(col.bins)]
  y.height <- diff(col.bins)
  res2 <- y.pos + 0.5*y.height
  y.col <- ifelse(res2 > 0, col[1], col[2])
  legend.col <- list(pos = y.pos, height = y.height, col = y.col)

  rval <- list(fill = fill, legend.col = legend.col)
  class(rval) <- c("vcd.gpar", "gpar")
  return(rval)
}

gp.Z <- function(observed, residuals)
{
  gp.HCLshading(observed, residuals, hue = c(130, 30), chroma = c(80, 20), luminance = c(95, 70), lty = 1)
}
