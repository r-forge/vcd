## color palettes

rainbow_hcl <- function(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n, ...)
{
  if(n > 0) hcl(seq(start, end, length = n), c = c, l = l, ...)
    else character(0)
}

diverge_hcl <- function(n, h = c(260, 0), c = 100, l = c(90, 50), ...)
{
  if(n < 1) return(character(0))
  h <- rep(h, length.out = 2)
  c <- c[1]
  l <- rep(l, length.out = 2)
  rval <- seq(1, -1, length = n)
  rval <- hcl(h = ifelse(rval > 0, h[1], h[2]),
              c = c * abs(rval),
              l = l[1] + diff(l) * abs(rval),
              ...)
  return(rval)
}

diverge_hsv <- function(n, h = c(2/3, 0), s = 1, v = 1, ...)
{
  if(n < 1) return(character(0))
  h <- rep(h, length.out = 2)
  s <- s[1]
  v <- v[1]
  rval <- seq(-s, s, length = n)
  rval <- hsv(h = ifelse(rval > 0, h[2], h[1]), s = abs(rval), v = v, ...)
  return(rval)
}

heat_hcl <- function(n, h = c(0, 90), c = 100, l = c(50, 90), correct = 5, ...)
{
  if(n < 1) return(character(0))
  h <- rep(h, length.out = 2)
  c <- c[1]
  l <- rep(l, length.out = 2)
  rval <- seq(0, 1, length = n)
  rval <- hcl(h = h[1] + diff(h) * rval,
              c = c * rev(rval)^(1/correct),
              l = l[1] + diff(l) * rval,
              ...)
  return(rval)
}

terrain_hcl <- function(n, h = c(130, 0), c = 80, l = c(60, 100), correct = 10, ...)
  heat_hcl(n, h = h, c = c, l = l, correct = correct, ...)

decrease_hcl <- function(n, h = 260, c = 100, l = c(50, 90), ...)
  heat_hcl(n = n, h = h, c = c, l = l, correct = 1, ...)
