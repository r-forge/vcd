##################################################################
## spacings

spaces.equal <- function(sp = unit(0.5, "lines")) {
  if (!is.unit(sp)) sp <- unit(sp, "lines")
  function(d, condvars = NULL) lapply(d, function(x) unit.rep(sp, x - 1))
}

spaces.dimequal <- function(sp) {
  if (!is.unit(sp)) sp <- unit(sp, "lines")
  function(d, condvars = NULL)
    lapply(seq(along = d), function(i) unit.rep(sp[i], d[[i]] - 1))
}

spaces.increase <- function(start = unit(0.3, "lines"), rate = 1.5) {
  if (!is.unit(start)) start <- unit(start, "lines")
  function(d, condvars = NULL) {
    sp <- start * rev(cumprod(c(1, rep.int(rate, length(d) - 1))))
    lapply(seq(along = d), function(i) unit.rep(sp[i], d[[i]] - 1))
  }
}

spaces.doubledecker <- function(start = unit(0.3, "lines"), rate = 1.8)
  function(d, condvars)
    spaces.conditional(sp = 0, start = start, rate = rate)(d, condvars)

spaces.conditional <- function(sp = unit(0.5, "lines"),
                               start = unit(2, "lines"), rate = 1.8) {
  condfun <- spaces.increase(start, rate)
  equalfun <- spaces.equal(sp)
  equalfun2 <- spaces.equal(start)
  function(d, condvars) {
    ret <- vector("list", length(d))
    ret[condvars] <- if (length(condvars) < 3)
      equalfun2(d[condvars])
    else
      condfun(d[condvars])
    ret[-condvars] <- equalfun(d[-condvars])
    ret
  }
}
