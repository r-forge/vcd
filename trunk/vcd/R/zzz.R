if(!("package:MASS" %in% search() || require(MASS))) warning("could not load package MASS")
if(!("package:grid" %in% search() || require(grid))) warning("could not load package grid")

if(!exists("pushViewport")) {
  if(!exists("push.viewport")) stop("pushViewport/push.viewport cannot be found")
  pushViewport <- push.viewport
}

if(!exists("popViewport")) {
  if(!exists("pop.viewport")) stop("popViewport/pop.viewport cannot be found")
  popViewport <- pop.viewport
}
