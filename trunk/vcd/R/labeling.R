#################################################################
## labeling

pexpand <- function(par, len, default.value, default.names) {
  nam <- names(par)
  if (is.null(nam))
    default.value <- par
  else if (length(nam[nam == ""])) {
    default.value <- par[nam == ""]
    nam <- nam[nam != ""]
  }
  ret <- rep(default.value, length.out = len)
  if (!is.null(nam)) {
    names(ret) <- default.names
    ret[nam] <- par[nam]
    ret
  } 
  ret
}

labeling.list <- function(gp = gpar(),
                        just = "left",
                        pos = "left",
                        lsep = ": ", sep = " ",
                        offset = unit(c(2, 2), "lines"),
                        varnames = TRUE,
                        cols = 2,
                        ...) 
  function(d, split.vertical, condvars) {
    if (is.table(d))
      d <- dimnames(d)
    ld <- length(d)
    labeling.text(labels = FALSE, varnames = varnames)(d, split.vertical, condvars)
    seekViewport("marginBottom")
    pos <- unit(switch(pos, left = 0, centre = 0.5, 1) / cols, "npc")
    ind <- split(seq(ld), rep.int(seq(cols), ceiling(ld / cols))[seq(ld)])
    
    for (i in seq(along = ind))
      grid.text(x = offset[1] + pos + unit((i - 1) / cols, "npc"),
                y = unit(1, "npc") - offset[2],
                paste(names(d[ind[[i]]]),
                      sapply(d[ind[[i]]], paste, collapse = sep),
                      sep = lsep,
                      collapse = "\n"
                      ),
                just = c(just, "top"),
                gp = gp
                )
  }
class(labeling.list) <- "vcdLabeling"

labeling.conditional <- function(...)
  function (d, split.vertical, condvars) {
    if (is.table(d))
      d <- dimnames(d)
    v <- rep.int(TRUE, length(d))
    v[condvars] <- FALSE
    labeling.text(labels = !v, ...)(d, split.vertical, condvars)
    labeling.cells(labels = v, ...)(d, split.vertical, condvars)
  }
class(labeling.conditional) <- "vcdLabeling"

labeling.cells <- function(labels = TRUE, varnames = TRUE,
                         abbreviate.labels = FALSE, abbreviate.varnames = FALSE,
                         gp = gpar(), lsep = ": ", lcollapse = "\n",
                         just = "centre", pos = "centre", rot = 0,
                         margin = unit(0.5, "lines"), clip.cells = TRUE,
                         text = NULL, ...)
  function(d, split.vertical, condvars) {
    if (is.table(d))
      d <- dimnames(d)
    dn <- names(d)
    ld <- length(d)

    ## expand parameters
    if (length(pos) < 2) pos <- c(pos, pos)
    labels <- pexpand(labels, ld, TRUE, dn)
    varnames <- pexpand(varnames, ld, TRUE, dn)
    abbreviate.labels <- pexpand(abbreviate.labels, ld, FALSE, dn)
    abbreviate.varnames <- pexpand(abbreviate.varnames, ld, FALSE, dn)

    ## margin
    if (!is.unit(margin))
      margin <- unit(margin, "lines")
    
    prvars <- ifelse(abbreviate.varnames,
                     sapply(seq(along = dn),
                            function(i) abbreviate(dn[i], abbreviate.varnames[i])),
                     dn)
    prvars <- ifelse(varnames, paste(prvars, lsep, sep = ""), "")

    ## draw labels
    split <- function(vind = 1, labs = c()) {
      n <- d[[vind]]
      for (labind in seq(along = n)) {
        lab <- c(labs, n[labind])
        names(lab) <- names(d)[1:vind]
        mlab <- paste("cell", paste(dn[1:vind], lab, sep = ".", collapse = ".."),
                      sep = "..")

        if (vind < ld)
          split(vind + 1, lab)
        else {
          seekViewport(mlab)
          pushViewport(viewport(width = max(unit(0, "npc"), unit(1, "npc") - 2 * margin),
                                height = unit(1, "npc") - 2 * margin,
                                clip = clip.cells))
          txt <- if (!is.null(text)) {
            lab <- lab[names(dimnames(text))]
            do.call("[", c(list(text), as.list(lab)))
          } else {
            prlab <- ifelse(abbreviate.labels,
                            sapply(seq(along = lab),
                                   function(i) abbreviate(lab[i], abbreviate.labels[i])),
                            lab)
            prlab <- prlab[labels[1:ld]]
            paste(prvars[labels[1:ld]], prlab, sep = "", collapse = lcollapse) 
          } 
          
          grid.text(if(!is.na(txt)) txt,
                    x = switch(pos[1], left =, top = 0, centre = 0.5, 1),
                    y = switch(pos[2], left =, top = 1, centre = 0.5, 0),
                    gp = gp, just = just, rot = rot)
          popViewport()
        }
      }
    }
    split()
    
}
class(labeling.cells) <- "vcdLabeling"

labeling.text <- function(labels = TRUE, varnames = labels,
                        tl.labels = NULL, tl.varnames = NULL, 
                        gp.labels = gpar(fontsize = 12),
                        gp.varnames = gpar(fontsize = 12, fontface = 2),
                        rot.labels = c(0, 90, 0, 90),
                        rot.varnames = c(0, 90, 0, 90),
                        pos.labels = "centre", pos.varnames = "centre",
                        just.labels = "centre", just.varnames = pos.varnames,
                        boxes = FALSE, fill.boxes = NULL,
                        
                        labbl.varnames = NULL,
                        labels.varnames = FALSE, sep = ": ",
                        
                        abbreviate = FALSE, rep = TRUE,
                        clip = FALSE, ...
                        )
  function(d, split.vertical, condvars) {
    if (is.table(d))
      d <- dimnames(d)
    dn <- names(d)
    ld <- length(d)

    ## expand parameters
    clip <- pexpand(clip, ld, TRUE, dn)
    labels <- pexpand(labels, ld, TRUE, dn)
    labels.varnames <- pexpand(labels.varnames, ld, FALSE, dn)
    pos.labels <- pexpand(pos.labels, 4, "centre", c("top", "right", "bottom", "left"))
    just.labels <- pexpand(just.labels, 4, "centre", c("top", "right", "bottom", "left"))

    ## tl.labels
    def <- logical()
    def[split.vertical] <- rep(c(TRUE, FALSE), length.out = sum(split.vertical))
    def[!split.vertical] <- rep(c(TRUE, FALSE), length.out = sum(!split.vertical))
    tl.labels <- if (is.null(tl.labels)) 
      def
    else
      pexpand(tl.labels, ld, def, dn)

    ## rep labels
    rep <- pexpand(rep, ld, TRUE, dn)
    printed <- lapply(d, function(i) rep.int(FALSE, length(i)))
    
    ## abbreviate
    abbreviate <- pexpand(abbreviate, ld, FALSE, dn)
    labs <- d
    for (i in seq(along = d))
      if (abbreviate[i])
        labs[[i]] <- abbreviate(labs[[i]], abbreviate[i])

    ## gp.labels
    if (inherits(gp.labels, "gpar"))
      gp.labels <- list(gp.labels)
    gp.labels <- pexpand(gp.labels, ld, gpar(fontsize = 12), dn)

    ## rot.labels: top/right/bottom/left
    rot.labels <- pexpand(rot.labels, 4, c(0, 90, 0, 90),
                          c("top", "right", "bottom", "left"))

    ## varnames
    varnames <- pexpand(varnames, ld, labels, dn)

    ## gp.varnames: top/right/bottom/left!
    if (inherits(gp.varnames, "gpar"))
      gp.varnames <- list(gp.varnames)
    gp.varnames <- pexpand(gp.varnames, 4, gpar(fontsize = 12, fontface = 2),
                           c("top", "right", "bottom", "left"))

    ## rot.varnames: top/right/bottom/left!
    rot.varnames <- pexpand(rot.varnames, 4, c(0, 90, 0, 90),
                          c("top", "right", "bottom", "left"))

    ## pos.varnames: top/right/bottom/left!
    pos.varnames <- pexpand(pos.varnames, 4, "centre",
                           c("top", "right", "bottom", "left"))

    ## just.varnames: top/right/bottom/left!
    just.varnames <- pexpand(just.varnames, 4, pos.varnames,
                             c("top", "right", "bottom", "left"))

    ## tl.varnames
    if (is.null(tl.varnames) && is.null(labbl.varnames))
      tl.varnames <- tl.labels
    tl.varnames <- pexpand(tl.varnames, ld, tl.labels, ld)

    ## labbl.varnames
    if (!is.null(labbl.varnames))
      labbl.varnames <- pexpand(labbl.varnames, ld, TRUE, ld)

    ## boxes
    boxes <- pexpand(boxes, ld, FALSE, dn)

    ## fill.boxes
    if (is.null(fill.boxes))
      fill.boxes <- lapply(sapply(d, length),
                           function(i) gray(0.3 + 0.4 * rev(seq(i)) / i))

    ## precompute spaces
    lsp <- tsp <- bsp <- rsp <- 0
    labsp <- rep.int(0, ld)
    for (i in seq(along = dn)[tl.labels & labels])
      labsp[i] <- if (split.vertical[i])
        tsp <- tsp + 1
      else
        lsp <- lsp - 1
    for (i in rev(seq(along = dn)[!tl.labels & labels]))
      labsp[i] <- if (split.vertical[i])
        bsp <- bsp - 1
      else
        rsp <- rsp + 1
    
    if(is.null(labbl.varnames)) {
    ## varnames in the outer margin  
      ## compute axis names
      tt <- bt <- lt <- rt <- ""
      for (i in seq(along = dn))
        if (varnames[i]) {
          if (split.vertical[i]) {
            if (tl.varnames[i])
              tt <- paste(tt, dn[i], sep = if (tt == "") "" else " / ")
            else
              bt <- paste(bt, dn[i], sep = if (bt == "") "" else " / ")
          } else {
            if (tl.varnames[i])
              lt <- paste(lt, dn[i], sep = if (lt == "") "" else " / ")
            else
              rt <- paste(rt, dn[i], sep = if (rt == "") "" else " / ")
          }
        }

      ## draw axis names
      if (tt != "")
        grid.text(tt, y = unit(1, "npc") + unit(tsp + 1, "lines"),
                  x = switch(pos.varnames[1], left =, bottom = 0, center =, centre = 0.5, 1),
                  rot = rot.varnames[1], just = just.varnames[1], gp = gp.varnames[[1]])
      if (bt != "")
        grid.text(bt, y = unit(bsp - 1, "lines"),
                  x = switch(pos.varnames[3], left =, bottom = 0, center =, centre = 0.5, 1),
                  rot = rot.varnames[3], just = just.varnames[3], gp = gp.varnames[[3]])
      if (lt != "")
        grid.text(lt, x = unit(lsp - 1, "lines"),
                  y = switch(pos.varnames[4], left =, bottom = 0, center =, centre = 0.5, 1),
                  rot = rot.varnames[4], just = just.varnames[4], gp = gp.varnames[[4]])
      if (rt != "")
        grid.text(rt, x = unit(1, "npc") + unit(rsp + 1, "lines"),
                  y = switch(pos.varnames[2], left =, bottom = 0, center =, centre = 0.5, 1),
                  rot = rot.varnames[2], just = just.varnames[2], gp = gp.varnames[[2]])
    } else {
    ## varnames beneath labels
      for (i in seq(along = dn))
        if (varnames[i]) {
          if (split.vertical[i]) {
            if (tl.labels[i]) {
              if (labbl.varnames[i]) {
                grid.text(dn[i], y = unit(1, "npc") + unit(1 + tsp - labsp[i], "lines"),
                          x = unit(-0.5, "lines"),
                          just = "right", gp = gpar(fontface = 2))
              } else {
                 grid.text(dn[i], y = unit(1, "npc") + unit(1 + tsp - labsp[i], "lines"),
                           x = unit(1, "npc") + unit(0.5, "lines"),
                           just = "left", gp = gpar(fontface = 2))
              }
            } else {
              if (labbl.varnames[i]) {
                grid.text(dn[i], y = unit(labsp[i], "lines"),
                          x = unit(-0.5, "lines"), just = "right",
                          gp = gpar(fontface = 2))
              } else {
                grid.text(dn[i], unit(labsp[i], "lines"),
                          x = unit(1, "npc") + unit(0.5, "lines"),
                          just = "left", gp = gpar(fontface = 2))
              }
            }
          } else {
            if (tl.labels[i]) {
              if (labbl.varnames[i]) {
                grid.text(dn[i], x = unit(lsp - 1 - labsp[i], "lines"),
                          y = unit(-0.5, "lines"), just = "right", rot = 90,
                          gp = gpar(fontface = 2))
              } else {
                grid.text(dn[i], x = unit(lsp - 1 - labsp[i], "lines"),
                          y = unit(1, "npc") + unit(0.5, "lines"),
                          just = "left", rot = 90, gp = gpar(fontface = 2))
              }
            } else {
              if (labbl.varnames[i]) {
                grid.text(dn[i], x = unit(1, "npc") + unit(labsp[i], "lines"),
                          y = unit(-0.5, "lines"),
                          just = "right", rot = 90, gp = gpar(fontface = 2))
              } else {
                grid.text(dn[i], x = unit(1, "npc") + unit(labsp[i], "lines"),
                          y = unit(1, "npc") + unit(0.5, "lines"),
                          just = "left", rot = 90, gp = gpar(fontface = 2))
              }
            }
          }
        }
    }

    ## draw labels
    split <- function(vind = 1, root = "cell",
                      left = TRUE, right = TRUE, top = TRUE, bottom = TRUE) {
      n <- d[[vind]]
      vl <- length(n)
      sp <- split.vertical[vind]
      labseq <- seq(along = n)
      if (!sp) labseq <- rev(labseq)
      
      for (labind in labseq) {
        mlab <- paste(root, "", dn[vind], n[labind], sep = ".")
        if (labels[vind] && (rep[vind] || !printed[[vind]][labind])) {
          lab <- labs[[vind]][labind]
          if (labels.varnames[i])
            lab <- paste(dn[vind], lab, sep = sep)
          if (sp) {
            if (tl.labels[vind]) {
              if (top) {
                seekViewport(mlab)
                if (clip[vind])
                  pushViewport(viewport(height = unit(1, "npc") +
                                        unit(2 * (2 + tsp - labsp[vind]), "lines"),
                                        clip = "on"))
                if (boxes[vind])
                  grid.rect(height = unit(0.8, "lines"),
                            y = unit(1, "npc") +
                                unit(1 + tsp - labsp[vind] - (2 + tsp - labsp[vind]) * clip[vind], "lines"),
                            gp = gpar(fill = fill.boxes[[vind]][labind]))
                grid.text(lab,
                          y = unit(1, "npc") +
                              unit(1 + tsp - labsp[vind] - (2 + tsp - labsp[vind]) * clip[vind], "lines"),
                          x = unit(0.15 * switch(pos.labels[1], left =, bottom = 1, center =, centre = 0, -1) * boxes[vind], "lines") +
                          unit(switch(pos.labels[1], left =, bottom = 0, center =, centre = 0.5, 1), "npc"),
                          rot = rot.labels[1], just = just.labels[1],
                          gp = gp.labels[[vind]])
                if (clip[vind]) popViewport()
                printed[[vind]][labind] <<- TRUE
              }
            } else {
              if (bottom) {
                seekViewport(mlab)
                if (clip[vind])
                  pushViewport(viewport(height = unit(1, "npc") +
                                        unit(2 * (1 + abs(labsp[vind])), "lines"),
                                        clip = "on"))
                if (boxes[vind])
                  grid.rect(height = unit(0.8, "lines"),
                            y = unit(labsp[vind] + (1 + abs(labsp[vind])) * clip[vind], "lines"),
                            gp = gpar(fill = fill.boxes[[vind]][labind]))

                grid.text(lab,
                          y = unit(labsp[vind] + (1 + abs(labsp[vind])) * clip[vind], "lines"),
                          x = unit(0.15 * switch(pos.labels[3], left =, bottom = 1, center =, centre = 0, -1) * boxes[vind], "lines") +
                              unit(switch(pos.labels[3], left =, bottom = 0, center =, centre = 0.5, 1), "npc"),
                          rot = rot.labels[3], just = just.labels[3],
                          gp = gp.labels[[vind]])
                if (clip[vind]) popViewport()
                printed[[vind]][labind] <<- TRUE
              }
            }
          } else {
            if (tl.labels[vind]) {
              if (left) {
                seekViewport(mlab)
                if (clip[vind])
                  pushViewport(viewport(width = unit(1, "npc") +
                                        unit(2 * (2 - lsp + labsp[vind]), "lines"),
                                        clip = "on"))
                if (boxes[vind])
                  grid.rect(width = unit(0.8, "lines"),
                            x = unit(lsp - 1 - labsp[vind] + (2 - lsp + labsp[vind]) * clip[vind], "lines"),
                            gp = gpar(fill = fill.boxes[[vind]][labind]))

                grid.text(lab,
                          x = unit(lsp - 1 - labsp[vind] + (2 - lsp + labsp[vind]) * clip[vind], "lines"),
                          y = unit(0.15 * switch(pos.labels[4], left =, bottom = 1, centre = 0, -1) * boxes[vind], "lines") +
                          unit(switch(pos.labels[4], left =, bottom = 0, center =, centre = 0.5, 1), "npc"),
                          rot = rot.labels[4], just = just.labels[4],
                          gp = gp.labels[[vind]])

                if (clip[vind]) popViewport()
                printed[[vind]][labind] <<- TRUE
              }
            } else {
              if (right) {
                seekViewport(mlab)
                if (clip[vind])
                  pushViewport(viewport(width = unit(1, "npc") +
                                        unit(2 * (1 + abs(labsp[vind])), "lines"),
                                        clip = "on"))
                if (boxes[vind])
                  grid.rect(width = unit(0.8, "lines"),
                            x = unit(1, "npc") +
                                unit(labsp[vind] - (1 + abs(labsp[vind])) * clip[vind], "lines"),
                            gp = gpar(fill = fill.boxes[[vind]][labind]))
                grid.text(lab,
                          x = unit(1, "npc") + unit(0.1, "lines") +
                              unit(labsp[vind] - (1 + abs(labsp[vind])) * clip[vind], "lines"),
                          y = unit(0.15 * switch(pos.labels[2], left =, bottom = 1, center =, centre = 0, -1) * boxes[vind], "lines") +
                              unit(switch(pos.labels[2], left =, bottom = 0, center =, centre = 0.5, 1), "npc"),
                          rot = rot.labels[2], just = just.labels[2],
                          gp = gp.labels[[vind]])

                if (clip[vind]) popViewport()
                printed[[vind]][labind] <<- TRUE
              }
            }
          }
        }
        
        if (vind < ld) Recall(vind + 1, mlab,
                             if (sp) left && labind == 1 else left,
                             if (sp) right && labind == vl else right,
                             if (!sp) top && labind == 1 else top,
                             if (!sp) bottom && labind == vl else bottom)
      }
    }
    split()
    
  }
class(labeling.text) <- "vcdLabeling"

labeling.doubledecker <- function(labels = "bottom", ...)
  function(d, split.vertical, condvars) {
    if (is.table(d))
      d <- dimnames(d)
    labeling.text(boxes = c(rep.int(TRUE, length(d) - 1), FALSE),
                clip = c(rep.int(TRUE, length(d) - 1), FALSE),
                labbl.varnames = FALSE,
                rot.labels = rep.int(0, 4),
                pos.labels = c("left", "center", "left", "center"),
                just.labels = c("left", "left", "left", "center"),
                varnames = c(c(rep.int(TRUE, length(d) - 1), FALSE)),
                tl.labels = c(rep.int(labels == "top", length(d) - 1), FALSE))(d, split.vertical, condvars)
    seekViewport("marginRight")
    grid.text(names(d)[length(d)],
              x = unit(0.5, "lines"), y = unit(1, "npc"), just = c("left","top"),
              gp = gpar(fontface = 2))
  }
class(labeling.doubledecker) <- "vcdLabeling"

labeling.left <- function(tl.labels = TRUE, clip = TRUE, pos.varnames = "left",
                        pos.labels = "left", just.labels = "left", ...)
  labeling.text(tl.labels = tl.labels, clip = clip, pos.varnames = pos.varnames,
              pos.labels = pos.labels, just.labels = just.labels, ...)
class(labeling.left) <- "vcdLabeling"

labeling.boxed <- function(tl.labels = TRUE, boxes = TRUE, clip = TRUE, pos.labels = "center", ...)
  labeling.text(tl.labels = tl.labels, boxes = boxes, clip = clip, pos.labels = pos.labels, ...)
class(labeling.boxed) <- "vcdLabeling"

labeling.augsburg <- function(tl.labels = FALSE, boxes = TRUE, clip = TRUE, pos.labels = "left", just.labels = "left", labbl.varnames = FALSE, ...)
  labeling.text(tl.labels = tl.labels, boxes = boxes, clip = clip, pos.labels = pos.labels, labbl.varnames = labbl.varnames, just.labels = just.labels, ...)
class(labeling.augsburg) <- "vcdLabeling"
