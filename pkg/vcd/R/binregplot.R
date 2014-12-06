binreg_plot <-
function(model, main = NULL, jitter_factor = 0.1, lwd = 5,
         pch = NULL, cex = 1, xlab = NULL, ylab = NULL,
         legend = TRUE, legendpos = "topright",
         labels = FALSE, labels.place = "right", labels.pos = 1,
         labels.offset = 0.5,
         conf.level = 0.95,
         predvar = NULL, condvar = NULL, ylevel = NULL,
         col.band = NULL, col.line = NULL,
         type = c("response", "link"), ..., subset)
{
    if (!inherits(model, "glm"))
        stop("Method requires a model of class 'glm'.")
    type <- match.arg(type)
    term <- terms(model)
    data.classes <- attr(term, "dataClasses")
    nam <- names(data.classes)
    resp <- nam[attr(term, "response")]
    if (is.null(pch))
        pch <- c(19,15,17, 1:14, 16, 18, 20:25)
    data.classes[resp] <- ""
    if (is.null(predvar))
        predvar <- nam[match("numeric", data.classes)]
    if (is.null(xlab))
        xlab <- predvar
    if (is.null(condvar))
        condvar <- nam[match("factor", data.classes)]
    dat <- model$model[order(model$model[,predvar]),]
    if (!missing(subset)) {
        e <- substitute(subset)
        i <- eval(e, dat, parent.frame())
        i <- i & !is.na(i)
        dat <- dat[i,]
    }

    ylim <- if (type == "response")
        c(0,1)
    else
        range(predict(model, dat, type = "link"))

    if (is.null(ylevel))
        ylevel <- if(is.factor(dat[,resp]))
                      levels(dat[,resp])[2]
                  else
                      1

    lev <- levels(dat[,condvar])

    if (is.null(ylab))
        ylab <- if (type == "response")
                    paste0("P(", resp, ")")
                else
                    paste0("logit(", resp, ")")
    plot(dat[,predvar], dat[,resp],
         type = "n",
         xlab = xlab, ylab = ylab, main = main, ylim = ylim, ...)

    quantile <- qnorm((1 + conf.level) / 2)
    draw <- function(ind, colband, colline, pch, label) {
        points(dat[ind, predvar],
               jitter(ylim[1 + (dat[ind, resp] == ylevel)],
                      jitter_factor),
               pch = pch, cex = cex, col = colline)
        pr <- predict(model, dat[ind,], type = type, se.fit = TRUE)
        polygon(c(dat[ind, predvar], rev(dat[ind, predvar])),
                c(pr$fit - quantile * pr$se.fit,
                  rev(pr$fit + quantile * pr$se.fit)),
                col = colband, border = NA)
        lines(dat[ind, predvar],
              pr$fit,
              lwd = lwd,
              col = colline)
        if (labels) {
            x = switch(labels.place,
                       left = dat[ind, predvar][1],
                       right = dat[ind, predvar][length(dat[ind, predvar])])
            y = switch(labels.place,
                       left = pr$fit[1],
                       right = pr$fit[length(pr$fit)])
            text(x, y, labels = label, pos = labels.pos,
                 offset = labels.offset, col = colline)
        }
    }

    if (is.null(condvar) || is.na(condvar) ||
        is.logical(condvar) && !condvar[1]) {
        if (is.null(col.band))
            col.band <- rainbow_hcl(1, alpha = 0.2)
        if (is.null(col.line))
            col.line <- rainbow_hcl(1, l = 50)
        draw(1:nrow(dat),
             col.band,
             col.line,
             pch[1])
    } else {
        llev <- length(lev)
        pch <- rep(pch, length.out = llev)
        if (is.null(col.band))
            col.band <- rainbow_hcl(llev, alpha = 0.2)
        if (is.null(col.line))
            col.line <- rainbow_hcl(llev, l = 50)
        for (i in seq_along(lev)) {
            ind <- dat[,condvar] == lev[i]
            draw(ind, col.band[i], col.line[i], pch[i], lev[i])
        }
        if (legend)
            legend(legendpos,
                   legend = lev,
                   col = col.line,
                   lwd = lwd,
                   title.adj = 0.15,
                   title = condvar)
    }




}
