logregplot <-
function(model, main = NULL, jitter_factor = 0.1, lwd = 5,
         pch = 19, cex = 1, xlab = NULL, ylab = NULL,
         legendpos = "topright", conf.level = 0.95,
         predvar = NULL, condvar = NULL, ylevel = NULL,
         type = c("response", "link"), ..., subset)
{
    if (!inherits(model, "glm"))
        stop("Method requires a model of class 'glm'.")
    type <- match.arg(type)
    term <- terms(model)
    data.classes <- attr(term, "dataClasses")
    nam <- names(data.classes)
    resp <- nam[attr(term, "response")]
    if (is.null(ylab))
        ylab <- resp
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
    plot(dat[,predvar], dat[,resp],
         type = "n",
         xlab = xlab, ylab = ylab, main = main, ylim = ylim, ...)

    quantile <- qnorm((1 + conf.level) / 2)
    draw <- function(ind, colband, colline) {
        points(dat[ind, predvar],
               jitter(ylim[1 + (dat[ind,resp] == ylevel)],
                      jitter_factor),
               pch = pch, cex = cex, col = colline)
        pr <- predict(model, dat[ind,], type = type, se.fit = TRUE)
        polygon(c(dat[ind,predvar], rev(dat[ind,predvar])),
                c(pr$fit - quantile * pr$se.fit,
                  rev(pr$fit + quantile * pr$se.fit)),
                col = colband, border = NA)
        lines(dat[ind, predvar],
              pr$fit,
              lwd = lwd,
              col = colline)
    }

    if (is.null(condvar) || is.na(condvar)) {
        draw(1:nrow(dat),
             rainbow_hcl(1, alpha = 0.2,),
             rainbow_hcl(1, l = 50))
    } else {
        lev <- levels(dat[,condvar])
        colline <- rainbow_hcl(length(lev), l = 50)
        colband <- rainbow_hcl(length(lev), alpha = .2)
        for (i in seq_along(lev)) {
            ind <- dat[,condvar] == lev[i]
            draw(ind, colband[i], colline[i])
        }
        legend(legendpos,
               legend = lev,
               col = colline,
               lwd = lwd,
               title.adj = 0.15,
               title = condvar)
    }




}
