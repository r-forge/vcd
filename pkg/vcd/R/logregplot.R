##library(colorspace)
##library(vcdExtra)
##m = glm(survived ~ age + sex, data = Donner, family = binomial())

logregplot <-
function(model, main = NULL, jitter_factor = 0.1, lwd = 5,
         legendpos = "topright", predvar = NULL, condvar = NULL,
         subset)
{
    if (!inherits(model, "glm"))
        stop("Method requires a model of class 'glm'.")
    if(family(model)$family != "binomial")
        stop("Method requires a glm model with binomial link.")
    term <- terms(model)
    data.classes <- attr(term, "dataClasses")
    nam <- names(data.classes)
    resp <- nam[attr(term, "response")]
    data.classes[resp] <- ""
    if (is.null(predvar))
        predvar <- nam[match("numeric", data.classes)]
    if (is.null(condvar))
        condvar <- nam[match("factor", data.classes)]
    dat <- model$model[order(model$model[,predvar]),]
    if (!missing(subset)) {
        e <- substitute(subset)
        i <- eval(e, dat, parent.frame())
        i <- i & !is.na(i)
        dat <- dat[i,]
    }

    plot(dat[,predvar], dat[,resp], type = "n",
         xlab = predvar, ylab = resp, main = main)

    draw <- function(ind, colband, colline) {
        points(dat[ind, predvar],
               jitter(dat[ind, resp], jitter_factor),
               col = colline)
        pr <- predict(model, dat[ind,], type = "response", se.fit = TRUE)
        polygon(c(dat[ind,predvar], rev(dat[ind,predvar])),
                c(pr$fit -1.96 * pr$se.fit,
                  rev(pr$fit +1.96 * pr$se.fit)),
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
