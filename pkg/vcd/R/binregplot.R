binreg_plot <-
function(model, main = NULL, jitter_factor = 0.1, lwd = 5,
         pch = NULL, cex = 1, xlab = NULL, ylab = NULL,
         legend = TRUE, legend_pos = "topright",
         labels = FALSE, labels_place = "right", labels_pos = 1,
         labels_offset = 0.5,
         conf_level = 0.95,
         pred_var = NULL, cond_vars = NULL, ylevel = NULL,
         col_band = NULL, col_line = NULL,
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
    if (is.null(pred_var))
        pred_var <- nam[match("numeric", data.classes)]
    if (is.null(xlab))
        xlab <- pred_var
    dat <- model$model[order(model$model[,pred_var]),]
    if (!missing(subset)) {
        e <- substitute(subset)
        i <- eval(e, dat, parent.frame())
        i <- i & !is.na(i)
        dat <- dat[i,]
    }
    if (is.null(cond_vars)) {
        cond_vars <- nam[data.classes %in% "factor"]
        sing <- sapply(dat, function(i) all(i == i[1]))
        if (any(sing)) {
            cond_vars <- setdiff(cond_vars, names(sing)[sing])
            if(length(cond_vars) < 1)
                cond_vars <- FALSE
        }
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

    if (is.na(cond_vars) || is.logical(cond_vars) && !cond_vars[1])
        cond_vars <- NULL
    if (is.null(cond_vars)) {
        labels <- legend <- FALSE
    } else {
        if (length(cond_vars) > 1) {
            cross <- paste(cond_vars, collapse = "*")
            dat[,cross] <- factor(apply(dat[,cond_vars], 1, paste, collapse = ":"))
            cond_vars <- cross
        }
        lev <- levels(dat[,cond_vars])
    }

    if (is.null(ylab))
        ylab <- if (type == "response")
                    paste0("P(", resp, ")")
                else
                    paste0("logit(", resp, ")")
    plot(dat[,pred_var], dat[,resp],
         type = "n",
         xlab = xlab, ylab = ylab, main = main, ylim = ylim, ...)

    quantile <- qnorm((1 + conf_level) / 2)
    draw <- function(ind, colband, colline, pch, label) {
        points(dat[ind, pred_var],
               jitter(ylim[1 + (dat[ind, resp] == ylevel)],
                      jitter_factor),
               pch = pch, cex = cex, col = colline)
        pr <- predict(model, dat[ind,], type = type, se.fit = TRUE)
        polygon(c(dat[ind, pred_var], rev(dat[ind, pred_var])),
                c(pr$fit - quantile * pr$se.fit,
                  rev(pr$fit + quantile * pr$se.fit)),
                col = colband, border = NA)
        lines(dat[ind, pred_var],
              pr$fit,
              lwd = lwd,
              col = colline)
        if (labels) {
            x = switch(labels_place,
                       left = dat[ind, pred_var][1],
                       right = dat[ind, pred_var][length(dat[ind, pred_var])])
            y = switch(labels_place,
                       left = pr$fit[1],
                       right = pr$fit[length(pr$fit)])
            text(x, y, labels = label, pos = labels_pos,
                 offset = labels_offset, col = colline)
        }
    }

    if (is.null(cond_vars) || is.na(cond_vars) ||
        is.logical(cond_vars) && !cond_vars[1]) {
        if (is.null(col_band))
            col_band <- rainbow_hcl(1, alpha = 0.2)
        if (is.null(col_line))
            col_line <- rainbow_hcl(1, l = 50)
        draw(1:nrow(dat),
             col_band,
             col_line,
             pch[1])
    } else {
        llev <- length(lev)
        pch <- rep(pch, length.out = llev)
        if (is.null(col_band))
            col_band <- rainbow_hcl(llev, alpha = 0.2)
        if (is.null(col_line))
            col_line <- rainbow_hcl(llev, l = 50)
        for (i in seq_along(lev)) {
            ind <- dat[,cond_vars] == lev[i]
            draw(ind, col_band[i], col_line[i], pch[i], lev[i])
        }
        if (legend)
            legend(legend_pos,
                   legend = lev,
                   col = col_line,
                   lwd = lwd,
                   title.adj = 0.15,
                   title = cond_vars)
    }




}
