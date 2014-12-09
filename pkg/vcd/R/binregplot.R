binreg_plot <-
function(model, main = NULL, xlab = NULL, ylab = NULL,
         xlim = NULL, ylim = NULL,
         pred_var = NULL, cond_vars = NULL, base_level = NULL, subset,
         type = c("response", "link"), conf_level = 0.95,
         pch = NULL, cex = 0.6, jitter_factor = 0.1,
         lwd = 5, col_lines = NULL, col_bands = NULL,
         legend = TRUE, legend_pos = NULL, legend_inset = c(0, 0.1),
         legend_vgap = unit(0.5, "lines"),
         labels = FALSE, labels_side = c("right", "left"), labels_pos = c("left","center"),
         labels_offset = c(0.01, 0),
         gp_main = gpar(fontface = "bold", fontsize = 14),
         gp_legend_frame = gpar(lwd = 1, col = "black"),
         gp_legend_title = gpar(fontface = "bold"),
         newpage = TRUE, pop = TRUE)
{
    if (!inherits(model, "glm"))
        stop("Method requires a model of class 'glm'.")
    type <- match.arg(type)
    labels_side <- match.arg(labels_side)

    ## extract data from model
    term <- terms(model)
    data.classes <- attr(term, "dataClasses")
    nam <- names(data.classes)

    ## determine response
    resp <- nam[attr(term, "response")]
    data.classes[resp] <- ""

    ## determine numeric predictor (take first)
    if (is.null(pred_var))
        pred_var <- nam[match("numeric", data.classes)]

    ## sort observations using order of numeric predictor
    dat <- model$model[order(model$model[,pred_var]),]

    ## apply subset argument, if any
    if (!missing(subset)) {
        e <- substitute(subset)
        i <- eval(e, dat, parent.frame())
        i <- i & !is.na(i)
        dat <- dat[i,]
    }

    ## determine conditioning variables. Remove all those with only one level observed.
    if (is.null(cond_vars)) {
        cond_vars <- nam[data.classes %in% "factor"]
        sing <- sapply(dat, function(i) all(i == i[1]))
        if (any(sing))
            cond_vars <- setdiff(cond_vars, names(sing)[sing])
        if(length(cond_vars) < 1)
            cond_vars <- NULL
    } else
        if (is.na(cond_vars) || is.logical(cond_vars) && !cond_vars[1])
            cond_vars <- NULL

    ## set y axis limits - either probability or logit scale
    if(is.null(ylim))
        ylim <- if (type == "response")
                    c(0,1)
                else
                    range(predict(model, dat, type = "link"))
    ## allow for some cosmetic extra space
    ylimaxis <- ylim + c(-1, 1) * diff(ylim) * 0.04

   if(is.null(xlim))
        xlim <- range(dat[,pred_var])
    xlimaxis <- xlim + c(-1, 1) * diff(xlim) * 0.04

    ## set default base level ("no effect") of response to first level/0
    if (is.null(base_level))
        base_level <- if(is.factor(dat[,resp]))
                          levels(dat[,resp])[1]
                      else
                          0

    ## determine labels of conditioning variables, if any
    if (is.null(cond_vars)) {
        labels <- legend <- FALSE
    } else {
        ## compute cross-factors for more than two conditioning variables
        if (length(cond_vars) > 1) {
            cross <- paste(cond_vars, collapse = " x ")
            dat[,cross] <- factor(apply(dat[,cond_vars], 1, paste, collapse = " : "))
            cond_vars <- cross
        }
        lev <- levels(dat[,cond_vars])
    }

    ## set x- and y-lab
    if (is.null(xlab))
        xlab <- pred_var
    if (is.null(ylab))
        ylab <- if (type == "response")
                    paste0("P(", resp, ")")
                else
                    paste0("logit(", resp, ")")

    ## rearrange default plot symbol palette
    if (is.null(pch))
        pch <- c(19,15,17, 1:14, 16, 18, 20:25)

    ## determine normal quantile for confidence band
    quantile <- qnorm((1 + conf_level) / 2)

    ## determine default legend position, given the curve's slope
    ## (positive -> topleft, negative -> topright)
    if (is.null(legend_pos))
        legend_pos <-
            if (coef(model)[pred_var] > 0)
                "topleft"
            else
                "topright"

    ## work horse for drawing points, fitted curve and confidence band
    draw <- function(ind, colband, colline, pch, label) {
        ## plot observed data as points on top or bottom
        grid.points(unit(dat[ind, pred_var], "native"),
                    unit(jitter(ylim[1 + (dat[ind, resp] != base_level)],
                                 jitter_factor), "native"),
                    pch = pch, size = unit(cex, "char"), gp = gpar(col = colline),
                    default.units = "native"
                    )

        ## confidence band
        pr <- predict(model, dat[ind,], type = type, se.fit = TRUE)
        grid.polygon(unit(c(dat[ind, pred_var], rev(dat[ind, pred_var])), "native"),
                     unit(c(pr$fit - quantile * pr$se.fit,
                            rev(pr$fit + quantile * pr$se.fit)), "native"),
                     gp = gpar(fill = colband, col = NA))
        grid.lines(unit(dat[ind, pred_var], "native"),
                   unit(pr$fit, "native"),
                   gp = gpar(col = colline, lwd = lwd))

        ## add labels, if any
        if (labels) {
            x = switch(labels_side,
                       left = dat[ind, pred_var][1],
                       right = dat[ind, pred_var][length(dat[ind, pred_var])])
            y = switch(labels_side,
                       left = pr$fit[1],
                       right = pr$fit[length(pr$fit)])
            grid.text(x = unit(x, "native") + unit(labels_offset[1], "npc"),
                      y = unit(y, "native") + unit(labels_offset[2], "npc"),
                      label = label,
                      just = labels_pos,
                      gp = gpar(col = colline))
        }
    }

    ## determine colors and plot symbols
    llev <- if (is.null(cond_vars)) 1 else length(lev)
    pch <- rep(pch, length.out = llev)
    if (is.null(col_bands))
        col_bands <- rainbow_hcl(llev, alpha = 0.2)
    if (is.null(col_lines))
        col_lines <- rainbow_hcl(llev, l = 50)

    ## set up plot region, similar to plot.xy()
    if (newpage) grid.newpage()
    pushViewport(plotViewport(xscale = xlimaxis, yscale = ylimaxis, default.units = "native"))
    grid.rect(gp = gpar(fill = "transparent"))
    grid.xaxis()
    grid.yaxis()
    grid.text(xlab, y = unit(-3.5, "lines"))
    grid.text(ylab, x = unit(-3, "lines"), rot = 90)
    grid.text(main, y = unit(1, "npc") + unit(2, "lines"), gp = gp_main)

    ## draw fitted curve(s)
    if (is.null(cond_vars)) {
        ## single curve
        draw(1:nrow(dat),
             col_bands,
             col_lines,
             pch[1])
    } else {
        ## multiple curves
        for (i in seq_along(lev)) {
            ind <- dat[,cond_vars] == lev[i]
            draw(ind, col_bands[i], col_lines[i], pch[i], lev[i])
        }

        if (legend)
            grid_legend(legend_pos,
                        labels = lev,
                        col = col_lines,
                        lty = "solid",
                        lwd = lwd,
                        vgap = legend_vgap,
                        gp_frame = gp_legend_frame,
                        inset = legend_inset,
                        title = cond_vars,
                        gp_title = gp_legend_title)
    }

    if (pop) popViewport() else upViewport()

    invisible()
}
