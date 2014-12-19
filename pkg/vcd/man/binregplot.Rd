\name{binreg_plot}
\alias{binreg_plot}
\title{Binary Regression Plot}
\description{
  Creates a display of observed and fitted values for binary regression
  models with one numeric predictor, conditioned by zero or many co-factors.
}
\usage{
binreg_plot(model, main = NULL, xlab = NULL, ylab = NULL,
         xlim = NULL, ylim = NULL,
         pred_var = NULL, cond_vars = NULL, base_level = NULL, subset,
         type = c("response", "link"), conf_level = 0.95,
         pch = NULL, cex = 0.6, jitter_factor = 0.1, 
         lwd = 5, lty = 1, point_size = 0, col_lines = NULL, col_bands = NULL, 
         legend = TRUE, legend_pos = NULL, legend_inset = c(0, 0.1),
         legend_vgap = unit(0.5, "lines"),
         labels = FALSE, labels_pos = c("right", "left"),
         labels_just = c("left","center"), labels_offset = c(0.01, 0),
         gp_main = gpar(fontface = "bold", fontsize = 14),
         gp_legend_frame = gpar(lwd = 1, col = "black"),
         gp_legend_title = gpar(fontface = "bold"),
         newpage = TRUE, pop = TRUE, return_grob = FALSE)
}
\arguments{
  \item{model}{a binary regression model fitted with \code{\link[stats]{glm}}}
  \item{main}{user-specified main title.}
  \item{xlab}{x-axis label. Defaults to the name of the (first) numeric predictor.}
  \item{ylab}{y-axis label. Defaults to the name of the response -
    within either 'P(...)' or 'logit(...)', depending on the response
    type.}
  \item{xlim}{Range of the x-axis. Defaults to the range of the numeric
    predictor.}
  \item{ylim}{Range of the y-axis. Defaults to the unit interval on
    probability scale or the fitted values range on the link scale,
    depending on \code{type}}
  \item{pred_var}{character string of length 1 giving the name of
    the numeric predictor. Defaults to the first one found in the data set.}
  \item{cond_vars}{optional character string of conditioning
    variables. Defaults to all factors found in the data set, response
    excluded. If \code{FALSE}, no variables are used for conditioning.}
  \item{base_level}{vector of length one,
    specifying the base ('no effect') value of the response variable
    (e.g., "Placebo", 0, FALSE, etc.). Defaults to the first level for
    factor responses, or 0 for numeric/binary variables. This controls
    which observations will be plotted on the top or the bottom of the
    display.}
  \item{subset}{an optional vector specifying a subset of the data
    rows. The value is evaluated in the data environment, so expressions
    can be used to select the data (see examples).}
  \item{type}{either "response" or "link" to select the scale of the
    fitted values. The y-axis will be adapted accordingly.}
  \item{conf_level}{confidence level used for calculating 
    confidence bands.}
  \item{pch}{character or numeric vector of symbols used for plotting
    the (possibly conditioned) observed values, recycled as needed.}
  \item{cex}{size of the plot symbols (in lines).}
  \item{jitter_factor}{argument passed to \code{\link[base]{jitter}}
    used for the points representing the observed values.}
  \item{lwd}{Line width for the fitted values.}
  \item{lty}{Line type for the fitted values.}
  \item{point_size}{size of points for the fitted values in char units (default: 0, so
    no points are plotted).}
  \item{col_lines, col_bands}{character vector specifying the colors of the fitted
    lines and confidence bands,
    by default chosen with \code{\link[colorspace]{rainbow_hcl}}. The
    conidence bands are using alpha blending with alpha = 0.2.}
  \item{legend}{logical; if \code{TRUE} (default), a legend is drawn.}
  \item{legend_pos}{numeric vector of length 2, specifying x and y
    coordinates of the legend, or a character string (e.g., 'topleft',
    'center' etc.). Defaults to 'topleft' if the fitted curve's slope is
    positive, and 'topright' else.}
  \item{legend_inset}{numeric vector or length 2 specifying the inset
    from the legend's x and y coordinates in npc units.}
  \item{legend_vgap}{vertical space between the legends' line entries.}
  \item{labels}{logical; if \code{TRUE}, labels corresponding to the
    factor levels are plotted next to the fitted lines.}
  \item{labels_pos}{either "right" or "left", determining on which side
    of the fitted lines (start or end) the labels should be placed.}
  \item{labels_just}{character vector of length 2, specifying the
    relative justification of the labels to their coordinates. See the
    documentation of the \code{just} parameter of
    \code{\link[grid]{grid.text}} for more details.}
  \item{labels_offset}{numeric vector of length 2, specifying the offset    
    of the labels' coordinates in npc units.}
  \item{gp_main}{object of class \code{"gpar"} used for the main title.}
  \item{gp_legend_frame}{object of class \code{"gpar"} used for the main title.}
  \item{gp_legend_title}{object of class \code{"gpar"} used for the main title.}
  \item{newpage}{logical; if \code{TRUE}, the plot is drawn on a new page.}
  \item{pop}{logical; if \code{TRUE}, all newly generated viewports are
    popped after plotting.}
  \item{return_grob}{logical. Should a snapshot of the display be
    returned as a grid grob?}
}
\details{
  The primary purpose of this function is to visualize observed and
  fitted values for binary regression models (like the logistic or probit
  regression model) with one numeric predictor. If one or more
  categorical predictors are used in the model, the fitted values are
  conditioned on them, i.e. separate curves are drawn corresponding to
  the factor level combinations.
  
  The implementation relies on objects returned by
  \code{\link[stats]{glm}}, as it uses its \code{"terms"} and
  \code{"model"} components. 

  The function tries to determine suitable values for the legend and/or
  labels, but depending on the data, this might require some tweaking.
}
\value{
\code{NULL} (invisibly).
}
\references{
Michael Friendly (2000),
  \emph{Visualizing Categorical Data}.
  SAS Institute, Cary, NC.
}
\author{
  David Meyer \email{David.Meyer@R-project.org}
}

\examples{
## Simple model with no conditioning variables
art.mod0 <- glm(Improved > "None" ~ Age, data = Arthritis, family = binomial)

binreg_plot(art.mod0, "Arthritis Data")
binreg_plot(art.mod0, type = "link") ## logit scale

## one conditioning factor
art.mod1 <- update(art.mod0, . ~ . + Sex)
binreg_plot(art.mod1)
binreg_plot(art.mod1, legend = FALSE, labels = TRUE, xlim = c(20, 80))

## two conditioning factors
art.mod2 <- update(art.mod1, . ~ . + Treatment)
binreg_plot(art.mod2)
binreg_plot(art.mod2, subset = Sex == "Male") ## subsetting

## some tweaking
binreg_plot(art.mod2, gp_legend_frame = gpar(col = NA, fill = "white"), col_bands = NA)
binreg_plot(art.mod2, legend = FALSE, labels = TRUE,
            labels_pos = "left", labels_just = c("left", "top"))
}
\keyword{category}
\keyword{hplot}
