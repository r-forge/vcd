\name{mosaicpairs}
\alias{mosaicpairs}
\title{Mosaic Matrices}
\description{Plots matrices of mosaics.}
\usage{
mosaicpairs(x, main = NULL, xlab = NULL, ylab = NULL, labels, \dots,
            type = c("simple", "independent", "conditional", "joint"),
            shade = FALSE, oma = NULL, cex.labels = NULL, label.pos = 0.5,
	    font.labels = 1, gap = 1)
}
\arguments{
  \item{x}{a contingency table in array form, with optional category
    labels specified in the \code{dimnames(x)} attribute.  The table is
    best created by the \code{table()} command.}
  \item{main}{character string for the mosaic title.}
  \item{xlab,ylab}{x- and y-axis labels used for the plot; by default,
    the first and second element of \code{names(dimnames(X))} (i.e., the
    name of the first and second variable in \code{X}).}
  \item{labels}{the names of the variables.}
  \item{\dots}{further parameters (\emph{currently not used}).}
  \item{type}{a character string indicating the type of mosaic matrix,
    see details for more information.}
  \item{shade}{a logical indicating whether to produce extended mosaic
    plots, or a numeric vector of at most 5 distinct positive numbers
    giving the absolute values of the cut points for the residuals.  By
    default, \code{shade} is \code{FALSE}, and simple mosaics are
    created.  Using \code{shade = TRUE} cuts absolute values at 2 and
    4.}
  \item{oma}{A vector of the form c(bottom, left, top, right) giving the
    size of the outer margins in lines of text.}
  \item{cex.labels, font.labels}{graphics parameters for the text panel.}
  \item{label.pos}{y position of labels in the text panel.}
  \item{gap}{distance between subplots, in margin lines.}
}
\details{
  \code{mosaicpairs()} plots a matrix of pairwise mosaic plots, three
  types are distinguished: \code{"simple"}, \code{"independent"}, \code{"conditional"}.
  The simple mosaic shows bivariate marginal relations, collapsed over
  all other variables. The independent mosaic matrix shows mosaic plots
  for mutual independence, i.e. for marginal and conditional independence
  among all pairs of variables. The conditional mosaic matrix shows
  mosaic plots for marginal independence given all other variables. The
  joint option shows mosaic plots for joint independence of all pairs of
  variables from the others.
}

\references{
M. Friendly (2000),
\emph{Visualizing Categorical Data}. SAS Institute, Cary, NC.
}
\seealso{
  \code{\link{mosaicplot}},
  \code{\link{pairs}}.
}
\examples{
data(UCBAdmissions)
data(PreSex)

mosaicpairs(PreSex, shade = TRUE)
mosaicpairs(UCBAdmissions, shade = TRUE)
mosaicpairs(UCBAdmissions, shade = TRUE, type="conditional")
}
\keyword{hplot}



