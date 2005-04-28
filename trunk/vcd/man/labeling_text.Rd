\name{labeling_text}
\alias{labeling_text}
\alias{labeling_conditional}
\alias{labeling_left}
\alias{labeling_cboxed}
\alias{labeling_lboxed}
\alias{labeling_doubledecker}
\alias{labelings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Labeling functions for strucplot labeling}
\description{
These functions generate labeling functions used for strucplots.
}
\usage{
labeling_text(labels = TRUE, varnames = labels, tl_labels = NULL,
              tl_varnames = NULL, gp_labels = gpar(fontsize = 12),
              gp_varnames = gpar(fontsize = 12, fontface = 2),
              rot_labels = c(0, 90, 0, 90), rot_varnames = c(0, 90, 0, 90),
              pos_labels = "center", pos_varnames = "center",
              just_labels = "center", just_varnames = pos_varnames,
              boxes = FALSE, fill_boxes = NULL, offset = c(0, 0, 0, 0),
              labbl_varnames = NULL, labels_varnames = FALSE, sep = ": ",
              abbreviate = FALSE, rep = TRUE, clip = FALSE, ...)
labeling_conditional(...)
labeling_left(tl_labels = TRUE, clip = TRUE, pos_varnames = "left",
              pos_labels = "left", just_labels = "left", ...)
labeling_cboxed(tl_labels = TRUE, boxes = TRUE, clip = TRUE, pos_labels = "center", ...)
labeling_lboxed(tl_labels = FALSE, boxes = TRUE, clip = TRUE, pos_labels = "left",
                just_labels = "left", labbl_varnames = FALSE, ...)
labeling_doubledecker(labels = "bottom", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{  }
  \item{varnames}{  }
  \item{tl_labels}{  }
  \item{tl_varnames}{  }
  \item{gp_labels}{  }
  \item{gp_varnames}{  }
  \item{rot_labels}{  }
  \item{rot_varnames}{  }
  \item{pos_labels}{  }
  \item{pos_varnames}{  }
  \item{just_labels}{  }
  \item{just_varnames}{  }
  \item{boxes}{  }
  \item{fill_boxes}{  }
  \item{offset}{  }
  \item{labbl_varnames}{  }
  \item{labels_varnames}{  }
  \item{sep}{  }
  \item{abbreviate}{  }
  \item{rep}{  }
  \item{clip}{  }
  \item{\dots}{  }
}
\details{
  These functions generate labeling functions called by
  \code{\link{strucplot}} for their side-effect of adding labels to the
  plot. They suppose that a strucplot has been drawn and the
  corresponding viewport structure is pushed, since the positions of the
  viewports are used for the label positioning.

  This help page only documents \code{labeling_text} and
  derived functions, more functions are described on the help page
  for \code{\link{labeling_cells}} and \code{\link{labeling_list}}.
  
  The functions can also be used sQuote{stand-alone} as shown in the examples.
}
\value{
  A function with arguments:
  \item{d}{\code{"dimnames"} attribute from the visualized contingency
    table, or the visualized table itself from which the
    \code{"dimnames"} attributes will then be extracted.}
  \item{split_vertical}{logical vector indicating the split directions.}
  \item{condvars}{integer vector of conditioning dimensions}
}
\author{David Meyer (\email{David.Meyer@R-project.org})}

\seealso{
  \code{\link{labeling_cells}}, 
  \code{\link{labeling_list}}, 
  \code{\link{structable}}, 
  \code{\link[grid]{grid.text}}
}

\examples{
mosaic(Titanic, labeling = labeling_cells)
mosaic(Titanic, labeling = labeling_list)

## A more comlex example, adding the observed frequencies to a mosaicplot:
tab <- ifelse(Titanic < 6, NA, Titanic)
mosaic(Titanic)
labeling_cells(text = tab, margin = 0)(Titanic)
}
\keyword{hplot}% at least one, from doc/KEYWORDS

