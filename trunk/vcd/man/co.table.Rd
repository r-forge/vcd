\name{co.table}
\alias{co.table}
\title{Compute conditional tables}
\description{
  For a contingency table in array form, compute a list of conditional
  tables given some margins.
}
\usage{
co.table(x, margin, collapse = ".")
}
\arguments{
  \item{x}{array}
  \item{margin}{margin index(es) or name(s)}
  \item{collapse}{character used when collapsing level names
    (only if more than 1 margin is given)}
}
\details{
  This is essentially an interface to \code{\link{"["}} which is more 
  convenient for arrays of arbitrary dimension.   
}
\value{
  A list of the resulting conditional tables.
}
\examples{
data(HairEyeColor)
co.table(HairEyeColor, 1)
co.table(HairEyeColor, c("Hair", "Eye"))
co.table(HairEyeColor, 1:2, collapse = "")
}
\keyword{array}
