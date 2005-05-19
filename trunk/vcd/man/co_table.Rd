\name{co_table}
\alias{co_table}
\title{Compute Conditional Tables}
\description{
  For a contingency table in array form, compute a list of conditional
  tables given some margins.
}
\usage{
co_table(x, margin, collapse = ".")
}
\arguments{
  \item{x}{array}
  \item{margin}{margin index(es) or name(s)}
  \item{collapse}{character used when collapsing level names
    (only if more than 1 margin is given)}
}
\details{
  This is essentially an interface to \code{\link[base]{"Extract"}}
  which is more convenient for arrays of arbitrary dimension.   
}
\value{
  A list of the resulting conditional tables.
}
\examples{
data(HairEyeColor)
co_table(HairEyeColor, 1)
co_table(HairEyeColor, c("Hair", "Eye"))
co_table(HairEyeColor, 1:2, collapse = "")
}
\keyword{array}
