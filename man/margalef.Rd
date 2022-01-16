\name{margalef}
\alias{margalef}
\title{
Margalef Richness Index
}
\description{
Number of taxa minus one divided by logarithm of general individuals number.
}
\usage{
margalef(data, log.base)
}
\arguments{
  \item{data}{
Community data, a matrix-like object or a vector.
}
  \item{log.base}{
Number; specifies the base of logarithm in the formula.
}
}
\details{
Margalef index is defined as

\emph{Ma = (S-1)/ln(N)}

where \emph{N} is overall number of all individuals in the sample, \emph{S} is number of taxa.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Margalef R. Perspective in Ecological Theory, Uni. of Chicago Press, 1968, 112.
}
\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
margalef(winter_birds)

margalef(winter_birds, log.base = 10)
}
