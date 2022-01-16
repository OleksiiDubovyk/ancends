\name{menhinick}
\alias{menhinick}
\title{
Menhinick Richness Index
}
\description{
Menhinick's richness index - the ratio of the number of taxa to the square root of sample size.
}
\usage{
menhinick(data)
}
\arguments{
  \item{data}{
Community data, a matrix-like object or a vector.
}
}
\details{
Menhinick index is defined as

\emph{Me = S/sqrt(N)}

where \emph{N} is the overall number of all individuals in the sample, \emph{S} is number of taxa.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons

Ludwig J.A. and Reynolds J.F. 1988. Statistical Ecology – a Primer on Methods and Computing.
John Wiley and Sons, Toronto.

Williams WL, Witkowski ETF, Balkwill K (2005) Application of diversity indices to appraise plant availability in the traditional medicinal markets of Johannesburg, South Africa. Biodiversity and Conservation 14: 2971-3001
}

\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
menhinick(winter_birds)

menhinick(winter_birds[1])
}
