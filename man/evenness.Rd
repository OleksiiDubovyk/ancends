\name{evenness}
\alias{evenness}
\title{
Buzas and Gibson's Evenness
}
\description{
Constant e raised to the power of Shannon index value and divided by number of taxa.
}
\usage{
evenness(data, mode = "Biased", log.base)
}
\arguments{
  \item{data}{
Community data, a matrix-like object or a vector.
}
  \item{mode}{
Method of calculating \code{"Biased"} or \code{"Unbiased"}. Defaults to \code{"Biased"}.
}
  \item{log.base}{
  Number; specifies the base of logarithm in the formula.
  }
}
\details{
\code{mode} and \code{log.base} are being used for Shannon index calculation, see \code{\link{shannon}}.

Buzas and Gibson's Evenness is defined as

\emph{E = (e^H)/S}

where \emph{H} - Shannon diversity index, see \code{\link{shannon}}, \emph{S} is the number of taxa in the sample.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons

Ludwig J.A. and Reynolds J.F. 1988. Statistical Ecology – a Primer on Methods and Computing.
John Wiley and Sons, Toronto.
}

\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
evenness(winter_birds[1])

evenness(winter_birds[1], mode = "Unbiased")

evenness(winter_birds[1], mode = "Unbiased", log.base = 10)

evenness(winter_birds)
}
