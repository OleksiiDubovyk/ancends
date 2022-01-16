\name{brillouin}
\alias{brillouin}
\title{
Brillouin Diversity Index
}
\description{
Brillouin index equals logarithm of factorial of total individuals number with subtracted sum of logarithms of factorials of separate species divided by total number of individuals.
}
\usage{
brillouin(data, log.base)
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
Brillouin index is defined as

\emph{HB = (ln(n[i]!) - sum ln(n[i]!))/N}

where \emph{n[i]} is number of individuals of \emph{i} taxa and \emph{N} is general number of all individuals in the sample.
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
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
brillouin(winter_birds)

brillouin(winter_birds, log.base = 10)
}
