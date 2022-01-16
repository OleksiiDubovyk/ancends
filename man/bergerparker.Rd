\name{bergerparker}
\alias{bergerparker}
\title{
Berger-Parker Dominance Index
}
\description{
The number of individuals in the dominant taxa relative to overall individuals number.
}
\usage{
bergerparker(data)
}
\arguments{
  \item{data}{
Community data, a matrix-like object or a vector.
}
}
\details{
Berger-Parker index is defined as

\emph{BP = max(n[i])/N}

where \emph{n[i]} is number of individuals of \emph{i} taxa and \emph{N} is general number of all individuals in the sample.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons.
}
\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
bergerparker(winter_birds)
}
