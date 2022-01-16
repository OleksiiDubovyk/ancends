\name{equit}
\alias{equit}

\title{
Equitability
}
\description{
Shannon diversity index divided by the logarithm of number of taxa. This measures the evenness with which individuals are divided among the taxa present.
}
\usage{
equit(data, log.base)
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
\code{log.base} is being used for Shannon index calculation, see \code{\link{shannon}}.

Equitability is defined as

\emph{Eq = H/ln(S)}

where \emph{H} - Shannon diversity index, see \code{\link{shannon}}, \emph{S} is the number of taxa in the sample.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons.
}
\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
equit(winter_birds)

equit(winter_birds, log.base = 10)
}
