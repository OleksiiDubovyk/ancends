\name{shannon}
\alias{shannon}
\title{
Shannon Diversity Index
}
\description{
The Shannon diversity index is commonly used to characterize species diversity in a community. Shannon index accounts for both abundance and evenness of the species present. The proportion of species relative to the total number of species is calculated, and then multiplied by the natural logarithm of this proportion. The resulting product is summed across species, and multiplied by -1.

The more unequal the abundances of the types, the larger the weighted geometric mean of the species proportions values, and the smaller the corresponding Shannon entropy. If practically all abundance is concentrated to one type, and the other types are very rare (even if there are many of them), Shannon entropy approaches zero. When there is only one type in the data set, Shannon entropy exactly equals zero (there is no uncertainty in predicting the type of the next randomly chosen entity).

Shannon index commonly varies between 1.5 and 3.5, and rarely achieves 4.
}
\usage{
shannon(data, mode = "Biased", log.base, std = FALSE)
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
  \item{std}{
  Logical; should the index value be standardized by species richness? If \code{TRUE}, the value corresponds to a ratio of the observed Shannon index to the maximum possible value for observed species richness (i.e., when all taxa are evely distributed). Defaults to \code{FALSE}.
  }
}
\details{
In mode \code{'Unbiased'} index is corrected by subtraction of taxa number minus one divided by doubled general individuals number.

Biased Shannon Index is defined as

\emph{H = - sum((n[i]/N)*log(n[i]/N))}

where \emph{n[i]} is number of individuals of \emph{i} taxa and \emph{N} is general number of all individuals in the sample.

Unbiased Shannon Index is defined as

\emph{H[u] = H - (S-1)/2*N}

where \emph{S} is the number of taxa in the sample.
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Shannon CE (1948) A mathematical theory of communication. The Bell System Technical Journal, 27: 379-423 and 623-656
}

\seealso{
\code{\link{simpson}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
shannon(winter_birds[1])

shannon(winter_birds[1], mode = "Unbiased")

shannon(winter_birds[1], mode = "Unbiased", log.base = 10)

shannon(winter_birds)
}
