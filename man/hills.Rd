\name{hills}
\alias{Hill's}
\title{
Hill's number
}
\description{
An effective number of species
}
\usage{
hills(data, q)
}
\arguments{
  \item{data}{
vector of species abundances
  }
  \item{q}{
Hill's number power
  }
}
\details{
Hill's number estimates the effective number of species - the equivalent number of equally abundant species needed to yield the observed diversity index (e.g., species richness for q = 0, Shannon diversity when q = 1, Simpson diversity when q = 2, etc.) Higher values of q give heavier weight to the more common species.
}
\value{
Numeric, equivalent number of species.
}
\references{
Hill, M.O. (1973), Diversity and Evenness: A Unifying Notation and Its Consequences. Ecology, 54: 427-432. https://doi.org/10.2307/1934352

Gotelli, N.J., Ellison, A.M. (2018), A Primer of Ecological Statistics, Second Edition. Oxford University Press.
}
