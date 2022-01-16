\name{simpson}
\alias{simpson}
\title{
Simpson Diversity Index
}
\description{
Simpson's Index measures the probability that two individuals randomly selected from a sample will belong to the same species (or some category other than species). With this index, 0 represents infinite diversity and 1, no diversity. That is, the bigger the value of Simpson's index value is, the lower the diversity is.

Simpson Diversity Index equals to 1-Simpson's Index of Dominance. The value of this index also ranges between 0 and 1, but now, the greater the value, the greater the sample diversity. In this case, the index represents the probability that two individuals randomly selected from a sample will belong to different species.
}
\usage{
simpson(data, mode = "Biased")
}
\arguments{
  \item{data}{
Community data, a matrix-like object or a vector.
}
  \item{mode}{
Method of calculating \code{"Biased"} or \code{"Unbiased"}. Defaults to \code{"Biased"}.
}
}
\details{
In mode \code{"Unbiased"} index is corrected by multiplying data by value minus one.

Biased Simpson Index is defined as

\emph{1 - D = 1 - sum (n[i]/N)^2}

where \emph{n[i]} is number of individuals of \emph{i} taxa and \emph{N} is general number of all individuals in the sample.

Unbiased Simpson Index is defined as

\emph{1 - D = 1 - (sum n[i]*(n[i]-1))/N*(N-1)}
}
\value{
Single value for vector data and vector for data frame.
}
\references{
Simpson, EH (1949) Measurement of diversity. Nature. 163: 688
}

\seealso{
\code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}, \code{\link{diversity}},

\code{\link[vegan]{diversity}} (vegan package)
}
\examples{
simpson(winter_birds[1])

simpson(winter_birds[1], mode = "Unbiased")

simpson(winter_birds)
}
