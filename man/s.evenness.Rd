\name{s.evenness}
\alias{s.evenness}
\title{
Simple Buzas and Gibson's Evenness
}
\description{
Constant e raised to the power of Shannon index value and divided by number of taxa.
}
\usage{
s.evenness(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data frame or vector representing community
}
}
\details{
The same as \code{\link{evenness}}; \code{mode = 'Biased'} and \code{log.base} is \code{NULL}, thus logarithm in the Shannon Index formula is natural
}
\value{
Number for vector and vector for data frame
}
\references{
Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons
}

\seealso{
\code{\link{shannon}}, \code{\link{evenness}},  \code{\link{diversity}}
}
\examples{
s.evenness(winter_birds[1])

s.evenness(winter_birds)
}
