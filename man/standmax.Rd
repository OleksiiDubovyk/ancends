\name{standmax}
\alias{standmax}
\title{
Scale to the max value
}
\description{
Scales sample to range (0...1) where 0 is the min value and 1 is the max value.
}
\usage{
standmax(x)
}
\arguments{
  \item{x}{
Sample data frame column or vector
}
}
\value{
Vector of values identical to \code{x}
}
\seealso{
\code{\link{standmin}}
}
\examples{
standmax(c(1, 2, 3, 4, 5))
}
