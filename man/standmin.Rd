\name{standmin}
\alias{standmin}
\title{
Scale to the Min Value
}
\description{
Scales sample to range (0...Inf) where 0 is the min value. It is useful to rescale sample with both positive and negative values.  If \code{to.max = TRUE} scales sample to range (0...1) where 0 is the min value and 1 is the max value.
}
\usage{
standmin(x, to.max = FALSE)
}
\arguments{
  \item{x}{
Sample data frame column or vector
}
 \item{to.max}{
 Logical. Scale to max value too? Defaults to \code{FALSE}
 }
}
\value{
Vector of values identical to /code{x}
}

\seealso{
\code{\link{standmax}}
}
\examples{
standmin(c(-1, -3, 2, 1, 3, 4))
}
