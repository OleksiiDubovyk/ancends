\name{percentage}
\alias{percentage}

\title{
Percentage
}
\description{
Returns the value of percentage of each element in an array
}
\usage{
percentage(data, per.cent = FALSE)
}

\arguments{
  \item{data}{
Array
}
\item{per.cent}{
Logical. Should return values in percents 0...100?
}
}
\value{
The matrix or vector similar to that was array with elements replaced by percents.
}
\examples{
percentage(winter_birds)
#for all array
percentage(winter_birds[1])
#for 'BP' community only
}

