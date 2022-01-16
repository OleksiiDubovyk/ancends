\name{align.by.names}
\alias{align_by_names}
\title{
Align a List of Vectors into a Matrix by Their Names
}
\description{
Takes a list of named vectors and returns a matrix where rows correspond to unique names and columns correspond to input vectors.
}
\usage{
align.by.names(l, replacement = 0)
}
\arguments{
  \item{l}{
list of named vectors
}
  \item{replacement}{
  Specifies with which values to replace the absences of occurence of names in enlisted vectors. Defaults to 0.
  }
}
\value{
Matrix.
}

\note{
The output matrix can be transposed if neccessary.
}
\examples{
a <- c(1, 2, 3, 4)
b <- c(1, 2, 3, 4)
c <- c(1, 2, 3, 4)
names(a) <- c("a", "b", "c", "d")
names(b) <- c("b", "c", "d", "e")
names(c) <- c("d", "e", "f", "g")
align.by.names(list(a, b, c))
}
