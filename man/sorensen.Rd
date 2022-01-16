\name{sorensen}
\alias{sorensen}
\title{
Sorensen Similarity Index
}
\description{
Sørensen presence-absence communities similarity index.
}
\usage{
sorensen(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This index is defined as \emph{K = 2*c/(a+b)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50, 296-302.

Sorensen TA (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species content, and its application to analyses of the vegetation on Danish commons. K dan Vidensk Selsk Biol Skr 5:1-34
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
sorensen(winter_birds)
#compare each-with-each

sorensen(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
