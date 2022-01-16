\name{baroni}
\alias{baroni}
\title{
Baroni-Urbani and Buser Similarity Index
}
\description{
Baroni-Urbani and Buser binary similarity index.
}
\usage{
baroni(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This index is defined as \emph{K = (sqrt(c*d)+c)/(sqrt(c*d)+a+b-c)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities, \emph{d} - the number of taxa absent in both communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50, 296-302

Baroni-Urbani C, Buser MW (1976) Similarity of binary data. Syst Zool 25:251-259
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
baroni(winter_birds)
#compare each-with-each

baroni(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
