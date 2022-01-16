\name{mountford}
\alias{mountford}

\title{
Mountford Similarity Index
}
\description{
Mountford presence-absence communities similarity index.
}
\usage{
mountford(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
}
\details{
This index is defined as \emph{K = 2*c/(2*a*b-(a+b)*c)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50, 296-302

Mountford MD (1962) An index of similarity and its application to classificatory problems. In: PW Murphy (ed), Progress in Soil Zoology. Butterworths, London, pp. 43-50
}
\note{
The problem of Mountford index method is that in case of zero dissimilarity between communities (they are identical) Mountford index equals infinity. Also, it is not sensitive enough in a lower range.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{bblanq}}
}
\examples{
mountford(winter_birds)
#compare each-with-each

mountford(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
