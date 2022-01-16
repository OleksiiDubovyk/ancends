\name{jaccard}
\alias{jaccard}
\title{
Jaccard Similarity Index
}
\description{
Jaccard presence-absence communities similarity index.
}
\usage{
jaccard(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
The Jaccard index, also known as Intersection over Union and the Jaccard similarity coefficient (originally called coefficient de communauté by Paul Jaccard), is a statistic used for comparing the similarity and diversity of sample sets. The Jaccard coefficient measures similarity between finite sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets.

This index is defined as \emph{K = c/(a+b-c)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Jaccard P (1901) Étude comparative de la distribution florale dans une portion des Alpes et des Jura. Bulletin de la Société Vaudoise des Sciences Naturelles, 37: 547–579.
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
jaccard(winter_birds)
#compare each-with-each

jaccard(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
