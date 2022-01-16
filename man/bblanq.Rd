\name{bblanq}
\alias{bblanq}
\title{
Braun-Blanquet Similarity Index
}
\description{
Braun-Blanquet presence-absence communities similarity index.
}
\usage{
bblanq(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This similarity index is defined as \emph{K = c/max(a, b)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Braun-Blanquet J (1928) Pflanzensoziologie Grundzüge der Vegetationskunde. Berlin: Verlaq von Julius springer, 1928. 330 s.
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{mountford}}
}
\examples{
bblanq(winter_birds)
#compare each-with-each

bblanq(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
