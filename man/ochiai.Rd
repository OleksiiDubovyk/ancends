\name{ochiai}
\alias{ochiai}
\title{
Ochiai Similarity Index
}
\description{
Ochiai presence-absence communities similarity index.
}
\usage{
ochiai(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This index is defined as \emph{K = c/sqrt(a*b)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Ochiai A (1957) Zoogeographic studies on the soleoid fishes found in Japan and its neighbouring regions. Bull Jpn Soc Sci Fish 22: 526–530
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{diceai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
ochiai(winter_birds)
#compare each-with-each

ochiai(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
