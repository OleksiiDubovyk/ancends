\name{kulcz}
\alias{kulcz}
\title{
Kulczinsky Similarity Index
}
\description{
Kulczinsky presence-absence communities similarity index.
}
\usage{
kulcz(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This index is defined as \emph{K = (2/c)*(1/a+1/b)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Kulczynski S (1927) Die Pflanzenassoziationen der Pieninen, Bulletin Internationalde l'Academie A Polonaise des Sciences et des Lettres, Classe des Sciences, 1927.

Zakani FR, Arhid K, Bouksim M,Gadi T, Aboulfatah M, (2016) Kulczynski similarity index for objective evaluation of mesh segmentation algorithms. 2016 5th International Conference on Multimedia Computing and Systems (ICMCS), Marrakech: 12-17.
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
kulcz(winter_birds)
#compare each-with-each

kulcz(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
