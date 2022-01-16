\name{diceai}
\alias{diceai}
\title{
Dice Association Index
}
\description{
Dice (also known as Szymkiewicz-Simpson) presence-absence communities similarity index.
}
\usage{
diceai(data)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object.
}
}
\details{
This index is defined as \emph{K = c/min(a, b)} where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia 50: 296-302

Dice LR (1945) Measures of the amount of ecological association between species. Ecology 26 : 297-302

Szymkiewicz D (1934) Une contribution statistique a la géographie floristique. Acta Soc. Bot. Polon. 34(3): 249-265
}
\note{
In \code{data} rows must be species, columns - analyzed communities.
}
\seealso{
\code{\link{ccompare}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{bblanq}}, \code{\link{mountford}}
}
\examples{
diceai(winter_birds)
#compare each-with-each

diceai(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
