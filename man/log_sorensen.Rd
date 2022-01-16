\name{log_sorensen}
\alias{log_sorensen}
\title{
Logarithmic Modification of the Sorensen Similarity Index
}
\description{
Paired modificated Sorensen index showing similarity between community structure.
}
\usage{
log_sorensen(data, as.per.cent = FALSE, result.as.per.cent = FALSE)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
  \item{as.per.cent}{
Logical. Should data be transformed into percent? Defaults to \code{FALSE}
}
  \item{result.as.per.cent}{
Logical. Should return index values in percents? Defaults to \code{FALSE}.
}
}
\details{
The modification is substitution of the number of species in common between both communities in the Sorensen formula with the sum of ratio of logarithm of minimal and maximal species densities among two communities. Thus, this index will show not presence-absence similarity, but structural similarity between two communities.

This index is defined as

\emph{K = 2*sum(ln(min n[ji])/ln(max n[ji]))/a+b}

where \emph{n[ji]} - number of individuals of \emph{i} taxa in \emph{j} community, \emph{a} and \emph{b} are numbers of taxa in two communities.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Sorensen TA (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species content, and its application to analyses of the vegetation on Danish commons. K dan Vidensk Selsk Biol Skr 5:1-34

Dubovyk O, Kuzyo H (2017) Comparison of bird assemblage structure of cemeteries and parks in Lviv.  Materials of the XIII scientific conference "Current state and biodiversity of ecosystems in Shatsky National Natural Park and other conservation areas". Lviv, 2017. P. 34-38 [In Ukrainian]
}
\author{
Oleksii Dubovyk
}
\seealso{
\code{\link{braycurtis}}, \code{\link{canber}}, \code{\link{sqeudist}}, \code{\link{renkonen}}, \code{\link{morisita}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
log_sorensen(winter_birds)
#compare each-with-each

log_sorensen(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
