\name{canber}
\alias{canber}
\title{
Canberra Distance
}
\description{
Canberra metric (also known as Canberra distance) as a community structural similarity index.
}
\usage{
canber(data, zeros, log.transform = FALSE)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
 \item{zeros}{
 Number; which value replaces zero in \code{data} to? Defaults to \code{0.01}.
 }
  \item{log.transform}{
 Logical. Should the data be logarithmically transformed? Defaults to \code{FALSE}.
 }
}
\details{
Canberra metric is quite similar to \code{\link{braycurtis}} Bray and Curtis dissimilarity index, but have the correction on the species number. While Bray-Curtis is the sum of the absolute of difference between densities of every species in two communities divided by the sum of sums of densities of every species in two communities  index, Canberra metric is defined as

\emph{1 - CM = 1 - (1/(a+b-c))*sum((abs(n[1i]-n[2i]))/(n[1i]+n[2i]))}

where \emph{n[ji]} - number of individuals of \emph{i} taxa in \emph{j} community,
      \emph{a} and \emph{b} are numbers of taxa in two communities,
      \emph{c} - number of taxa shared between communities.
}
\value{
Matrix in which columns and rows fit communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50: 296-302

Lance GN, Williams WT (1967) Mixed-data classificatory programs. I. Agglomerative systems. Aust Comput J 1: 15-20
}
\seealso{
\code{\link{braycurtis}}, \code{\link{sqeudist}}, \code{\link{renkonen}}, \code{\link{morisita}}, \code{\link{log_sorensen}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
canber(winter_birds)
#compare each-with-each

canber(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)

canber(winter_birds[,c(1,3)], zeros = 0.2, log.transform = TRUE)
##compare community 1 (BP) with community 3 (HC), if n[ji]=0, replace it
#with 0.2 and analyze logarithms of given values
}
