\name{braycurtis}
\alias{braycurtis}
\title{
Bray and Curtis Similarity Index
}
\description{
Bray and Curtis communities structural similarity index.
}
\usage{
braycurtis(data, as.per.cent = FALSE, log.transform = FALSE, log.base)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
 \item{as.per.cent}{
Logical. Should data be transformed into percent? Defaults to \code{FALSE}.
 }
 \item{log.transform}{
Logical. Should data be logarithmically transformed? Defaults to \code{FALSE}.
 }
  \item{log.base}{
Numeric. If \code{log.transform = FALSE}, specifies the base of logarithm in the formula if you do not want it to be natural.
 }
}
\details{
The Bray Curtis dissimilarity is used to quantify the differences in species populations between two different sites. It is being used primarily in ecology and biology. It is defined as

\emph{BC = (sum abs(n[1i] - n[2i]))/sum (n[1i]+n[2i])}

where \emph{n[ji]} - number of individuals of \emph{i} taxa in \emph{j} community.

The Bray-Curtis similarity equals \emph{K = 1 - BC = 1 - (sum abs(n[1i] - n[2i]))/sum (n[1i]+n[2i])}.

The Bray-Curtis similarity is always a number between 0 and 1. If 1, the two sites share all the same species; if 0, they don’t share any species.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50: 296-302

Bray JR, Curtis JT (1957) An ordination of the upland forest communities in southern Wisconsin. Ecol Monogr 27: 325-349
}
\seealso{
\code{\link{canber}}, \code{\link{sqeudist}}, \code{\link{renkonen}}, \code{\link{morisita}}, \code{\link{log_sorensen}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
braycurtis(winter_birds)
#compare each-with-each

braycurtis(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)

braycurtis(winter_birds[,c(1,3)], log.transform = TRUE)
##compare community 1 (BP) with community 3 (HC) after replacing values
#with logarithms of given values

braycurtis(winter_birds[,c(1,3)], log.transform = TRUE, log.base = 10)
##compare community 1 (BP) with community 3 (HC) after replacing values
#with decimal logarithms of given values

braycurtis(winter_birds[,c(1,3)], as.per.cent = TRUE)
##compare community 1 (BP) with community 3 (HC) after replacing values
#with percenage of given values
}
