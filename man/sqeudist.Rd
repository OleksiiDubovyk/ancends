\name{sqeudist}
\alias{sqeudist}

\title{
Squared Euclidian Distance
}
\description{
Squared Euclidian distance as communities structural similarity index.
}
\usage{
sqeudist(data, log.transform = FALSE)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
 \item{log.transform}{
Logical. Should data be logarithmically transformed? Defaults to \code{FALSE}.
 }
}
\details{
Squared Euclidian distance is defined as

\emph{1 - D^2 = 1 - sum((p[1i]-p[2i])^2)}

where \emph{p[ji]} - percentage of individuals of \emph{i} taxa in \emph{j} community.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50: 296-302

Clifford HT, Steptlenson W (1975) An introduction to numerical classification. Academic Press, New York-San Francisco-London. P. 65
}

\note{
The Squared Euclidian Distance invariably has a very high value. Even when the two samples are completely different this index is going to 1. For this reason this index is not very useful when comparing two different faunal samples (from Wolda, 1981).
}
\seealso{
\code{\link{braycurtis}}, \code{\link{canber}}, \code{\link{renkonen}}, \code{\link{morisita}}, \code{\link{log_sorensen}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
sqeudist(winter_birds)
#compare each-with-each

sqeudist(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
