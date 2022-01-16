\name{renkonen}
\alias{renkonen}
\title{
Renkonen Similarity Index
}
\description{
Renkonen communities structural similarity index.
}
\usage{
renkonen(data, per.cent = FALSE)
}
\arguments{
  \item{data}{
Communities data, a matrix-like object. In \code{data} rows must be species, columns - analyzed communities.
}
  \item{per.cent}{
Logical. Should return Renkonen index values in percents? Defaults to \code{FALSE}.
  }
}
\details{
The Renkonen similarity index, is a measure of dissimilarity between two communities (sites), based on relative (proportional) abundances of individuals of composite species. The codomain of this distance function ranges from 1 (identical proportional abundances) to 0 (no taxa shared).

This index is defined as

\emph{K = sum(min(p[1i], p[2i]))}

where \emph{p[ji]} - percentage of individuals of \emph{i} taxa in \emph{j} community.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50, 296-302.

Renkonen O (1938) Statisch-ökologische Untersuchungen über die terrestrische Käferwelt der finnischen Bruchmoore. Ann. Zool. Soc. Bot. Fenn. Vanamo 6:1-231.
}
\seealso{
\code{\link{braycurtis}}, \code{\link{canber}}, \code{\link{sqeudist}}, \code{\link{morisita}}, \code{\link{log_sorensen}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
renkonen(winter_birds)
#compare each-with-each

renkonen(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
