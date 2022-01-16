\name{morisita}
\alias{morisita}
\title{
Morisita Similarity Index
}
\description{
Morisita communities structural similarity index.
}
\usage{
morisita(data, log.transform = FALSE)
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
The Morisita Index of Dispersion (or Morisita overlap index) is a measure of how similar or different two sets of data are. The index ranges from 0 (no similarity) to 1 (complete similarity). It can, however, return values larger than 1, which may lead to misleading interpretations. This is especially true for small samples.

This index is defined as

\emph{K = 2*sum(n[1i]*n[2i])/((lambda[1] + lambda[2])*N[1]*N[2])}

where \emph{lambda[i] = (sum(n[ji]*(n[ji]-1)))/N[j]*(N[j]-1) = Unbiased Simpson Dominance},
      \emph{n[ji]} - number of individuals of \emph{i} taxa in \emph{j} community,
      \emph{N[j]} - overall number of all individuals in \emph{j} community.
}
\value{
Similarity matrix in which columns and rows correspond to communities.
}
\references{
Wolda H (1981) Similarity indices, sample size and diversity. Oecologia, 50: 296-302

Morisita M (1959) Measuring of interspecific association and similarity between communities. Mem Fac Sci Kyushu Univ, Ser E, Bio., 3:65-80

Bakus, G. (2007). Quantitative Analysis of Marine Biological Communities: Field Biology and Environment. John Wiley & Sons.
}
\note{
The index has been criticized for being complicated to use and interpret.
}
\seealso{
\code{\link{braycurtis}}, \code{\link{canber}}, \code{\link{sqeudist}}, \code{\link{renkonen}}, \code{\link{log_sorensen}},
\code{\link[vegan]{vegdist}} (vegan package)
}
\examples{
morisita(winter_birds)
#compare each-with-each

morisita(winter_birds[,c(1,3)])
#compare community 1 (BP) with community 3 (HC)
}
