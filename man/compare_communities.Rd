\name{compare.communities}
\alias{compare_communities}
\title{
Permutational Hypothesis Testing in Comparison of Communities
}
\description{
For a chosen metric, this function calculates the metric value for two vectors corresponding to species abundances in those communities, then the community data from two vectors is being merged as if it was one community (i.e., under null hypothesis). Repetitive sampling from the merged pseudo-community allows one to build the distribution of smilarity metric under the null hypothesis and, consequently, to test the hypothesis that two analyzed communities were drawn from one community (in statistical terms, from a population, but in this case such term will be ambiguous).
}
\usage{
compare.communities(com1, com2, nperm = 1000, distance = ancends::jaccard, show.hist = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{com1, com2}{
Numeric; named vectors of interegs that correspond to species abundances.
}
  \item{nperm}{
  Integer; number of permutations to be performed. Defaults on \code{1000}.
  }
  \item{distance}{
  Function; metric to be used. Defaults on \code{ancends::jaccard}.
  }
  \item{show.hist}{
  Logical; whether to show the histogram of metric's distribution. Defaults on \code{FALSE}.
  }
}
\details{
See the [\code{comparison_test}](library/ancends/doc/comparison_test.html) vignette in this package.
}
\value{
Named vector, two elements: metric value and p-level.
}

\note{
Note that if the p-value is smaller than 1/nperm, i.e., the observed value was bigger than any of valued in simulated datasets, then the output p-value will be a character, e.g., \code{"< 0.001"}.
}

\seealso{
[\code{comparison_test}](library/ancends/doc/comparison_test.html)
}
\examples{
lbt <- data.frame(a = c(1, 2, 3, 0, 0, 0), b = c(0, 0, 0, 1, 1, 1))
compare.communities(lbt[1], lbt[2], nperm = 1000, distance = ancends::jaccard,
show.hist = FALSE)
}
