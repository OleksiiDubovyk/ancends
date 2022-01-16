\name{ancends}
\alias{ancends}

\title{Simple Analysis of Community Structure}

\description{
The \strong{ancends} package provides tools for analysis of communities similarity in ecological researches.
}

\details{
Function \code{\link{csf}} is aimed to make paired comparison of the structure of different communities using (dis-)similarity index, combined with simple paired subtraction between variables that correspond to environmental factors. This function is useful for building linear regression or other statistical analysis method for studying factors' impact on the community structure. The underlying assumption is that if difference between factors explains dissimilarity between communities, these factors  explain these communities' structure.

Additional functions were added for data transformation and simple analysis. This package provides the means for easy calculation pf the next indices.

Similarity indices:
- presence-absence: Jaccard, Sorensen, Kulczinsky, Dice association index, Braun-Blanquet, Baroni-Urbani and Buser, Mountford;
- structural: Bray-Curtis, Canberra metric, Squared Euclidian distance, Renkonen, Morisita, logarithmically modified Sorensen;

Diversity indices:
Simpson, Shannon, Buzas and Gibson evenness, Brillouin, Menhinick, Margalef, Equitability, Berger-Parker, Hurlbert's PIE, Hill's numbers.

These indices can be calculated using the \strong{vegan} package, but not all of them (e. g. Renkonen) and without ability to set some details in formulas (logarithm base etc.). Nevertheless, functionality of this package is much higher than \strong{ancends}.
}

\author{
Oleksii Dubovyk
\href{https://orcid.org/0000-0003-2936-6383}{0000-0003-2936-6383}

oadubovyk@gmail.com
}
