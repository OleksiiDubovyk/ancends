\name{csf}
\alias{csf}
\title{
Compare Similarity and Factors
}
\description{
Paired comparison of the structure of different communities using similarity index, conjugated with simple paired subtraction between variables that can mean environmental factors.

This function is useful for building linear regression or other statistical analysis method for studying factors' impact on the communities structure. The assumption is that if difference between factors explains dissimilarity between communities, then factors can explain these communities' structure.
}
\usage{
csf(community, factors, method = "sorensen",
    transposed = FALSE, shorten.result = TRUE)
}
\arguments{
  \item{community}{
Depended (response values) part of data frame, which is a table (data frame) of species density or percentage in different communities.
  }
  \item{factors}{
Independent (predictors) part of data frame, which is a table (data frame) of factor values.
  }
  \item{method}{
Method of comparison, which index should be calculated.
\code{method = "jaccard"} - Jaccard index,
\code{"sorensen"} - Sorensen index,
\code{"braycurtis"} - Bray-Curtis index,
\code{"braycurtis2"} - Squared Bray-Curtis index,
\code{"canber"} - Canberra metric,
\code{"sqeudist"} - Squared Euclidian distance,
\code{"morisita"} - Morisita index,
\code{"log.sorensen"} - logarithmic modification of Sorensen index,
\code{"renkonen"} - Renkonen index;
defaults to \code{"sorensen"}
  }
  \item{transposed}{
Logical; what structure does the data have? (\code{FALSE}) - initial data frame have the structure "columns are species and factors, rows are samples" or (\code{TRUE}) - initial data frame have the structure "columns are samples, rows are species and factors"; defaults to (\code{FALSE}).
  }
  \item{shorten.result}{
Logical; should be returned shorten comparison table without "self-with-self" and negative comparisons? Defaults to (\code{TRUE}).
  }
}
\details{
This algorithm can be used for analysis of the factors effects on the species composition of community using presence-absence similarity indices and for studying this effects influence on the species distribution in communities using structural similarity indices.

Presence-absence similarity indices:

\code{"jaccard"} Jaccard SI \emph{K = c/(a+b-c)}

\code{"sorensen"} Sørensen SI \emph{K = 2*c/(a+b)}

where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities.

Structural similarity indices:

\code{"braycurtis"} Bray-Curtis SI \emph{1 - BC = 1 - (sum abs(n[1i] - n[2i]))/sum (n[1i]+n[2i])}

\code{"braycurtis2"} Squared Bray-Curtis SI \emph{(1 - BC)^2 = (1 - (sum abs(n[1i] - n[2i]))/sum (n[1i]+n[2i]))^2}

\code{"canber"} Canberra metric \emph{1 - CM = 1 - (1/(a+b-c))*sum((abs(n[1i]-n[2i]))/(n[1i]+n[2i]))}

\code{"sqeudist"} Squared Euclidian Distance \emph{1 - D^2 = 1 - sum((p[1i]-p[2i])^2)}

\code{"morisita"} Morisita SI \emph{K = 2*sum(n[1i]*n[2i])/((D[1] + D[2])*N[1]*N[2])}

\code{"log.sorensen"} logarithmic modification of Sorensen index \emph{K = 2*sum(ln(min n[ji])/ln(max n[ji]))/a+b}

\code{"renkonen"} Renkonen SI \emph{K = sum(min(p[1i], p[2i]))}

where \emph{n[ji]} - number of individuals of \emph{i} taxa in \emph{j} community,

      \emph{p[ji]} - percentage of individuals of \emph{i} taxa in \emph{j} community,

      \emph{N[j]} - general number of all individuals in \emph{j} community,

      \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities,

      \emph{D[i] = (sum(n[ji]*(n[ji]-1)))/N[j]*(N[j]-1) = Unbiased Simpson Dominance}.

For comparing factors simple arithmetic subtraction is being used.

In the case of \code{shorten.result = FALSE} in the result matrix will be \emph{n[c] = n[s]^2} rows, where \emph{n[s]} is number of samples in the initial data frame. Linear model will return estimates close to zero and p-values 1: this is because result of the \code{csf} function is extremely heteroscedastic since negative factors' differences compendate for the positive ones. This can be solved easily by using \code{\link{abs}} function, but in this data frame all values will be doubled which can cause type II error.

\code{shorten.result = TRUE} erases all rows in which all factors differences are zero and "self-with-self" comparisons and rows with negative first factor differences. Ideally, in this case in the result matrix should be \emph{n[c] = (n[s]^2-n[s])/2} rows, but can be less. So, \code{shorten.result = TRUE} is undesirable when the initial data sample number \emph{n[s]} is limited.
}
\value{
Matrix in which first column is similarity between samples, next are differences between factors.
}
\note{
If \code{'shorten.result = TRUE'} and differences between all of the factors equal zero, this row will be ignored
}
\examples{
result <- abs(csf(community = expl_communities[4:30],
  factors = expl_communities[1:3],
  method = "braycurtis2",
  shorten.result = FALSE))
#create the extended version of comparisons
#of communities of species
#that each can be easily predicted by
#3 factors
fit <- lm(Similarity ~ Factor1 + Factor2 + Factor3, result)
summary(fit)
#all of predictors have significant impact on the similarity value
}
