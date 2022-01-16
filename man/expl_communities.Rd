\name{expl_communities}
\alias{expl_communities}
\docType{data}
\title{
Explained by Factors Communities
}
\description{
Synthetic data set made for analysis of \code{\link{csf}} function. "A:Z" columns are species, each of which is linearly related to some of three factors or their combination and have it's own formula. The idea is that if each species in community can be described ideally by factors, consequently, community structure can be described ideally by these factors too. This hypothesis can be checked by \code{\link{csf}}.
}
\usage{expl_communities}
\format{
  A data frame with 26 observations on the following 30 variables.
  \describe{
    \item{\code{Factor1}}{a factor with levels "0", "1", "2"}
    \item{\code{Factor2}}{a factor with levels "0", "1", "2"}
    \item{\code{Factor3}}{a factor with levels "0", "1", "2"}
    \item{\code{A}}{= Coef + Factor1 + Factor2 + Factor3}
    \item{\code{B}}{= Coef + Factor1 + Factor2 - Factor3}
    \item{\code{C}}{= Coef + Factor1 + Factor2}
    \item{\code{D}}{= Coef + Factor1 - Factor2 + Factor3}
    \item{\code{E}}{= Coef + Factor1 - Factor2 - Factor3}
    \item{\code{F}}{= Coef + Factor1 - Factor2}
    \item{\code{G}}{= Coef + Factor1 + Factor3}
    \item{\code{H}}{= Coef + Factor1 - Factor3}
    \item{\code{I}}{= Coef + Factor1}
    \item{\code{J}}{= Coef - Factor1 + Factor2 + Factor3}
    \item{\code{K}}{= Coef - Factor1 + Factor2 - Factor3}
    \item{\code{L}}{= Coef - Factor1 + Factor2}
    \item{\code{M}}{= Coef - Factor1 - Factor2 + Factor3}
    \item{\code{N}}{= Coef - Factor1 - Factor2 - Factor3}
    \item{\code{O}}{= Coef - Factor1 - Factor2}
    \item{\code{P}}{= Coef - Factor1 + Factor3}
    \item{\code{Q}}{= Coef - Factor1 - Factor3}
    \item{\code{R}}{= Coef - Factor1}
    \item{\code{S}}{= Coef + Factor2 + Factor3}
    \item{\code{T}}{= Coef + Factor2 - Factor3}
    \item{\code{U}}{= Coef + Factor2}
    \item{\code{V}}{= Coef - Factor2 + Factor3}
    \item{\code{W}}{= Coef - Factor2 - Factor3}
    \item{\code{X}}{= Coef - Factor2}
    \item{\code{Y}}{= Coef + Factor3}
    \item{\code{Z}}{= Coef - Factor3}
    \item{\code{Coef}}{= Coef = max(Factor1) + max(Factor2) + max(Factor3)}
  }
}
\details{
Linear model \code{\link{lm}} can ideally describe any species distribution in formula Species ~ Factor1 + Factor2 + Factor3.
}
\examples{
t(expl_communities)

fitA <- lm(A ~ Factor1 + Factor2 + Factor3, expl_communities)
summary(fitA)
#the linear model ideally describes
#any species of dataset,
#except expl_communities$Coef

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
\keyword{datasets}
