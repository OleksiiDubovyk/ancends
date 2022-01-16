\name{diversity}
\alias{diversity}
\title{
Diversity menu
}
\description{
Diversity indices: Simpson index, Shannon index, Buzas & Gibson Evenness, Brillouin index, Menhinick dominance index, Margalef dominance index, Equitability, Berger-Parker index.
}
\usage{
diversity(data, transposed = FALSE)
}
\arguments{
  \item{data}{
  Community data, a matrix-like object or a vector.
  }
  \item{transposed}{
  Logical. Calculates indices by \code{data} rows if \code{TRUE} or by columns if \code{FALSE}. Defaults to \code{FALSE}.
  }
}
\details{
For all indices require logarithms (Shannon index, Buzas & Gibson Evenness, Brillouin index, Margalef dominance index, Equitability) it is presumed that logarithm is natural. Simpson index and Shannon index are being calculated in the biased mode.

\code{Simpson} \emph{1 - D = 1 - sum (n[i]/N)^2}

where \emph{n[i]} is number of individuals of \emph{i} taxa and \emph{N} is general number of all individuals in the sample.

\code{Shannon} \emph{H = - sum (n[i]/N*ln(n[i]/N))}.

Buzas and Gibson \code{Evenness} \emph{E = (e^H)/S}

where \emph{S} is number of taxa.

\code{Brillouin} \emph{HB = (ln(n[i]!) - sum ln(n[i]!))/N}.

\code{Menhinick} \emph{Me = S/sqrt(N)}.

\code{Margalef} \emph{Ma = (S-1)/ln(N)}.

\code{Equitability} \emph{Eq = H/ln(S)}.

\code{BergerParker} Dominance \emph{BP = max(n[i])/N}.

Modifiable versions of all this indices with configurable logarithm bases and unbiased modes are available in \code{\link{simpson}},
\code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{margalef}}, \code{\link{equit}}.
}

\value{
Matrix with indices values.
}
\references{
Simpson, EH (1949) Measurement of diversity. Nature. 163: 688

Shannon CE (1948) A mathematical theory of communication. The Bell System Technical Journal, 27: 379-423 and 623-656

Ludwig J.A. and Reynolds J.F. 1988. Statistical Ecology – a Primer on Methods and Computing. John Wiley and Sons, Toronto.

Harper, D.A.T. (ed.). 1999. Numerical Palaeobiology. John Wiley & Sons

Hammer Ø, Harper DAT, Ryan PD (2001) PAST: Paleontological statistics software package for education and data analysis. Palaeontologia Electronica 4(1): 9pp
}

\seealso{
\code{\link{simpson}}, \code{\link{shannon}}, \code{\link{evenness}}, \code{\link{brillouin}}, \code{\link{menhinick}}, \code{\link{margalef}}, \code{\link{equit}}, \code{\link{bergerparker}}

\code{\link[vegan]{diversity}} (vegan package)
}

\examples{
diversity(winter_birds)

diversity(winter_birds[1])
}
