\name{ccompare}
\alias{ccompare}
\title{
Taxa Presence-Absence Communities Comparison
}
\description{
Provides presence-absence comparison of data about two communities using certain similarity index.
}
\usage{
ccompare(data1, data2,  method = "jaccard", just.index = TRUE)

}
\arguments{
  \item{data1}{
Data frame or vector with taxa abundance values (1 column, number of rows equals number of species). If \code{data2} is missed, it should be a data frame with data about two communities. Note that in rows must be species, columns - analyzed communities
}
  \item{data2}{
Second data frame or vectors with taxa density values (1 column, number of rows equals number of species). Rows of species should be identical to appropriate in \code{data1}
  }
  \item{method}{
Method of comparison: Jaccard (\code{method = "jaccard"}, default), Sorensen (\code{"sorensen"}), Kulczinski(\code{"kulczinski"}), Ochiai(\code{"ochiai"}), Dice(\code{"dice"}), Braun-Blanquet (\code{"bblanquet"}), Baroni-Urbani & Buser (\code{"baroni"}) or Mountford (\code{"mountford"}) index
  }
  \item{just.index}{
  Logical. Return just similarity index (\code{TRUE}) or list with formula coefficients, method name, index value and presence-absence table?
  }
}
\details{
Presence-absence comparison transforms density data about taxa distribution in two samples into presence (1) or absence (0) table. After this program compare two samples using one of those indices:

\emph{Jaccard index = c/(a+b+c)}

\emph{Sorensen index = (2*c)/(a+b)}

\emph{Kulczinsky index = (c/2)*((1/a)+(1/b))}

\emph{Ochiai index = c/sqrt(a*b)}

\emph{Dice association index = c/min(a, b)}

\emph{Braun-Blanquet index = c/max(a, b)}

\emph{Baroni-Urbani & Buser index = (sqrt(c*d)+c)/(sqrt(cd)+a+b-c)}

\emph{Mountford index = (2*c)/((2*a*b)-((a+b)*c))}

where \emph{a} and \emph{b} are numbers of taxa in two communities, \emph{c} - number of taxa shared between communities, \emph{d} - the number of taxa absent in both communities.
}
\value{

If \code{just.index = FALSE}
list

\item{species}{vector; number of species in first (A) and second (B) sample, number of species in common between two samples, different species between two samples and general number of species presented in both samples}
\item{method}{index that was used to calculate similarity index}
\item{statistic}{the value of the selected index}
\item{presence_absence_table}{presence (1) or absence (0) table}

If \code{just.index = TRUE}
numeric object - chosen index value.
}
\references{
Jaccard P (1901) Etude comparative de la distribution florale dans une portion des Alpes et des Jura. Bulletin de la Societe Vaudoise des Sciences Naturelles, 37: 547–579

Sorensen TA (1948) A method of establishing groups of equal amplitude in plant sociology based on similarity of species content, and its application to analyses of the vegetation on Danish commons. K dan Vidensk Selsk Biol Skr 5:1-34

Kulczynski S (1927) Die Pflanzenassoziationen der Pieninen, Bulletin Internationalde l'Academie A Polonaise des Sciences et des Lettres, Classe des Sciences, 1927

Ochiai A (1957) Zoogeographic studies on the soleoid fishes found in Japan and its neighbouring regions. Bull Jpn Soc Sci Fish 22: 526–530

Dice LR (1945) Measures of the amount of ecological association between species. Ecology 26 : 297-302

Braun-Blanquet J (1928) Pflanzensoziologie Grundzüge der Vegetationskunde. Berlin: Verlaq von Julius springer, 1928. 330 s.

Baroni-Urbani C, Buser MW (1976) Similarity of binary data. Syst Zool 25:251-259

Mountford MD (1962) An index of similarity and its application to classificatory problems. In: PW Murphy (ed), Progress in Soil Zoology. Butterworths, London, pp. 43-50
}
\seealso{
\code{\link{bblanq}}, \code{\link{baroni}}, \code{\link{jaccard}}, \code{\link{sorensen}}, \code{\link{kulcz}}, \code{\link{ochiai}}, \code{\link{diceai}}, \code{\link{mountford}}
}
\examples{
ccompare(winter_birds[1:2])
#returns the Jaccard index value as default method

ccompare(winter_birds[1], winter_birds[2])

ccompare(winter_birds[1], winter_birds[2], method = "sorensen")
}
