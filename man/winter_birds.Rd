\name{winter_birds}
\alias{winter_birds}
\docType{data}
\title{
Winter bird assemblages in urban greenery in Lviv, Ukraine
}
\description{
This dataset is a result of winter avifauna censuses in eight parks and cemeteries in Lviv of different area.
}
\usage{winter_birds}
\format{
  \code{winter_birds} is a data frame with 40 observations on the following 8 variables. Observations are the number of individuals of one species per transect (i. e. individuals per 0.05 km2 sunce in this research transect length was 500 m). Variables are model plots.
  Actually, table structure is statistically incorrect, but it is typical for results of faunistic works. Correct table might be transposed.
}
\details{
Model plots names are codes of parks in Lviv: \code{BP} - Bilogorsha park (135 ha), \code{OP} - Osvytsky Hay and John Paul II Park (64 ha), \code{HC} - Holoskiv cemetery (41 ha), \code{LC} - Lychakiv cemetery (28 ha), \code{YC} - Yaniv cemetery (36 ha), \code{PP} - Pohulianka park (113 ha), \code{LP} - Lychakiv park (10 ha), \code{SP} - Stryisky park (53 ha).
Rownames are species names abbreviations (e. g. \code{Strura} - \emph{Strix uralensis}, \code{Denmaj} - \emph{Dendrocopos major}, but \code{Ccorax} - \emph{Corvus corax}, \code{Ccornix} - \emph{Corvus cornix}).
}
\source{
Dubovyk OA (2018) Winter bird assemblages in urban greenery in Lviv, Ukraine. Book of Abstracts of XIV International Scientific Conference for Students and PhD Students "Youth and Progress of Biology": 181
}
\examples{
t(winter_birds)
}
\keyword{datasets}
