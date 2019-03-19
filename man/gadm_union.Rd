\name{gadm_union}
\alias{gadm_union}
\alias{gadm.union}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merges regions}
\description{
This function merges regions by removing common borders.
}
\usage{
   gadm_union(x, level = 0, type = "?")

   gadm.union(x, level = 0, type = "?") # deprecated
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\bold{Object} gadm_sf or gadm_sp containing regions.}
  \item{level}{\bold{integer} level @ union is procesed. For gadm_sf objects only.
  For gadm_sp objects, union is processed on the whole map.}
  \item{type}{\bold{character} alternative name.}
}


\value{ \bold{Object} same as input, gadm_sf or gadm_sp }
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\note{
gadm.union() is deprecated, it will be removed in the next release. Please use gadm_union()
}

\examples{
library(GADMTools)
data("Corsica")

plotmap(Corsica)

Corse <- gadm_union(Corsica, level=2)
plotmap(Corse)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
