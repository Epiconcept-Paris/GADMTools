\name{gadm.union}
\alias{gadm.union}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merges regions}
\description{
This function merges regions by removing common borders.
}
\usage{
gadm.union(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{GADMWrapper Object} containing regions.
}

}


\value{ \bold{Object} GADMWrapper }
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}


\examples{
# library(GADMTools)
# FRA <- gadm.loadCountries("FRA", 2, basefile = "./")
# BRE <- GADMTools::subset(FRA, level=1, regions=c("Bretagne"))
# plotmap(BRE)
# BRE <- gadm.union(BRE)
# plotmap(BRE)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
