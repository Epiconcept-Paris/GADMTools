\name{gadm_subset}
\alias{gadm_subset}
\alias{gadm.subset}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract regions}
\description{
With subset you can extract one or more regions from a country at the current level.
}
\usage{
   gadm_subset(x, level = NULL, regions = NULL, usevar = NULL)

   gadm.subset(x, level = NULL, regions = NULL, usevar = NULL) # deprecated

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \bold{Object} gadm_sf or gadm_sp }
  \item{level}{ \bold{Integer} the level at which the regions are extracted from }
  \item{regions}{ \bold{character vector} of named regions }
  \item{usevar}{ \bold{character} name of an other var of the internal dataset of map }
}


\value{ \bold{Object} As input object, gadm_sf or gadm_sp }

\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}

\note{
gadm.subset() is deprecated, it will be removed in the next release. Please use gadm_subset()
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{listNames}}
}
\examples{
library(GADMTools)
data("Corsica")

Calvi <- gadm_subset(Corsica, 4, "Calvi")
plotmap(Calvi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
