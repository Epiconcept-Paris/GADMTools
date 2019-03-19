\name{gadm_removeBackground}
\alias{gadm_removeBackground}
\alias{gadm.removeBackground}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Removes the background of a map}
\description{
Removes the backround previously loaded with gadm_getBackground. Original .tif file is not deleted.
}
\usage{
   gadm_removeBackground(x)
   
   gadm.removeBackground(x) # deprecated
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} gadm_sp or gadm_sf of the region that you want to remove the background.}

}

\value{ \bold{Object} gadm_sp or gadm_sf}

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\seealso{\code{\link{gadm_getBackground}}}

\examples{
# library(GADMTools)
# Loads France @ level 2 (departements)
# FRA <- gadm_sf.loadCountries("FRA", level = 2, basefile = "DATA/")
# FRA <- gadm_getBackground(FRA, name = "FRABGND", clip = FALSE)
# plotmap(FRA)
# FRA <- gadm_removeBackground(FRA)
# plotmap(FRA)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
