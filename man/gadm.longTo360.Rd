\name{gadm_longTo360}
\alias{gadm_longTo360}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts longitudes from -180° - 0° - 180° to 0° - 360°}
\description{
Converts longitudes of a GADM shapefile to a range of 0° - 360° using the modulo R function.
}
\usage{
gadm_longTo360(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} gadm_sf or gadm_sp.
}
}


\value{ \bold{Object} gadm_sp }

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\note{ For gadm_sp maps, the transformation is done only when rendering a graph. The original data are not modified. For gadm_sf maps, the internal geometry is modified.
}

\examples{
# library(GADMTools)
# MAP <- gadm_sf.loadCountries("FJI", level = 0)
# plotmap(MAP)
# MAP <- gadm_longTo360(MAP)
# plotmap(MAP)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
