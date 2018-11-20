\name{gadm.longTo360}
\alias{gadm.longTo360}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts longitudes from -180° - 0° - 180° to 0° - 360°}
\description{
Converts longitudes of a GADM shapefile to a range of 0° - 360° using the modulo R function.
}
\usage{
gadm.longTo360(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{GADMWrapper Object} to convert.
}
}


\value{ \bold{Object} GADMWrapper }
\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\note{ The transformation is done only when rendering a graph. The original data are not modified.
}

\examples{
# library(GADMTools)
# MAP <- gadm.loadCountries("FJI", level = 0)
# plotmap(MAP)
# MAP <- gadm.longTo360(MAP)
# plotmap(MAP)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
