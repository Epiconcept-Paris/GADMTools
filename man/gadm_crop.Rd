\name{gadm_crop}
\alias{gadm_crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{crop a region to a specific rectangle}
\description{
crop a region to a specific rectangle
}
\usage{
gadm_crop(x, xmin, ymin, xmax, ymax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\bold{gadm_sp or gadm_sf Object} containing regions.}
  \item{xmin}{\bold{numeric} Longitude min}
  \item{ymin}{\bold{numeric} Latitude min}
  \item{xmax}{\bold{numeric} Longitude max}
  \item{ymax}{\bold{numeric} Latitude max}
}


\value{ \bold{Object} gadm_sf or gadm_sp }

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}


\examples{
library(GADMTools)
data("Corsica")

area <- gadm_crop(Corsica, xmin=9.3, ymin=42.96, xmax=9.566, ymax=43.02819)
plotmap(area)
}

\keyword{ ~documentation }

