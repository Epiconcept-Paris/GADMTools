\name{gadm_getBbox}
\alias{gadm_getBbox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get the bounding box of the map}
\description{
get the bounding box of the map
}
\usage{
gadm_getBbox(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\bold{Object} of class gadm_sf or gadm_sp }
}

\value{ \bold{vector} of numeric values of: 
  \itemize{
    \item{\bold{xmin} minimum longitude}
    \item{\bold{ymin} minimum latitude}
    \item{\bold{xmax} maximum longitude}
    \item{\bold{ymax} maximum latitude}
  }
}

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\seealso{\code{\link{gadm_crop}}}
\examples{
library(GADMTools)
data("Corsica")

gadm_getBbox(Corsica)

}
\keyword{ ~documentation }

