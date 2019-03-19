\name{gadm_remove}
\alias{gadm_remove}
\alias{gadm.remove}

\title{ Remove one or more regions from a map }

\description{
Remove the polygons of one or more regions from a map.
}
\usage{
   gadm_remove(x, level=NULL, regions=NULL)

   gadm.remove(x, level=NULL, regions=NULL) # deprecated

}
\arguments{
  \item{x}{ \bold{Object} gadm_sf or gadm_sp }
  
  \item{level}{
  \bold{Integer} - level from which shapes are removed. If NULL, curent level is used.
  }

  \item{regions}{
  \bold{String} - vector of regions to be removed
  }
}
\value{
    \bold{Object} - As input object, gadm_sf or gadm_sp.
}
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}



\seealso{\code{\link{listNames}}}

\examples{
library(GADMTools)
data("Corsica")

HCorse <- gadm_remove(Corsica, level=2, "Corse-du-Sud")
plotmap(HCorse)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }

