\name{gadm.remove}
\alias{gadm.remove}

\title{ Remove one or more regions from a map in a GADMWrapper object }

\description{
Remove the shapes of one or more regions in a GADMWrapper object.
}
\usage{
gadm.remove(x, level=NULL, regions=NULL)
}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{level}{
  \bold{Integer} - level from which shapes are removed. If NULL, curent level is used.
  }

  \item{regions}{
  \bold{String} - vector of regions to be removed
  }
}
\details{
----
}
\value{
    \bold{Object} - A GADMWrapper object.
}
\references{
----
}
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}
\note{
----
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
----
}
\examples{
# library(GADMTools)
# library(sp)
# map <- gadm.loadCountries(c("FRA"), level=1, basefile="./")
# remove(map, regions="Auvergne")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
