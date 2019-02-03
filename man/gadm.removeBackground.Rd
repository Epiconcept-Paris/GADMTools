\name{gadm.removeBackground}
\alias{gadm.removeBackground}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Removes the background of a map}
\description{
Removes the backround previously loaded with gadm.getBackground. Original .tif file is not deleted.
}
\usage{
gadm.removeBackground(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} GADMWrapper or GT2 of the region that you want to remove the background.}

}

\value{ \bold{Object} GADMWrapper or GT2}

\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}
\note{
----
}

\seealso{\code{\link{gadm.loadCountries}}}

\examples{
# library(GADMTools)
# Loads France @ level 2 (departements)
# FRA <- gadm.sf.loadCountries("FRA", level = 2, basefile = "DATA/")
# FRA <- gadm.getBackground(FRA, name = "FRA", clip = FALSE)
# plotmap(FRA)
# FRA <- gadm.removeBackground(FRA)
# plotmap(FRA)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
