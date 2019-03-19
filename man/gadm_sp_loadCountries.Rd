\name{gadm_sp_loadCountries}
\alias{gadm_sp_loadCountries}
\alias{gadm_sp.loadCountries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load one or more GADM shapefiles (SpatialPolygonsDataFrame)}
\description{
Load one or more GADM shapefiles as SpatialPolygonsDataFrame from a local path or from a remote repository.
}
\usage{
   gadm_sp.loadCountries(fileNames, level = 0, basefile=GADM_BASE,
                         baseurl=GADM_URL, simplify=NULL)

   # deprecated
   gadm_sp.loadCountries(fileNames, level = 0, basefile=GADM_BASE,
                         baseurl=GADM_URL, simplify=NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileNames}{
\bold{Character vector} of named regions. An ISO-3166-1 code or a custom name. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{level}{
\bold{Integer} the level of the administrative boundaries (0 is the country, higher values equal finer divisions) 
}
  \item{basefile}{
\bold{Character vector} the path of the directory where shapefiles are stored. Default is "./GADM"
}
  \item{baseurl}{
\bold{Character vector} The url of GADM files. Default is "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/"
}
  \item{simplify}{
\bold{Numeric} Numerical tolerance value to be used by the Douglas-Peuker algorithm. Higher values use less polygon points (and less memory) and lower values use more polygon points (and more memory). We suggest not going higher than 0.025 in order for intra-country boundaries to align.
}

}

\section{ISO-3166-1 }{

See : \url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3} or take a look on the vignette "GADMTools - ISO 3166-1 alpha-3"
}

\value{ \bold{Object} gadm_sp }
\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}

\note{
gadm_sp.loadCountries() and gadm.loadCountries are deprecated, they will be removed in the next release. Please use gadm_sp_loadCountries()
}

\seealso{\code{\link{gadm_sf.loadCountries}}}

\examples{
# library(GADMTools)
#
# Belgium = gadm_sp_loadCountries("BEL", level=2, basefile="./")
# plotmap(Belgium)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
