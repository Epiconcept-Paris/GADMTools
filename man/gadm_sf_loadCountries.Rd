\name{gadm_sf_loadCountries}
\alias{gadm_sf_loadCountries}
\alias{gadm_sf.loadCountries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load one or more GADM shapefiles}
\description{
Load one or more GADM shapefiles as Simple Features (SF) format from a local path or from a remote repository.
}
\usage{
   gadm_sf_loadCountries(fileNames, level = 0, basefile="./",
                         baseurl=GADM_SF_URL, simplify=NULL)

   # deprecated :
   gadm_sf.loadCountries(fileNames, level = 0, basefile="./",
                         baseurl=GADM_SF_URL, simplify=NULL)  
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
\bold{Character vector} the path of the directory where shapefiles are stored. Default is "./"
}
  \item{baseurl}{
\bold{Character vector} The url of GADM files. Default is "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/"
}
  \item{simplify}{
\bold{Numeric} Numerical tolerance value to be used by the Douglas-Peuker algorithm. Higher values use less polygon points (and less memory) and lower values use more polygon points (and more memory). We suggest not going higher than 0.025 in order for intra-country boundaries to align.
}

}

\value{ \bold{Object} of class gadm_sf (Simple Features wrapper) }


\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}
\note{
See : \url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3} for a list of ISO3 codes or 
take a look on the vignette "GADMTools - ISO 3166-1 alpha-3".

gadm_sf.loadCountries() is deprecated, it will be removed in the next release. Please use gadm_sf_loadCountries()
}

\seealso{\code{\link{gadm_sp_loadCountries}}}

\examples{
# library(GADMTools)
# library(sp)
# Belgium = gadm_sf_loadCountries("BEL", level=2, basefile="./")
# plotmap(Belgium)
}

\keyword{ ~spatial }

