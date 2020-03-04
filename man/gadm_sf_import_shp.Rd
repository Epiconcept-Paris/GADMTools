\name{gadm_sf_import_shp}
\alias{gadm_sf_import_shp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{read and import a file in shapefile format}
\description{
read and import a file in shapefile format (.shp,.dbf,.proj) and put it in gadm_sf format for use with GADMTools }
\usage{
  gadm_sf_import_shp(dir, name, level, del = NULL, 
                     renamed = NULL, keepall = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
\bold{Character} path to the directory where .shp file is located (eg. “./”)
}
  \item{name}{
\bold{Character} name of the .shp file without the extension (example: “india”) 
}
  \item{level}{
\bold{Integer} the administrative level
}
  \item{del}{
\bold{Character vector} the variables (columns) to be deleted (optional if keepall == FALSE)
}
  \item{renamed}{
\bold{Character vector} the variables to be renamed (eg. the administrative fields in GADM are named NAME_X where X is the level, and the ISO code(3))
}
  \item{keepall}{
\bold{Boolean} if FALSE (default), allows to keep only the columns useful for GADMTools
}

}

\value{ \bold{Object} of class gadm_sf (Simple Features wrapper) }


\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}


\examples{
# library(GADMTools)
# map <- gadm_sf_import_shp(dir="./", name = "india", level = 2,
#                           del = c("DCODE", "NAME3", "SDCODE"),
#                           renamed = c(’ISO’ = ’COUNTRY’,
#                                       ’NAME_0’ = ’COUNTRY_LO’,
#                                       ’NAME_1’ = ’NAME1’,
#                                       ’NAME_2’ = ’NAME2’),
#                           keepall = FALSE
#                          )
# map$sf$ISO <- "IND"
# map$sf$NAME_0 <- "India"}

\keyword{ ~spatial }

