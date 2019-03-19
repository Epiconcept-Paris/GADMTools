\name{saveAsStripped}
\alias{saveAsStripped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strip a gadm_sp object}
\description{
Strip a gadm_sp object (with property 'stripped' == FALSE) and save it  stripped (with property 'stripped' == TRUE).
}
\usage{
saveAsStripped(x, fname, name= NULL, basefile = './')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} gadm_sp with stripped property == FALSE
}
  \item{fname}{
\bold{String} file name of the region to save. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{name}{
\bold{String} the name of the field in spdf,  like "NAME_1". 
}
  \item{basefile}{
\bold{String} the path of the directory where shapefiles are stored. Default is "./"
}
}


\value{ \bold{Object} gadm_sp with stripped property == TRUE }
\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}
\note{
----
}

\seealso{
----
}
\examples{
# library(GADMTools)
# library(sp)
# BE <- gadm_loadCountries('BEL', level=2)
# saveAsStripped(BE, "BEL", level=1)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
