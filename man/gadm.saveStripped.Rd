\name{gadm.saveStripped}
\alias{gadm.saveStripped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Save a stripped GADM object}
\description{
Save a stripped ( with stripSP() ) GADM object for later use it with ggplot2.
}
\usage{
gadm.saveStripped(x,fname, basefile = './')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} GADMWrapper with stripped property == TRUE
}
  \item{fname}{
\bold{String} file name of a region. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{basefile}{
\bold{Character vector} the path of the directory where shapefiles are stored. Default is "./"
}
}


\value{ \bold{Boolean} TRUE}
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
# BE <- gadm.loadCountries('BEL', level=2)
# S_BE <- stripSP(BE)
# gadm.saveStripped(S_BE, "BEL")
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
