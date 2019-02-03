\name{strippedExists}
\alias{strippedExists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test if a stripped GADMWrapper object exists}
\description{
Test if a stripped GADMWrapper object exists on the file system in the directory 'basefile'
}
\usage{
strippedExists(name, level, basefile = './')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
\bold{Character vector} of a named region. An ISO-3166-1 code or a custom name. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{level}{
\bold{Integer} the level of the administrative boundaries (0 is the country, higher values equal finer divisions) 
}
  \item{basefile}{
\bold{Character vector} the path of the directory where shapefiles are stored. Default is "./"
}
}


\value{ \bold{Boolean} TRUE if the file exists, FALSE if not }
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
# if (strippedExists('BEL', level = 2) {
#   BE <- gadm.loadStripped("BEL", level=2)
# }
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
