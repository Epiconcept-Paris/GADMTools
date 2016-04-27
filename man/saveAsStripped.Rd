\name{saveAsStripped}
\alias{saveAsStripped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strip a GADMWrapper object}
\description{
Strip a GADMWrapper object (with property 'stripped' == FALSE) and save it  stripped (with property 'stripped' == TRUE).
}
\usage{
saveAsStripped(x, name, level= NULL, basefile = './')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} GADMWrapper with stripped property == FALSE
}
  \item{name}{
\bold{String} of a named region. An ISO-3166-1 code or a custom name. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{level}{
\bold{Integer} the level of the administrative boundaries (0 is the country, higher values equal finer divisions) 
}
  \item{basefile}{
\bold{String} the path of the directory where shapefiles are stored. Default is "./"
}
}


\value{ \bold{Object} GADMWrapper with stripped property == TRUE }
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
# saveAsStripped(BE, "BEL", level=1)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
