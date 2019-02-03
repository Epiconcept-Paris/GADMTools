\name{listNames}
\alias{listNames}

\title{ List the region names for a specific administrative level}
\description{
Returns a list of the names associated with the particular administration level.
}
\usage{
listNames(x, level = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} - a GADMWrapper object
}
  \item{level}{
\bold{Integer} - the value of the administration level to list. Attention: only the administrative levels that have been loaded in the loadCountries object can be listed. Names are given in the country's language or English. 
}
}
\details{
Some GADM country maps provide five or more administrative levels.
}
\value{
\bold{Character vector} of names 
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
# MAP <- gadm.loadCountries("FRA", level=1, basefile="./")
# listNames(MAP, level=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
