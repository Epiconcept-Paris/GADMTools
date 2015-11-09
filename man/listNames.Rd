\name{listNames}
\alias{listNames}

\title{ List the names of regions for a specific level}
\description{
List the names associated with the administration level.
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
\bold{Integer} - the value of the administration level (eg 0 to 3)
}
}
\details{
Some countries provides by GADM have 5 levels or more.
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
library(GADMTools)
library(sp)
MAP <- gadm.loadCountries("FRA", level=1, basefile="./")
listNames(MAP)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
