\name{stripSP}
\alias{stripSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Strip a GADMWrapper object}
\description{
Strip a GADMWrapper object (with property 'stripped' == FALSE) and return a stripped GADMWrapper object (with property 'stripped' == TRUE)
}
\usage{
stripSP(x, level=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} GADMWrapper with property 'stripped' == FALSE
}
  \item{level}{
\bold{Int} admin level to be stripped/extracted. If NULL, the current level is selected
}
}


\value{ \bold{Object} GADMWrapper with property 'stripped' == TRUE }
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
# Belgique <- stripSP(BE, level=2)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
