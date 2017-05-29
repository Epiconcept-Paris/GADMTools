\name{subset}
\alias{subset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract regions}
\description{
With subset you can extract one or more regions from a country at the current level.
}
\usage{
subset(x, level = NULL, regions = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  \item{level}{ \bold{Integer} the level at which the regions are extracted from }
  \item{regions}{ \bold{String vector} of named regions }
}
\details{
----
}
\value{ \bold{Object} GADMWrapper }
%% ------------------------------------------------------------------
\references{
----
}
%% ------------------------------------------------------------------
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}
%% ------------------------------------------------------------------
\note{
----
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{listNames}}
}
\examples{
# library(GADMTools)
# library(sp)
# France = gadm.loadCountries("FRA", level=1, basefile="./")
# Auvergne = subset(France,regions = "Auvergne", level=1)
# plotmap(Auvergne, title="Auvergne - level 1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
