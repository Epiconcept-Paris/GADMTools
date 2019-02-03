\name{plotmap}
\alias{plotmap}

\title{ Draw a shapefile from a GADMWrapper object }

\description{
Given the graphic object of a map, this function will display it with a title.
}
\usage{
plotmap(x, title="")
}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{title}{
  \bold{String} - Title of the plot. Default is an empty string
}
}
\details{
----
}
\value{
    \bold{Object}  ggplot2
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
# map <- gadm.loadCountries(c("FRA"), level=1, basefile="./")
# plotmap(map, title="carte de la France")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
