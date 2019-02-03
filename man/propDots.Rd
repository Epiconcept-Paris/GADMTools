\name{propDots}
\alias{propDots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot proportionnal circles (dots) on a map}
\description{
Plot values as proportionnal circles on a map.
}
\usage{
propDots(x, data, value, breaks=NULL, range=NULL,
         labels=NULL, color="red", title="", 
         subtitle = "", caption = "", note=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  \bold{Object} GADMWrapper
  }
  \item{data}{
  \bold{Object} data.frame with columns 'latitude' and 'longitude'
}
  \item{value}{
  \bold{Character} Name of a column of the data.frame.}
  
  \item{breaks}{ a vector of breaks
}
  \item{range}{ vector min, max}
  \item{labels}{vector of labels}
  \item{color}{a valid color}
  \item{title}{\bold{Character} title of the plot}
  \item{subtitle}{\bold{Character} subtitle of the plot}
  \item{caption}{\bold{Character} caption of the plot}
  \item{note}{\bold{Character} A note associated with the plot}
}
\details{
----
}
\value{
\bold{Object} ggplot2
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
# data(quakes)
# M <- gadm.loadCountries("FJI", basefile="./")
# colnames(quakes)[1] <- "latitude"
# colnames(quakes)[2] <- "longitude"
# propDots(M, quakes, value="mag")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
