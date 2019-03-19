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
  \bold{Object} gadm_sf or gadm_sp
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

\value{
\bold{Object} ggplot2
}

\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{classDots}}}

\examples{
library(GADMTools)
data("Corsica")

longitude <- runif(7, min=8.74, max = 9.25)
latitude  <- runif(7, min=41.7, max = 42.6)
Cases <- runif(7, 25, 100)
DAT <- data.frame(longitude, latitude, Cases)

propDots(Corsica, data = DAT, value="Cases",
         breaks=c(0, 25, 50, 75, 100), range = c(25, 100))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
