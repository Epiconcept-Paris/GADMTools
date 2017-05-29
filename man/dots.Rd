\name{dots}
\alias{dots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot dots on a map}
\description{
Plot points on a map with different colors and shapes.
}
\usage{
dots(x, points, color="red", value = NULL,
    breaks = NULL, steps = 5, palette = NULL, labels = NULL, strate = NULL,
    title="", legend = NULL, note=NULL)

}

\arguments{
  \item{x}{ \bold{Object} GADMWrapper}
  \item{points}{ 
  \bold{Object} data.frame with columns 'latitude' and 'longitude'
}
  \item{color}{ a valid color}
  \item{value}{
  \bold{Character} Name of a column in the data.frame. If is not null, colored dots are displayed according to the value. 
}
  \item{breaks}{\bold{vector} of breaks}
  \item{steps}{
  \bold{Integer} Number of breaks for the value field.
}
  \item{palette}{ a valid palette
}
  \item{labels}{\bold{vector} of labels}
  \item{strate}{
  \bold{Character} name of a column in the data.frame. If is not null,
  display dots with different shapes according to the value.
}
  \item{title}{\bold{Character} The title of the plot}
  \item{legend}{\bold{Character} The title of the legend}
  \item{note}{\bold{Character} Add an annotation}
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
\code{\link{RColorBrewer}}
}
\examples{
# library(GADMTools)
# library(sp)
# data(quakes)
# M <- gadm.loadCountries("FJI", basefile="./")
# colnames(quakes)[1] <- "latitude"
# colnames(quakes)[2] <- "longitude"
# dots(M, quakes, value="mag")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
