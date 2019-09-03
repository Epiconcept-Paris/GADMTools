\name{dotDensity}
\alias{dotDensity}

\title{ Multivariate Dot-Density maps }

\description{
A dot-density map is one way to map aggregated spatial data without some of the distortions inherent in choropleths.
}
\usage{
dotDensity(map, data, adm.join  = NULL, values = NULL,
                            cases.by.dots = 100, dot.size = .25, labels = NULL,
                            palette   = NULL, title = NULL, subtitle  = NULL,
                            caption   = NULL)

}
\arguments{
  \item{map}{ \bold{Object} gadm_sf }
  
  \item{data}{ \bold{data.frame} - data to plot }
  
  \item{values}{   \bold{String} - the names of the columns in the data.frame we want to plot. (eg: number of cases)}
  \item{cases.by.dots}{\bold{integer} of breaks values }
  
  \item{dot.size}{ \bold{numericr} - size of dots. Default = 0.25. }
  
  \item{adm.join}{ \bold{String} - the name in your dataset joined with the field NAME_X of the map, where X is the level of the administrative boundaries. For instance if the level is about 'Districts' of a country, and your dataset has a field named "Study_Location" containing a list of districts, just do adm.join = "Study_Location". }

  \item{labels}{ \bold{String vector} labels for the legend. Default NULL. If NULL values are used as labels}
  
  \item{palette}{ \bold{String} - An RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}. }
  
  \item{title}{ \bold{String} - title of the plot. Default is NULL}
  \item{subtitle}{ \bold{String} - subtitle of the plot. Default is NULL.}
  \item{caption}{ \bold{String} - caption of the plot. Default is NULL.}
}
\details{
----
}
\value{
    \bold{Object}  ggplot2
}
\references{
\url{https://en.wikipedia.org/wiki/Dot_distribution_map}
}
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}
\note{
dotDensity only works with maps loaded with gadm_sf_loadCountries
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{classIntervals}
}
\examples{
library(GADMTools)
data("Corsica")

# Creates test data.frame (fake data) --------------------------------------
# --------------------------------------------------------------------------
VAR_1 <- as.integer(runif(n = 43, min = 800, max = 15800))
VAR_2 <- as.integer(runif(n = 43, min = 1000, max = 15800))
VAR_3 <- as.integer(runif(n = 43, min = 1500, max = 15800))
Cantons <- listNames(Corsica, 4)
DF <- data.frame(Cantons, VAR_1, VAR_2, VAR_3, stringsAsFactors = FALSE)

dotDensity(Corsica,
                DF,
                adm.join="Cantons",
                values = c("VAR_1", "VAR_2", "VAR_3"),
                labels = c("H1N1", "H1N2", "H2N2"),
                palette = c("#ffff00", "#ffaa00", "#FF3200"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }

