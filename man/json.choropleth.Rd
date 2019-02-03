\name{json.choropleth}
\alias{json.choropleth}

\title{ Create a geojson choropleth of selected regions.}

\description{
Drawing a choropleth (colored regions based on data values) with GADMTools is straightforward. You just have to select your shape(s) file(s) with \emph{gadm.loadcountries}, load your data from a
csv file for example, and call the json.choropleth function with the right arguments. json.choropleth creaate a GEOJSON file (output.json) that can be used with Leaflet library.
}
\usage{
json.choropleth (x, data, value=NULL, breaks = NULL, steps = 5,
  adm.join=NULL, legend = NULL, labels = NULL, palette=NULL,
  title="")
}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{data}{
\bold{data.frame} - data to plot
}
  \item{value}{
  \bold{String} - the name of the column in the data.frame we want to plot (eg: an incidence in epidemiology studies)
}
  \item{breaks}{
%%     ~~Describe \code{regioncolor} here~~
}
  \item{steps}{ \bold{Integer} - number of breaks. Default = 5. If \emph{breaks} is \bold{NOT NULL} this value is used internally with cut().
}
  \item{adm.join}{ \bold{String} - the name in GADM spdf dataset which will be
  joined with a column of the data.
}
  \item{legend}{
  \bold{String} - legend title. Default \bold{NULL}. 
}
  \item{labels}{ \bold{String vector} labels for the legend. Default \bold{NULL}}
  
  \item{palette}{
  \bold{String} - An RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}.
}
  \item{title}{
  \bold{String} - Title of the plot. Default is an empty string.
}
}
\details{
----
}
\value{
    \bold{Object} a lattice plot of class "trellis"
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
\link{classIntervals}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
