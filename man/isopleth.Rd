\name{isopleth}
\alias{isopleth}

\title{ Draw an isopleth on selected regions }

\description{
Drawing an isopleth (also known as heat maps) with GADMTools is straightforward. You just have to select your shape(s) file(s) with \emph{gadm_loadcountries}, load your data from a
csv file for example, and call the isoopleth function with the right arguments.
}
\usage{
isopleth(x, data, palette=NULL, title="", subtitle = "", caption = "")
}
\arguments{
  \item{x}{ \bold{Object} gadm_sp }
  
  \item{data}{\bold{data.frame} - data to plot}
  
  \item{palette}{
  \bold{String} - An RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}.
}
  \item{title}{  \bold{String} - Plot title. Default is an empty string.}
  \item{subtitle}{  \bold{String} - Plot subtitle. Default is an empty string.}
  \item{caption}{  \bold{String} - Plot caption. Default is an empty string.}
}

\value{
    \bold{Object}  ggplot2
}

\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
library(GADMTools)
data(Corsica)

longitude <- runif(6, min=8.74, max = 9.25)
latitude  <- runif(6, min=41.7, max = 42.6)
Cases <- runif(6, 25, 112)
DAT <- data.frame(longitude, latitude, Cases)

isopleth(Corsica, data = DAT, palette = "Blues")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
