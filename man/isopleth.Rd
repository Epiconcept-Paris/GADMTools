\name{isopleth}
\alias{isopleth}

\title{ Draw an isopleth on selected regions }

\description{
Drawing an isopleth (also known as heat maps) with GADMTools is straightforward. You just have to select your shape(s) file(s) with \emph{gadm.loadcountries}, load your data from a
csv file for example, and call the isoopleth function with the right arguments.
}
\usage{
isopleth(x, data, palette=NULL, title="")
}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{data}{
\bold{data.frame} - data to plot
}
  
  \item{palette}{
  \bold{String} - An RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}.
}
  \item{title}{
  \bold{String} - Plot title. Default is an empty string.
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

# France = gadm.loadCountries("FRA", level=1, simplify=0.01)
# W <- read.csv2("wepi.csv")
# W$lieux_lat <- as.double(as.character(W$lieux_lat))
# W$lieux_long <- as.double(as.character(W$lieux_long))
# colnames(W)[1] <- "latitude"
# colnames(W)[2] <- "longitude"
# Region = subset(France, regions=c("Ile-de-France","Haute-Normandie"), level=1)
# isopleth(Region, W)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
