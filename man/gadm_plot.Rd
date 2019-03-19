\name{gadm_plot}
\alias{gadm_plot}
\alias{plotmap}

\title{ Draw a gadm_sf or gadm_sp object}

\description{
Draw a gadm_sf or gadm_sp object with ggplot2
}

\usage{
   gadm_plot(x, title="") 

   plotmap(x, title="") # deprecated
}
\arguments{
  \item{x}{ \bold{Object} gadm_sf or gadm_sp }
  
  \item{title}{\bold{String} - Title of the plot. Default is an empty string}
}

\value{
    \bold{Object}  ggplot2
}
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}

\note{
plotmap() is deprecated, it will be removed in the next release
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
library(GADMTools)
data("Corsica")

gadm_plot(Corsica)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
