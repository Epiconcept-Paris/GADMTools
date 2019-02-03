\name{vignette}
\alias{vignette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a vignette
}
\description{
Vignette will superimpose a region map over a larger (lower level) map.
}
\usage{
vignette(main, region, maincolor = "black",
        regioncolor = "white", mainfill = "grey", 
        regionfill = "black", 
        mainsize = 1, regionsize = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{main}{ \bold{Object} GADMWrapper }
  \item{region}{ \bold{Object} GADMWrapper }
  \item{maincolor}{ a valid color }
  \item{regioncolor}{ a valid color }
  \item{mainfill}{ a valid color }
  \item{regionfill}{ a valid color }
  \item{mainsize}{ \bold{Numeric} border size }
  \item{regionsize}{ \bold{Numeric} border size }
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
# library(ggplot2)
# FR <- gadm.loadCountries("FRA", level=1, basefile="./")
# AU <- subset(FR, regions="Auvergne", level=1)
# vignette(FR, AU)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
