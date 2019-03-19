\name{saveAs}
\alias{saveAs}
\alias{saveas}

\title{ Save your own GADM shapefile as an rds file}
\description{
Save a GADM shapefile (.rds)
}
\usage{
saveAs(x, name = NULL, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} - GADMWWrapper
}
  \item{name}{\bold{String} - filename}
  \item{directory}{\bold{String} - path to an alternative directory}
}
\details{
If directory is NULL (default), the file is stored in the same directory as specified in basefile parameter of gadm_loadCountries or gt2.loadCountries
}

\value{
----

}
\references{
----
}
\author{
Jean Pierre Decorps <jp.decorps@epiconcept.fr>
}
\note{
Do not specify the rds extension, it is added automatically.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
----
}
\examples{
# library(GADMTools)
# library(sp)
# France = gadm_loadCountries("FRA", level=1, basefile="./")
# Auvergne = subset(France,regions = "Auvergne", level=1)
# saveas(Auvergne, "./AUVERGNE")
# AUV <- gadm_loadCountries("AUVERGNE", level=1, basefile="./")
# plotmap(AUV)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
