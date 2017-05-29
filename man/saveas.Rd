\name{saveas}
\alias{saveas}

\title{ Save your own GADM shapefile as an rds  file}
\description{
Save a GADM shapefile (.rds)
}
\usage{
saveas(x, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\bold{Object} - GADMWWrapper
}
  \item{name}{
\bold{String} - File path
}
}
\details{
----
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
# France = gadm.loadCountries("FRA", level=1, basefile="./")
# Auvergne = subset(France,regions = "Auvergne", level=1)
# saveas(Auvergne, "./AUVERGNE")
# AUV <- gadm.loadCountries("AUVERGNE", level=1, basefile="./")
# plotmap(AUV)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
