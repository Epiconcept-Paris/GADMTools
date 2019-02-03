\name{grid.map}
\alias{grid.map}

\title{ Arrange maps on a grid }

\description{
Allows you to arrange multiple maps into one image. This is useful for showing a country together with its territories in other parts of the world (ex: showing France and Reunion island) or placing two or more countries side by side.
}
\usage{
grid.map(left, right, center=NULL, title=NULL)
}
\arguments{
  \item{left}{ \bold{Object} GADMWrapper }
  
  \item{right}{
\bold{data.frame} - data to plot
}
  
  \item{center}{
  \bold{String} - an RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}.
}
  \item{title}{
  \bold{String} - plot title. Default is an empty string.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
