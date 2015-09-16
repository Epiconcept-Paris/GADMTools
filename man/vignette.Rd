\name{vignette}
\alias{vignette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
vignette}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vignette(main, region, maincolor = "black", regioncolor = "white", mainfill = "grey", regionfill = "black", mainsize = 1, regionsize = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{maincolor}{
%%     ~~Describe \code{maincolor} here~~
}
  \item{regioncolor}{
%%     ~~Describe \code{regioncolor} here~~
}
  \item{mainfill}{
%%     ~~Describe \code{mainfill} here~~
}
  \item{regionfill}{
%%     ~~Describe \code{regionfill} here~~
}
  \item{mainsize}{
%%     ~~Describe \code{mainsize} here~~
}
  \item{regionsize}{
%%     ~~Describe \code{regionsize} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (main, region, maincolor = "black", regioncolor = "white", 
    mainfill = "grey", regionfill = "black", mainsize = 1, regionsize = 0.5) 
{
    Map <- gadm.initMap("")
    Map <- addMap(Map, main, color = maincolor, fill = mainfill, 
        size = mainsize)
    Map <- addMap(Map, region, color = regioncolor, fill = regionfill, 
        size = regionsize)
    Map$P <- Map$P + theme_nothing(legend = TRUE)
    render(Map)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
