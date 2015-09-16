\name{gadm.loadCountries}
\alias{gadm.loadCountries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gadm.loadCountries}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gadm.loadCountries(fileNames, level = 0, basefile = "./", baseurl = "http://biogeo.ucdavis.edu/data/gadm2/R/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileNames}{
%%     ~~Describe \code{fileNames} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{basefile}{
%%     ~~Describe \code{basefile} here~~
}
  \item{baseurl}{
%%     ~~Describe \code{baseurl} here~~
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
function (fileNames, level = 0, basefile = "./", baseurl = "http://biogeo.ucdavis.edu/data/gadm2/R/") 
{
    loadChangePrefix <- function(fileName, level = 0) {
        FILENAME = sprintf("\%s_adm\%d.RData", fileName, level)
        LOCAL_FILE = sprintf("\%s\%s", basefile, FILENAME)
        if (file.exists(LOCAL_FILE)) {
            load(LOCAL_FILE)
            theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), 
                sep = "_"))
            theFile
        }
        else {
            gadm <- NULL
            REMOTE_FILE = sprintf("\%s\%s", baseurl, FILENAME)
            load(url(REMOTE_FILE))
            if (!is.null(gadm)) {
                save(gadm, file = LOCAL_FILE)
                theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), 
                  sep = "_"))
                theFile
            }
        }
    }
    polygon <- sapply(fileNames, loadChangePrefix, level)
    polyMap <- do.call("rbind", polygon)
    structure(list(basename = basefile, spdf = polyMap, level = level), 
        class = "GADMWrapper")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
