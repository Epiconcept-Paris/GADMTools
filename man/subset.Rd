\name{subset}
\alias{subset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
subset}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subset(this, level = 1, regions = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{this}{
%%     ~~Describe \code{this} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{regions}{
%%     ~~Describe \code{regions} here~~
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
function (this, level = 1, regions = NA) 
{
    NAME <- sprintf("NAME_\%d", level)
    df <- this$spdf
    x <- as.data.frame(df[, NAME])
    colnames(x) <- c("N")
    df <- df[x$N \%in\% regions, ]
    structure(list(basename = this$basename, spdf = df, level = this$level), 
        class = "GADMWrapper")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
