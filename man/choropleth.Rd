\name{choropleth}
\alias{choropleth}

\title{ Draw a choropleth on selected regions }

\description{
Drawing a choropleth with GADMTools is easy and straightforward. You just have to select your shape(s) file(s) with \emph{gadm.loadcountries}, load your data from a
csv file for example, and call the choropleth function with the rights arguments.
}
\usage{
choropleth (x, data, value=NULL, breaks = NULL, steps = 5, adm.join=NULL,
            data.join=NULL, legend = NULL, labels = NULL, palette=NULL,
            title="")

}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{data}{
\bold{data.frame} - data to plot
}
  \item{value}{
  \bold{String} - the name of the column in the data.frame we want to plot (eg: an incidence in epidemiology studies)
}
  \item{breaks}{
%%     ~~Describe \code{regioncolor} here~~
}
  \item{steps}{ \bold{Integer} - number of breaks. Default = 5. If \emph{breaks} is \bold{NOT NULL} this value is used internally with cut().
}
  \item{adm.join}{ \bold{String} - the name in GADM spdf dataset wich will be
  joined with a column of the data..
}
  \item{data.join}{ \bold{String} - the name of the column in the data.frame
  wich will be joined with the adm.join.
}
  \item{legend}{
  \bold{String} - title of legend. Default \bold{NULL}. 
}
  \item{labels}{ \bold{String vector} labels for the legend. Default \bold{NULL}}
  
  \item{palette}{
  \bold{String} - A RColorBrewer palette name or a \bold{String vector} a
  vector of colors. Default \bold{NULL}.
}
  \item{title}{
  \bold{String} - Title of the plot. Default is an empty string
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
\link{classIntervals}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
