\name{classDots}
\alias{classDots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot dots on a map with values between different fixed classes.}
\description{
Plot values as discretized scale circles on a map.
}
\usage{
classDots(x, data, color="red", value = NULL, breaks = NULL,
                      steps = 5, labels = NULL, opacity = 0.5, title="",
                      note=NULL, legend = NULL)
}

\arguments{
  \item{x}{
  \bold{Object} GADMWrapper
  }
  \item{data}{
  \bold{Object} data.frame with columns 'latitude' and 'longitude'
}
  \item{color}{a valid color}
  \item{value}{
  \bold{Character} Name of a column of the data.frame.}
  
  \item{breaks}{\bold{vector} of breaks}
  \item{steps}{ unused}
  \item{labels}{\bold{vector} of labels}
  \item{opacity}{\bold{float} Background opacity of the filled circles} 
  \item{title}{\bold{Character} The title of the plot}
  \item{note}{\bold{Character} Add an annotation}
  \item{legend}{\bold{Character} The title of the legend}
}
\details{
----
}
\value{
\bold{Object} ggplot2
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
# todo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
