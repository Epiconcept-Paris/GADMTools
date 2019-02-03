\name{choropleth}
\alias{choropleth}

\title{ Draw a choropleth on selected regions }

\description{
Drawing a choropleth (colored regions based on data values) with GADMTools is straightforward. You just have to select your shape(s) file(s) with \emph{gadm.loadcountries}, load your data from a
csv file for example, and call the choropleth function with the right arguments.
}
\usage{
choropleth (x, data, value=NULL, breaks = NULL, steps = 5, adm.join=NULL,
            legend = NULL, labels = NULL, palette=NULL,
            title="", subtitle = NULL, caption = NULL)

}
\arguments{
  \item{x}{ \bold{Object} GADMWrapper }
  
  \item{data}{
\bold{data.frame} - data to plot
}
  \item{value}{
  \bold{String} - the name of the column in the data.frame we want to plot (eg: an incidence in epidemiology studies)
}
  \item{breaks}{\bold{Vector} of breaks values or a \bold{Sring} name of a function from \emph{classIntervals} (one of "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks")
}
  \item{steps}{ \bold{Integer} - number of breaks. Default = 5. If \emph{breaks} is \bold{NOT NULL} this value is used internally with cut().
}
  \item{adm.join}{ \bold{String} - the name in GADM spdf dataset which will be
  joined with a column of the data.
}
  \item{legend}{
  \bold{String} - legend title. Default \bold{NULL}. 
}
  \item{labels}{ \bold{String vector} labels for the legend. Default \bold{NULL}}
  
  \item{palette}{
  \bold{String} - An RColorBrewer palette name or a \bold{String vector} 
  vector of colors. Default \bold{NULL}.
}
  \item{title}{ \bold{String} - Title of the plot. Default is an empty string.}
  \item{subtitle}{ \bold{String} - subtitle of the plot. Default is NULL.}
  \item{caption}{ \bold{String} - caaption of the plot. Default is NULL.}
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
# library(GADMTools)
# library(sp)
# library(dplyr)

# MAP <- gadm.loadCountries("BEL", level = 3, simplify=0.01)
# DAT = read.csv2("BE_clamydia_incidence.csv")

# Here is the main trick !
# -----------------------------------------------------
# DAT <- rename(DAT, NAME_3 = district)

# choropleth(MAP, DAT,
#            adm.join = "NAME_3",
#            value = "rate03",
#            breaks = "sd",
#            palette="Oranges",
#            legend = "Incidence",
#            title="Chlamydia incidence by Belgian district for 2003)")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
\keyword{ ~hplot }% __ONLY ONE__ keyword per line
