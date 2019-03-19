\name{gadm_showScale}
\alias{gadm_showScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{display a scale on a plot}
\description{
display a scale for measuring distances on a plot (ggplot2)
}
\usage{
gadm_showScale(plot, where="bl")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{\bold{ggplot2}}
  \item{where}{\bold{character} location of the scale. Can be:
    \itemize{
       \item{"tl"}{ - top left}
       \item{"tr"}{ - top right}
       \item{"bl"}{ - bottom left (default)}
       \item{"br"}{ - bottom right}
    }
  }
}

\value{ \bold{Object} ggplot2 }

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}


\examples{
library(GADMTools)
data("Corsica")

plotmap(Corsica) \%>\% gadm_showScale()
}
\keyword{ ~documentation }

