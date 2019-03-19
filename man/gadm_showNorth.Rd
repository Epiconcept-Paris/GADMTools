\name{gadm_showNorth}
\alias{gadm_showNorth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{display a north arrow on a plot}
\description{
display a north arrow on a plot (ggplot2)
}
\usage{
gadm_showNorth(plot, where="br")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{\bold{ggplot2}}
  \item{where}{\bold{character} location of the arrow. Can be:
    \itemize{
       \item{"tl"}{ - top left}
       \item{"tr"}{ - top right}
       \item{"bl"}{ - bottom left}
       \item{"br"}{ - bottom right (default)}
    }
  }
}


\value{ \bold{Object} ggplot2 }

\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}


\examples{
library(GADMTools)
data("Corsica")

plotmap(Corsica) \%>\% gadm_showNorth()
}
\keyword{ ~documentation }

