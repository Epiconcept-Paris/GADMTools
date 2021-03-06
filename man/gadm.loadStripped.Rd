\name{gadm_loadStripped}
\alias{gadm_loadStripped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load one GADM stripped shapefile}
\description{
Load one GADM stripped shapefiles from a local path for use with ggplot2.
}
\usage{
gadm_loadStripped(name, level, basefile='./')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
\bold{Character vector} of a named region. An ISO-3166-1 code or a custom name. You don't have to specify the suffix (admX) nor the file extension (.rds).
}
  \item{level}{
\bold{Integer} the level of the administrative boundaries (0 is the country, higher values equal finer divisions) 
}
  \item{basefile}{
\bold{Character vector} the path of the directory where shapefiles are stored. Default is "./"
}

}

\section{ISO-3166-1 }{

See : \url{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}

\code{
 "ABW","AFG","AGO","AIA","ALA","ALB","AND","ANT","ARE","ARG", 
 "ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL", 
 "BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ", 
 "BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN", 
 "CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL", 
 "COM","CPV","CRI","CUB","CXR","CYM","CYP","CZE","DEU","DJI", 
 "DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST", 
 "ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO", 
 "GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD", 
 "GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI", 
 "HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR", 
 "ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR", 
 "KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA", 
 "LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG", 
 "MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP", 
 "MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","NAM","NCL", 
 "NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL", 
 "OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI", 
 "PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA", 
 "SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV", 
 "SMR","SOM","SPM","SRB","STP","SUR","SVK","SVN","SWE","SWZ", 
 "SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS", 
 "TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI", 
 "URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT",  
 "WLF","WSM","YEM","ZAF","ZMB","ZWE"}

}

\value{ \bold{Object} gadm_sp with stripped properties == TRUE}
\references{
----}
\author{Jean Pierre Decorps <jp.decorps@epiconcept.fr>}
\note{
----
}

\seealso{
----
}
\examples{
# library(GADMTools)
# library(sp)
# BE <- gadm_loadStripped('BEL', level=2)
# plotmap(BE)
}
\keyword{ ~documentation }
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
