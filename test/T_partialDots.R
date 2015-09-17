source("../GADMTools/R/gadm.R")

MAP <- gadm.loadCountries("SYR", level = 1)

SYR = read.table("SYR_places.csv", sep=";", header=TRUE);

#choropleth(MAP, SYR, title="Isopleth avec palette par défaut (Heatmap)")

str(SYR)
coordinates(SYR)  <- c("longitude", "latitude")
MAP <- subset(MAP, 1, c("Aleppo"))
M1 <- as(MAP$spdf, "SpatialPolygons")

plot(MAP$spdf)
plot(SYR)

SYR@proj4string@projargs <- MAP$spdf@proj4string@projargs

#plot(SYR$spdf)

SYR$index <-over(SYR, M1)
plot(SYR$)

