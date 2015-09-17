source("../GADMTools/R/gadm.R")

MAP <- gadm.loadCountries("SYR", level = 1)
SYR = read.table("SYR_places.csv", sep=";", header=TRUE);

isopleth(MAP, SYR, title="Isopleth avec palette par défaut (Heatmap)")
isopleth(MAP, SYR, palette="Purples", title="Isopleth avec la palette Purples")