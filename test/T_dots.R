library(GADMTools)

MAP <- gadm.loadCountries("SYR", level = 1)
P = read.table("SYR_places.csv", sep=";", header=TRUE);

dots(MAP, points=P)
dots(MAP, points=P, color="blue")
dots(MAP, points=P, color="green", title="Cas de Lechmaniose en Syrie")
dots(MAP, points=P, title="Cas de Lechmaniose en Syrie",
     subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015")
MAP <- gadm.loadCountries("SYR", level = 0)
dots(MAP, points=P, title="Cas de Lechmaniose en Syrie",
     subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015")


