library(GADMTools)

MAP <- gadm.loadCountries("SYR", level = 1)
SYR = read.table("SYR_places.csv", sep=";", header=TRUE);

#breaks <- c(10,20,50,100,200,400,600)
breaks <- c(20,40,60,80,100)
# SYR$population <- runif(n = 81, 50, 600)
propDots(MAP, data=SYR, value="population")
# 
# SYR$population <- runif(n = 81, 10, 300)
# propDots(MAP, data=SYR, color="#00aa00", value="population",
#          title="Cas de Lechmaniose en Syrie",
#          subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015",
#          breaks = breaks)
# 
# SYR$population <- runif(n = 81, 200, 599)
# propDots(MAP, data=SYR, color="black", value="population",
#          title="Cas de Lechmaniose en Syrie",
#          subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015",
#          breaks = breaks)
# 
# SYR$population <- runif(n = 81, 20, 350)
# propDots(MAP, data=SYR, color="black", value="population",
#          title="Cas de Lechmaniose en Syrie",
#          subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015",
#          breaks = breaks)

# SYR$population <- round(runif(n = 81, 0, 100),0)
# 
# propDots(MAP, data=SYR, color="blue", value="population",
#          title="Cas de Lechmaniose en Syrie",
#          subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015",
#          breaks = breaks,
#          range = c(1, 100))
# 
.labels <- c("20%","40%","60%","80%","100%")
SYR$population <- round(runif(n = 81, 5, 1200),0)

propDots(MAP, data=SYR, color="#9900ff", value="population",
         title="Cas de Lechmaniose en Syrie",
         subtitle = "Enquête du 10 Janvier 2015 au 10 Juillet 2015",
         breaks = breaks,
         range = c(1, 100),
         labels = .labels)
