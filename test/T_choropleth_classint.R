library(GADMTools)


MAP <- gadm.loadCountries("FRA", level = 1, simplify=0.01)
DAT = read.csv2("FRA_REGIONS.csv")

#str(DAT)
#listNames(MAP, level = 1)

# boxplot(DAT$Population.2011)
# hist(DAT$Population.2011)

#XB <- classIntervals(DAT$Population.2011, 7, style="quantile")
#XB <- classIntervals(DAT$Population.2011, 5, style="sd")
#XB <- classIntervals(DAT$Population.2011, 5, style="fisher")
#XB <- classIntervals(DAT$Population.2011, 5, style="kmeans")
#XB <- classIntervals(DAT$Population.2011, 5, style="jenks")
#XB <- classIntervals(DAT$Population.2011, 7, style="pretty")
#XB <- classIntervals(DAT$Population.2011, 7, style="equal")
#XB <- classIntervals(DAT$Population.2011, 5, style="hclust")
XB <- classIntervals(DAT$Population.2011, 5, style="bclust")



#Region = MAP$spdf@data$NAME_1
choropleth(MAP, DAT,
          join.id = "Code.INSEE",
          value = "Population.2011",
          breaks = XB$brks,
          palette="Blues",
          legend = "Population par régions",
          title="Choropleth avec palette par défaut (Heatmap - 5 steps)")

# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 9,
#            legend = "Population par régions",
#            title="Choropleth avec palette par défaut (Heatmap - 9 steps)")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 9,
#            palette = "Blues",
#            legend = "Population par régions",
#            title="Choropleth avec palette nommée (Blues) - 9 steps)")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 8,
#            palette = "Greys",
#            legend = "Population par régions",
#            title="Choropleth avec palette printable (Greys) - 8 steps)")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 8,
#            palette = c("red","yellow","green"),
#            legend = "Population par régions",
#            title="Choropleth avec palette custom")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 8,
#            palette = c("#ff9999","#ffff99","#99ff99"),
#            legend = "Population par régions",
#            title="Choropleth avec palette custom (hexadeimal)")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 8,
#            palette = rev(c("#ff9999","#ffff99","#99ff99")),
#            legend = "Population par régions",
#            title="Choropleth avec palette custom inversée")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            steps = 8,
#            palette = rev(c("#ff33ff","#ff88ff","#ffccff")),
#            labels  = rev(c("Région invivable","Agitée","Tranquille")),
#            title="Choropleth avec labels custom")
# 
# #breaks=c(0,40000,800000,2000000,3000000,5000000,10000000,20000000)
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            breaks = c(0,400000,800000,2000000,3000000,5000000,10000000,Inf),
#            palette = "Greens",
#            title="Choropleth avec breaks custom (7 steps)")
# 
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            breaks = c(0,400000,800000,2000000,3000000,5000000,10000000,Inf),
#            labels = c("≾ 400 000","400 000 < P ≾ 800 000", "800 000 < P ≾ 2 000 000",
#                       "2 000 000 < P ≾ 3 000 000", "3 000 000 < P ≾ 5 000 000",
#                       "5 000 000 < P ≾ 10 000 000", "≿ 10 000 000"),
#            title="Choropleth avec breaks et labels custom")
# 
# regions = c("Auvergne","Rhône-Alpes","Provence-Alpes-Côte d'Azur",
#             "Languedoc-Roussillon","Bretagne")
# SUD_EST <- subset(MAP, level = 1, regions = regions)
# DAT2 <- DAT[DAT$Nom %in% regions,]
# 
# choropleth(SUD_EST, DAT2,
#            join.id = "Code.INSEE",
#            value = "Population.2011",
#            breaks = c(0,400000,800000,2000000,3000000,5000000,10000000,Inf),
#            labels = c("≾ 400 000","400 000 < P ≾ 800 000", "800 000 < P ≾ 2 000 000",
#                       "2 000 000 < P ≾ 3 000 000", "3 000 000 < P ≾ 5 000 000",
#                       "5 000 000 < P ≾ 10 000 000", "≿ 10 000 000"),
#            title="Choropleth d'un subset avec breaks consistants")
# 
# DAT$Categorie <- as.factor(rep(c("A","B","C","D"), each=6, length.out=22))
# choropleth(MAP, DAT,
#            join.id = "Code.INSEE",
#            value = "Categorie",
#            palette = "Purples",
#            title="Choropleth sur un factor")
# 
# 
