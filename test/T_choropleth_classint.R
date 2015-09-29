library(GADMTools)


MAP <- gadm.loadCountries("FRA", level = 1, simplify=0.01)
DAT = read.csv2("FRA_REGIONS.csv")

#str(DAT)
#listNames(MAP, level = 1)

# boxplot(DAT$Population.2011)
# hist(DAT$Population.2011)



choropleth(MAP, DAT,
          join.id = "Code.INSEE",
          value = "Population.2011",
          breaks = "sd",
          palette="Blues",
          legend = "Population par régions",
          title="Choropleth méthode 'sd' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "equal",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'equal' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "pretty",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'pretty' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "quantile",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'quantile' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "kmeans",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'kmeans' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "hclust",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'hclust' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "bclust",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'bclust' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "fisher",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'fisher' - 5 steps)")

choropleth(MAP, DAT,
           join.id = "Code.INSEE",
           value = "Population.2011",
           breaks = "jenks",
           palette="Blues",
           legend = "Population par régions",
           title="Choropleth méthode 'jenks' - 5 steps)")

