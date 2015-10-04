library(GADMTools)


MAP <- gadm.loadCountries("WORLD", level = 0, basefile = "./")
DAT = read.csv("WData_01.csv")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2006",
           breaks = "jenks",
           palette="Greens",
           legend = "2006",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2007",
           breaks = "jenks",
           palette="Greens",
           legend = "2007",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2008",
           breaks = "jenks",
           palette="Greens",
           legend = "2008",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2009",
           breaks = "jenks",
           palette="Greens",
           legend = "2009",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2010",
           breaks = "jenks",
           palette="Greens",
           legend = "2010",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2011",
           breaks = "jenks",
           palette="Greens",
           legend = "2011",
           title="Arable land (hectares per person")

choropleth(MAP, DAT,
           join.name = "Country.Code",
           value = "X2012",
           breaks = "jenks",
           palette="Greens",
           legend = "2012",
           title="Arable land (hectares per person")
