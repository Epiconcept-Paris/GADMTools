# =================================================================================================================
# S3 Methods ------------------------------------------------------------------------------------------------------
# =================================================================================================================

choropleth <- function(x, data,
                       value=NULL,
                       breaks = NULL,
                       steps = 5,
                       adm.join=NULL,
                       legend = NULL,
                       labels = NULL,
                       palette=NULL,
                       title="",
                       subtitle = NULL,
                       caption  = NULL) UseMethod("choropleth", x)

dots <- function(x, points, 
                 color="red",
                 size=8,
                 value = NULL,
                 breaks = NULL,
                 steps = 5,
                 palette = NULL,
                 labels = NULL,
                 strate = NULL ,
                 title    = "",
                 subtitle = "",
                 caption  = "",
                 legend = NULL,
                 note=NULL) UseMethod("dots", x)

gadm.getBackground <- function(x, 
                               name, 
                               type="osm", 
                               clip=TRUE)  UseMethod("gadm.getBackground", x)

gadm.removeBackground <- function(x) UseMethod("gadm.removeBackground", x)
  
gadm.union <- function(x, gid = "XYZ", name = "UNKNOWN") UseMethod("gadm.union", x)

gadm.subset <- function(x, 
                        level=NULL, 
                        regions=NULL, 
                        usevar=NULL) UseMethod("gadm.subset", x)
  
classDots <- function(x, 
                      data,
                      color="red",
                      value = NULL,
                      breaks = NULL,
                      steps = 5,
                      labels = NULL,
                      opacity = 0.5,
                      title="",
                      note=NULL,
                      legend = NULL) UseMethod("classDots", x)

plotmap <- gadm.plot <- function(x, title="") UseMethod("plotmap", x)

propDots <- function(x, 
                     data, 
                     value, 
                     breaks=NULL, 
                     range=NULL, 
                     labels=NULL, 
                     color="red", 
                     title="", 
                     subtitle = "",
                     caption  = "",
                     note=NULL) UseMethod("propDots", x)

remove <- gadm.remove <- function(x, level=NULL, regions=NULL) UseMethod("gadm.remove", x)

saveas <- saveAs <- function(x, name=NULL, directory=NULL) UseMethod("saveAs", x)
