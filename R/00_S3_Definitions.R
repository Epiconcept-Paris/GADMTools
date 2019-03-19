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


gadm_crop <- function(x, xmin, ymin, xmax, ymax) UseMethod("gadm_crop", x)

gadm_getBackground <- gadm.getBackground <- function(x, 
                               name, 
                               type="osm", 
                               clip=TRUE)  UseMethod("gadm_getBackground", x)

gadm_getBbox <- function(x) UseMethod("gadm_getBbox", x) 

gadm_longTo360 <- gadm.longTo360 <- function(x) UseMethod("gadm_longTo360", x)

gadm_removeBackground <- gadm.removeBackground <- function(x) UseMethod("gadm_removeBackground", x)

gadm_remove <- gadm.remove <- function(x, level=NULL, regions=NULL) UseMethod("gadm_remove", x)

gadm_union <- gadm.union <- function(x, level=0, type="?") UseMethod("gadm_union", x)

gadm_subset <- gadm.subset <- function(x, 
                        level=NULL, 
                        regions=NULL, 
                        usevar=NULL) UseMethod("gadm_subset", x)

  
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

gadm_plot <- plotmap <- function(x, title="") UseMethod("gadm_plot", x)

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

saveas <- saveAs <- function(x, name=NULL, directory=NULL) UseMethod("saveAs", x)
