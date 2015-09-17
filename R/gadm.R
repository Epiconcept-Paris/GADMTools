# library(rgdal)
# library(maptools)
library(ggplot2);
# library(plyr)
# library(maptools)
# library(rgeos)
# library(sp);
# library(ggmap);
# library(dplyr);
# library(scales)

#/home/kdo/GEODATA/GADM/SYR_adm0.RData

GADM_BASE = "/home/kdo/GEODATA/GADM/";
GADM_URL  = "http://biogeo.ucdavis.edu/data/gadm2/R/"

# gadm.init <- function(basename="./") {
#   if (!is.character(basename)) stop("basename must be character string")
#   structure(list(basename), class = "GADMWrapper")
# }
# 
# gadm.getBaseName <- function(obj) UseMethod("gadm.getBaseName", obj)
# #gadm.getCountries <- function (fileNames, level = 0, ...) UseMethod("gadm.getCountries", obj)
#   
# gadm.getBaseName.GADMWrapper <- function(obj) {
#   obj[[1]];
# }

# GADM <- gadm.init()
# class(GADM)
# gadm.getBaseName(GADM);

## ---------------------------------------------------------------------------
## Function : gadm.getCountries (constructor)
## Description : load a file from local system or from GADM repository 
##               You just have to specify the countries (ISO3 CODE) of the
##               file name, like "ARG" for Argentina.
##               Optionally you can specify which level you want to have
## Return : This function creates a GADMWrapper that contains a 
##          SpatialPolygonsDataFrame object that contains all maps you 
##          specify in "fileNames".
## ---------------------------------------------------------------------------
gadm.loadCountries <- function (fileNames, level = 0, basefile="./", baseurl="http://biogeo.ucdavis.edu/data/gadm2/R/") {
  require(ggplot2)
  require(rgdal)
  require(maptools)
  require(sp)
  require(dplyr)
  require(RColorBrewer)
  ## load file and change prefix
  loadChangePrefix <- function (fileName, level = 0) {
    FILENAME = sprintf("%s_adm%d.RData", fileName,level)
    LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
    if (file.exists(LOCAL_FILE)) {
      load(LOCAL_FILE)
      theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
      theFile
    } else {
      gadm <- NULL
      REMOTE_FILE = sprintf("%s%s", baseurl, FILENAME)
      load(url(REMOTE_FILE))
      if (!is.null(gadm)) {
        save(gadm, file=LOCAL_FILE)
        theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
        theFile
      }
    }
  }
  polygon <- sapply(fileNames, loadChangePrefix, level)
  polyMap <- do.call("rbind", polygon)
  structure(list("basename"=basefile,
                 "spdf"=polyMap,
                 "level"=level),
            class = "GADMWrapper")
}

## ---------------------------------------------------------------------------
## Method : subset
## Return : a new object GADMWrapper with the selected regions
## ---------------------------------------------------------------------------
subset <- function(this, level=0, regions=NA) UseMethod("subset", this)
subset <- function(this, level=1, regions=NA) {
  NAME <- sprintf("NAME_%d", level)
  df <- this$spdf
  x <- as.data.frame(df[, NAME])
  colnames(x) <- c("N")
  df <- df[x$N %in% regions,];
  structure(list("basename"=this$basename,
                  "spdf"=df,
                 "level"=this$level),
            class = "GADMWrapper")  
}

## ---------------------------------------------------------------------------
## Method : listNames
## Return : a list of names for the selected level
## ---------------------------------------------------------------------------
listNames <- function(this, level=0) UseMethod("listNames", this)
listNames.GADMWrapper <- function(this, level=0) {
  if (level > this$level) {
    cat(sprintf("Warning: max level=%d\n", this$level))
    level = this$level
  }
  if (this$level == 0) {
    name <-"NAME_ISO"
  } else {
    name <- sprintf("NAME_%d", level)
  }
  unique(this$spdf@data[, name])
}

## ---------------------------------------------------------------------------
## Method : saveas
## Return : the name of the saved file
## ---------------------------------------------------------------------------
saveas <- function(this, name=NULL) UseMethod("saveas", this)
saveas.GADMWrapper <- function(this, name=NULL) {
  if (is.null(name)) {
    stop("You have to provide a name.")
  }
  FName <- sprintf("%s_adm%d.RData", name, this$level)
  gadm = this$spdf
  save(gadm, file=FName);
  FName
}

### MAPPING
gadm.initMap <- function(title="MAP") {
  Plot <- ggplot()
  structure(list("P" = Plot,
                 "title"=title),
            class = "GADMmap")
}

addMap <- function(this, layer, ...) UseMethod("addMap", this)
addMap.GADMmap <- function(this, layer, level=0, color="black", fill="black", size=1.0) {
  if (layer$level == 0) {
    name <-"NAME_ISO"
  } else {
    name <- sprintf("NAME_%d", level)
  }
  D <- fortify(layer$spdf, region=name)
  this$P <- this$P + geom_polygon(data = D, aes(x = long, y = lat, group = group),
               fill=fill, color=color, size=size)
  this
}

render <- function(this) UseMethod("render", this)
render.GADMmap <- function(this) {
  this$P <- this$P + labs(title = this$title, fill = "")
  this$P <- this$P + coord_map()
  print(this$P)
}

vignette <- function(main, region,
                     maincolor="black",
                     regioncolor="white",
                     mainfill="grey",
                     regionfill="black",
                     mainsize=1.0,
                     regionsize=0.5)
{
  Map <- gadm.initMap("") 
  Map <- addMap(Map, main,   color=maincolor,   fill=mainfill,   size=mainsize )
  Map <- addMap(Map, region, color=regioncolor, fill=regionfill, size=regionsize)
  Map$P <- Map$P + theme_nothing(legend = TRUE)
  render(Map)
}

dots <- function(this, points, ...) UseMethod("dots", this)
dots.GADMWrapper <- function(this, points, color="red", title="", subtitle="") {
  if (this$level == 0) {
    .name <-"NAME_ISO"
  } else {
    .name <- sprintf("NAME_%d", this$level)
  }
  
  .data <- fortify(this$spdf, region=.name);
  .titles <- sprintf("%s\n%s", title, subtitle)
  .pcolor <- color
  
  ggplot() +
    geom_polygon(data=.data, aes(x=long, y=lat,  group=group),
                 fill=NA, color="black", size = 0.5)+
      geom_point(data=P, aes(x=longitude, y=latitude, size=5), color=.pcolor, shape=16) +
      labs(title = .titles, fill = "") + theme(legend.position="none")+
      coord_map();
}  

propDots <- function(this, data, ...) UseMethod("propDots", this)
propDots.GADMWrapper <- function(this, data, value, breaks=NULL, range=NULL, labels=NULL, color="red", title="", subtitle="") {
  if (this$level == 0) {
    .name <-"NAME_ISO"
  } else {
    .name <- sprintf("NAME_%d", this$level)
  }
  
  .map <- fortify(this$spdf, region=.name)
  .data <- data
  .value <- value
  .titles <- sprintf("%s\n%s", title, subtitle)
  .pcolor <- color

  getBreaks <- function(value) {
    .min = min(data[,value], na.rm = T)
    .max = max(data[,value], na.rm = T)
    .r = .max - .min
    .B <- round(c(.r * 0.2, .r * 0.4, .r * 0.6, .r * 0.8, .r * 1.0), 0)
    list(.B, .min, .max)
  }

  .inter <- getBreaks(value)
  .breaks <- breaks
  
  if (is.null(breaks)) {
    .breaks <- .inter[[1]]
  }
  
  .range <- range
  if (is.null(range)) {
    .range <- c(.inter[[2]], .inter[[3]])
  }
  
  .labels = labels
  if (is.null(labels)) {
    .labels <- .breaks
  }
  
  ggplot() +
  geom_polygon(data=.map, aes(x=long, y=lat,  group=group),
                 fill=NA, color="black", size = 0.5) +
  
  geom_point(data=.data,
                    aes_string(x="longitude", y="latitude", 
                    size=eval(value)), 
                    color=.pcolor, shape=16, alpha=0.3) +

  scale_size_area(max_size = 24, breaks=.breaks, limits = .range, labels=.labels) +
  labs(title = .titles, fill = "") + 
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) +
  coord_map();
}  

isopleth <- function(this, ...) UseMethod("isopleth", this)
isopleth.GADMWrapper <- function(this, data, palette=NULL, title="", subtitle="") {
  if (this$level == 0) {
    .name <-"NAME_ISO"
  } else {
    .name <- sprintf("NAME_%d", this$level)
  }
  
  .map <- fortify(this$spdf, region=.name)
  .data <- data
  .titles <- sprintf("%s\n%s", title, subtitle)
  .palette <- palette

  if (is.null(palette)) {
    .palette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  }
  else {
    .palette <- colorRampPalette(brewer.pal(9, palette), space="Lab")
  }
  
  ggplot() +
    geom_polygon(data=.map, aes(x=long, y=lat,  group=group),
                 fill=NA, color="black", size = 0.5) +
    stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), 
                   alpha=0.5, size = 10, bins = 10, data = .data, geom = "polygon")+
    scale_fill_gradientn(colours = .palette(100)) +
    labs(title = .titles, fill = "") + 
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_map();
  
} 

choropleth <- function(this, ...) UseMethod("choropleth", this)
choropleth.GADMWrapper <- function(this,
                                   data,
                                   value=NULL,
                                   breaks = NULL,
                                   steps = 5,
                                   join.id=NULL,
                                   join.name=NULL,
                                   legend = NULL,
                                   labels = NULL,
                                   palette=NULL,
                                   title="") {
  
  if (is.null(value)) stop("Unknown value (NULL)\n")
  
  if (this$level == 0) {
    .name <-"NAME_ISO"
  } else {
    .name <- sprintf("NAME_%d", this$level)
    .id   <- sprintf("ID_%d", this$level)
  }

  .data <- data
  .value <- value
  .range <- range
  .legend <- legend
  .palette <- palette
  .steps <- steps
  .labels <- labels
  
  if (!is.null(join.name)) {
    names(.data)[names(.data)==join.name] <- .name
    .map <- fortify(this$spdf, region=.name)
    names(.map)[names(.map)=="id"] <- .name
  }
  else if (!is.null(join.id)){
    names(.data)[names(.data)==join.id] <- .id
    .map <- fortify(this$spdf, region=.id)
    names(.map)[names(.map)=="id"] <- .id
    .map[,.id] <- as.integer(.map[,.id])
  }
  
  
  P <- left_join(.map, .data)
  if (!is.factor(P[,value])) {
    P[,value] <- as.numeric(P[,value])
  }

  names(P)[names(P)==value] <- "CHPLT_VALUE"

  # -------------------------------------------------------
  # Palettes
  # -------------------------------------------------------
  if (is.null(palette)) {
    .palette <- rev(brewer.pal(9, "Spectral"))
  }
  else {
    if (length(palette)==1) {
      .palette <- brewer.pal(9, palette)
    }
    else {
      .steps <- length(palette)
    }
  }
  
  if (is.null(breaks)) {
    if (!is.factor(P$CHPLT_VALUE)) {
      P$CHPLT_VALUE <- cut(P$CHPLT_VALUE,.steps)
    }
  }
  else {
    P$CHPLT_VALUE <- cut(P$CHPLT_VALUE, breaks=breaks, labels = .labels)
  }
  
  if (is.null(labels)) {
    .labels <- levels(P$CHPLT_VALUE)
  }

  if (is.null(legend)) .legend <- value
  
  
#  if (is.null(range)) .range <- c(min(P$CHPLT_VALUE, na.rm=T), max(P$CHPLT_VALUE, na.rm=T))
  
  ggplot(P, aes(x=long, y=lat, group=group)) +
    geom_polygon(data=P, 
                  aes(x=long, y=lat, group=group, fill=CHPLT_VALUE),
                  color = "black", size = 0.25) +
 
     scale_fill_manual(.legend, values = .palette, 
                       limits=levels(P$CHPLT_VALUE),
                       labels=.labels,
                       guide = guide_legend(reverse = TRUE)) +

    labs(title = title, fill = "") + 
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_map();
}


