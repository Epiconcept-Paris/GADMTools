# library(ggplot2);
# library(dplyr);
# require(sp)
loadNamespace("sp")  
#importFrom(gridExtra, arrangeGrob)

GADM_BASE = "GADM/";
#GADM_URL  = "http://biogeo.ucdavis.edu/data/gadm2.8/rds/"
GADM_URL  = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/"

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y 

## ---------------------------------------------------------------------------
## Function : gadm.loadtCountries (constructor)
## Description : load a file from local system or from GADM repository 
##               You just have to specify the countries (ISO3 CODE) of the
##               file name, like "ARG" for Argentina.
##               Optionally you can specify which level you want to have and
##               simplify or not theshapefile (a value less or equal 0.01
##               is recommended)
## Return : This function creates a GADMWrapper that contains a 
##          SpatialPolygonsDataFrame object that contains all maps you 
##          specify in "fileNames".
## ---------------------------------------------------------------------------
gadm.loadCountries <- function (fileNames, 
                                level = 0, 
                                basefile=GADM_BASE, 
                                baseurl=GADM_URL,
                                simplify=NULL)
  {
  #   requireNamespace("ggplot2","classInt", "rgdal", "rgeos")
  #   requireNamespace("maptools","sp", "dplyr", "RColorBrewer")
  loadNamespace("sp")  
  # ---- Load file and change prefix
  loadChangePrefix <- function (fileName, level = 0) {
    
    FILENAME = sprintf("%s_adm%d.rds", fileName,level)
    REMOTEFILE = sprintf("gadm36_%s_%d_sp.rds", fileName,level)
    LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
    if (file.exists(LOCAL_FILE)) {
      gadm <- readRDS(LOCAL_FILE)
      theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
      theFile
    } else {
      gadm <- NULL
      REMOTE_LINK <- sprintf("%s%s", baseurl, REMOTEFILE)
      r <- download.file(REMOTE_LINK, LOCAL_FILE)
      gadm <- readRDS(LOCAL_FILE)
      if (!is.null(gadm)) {
        saveRDS(gadm, file=LOCAL_FILE)
        theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
        theFile
      }
    }
  }
  polygon <- sapply(fileNames, loadChangePrefix, level)
  polyMap <- do.call("rbind", polygon)
  #  polyMap <- sp::rbind(polygon)
  # ---- Simplify polygones if requested by user
  if (!is.null(simplify)) {
    S <- gSimplify(polyMap, simplify, topologyPreserve = TRUE)
    polyMap@polygons <- S@polygons
  }
  
  # ---- Create GADMWrapper object
  structure(list("basename"=basefile,
                 "spdf"=polyMap,
                 "level"=level,
                 "stripped" = FALSE),
            class = "GADMWrapper")
}


## ---------------------------------------------------------------------------
## Method : subset
## Return : a new object GADMWrapper with the selected regions
## ---------------------------------------------------------------------------
subset <- function(x, level=NULL, regions=NULL) UseMethod("subset", x)
subset <- function(x, level=NULL, regions=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  NAME <- sprintf("NAME_%d", level)
  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[df1$N %in% regions,];
  structure(list("basename"=x$basename,
                  "spdf"=df2,
                 "level"=x$level,
                 "stripped"=FALSE),
            class = "GADMWrapper")  
}

## ---------------------------------------------------------------------------
## Method : remove
## Return : a new object GADMWrapper without the selected regions
## ---------------------------------------------------------------------------
remove <- function(x, level=NULL, regions=NULL) UseMethod("remove", x)
remove <- function(x, level=NULL, regions=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  NAME <- sprintf("NAME_%d", level)
  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[!df1$N %in% regions,];
  structure(list("basename"=x$basename,
                 "spdf"=df2,
                 "level"=x$level,
                 "stripped"=FALSE),
            class = "GADMWrapper")  
}

## ---------------------------------------------------------------------------
## Method : listNames
## Return : a list of names for the selected level
## ---------------------------------------------------------------------------
listNames <- function(x, level=0) UseMethod("listNames", x)
listNames.GADMWrapper <- function(x, level=0) {
  if (level > x$level) {
    cat(sprintf("Warning: max level=%d\n", x$level))
    level = x$level
  }
  if (x$level == 0) {
    name <-"NAME_ISO"
  } else {
    name <- sprintf("NAME_%d", level)
  }
  unique(x$spdf@data[, name])
}

## ---------------------------------------------------------------------------
## Method : saveas
## Return : the name of the saved file
## ---------------------------------------------------------------------------
saveas <- function(x, name=NULL) UseMethod("saveas", x)
saveas.GADMWrapper <- function(x, name=NULL) {
  if (is.null(name)) {
    stop("You have to provide a name.")
  }
  FName <- sprintf("%s%s_adm%d.rds", x$basename, name, x$level)
  gadm = x$spdf
  saveRDS(gadm, file=FName);
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
  long = lat = group <- NULL
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
                     regionsize=0.5) UseMethod("vignette", main)

vignette.GADMWrapper <- function(main, region,
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


dots <- function(x, points, color="red",
                 value = NULL,
                 breaks = NULL,
                 steps = 5,
                 palette = NULL,
                 labels = NULL,
                 strate = NULL ,
                 title="",
                 legend = NULL,
                 note=NULL) UseMethod("dots", x)

dots.GADMWrapper <- function(x, points, color="red",
                             value = NULL,
                             breaks = NULL,
                             steps = 5,
                             palette = NULL,
                             labels = NULL,
                             strate = NULL ,
                             title="",
                             legend = NULL,
                             note="") {
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }
  
  .data <- fortify(x$spdf, region=.name);
  .title <- title
  .pcolor <- color
  .value <- value
  .points <- points
  .palette = palette
  .steps <- steps
  .labels <- labels
  .strate <- strate
  
  BREAKSVAL <- NULL
  
  # Removing missing values
  # -------------------------------------------
  .points <- .points[!is.na(.points[,.value]),]
  
  if (!is.null(.value)) {
    .BRK <- computeBreaks(points[, .value], breaks = breaks, steps = .steps, labels = labels)
    .BRK <- as.factor(.BRK)
    .points$BREAKSVAL = .BRK
  }
  
  if (!is.null(.strate)) {
    STRATE <- as.factor(points[,.strate])
  }

  # Palettes
  # ---------------------------------------------------------
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
  
  # Labels
  # ---------------------------------------------------------
  if (is.null(labels)) {
    .labels <- levels(.points$BREAKSVAL)
  }
  
  
  # Theme base
  # ----------------------------------------------------------
  .Theme <-   theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
#    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank())
  # ----------------------------------------------------------
  
  long = lat = group <- NULL
  if (!is.null(note)) {
    note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  }
  
P <- ggplot() +
  geom_polygon(data=.data, aes(x=long, y=lat,  group=group),
               fill=NA, color="black", size = 0.5) +
  xlab(paste("\n\n", note, sep="")) + ylab("")


  if (!is.null(.value)) {
    # Colored dots from breaks
    # ----------------------------------------------------------
    P <- P + geom_point(data=.points,
                        aes(x=longitude, y=latitude,
                             color=factor(BREAKSVAL)), 
                        size=8, alpha=0.8) +
      geom_point(data=.points,
                          aes(x=longitude, y=latitude), 
                          size=8, alpha=0.8, shape=21) +
        
      scale_color_manual(legend, values = .palette, 
                        limits=levels(.BRK),
                        labels=.labels,
                        guide = guide_legend(reverse = T)) +
      labs(title = title) +
    .Theme + coord_map();
    return(P)
  }
  else {
    longitude = latitude <- NULL
    if (is.null(strate)) {
      print("no stratification\n")
      P <- P + geom_point(data=points, aes(x=longitude, y=latitude), size=4, color=.pcolor, shape=16) +
      labs(title = title) + .Theme + coord_map();
      return(P)
    }
    else {
      print("stratification\n")
      P <- P + geom_point(data=points, aes(x=longitude, y=latitude, shape=STRATE), size=4, color=.pcolor) +
        labs(title = title) +
        scale_shape_manual(values = c(15:18,65:75)) +
        .Theme + coord_map();
        return(P)
     }
  }
}  

propDots <- function(x, data, value, breaks=NULL, range=NULL, labels=NULL, color="red", title="", note=NULL) UseMethod("propDots", x)
propDots.GADMWrapper <- function(x, data, value, breaks=NULL, range=NULL, labels=NULL, color="red", title="", note=NULL) {
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }
  
  .map <- fortify(x$spdf, region=.name)
  .data <- data
  .value <- value
  .title <- title
  .pcolor <- color

  getBreaks <- function(value) {
    .min = min(data[,value], na.rm = T)
    .max = max(data[,value], na.rm = T)
    .r = .max - .min
    .B <- round(c(.r * 0.2, .r * 0.4, .r * 0.6, .r * 0.8, .r * 1.0), 0)
    list(.B, .min, .max)
  }

  .data <- .data[order(-.data[,.value]),]
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
  
  long = lat = group <- NULL
  if (!is.null(note)) {
    note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  }
  
  P_ <- ggplot() +
  geom_polygon(data=.map, aes(x=long, y=lat,  group=group),
                 fill=NA, color="black", size = 0.5) +
  
  geom_point(data=.data,
                    aes_string(x="longitude", y="latitude", 
                    size=eval(value)), 
                    fill=.pcolor, color="#000000", shape=21, alpha=0.25) +
    xlab(paste("\n\n", note, sep="")) + ylab("") +
  scale_size_area(max_size = 24, breaks=.breaks, limits = .range, labels=.labels) +
    labs(title = .title, fill = "") + 
    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
#    theme(axis.title = element_blank()) +
    coord_map();
    P_
}  

isopleth <- function(x,  data, palette=NULL, title="") UseMethod("isopleth", x)
isopleth.GADMWrapper <- function(x, data, palette=NULL, title="") {
  if (x$level == 0) {
    .name <-"NAME_ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }
  
  .map <- fortify(x$spdf, region=.name)
  .data <- data
  .titles <- title
  .palette <- palette

  if (is.null(palette)) {
    .palette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  }
  else {
    .palette <- colorRampPalette(brewer.pal(9, palette), space="Lab")
  }
  
  with(.map, {
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
  })
} 

plotmap <- function(x, title="") UseMethod("plotmap", x)
plotmap.GADMWrapper <- function(x, title="") {
  #  stop("input function ok")
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }
  
  .map <- x$spdf
  if (x$stripped == FALSE) {
    .map <- fortify(x$spdf, region=.name)
  }
  
  
  long = lat = group <- NULL
  
  P <- ggplot() +
    geom_polygon(data=.map, aes(x=long, y=lat, group=group),
                 fill=NA, color="black", size = 0.5)+
    labs(title = title, fill = "") + 
#    theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
#    theme(axis.text = element_blank()) +
#    theme(axis.title = element_blank()) +
#    theme(axis.ticks = element_blank()) +
    coord_map();
  P
}


grid.map <- function(left, right, center=NULL, title=NULL) {
  LS = do.call(arrangeGrob, c(left, list(ncol=1)))
  RS = do.call(arrangeGrob, c(right, list(ncol=1)))
  
  .title = sprintf("\n%s", title)
  if (!is.null(center)) {
    CS = do.call(arrangeGrob, center)
    gridExtra::grid.arrange(LS, CS, RS, ncol=3, main=.title, widths=c(1,3,1))
  } else {
    gridExtra::grid.arrange(LS, RS, ncol=2, main=title, widths=c(1,3), heights=c(1,1.5))
  }
}
