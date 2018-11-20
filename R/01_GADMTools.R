loadNamespace("sp")  

GADM_BASE = "GADM/";
#GADM_URL  = "http://biogeo.ucdavis.edu/data/gadm2.8/rds/"
GADM_URL  = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/"

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y 

splitShapes <- function(x, name) {
  .map <- fortify(x$spdf, region = name)
  
  if (x$L360 == TRUE) {
    .map$long <- .map$long %% 360
  }
  # if (x$spdf@bbox[1,1] < -179 & x$spdf@bbox[1,2] > 179) {
  #   .map$long <- .map$long %% 360
  # }
  # if (x$spdf@bbox[2,1] < -90 & x$spdf@bbox[2,2] > 90) {
  #   .map$lat <- .map$lat %% 360
  # }
  .map  
}

gadm.longTo360 <- function(x) {
  x$L360 <- TRUE
  x$spdf@bbox[1] <- x$spdf@bbox[1] %% 360
  x
}

# Select a name to fortify it (for ggplot2) -------------------------------
gadm.getLevelName <- function(x, level=NULL) {

  .L <- level
  if (is.null(level)) {
    .L <- x$level   
  }
  
  if (.L == 0) {
    if ("GID_0" %in% colnames(x$spdf@data)) {
      .name <-"GID_0"
    } else {
      .name <-"ISO"
    }
  } else {
    .name <- sprintf("NAME_%d", .L)
  }
  
}
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
  loadNamespace("sp")  

  # Load file and change Prefix ---------------------------------------------
  loadChangePrefix <- function (fileName, level = 0) {
    FILENAME = sprintf("%s_adm%d.rds", fileName,level)
    LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
    if (!file.exists(LOCAL_FILE)) {
      .OS <- Sys.info()["sysname"]
      REMOTEFILE = sprintf("gadm36_%s_%d_sp.rds", fileName,level)
      REMOTE_LINK <- sprintf("%s%s", baseurl, REMOTEFILE)
      if (.OS == "windows") {
        download.file(REMOTE_LINK, LOCAL_FILE, method="wininet",mode="wb")
      } else {
        download.file(REMOTE_LINK, LOCAL_FILE, method = 'auto')
      }
    }
    gadm <- readRDS(LOCAL_FILE)
    if (!is.null(gadm)) {
      # saveRDS(gadm, file=LOCAL_FILE)
      theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
      theFile
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
                 "L360" = FALSE,
                 "stripped" = FALSE,
                 "hasBGND"  = FALSE),
            class = "GADMWrapper")
}


# union -------------------------------------------------------------------
gadm.union <- function(x) {
  
  if (x$stripped == FALSE) {
    .name <- gadm.getLevelName(x)
    x$spdf@data[, "UNIFY"] <- rep("WXY", length(x$spdf))
    .sp <- unionSpatialPolygons(x$spdf, x$spdf@data[, "UNIFY"])
    .df <- data.frame(rep("WXY", length(.sp)))
    colnames(.df) <- c(.name)
    .sp <- SpatialPolygonsDataFrame(.sp, .df, match.ID = FALSE) 
    return (structure(
      list(
        "basename" = x$basename,
        "spdf"     = .sp,
        "level"    = x$level,
        "L360"     = x$L360,
        "stripped" = FALSE,
        "hasBGND"  = FALSE
      ),
      class = "GADMWrapper"
    ))
  }
  else {
    stop("oooops!")
    return(x)
  }
}                              

## ---------------------------------------------------------------------------
## Method : subset
## Return : a new object GADMWrapper with the selected regions
## ---------------------------------------------------------------------------
subset <- function(x, level=NULL, regions=NULL, usevar=NULL) UseMethod("subset", x)
subset.GADMWrapper <- function(x, level=NULL, regions=NULL, usevar=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  
  
  # Select name of column ---------------------------------------------------
  if (is.null(usevar)) {
    NAME <- gadm.getLevelName(x, level)
  } else {
    NAME <- usevar
  }

  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[df1$N %in% regions,];
  structure(list("basename"=x$basename,
                 "spdf"=df2,
                 "level"=x$level,
                 "L360" = FALSE,
                 "stripped"=FALSE,
                 "hasBGND"  = FALSE),
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
  # if (x$level == 0) {
  #   name <-"NAME_ISO"
  # } else {
    name <- sprintf("NAME_%d", level)
  # }
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





plotmap <- function(x, title="") UseMethod("plotmap", x)
plotmap.GADMWrapper <- function(x, title="") {

  .x <- x
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  .name <- gadm.getLevelName(x)
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  } 
  
  if (x$stripped == FALSE) {
    .map <- splitShapes(x, .name)
  }
  
  long = lat = group = x = y <- NULL
  
  P <- ggplot() 
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  

  # Draw the shapefile ------------------------------------------------------
  P <- P + geom_polygon(data=.map, aes(x=long, y=lat, group=group),
                        fill=NA, color="black", size = 0.5)+
    labs(title = title, fill = "") + 
    theme(panel.border = element_blank(),
          plot.margin = margin(0, 0.1, 0, 0.1, "cm")) +
    theme(legend.key = element_blank()) +
    coord_quickmap();
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
