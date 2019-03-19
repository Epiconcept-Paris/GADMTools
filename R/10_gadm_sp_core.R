loadNamespace("sp")  
loadNamespace("raster")  

GADM_BASE = "GADM/";
GADM_URL  = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/"

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y 


splitShapes <- function(x, name) {
  .map <- fortify(x$spdf, region = name)
  
  if (x$L360 == TRUE) {
    .map$long <- .map$long %% 360
  }
  .map  
}

gadm_longTo360.gadm_sp <- function(x) {
  x$L360 <- TRUE
  x$spdf@bbox[1] <- x$spdf@bbox[1] %% 360
  x
}

# Select a name to fortify it (for ggplot2) -------------------------------
gadm_getLevelName <- function(x, level=NULL) {

  .L <- level
  if (is.null(level)) {
    .L <- x$level   
  }
  
  if (.L == 0) {
    if (class(x) == "gadm_sf") {
      if ("GID_0" %in% colnames(x$sf)) {
        .name <-"GID_0"
      } else {
        .name <-"ISO"
      }
    }
    
    if (class(x) == "gadm_sp") {
      if ("GID_0" %in% colnames(x$spdf@data)) {
        .name <-"GID_0"
      } else {
        .name <-"ISO"
      }
    }
    
  } else {
    .name <- sprintf("NAME_%d", .L)
  }
  return(.name)
}
## ---------------------------------------------------------------------------
## Function : gadm_loadtCountries (constructor)
## Description : load a file from local system or from GADM repository 
##               You just have to specify the countries (ISO3 CODE) of the
##               file name, like "ARG" for Argentina.
##               Optionally you can specify which level you want to have and
##               simplify or not theshapefile (a value less or equal 0.01
##               is recommended)
## Return : This function creates a gadm_sp that contains a 
##          SpatialPolygonsDataFrame object that contains all maps you 
##          specify in "fileNames".
## ---------------------------------------------------------------------------
gadm_sp_loadCountries <- gadm_sp.loadCountries <- function (fileNames, 
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
      .OS <- toupper(Sys.info()["sysname"])
      REMOTEFILE = sprintf("gadm36_%s_%d_sp.rds", fileName,level)
      REMOTE_LINK <- sprintf("%s%s", baseurl, REMOTEFILE)
      if (.OS == "WINDOWS") {
        download.file(REMOTE_LINK, LOCAL_FILE, method="wininet",mode="wb")
      } else {
        download.file(REMOTE_LINK, LOCAL_FILE, method = 'auto')
      }
    }
    gadm <- readRDS(LOCAL_FILE)
    if (!is.null(gadm)) {
      theFile <- spChFIDs(gadm, paste(fileName, row.names(gadm), sep = "_"))
      theFile
    }
  }

  polygon <- sapply(fileNames, loadChangePrefix, level)
  polyMap <- do.call("rbind", polygon)
  # ---- Simplify polygones if requested by user
  if (!is.null(simplify)) {
    S <- gSimplify(polyMap, simplify, topologyPreserve = TRUE)
    polyMap@polygons <- S@polygons
  }
  
  # ---- Create gadm_sp object
  structure(list("basename"=basefile,
                 "spdf"=polyMap,
                 "level"=level,
                 "holes" = c(),
                 "L360" = FALSE,
                 "stripped" = FALSE,
                 "hasBGND"  = FALSE),
            class = "gadm_sp")
}



# gadm_simplify.gadm_sp ---------------------------------------------------------------------------------------
# =================================================================================================================
# gadm_simplify.gadm_sp <- function(x, keep = 0.05) {
#   .x <- x
#   .x$spdf <- rmapshaper::ms_simplify(.x$spdf, keep = keep, keep_shapes = TRUE)
#   .x
# }


## ---------------------------------------------------------------------------
## Method : listNames
## Return : a list of names for the selected level
## ---------------------------------------------------------------------------
listNames <- function(x, level=0) UseMethod("listNames", x)
listNames.gadm_sp <- function(x, level=0) {

  if (level > x$level) {
    cat(sprintf("Warning: max level=%d\n", x$level))
    level = x$level
  }
  name <- sprintf("NAME_%d", level)

  ret <- dplyr::distinct(as.data.frame(x$spdf), get(name))
  ret[,1]
  
}

## ---------------------------------------------------------------------------
## Method : saveas
## Return : the name of the saved file
## ---------------------------------------------------------------------------
saveAs.gadm_sp <- function(x, name=NULL, directory=NULL) {
  if (is.null(directory)) {
    directory <- x$basename
  }
  if (is.null(name)) {
    stop("You have to provide a name.")
  }
  FName <- sprintf("%s%s_adm%d.rds", directory, name, x$level)
  gadm = x$spdf
  saveRDS(gadm, file=FName);
  FName
}

### MAPPING
gadm_initMap <- function(title="MAP") {
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

vignette.gadm_sp <- function(main, region,
                     maincolor="black",
                     regioncolor="white",
                     mainfill="grey",
                     regionfill="black",
                     mainsize=1.0,
                     regionsize=0.5)
{
  Map <- gadm_initMap("") 
  Map <- addMap(Map, main,   color=maincolor,   fill=mainfill,   size=mainsize )
  Map <- addMap(Map, region, color=regioncolor, fill=regionfill, size=regionsize)
  Map$P <- Map$P + theme_nothing(legend = TRUE)
  render(Map)
}





gadm_plot.gadm_sp <- function(x, title="") {

  .x <- x
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  .name <- gadm_getLevelName(x)
  
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
