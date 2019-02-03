# NEEDS: udunits2-devel.x86_64
# NEEDS: nngeo, lwgeom
# NEEDS: postgis-devel.x86_64


#loadNamespace("sp")
loadNamespace("prettymapr")

GADM_BASE = "GADM/";
GADM_SF_URL  = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/"

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

splitShapes <- function(x, name) {
  .map <- fortify(x$spdf, region = name)

  if (x$L360 == TRUE) {
    .map$long <- .map$long %% 360
  }
  .map
}


# gt2_internal_makebbox -------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
gt2_internal_makebbox <- function(x) {
  .bb <- as.vector(sf::st_bbox(x))
  prettymapr::makebbox(.bb[4], .bb[3], .bb[2], .bb[1])
}

# # Select a name to fortify it (for ggplot2) -------------------------------
# gadm.getLevelName <- function(x, level=NULL) {
# 
#   .L <- level
#   if (is.null(level)) {
#     .L <- x$level
#   }
# 
#   .df <- as.data.frame(x$spdf)
#   if (.L == 0) {
#     if ("GID_0" %in% colnames(.df)) {
#       .name <-"GID_0"
#     } else {
#       .name <-"ISO"
#     }
#   } else {
#     .name <- sprintf("NAME_%d", .L)
#   }
#   .name
# }
# 


# internal.gt2.getBreaks ------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
internal.gt2.getBreaks <- function(data, value, breaks, steps, labels) {
  .data <- data
  .value <- value
  .new <- sprintf("%s_cut", value)
  .steps <- steps
  .labels <- labels

  # No breaks, just steps -----------------------------------------------------------------------------------------
  if (is.null(breaks)) {
    if (!is.factor(.data[,.value])) {
      .data[,.new] <- cut(.data[,.value],.steps)
    }
  }
  else if (length(breaks) > 1) {
    .data[,.new] <- cut(.data[,.value], breaks=breaks, labels = .labels)
  }
  else {
    .type <- c("sd", "equal", "pretty", "quantile", "kmeans",
               "hclust", "bclust", "fisher", "jenks")
    if (breaks %in% .type) {
      XB <- classIntervals(.data[,.value], n=.steps, style=breaks)
      .data[,.new] <- cut(.data[,.value], breaks=XB$brks, labels = .labels)
    }
    else {
      .MSG <- sprintf("%s not in %s", breaks, .type)
      stop(cat())
    }
  }
  
  .data
}

internal.gt2.cleanUp <- function(df) {
  GID_0 <- contains <- starts_with <- NULL
  df %>% 
    dplyr::rename_all(dplyr::recode, GID_0 = "ISO") %>%
    dplyr::select(-starts_with("GID"),
                  -starts_with("NL_"),
                  -starts_with("VAR"),
                  -contains('C_'))
}


# gt2_loadCountries -----------------------------------------------------------------------------------------------
# =================================================================================================================
gadm.sf.loadCountries <- function (fileNames,
                                level = 0,
                                basefile=GADM_BASE,
                                baseurl=GADM_SF_URL)
  {
  loadNamespace("sp")

  # Load file and change Prefix ---------------------------------------------
  loadSF <- function (fileName, level = 0) {
    FILENAME = sprintf("%s_adm%d.sf.rds", fileName,level)
    LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
    if (!file.exists(LOCAL_FILE)) {
      .OS <- toupper(Sys.info()["sysname"])
      REMOTEFILE = sprintf("gadm36_%s_%d_sf.rds", fileName,level)
      REMOTE_LINK <- sprintf("%s%s", baseurl, REMOTEFILE)
      if (.OS == "WINDOWS") {
        download.file(REMOTE_LINK, LOCAL_FILE, method="wininet",mode="wb")
      } else {
        download.file(REMOTE_LINK, LOCAL_FILE, method = 'auto')
      }
    }
    gadm <- readRDS(LOCAL_FILE)
    if (!is.null(gadm)) {
      return(as.data.frame(gadm))
    }
  }

  pol <- 1

  for (N in fileNames) {
    .gadm  <-loadSF(N, level)
    if (pol == 1) {
      .gadmSF <- .gadm
    } else {
      .gadmSF <- rbind(.gadmSF, .gadm)
    }
    pol <- pol + 1
  }

  
  .gadmSF <- internal.gt2.cleanUp(.gadmSF)
  
  # Creates GT2 object ----------------------------------------------------------------------------------------------
  structure(list("basename"=basefile,
                 "spdf" = sf::st_as_sf(.gadmSF),
                 "level"=level,
                 "L360" = FALSE,
                 "stripped" = FALSE,
                 "hasBGND"  = FALSE),
            class = "GT2")
}


# gadm.union.GT2 --------------------------------------------------------------------------------------------------
# =================================================================================================================
gadm.union.GT2 <- function(x, gid = "XYZ", name = "UNKNOWN") {

  if(x$level == 0) {
    warning("The 'union' operation does not make sense for an administrative map at level 0")
  }
  
  .newlevel = 0; # x$level - 1
  .geom <- sf::st_union(x$spdf)
  .sf <- sf::st_sf(GID_0 = gid, NAME_0 = name, .geom, stringsAsFactors = FALSE)

  return (structure(
    list(
      "basename" = x$basename,
      "spdf"     = .sf,
      "level"    = x$level - 1,
      "L360"     = x$L360,
      "hasBGND"  = FALSE
    ),
    class = "GT2"
  ))
}


# gt2.subbset -----------------------------------------------------------------------------------------------------
# =================================================================================================================
gadm.subset.GT2 <- function(x, level=NULL, regions=NULL, usevar=NULL) {
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
            class = "GT2")
}


# gadm.remove -----------------------------------------------------------------------------------------------------
# =================================================================================================================
gadm.remove.GT2 <- function(x, level=NULL, regions=NULL) {
  if (is.null(level)) {
    level <- x$level
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  NAME <- gadm.getLevelName(x, level)
  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[!df1$N %in% regions,];
  structure(list("basename"=x$basename,
                 "spdf"=df2,
                 "level"=x$level,
                 "hasBGND"  = FALSE),
            class = "GT2")
}


# gt2.listNames ---------------------------------------------------------------------------------------------------
# =================================================================================================================
listNames.GT2 <- function(x, level=0) {
  if (level > x$level) {
    cat(sprintf("Warning: max level=%d\n", x$level))
    level = x$level
  }
  name <- sprintf("NAME_%d", level)
  # }
  ret <- dplyr::distinct(as.data.frame(x$spdf), get(name))
  ret[,1]
}

# ---------------------------------------------------------------------------
# Method : saveas
# Return : the name of the saved file
# ---------------------------------------------------------------------------
saveAs.GT2 <- function(x, name=NULL, directory=NULL) {
  if (is.null(directory)) {
    directory <- x$basename
  }
  if (is.null(name)) {
    stop("You have to provide a name.")
  }
  FName <- sprintf("%s%s_adm%d.sf.rds", directory, name, x$level)
  gadm = x$spdf
  saveRDS(gadm, file=FName);
  FName
}


# Function: internal_getNorthScaleBar -----------------------------------------------------------------------------
# =================================================================================================================
internal_getNorthScaleBar <- function(P) {
  P <- P + ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(0.25, "cm"),
                                      pad_y = unit(0.25, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering)
  P
}


# Function: gt2.plot ----------------------------------------------------------------------------------------------
# =================================================================================================================
plotmap.GT2 <- function(x, title="") {
  .map = x$spdf

  if (x$hasBGND == TRUE) {
    .raster <- x$BGND
  } 
  
  P <- ggplot()

  # Draw background if exists -----------------------------------------------
  if (x$hasBGND) {
    x <- y <- NULL
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }


  # Draw the shapefile ------------------------------------------------------
  P <- P + geom_sf(data=.map, fill=NA, color="black", size = 0.25)
  P <-  internal_getNorthScaleBar(P) +
    labs(title = title, fill = "") +
    theme(panel.border = element_blank(),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
    theme(legend.key = element_blank()) +
    coord_sf();
  P
}


# grid.map <- function(left, right, center=NULL, title=NULL) {
#   LS = do.call(arrangeGrob, c(left, list(ncol=1)))
#   RS = do.call(arrangeGrob, c(right, list(ncol=1)))
# 
#   .title = sprintf("\n%s", title)
#   if (!is.null(center)) {
#     CS = do.call(arrangeGrob, center)
#     gridExtra::grid.arrange(LS, CS, RS, ncol=3, main=.title, widths=c(1,3,1))
#   } else {
#     gridExtra::grid.arrange(LS, RS, ncol=2, main=title, widths=c(1,3), heights=c(1,1.5))
#   }
# }
