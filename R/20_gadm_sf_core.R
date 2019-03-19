# NEEDS: udunits2-devel.x86_64
# NEEDS: nngeo, lwgeom
# NEEDS: postgis-devel.x86_64


#loadNamespace("sp")
loadNamespace("prettymapr")

GADM_SF_URL  = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/"
DL_FILE <- TRUE

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

splitShapes <- function(x, name) {
  .map <- fortify(x$spdf, region = name)

  if (x$L360 == TRUE) {
    .map$long <- .map$long %% 360
  }
  .map
}


# gadm_sf_internal_makebbox -------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
gadm_sf_internal_makebbox <- function(x) {
  .bb <- as.vector(sf::st_bbox(x))
  prettymapr::makebbox(.bb[4], .bb[3], .bb[2], .bb[1])
}



# internal.gadm_sf.getBreaks ------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------
internal.gadm_sf.getBreaks <- function(data, value, breaks, steps, labels) {
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

internal.gadm_sf.cleanUp <- function(df) {
  GID_0 <- contains <- starts_with <- NULL
  df %>% 
    dplyr::rename_all(dplyr::recode, GID_0 = "ISO") %>%
    dplyr::select(-starts_with("GID"),
                  -starts_with("NL_"),
                  -starts_with("VAR"),
                  -contains('C_'))
}


# gadm_sf_loadCountries -----------------------------------------------------------------------------------------------
# =================================================================================================================
gadm_sf_loadCountries <- gadm_sf.loadCountries <- function (fileNames,
                                level = 0,
                                basefile="./",
                                baseurl=GADM_SF_URL,
                                simplify=NULL)
  {
#  loadNamespace("sp")

  # Load file and change Prefix ---------------------------------------------
  loadSF <- function (fileName, level = 0) {
    DL_FILE <- TRUE
    FILENAME = sprintf("%s_adm%d.sf.rds", fileName,level)
    LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
    if (!file.exists(LOCAL_FILE)) {
      .OS <- toupper(Sys.info()["sysname"])
      REMOTEFILE = sprintf("gadm36_%s_%d_sf.rds", fileName,level)
      REMOTE_LINK <- sprintf("%s%s", baseurl, REMOTEFILE)
      if (.OS == "WINDOWS") {
        download.file(REMOTE_LINK, LOCAL_FILE, method="wininet",mode="wb")
      } else {
        tryCatch(
          download.file(REMOTE_LINK, LOCAL_FILE, method = 'auto'),
          error=function(e) return(FALSE))
      }
    }
    tryCatch({
      gadm <- readRDS(LOCAL_FILE)
      if (!is.null(gadm)) {
        return(as.data.frame(gadm))
      }}, error=function(e) return(NULL))
  }
  
  pol <- 1
  
  for (N in fileNames) {
    .gadm <-loadSF(N, level)
    if (is.null(.gadm)) {
      return(NULL)
    }
    if (pol == 1) {
      .gadmSF <- .gadm
    } else {
      .gadmSF <- rbind(.gadmSF, .gadm)
    }
    pol <- pol + 1
  }

  
  .gadmSF <- internal.gadm_sf.cleanUp(.gadmSF)
  .sf <- sf::st_as_sf(.gadmSF)
  
  # ---- Simplify polygones if requested by user
  if (!is.null(simplify)) {
    .sf <- st_simplify(.sf, preserveTopology = TRUE, dTolerance = simplify)
  }
  
  # Creates gadm_sf object ----------------------------------------------------------------------------------------------
  structure(list("basename"=basefile,
                 "sf" = .sf,
                 "level"=level,
                 "hasBGND"  = FALSE),
            class = "gadm_sf")
}


# gadm_simplify.gadm_sf ---------------------------------------------------
# =========================================================================
# gadm_simplify.gadm_sf <- function(x, tolerance = 0.025) {
#   .x <- x
#   .x$f <- sf::st_simplify(.x$sf, preserveTopology=TRUE, dTolerance=tolerance)
#   .x
# }




internal_gadm_union <- function(sf, maplevel, level=0, type="?") {
  .sf <- sf
  if(level >= maplevel) {
    return(sf)
  }
  
  ISO <- NAME_0 <- NULL
  
  .sfd <- as.data.frame(.sf)
  .ISO <- unique(.sfd$ISO)
  .NAME_0 <- unique(.sfd$NAME_0)
  
  if(maplevel > 0) {
    .NAME_1 <- unique(.sfd$NAME_1)
  }
  
  # level == 0
  # -----------------------------------------------------------------------
  if (level == 0) {
    .sf2 <- sf::st_union(.sf)
    .sf <- .sf2 %>% st_sf() %>%
      dplyr::mutate(ISO = .ISO, NAME_0 = .NAME_0) %>%
      dplyr::select(ISO, NAME_0)
    
    
  }
  # level > 0
  # -----------------------------------------------------------------------
  if (level >  0) {
    # .sf2 <- get_usf(.sf, level)
    .name <- sprintf("NAME_%d", level)
    .tname <- sprintf("TYPE_%d", level)
    .sf2 <- .sf %>%  dplyr::group_by(get(.name)) %>% 
      dplyr::summarise() %>%
      dplyr::mutate(ISO = .ISO, NAME_0 = .NAME_0, TYPE = type)
    
    colnames(.sf2)[1] <- .name
    
    .sf <- .sf2 %>%  dplyr::select_("ISO", "NAME_0", .name, "TYPE")
    colnames(.sf)[4] <- .tname
    
  }

  .sf  
}


# gadm_sf.listNames ------------------------------------------------------------
# ==============================================================================
listNames.gadm_sf <- function(x, level=0) {
  if (level > x$level) {
    cat(sprintf("Warning: max level=%d\n", x$level))
    level = x$level
  }
  name <- sprintf("NAME_%d", level)
  # }
  ret <- dplyr::distinct(as.data.frame(x$sf), get(name))
  ret[,1]
}

# ---------------------------------------------------------------------------
# Method : saveas
# Return : the name of the saved file
# ---------------------------------------------------------------------------
saveAs.gadm_sf <- function(x, name=NULL, directory=NULL) {
  if (is.null(directory)) {
    directory <- x$basename
  }
  if (is.null(name)) {
    stop("You have to provide a name.")
  }
  FName <- sprintf("%s%s_adm%d.sf.rds", directory, name, x$level)
  gadm = x$sf
  saveRDS(gadm, file=FName);
  FName
}


# Function: internal_getNorthScaleBar -----------------------------------------------------------------------------
# =================================================================================================================
internal_getNorthScaleBar <- function(P, sn, wn) {
  # P <- P + ggspatial::annotation_scale(location = "bl", width_hint = 0.25)
  
  if (sn == TRUE) {
    P <- P + ggspatial::annotation_north_arrow(location = wn, which_north = "true",
                                      height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(0.25, "cm"),
                                      pad_y = unit(0.25, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering)
  }
  P
}


# Function: gadm_sf.plot ----------------------------------------------------------------------------------------------
# =================================================================================================================
gadm_plot.gadm_sf <- function(x, title="") {
  
  .map = x$sf
  .showNorth <- x$showNorth
  .locNorth <- x$locNorth
  
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
  P <- P + geom_sf(data=.map, fill=NA, color="black", size = 0.25) +
  # P <-  internal_getNorthScaleBar(P, .showNorth, .locNorth) +
    labs(title = title, fill = "") +
    theme(panel.border = element_blank(),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
    theme(legend.key = element_blank()) + theme_light()
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
