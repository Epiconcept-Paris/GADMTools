## ---------------------------------------------------------------------------
## Method : stripSP
## Return : a GADMWrapper ready to use with ggplot2
## ---------------------------------------------------------------------------
stripSP <- function(x, level=NULL) UseMethod("stripSP", x)
stripSP.gadm_sp <- function(x, level=NULL) {

  .level <- x$level
  
  if (is.null(level)) {
    if (x$level == 0) {
      .name <-"ISO"
    } else {
      .name <- sprintf("NAME_%d", x$level)
    }
  } else {
      if (level > x$level || level < 0) {
        .name <- sprintf("NAME_%d", x$level)
      } else {
        .name <- sprintf("NAME_%d", level)
        .level <- level
      }
  }

  .map <- fortify(x$spdf, region=.name)
  
  # ---- Create GADMWrapper object
  structure(list("basename"=x$basefile,
                 "spdf"=.map,
                 "level"=.level,
                 "stripped" = TRUE),
            class = "GADMWrapper")
}
## ===========================================================================


gadm_loadStripped <- function(name, level, basefile='./') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", name,level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  print(LOCAL_FILE)
  .map <- readRDS(LOCAL_FILE)
  if (is.null(.map)) {
    stop("Error: Enable to read file!")
  }
  .map
}

saveAsStripped <- function(x, fname, name= NULL, basefile = './') UseMethod("saveAsStripped")
saveAsStripped.gadm_sp <- function(x,  fname, name = NULL, basefile = './') {
  SP <- x
  if (x$stripped == FALSE) {
    SP <- stripSP(x, name)
  }
  gadm_saveStripped(SP, fname, basefile)
}
  
strippedExists <- function(name, level, basefile = './') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", name,level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  file.exists(LOCAL_FILE) 
}

gadm_saveStripped <- function(x, fname, basefile = './') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", fname,x$level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  saveRDS(x, file = LOCAL_FILE)
  TRUE
}
