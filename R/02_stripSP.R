## ---------------------------------------------------------------------------
## Method : stripSP
## Return : a data.frame ready to use with ggplot2
## ---------------------------------------------------------------------------
stripSP <- function(x, level=NULL) UseMethod("stripSP", x)
stripSP.GADMWrapper <- function(x, level=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }

  .map <- fortify(x$spdf, region=.name)
  
  # ---- Create GADMWrapper object
  structure(list("basename"=x$basefile,
                 "spdf"=.map,
                 "level"=level,
                 "stripped" = TRUE),
            class = "GADMWrapper")
}

gadm.loadStripped <- function(name, level, basefile='./') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", name,level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  .map <- readRDS(LOCAL_FILE)
  if (is.null(.map)) {
    stop("Error: Enable to read file!")
  }
  .map
}

saveAsStripped <- function(x, name, level= NULL, basefile = './') UseMethod("saveAsStripped")
saveAsStripped.GADMWrapper <- function(x,  name, level = NULL, basefile = './') {
  SP <- x
  if (x$stripped == FALSE) {
    SP <- stripSP(x, level)
  }
  gadm.saveStripped(SP, name, basefile)
}
  
strippedExists <- function(name, level, basefile = './') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", name,level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  file.exists(LOCAL_FILE) 
}

gadm.saveStripped <- function(x, name, basefile = './') {
  FILENAME = sprintf("STRIP_%s_adm%d.rds", name,x$level)
  LOCAL_FILE = sprintf("%s%s", basefile, FILENAME)
  saveRDS(x, file = LOCAL_FILE)
  x
}
