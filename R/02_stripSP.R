## ---------------------------------------------------------------------------
## Method : stripSP
## Return : a data.frame ready to use with ggplot2
## ---------------------------------------------------------------------------
stripSP <- function(x, level=NULL) UseMethod("stripSP", x)
stripSP <- function(x, level=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }

  .map <- fortify(x$spdf, region=.name)
  .map
}

