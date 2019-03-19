fast.choropleth <- function(x, data,
                            value=NULL,
                            breaks = NULL,
                            steps = 5,
                            adm.join=NULL,
                            legend = NULL,
                            labels = NULL,
                            palette=NULL,
                            title="") UseMethod("fast.choropleth", x)

fast.choropleth.gadm_sp <- function(x,
                                        data,
                                        value=NULL,
                                        breaks = NULL,
                                        steps = 5,
                                        adm.join = NULL,
                                        legend = NULL,
                                        labels = NULL,
                                        palette=NULL,
                                        title="") {
  .map <- x$spdf
  
  .data <- data
  .level <- x$level
  .value <- value
  .range <- range
  .legend <- legend
  .palette <- palette
  .steps <- steps
  

  # -------------------------------------------------------
  # Breaks
  # -------------------------------------------------------
  getBreaksValues <- function(x, breaks, steps)
  {
    if (is.null(breaks)) {
      if (!is.factor(x)) {
        .ret <- cut(x, steps)
      }
    }
    else if (length(breaks) > 1) {
      .steps = length(breaks) - 1
      .ret <- classIntervals(x, n=.steps, style="fixed", fixedBreaks=breaks)
    }
    else {
      .type <- c("sd", "equal", "pretty", "quantile", "kmeans",
                 "hclust", "bclust", "fisher", "jenks")
      if (breaks %in% .type) {
        .ret <- classIntervals(x, n=steps, style=breaks)
        #.ret <- cut(x, breaks=XB$brks, labels = labels)
      }
      else {
        .MSG <- sprintf("%s not in %s", breaks, .type)
        stop(.MSG)
      }
    }
    .ret
  }  
  
  # -------------------------------------------------------
  # Palettes
  # -------------------------------------------------------
  if (is.null(palette)) {
    .palette <- rev(RColorBrewer::brewer.pal(.steps, "Spectral"))
  }
  else {
    if (length(palette)==1) {
      .palette <- RColorBrewer::brewer.pal(.steps, palette)
    }
    else {
      .steps <- length(palette)
    }
  }

  .breaks <- getBreaksValues(.data[, .value], breaks, .steps)
  .brks <- .breaks$brks
  
  
  # merge dataset and mapdata
  # --------------------------------------------------------
  .map <- merge(.map, .data, by=adm.join)
  
  trellis.par.set(axis.line=list(col=NA)) 
  spplot(.map, value, col.regions = .palette, at=.brks, main=title)
}
