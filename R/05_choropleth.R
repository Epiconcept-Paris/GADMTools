choropleth <- function(x, data,
                       value=NULL,
                       breaks = NULL,
                       steps = 5,
                       adm.join=NULL,
                       data.join=NULL,
                       legend = NULL,
                       labels = NULL,
                       palette=NULL,
                       title="") UseMethod("choropleth", x)

choropleth.GADMWrapper <- function(x,
                                   data,
                                   value=NULL,
                                   breaks = NULL,
                                   steps = 5,
                                   adm.join = NULL,
                                   data.join=NULL,
                                   legend = NULL,
                                   labels = NULL,
                                   palette=NULL,
                                   title="") {
  
  if (is.null(value)) stop("Unknown value (NULL)\n")
  
  if (!is.null(adm.join)) {
    .name = adm.join
  }
  else {
    stop("adm.join is NULL! You MUST provide a name.")
  }
  
  .data <- data
  .value <- value
  .range <- range
  .legend <- legend
  .palette <- palette
  .steps <- steps
  .labels <- labels

  # -------------------------------------------------------
  # BREAKS
  # -------------------------------------------------------
  if (is.null(breaks)) {
    if (!is.factor(.data[,.value])) {
      .data[,.value] <- cut(.data[,.value],.steps)
    }
  }
  else if (length(breaks) > 1) {
    .data[,.value] <- cut(.data[,.value], breaks=breaks, labels = .labels)
  }
  else {
    .type <- c("sd", "equal", "pretty", "quantile", "kmeans",
               "hclust", "bclust", "fisher", "jenks")
    if (breaks %in% .type) {
      XB <- classIntervals(.data[,.value], n=.steps, style=breaks)
      .data[,.value] <- cut(.data[,.value], breaks=XB$brks, labels = .labels)
    }
    else {
      .MSG <- sprintf("%s not in %s", breaks, .type)
      stop(cat())
    }
  }
  
  if (!is.null(adm.join)) {
    names(.data)[names(.data)==adm.join] <- .name
    .map <- fortify(x$spdf, region=.name)
    names(.map)[names(.map)=="id"] <- .name
  }
  
  P <- dplyr::left_join(.map, .data)
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
  

  if (is.null(labels)) {
    .labels <- levels(P$CHPLT_VALUE)
  }
  
  if (is.null(legend)) .legend <- value
  long = lat = group = CHPLT_VALUE <- NULL
  .P <- ggplot(P, aes(x=long, y=lat, group=group)) +
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
    if (.legend == FALSE) {
      .P <- .P + theme(legend.position="none")
    }
  .P
}

