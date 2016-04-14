# -------------------------------------------------------
# FUNCTION computeBreaks
# -------------------------------------------------------
computeBreaks <- function(x, breaks, steps, labels)
{
  if (is.null(breaks)) {
    if (!is.factor(x)) {
      .ret <- cut(x, steps)
    }
  }
  else if (length(breaks) > 1) {
    .ret <- cut(x, breaks=breaks, labels = labels)
  }
  else {
    .type <- c("sd", "equal", "pretty", "quantile", "kmeans",
               "hclust", "bclust", "fisher", "jenks")
    if (breaks %in% .type) {
      XB <- classIntervals(.data[,.value], n=.steps, style=breaks)
      .ret <- cut(x, breaks=XB$brks, labels = .labels)
    }
    else {
      .MSG <- sprintf("%s not in %s", breaks, .type)
      stop(.MSG)
    }
  }
}  
  
json.choropleth <- function(x, data,
                       value=NULL,
                       breaks = NULL,
                       steps = 5,
                       adm.join=NULL,
                       data.join=NULL,
                       legend = NULL,
                       labels = NULL,
                       palette=NULL,
                       title="") UseMethod("json.choropleth", x)

json.choropleth.GADMWrapper <- function(x,
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
  .level <- x$level
  .value <- value
  .range <- range
  .legend <- legend
  .palette <- palette
  .steps <- steps
  .labels <- labels

  
  # -------------------------------------------------------
  # Palettes
  # -------------------------------------------------------
  if (is.null(palette)) {
    .palette <- rev(brewer.pal(.steps, "Spectral"))
  }
  else {
    if (length(palette)==1) {
      .palette <- brewer.pal(.steps, palette)
    }
    else {
      .steps <- length(palette)
    }
  }
  

.BRK <- computeBreaks(.data[, .value], breaks = breaks, steps = .steps, labels = labels)
.BRK <- as.factor(.BRK)
DFColors <- data.frame(Breaks = levels(.BRK), color=I(.palette))

.data$Breaks <- .BRK
.data <- merge(.data, DFColors, by = "Breaks")

#.data
.name = sprintf("LEVEL%d", .level)
.fname <- sprintf("%s%s", .name, ".geojson")
x$spdf <- merge(x$spdf, .data, by=adm.join)
x$spdf@data$color <- as.character(x$spdf@data$color)
names(x$spdf@data) <- sub(adm.join, "ADMINAREA", names(x$spdf@data))
# ------------------------------------------------------------
Palette = toJSON(palette);
if (is.null(labels)) {
  labels <-rev(levels(.BRK));
}
P1 <- list(Palette = palette, Labels = labels)
P2 <- list(params = P1)
P3 <- toJSON(P2)
sink("params.json")
cat(P3, "\n")
sink()

#cat(P3)

writeOGR(x$spdf, dsn=.name, layer=.name, driver="GeoJSON")
file.rename(.name, .fname)
G <- fromJSON(.fname)

P1 <- list(Palette = palette, Labels = labels)
P2 <- list(params = P1)
P3 <- list(geojson = G)
R <- toJSON(c(P2, P3))
sink("output.json")
cat(R, "\n")
sink()


return(T)
}

