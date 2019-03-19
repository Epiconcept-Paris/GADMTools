json.choropleth <- function(x, data,
                       value=NULL,
                       breaks = NULL,
                       steps = 5,
                       adm.join=NULL,
                       legend = NULL,
                       labels = NULL,
                       palette=NULL,
                       title="") UseMethod("json.choropleth", x)

json.choropleth.gadm_sp <- function(x,
                                   data,
                                   value=NULL,
                                   breaks = NULL,
                                   steps = 5,
                                   adm.join = NULL,
                                   legend = NULL,
                                   labels = NULL,
                                   palette=NULL,
                                   title="") {
  
  if (is.null(value)) stop("Unknown value (NULL)\n")
  
  if (!is.null(adm.join)) {
    .name = adm.join
  }
  else {
    stop("adm.join is NULL! You MUST provide a value for adm.join.")
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
  

.BRK <- internal_getBreaks(.data[, .value], breaks = breaks, steps = .steps, labels = labels)
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
P1 <- toJSON(list(Palette = palette, Labels = labels))
P1 <- str_replace_all(P1, '"', "'")
x$spdf@data$params <- rep(P1, length.out = nrow(x$spdf@data))
# P2 <- list(params = P1)
# P3 <- toJSON(P2)
# sink("params.json")
# cat(P3, "\n")
# sink()

#cat(P3)

writeOGR(x$spdf, dsn=.name, layer=.name, driver="GeoJSON")
file.rename(.name, "output.json")
# G <- fromJSON(.fname)
# 
# P1 <- list(Palette = palette, Labels = labels)
# P2 <- list(params = P1)
# P3 <- list(geojson = G)
# R <- toJSON(c(P2, P3))
# sink("output.json")
# cat(R, "\n")
# sink()
# # geojson <- geojson_json(x$spdf)
# # params <- toJSON(P2)
# # choropleth <- params + geojson
# 
# #cat(params)
return(T)
}

