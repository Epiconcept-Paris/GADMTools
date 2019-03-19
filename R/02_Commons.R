# -------------------------------------------------------
# FUNCTION internal_getBreaks
# -------------------------------------------------------
internal_getBreaks <- function(x, breaks, steps, labels)
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
      XB <- classIntervals(x, n=steps, style=breaks)
      .ret <- cut(x, breaks=XB$brks, labels = labels)
    }
    else {
      .MSG <- sprintf("%s not in %s", breaks, .type)
      stop(.MSG)
    }
  }
  .ret
}  

# internal.gt2.getBreaks ----------------------------------------------------
# ---------------------------------------------------------------------------
internal.gt2.getBreaks <- function(data, value, breaks, steps, labels) {
  .data <- data
  .value <- value
  .steps <- steps
  .labels <- labels
  
  # No breaks, just steps -----------------------------------------------------------------------------------------
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
  
  .data
}
