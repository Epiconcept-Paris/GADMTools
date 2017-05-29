# -------------------------------------------------------
# FUNCTION computeBreaks
# -------------------------------------------------------
computeBreaks <- function(x, breaks, steps, labels=NULL)
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
