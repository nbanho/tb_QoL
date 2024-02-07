#' Median and interquartile range
#'

median_iqr <- function(x, name = "") {
  m <- median(x, na.rm = TRUE)
  l <- quantile(x, .25, na.rm = TRUE)
  u <- quantile(x, .75, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  nas <- sum(is.na(x))
  smry <- round(c(m, l, u, min, max), 0)
  paste0(
    name,
    "Median ",
    smry[1],
    " (IQR ",
    smry[2],
    " - ",
    smry[3],
    ", Range ",
    smry[4],
    " - ",
    smry[5],
    ", NA ",
    nas,
    ")"
  )
}

#' Count of positive binary outcome (percent of total)
#'

count_bin <- function(x, name = "") {
  n0 <- length(x)
  n1 <- sum(x, na.rm = TRUE)
  n3 <- sum(is.na(x))
  n2 <- n0 - n1 - n3
  n <- c(n0, n1, n2, n3)
  p <- round(100 * n[-1] / n[1])


  paste0(
    name,
    "Yes: ", n[2], " (", p[1], "%)",
    ", No: ", n[3], " (", p[2], "%)",
    ", NA: ", n[4], " (", p[3], "%)"
  )
}


#' Count per categorical outcome (percent of total)
#'

count_cat <- function(x) {
  counts <- table(x, useNA = "always")
  names <- names(counts)
  names <- ifelse(is.na(names), "NA", names)
  cdf <- data.frame(
    category = names,
    count = c(unname(counts)),
    percent = paste0(round(100 * c(unname(counts)) / length(x)), "%")
  )
  return(cdf)
}
