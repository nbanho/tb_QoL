#' Median and interquartile range
#' 

median_iqr <- function(x, name = "", omit.na = T) {
  m <- median(x, na.rm = omit.na)
  l <- quantile(x, .25, na.rm = omit.na)
  u <- quantile(x, .75, na.rm = omit.na)
  mlu <- round(c(m, l, u), 0)
  paste0(name, " ", mlu[1], " (", mlu[2], " - ", mlu[3], ")")
}

#' Count of positive binary outcome (percent of total)
#' 

count_per.bin <- function(x, name = "", omit.na = T)  {
  n <- sum(x, na.rm = omit.na)
  p <- round(100 * n/length(x))
  paste0(name, " ", n, " (", p, "%)")
}


#' Count per categorical outcome (percent of total)
#' 

count_per.cat <- function(x) {
  paste0(names(table(x)), " ", table(x), " (", round(table(x) / length(x) * 100) ,"%)")
}
