getmode <- function(column) {
  uniqv <- unique(column)
  uniqv[which.max(tabulate(match(column, uniqv)))]
}
