getpercentile <- function(column, percent_list) {
  quantile(column, probs = percent_list)
}