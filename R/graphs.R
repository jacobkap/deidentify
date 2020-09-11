#' Graph how much data is dropped for each percent limit to remove rare values
#'
#' Makes a graph showing what percent of values are dropped when excluding values
#' that are less common than k% of the data for every value of k from 1%-100%.
#'
#' @param data
#' A vector of strings or numbers.
#' @export
#' @return
#' A `ggplot2` graph
#'
#' @examples
#' graph_group_rare_values(mtcars$mpg)
graph_group_rare_values <- function(data) {
  length_non_na <- length(data[!is.na(data)])
  graph_data <- data.frame(k_percent = 1:100,
                      percent_dropped = NA)

  for (k_percent in 1:100) {
  temp <- get_values_rarer_than_k_percent(data,
                                          k_percent)
  temp <- data[data %in% temp]
  temp <- length(temp)
  graph_data$percent_dropped[graph_data$k_percent == k_percent] <- percent_change(length_non_na,
                                                                                  length_non_na - temp)
  }
  graph_data$percent_dropped <- graph_data$percent_dropped * -1
  ggplot2::ggplot(graph_data, ggplot2::aes_string(x = "k_percent",
                                                  y = "percent_dropped")) +
    ggplot2::geom_line(size = 1.4, color = "#e41a1c") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "'K%' Limit to Drop Values Below",
                  y = "% of Data Dropped",
                  title = "Percent of Data Removed when Excluding Values Rarer than K% of the Data")

}


percent_change <- function(old_num, new_num) {
  data <- new_num - old_num
  data <- data / old_num * 100
  return(data)
}
