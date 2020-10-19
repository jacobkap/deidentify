#' Graph effect of dropping "rare" values from a vector of data
#'
#' Makes a graph showing what either the number of unique values or the percent of values are dropped when excluding values that are less common than k% of the data for every value of k from 1%-100%.
#'
#' @param data
#' A vector of strings or numbers.
#' @param percent_dropped
#' If FALSE (default), will show the number of unique values in the data after excluding values that are rarer than k% (from 1-99%). Else, will show the percent of the data dropped after excluding values that are rarer than k% (from 1-99%)
#' @export
#' @return
#' A `ggplot2` graph
#'
#' @examples
#' graph_group_rare_values(deidentify::initiations$offense_category)
#' graph_group_rare_values(deidentify::initiations$offense_category, percent_dropped = TRUE)
graph_group_rare_values <- function(data, percent_dropped = FALSE) {
  y_label <- "# of Unique Values"
  y_var <- "number_of_unique_values"
  title <- "Number of Unique Values When Excluding Values Rarer than K% of the Data"
  if (percent_dropped) {
    y_label <- "% of Data Dropped"
    y_var <- "percent_dropped"
    title  <- "Percent of Data Removed When Excluding Values Rarer than K% of the Data"
  }
  length_non_na <- length(data[!is.na(data)])
  graph_data <- data.frame(k_percent = 1:100,
                           percent_dropped = NA)

  for (k_percent in 1:100) {
    temp <- get_rare_values(data, k_percent)
    temp <- data[data %in% temp]
    temp <- length(temp)
    graph_data$percent_dropped[k_percent] <- percent_change(length_non_na,
                                                            length_non_na - temp)
    graph_data$number_of_unique_values[k_percent] <- length_non_na - temp
  }


  graph_data$percent_dropped <- graph_data$percent_dropped * -1
  ggplot2::ggplot(graph_data, ggplot2::aes_string(x = "k_percent",
                                                  y = y_var)) +
    ggplot2::geom_line(size = 1.4, color = "#e41a1c") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "'K%' Limit to Drop Values Below",
                  y = y_label,
                  title = title)

}

#' Create a barplot showing the number of unique dates at each level of aggregation
#'
#' This takes a vector of Dates and creates a barplot showing how many unique dates are in the data, at the original inputted unit and at every level of aggregation (week, month, bimonth, quarter, halfyear, year).
#'
#' @param data
#' A vector of dates in the Date format.
#'
#' @return
#' A `ggplot2` graph
#' @export
#'
#' @examples
#' graph_aggregate_dates(deidentify::initiations$received_date)
graph_aggregate_dates <- function(data) {
  data <- unique(data)
  final <- data.frame(date_aggregation = "No Aggregation",
                      number_of_unique_dates = length(unique(data)))
  for (i in c("week",
              "month",
              "bimonth",
              "quarter",
              "halfyear",
              "year")) {
    temp <- deidentify_date(data, i)
    temp <- data.frame(date_aggregation = i,
                        number_of_unique_dates = length(unique(temp)))
    final <- rbind(final, temp)

  }
  final$date_aggregation <- crimeutils::capitalize_words(final$date_aggregation)
  final$date_aggregation <- factor(final$date_aggregation,
                                   levels = rev(c("No Aggregation",
                                                  "Week",
                                                  "Month",
                                                  "Bimonth",
                                                  "Quarter",
                                                  "Halfyear",
                                                  "Year")))
  p <- ggplot2::ggplot(final, ggplot2::aes_string(x = "number_of_unique_dates",
                                      y = "date_aggregation")) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "# of Unique Date Values",
                  y = "Level of Aggregation",
                  title = "The number of unique date values at each level of aggregation")
  return(p)
}


percent_change <- function(old_num, new_num) {
  data <- new_num - old_num
  data <- data / old_num * 100
  return(data)
}
