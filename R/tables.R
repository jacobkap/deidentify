#' Make a data.frame showing either the outcome (k-score) of all possible de-identify choices or only those that meet a certain k-score threshold.
#'
#' @inheritParams deidentify_data
#' @param data
#' @param group_rare_values_cols
#' @param preferred_k_score
#' A number of vector of numbers to set the minimum (and maximum if a vector) k-score you want from the possible choices.
#'
#' @return
#' Returns a data.frame that only has all possible choices of decisions to make and the k-score that it returns. Each row is a possible decision when using the `deidentify_data()` function and includes summary statistics for the k-score of that decision. If preferred_k_score is set, returns only choices that meet this parameter. If no choices meet this k-score minimum, will return an empty data.frame.
#' @export
#'
#' @examples
deidentify_choices_table <- function(data,
                                     group_rare_values_cols,
                                     k_score_columns,
                                     allow_column_combinations,
                                     preferred_k_score = NULL) {

  col_types <- sapply(data, class)

  if (!is.null(group_rare_values_cols) & any(col_types %in% "Date")) {
    combinations <- as.vector(outer(c("week",
                                      "month",
                                      "bimonth",
                                      "quarter",
                                      "halfyear",
                                      "year"), 1:100, paste, sep="."))
  } else if (!is.null(group_rare_values_cols) & !any(col_types %in% "Date")){
    combinations <- 1:99
    combinations <- paste0(".", combinations)
  } else if (is.null(group_rare_values_cols) & !any(col_types %in% "Date")) {
    combinations <- c("week",
                      "month",
                      "bimonth",
                      "quarter",
                      "halfyear",
                      "year")
  }

  column_combinations <- do.call("c",
                                 lapply(seq_along(columns),
                                        function(i) utils::combn(columns , i, FUN = list)))

  final <- data.frame()
  for (i in 1:length(combinations)) {
    date_choice <- strsplit(combinations[i], "\\.")[[1]]
    rare_val <- date_choice[2]
    date_choice <- date_choice[1]
    date_choice[date_choice == ""] <- NA
    temp <- deidentify_data(data,
                            group_rare_values_cols = group_rare_values_cols,
                            group_rare_values_limit = as.numeric(rare_val),
                            date_aggregation = date_choice,
                            quiet = TRUE)
    temp <- make_k_score(temp, quiet = TRUE)
    temp <- data.frame(date_aggregation = date_choice,
                       rare_values_limit = as.numeric(rare_val),
                       min_k_score      = min(temp$number_of_observations),
                       mean_k_score     = mean(temp$number_of_observations),
                       median_k_score   = stats::median(temp$number_of_observations),
                       max_k_score      = max(temp$number_of_observations))
    final <- rbind(final, temp)
  }

  if (!is.null(preferred_k_score)) {
    if (length(preferred_k_score) == 1) {
      final <- final[final$min_k_score >= preferred_k_score, ]
    } else {
      final <- final[final$min_k_score >= min(preferred_k_score) &
                       final$min_k_score <= max(preferred_k_score), ]
    }
  }

  return(final)

}

