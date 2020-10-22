#' Make a data.frame showing either the outcome (k-score) of all possible de-identify choices or only those that meet a certain k-score threshold.
#'
#' @inheritParams deidentify_data
#' @param date_cols
#' A vector of strings with the name of date columns that you want to be aggregated. If NULL, will use
#' all date columns in the data.
#' @param group_rare_values_cols
#' A string or vector of strings with the columns that you want to turn rare values (below k% where k is 1-99) into NA.
#' @param k_score_columns
#' A string or vector of strings for the names of columns to generate the k-score from. If NULL (default),
#' will use the columns inputted in date_cols and group_rare_values_cols. Note that if you select columns for these parameters and don't include them in k_score_columns, deidentifying these columns won't affect the k-score.
#' @param preferred_k_score
#' A number of vector of numbers to set the minimum (and maximum if a vector) k-score you want from the possible choices.
#'
#' @return
#' Returns a data.frame that only has all possible choices of decisions to make and the k-score that it returns. Each row is a possible decision when using the `deidentify_data()` function and includes summary statistics for the k-score of that decision. If preferred_k_score is set, returns only choices that meet this parameter. If no choices meet this k-score minimum, will return an empty data.frame.
#' @export
#'
#' @examples
#' deidentify_choices_table(mtcars, group_rare_values_cols = c("mpg", "vs"),
#' k_score_columns = c("mpg", "vs"))
#'
#' deidentify_choices_table(mtcars, group_rare_values_cols = c("mpg", "vs"), preferred_k_score = 5:15)
#'
#' \dontrun{
#'deidentify_choices_table(deidentify::initiations,
#'date_cols = c("arrest_date", "felony_review_date"),
#'group_rare_values_cols = c("race", "primary_charge_flag"),
#' k_score_columns = c("primary_charge_flag", "gender", "race",
#'  "arrest_date", "felony_review_date"))
#'}
deidentify_choices_table <- function(data,
                                     date_cols = NULL,
                                     group_rare_values_cols,
                                     k_score_columns = NULL,
                                     preferred_k_score = NULL) {

  col_types <- sapply(data, is_any_date_type)
  if (is.null(date_cols)) {
    date_cols <- names(col_types)[col_types]
  }
  # Drops all unnecessary columns to speed up processing a bit.
  data <- data[, names(data) %in% unique(c(date_cols, group_rare_values_cols, k_score_columns))]

  if (is.null(k_score_columns)) {
    k_score_columns <- unique(c(date_cols, group_rare_values_cols))
  }

  if (!is.null(group_rare_values_cols) & length(date_cols) > 0) {
    combinations <- as.vector(outer(c("week",
                                      "month",
                                      "bimonth",
                                      "quarter",
                                      "halfyear",
                                      "year"), 1:100, paste, sep="."))

  } else if (!is.null(group_rare_values_cols) & length(date_cols) == 0) {
    combinations <- 1:99
    combinations <- paste0(".", combinations)

  } else if (is.null(group_rare_values_cols) & length(date_cols) > 0) {
    combinations <- c("week",
                      "month",
                      "bimonth",
                      "quarter",
                      "halfyear",
                      "year")
  }

  final <- data.frame()
  pb <- progress::progress_bar$new(format = "  downloading [:bar] :percent eta: :eta", total = length(combinations))
  pb$tick(0)
  for (i in 1:length(combinations)) {
    date_choice <- strsplit(combinations[i], "\\.")[[1]]
    rare_val <- date_choice[2]
    date_choice <- date_choice[1]
    date_choice[date_choice == ""] <- NA
    temp <- deidentify_data(data,
                            date_cols = date_cols,
                            group_rare_values_cols = group_rare_values_cols,
                            group_rare_values_limit = as.numeric(rare_val),
                            date_aggregation = date_choice,
                            quiet = TRUE)
    temp <- make_k_score(temp,
                         columns = k_score_columns,
                         quiet = TRUE)
    temp <- data.frame(date_aggregation    = date_choice,
                       date_columns        = paste0(date_cols, collapse = ", "),
                       rare_values_limit   = as.numeric(rare_val),
                       rare_values_columns = paste0(group_rare_values_cols, collapse = ", "),
                       k_score_columns     = paste0(k_score_columns, collapse = ", "),
                       min_k_score         = min(temp$number_of_observations),
                       mean_k_score        = mean(temp$number_of_observations),
                       median_k_score      = stats::median(temp$number_of_observations),
                       max_k_score         = max(temp$number_of_observations))
    final <- dplyr::bind_rows(final, temp)
    pb$tick()
  }
  final$date_columns[final$date_columns == ""] <- NA

  if (!is.null(preferred_k_score)) {
    if (length(preferred_k_score) == 1) {
      final <- final[final$min_k_score >= preferred_k_score, ]
    } else {
      final <- final[final$min_k_score >= min(preferred_k_score) &
                       final$min_k_score <= max(preferred_k_score), ]
    }
  }

  # Makes sure rownames are sequential for `get_choices_code()`.
  if (nrow(final) > 0) {
    rownames(final) <- 1:nrow(final)
  }

  return(final)
}

#' Produces the code to deidentify the data using `deidentify_data()` based on
#' a row (or rows) in the output of `deidentify_choices_table()`.
#'
#' This function takes the output of the `deidentify_choices_table()` function and
#' a row number or vector of row numbers and outputs the code that you will
#' use to deidentify the data. NOTE: this function will not have any code to
#' encrypt any columns so you must input that yourself.
#'
#' @param deidentify_choices_table
#' A data.frame returned from the `deidentify_choices_table()` function.
#' @param row_number
#' A number or vector of numbers for the row(s) that you want to output code for
#' @param data_name
#' A string with the name of the data object you want to deidentify. This should
#' be the same data you inputted in `deidentify_choices_table()`.
#'
#'
#' @return
#' Prints in the console the code to use for the `deidentify_data()` function. Also returns
#' this string invisibly.
#' @export
#'
#' @examples
get_choices_code <- function(deidentify_choices_table, row_number, data_name = "data") {

}
