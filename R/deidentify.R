#' De-identify data through encrypting text columns, grouping rare values, and,
#' aggregating numeric or date columns.
#'
#'
#' @param data
#' A data.frame with the data you want to de-identify.
#' @param date_cols
#' A vector of strings with the name of date columns that you want to be aggregated
#' to the unit set in the `date_aggregation` parameter. If NULL, will use
#' all date columns in the data.
#' @param date_aggregation
#' A string with the time unit to aggregate all Date variables to.
#' Can take one of the following: 'week', 'month', 'bimonth', 'quarter',
#'  'halfyear', 'year'.
#' @param cols_to_encrypt
#' A string or vector of strings with the columns that you want to encrypt.
#' @param group_rare_values_cols
#' A string or vector of strings with the columns that you want to convert
#' rare values (below a certain percent of all values as set in
#' `group_rare_values_limit`) into NA or a particular string (or NA) set in
#' `group_rare_values_text`.
#' @param group_rare_values_limit
#' A string or vector of strings (one for each col in `group_rare_values_cols`)
#' for what threshold (in percent of all non-NA values) to determine that a value
#' is rare enough to change to NA (or the string set in `group_rare_values_text`).
#' @param group_rare_values_text
#' A string or vector of strings (one for each col in `group_rare_values_cols`)
#' for what to rename the values that are determined to be rare enough (based on
#' threshold set in `group_rare_values_limit` to rename them. If NULL (default),
#' and the vector is strings, replaces them with a string that concatenates all
#' of the rare values together (separated by a comma). If NA, replaces them with NA.
#' @param quiet
#' A Boolean for whether you want to output a message that tells you which
#' columns that you are encrypting and the seed set for each column to
#' do the encryption. If you don't set the seed yourself, you need these
#' seeds to decrypt.
#'
#' @return
#' A data.frame with the selected columns de-identify based on user parameters.
#' @examples
#' @export
deidentify_data <- function(data,
                            date_cols = NULL,
                            date_aggregation = c("week",
                                                 "month",
                                                 "bimonth",
                                                 "quarter",
                                                 "halfyear",
                                                 "year"),
                            cols_to_encrypt = NULL,
                            group_rare_values_cols = NULL,
                            group_rare_values_limit = 5,
                            group_rare_values_text = NULL,
                            quiet = FALSE) {

  col_types <- sapply(data, is_any_date_type)
  if (is.null(date_cols)) {
    date_cols <- names(col_types)[col_types]
  }

  data <-
    data %>%
    dplyr::mutate_at(date_cols, deidentify_date, date_aggregation)


  # Looks in each selected columns and replaces all the values that are less
  # than k% of the non-NA total with a selected replacement text (default
  # is NA). This is to reduce privacy concerns with rare values (i.e. if
  # person is only one with X offense, can identify through that).
  group_rare_values_limit <- rep(group_rare_values_limit,
                                 length.out = length(group_rare_values_cols))
  if (!is.null(group_rare_values_cols)) {
    for (i in 1:length(group_rare_values_cols)) {
      col <- group_rare_values_cols[i]
      rare_values <- get_rare_values(data[, col], group_rare_values_limit[i])
      if (is.null(group_rare_values_text)){
        data[, col][data[, col] %in% rare_values] <- paste0(rare_values,
                                                            collapse = ", ")
      } else if (is.na(group_rare_values_text)) {
        data[, col][data[, col] %in% rare_values] <- NA
      } else {
        data[, col][data[, col] %in% rare_values] <- group_rare_values_text[i]
      }
    }
  }

  return(data)
}

deidentify_date <- function(data, date_aggregation) {
  data <- lubridate::floor_date(data, date_aggregation)
  return(data)
}

deidentify_group <- function(data,
                             group_rare_values_limit,
                             group_rare_values_text) {
  values_under_k_percent <-
    get_rare_values(data,
                    group_rare_values_limit)
  data[data %in% values_under_k_percent] <- group_rare_values_text
}

get_rare_values <- function(data,
                            k_percent = 5) {

  numeric_data <- is.numeric(data)
  values_by_percent <- table(data) / length(data[!is.na(data)]) * 100
  values_under_k_percent <- names(values_by_percent[values_by_percent < k_percent])

  if(numeric_data) {
    values_under_k_percent <- as.numeric(values_under_k_percent)
  }

  # Sorts alphabetically (or smallest to largest if numeric) for easier testing.
  values_under_k_percent <- sort(values_under_k_percent)
  # Returns NULL if no responses
  if (length(values_under_k_percent) == 0) {
    values_under_k_percent <- NULL
  }
  return(values_under_k_percent)
}


bin_numbers <- function(data, percent_per_group) {
  if (!is.numeric(percent_per_group) | length(percent_per_group) != 1 | !percent_per_group %in% 1:100) {
    stop("percent_per_group must be a single integer from 1:100.")
  }
  if (!is.numeric(data)) {
    stop("data must be a vector of numbers.")
  }

  # Convert to a proportion
  percent_per_group <- percent_per_group / 100

}
