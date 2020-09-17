#' De-identify data through encrypting text columns, grouping rare values, and,
#' aggregating numeric or date columns.
#'
#'
#' @param data
#' A data.frame with the data you want to de-identify.
#' @param cols_to_encrypt
#' A string or vector of strings with the columns that you want to encrypt
#' using the `seed_cipher()` function from the `caesar` package.
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
#' threshold set in `group_rare_values_limit` to rename (default is just
#' calling them NA).
#' @param date_cols
#' A vector of strings with the name of date columns that you want to be aggregated
#' to the unit set in the `date_aggreagtion` parameter. If NULL, will use
#' all date columns in the data.
#' @param date_aggregation
#' A string with the time unit to aggregate all Date variables to.
#' Can take one of the following: 'week', 'month', 'bimonth', 'quarter',
#'  'halfyear', 'year'.
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
                            group_rare_values_limit = NULL,
                            group_rare_values_text = NA,
                            quiet = FALSE) {

  # if (!is.null(cols_to_encrypt) & is.null(seeds_for_encryption)) {
  #   seeds_for_encryption <- sample(1e3:1e11, length(cols_to_encrypt))
  # }

  data <-
    data %>%
    dplyr::mutate_if(lubridate::is.Date, lubridate::floor_date, unit = date_aggregation)


  # Looks in each selected columns and replaces all the values that are less
  # than k% of the non-NA total with a selected replacement text (default
  # is NA). This is to reduce privacy concerns with rare values (i.e. if
  # person is only one with X offense, can identify through that).
  group_rare_values_limit <- rep(group_rare_values_limit,
                                 length.out = length(group_rare_values_cols))
  if (!is.null(group_rare_values_cols)) {
    for (i in 1:length(group_rare_values_cols)) {
      col <- group_rare_values_cols[i]
      values_under_k_percent <-
        get_values_rarer_than_k_percent(data[, col],
                                        group_rare_values_limit[i])
      data[, col][data[, col] %in% values_under_k_percent] <- group_rare_values_text[i]
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
    get_values_rarer_than_k_percent(data,
                                    group_rare_values_limit)
  data[data %in% values_under_k_percent] <- group_rare_values_text
}

get_values_rarer_than_k_percent <- function(data,
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
