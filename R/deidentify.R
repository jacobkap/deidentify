
#' De-identify data through encrypting text columns, grouping rare values, and,
#' aggregating numeric or date columns.
#'
#'
#'
#' @param data
#' A data.frame with the data you want to deidentify.
#' @param cols_to_encrpyt
#' A string or vector of strings with the columns that you want to encrypt
#' using the `seed_cipher()` function from the `caesar` package.
#' @param seeds_for_encryption
#' A string or vector of strings with the columns that you want to use for
#' each column in the cols_to_encrpyt parameter. If NULL, will use random seeds
#' andwill print those seeds if the paramter quiet is set to TRUE.
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
#' @param time_aggregation
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
#' @export
#'
#' @examples
deidentify_data <- function(data,
                            cols_to_encrpyt = NULL,
                            seeds_for_encryption = NULL,
                            group_rare_values_cols = NULL,
                            group_rare_values_limit = NULL,
                            group_rare_values_text = NA,
                            time_aggregation = c("week",
                                                 "month",
                                                 "bimonth",
                                                 "quarter",
                                                 "halfyear",
                                                 "year"),
                            quiet = FALSE) {

  if (!is.null(cols_to_encrpyt) & is.null(seeds_for_encryption)) {
    seeds_for_encryption <- sample(1000:10000000, length(cols_to_encrpyt))
  }
  column_types <- apply(data, 2, typeof)

  data <-
    data %>%
    dplyr::mutate_if(lubridate::is.Date, lubridate::floor_date, unit = time_aggregation)


  # Looks in each selected columns and replaces all the values that are less
  # than k% of the non-NA total with a selected replacement text (default
  # is NA). This is to reduce privacy concerns with rare values (i.e. if
  # person is only one with X offense, can identify through that).
  if (!is.null(group_rare_values_cols)) {
    for (i in 1:length(group_rare_values_cols)) {
      col <- group_rare_values_cols[i]
      data[, col] <- get_values_less_than_k_percent(data,
                                                    col,
                                                    group_rare_values_limit[i],
                                                    group_rare_values_text[i])
    }
  }


  if (!is.null(cols_to_encrpyt)) {
    # Encrypts all of the columns that should be encrypted.
    for (i in 1:length(cols_to_encrpyt)) {
      col <- cols_to_encrpyt[i]
      data[, col] <- caesar::seed_cipher(data[, col], seed = seeds_for_encryption[i])
    }

    cols_and_seeds <- cols_to_encrpyt
    names(cols_and_seeds) <- seeds_for_encryption
    print(paste0("Below are the columns that you encrypted and the seed set for each column. Please keep a record of this so you can decrypt later."))
    print(cols_and_seeds)
  }

  return(data)
}

get_values_less_than_k_percent <- function(data,
                                           column,
                                           k_percent,
                                           replacement_text = NA) {
  unique_values <- unique(data[, col])
  unique_values <- unique_values[!is.na(unique_values)]

  values_by_percent <- table(data[, col]) / nrow(data[!is.na(data[, col]), ]) * 100
  values_under_k_percent <- names(values_by_percent[values_by_percent < 5])

  data[, col][data[, col] %in% values_under_k_percent] <- replacement_text

  return(data)
}



