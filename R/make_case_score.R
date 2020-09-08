# data <- mtcars
# columns <- c("cyl", "vs", "am", "gear")
#
# make_case_score(data, c("cyl", "vs", "am", "gear"))

#' Make case score out of de-identified data
#'
#' This gives you a data.frame with rows grouped based on the columns
#' parameter and tell you how many unique values are in each group.
#'
#' @param data
#' The data.frame with the data you want to make the case score from.
#' @param columns
#' A string or vector of strings for the names of columns to group the data by.
#' @param quiet
#' A Boolean (default FALSE) for whether you want to output a message that tells you if
#' there are groups with too few observations - you can set how many is 'too few'
#' in the paramter `minimum_case_score`, default is 3.
#' @param minimum_case_score
#' A single integer which sets the number of observations in each group that will
#' message (if `quiet` is FALSE) saying how many groups have fewer observations below
#' that group.
#'
#' @return
#' A data.frame
#' @export
#'
#' @examples
#' make_case_score(mtcars, columns = c("cyl", "vs", "am", "gear"))
make_case_score <- function(data,
                            columns,
                            quiet = FALSE,
                            minimum_case_score = 3) {

  if (!is.numeric(minimum_case_score) | length(minimum_case_score) != 1) {
    stop("minimum_case_score must be a single positive integer.")
  }
  if (minimum_case_score <= 0) {
    stop("minimum_case_score must be a single positive integer.")
  }
  if (!is.vector(quiet) | length(quiet)  != 1) {
    stop("quiet by be a either TRUE or FALSE")
  }
  if (!quiet %in% c(TRUE, FALSE)) {
    stop("quiet by be a either TRUE or FALSE")
  }
  # Tibbles and data.tables also return positive for as.data.table
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  if (!is.character(columns) | length(columns) < 1) {
    stop("columns must be a vector of strings with the names of the columns you want
         to group the data by.")
  }
  if (any(!columns %in% names(data))) {
    missing_columns <- columns[!columns %in% names(data)]
    stop(paste0("The following columns are not in the inputted dataset: ",
                missing_columns))
  }


  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(columns))) %>%
    dplyr::summarize(number_of_observations = dplyr::n(), .groups = "keep")
  data <- dplyr::arrange(data, data$number_of_observations)

  if (!quiet) {
    if (any(data$number_of_observations  < 3)) {
      message(paste0("Note: There are ",
                     sum(data$number_of_observations <= 3),
                     " groups with ", minimum_case_score,
                     " or fewer observations."))
    }
  }
  data <- data.frame(data)
  return(data)
}
