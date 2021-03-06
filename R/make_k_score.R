# data <- mtcars
# columns <- c("cyl", "vs", "am", "gear")
#
# make_k_score(data, c("cyl", "vs", "am", "gear"))

#' Make k score out of de-identified data
#'
#' This gives you a data.frame with rows grouped based on the columns
#' parameter and tell you how many unique values are in each group.
#'
#' @param data
#' The data.frame with the data you want to make the k score from.
#' @param columns
#' A string or vector of strings for the names of columns to group the data by.
#' @param quiet
#' A Boolean (default FALSE) for whether you want to output a message that tells you if
#' there are groups with too few observations - you can set how many is 'too few'
#' in the parameter `minimum_k_score`, default is 3.
#' @param minimum_k_score
#' A single integer which sets the number of observations in each group that will
#' message (if `quiet` is FALSE) saying how many groups have fewer observations below
#' that group.
#'
#' @export
#' @return
#' A data.frame
#' @examples
#' make_k_score(mtcars, columns = c("cyl", "vs", "am", "gear"))
make_k_score <- function(data,
                         columns = NULL,
                         quiet = FALSE,
                         minimum_k_score = 3) {

  if (!is.numeric(minimum_k_score) | length(minimum_k_score) != 1) {
    stop("minimum_k_score must be a single positive integer.")
  }
  if (minimum_k_score <= 0) {
    stop("minimum_k_score must be a single positive integer.")
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
  if (is.null(columns)) {
    columns <- names(data)
  }
  if (!is.character(columns) | length(columns) < 1) {
    stop("columns must be a vector of strings with the names of the columns you want
         to group the data by.")
  }
  if (any(!columns %in% names(data))) {
    missing_columns <- columns[!columns %in% names(data)]
    stop(paste0("The following columns are not in the inputted dataset: ",
                paste(missing_columns, collapse = " ")))
  }



  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(columns))) %>%
    dplyr::summarize(number_of_observations = dplyr::n(), .groups = "keep")
  data <- dplyr::arrange(data, data$number_of_observations)

  if (!quiet) {
    if (any(data$number_of_observations  < minimum_k_score)) {
      message(paste0("Note: There are ",
                     sum(data$number_of_observations <= minimum_k_score),
                     " groups with ", minimum_k_score,
                     " or fewer observations."))
    }
  }
  data <- data.frame(data)
  return(data)
}
