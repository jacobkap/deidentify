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
#' A Boolean for whether you want to output a message that tells you if
#' there are groups with 3 or fewer observations (default is FALSE meaning
#' that it is NOT quiet, and thus will give the message).
#' @return
#' A data.frame
#' @examples
#' \dontrun{
#' make_case_score(mtcars, columns = c("cyl", "vs", "am", "gear"))
#' }
#' @export
make_case_score <- function(data, columns, quiet = FALSE) {
  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(columns))) %>%
    dplyr::summarize(number_of_observations = dplyr::n(), .groups = "keep")
  data <- dplyr::arrange(data, data$number_of_observations)

  if (!quiet) {
    if (any(data$number_of_observations  < 3)) {
      message(paste0("Note: There are ",
                     sum(data$number_of_observations <= 3),
                     " groups with three or fewer observations."))
    }
  }
  data <- data.frame(data)
  return(data)
}
