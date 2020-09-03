# data <- mtcars
# columns <- c("cyl", "vs", "am", "gear")
#
# make_case_score(data, c("cyl", "vs", "am", "gear"))
#' Title
#'
#' @param data
#' The data.frame with the data you want to make the case score from.
#' @param columns
#' A string or vector of strings for the names of columns to group the data by.
#' @param quiet
#' A Boolean for whether you want to output a message that tells you if
#' there are groups with 3 or fewer observations (default is FALSE meaning
#' that it is NOT quiet, and thus will give the message).
#'
#' @return
#' A data.frame
#' @export
#'
#' @examples
#' make_case_score(mtcars, columns = c("cyl", "vs", "am", "gear"))
make_case_score <- function(data, columns, quiet = FALSE) {
  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(columns))) %>%
    dplyr::summarize(number_of_observations = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(number_of_observations)

  if (!quiet) {
    if (any(data$number_of_observations  < 3)) {
      message(paste0("Note: There are ",
                     sum(data$number_of_observations <= 3),
                     " groups with three or fewer observations."))
    }
  }
  return(data)
}
