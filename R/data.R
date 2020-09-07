# load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1991_2018.rda")
# hate_crimes <- ucr_hate_crimes_1991_2018 %>%
#   dplyr::select(
#     ori,
#     hate_crime_incident_present_flag,
#     incident_date,
#     population,
#     agency_name,
#     unique_id,
#     ucr_offense_code_1,
#     bias_motivation_offense_1
#   ) %>%
#   dplyr::filter(hate_crime_incident_present_flag %in% "one or more hate crime incidents present") %>%
#   dplyr::sample_n(1000) %>%
#   dplyr::select(-hate_crime_incident_present_flag) %>%
#   dplyr::mutate(incident_date = lubridate::ymd(incident_date))
# usethis::use_data(hate_crimes, overwrite = TRUE)


#' A random selection of 1,000 hate crimes in the United States
#'
#' A dataset containing details about 1,000 hate crimes in the United States.
#'
#' @format A data frame with 1000 rows and 7 variables:
#' \describe{
#'   \item{ori}{A unique identifier code for the police agency}
#'   \item{incident_date}{The date of the incident}
#'   \item{population}{The population of the city where the police agency has jurisdiction (generally the city or county population)}
#'   \item{agency_name}{The name of the police agency}
#'   \item{unique_id}{A unique identifier for the incident}
#'   \item{ucr_offense_code_1}{The crime that occurred (e.g. murder)}
#'   \item{bias_motivation_offense_1}{The type of hate crime (e.g. anti-Black)}
#'   ...
#' }
"hate_crimes"
