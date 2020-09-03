# load("C:/Users/user/Dropbox/R_project/crime_data/clean_data/hate_crimes/ucr_hate_crimes_1991_2018.rda")
# deidentify_example <- ucr_hate_crimes_1991_2018 %>%
#   dplyr::select(
#     ori,
#     ori9,
#     hate_crime_incident_present_flag,
#     state,
#     incident_date,
#     year,
#     population,
#     agency_name,
#     city_name,
#     population_group,
#     country_division,
#     country_region,
#     incident_number,
#     unique_id,
#     total_num_of_individual_victims,
#     total_offenders,
#     offenders_race_as_a_group,
#     number_of_victims_offense_1,
#     ucr_offense_code_1,
#     bias_motivation_offense_1,
#     location_code_offense_1
#   ) %>%
#   dplyr::filter(hate_crime_incident_present_flag %in% "one or more hate crime incidents present") %>%
#   dplyr::sample_n(10000) %>%
#   dplyr::select(-hate_crime_incident_present_flag) %>%
#   dplyr::mutate(incident_date = lubridate::ymd(incident_date))
# usethis::use_data(deidentify_example, overwrite = TRUE)


#' A random selection of 10,000 hate crimes in the United States
#'
#' A dataset containing details about 10,000 hate crimes in the United States.
#'
#' @format A data frame with 10000 rows and 21 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
"deidentify_example"
