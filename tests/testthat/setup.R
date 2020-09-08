example <- data.frame(dates = sort(as.Date(c("2020-04-15", "2012-08-07",
                                        "1996-04-24", "2020-01-22"))),
                      numbers = 1:4,
                      groups = c("cat", "cat", "dog", "puppy"))
tib_example <- dplyr::as_tibble(example)
dt_example <- data.table::as.data.table(example)
