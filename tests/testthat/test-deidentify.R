

test_that("Identify rare values (less thank k% of the non-NA data) works", {
  expect_equal(get_values_rarer_than_k_percent(mtcars$am), NULL)

  expect_equal(get_values_rarer_than_k_percent(c(1, 1, 3, 4), 50),  3:4)
  expect_equal(get_values_rarer_than_k_percent(mtcars$carb, 5), c(6, 8))
  expect_equal(get_values_rarer_than_k_percent(c("cat", "cat", "dog",
                                                 "cat", "puppy", "puppy"), 20),
               "dog")
  expect_equal(get_values_rarer_than_k_percent(c("cat", "cat", "dog",
                                                 "cat", "puppy", "puppy"), 35),
               c("dog", "puppy"))
  expect_equal(get_values_rarer_than_k_percent(mtcars$disp, 3), NULL)
  expect_equal(get_values_rarer_than_k_percent(mtcars$disp, 4),
               c(71.1, 75.7, 78.7, 79, 95.1, 108, 120.1, 120.3, 121,
                 140.8, 145, 146.7, 225, 258, 301, 304, 318, 350, 351,
                 400, 440, 460, 472))
})


test_that("date aggregate works", {
  expect_equal(deidentify_data(example, time_aggregation = "month", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-08-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(example, time_aggregation = "bimonth", quiet = TRUE)$dates, as.Date(c("1996-3-01", "2012-07-01", "2020-01-01", "2020-03-01")))
  expect_equal(deidentify_data(example, time_aggregation = "quarter", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-07-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(example, time_aggregation = "halfyear", quiet = TRUE)$dates, as.Date(c("1996-01-01", "2012-07-01", "2020-01-01", "2020-01-01")))
  expect_equal(deidentify_data(example, time_aggregation = "year", quiet = TRUE)$dates,
               as.Date(c("1996-01-01", "2012-01-01", "2020-01-01", "2020-01-01")))


})
