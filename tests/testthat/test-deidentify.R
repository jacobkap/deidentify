
test_that("Identify rare values (less thank k% of the non-NA data) works", {
  expect_equal(get_rare_values(mtcars$am), NULL)

  expect_equal(get_rare_values(c(1, 1, 3, 4), 50),  3:4)
  expect_equal(get_rare_values(mtcars$carb, 5), c(6, 8))
  expect_equal(get_rare_values(c("cat", "cat", "dog",
                                 "cat", "puppy", "puppy"), 20),
               "dog")
  expect_equal(get_rare_values(c("cat", "cat", "dog",
                                 "cat", "puppy", "puppy"), 35),
               c("dog", "puppy"))
  expect_equal(get_rare_values(mtcars$disp, 3), NULL)
  expect_equal(get_rare_values(mtcars$disp, 4),
               c(71.1, 75.7, 78.7, 79, 95.1, 108, 120.1, 120.3, 121,
                 140.8, 145, 146.7, 225, 258, 301, 304, 318, 350, 351,
                 400, 440, 460, 472))
})


test_that("date aggregate works", {
  expect_equal(deidentify_data(example, date_aggregation = "month", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-08-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(example, date_aggregation = "bimonth", quiet = TRUE)$dates, as.Date(c("1996-3-01", "2012-07-01", "2020-01-01", "2020-03-01")))
  expect_equal(deidentify_data(example, date_aggregation = "quarter", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-07-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(example, date_aggregation = "halfyear", quiet = TRUE)$dates, as.Date(c("1996-01-01", "2012-07-01", "2020-01-01", "2020-01-01")))
  expect_equal(deidentify_data(example, date_aggregation = "year", quiet = TRUE)$dates,
               as.Date(c("1996-01-01", "2012-01-01", "2020-01-01", "2020-01-01")))

  expect_equal(deidentify_data(tib_example, date_aggregation = "month", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-08-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(tib_example, date_aggregation = "bimonth", quiet = TRUE)$dates, as.Date(c("1996-3-01", "2012-07-01", "2020-01-01", "2020-03-01")))
  expect_equal(deidentify_data(tib_example, date_aggregation = "quarter", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-07-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(tib_example, date_aggregation = "halfyear", quiet = TRUE)$dates, as.Date(c("1996-01-01", "2012-07-01", "2020-01-01", "2020-01-01")))
  expect_equal(deidentify_data(tib_example, date_aggregation = "year", quiet = TRUE)$dates,
               as.Date(c("1996-01-01", "2012-01-01", "2020-01-01", "2020-01-01")))

  expect_equal(deidentify_data(dt_example, date_aggregation = "month", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-08-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(dt_example, date_aggregation = "bimonth", quiet = TRUE)$dates, as.Date(c("1996-3-01", "2012-07-01", "2020-01-01", "2020-03-01")))
  expect_equal(deidentify_data(dt_example, date_aggregation = "quarter", quiet = TRUE)$dates, as.Date(c("1996-04-01", "2012-07-01", "2020-01-01", "2020-04-01")))
  expect_equal(deidentify_data(dt_example, date_aggregation = "halfyear", quiet = TRUE)$dates, as.Date(c("1996-01-01", "2012-07-01", "2020-01-01", "2020-01-01")))
  expect_equal(deidentify_data(dt_example, date_aggregation = "year", quiet = TRUE)$dates,
               as.Date(c("1996-01-01", "2012-01-01", "2020-01-01", "2020-01-01")))


})

# test_that("tibbles and data.tables encrypt characters in the same way as data.frames",{
#   expect_equivalent(
#     deidentify_data(
#       tib_example,
#       cols_to_encrpyt = "groups",
#       seeds_for_encryption = 1e4,
#       quiet = T,
#       date_aggregation = "month"
#     ),
#     deidentify_data(
#       example,
#       cols_to_encrpyt = "groups",
#       seeds_for_encryption = 1e4,
#       quiet = T,
#       date_aggregation = "month"
#     ))
#     expect_equivalent(
#       deidentify_data(
#         dt_example,
#         cols_to_encrpyt = "groups",
#         seeds_for_encryption = 1e4,
#         quiet = T,
#         date_aggregation = "month"
#       ),
#       deidentify_data(
#         example,
#         cols_to_encrpyt = "groups",
#         seeds_for_encryption = 1e4,
#         quiet = T,
#         date_aggregation = "month"
#       )
#   )
# })
