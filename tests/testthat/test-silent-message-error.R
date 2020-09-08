test_that("messages work as expected", {
  expect_message(make_case_score(example, columns = "groups"))
  expect_message(make_case_score(example, columns = "groups"),
                 "Note: There are 3 groups with 3 or fewer observations.")
  expect_message(make_case_score(example, minimum_case_score = 5, columns = "groups"),
                 "Note: There are 3 groups with 5 or fewer observations.")
})


test_that("errors work as expected", {
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = 0))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = -1))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = 1:4))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = NULL))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = NA))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = ""))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = "test"))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = mtcars))
  expect_error(make_case_score(example, columns = "groups", quiet = TRUE,
                               minimum_case_score = mtcars$cyl))

  expect_error(make_case_score(example, columns = "groups", quiet = 2))
  expect_error(make_case_score(example, columns = "groups", quiet = NULL))
  expect_error(make_case_score(example, columns = "groups", quiet = NA))
  expect_error(make_case_score(example, columns = "groups", quiet = "test"))
  expect_error(make_case_score(example, columns = "groups", quiet = ""))
  expect_error(make_case_score(example, columns = "groups", quiet = mtcars))
  expect_error(make_case_score(example, columns = "groups", quiet = mtcars$mpg))
  expect_error(make_case_score(example, columns = "groups", quiet = -1))
  expect_error(make_case_score(example, columns = "groups", quiet = 2:5))
  expect_error(make_case_score(example, columns = "groups", quiet = c(TRUE, FALSE)))
})
