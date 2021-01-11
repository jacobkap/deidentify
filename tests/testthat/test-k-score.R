test_that("Test k score", {
  expect_equal(make_k_score(example, columns = "groups", quiet = TRUE),
               data.frame(groups = c("dog", "puppy", "cat"),
                          number_of_observations = c(1, 1, 2)))
  expect_equal(min(make_k_score(example, columns = "groups", quiet = TRUE)$number_of_observations), 1)
  expect_equal(max(make_k_score(example, columns = "groups", quiet = TRUE)$number_of_observations), 2)

  expect_equal(make_k_score(example,
                            columns = c("dates", "dates2", "numbers", "groups"), quiet = TRUE), cbind(example, number_of_observations = c(1, 1, 1, 1)))

})
