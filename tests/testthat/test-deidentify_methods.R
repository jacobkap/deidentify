# check for errors
test_that("Errors are working as expected", {
  expect_error(deidentify_text("cat"), regexp = "Key must be provided*")
  expect_error(deidentify_text("cat",cols_to_encrypt = "t"),regexp = "Key must be provided*")
  expect_error(deidentify_text("cat",key = "1"),"key must be a cyphr key*")
  expect_error(deidentify_text(data.frame("dog"),gen_aes_key(16)),"* \"cols_to_encrypt\" is missing*")
  expect_error(deidentify_text(data.frame(c1 = "dog"),gen_aes_key(16),"c"),"You have selected*")
})


# check that things are working correctly
test_that("Check to work that things are working as expected", {
  # a 24 bit aes key will generate a 64 character string
  expect_equal(nchar(deidentify_text("cat", gen_aes_key(24))), 64)
  # a length n character string returns the same length output
  expect_length(deidentify_text(rep("cat", 100), gen_aes_key()), 100)
  # checking that strings and data.frames return objects of the same class.
  expect_type(deidentify_text("cat", gen_aes_key()), "character")
  expect_type(
    deidentify_text(data.frame(col1 = "cat"), gen_aes_key(), cols_to_encrypt = "col1"),
    "list"
  )
  # ensure that keys that are encrypted with different keys are different
  expect_equal(deidentify_text("cat", gen_aes_key(16)) != deidentify_text("cat", gen_aes_key(24)), TRUE)
  # data.frame checks
  expect_named(deidentify_text(data.frame(col1 = "cat"), gen_aes_key(), cols_to_encrypt = "col1"))
  expect_equal(dim(deidentify_text(data.frame(col1 = "cat"), gen_aes_key(), cols_to_encrypt = "col1")), c(1, 2))
})

