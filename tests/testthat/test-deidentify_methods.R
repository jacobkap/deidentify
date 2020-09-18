test_that("Errors are working as expected",{
  expect_error(deidentify_text("cat",cols_to_encrypt = "c"),"key must be a cyphr key")
  expect_error(deidentify_text(list(c = "cat"),cols_to_encrypt = "c",gen_aes_key()),"You must pass a dataframe")
  expect_error(deidentify_text(data.frame(c = "cat"),cols_to_encrypt = "c"),"key must be a cyphr key")
})
