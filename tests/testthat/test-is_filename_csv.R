test_that("is_filename_csv works", {
  expect_true(is_filename_csv("test.csv"))
  expect_true(is_filename_csv("test.CSV"))
  expect_true(is_filename_csv("test.CsV"))
  expect_true(is_filename_csv("a/b/c/test.csv"))
  expect_false(is_filename_csv("test.txt"))

})
