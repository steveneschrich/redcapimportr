test_that("is_filename_r works", {
  expect_true(is_filename_r("test.r"))
  expect_true(is_filename_r("test.R"))
  expect_true(is_filename_r("a/b/c/test.R"))
  expect_false(is_filename_r("test"))
})
