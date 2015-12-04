context("nested-directories")

test_that("nest_dir creates a nested directory structure", {
  expect_equal(class(nest_dir(".")), "list")
})

