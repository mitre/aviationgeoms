context("package name tests")

test_that("package is named correctly", {
  DESC_path <- ifelse(file.exists('DESCRIPTION'), 'DESCRIPTION', '../../DESCRIPTION')
  expect_true(read.dcf(DESC_path)[,"Package"]=="aviationgeoms")
})
