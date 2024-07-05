library(testthat)

# Test Case 1: Basic Addition
test_that("add_two_numbers works correctly", {
  result <- add_two_numbers(3, 5)
  expect_equal(result, 8)
})

# Test Case 2: Negative Numbers
test_that("add_two_numbers handles negative numbers", {
  result <- add_two_numbers(-2, 4)
  expect_equal(result, 2)
})

# Test Case 3: Edge Case (Zero)
test_that("add_two_numbers handles zero", {
  result <- add_two_numbers(0, 10)
  expect_equal(result, 10)
})

# Test Case 4: Input Validation
test_that("add_two_numbers throws an error on invalid input", {
  expect_error(add_two_numbers("a", 5))
  expect_error(add_two_numbers(3, "b"))
})