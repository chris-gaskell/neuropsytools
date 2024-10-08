library(testthat)
library(neuropsytools)

test_that("percentile function returns correct generate_percentile", {
  data <- c(1, 2, 3, 4, 5)
  # Test percentile method 'c'
  expect_equal(percentile(data, 3, method = "c"), 50)

  # Test percentile method 'a'
  expect_equal(percentile(data, 3, method = "a"), 40)

  # Test percentile method 'b'
  expect_equal(percentile(data, 3, method = "b"), 60)
})
