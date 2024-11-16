test_that("center", {
  x <- rnorm(193)
  expect_equal(center(x), (x - mean(x)))
})
