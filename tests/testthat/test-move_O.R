test_that("move_O", {
  x <- rnorm(193)
  expect_equal(move_O(x), (x - mean(x)))
})
