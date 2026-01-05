test_that("divide() differs from cut()", {
  x <- seq(0, 100, 0.01)
  expect_true(is.ordered(divide(x)))
})
