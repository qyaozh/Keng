test_that("std", {
  x <- rnorm(193)
  expect_equal(std(x), (x - mean(x, na.rm = TRUE))/sd(x, na.rm = FALSE))
})
