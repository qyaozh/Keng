test_that("Scale", {
  x <- rnorm(10)
  expect_equal(Scale(x), (x - mean(x))) # mean-centering
  expect_equal(Scale(x, sd = 1), (x - mean(x))/sd(x)) # z-score
  expect_equal(Scale(x), as.vector(scale(x, scale = FALSE))) # mean-centering
  expect_equal(Scale(x, sd = 1), as.vector(scale(x))) # z-score
  expect_equal(mean(Scale(x, m = 100, sd = 15)), 100) # M of standardized score
  expect_equal(sd(Scale(x, m = 100, sd = 15)), 15) # SD of standardized score
  expect_equal(Scale(x, oadvances = 2), Scale(x, m = mean(x) - 2))
})
