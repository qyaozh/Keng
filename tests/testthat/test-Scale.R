test_that("Scale", {
  x <- rnorm(10)
  expect_equal(Scale(x), (x - mean(x))) # mean-centering
  expect_equal(Scale(x, expected_SD = 1), (x - mean(x))/sd(x)) # z-score
  expect_equal(Scale(x), as.vector(scale(x, scale = FALSE))) # mean-centering
  expect_equal(Scale(x, expected_SD = 1), as.vector(scale(x))) # z-score
  expect_equal(mean(Scale(x, expected_M = 100, expected_SD = 15)), 100) # M of standardized score
  expect_equal(sd(Scale(x, expected_M = 100, expected_SD = 15)), 15) # SD of standardized score
  expect_equal(Scale(x, oadvances = 2), Scale(x, expected_M = mean(x) - 2))
})
