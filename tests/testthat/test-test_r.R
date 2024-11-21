test_that("test_r return a data.frame", {
  expect_lt(
    abs(test_r(0.2, 100)[[1]][4]-test_r(0.2, 100)[[2]][4]), 0.001)
})
