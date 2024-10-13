test_that("test_r return a data.frame", {
  expect_equal(nrow(test_r(0.2, 193)), 1)
})
