test_that("pick_dcb", {
  expect_equal(length(pick_dcb(10, verbose = FALSE)[[1]]), 7)
})
