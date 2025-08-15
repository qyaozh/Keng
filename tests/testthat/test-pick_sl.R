test_that("pick_sl", {
  expect_equal(length(pick_sl(10, verbose = FALSE)[[1]]), 7)
})
