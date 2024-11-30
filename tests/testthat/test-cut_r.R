test_that("cut_r return a data.frame", {
  expect_error(cut_r(2))
  expect_error(cut_r(2.6))
  expect_equal(nrow(cut_r(193)), 4)
})
