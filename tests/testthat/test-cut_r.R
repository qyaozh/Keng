test_that("cut_r return a data.frame", {
  expect_equal(nrow(cut_r(193)), 4)
})
