test_that("powered_r", {
  expect_true(powered_r(0.2, 200)$power > 0.80)
})
