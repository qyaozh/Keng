test_that("powered_lm", {
  expect_true(powered_lm()$power > 0.8)
})
