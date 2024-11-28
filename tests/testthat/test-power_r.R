test_that("power_r", {
  r <- runif(1)
  expect_equal(
    power_lm(r^2, 1, 2)$minimum$n_i, power_r(r)$minimum$n_i)
})
