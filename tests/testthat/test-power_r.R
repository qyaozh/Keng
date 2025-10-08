test_that("power_r", {
  # test 1
  ## Due to precision loss, random r may yield different minimum n in power_lm()
  ## and power_r() accidentally when r is small (e.g., r = 0.0025).
  ## Hence fixed r is used here.
  r <- 0.123
  expect_equal(
    power_lm(r^2, 1, 2)$minimum$n_i, power_r(r)$minimum$n_i)

  # test 2, error, r
  expect_error(power_r(r = -2))
  expect_error(power_r(r = -1))
  expect_error(power_r(r = 0))
  expect_error(power_r(r = 1))
  expect_error(power_r(r = 2))

  # test 3, NA
  expect_error(power_r(r = NA))
})
