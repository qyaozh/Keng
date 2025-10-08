test_that("power_r", {
  # test 1
  ## Powers of power_lm() and power_r() change at different pace against discrete sample size,
  ## which lead to different minimum n in power_lm() and power_r(). Hence fixed r is used here.
  r <- 0.2
  expect_equal(power_lm(r^2, 1, 2)$minimum$n_i, power_r(r)$minimum$n_i)

  # test 2, error, r
  expect_error(power_r(r = -2))
  expect_error(power_r(r = -1))
  expect_error(power_r(r = 0))
  expect_error(power_r(r = 1))
  expect_error(power_r(r = 2))

  # test 3, NA
  expect_error(power_r(r = NA))
})
