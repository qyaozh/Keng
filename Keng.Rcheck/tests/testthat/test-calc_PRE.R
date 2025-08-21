test_that("calc_PRE", {
  # test 1
  f_squared <- abs(rnorm(1))
  f <- sqrt(f_squared)
  PRE <- f_squared/(1 + f_squared)
  r_p <- sqrt(PRE)

  expect_equal(calc_PRE(f_squared = f_squared), calc_PRE(f = f))
  expect_equal(calc_PRE(f_squared = f_squared), calc_PRE(r_p = r_p))

  # test 2
  expect_error(calc_PRE(f = -10))
  expect_equal(calc_PRE(f = 0)$PRE, 0)
  expect_equal(calc_PRE(f = 1)$PRE, 0.5)

  # test 3
  expect_error(calc_PRE(f_squared = -10))
  expect_equal(calc_PRE(f_squared = 0)$PRE, 0)
  expect_equal(calc_PRE(f_squared = 9)$f, 3)

  # test 4
  expect_error(calc_PRE(r_p = -10))
  expect_equal(calc_PRE(r_p = 0)$PRE, 0)
  expect_equal(calc_PRE(r_p = 1)$PRE, 1)

  # test NA
  expect_error(calc_PRE(f = NA))
})
