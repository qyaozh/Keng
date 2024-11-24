test_that("calc_PRE", {
  f_squared <- abs(rnorm(1))
  f <- sqrt(f_squared)
  PRE <- f_squared/(1 + f_squared)
  r_p <- sqrt(PRE)

  expect_equal(calc_PRE(f_squared = f_squared), calc_PRE(f = f))
  expect_equal(calc_PRE(f_squared = f_squared), calc_PRE(r_p = r_p))
})
