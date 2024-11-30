test_that("power_lm", {
  # test 1
  out1 <- unlist(power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2, n = 193)$F_test)
  out2 <- unlist(power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2)$minimum)
  expect_equal(out1, out2, ignore_attr = TRUE)

  # test 2, error if PRE <= 0
  expect_error(power_lm(0))
  expect_error(power_lm(-0.1))

  # test 3, error if PC > PA
  expect_error(power_lm(PC = 2, PA = 1))

  # test 4, error, power
  expect_error(power_lm(power = -0.1))
  expect_error(power_lm(power = 1.1)) # power.ul >= power is not TRUE
  expect_error(power_lm(power = 1.1, power.ul = 1.1)) # power <= 1 is not TRUE
  expect_error(power_lm(power.ul = 1.1)) # power.ul <= 1 is not TRUE

  # test 5, error n <= PA
  expect_error(power_lm(PC = 0, PA = 1, n = 1))
  expect_error(power_lm(PA = 10, n = 10))
})
