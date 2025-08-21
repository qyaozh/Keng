test_that("power_lm", {
  # test 1
  out1 <- power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2, n = 193)$F_test
  out2 <- unlist(power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2)$minimum)
  expect_equal(out1, out2, ignore_attr = TRUE)

  # test 2, power_lm, compare_lm
  data(depress)
  fit1 <- lm(dm1 ~ pm1, depress)
  fit2 <- lm(dm1 ~ pm1 + em1, depress)
  expect_equal(
    power_lm(PRE = compare_lm(fit1, fit2)[8, 4], PC = 2, PA = 3, n = nrow(depress))$F_test["F"],
    compare_lm(fit1, fit2)[9, 4],
    ignore_attr = TRUE)

  # test 3, error if PRE <= 0
  expect_error(power_lm(0))
  expect_error(power_lm(-0.1))

  # test 4, error if PC > PA
  expect_error(power_lm(PC = 2, PA = 1))

  # test 5, error, power and power.ul
  expect_error(power_lm(power = -0.1))
  expect_error(power_lm(power = 1.1)) # power.ul >= power is not TRUE
  expect_error(power_lm(power = 1.1, power.ul = 1.1)) # power <= 1 is not TRUE
  expect_error(power_lm(power.ul = 1.1)) # power.ul <= 1 is not TRUE

  # test 6, error if n <= PA
  expect_error(power_lm(PC = 0, PA = 1, n = 1))
  expect_error(power_lm(PA = 10, n = 10))

  # test 7, error of PRE
  expect_error(power_lm(PRE = 0))
  expect_error(power_lm(PRE = 1))

  # test 8, error if not identical(n, integer(0)) || n > PA
  expect_error(power_lm(n > 0))

  # test 9, error of n.ul
  expect_error(power_lm(PC = 0, PA = 1, n.ul = -1))
  expect_error(power_lm(PC = 0, PA = 1, n.ul = 0))
  expect_error(power_lm(PC = 0, PA = 1, n.ul = 1))
})
