test_that("compare_lm", {
  # test 1
  x1 <- rnorm(193)
  x2 <- rnorm(193)
  y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
  dat <- data.frame(y, x1 + x2)
  fit1 <- lm(y ~ 1, dat)
  fit2 <- lm(y ~ x1, dat)

  expect_equal(anova(fit1, fit2)["2", "F"], compare_lm(fit1, fit2)["F(PA-PC,n-PA)", "A vs. C"])

  n = 193
  PC = 1
  PA = 2
  SSEC = sum(residuals(fit1)^2)
  SSEA = sum(residuals(fit2)^2)

  expect_equal(compare_lm(fit1, fit2)[5, 3], compare_lm(fit1, fit2)[8, 3])
  expect_equal(compare_lm(fit1, fit2)[6, 3], compare_lm(fit1, fit2)[6, 4])
  expect_equal(compare_lm(fit1, fit2)[8, 3], compare_lm(fit1, fit2)[8, 4])
  expect_equal(compare_lm(fit1, fit2)[9, 3], compare_lm(fit1, fit2)[9, 4])
  expect_equal(compare_lm(fit1, fit2)[10, 3], compare_lm(fit1, fit2)[10, 4])
  expect_equal(compare_lm(fit1, fit2)[11, 3], compare_lm(fit1, fit2)[11, 4])
  expect_equal(compare_lm(fit1, fit2)[12, 3], compare_lm(fit1, fit2)[12, 4])
  expect_equal(compare_lm(fit1, fit2)[-c(5, 7), 4], compare_lm(n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA)[-c(5, 7), 4])

  fit3 <- lm(y ~ x1 + x2, dat)
  expect_equal(compare_lm(fit1, fit3)[5, 3], compare_lm(fit1, fit3)[8, 4])

  # test 2, error, lack arguments
  expect_error(compare_lm(fit1))
  expect_error(compare_lm(PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA))

  # test 3, error, redundant arguments
  expect_error(compare_lm(fit1, n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA))
  expect_error(compare_lm(fit1, fit2, n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA))

  # test 4, error, model C must has less parameters
  expect_error(compare_lm(fit2, fit1))

  # test 5, error if n < 1
  expect_error(compare_lm(n = 0, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA))

  # test 6, non-integer n, PC, and PA would be truncated towards zero.
  expect_equal(compare_lm(n = 12.6, PC = 2.6, PA = 4.6, SSEC = SSEC, SSEA = SSEA)[2, 1], 12)

  # test 7, error if PC > PA
  expect_error(compare_lm(n = n, PC = 3, PA = 2, SSEC = SSEC, SSEA = SSEA))

  # test 7, error if SSEC < SSEA
  expect_error(compare_lm(n = n, PC = PC, PA = PC, SSEC = 10, SSEA = 9))
})
