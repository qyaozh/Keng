test_that("compare_lm", {
  x1 <- rnorm(193)
  x2 <- rnorm(193)
  y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
  dat <- data.frame(y, x1 + x2)
  fit1 <- lm(y ~ 1, dat)
  fit2 <- lm(y ~ x1, dat)

  expect_equal(anova(fit1, fit2)[2, 5], compare_lm(fit1, fit2)[4, 7])

  n = 193
  PC = 1
  PA = 2
  SSEC = sum(residuals(fit1)^2)
  SSEA = sum(residuals(fit2)^2)

  expect_equal(compare_lm(fit1, fit2)[3, 3], compare_lm(fit1, fit2)[3, 6])
  expect_equal(compare_lm(fit1, fit2)[3, 4], compare_lm(fit1, fit2)[4, 4])
  expect_equal(compare_lm(fit1, fit2)[3, 6], compare_lm(fit1, fit2)[4, 6])
  expect_equal(compare_lm(fit1, fit2)[3, 7], compare_lm(fit1, fit2)[4, 7])
  expect_equal(compare_lm(fit1, fit2)[3, 8], compare_lm(fit1, fit2)[4, 8])
  expect_equal(compare_lm(fit1, fit2)[3, 9], compare_lm(fit1, fit2)[4, 9])
  expect_equal(compare_lm(fit1, fit2)[3, 10], compare_lm(fit1, fit2)[4, 10])
  expect_equal(compare_lm(fit1, fit2)[4, -c(3, 5)], compare_lm(n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA)[4, -c(3, 5)])

  fit3 <- lm(y ~ x1 + x2, dat)
  expect_equal(compare_lm(fit1, fit3)[3, 3], compare_lm(fit1, fit3)[4, 6])
})
