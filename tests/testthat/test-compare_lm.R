test_that("compare_lm", {
  x1 <- rnorm(193)
  x2 <- rnorm(193)
  y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
  dat <- data.frame(y, x1 + x2)
  fit1 <- lm(y ~ 1, dat)
  fit2 <- lm(y ~ x1, dat)

  expect_equal(anova(fit1, fit2)[2, 5], compare_lm(fit1, fit2)[8, 4])

  n = 193
  PC = 1
  PA = 2
  SSEC = sum(residuals(fit1)^2)
  SSEA = sum(residuals(fit2)^2)

  expect_equal(compare_lm(fit1, fit2)[4, 3], compare_lm(fit1, fit2)[7, 3])
  expect_equal(compare_lm(fit1, fit2)[5, 3], compare_lm(fit1, fit2)[5, 4])
  expect_equal(compare_lm(fit1, fit2)[7, 3], compare_lm(fit1, fit2)[7, 4])
  expect_equal(compare_lm(fit1, fit2)[8, 3], compare_lm(fit1, fit2)[8, 4])
  expect_equal(compare_lm(fit1, fit2)[9, 3], compare_lm(fit1, fit2)[9, 4])
  expect_equal(compare_lm(fit1, fit2)[10, 3], compare_lm(fit1, fit2)[10, 4])
  expect_equal(compare_lm(fit1, fit2)[11, 3], compare_lm(fit1, fit2)[11, 4])
  expect_equal(compare_lm(fit1, fit2)[-c(4, 6), 4], compare_lm(n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA)[-c(4, 6), 4])

  fit3 <- lm(y ~ x1 + x2, dat)
  expect_equal(compare_lm(fit1, fit3)[4, 3], compare_lm(fit1, fit3)[7, 4])
})
