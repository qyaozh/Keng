test_that("compare_lm", {
  x1 <- rnorm(193)
  x2 <- rnorm(193)
  y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
  dat <- data.frame(y, x1 + x2)
  fit1 <- lm(y ~ 1, dat)
  fit2 <- lm(y ~ x1, dat)

  expect_equal(anova(fit1, fit2)[2,"F"], compare_lm(fit1, fit2)[2,"F"])

  n = 193
  PC = 1
  PA = 2
  SSEC = sum(fit1$residuals^2)
  SSEA = sum(fit2$residuals^2)
  expect_equal(compare_lm(fit1, fit2)[,1:7], compare_lm(n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA)[,1:7])

  fit3 <- lm(y ~ x1 + x2, dat)
  expect_equal(compare_lm(fit1, fit3)[2,"PRE"], compare_lm(fit1, fit3)[2, "R_Square"])
  expect_false(compare_lm(fit2, fit3)[2,"PRE"] == compare_lm(fit2, fit3)[2, "R_Square"])
})
