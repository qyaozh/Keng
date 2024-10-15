test_that("test_PRE", {
  x <- rnorm(193)
  y <- 0.3 + 0.2*x + rnorm(193)
  dat <- data.frame(y, x)
  fitC <- lm(y ~ 1, dat)
  fitA <- lm(y ~ x, dat)
  expect_equal(anova(fitC, fitA)[2,"F"], test_PRE(fitC, fitA)$F)
  n = 193
  PC = 1
  PA = 2
  SSEC = sum(fitC$residuals^2)
  SSEA = sum(fitA$residuals^2)
  expect_equal(test_PRE(fitC, fitA), test_PRE(n = n, PC = PC, PA = PA, SSEC = SSEC, SSEA = SSEA))
})
