test_that("PRE", {
  x <- rnorm(193)
  y <- 0.3 + 0.2*x + rnorm(193)
  dat <- data.frame(y, x)
  fitC <- lm(y ~ 1, dat)
  fitA <- lm(y ~ x, dat)
  expect_equal(anova(fitC, fitA)[2,"F"], PRE(fitC, fitA)$F)
})
