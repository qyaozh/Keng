test_that("test_r", {
  r <- runif(1)
  out <- test_r(r, 100)
  expect_lt(abs(out$t_test["p_r"] - out$Fisher_z["p_fz"]), 0.002)
  expect_equal(out$t_test["p_r"], powered_lm(r^2, n = 100)$p, ignore_attr = TRUE)
})
