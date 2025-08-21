test_that("test_r", {
  r <- runif(1)
  out <- test_r(r, 100)
  expect_lt(
    abs(out$t_test["p_r"] - out$Fisher_z["p_fz"]), 0.002)

  p_r <- out$t_test["p_r"]
  p <- power_lm(r^2, n = 100)$F_test["p"]
  expect_equal(p_r, p, ignore_attr = TRUE)
})
