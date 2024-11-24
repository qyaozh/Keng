test_that("power_lm", {
  out1 <- unlist(power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2, n = 193)[[1]])[3:8]
  out2 <- unlist(power_lm(PRE = calc_PRE(r_p = 0.2)$PRE, PC = 1, PA = 2)[[2]])[1:6]
  names(out1) <- NULL
  names(out2) <- NULL
  expect_equal(out1, out2)
})
