test_that("assess_coa", {
  set.seed(20250831)
  data <- data.frame(session1 = 60 + sample.int(40, 100, 1), session2 = 60 + sample.int(40, 100, 1))
  weight_sessions <- c(0.4, 0.6)
  weight_objectives1 <- c(0.5, 0.3, 0.2)
  weight_objectives2 <- c(0.1, 0.3, 0.6)
  coa <- assess_coa(
    data,
    weight_sessions,
    weight_objectives1,
    weight_objectives2)
  expect_true(is.data.frame(coa))
  expect_true(is.list(attr(coa, "weights")))
})
