test_that("assess_coa", {
  set.seed(20250902)
  data <- data.frame(
    session1 = 60 + sample.int(40, 100, 1),
    session2 = 60 + sample.int(40, 100, 1),
    session3 = 60 + sample.int(40, 100, 1)
  )
  session_weights    <- c(0.2, 0.3, 0.5)
  objective_weights1 <- c(0.1, 0.4, 0.5)
  objective_weights2 <- c(0.2, 0.2, 0.6)
  objective_weights3 <- c(0.3, 0, 0.7)
  coa <- assess_coa(data,
                    session_weights,
                    objective_weights1,
                    objective_weights2,
                    objective_weights3
                    )
  expect_true(is.data.frame(coa))
  expect_true(is.list(attr(coa, "weights")))
})
