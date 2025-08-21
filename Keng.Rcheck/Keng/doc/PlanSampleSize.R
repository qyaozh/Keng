## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(Keng)
power_lm(PRE = 0.02, PC = 1, PA = 11)

## -----------------------------------------------------------------------------
library(Keng)
data("depress")
# WRONG PRE for the one-sample t-test
(Cohens_d <- effectsize::cohens_d(dm1 ~ 1, data = depress)$Cohens_d)
(r_from_d <- effectsize::d_to_r(Cohens_d, n1 = 94))
(PRE_from_d <- r_from_d^2)
# CORRECT PRE for the one-sample t-test
out <- t.test(dm1 ~ 1, depress)
(r_from_t <- effectsize::t_to_r(t = out$statistic, df_error = out$parameter)$r)
(PRE_from_t <- r_from_t^2)
# PRE from lm()
fit0 <- lm(dm1 ~ 0, depress)
fit1 <- lm(dm1 ~ 1, depress)
compare_lm(fit0, fit1)[7, 4]

