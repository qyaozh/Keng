## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Keng)
data(depress)

## -----------------------------------------------------------------------------
# multiple regression
fitC <- lm(dm1 ~ em1 + am1, depress)
fitA <- lm(dm1 ~ pm1 + em1 + am1, depress)
summary(fitA)

## -----------------------------------------------------------------------------
anova(fitC, fitA)

## -----------------------------------------------------------------------------
print(compare_lm(fitC, fitA), digits = 3)

## -----------------------------------------------------------------------------
dm_res <- lm(dm1 ~ em1 + am1, depress)$residuals
pm_res <- lm(pm1 ~ em1 + am1, depress)$residuals
resDat <- data.frame(dm_res, pm_res)
cor(dm_res, pm_res)

## -----------------------------------------------------------------------------
summary(lm(dm_res ~ pm_res, data.frame(dm_res, pm_res)))

## -----------------------------------------------------------------------------
fitC <- lm(dm_res ~ 1, resDat)
fitA <- lm(dm_res ~ pm_res, resDat)
print(compare_lm(fitC, fitA), digits = 3)

