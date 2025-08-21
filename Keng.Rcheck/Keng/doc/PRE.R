## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Keng)
library(effectsize)
library(car)
data("depress")

## -----------------------------------------------------------------------------
# factor gender and class
depress_factor <- depress
depress_factor$class <- factor(depress_factor$class, labels = c(3,5,9,12))
depress_factor$gender <- factor(depress_factor$gender, labels = c(0,1))

anova.fit <- lm(dm1 ~ gender + class, depress_factor)
Anova(anova.fit, type = 3)
cat("\n\n")
print(eta_squared(Anova(anova.fit, type = 3), partial = TRUE), digits = 6)

## -----------------------------------------------------------------------------
# class3 indicates whether the class is class3 
depress$class3 <- ifelse(depress$class == 3, 1, 0)
# class5 indicates whether the class is class5 
depress$class5 <- ifelse(depress$class == 5, 1, 0)
# class9 indicates whether the class is class9 
depress$class9 <- ifelse(depress$class == 9, 1, 0)

## -----------------------------------------------------------------------------
fitC <- lm(dm1 ~ class3 + class5 + class9, depress)
fitA <- lm(dm1 ~ class3 + class5 + class9 + gender, depress)
print(compare_lm(fitC, fitA), digits = 3)

## -----------------------------------------------------------------------------
fitC <- lm(dm1 ~ gender, depress)
fitA <- lm(dm1 ~ class3 + class5 + class9 + gender, depress)
print(compare_lm(fitC, fitA), digits = 3)

## -----------------------------------------------------------------------------
fitC <- lm(dm1 ~ 1, depress)
fitA <- lm(dm1 ~ class3 + class5 + class9 + gender, depress)
print(compare_lm(fitC, fitA), digits = 3)

