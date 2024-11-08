#' Compare lm()'s fitted outputs using PRE and R-squared.
#'
#' @param fitC The result of `lm()` of the Compact model (Model C).
#' @param fitA The result of `lm()` of the Augmented model (Model A).
#' @param n Sample size of the Model C or Model A. Model C and Model A must use the same sample, and hence have the same sample size.
#' @param PC The number of parameters in Model C.
#' @param PA The number of parameters in Model A. PA must be larger than PC.
#' @param SSEC The Sum of Squared Errors (SSE) of Model C.
#' @param SSEA The Sum of Squared Errors of Model A.
#'
#' @details `compare_lm()` compare Model A with Model C using *PRE* (Proportional Reduction in Error) and R-squared. *PRE* is partial R-squared (called partial Eta-squared in Anova).There are two ways of using `compare_lm()`. The first is giving `compare_lm()` fitC and fitA. The second is giving *n*, *PC*, *PA*, *SSEC*, and *SSEA*. The first way is more convenient, and it minimizes precision loss by omitting copying-and-pasting. If fitC and fitA are not inferior to the intercept-only model, R-squared and Adjusted R-squared are also computed. Note that the *F*-tests for *PRE* and R-squared change are equivalent. Please refer to Judd et al. (2017) for more details about *PRE*.
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). *Data analysis: A model comparison approach to regression, ANOVA, and beyond*. Routledge.
#'
#' @return A data.frame with 3 rows and 8 columns. The first row reports information for Model C, the second for Model A, and the third for the change. The data.frame presents *SSE*, *df* of *SSE*, *PRE*, the *F*-test of *PRE* (*F*, *p*), and *PRE_adjusted*. If fitC and fitA are not inferior to the intercept-only model, R-squared and Adjusted R-squared will also be computed.
#' @export
#'
#' @examples
#' x1 <- rnorm(193)
#' x2 <- rnorm(193)
#' y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
#' dat <- data.frame(y, x1, x2)
#' # Fix intercept to constant 1 using I().
#' fit1 <- lm(I(y - 1) ~ 0, dat)
#' # Free intercept.
#' fit2 <- lm(y ~ 1, dat)
#' compare_lm(fit1, fit2)
#' # One predictor.
#' fit3 <- lm(y ~ x1, dat)
#' compare_lm(fit2, fit3)
#' # Fix intercept to 0.3 using offset().
#' intercept <- rep(0.3, 193)
#' fit4 <- lm(y ~ 0 + x1 + offset(intercept), dat)
#' compare_lm(fit4, fit3)
#' # Two predictors.
#' fit5 <- lm(y ~ x1 + x2, dat)
#' compare_lm(fit2, fit5)
#' compare_lm(fit3, fit5)
#' # Fix slope of x2 to 0.05 using offset().
#' fit6 <- lm(y ~ x1 + offset(0.05*x2), dat)
#' compare_lm(fit6, fit5)

compare_lm <- function(fitC=NULL, fitA=NULL, n=NULL, PC=NULL, PA=NULL, SSEC=NULL, SSEA=NULL) {
  if (!(
    (sum(sapply(list(fitC, fitA), is.null)) == 0) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 5) |
    (sum(sapply(list(fitC, fitA), is.null)) == 2) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 0)
    ))
    stop("Provide fitc and fitA, or provide n, PC, PA, SSEC, and SSEA instead.")
  else if ((sum(sapply(list(fitC, fitA), is.null)) == 0) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 5)) {

           # Given fitC and fitA, compute SSEC and SSEA
           n = length(fitC$residuals)
           if (n != length(fitA$residuals))
             stop("sample size of Model C must be equal to Model A.")
           PC = length(fitC$coefficients)
           PA = length(fitA$coefficients)
           if (PC >= PA)
             stop("Model C must has less parameters than Model A.")
           SSEC = sum((fitC$residuals)^2)
           SSEA = sum((fitA$residuals)^2)

           # mean model and R-squared
           SSE_MEAN = sum((fitC[["model"]][,1] - mean(fitC[["model"]][,1]))^2)
           R_squared_C = ifelse(SSE_MEAN >= SSEC, (SSE_MEAN - SSEC)/SSE_MEAN, NA)
           R_squared_adj_C = ifelse(SSE_MEAN >= SSEC, 1 - (SSEC/(n - PC))/(SSE_MEAN/(n - 1)), NA)
           R_squared_A = ifelse(SSE_MEAN >= SSEA, (SSE_MEAN - SSEA)/SSE_MEAN, NA)
           R_squared_adj_A = ifelse(SSE_MEAN >= SSEA, 1 - (SSEA/(n - PA))/(SSE_MEAN/(n - 1)), NA)
  }
  else {
    # fitC and fitA are not given, assign NAs to R-squared
    R_squared_C = NA
    R_squared_adj_C = NA
    R_squared_A = NA
    R_squared_adj_A = NA
  }

  # PRE
  PRE <- 1 - SSEA/SSEC
  F <- ((SSEC - SSEA)/(PA - PC))/(SSEA/(n - PA))
  p <- stats::pf(F, (PA - PC), (n - PA), lower.tail = FALSE)
  PRE_adj <- 1 - (1 - PRE)*((n - PC)/(n - PA))

  # Return
  as.data.frame(matrix(
    c(SSEC,         n - PC,  R_squared_C,               R_squared_adj_C, NA,  NA, NA, NA,
      SSEA,         n - PA,  R_squared_A,               R_squared_adj_A, NA,  NA, NA, NA,
      SSEC - SSEA,  PA - PC, R_squared_A - R_squared_C, NA,              PRE, F,  p,  PRE_adj),
    nrow = 3, ncol = 8, byrow = TRUE,
    dimnames = list(
      c("Model C", "Model A", "A vs. C"),
      c("SSE", "df", "R_squared", "R_squared_adj", "PRE", "F(PA-PC,n-PA)", "p", "PRE_adj")
    )))
}
