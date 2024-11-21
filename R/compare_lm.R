#' Compare lm()'s fitted outputs using PRE and R-squared.
#'
#' @param fitC The result of `lm()` of the Compact model (model C).
#' @param fitA The result of `lm()` of the Augmented model (model A).
#' @param n Sample size of the model C or model A. model C and model A must use the same sample, and hence have the same sample size.
#' @param PC The number of parameters in model C.
#' @param PA The number of parameters in model A. PA must be larger than PC.
#' @param SSEC The Sum of Squared Errors (SSE) of model C.
#' @param SSEA The Sum of Squared Errors of model A.
#'
#' @details `compare_lm()` compare model A with model C using *PRE* (Proportional Reduction in Error) , *R-squared*, *f_squared*, and post-hoc power.
#' *PRE* is partial R-squared (called partial Eta-squared in Anova).
#' There are two ways of using `compare_lm()`.
#' The first is giving `compare_lm()` fitC and fitA.
#' The second is giving *n*, *PC*, *PA*, *SSEC*, and *SSEA*.
#' The first way is more convenient, and it minimizes precision loss by omitting copying-and-pasting.
#' Note that the *F*-tests for *PRE* and R-squared change are equivalent.
#' Please refer to Judd et al. (2017) for more details about *PRE*, and refer to Aberson (2019) for more details about *f_squared* and post-hoc power.
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). *Data analysis: A model Comparison approach to regression, ANOVA, and beyond*. Routledge.
#'
#' Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' @return A matrix with 4 rows and 10 columns.
#' The first row reports information for baseline model (intercept-only model).
#' the second for model C, the third for model A, and the fourth for the change (model A vs. model C).
#' *SSE* (Sum of Squared Errors) and *df* of *SSE* for baseline model, model C,
#' model A, and change (model A vs. model C) are reported in column 1 and column 2.
#' The information in the fourth row are all for the change; put differently,
#' These results quantify the effect of new parameters model A has but model C doesn't.
#' If fitC and fitA are not inferior to the intercept-only model,
#' *R-squared*, *Adjusted R-squared*, *PRE*, *PRE_adjusted*, and *f_squared* for the full model
#' (compared with the baseline model) are reported for model C and model A.
#' If model C or model A has at least 1 predictor, *F* -test with *p*,
#' and post-hoc power would be computed for the corresponding full model.
#'
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
           n = length(stats::resid(fitC))
           if (n != length(stats::resid(fitA)))
             stop("sample size of model C must be equal to model A.")
           PC = length(stats::coef(fitC))
           PA = length(stats::coef(fitA))
           if (PC >= PA)
             stop("model C must has less parameters than model A.")
           SSEC = sum(stats::resid(fitC)^2)
           SSEA = sum(stats::resid(fitA)^2)

           # mean model And R-squared
           SSE_MEAN <- sum((
             stats::model.response(stats::model.frame(fitC)) - mean(stats::model.response(stats::model.frame(fitC)))
           ) ^ 2)

           if (SSE_MEAN >= SSEC) {
             PRE_C <- 1 - SSEC/SSE_MEAN
             PRE_C_adj <- 1 - (1 - PRE_C)*((n - 1)/(n - PC))

             R_squared_C <- (SSE_MEAN - SSEC)/SSE_MEAN
             R_squared_adj_C <- 1 - (SSEC/(n - PC))/(SSE_MEAN/(n - 1))

             f_squared_C <- SSE_MEAN/SSEC - 1
             lambda_C <- f_squared_C*(n - PC)

             if (PC - 1 > 0) {
               F_C <- ((SSE_MEAN - SSEC)/(PC - 1))/(SSEC/(n - PC))
               p_C <- stats::pf(F_C, (PC - 1), (n - PC), lower.tail = FALSE)

               power_post_C <- stats::pf(stats::qf(0.95, (PC - 1), (n - PC)), (PC - 1), (n - PC), lambda_C, lower.tail = FALSE)
             }
             else {
               F_C <- NA
               p_C <- NA
               power_post_C <- NA
             }
           }
           else {
             PRE_C <- NA
             PRE_C_adj <- NA
             F_C <- NA
             p_C <- NA
             R_squared_C <- NA
             R_squared_adj_C <- NA
             f_squared_C <- NA
             lambda_C <- NA
             power_post_C <- NA
           }

           if (SSE_MEAN >= SSEA) {
             PRE_A <- 1 - SSEA/SSE_MEAN
             PRE_A_adj <- 1 - (1 - PRE_A)*((n - 1)/(n - PA))

             R_squared_A <- (SSE_MEAN - SSEA)/SSE_MEAN
             R_squared_adj_A <- 1 - (SSEA/(n - PA))/(SSE_MEAN/(n - 1)) # PRE_adj <- 1 - (1 - PRE)*((n - PC)/(n - PA))

             f_squared_A <- SSE_MEAN/SSEA - 1
             lambda_A <- f_squared_A*(n - PA)

             if (PA - 1 > 0) {
               F_A <- ((SSE_MEAN - SSEA)/(PA - 1))/(SSEA/(n - PA))
               p_A <- stats::pf(F_A, (PA - 1), (n - PA), lower.tail = FALSE)
               power_post_A <- stats::pf(stats::qf(0.95, (PA - 1), (n - PA)), (PA - 1), (n - PA), lambda_A, lower.tail = FALSE)
             }
             else {
               F_A <- NA
               p_A <- NA
               power_post_A <- NA
             }
           }
           else {
             PRE_A <- NA
             PRE_A_adj <- NA
             F_A <- NA
             p_A <- NA
             R_squared_A <- NA
             R_squared_adj_A <- NA
             f_squared_A <- NA
             lambda_A <- NA
             power_post_A <- NA
           }
  }
  else {
    # fitC and fitA are not given, assign NAs to R-squared
    SSE_MEAN <- NA

    PRE_C <- NA
    PRE_C_adj <- NA
    F_C <- NA
    p_C <- NA
    R_squared_C <- NA
    R_squared_adj_C <- NA
    f_squared_C <- NA
    lambda_C <- NA
    power_post_C <- NA

    PRE_A <- NA
    PRE_A_adj <- NA
    F_A <- NA
    p_A <- NA
    R_squared_A <- NA
    R_squared_adj_A <- NA
    f_squared_A <- NA
    lambda_A <- NA
    power_post_A <- NA
  }

  # PRE
  PRE <- 1 - SSEA/SSEC
  f_squared <- PRE/(1 - PRE)
  lambda <- f_squared*(n - PA)
  F <- ((SSEC - SSEA)/(PA - PC))/(SSEA/(n - PA))
  p <- stats::pf(F, (PA - PC), (n - PA), lower.tail = FALSE)
  power_post <- stats::pf(stats::qf(0.95, (PA - PC), (n - PA)), (PA - PC), (n - PA), lambda, lower.tail = FALSE)
  PRE_adj <- 1 - (1 - PRE)*((n - PC)/(n - PA))

  # Return
  matrix(
    c(   SSE_MEAN,    n - 1,                        NA,          NA,              NA,    NA,  NA,  NA,        NA,           NA,
             SSEC,   n - PC,               R_squared_C, f_squared_C, R_squared_adj_C, PRE_C, F_C, p_C, PRE_C_adj, power_post_C,
             SSEA,   n - PA,               R_squared_A, f_squared_A, R_squared_adj_A, PRE_A, F_A, p_A, PRE_A_adj, power_post_A,
      SSEC - SSEA,  PA - PC, R_squared_A - R_squared_C,   f_squared,              NA,   PRE,   F,   p,   PRE_adj,   power_post),
    nrow = 4, ncol = 10, byrow = TRUE,
    dimnames = list(
      c("Baseline", "C", "A", "A vs. C"),
      c("SSE", "df", "R_squared", "f_squared", "R_squared_adj", "PRE", "F(PA-PC,n-PA)", "p", "PRE_adj", "power_post")
    ))
}
