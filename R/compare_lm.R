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
#' Note that the *F*-tests for *PRE* and that for R-squared change are equivalent.
#' Please refer to Judd et al. (2017) for more details about *PRE*, and refer to Aberson (2019) for more details about *f_squared* and post-hoc power.
#'
#' @references Aberson, C. L. (2019). *Applied power analysis for the behavioral sciences*. Routledge.
#'
#' Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). *Data analysis: A model Comparison approach to regression, ANOVA, and beyond*. Routledge.
#'
#' @return A matrix with 11 rows and 4 columns.
#' The first column reports information for baseline model (intercept-only model).
#' the second for model C, the third for model A, and the fourth for the change (model A vs. model C).
#' *SSE* (Sum of Squared Errors) and *df* of *SSE* for baseline model, model C,
#' model A, and change (model A vs. model C) are reported in row 1 and row 2.
#' The information in the fourth column are all for the change; put differently,
#' These results could quantify the effect of one or a set of new parameters model A has but model C doesn't.
#' If fitC and fitA are not inferior to the intercept-only model,
#' *R-squared*, *Adjusted R-squared*, *PRE*, *PRE_adjusted*, and *f_squared* for the full model
#' (compared with the baseline model) are reported for model C and model A.
#' If model C or model A has at least one predictor, *F* -test with *p*,
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

           PC <- length(stats::coef(fitC))
           PA <- length(stats::coef(fitA))

           df_M  <- n - 1
           df_C_M <- PC - 1
           df_C  <- n - PC
           df_A_M <- PA - 1
           df_A  <- n - PA

           if (PC >= PA)
             stop("model C must has less parameters than model A.")

           SSEC = sum(stats::resid(fitC)^2)
           SSEA = sum(stats::resid(fitA)^2)

           # SSE of the mean model
           SSEM <- sum((
             stats::model.response(stats::model.frame(fitC)) - mean(stats::model.response(stats::model.frame(fitC)))
           ) ^ 2)

           # model C vs the mean model
           if (SSEM >= SSEC) {
             PRE_C <- 1 - SSEC/SSEM
             PRE_C_adj <- 1 - (1 - PRE_C)*(df_M/df_C)

             R_squared_C <- (SSEM - SSEC)/SSEM
             R_squared_adj_C <- 1 - (SSEC/df_C)/(SSEM/df_M)

             f_squared_C <- SSEM/SSEC - 1
             lambda_C <- f_squared_C*df_C

             if (PC - 1 > 0) {
               F_C <- ((SSEM - SSEC)/df_C_M)/(SSEC/df_C)
               p_C <- stats::pf(F_C, df_C_M, df_C, lower.tail = FALSE)

               power_post_C <- stats::pf(stats::qf(0.95, df_C_M, df_C), df_C_M, df_C, lambda_C, lower.tail = FALSE)
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

           if (SSEM >= SSEA) {
             PRE_A <- 1 - SSEA/SSEM
             PRE_A_adj <- 1 - (1 - PRE_A)*(df_M/df_A)

             R_squared_A <- (SSEM - SSEA)/SSEM
             R_squared_adj_A <- 1 - (SSEA/df_A)/(SSEM/df_M)

             f_squared_A <- SSEM/SSEA - 1
             lambda_A <- f_squared_A*df_A

             if (PA - 1 > 0) {
               F_A <- ((SSEM - SSEA)/df_A_M)/(SSEA/df_A)
               p_A <- stats::pf(F_A, df_A_M, df_A, lower.tail = FALSE)
               power_post_A <- stats::pf(stats::qf(0.95, df_A_M, df_A), df_A_M, df_A, lambda_A, lower.tail = FALSE)
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
    SSEM <- NA

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
  df_M <- n - 1
  df_C  <- n - PC
  df_A  <- n - PA
  df_A_C <- PA - PC

  PRE <- 1 - SSEA/SSEC
  f_squared <- PRE/(1 - PRE)
  lambda <- f_squared*df_A
  F <- ((SSEC - SSEA)/df_A_C )/(SSEA/df_A)
  p <- stats::pf(F, df_A_C , df_A, lower.tail = FALSE)
  power_post <- stats::pf(stats::qf(0.95, df_A_C , df_A), df_A_C, df_A, lambda, lower.tail = FALSE)
  PRE_adj <- 1 - (1 - PRE)*(df_C/df_A)

  # Return
  matrix(
    c(       SSEM,   df_M,       1,                         NA,          NA,              NA,    NA,  NA,  NA,        NA,           NA,
             SSEC,   df_C,      PC,                R_squared_C, f_squared_C, R_squared_adj_C, PRE_C, F_C, p_C, PRE_C_adj, power_post_C,
             SSEA,   df_A,      PA,                R_squared_A, f_squared_A, R_squared_adj_A, PRE_A, F_A, p_A, PRE_A_adj, power_post_A,
      SSEC - SSEA, df_A_C, PA - PC,  R_squared_A - R_squared_C,   f_squared,              NA,   PRE,   F,   p,   PRE_adj,   power_post),
    ncol = 4,
    dimnames = list(
      c("SSE", "df", "Number of parameters","R_squared", "f_squared", "R_squared_adj", "PRE", "F(PA-PC,n-PA)", "p", "PRE_adj", "power_post"),
      c("Baseline", "C", "A", "A vs. C")
    ))
}
