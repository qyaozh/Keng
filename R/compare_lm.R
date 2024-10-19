#' Compare lm.fit using PRE and R square.
#'
#' @param fitC The result of `lm()` of the Compact model (Model C).
#' @param fitA The result of `lm()` of the Augmented model (Model A).
#' @param n Sample size of the Model C or Model A. Model C and Model A must use the same sample, and hence have the same sample size.
#' @param PC The number of parameters in Model C.
#' @param PA The number of parameters in Model A. PA must be larger than PC.
#' @param SSEC The Sum of Squared Errors (SSE) of Model C.
#' @param SSEA The Sum of Squared Errors of Model A.
#'
#' @details `compare_lm()` compare Model A with Model C using PRE (Proportional Reduction in Error) and R square. There are two ways of using `compare_lm()`. The first is giving `compare_lm()` fitC and fitA. The second is giving *n*, *PC*, *PA*, *SSEC*, and *SSEA*. The first way is more convenient, and it minimizes precision loss by omitting copying and pasting SSEC and SSEA. If fitC and fitA are not inferior to the intercept-only model, R-Square and Adjusted R-Square are also computed. Note that the *F*-tests for *PRE* and R-square change are equivalent. Please refer to Judd et al. (2017) for more details about *PRE*.
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). *Data analysis: A model comparison approach to regression, ANOVA, and beyond*. Routledge.
#'
#' @return A data.frame including *SSE*, *PRE*, the *F*-test of *PRE* (*F*, *df1*, *df2*, *p*), and *PRE_adjusted*. If fitC and fitA are not inferior to the intercept-only model, R-Square and Adjusted R-Square will also be computed.
#' @export
#'
#' @examples
#' x1 <- rnorm(193)
#' x2 <- rnorm(193)
#' y <- 0.3 + 0.2*x1 + 0.1*x2 + rnorm(193)
#' dat <- data.frame(y, x1, x2)
#' fit0 <- lm(I(y-1) ~ 0, dat)
#' fit1 <- lm(y ~ 1, dat)
#' fit2 <- lm(y ~ x1, dat)
#' fit3 <- lm(y ~ x1 + x2, dat)
#' compare_lm(fit0, fit1)
#' compare_lm(fit1, fit2)
#' compare_lm(fit2, fit3)
#' compare_lm(fit1, fit3)
compare_lm <- function(fitC=NULL, fitA=NULL, n=NULL, PC=NULL, PA=NULL, SSEC=NULL, SSEA=NULL) {
  if (!(
    (sum(sapply(list(fitC, fitA), is.null)) == 0) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 5) |
    (sum(sapply(list(fitC, fitA), is.null)) == 2) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 0)
    ))
    stop("Provide fitc and fitA, or provide n, PC, PA, SSEC, and SSEA instead.")
  else if ((sum(sapply(list(fitC, fitA), is.null)) == 0) & (sum(sapply(list(n, PC, PA, SSEC, SSEA), is.null)) == 5)) {
           n = length(fitC$residuals)
           if (n != length(fitA$residuals))
             stop("sample size of Model C must be equal to Model A.")
           PC = length(fitC$coefficients)
           PA = length(fitA$coefficients)
           if (PC >= PA)
             stop("Model C must has less parameters than Model A.")
           SSEC = sum((fitC$residuals)^2)
           SSEA = sum((fitA$residuals)^2)
           # mean model and R_Square
           SSE_MEAN = sum((fitC[["model"]]$y - mean(fitC[["model"]]$y))^2)
           R_Square_C = ifelse(SSE_MEAN >= SSEC, (SSE_MEAN - SSEC)/SSE_MEAN, NA)
           R_Square_adj_C = ifelse(SSE_MEAN >= SSEC, 1 - (SSEC/(n - PC))/(SSE_MEAN/(n - 1)), NA)
           R_Square_A = ifelse(SSE_MEAN >= SSEA, (SSE_MEAN - SSEA)/SSE_MEAN, NA)
           R_Square_adj_A = ifelse(SSE_MEAN >= SSEA, 1 - (SSEA/(n - PA))/(SSE_MEAN/(n - 1)), NA)
  }
  else {
    R_Square_C = NA
    R_Square_adj_C = NA
    R_Square_A = NA
    R_Square_adj_A = NA
  }
  SSR <- SSEC - SSEA
  PRE <- 1 - SSEA/SSEC
  df1 <- PA - PC
  df2 <- n - PA
  F <- (SSR/df1)/(SSEA/df2)
  p <- stats::pf(F, df1, df2, lower.tail = FALSE)
  PRE_adj <- 1 - (1 - PRE)*((n - PC)/(n - PA))
  # Return
  out <- as.data.frame(matrix(
    c(SSEC, NA, NA, NA, NA, NA, NA, R_Square_C, R_Square_adj_C,
      SSEA, PRE, F, df1, df2, p, PRE_adj, R_Square_A, R_Square_adj_A),
    nrow = 2, ncol = 9, byrow = TRUE,
    dimnames = list(
      c("Model C","Model A"),
      c("SSE", "PRE", "F", "df1", "df2","p","PRE_adj","R_Square","R_Square_adj")
    )))
  return(out)
}
