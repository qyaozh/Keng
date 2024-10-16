#' Compute PRE and test it using the F-test.
#'
#' @param fitC The result of `lm()` of the Compact model (Model C).
#' @param fitA The result of `lm()` of the Augmented model (Model A).
#' @param n Sample size of the Model C or Model A. Model C and Model A must use the same sample, and hence have the same sample size.
#' @param PC The number of parameters in Model C.
#' @param PA The number of parameters in Model A. PA must be larger than PC.
#' @param SSEC The Sum of Squared Errors of Model C.
#' @param SSEA The Sum of Squared Errors of Model A.
#'
#' @details `test_PRE()` tests the `PRE` (Proportional Reduction in Error) of Model A compared with Model C. There are two ways of using `test_PRE()`. The first is giving `test_PRE()` fitC and fitA. The second is giving *n*, *PC*, *PA*, *SSEC*, and *SSEA*. The first way is more convenient, and it minimizes precision loss by omitting copying and pasting SSEC and SSEA. Please refer to Judd et al. (2017) for more details about *PRE*.
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). *Data analysis: A model comparison approach to regression, ANOVA, and beyond*. Routledge.
#'
#' @return A data.frame including *SSEC*, *SSEA*, *PRE*, *F*, *df1*, *df2*, and *p*.
#' @export
#'
#' @examples
#' x <- rnorm(193)
#' y <- 0.3 + 0.2*x + rnorm(193)
#' dat <- data.frame(y, x)
#' fitC <- lm(y ~ 1, dat)
#' fitA <- lm(y ~ x, dat)
#' test_PRE(fitC, fitA)
test_PRE <- function(fitC=NULL, fitA=NULL, n=NULL, PC=NULL, PA=NULL, SSEC=NULL, SSEA=NULL) {
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
           }
  SSR <- SSEC - SSEA
  PRE <- 1 - SSEA/SSEC
  df1 <- PA - PC
  df2 <- n - PA
  F <- (SSR/df1)/(SSEA/df2)
  p <- stats::pf(F, df1, df2, lower.tail = FALSE)
  out <- data.frame(SSEC, SSEA, PRE, F, df1, df2, p)
  row.names(out) <- paste0("Row_", row.names(out))
  return(out)
}
