#' Compute PRE and test it using F test.
#'
#' @param fitc The result of `lm()` of the Compact model (Model C).
#' @param fitA The result of `lm()` of the Augmented model (Model A).
#'
#' @return A data.frame including *SSEC*, *SSEA*, *PRE*, *F*, *df1*, *df2*, and *p*. SSEC  = The sum of squared errors of Model C, SSEA  = The sum of squared errors of Model A, PRE = The proportional reduction in error. The rest is the F test of PRE.
#' @export
#'
#' @examples
#' x <- rnorm(193)
#' y <- 0.3 + 0.2*x + rnorm(193)
#' dat <- data.frame(y, x)
#' fitC <- lm(y ~ 1, dat)
#' fitA <- lm(y ~ x, dat)
#' test_PRE(fitC, fitA)
test_PRE <- function(fitc, fitA) {
  n = length(fitc$residuals)
  if (n != length(fitA$residuals))
    stop("sample size of Model C does not equal to Model A.")
  PC = length(fitc$coefficients)
  PA = length(fitA$coefficients)
  if (PC >= PA)
    stop("Model C has more parameters than Model A.")
  SSEC = sum((fitc$residuals)^2)
  SSEA = sum((fitA$residuals)^2)
  SSR = SSEC - SSEA
  PRE = 1 - SSEA/SSEC
  df1 = PA - PC
  df2 = n - PA
  F = (SSR/df1)/(SSEA/df2)
  p = stats::pf(F, df1, df2, lower.tail = FALSE)
  out = data.frame(SSEC, SSEA, PRE, F, df1, df2, p)
  row.names(out) <- paste0("Result",row.names(out))
  return(out)
}
