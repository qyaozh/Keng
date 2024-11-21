#' Test r using the t-test and fisher's z given r and n.
#'
#' @param r Pearson correlation.
#' @param n Sample size of *r*.
#' @details To test the significance of the *r* using one-sample *t*-test,
#' the *SE* of the *r* is determined by the following formula: `SE = sqrt((1 - r^2)/(n - 2))`.
#' Another way is transforming *r* to Fisher's Z using the following formula:
#' `fz = atanh(r)` with the *SE* of `fz` being `sqrt(n - 3)`.
#' Note that Fisher's z is commonly used to compare two Pearson's correlations from independent samples.
#' Fisher's transformation is presented here for satisfying the curiosity of users interested in the difference of *t* -test and fisher's transformation.
#'
#' @return A list including *r*, *t* -test of *r* (`SE_r`, `t`, `p_r`),
#' *CI* of *r* based on *t* -test (`LLCI_r_t`, `ULCI_r_t`),
#' *fz* (Fisher's z)  of *r*, *z* -test of Fisher's z (`SE_fz`, `z`, `p_fz`), and *CI* of *r* derived from *fz*.
#'
#' @export
#'
#' @examples test_r(0.2, 193)
test_r <- function(r, n) {
  # t-test of r
  SE_t <- sqrt((1 - r ^ 2) / (n - 2))
  t <- r / SE_t
  p_r <- -2 * (stats::pt(abs(t), (n - 2)) - 1)
  CI_t_half <- stats::pt(q = 0.975, df = n - 2) * SE_t
  LLCI_r_t <- r - CI_t_half
  ULCI_r_t <- r + CI_t_half

  # CI_z, CI based on fisher's z
  fz <- atanh(r)
  SE_fz <- 1 / sqrt(n - 3)
  z <- fz / SE_fz
  p_fz <- -2 * (stats::pnorm(abs(z)) - 1)
  CI_z_half <- stats::qnorm(0.025) * (-1) * SE_fz
  LLCI_r_fz <- tanh(fz - CI_z_half)
  ULCI_r_fz <- tanh(fz + CI_z_half)
  list(
    t_test = c(
      r = r,
      SE_t = SE_t,
      t = t,
      p_r = p_r,
      LLCI_r_t = LLCI_r_t,
      ULCI_r_t = ULCI_r_t
    )
    ,
    Fisher_z = c(
      fz = fz,
      SE_fz = SE_fz,
      z = z,
      p_fz = p_fz,
      LLCI_r_fz = LLCI_r_fz,
      ULCI_r_fz = ULCI_r_fz
    )
  )
}
