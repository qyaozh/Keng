#' The 2nd wave data from the D research
#'
#' D refers to Depression. D research is a three-wave longitudinal survey research.
#' Here are four data sets, `depress1` only includes data collected at wave 1,
#' `depress2` at wave 2, and `depress3` at wave 3. `depress` was created by joining `depress1`,
#' `depress2`, and `depress3` together. `depress` has done reverse-coding and
#' has computed variable scores like `depr1`, which indicates the depression score at wave 1.
#' `depress` serves to ease users' practicing plotting and statistical analyzing. `depress1`,
#' `depress2`, and `depress3` serves to support users' practicing data wrangling.
#'
#' @format ## `depress2`
#' A data frame with 173 rows and 104 columns at wave 2:
#' \describe{
#'   \item{depr02}{Depression Scale, item 02. Item 02, 05, 06, 11, 12, 14, 16, 17, 18,
#'   and 20 has NOT been reverse-coded in `depress2`.}
#' }
#' @source Keng package.
"depress2"
