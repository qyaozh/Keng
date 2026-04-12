#' Data sets from the D research
#'
#' D refers to Depression. D research is a three-wave longitudinal survey research.
#' Here are four data sets, `depress1` only includes data collected at wave 1,
#' `depress2` at wave 2, and `depress3` at wave 3. `depress` was created by joining `depress1`,
#' `depress2`, and `depress3` together. `depress` has done reverse-coding and
#' has computed variable scores like `depr1`, which indicates the depression score at wave 1.
#' `depress` serves to ease users' practicing plotting and statistical analyzing. `depress1`,
#' `depress2`, and `depress3` serves to support users' practicing data wrangling.
#'
#' @format ## `depress1`
#' A data frame with 174 rows and 122 columns at wave 1:
#' \describe{
#'   \item{date}{Date of the response.}
#'   \item{id}{Student id.}
#'   \item{ecr01}{Experience in close relationships scale, item 01.}
#'   \item{ecr03}{Experience in close relationships scale, item 03.
#'   Item 03, 04, 06, and 09 has NOT been reverse-coded in `depress1`.}
#'   \item{attr12}{Attribution style questionnaire, situation 1, item 2.}
#'   \item{depr02}{Depression Scale, item 02. Item 02, 05, 06, 11, 12, 14, 16, 17, 18,
#'   and 20 has NOT been reverse-coded in `depress1`.}
#' }
#' @source Keng package.
"depress1"
