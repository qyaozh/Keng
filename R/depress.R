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
#' @format ## `depress`
#' A three-wave data frame with 185 rows and 323 columns:
#' \describe{
#'   \item{date100}{Date of the response. The 1 in the hundreds place indicates the 1st-wave survey. The rest variables also follow this naming rule.}
#'   \item{id}{Participant id}
#'   \item{gender100}{Gender. 1 = boy, 2 = girl.}
#'   \item{age100}{Age in year.}
#'   \item{grade100}{Grade.}
#'   \item{class100}{Class.}
#'   \item{boarder100}{Boarder. 1 = this student lived at school during term time, 2 = this student did not live at school during term time.}
#'   \item{cost100}{Cost in yuan per month.}
#'   \item{fedu100}{Father education level. 0 = no formal school education, 1 = primary school, 2 = middle school, 3 = high school or equivalent, 4 = college or equivalent, 5 = master's degree, 6 = doctor's degree.}
#'   \item{medu100}{Mother education level. The coding scheme is identical to that of `fedu`.}
#'   \item{intervene100}{Intervene. 0 = control group, 1 = intervention group.}
#'   \item{attr112}{Attributional style questionnaire. All items in this questionnaire were rated on a 7-point bipolar scale. The 1 in the tens place indicates the 1st situation, positive achievement situation. The 2 in the last place indicates the 2nd item, internal attribution. High scores indicate more internal attribution.}
#'   \item{attr113}{Attributional style questionnaire. The 3 in the last place indicates the 3rd item, stable attribution. High scores indicate more stable attribution.}
#'   \item{attr114}{Attributional style questionnaire. The 4 in the last place indicates the 4th item, global attribution. High scores indicate more global attribution.}
#'   \item{attr115}{Attributional style questionnaire. The 5 in the last place indicates the 5th item, the importance of this situation. High scores indicate more important.}
#'   \item{attr122}{Attributional style questionnaire. The 2 in the tens place indicates the 2nd situation, negative interpersonal relationship situation.}
#'   \item{attr132}{Attributional style questionnaire. The 3 in the tens place indicates the 3rd situation, positive interpersonal relationship situation.}
#'   \item{attr142}{Attributional style questionnaire. The 4 in the tens place indicates the 4th situation, negative achievement situation.}
#'   \item{attr152}{Attributional style questionnaire. The 5 in the tens place indicates the 5th situation, positive interpersonal relationship situation.}
#'   \item{attr162}{Attributional style questionnaire. The 6 in the tens place indicates the 6th situation, negative achievement situation.}
#'   \item{attr172}{Attributional style questionnaire. The 7 in the tens place indicates the 7th situation, positive achievement situation.}
#'   \item{attr182}{Attributional style questionnaire. The 8 in the tens place indicates the 8th situation, negative interpersonal relationship situation.}
#'   \item{cope101}{Coping inventory. All items in this inventory were rated on a 5-point Likert scale with 1 = rarely, 2 = seldom, 3 = moderately, 4 = often, 5 = very often. The "01" in the last two places indicates item 01. Item 01, as well as item 02, 06, 10, 16, 22, 25, 27, 28, 37, 40, 42, 43, 44, 47, and 48, measured task-oriented coping.}
#'   \item{cope105}{Coping inventory. The "05" in the last two places indicates item 05. Item 05, as well as item 07, 08, 14, 15, 17, 18, 20, 23, 26, 29, 31, 34, 35, 39, and 46, measured emotion-oriented coping.}
#'   \item{cope103}{Coping inventory. The "03" in the last two places indicates item 03. Item 03, as well as item 04, 09, 11, 12, 19, 21, 24, 30, 32, 33, 36, 38, 41, 45, and 49, measured avoidance-oriented coping.}
#'   \item{113}{Drinking. A coping strategy added by the D research to coping inventory.}
#'   \item{150}{Smoking. A coping strategy added by the D research to coping inventory.}
#'   \item{cope201}{Coping inventory, Wave 2, item 01.}
#'   \item{cope302}{Coping inventory, Wave 3, item 02.}
#'   \item{depr101}{Depression scale. All items in this scale were rated on a 4-point Likert scale with 1 = rarely, 2 = seldom, 3 = often, 5 = always.}
#'   \item{depr102}{Depression scale. Item 02, 05, 06, 11, 12, 14, 16, 17, 18, and 20 has been reverse-coded. High scores indicate severe depression.}
#'   \item{ecr101}{Experience in close relationships scale. All items in this scale were rated on a 7-point Likert scale with 1 = strongly disagree, 2 = disagree, 3 = somewhat disagree, 4 = Neutral, 5 = somewhat agree, 6 = agree, 7 = strongly agree. Item 01, 03, 04, 06, 08, and 09 measured attachment avoidance.}
#'   \item{ecr102}{Experience in close relationships scale. Item 02. Item 02, 05, and 07 measured attachment anxiety.}
#'   \item{ecr103}{Experience in close relationships scale. Item 03. Item 03, 04, 06, and 09 has been reverse-coded.}
#'   \item{na_waves}{Not-available waves, ranging from 1 to 3.}
#'   \item{attach_anx}{Attachment anxiety, computed by averaging corresponding items.}
#'   \item{attach_avo}{Attachment aviodance, computed by averaging corresponding items.}
#'   \item{internalP1}{Internal attribution for positive situations at wave 1, computed by averaging corresponding items.}
#'   \item{stableP1}{Stable attribution for positive situations at wave 1, computed by averaging corresponding items.}
#'   \item{globalP1}{Global attribution for positive situations at wave 1, computed by averaging corresponding items.}
#'   \item{internalN1}{Internal attribution for negative situations at wave 1, computed by averaging corresponding items.}
#'   \item{internalP2}{Internal attribution for positive situations at wave 2, computed by averaging corresponding items.}
#'   \item{cope_task1}{Task-oriented coping at wave 1, computed by averaging corresponding items.}
#'   \item{cope_emo1}{Emotion-oriented coping at wave 1, computed by averaging corresponding items.}
#'   \item{cope_avo1}{Avoidance-oriented coping at wave 1, computed by averaging corresponding items.}
#'   \item{cope_task2}{Task-oriented coping at wave 2, computed by averaging corresponding items.}
#'   \item{depr1}{Depression at wave 1, computed by averaging corresponding items.}
#' }
#' @source Keng package.
"depress"
