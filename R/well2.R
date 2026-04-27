#' The 2nd wave data from the W research
#'
#' W refers to well-being. W research is a three-wave longitudinal survey research.
#' Here are four data sets, `well1` only includes data collected at wave 1,
#' `well2` at wave 2, and `well3` at wave 3. `well` was created by joining `well1`,
#' `well2`, and `well3` together. `well` has done reverse-coding and
#' has computed variable scores like `flourish1`, which indicates the flourish score at wave 1.
#' `well` serves to ease users' practicing plotting and statistical analyzing. `well1`,
#' `well2`, and `well3` serves to support users' practicing data wrangling.
#'
#' @format ## `well`
#' The 2nd wave data frame with 200 rows and 152 columns:
#' \describe{
#'   \item{id}{Participant id}
#'   \item{gender}{Gender. 0 = boy, 1 = girl. This variable ends with "100".
#'   The 1st digit indicates the wave number, and the last two digits indicate the item number.
#'   The rest variables also follow this naming rule.}
#'   \item{age}{Age in years.}
#'   \item{grade}{Grade.}
#'   \item{income}{Monthly income of the family.}
#'   \item{cost}{Monthly cost of the participant.}
#'   \item{SSES}{Subjective Socioeconomic Status.
#'   SSES was measured through the 10-step social ladder,
#'   with high scores indicating high status.}
#'   \item{heema01}{HEEMA scale, item 01. HEEMA = The Hedonic, Eudaimonic,
#'   and Extrinsic Motivations for Activities (Lefebvre & Huta, 2021).
#'   HEEMA was rated on a 7-point scale (1 = not at all, 7 = very much).
#'   HEEMA measures three kind of motivations: Hedonic (item 01, 04, 06, 07, 09, 16),
#'   eudaimonic (02, 03, 05, 08, 10), and Extrinsic (item 11, 12, 13, 14, 15).}
#'   \item{cssp01}{Career Social Support from Parents, item 01.
#'   CSS investigates four kind of support using a 5-point scale (1 = almost none, 5 = very much; Hou et al, 2010):
#'   material (item 01, 02, 03), advice (item 04, 05, 06, 07, 08, 09),
#'   emotional (item 10, 11, 12, 13), and information (14, 15, 16, 17, 18, 19, 20).}
#'   \item{csss01}{Career Social Support from Siblings, item 01.}
#'   \item{csst01}{Career Social Support from Teachers, item 01.}
#'   \item{cssf01}{Career Social Support from Friends, item 01.}
#'   \item{cssr01}{Career Social Support from Relatives, item 01.}
#'   \item{grit01}{Grit scale, item 01. Grit (Wei & Hu, 2017) is rated on a 5-point scale
#'   (1 = not at all like me, 5 = very much like me). Item 1, 3, 5, and 6 should be reverse-scored.
#'   Grit has two dimensions: perseverance (item 02, 04,  07, 08) and passion (item 01, 03, 05, 06).
#'   After reverse scoring, high scores indicate high perseverance and passion.}
#'   \item{CET4}{CET4 score. CET4 = College English Test Band 4.
#'   CET4 = 0 indicates paticipants had not taken CET4 test.}
#'   \item{flourish01}{Flourish scale, item 01. Flourish scale (Tong & Wang, 2017) was rated on a 5-point scale
#'   (1 = very disagree, 5 = very agree), with high scores indicating high eudaimonic well-being.}
#'   \item{spne01}{SPNE scale, item 01. SPNE = Scale of Positive and Negative Experience (Tong & Wang, 2017).
#'   SPNE measures positive (item 01, 03, 05, 07, 10, 12) and negative (item 02, 04, 06, 08, 9, 11)
#'   emotions on a 5-point scale (1 = very rarely or never, 5 = very often or always).
#'   High positive emotion and low negative emotion indicate high hedonic well-being.}
#' }
#' @source Keng package.
#' @references Lefebvre, A., & Huta, V. (2021). Age and gender differences in eudaimonic, hedonic, and extrinsic motivations.
#' Journal of Happiness Studies, 22, 2299-2321.
#'
#' Hou, Z., Bai, R., & Yao, Y. (2010). Development of career social support inventory for Chinese college students.
#' Chinese Journal of Clinical Psychology, 18(4), 439-442.
#'
#' Wei, Y., & Hu, J. (2017). Grit: Concept, structure, influencing factors, and consequences.
#' Psychology: Techniques and Applications, 5(1), 52-61.
#'
#' Tong, K. K. & Wang, Y. Y. (2017). Validation of the Flourishing Scale and Scale of Positive and Negative Experience in a Chinese Community Sample. PlosOne 12(8): e0181616. https://doi.org/10.1371/journal.pone.0181616
"well2"
