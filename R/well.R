#' Three-wave data from the W research
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
#' A three-wave data frame with 200 rows and 413 columns:
#' \describe{
#'   \item{id}{Participant id}
#'   \item{gender100}{Gender. 0 = boy, 1 = girl. This variable ends with "100".
#'   The 1st digit indicates the wave number, and the last two digits indicate the item number.
#'   The rest variables also follow this naming rule.}
#'   \item{age100}{Age in years, wave 1.}
#'   \item{grade100}{Grade, wave 1.}
#'   \item{income100}{Monthly income (RMB) of the family, wave1.}
#'   \item{loan300}{National student loan in the past year. Reported at wave 3. 0 = Not received, 1 = Received.}
#'   \item{cost100}{Monthly cost of the participant, wave1. "1" = "<500RMB", "2" = "500-1000RMB",
#'   "3" = "1000-1500RMB","4" = "1500-2000RMB","5" = "2000-2500RMB", "6" = ">2500RMB".}
#'   \item{SSES100}{Subjective Socioeconomic Status, wave1.
#'   SSES was measured through the 10-step social ladder,
#'   with high scores indicating high status.}
#'   \item{heema101}{HEEMA scale, wave 1, item 01. HEEMA = The Hedonic, Eudaimonic,
#'   and Extrinsic Motivations for Activities (Lefebvre & Huta, 2021).
#'   HEEMA was rated on a 7-point scale (1 = not at all, 7 = very much).
#'   HEEMA measures three kind of motivations: Hedonic (item 01, 04, 06, 07, 09, 16),
#'   eudaimonic (item 02, 03, 05, 08, 10), and Extrinsic (item 11, 12, 13, 14, 15).}
#'   \item{cssp101}{Career Social Support from Parents, wave 1, item 01.
#'   CSS investigates four kind of support using a 5-point scale (1 = almost none, 5 = very much; Hou et al, 2010):
#'   material (item 01, 02, 03), advice (item 04, 05, 06, 07, 08, 09),
#'   emotional (item 10, 11, 12, 13), and information (14, 15, 16, 17, 18, 19, 20).}
#'   \item{csss101}{Career Social Support from Siblings, wave 1, item 01.}
#'   \item{csst101}{Career Social Support from Teachers, wave 1, item 01.}
#'   \item{cssf101}{Career Social Support from Friends, wave 1, item 01.}
#'   \item{cssr101}{Career Social Support from Relatives, wave 1, item 01.}
#'   \item{grit101}{Grit scale (Wei & Hu, 2017), wave 1, item 01. Grit is rated on a 5-point scale
#'   (1 = not at all like me, 5 = very much like me). Item 1, 3, 5, and 6 should be reverse-scored.
#'   Grit has two dimensions: perseverance (item 02, 04,  07, 08) and passion (item 01, 03, 05, 06).
#'   After reverse scoring, high scores indicate high perseverance and passion.}
#'   \item{CET4100}{CET4 score, wave 1. CET4 = College English Test Band 4.
#'   0 = participants had not taken CET4 test.}
#'   \item{flourish101}{Flourish scale, wave 1, item 01. Flourish scale (Tong & Wang, 2017) was rated on a 5-point scale
#'   (1 = very disagree, 7 = very agree), with high scores indicating high eudaimonic well-being.}
#'   \item{spne101}{SPNE scale, wave 1, item 01. SPNE = Scale of Positive and Negative Experience (Tong & Wang, 2017).
#'   SPNE measures positive (item 01, 03, 05, 07, 10, 12) and negative (item 02, 04, 06, 08, 9, 11)
#'   emotions on a 5-point scale (1 = very rarely or never, 5 = very often or always).
#'   High positive emotion and low negative emotion indicate high hedonic well-being.}
#'   \item{flourish1}{Flourish, wave 1. All scale scores and dimension scores in this data are computed by averaging corresponding items.}
#'   \item{posEmo1}{Positive Emotions, wave 1.}
#'   \item{negEmo1}{Negative Emotions, wave 1.}
#'   \item{grit1}{Grit, wave 1.}
#'   \item{perseverance1}{Perseverance, wave 1.}
#'   \item{passion1}{Passion, wave 1.}
#'   \item{hedonic1}{Hedonic well-being motivation, wave 1.}
#'   \item{eudaimonic1}{Eudaimonic well-being motivation, wave 1.}
#'   \item{extrinsic1}{Extrinsic well-being motivation, wave 1.}
#'   \item{csspm1}{Career Social Support, Parents, Material, wave 1.}
#'   \item{csspa1}{Career Social Support, Parents, Advice, wave 1.}
#'   \item{csspe1}{Career Social Support, Parents, Emotional, wave 1.}
#'   \item{csspi1}{Career Social Support, Parents, Information, wave 1.}
#'   \item{csssm1}{Career Social Support, Siblings, Material, wave 1.}
#'   \item{csstm1}{Career Social Support, Teachers, Material, wave 1.}
#'   \item{cssfm1}{Career Social Support, Friends, Material, wave 1.}
#'   \item{cssrm1}{Career Social Support, Relatives, Material, wave 1.}
#'   \item{cssp1}{Career Social Support, Parents, wave 1.}
#'   \item{cssm1}{Career Social Support, Material, wave 1.}
#' }
#' @source Keng package.
#' @references Lefebvre, A., & Huta, V. (2021). Age and gender differences in eudaimonic, hedonic, and extrinsic motivations.
#' *Journal of Happiness Studies*, *22*, 2299-2321.
#'
#' Hou, Z., Bai, R., & Yao, Y. (2010). Development of career social support inventory for Chinese college students.
#' *Chinese Journal of Clinical Psychology*, *18*(4), 439-442.
#'
#' Wei, Y., & Hu, J. (2017). Grit: Concept, structure, influencing factors, and consequences.
#' *Psychology: Techniques and Applications*, *5*(1), 52-61.
#'
#' Tong, K. K. & Wang, Y. Y. (2017). Validation of the Flourishing Scale and Scale of Positive and Negative Experience in a Chinese Community Sample. *PLUS ONE*, *12*(8), e0181616.
"well"
