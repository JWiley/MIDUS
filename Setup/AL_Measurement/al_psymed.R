library(checkpoint)
## checkpoint("2016-09-04", R.version = "3.3.1", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)
checkpoint("2017-05-26", R.version = "3.4.0", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)

library(JWileymisc)
library(pscore)
library(mice)
library(MplusAutomation)
library(grid)
library(data.table)
## library(qgraph)
## library(pcalg)
## library(lvnet)

dwclean <- readRDS("~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dwclean.RDS")

dwclean2 <- copy(dwclean[, .(
  M2ID, B1PAGE_M2, Sex,
  Radj_epi, Radj_nor, Radj_crt, b4bdheas,
  Rb4p1gs, pulpress, Rb4p1d,
  p4homair, Rb4bgluc, b4bha1c,
  b4bldl, b4bhdl, Rb4btrig, Rb4pwhr,
  Rb4bcrp, b4bil6, b4bfgn, b4bsicam, b4bsesel,
  Rb4p1d, avgb_rm, avgb_hf, avgb_sd, avgb_lf)])

dwclean2[, MissAll := Reduce(`+`, lapply(.SD, is.na)), by = M2ID]

fdat <- as.data.frame(dwclean[dwclean2$MissAll < 24])

med.vars <- c("fg_bpup", "fg_bpdwn", "fg_hrdwn", "fg_hrup",
  "fg_hpaup", "fg_sympdwn", "fg_sympup", "fg_paradwn",
  "fg_paraup", "fg_infldwn", "fg_influp", "fg_glucdwn",
  "fg_glucup", "fg_rxchol")

fdat$ALMedGroup <- as.integer(rowSums(fdat[, med.vars], na.rm=TRUE) == 0)


# reverse some biomarkers, to make the loadings all the same direction
# so that higher scores are always "worse"
## fdat <- within(fdat, {
##   avgb_sd <- (-avgb_sd)
##   avgb_rm <- (-avgb_rm)
##   avgb_lf <- (-avgb_lf)
##   avgb_hf <- (-avgb_hf)
##   b4bhdl  <- (-b4bhdl)
##   Radj_crt <- (-Radj_crt)
##   b4bdheas <- (-b4bdheas)
## })

n <- function(x) as.numeric(as.character(x))
z <- function(x) as.vector(scale(x))

pval.stars <- function(p) {
  symnum(p, cutpoints = c(0, .001, .01, .05, .1, 1),
         symbols = c("***", "**", "*", "^", ""))
}

pval.r4 <- function(p) {
    ifelse(p < .0001, "< .0001", paste("=", gsub("0\\.", "\\.", ifelse(p < .001,
                                            format(round(p, 4), digits = 4, nsmall = 4, scientific = 10),
                                            ifelse(p < .05,
                                            format(round(p, 3), digits = 3, nsmall = 3, scientific = 10),
                                            format(round(p, 2), digits = 2, nsmall = 2, scientific = 10))))))
    }




## res <- unlist(parLapply(cl, 1:1e7, function(i) sd(rnorm(10, mean = 0, sd = 1))))
## correction <- function(n) {
##     gamma((n-1)/2) * sqrt((n-1)/2) / gamma(n/2)
## }
## mean(res)
## mean(res * correction(10))

mymodels <- function(base) {
output <- list()

output$Equal <- update(base,
  TITLE = ~ "CFA for one factor of biomarkers with equal loadings;",
  MODEL = ~ . + "
  AL BY Radj_epi@1 Radj_nor@1
  Radj_crt@1 b4bdheas@1
  Rb4p1gs@1 pulpress@1 Rb4p1d@1
  p4homair@1 Rb4bgluc@1 b4bha1c@1
  b4bldl@1 b4bhdl@1 Rb4btrig@1 Rb4pwhr@1
  Rb4bcrp@1 b4bil6@1 b4bfgn@1 b4bsicam@1 b4bsesel@1
  Rb4p1d@1 avgb_rm@1 avgb_hf@1 avgb_sd@1
  avgb_lf@1;")

output$One <- update(base,
  TITLE = ~ "CFA for one factor of biomarkers;",
  MODEL = ~ . + "
  AL BY Radj_epi*1 Radj_nor*1
  Radj_crt b4bdheas
  Rb4p1gs pulpress Rb4p1d
  p4homair Rb4bgluc b4bha1c
  b4bldl b4bhdl Rb4btrig Rb4pwhr
  Rb4bcrp@1 b4bil6 b4bfgn b4bsicam b4bsesel Rb4p1d
  avgb_rm avgb_hf avgb_sd avgb_lf;
  avgb_rm WITH avgb_hf*;")

output$Fac <- update(base,
  TITLE = ~ "CFA for bio subsystems of biomarkers;",
  MODEL = ~ . + "
    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@-1 avgb_rm*-1 avgb_hf*-1 avgb_lf*-1 Rb4p1d*1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;")

output$AL <- update(base,
  TITLE = ~ "CFA for second order AL of biomarkers;",
  MODEL = ~ . + "
    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@-1 avgb_rm*-1 avgb_hf*-1 avgb_lf*-1 Rb4p1d*1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*1 hpa*1 card*1;
")

output$BiFac <- update(output$Fac,
  TITLE = ~ "BiFactor Model of biomarkers;",
  MODEL = ~ . + "
    AL BY Radj_epi*1 Radj_nor*1 avgb_sd*-1 avgb_rm*-1 avgb_lf*-1
    avgb_hf*-1 Radj_crt*1 b4bdheas*1 Rb4bcrp*1
    b4bil6*1 b4bfgn*1 b4bsicam*1 b4bsesel*1 pulpress*1 Rb4p1gs*1 Rb4p1d*1
    b4bha1c*1 Rb4bgluc*1
    p4homair*1 b4bldl*-1 b4bhdl*1 Rb4btrig*1 Rb4pwhr*1;
    AL@1;
    AL WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    Rb4bgluc@0;")

output$BiFac2 <- update(output$Fac,
  TITLE = ~ "BiFactor Model of biomarkers;",
  MODEL = ~ . + "
    AL1 BY Radj_epi*1 avgb_sd*-1 avgb_rm*-1 Radj_crt*1
      b4bil6*1 b4bfgn*1 b4bsicam*1 Rb4p1gs*1 Rb4bgluc*1
      p4homair*1 b4bldl*-1  Rb4btrig*1;
    AL2 BY Radj_nor*1 avgb_lf*-1 avgb_hf*-1
      b4bdheas*1 Rb4bcrp*1 b4bsesel*1
      pulpress*1 Rb4p1d*1 b4bha1c*1
      b4bhdl*1 Rb4pwhr*1;
    AL1@1;
    AL2@1;
    AL1 WITH AL2;
    AL1 WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    AL2 WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;

    Rb4bgluc@0;")


## Age Multiple Group Models ##
output$MGBiFacConfigMetricScalar <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, age config, metric, scalar;",
  VARIABLE = ~ . + "
  GROUPING = Agecat (1 = young 2 = middle 3 = old);")

output$MGBiFacConfigMetric <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, age config, metric;",
  VARIABLE = ~ . + "
  GROUPING = Agecat (1 = young 2 = middle 3 = old);",
  MODEL = ~ . + "
  MODEL YOUNG:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];
  MODEL MIDDLE:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];
  MODEL OLD:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];")

tmp <- paste(
  "MODEL YOUNG:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];",
  "MODEL MIDDLE:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];",
  "MODEL OLD:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];", sep = "\n")

## output$MGBiFacConfig <- update(output$BiFac,
##   TITLE = ~ "CFA for BiFactor of biomarkers, age config;",
##   VARIABLE = ~ . + "
##   GROUPING = Agecat (1 = young 2 = middle 3 = old);",
##   MODEL = substitute(~ . + x, list(x = tmp)))


## Medication Multiple Group Models ##
output$MGMedBiFacConfigMetricScalar <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, medication config, metric, scalar;",
  VARIABLE = ~ . + "
  GROUPING = ALMedGroup (0 = med 1 = nomed);")

output$MGMedBiFacConfigMetric <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, medication config, metric;",
  VARIABLE = ~ . + "
  GROUPING = ALMedGroup (0 = med 1 = nomed);",
  MODEL = ~ . + "
  MODEL NOMED:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];
  MODEL MED:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];")

tmp <- paste(
  "MODEL NOMED:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];",
  "MODEL MED:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];", sep = "\n")

output$MGMedBiFacConfig <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, medication config, metric;",
  VARIABLE = ~ . + "
  GROUPING = ALMedGroup (0 = med 1 = nomed);",
  MODEL = substitute(~ . + x, list(x = tmp)))

## Race/Ethnicity Multiple Group Models ##
output$MGRBiFacConfigMetricScalar <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, race config, metric, scalar;",
  VARIABLE = ~ . + "
  GROUPING = white (0 = nonwhite 1 = white);")

output$MGRBiFacConfigMetric <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, race config, metric;",
  VARIABLE = ~ . + "
  GROUPING = white (0 = nonwhite 1 = white);",
  MODEL = ~ . + "
  MODEL WHITE:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];
  MODEL NONWHITE:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];")

tmp <- paste(
  "MODEL WHITE:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];",
  "MODEL NONWHITE:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];", sep = "\n")

output$MGRBiFacConfig <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, race config;",
  VARIABLE = ~ . + "
  GROUPING = white (0 = nonwhite 1 = white);",
  MODEL = substitute(~ . + x, list(x = tmp)))


## Sex Multiple Group Models ##
output$MGSexBiFacConfigMetricScalar <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, sex config, metric, scalar;",
  VARIABLE = ~ . + "
  GROUPING = b1pgender (0 = male 1 = female);")

output$MGSexBiFacConfigMetric <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, sex config, metric;",
  VARIABLE = ~ . + "
  GROUPING = b1pgender (0 = male 1 = female);",
  MODEL = ~ . + "
  MODEL MALE:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];
  MODEL FEMALE:
  [Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];")

tmp <- paste(
  "MODEL MALE:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];",
  "MODEL FEMALE:",
  output$BiFac$MODEL,
  "[Radj_epi* Radj_nor* avgb_sd* avgb_rm* avgb_lf*
   avgb_hf* Radj_crt* b4bdheas* Rb4bcrp*
   b4bil6* b4bfgn* b4bsicam* b4bsesel* pulpress* Rb4p1gs* Rb4p1d*
   b4bha1c* Rb4bgluc*
   p4homair* b4bldl* b4bhdl* Rb4btrig* Rb4pwhr*];
  [AL@0 SYM@0 PARA@0 HPA@0 INFL@0 CARD@0 GLUC@0 LIPID@0];", sep = "\n")

output$MGSexBiFacConfig <- update(output$BiFac,
  TITLE = ~ "CFA for BiFactor of biomarkers, sex config;",
  VARIABLE = ~ . + "
  GROUPING = b1pgender (0 = male 1 = female);",
  MODEL = substitute(~ . + x, list(x = tmp)))


## output$MGAL <- update(output$AL,
##   TITLE = ~ "CFA for AL of biomarkers, age config, metric, scalar;",
##   VARIABLE = ~ . + "
##   GROUPING = Agecat (1 = young 2 = middle 3 = old);",
##   MODEL = ~ . + "
##     MODEL YOUNG:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     Rb4bgluc@0;
##     MODEL MIDDLE:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     Rb4bgluc@0;
##     MODEL OLD:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     lipid@0;")

## output$MGMedAL <- update(output$AL,
##   TITLE = ~ "CFA for AL of biomarkers, medication config, metric, scalar;",
##   VARIABLE = ~ . + "
##   GROUPING = ALMedGroup (0 = med 1 = nomed);",
##   MODEL = ~ . + "
##     MODEL MED:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     Rb4bgluc@0;
##     MODEL NOMED:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     lipid@0;")

## output$MGRAL <- update(output$AL,
##   TITLE = ~ "CFA for AL of biomarkers, race config, metric, scalar;",
##   VARIABLE = ~ . + "
##   GROUPING = white (0 = nonwhite 1 = white);",
##   MODEL = ~ . + "
##     MODEL nonwhite:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     Rb4bgluc@0;
##     MODEL white:
##     [lipid infl gluc sym para hpa card] (m1-m7);
##     lipid@0;")

return(output)
}

myruns <- function(models, pre) {
  output <- lapply(names(models), function(n) {
    mplusModeler(models[[n]], paste0(pre, "_", n, ".dat"), run = TRUE)
  })
  names(output) <- names(models)

  res <- do.call(cbind, lapply(output, fit))
  colnames(res) <- names(output)

  return(list(Models = output, Fit = res))
}

m.body.allcov <- mymodels(
mplusObject(
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
    b1pgender b1page_m2;
    !CLUSTER = m2famnum;
    !idvariable = m2id;
",
  ANALYSIS = "
    !TYPE = COMPLEX;
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    Radj_epi Radj_nor ON b1pgender b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1pgender b1page_m2;
    Rb4p1d ON b1pgender b1page_m2;
    Radj_crt b4bdheas ON b1pgender b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON b1pgender b1page_m2;
    Rb4p1gs pulpress ON b1pgender b1page_m2;
    Rb4bgluc p4homair b4bha1c ON b1pgender b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON b1pgender b1page_m2;
  ",
  OUTPUT = "STDYX;",
  usevariables = c(unlist(vars), "m2famnum", "m2id", "b1pgender", "b1page_m2", "Agecat", "ALMedGroup", "white"),
    rdata = within(fdat, {b1pgender <- as.numeric(b1pgender) - 1})))

m.allcov <- myruns(m.body.allcov[c(1:6, 8)], "factor_allcov")

m.body.allcov.clustering <- mymodels(
mplusObject(
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
    b1pgender b1page_m2;
    CLUSTER = m2famnum;
    idvariable = m2id;
",
  ANALYSIS = "
    TYPE = COMPLEX;
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    Radj_epi Radj_nor ON b1pgender b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1pgender b1page_m2;
    Rb4p1d ON b1pgender b1page_m2;
    Radj_crt b4bdheas ON b1pgender b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON b1pgender b1page_m2;
    Rb4p1gs pulpress ON b1pgender b1page_m2;
    Rb4bgluc p4homair b4bha1c ON b1pgender b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON b1pgender b1page_m2;
  ",
  OUTPUT = "
    STDYX;
    CINTERVAL;",
  usevariables = c(unlist(vars), "m2famnum", "m2id", "b1pgender", "b1page_m2", "Agecat", "ALMedGroup", "white",
     "p4majorconditions", "p4minorconditions", "p4sumburden", "b3tem", "exec_fxn", "compression",
      "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
    rdata = within(fdat, {b1pgender <- as.numeric(b1pgender) - 1})))

## Accomodation for Sex invariance because do not want to covary
## for it as well
m.body.allcov.clustering[c("MGSexBiFacConfigMetricScalar",
"MGSexBiFacConfigMetric", "MGSexBiFacConfig")] <- mymodels(
mplusObject(
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
    b1page_m2;
    CLUSTER = m2famnum;
    idvariable = m2id;
",
  ANALYSIS = "
    TYPE = COMPLEX;
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    Radj_epi Radj_nor ON b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1page_m2;
    Rb4p1d ON b1page_m2;
    Radj_crt b4bdheas ON b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON b1page_m2;
    Rb4p1gs pulpress ON b1page_m2;
    Rb4bgluc p4homair b4bha1c ON b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON b1page_m2;
  ",
  OUTPUT = "
    STDYX;
    CINTERVAL;",
  usevariables = c(unlist(vars), "m2famnum", "m2id", "b1pgender", "b1page_m2", "Agecat", "ALMedGroup", "white",
     "p4majorconditions", "p4minorconditions", "p4sumburden", "b3tem", "exec_fxn", "compression",
      "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
    rdata = within(fdat, {b1pgender <- as.numeric(b1pgender) - 1})))[c("MGSexBiFacConfigMetricScalar",
"MGSexBiFacConfigMetric", "MGSexBiFacConfig")]

m.allcov.clustering <- myruns(m.body.allcov.clustering, "factor_allcov_clustering")
saveRDS(m.allcov.clustering, file = "m_allcov_clustering.RDS")
#m.allcov.clustering <- readRDS("m_allcov_clustering.RDS")

options(width=160)

#write.table(m.allcov$Fit, "clipboard", sep = "\t")
write.table(m.allcov.clustering$Fit, "clipboard", sep = "\t")




# AL vs. correlated
compareModels(m.allcov.clustering$Models$Fac$results, m.allcov.clustering$Models$AL$results, show = 'summaries', diffTest=TRUE)

# All vs. bifactor
compareModels(m.allcov.clustering$Models$BiFac$results, m.allcov.clustering$Models$One$results, show = 'summaries', diffTest=TRUE)
compareModels(m.allcov.clustering$Models$BiFac$results, m.allcov.clustering$Models$Fac$results, show = 'summaries', diffTest=TRUE)
compareModels(m.allcov.clustering$Models$BiFac$results, m.allcov.clustering$Models$AL$results, show = 'summaries', diffTest=TRUE)

# Age Invariance
compareModels(m.allcov.clustering$Models$MGBiFacConfigMetric$results,
              m.allcov.clustering$Models$MGBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)

# Medication Invariance
compareModels(m.allcov.clustering$Models$MGMedBiFacConfig$results,
              m.allcov.clustering$Models$MGMedBiFacConfigMetric$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGMedBiFacConfig$results,
              m.allcov.clustering$Models$MGMedBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGMedBiFacConfigMetric$results,
              m.allcov.clustering$Models$MGMedBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)


with(coef(m.allcov.clustering$Models$MGMedBiFacConfig, param = "loading", type = "stdyx"),
     median(abs(MED$est - NOMED$est)))

with(coef(m.allcov.clustering$Models$MGMedBiFacConfig, param = "loading", type = "stdyx"),
     cbind.data.frame(N = MED$Label, Diff = abs(MED$est - NOMED$est), MED$est, NOMED$est))


# Race/Ethnicity Invariance
compareModels(m.allcov.clustering$Models$MGRBiFacConfig$results,
              m.allcov.clustering$Models$MGRBiFacConfigMetric$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGRBiFacConfig$results,
              m.allcov.clustering$Models$MGRBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGRBiFacConfigMetric$results,
              m.allcov.clustering$Models$MGRBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)

# Sex Invariance
compareModels(m.allcov.clustering$Models$MGSexBiFacConfig$results,
              m.allcov.clustering$Models$MGSexBiFacConfigMetric$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGSexBiFacConfig$results,
              m.allcov.clustering$Models$MGSexBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)

compareModels(m.allcov.clustering$Models$MGSexBiFacConfigMetric$results,
              m.allcov.clustering$Models$MGSexBiFacConfigMetricScalar$results,
              show = 'summaries', diffTest=TRUE)



## All Parameter Estimates from Final Models ##
l1 <- m.allcov.clustering$Models$BiFac$results$parameters$stdyx
l1 <- l1[grepl("\\.BY", l1$paramHeader), c("paramHeader", "param", "est", "se", "pval")]
l1.conf <- m.allcov.clustering$Models$BiFac$results$parameters$ci.stdyx.standardized
l1.conf <- l1.conf[grepl("\\.BY", l1.conf$paramHeader), c("paramHeader", "param", "low2.5", "est", "up2.5")]

l1.all <- cbind(l1.conf, l1[, c("se", "pval")])

l1.all$Final <- sprintf("%0.2f%s [%0.2f, %0.2f]",
                        l1.all$est,
                        pval.stars(l1.all$pval),
                        l1.all$low2.5,
                        l1.all$up2.5)

l1.all.a <- l1.all[1:24, c("paramHeader", "param", "Final")]
l1.all.b <- l1.all[25:47, c("paramHeader", "param", "Final")]
l1.all.all <- merge(l1.all.a, l1.all.b, by = "param", all = TRUE)


bifac.alt <- readModels("factor_allcov_clustering_bifac_alt.out")
l2 <- bifac.alt$parameters$stdyx
l2 <- l2[grepl("\\.BY", l2$paramHeader), c("paramHeader", "param", "est", "se", "pval")]
l2.conf <- bifac.alt$parameters$ci.stdyx.standardized
l2.conf <- l2.conf[grepl("\\.BY", l2.conf$paramHeader), c("paramHeader", "param", "low2.5", "est", "up2.5")]

l2.all <- cbind(l2.conf, l2[, c("se", "pval")])

l2.all$Final <- sprintf("%0.2f%s [%0.2f, %0.2f]",
                        l2.all$est,
                        pval.stars(l2.all$pval),
                        l2.all$low2.5,
                        l2.all$up2.5)

l2.all.a <- l2.all[1:18, c("paramHeader", "param", "Final")]
l2.all.b <- l2.all[19:35, c("paramHeader", "param", "Final")]
l2.all.all <- merge(l2.all.a, l2.all.b, by = "param", all = TRUE)


l3 <- subset(m.allcov.clustering$Models$MGMedBiFacConfig$results$parameters$stdyx.standardized, Group == "NOMED")
l3 <- l3[grepl("\\.BY", l3$paramHeader), c("paramHeader", "param", "est", "se", "pval")]
l3.conf <- subset(m.allcov.clustering$Models$MGMedBiFacConfig$results$parameters$ci.stdyx.standardized, Group == "NOMED")
l3.conf <- l3.conf[grepl("\\.BY", l3.conf$paramHeader), c("paramHeader", "param", "low2.5", "est", "up2.5")]

l3.all <- cbind(l3.conf, l3[, c("se", "pval")])

l3.all$Final <- sprintf("%0.2f%s [%0.2f, %0.2f]",
                        l3.all$est,
                        pval.stars(l3.all$pval),
                        l3.all$low2.5,
                        l3.all$up2.5)

l3.all.a <- l3.all[1:24, c("paramHeader", "param", "Final")]
l3.all.b <- l3.all[25:47, c("paramHeader", "param", "Final")]
l3.all.all <- merge(l3.all.a, l3.all.b, by = "param", all = TRUE)

colnames(l1.all.all) <- colnames(l2.all.all) <- colnames(l3.all.all)<- c("param", "fHeader", "fFinal", "alHeader", "alFinal")


lall <- merge(l1.all.all, l2.all.all, by = c("param", "fHeader"), all = TRUE)
lall <- merge(lall, l3.all.all, by = c("param", "fHeader"), all = TRUE)

lall <- lall[order(lall$fHeader, decreasing = TRUE), c("param", "fHeader", "fFinal.x", "alFinal.x", "fFinal.y", "alFinal.y", "fFinal", "alFinal")]
write.table(lall, "clipboard", row.names = FALSE, sep = "\t")


tmp <- readModels("factor_allcov_clustering_bifac_corrtech.out")
rtmp <- tmp$tech4$latCorEst[1:8, 1:8]
rtmp[upper.tri(rtmp)] <- t(rtmp)[upper.tri(rtmp)]
i <- c(1, 7, 2, 6, 3, 4, 5, 8)
rtmp <- rtmp[i, i]

rtmp[] <- as.character(round(rtmp, 2))
diag(rtmp) <- " - "
rtmp[upper.tri(rtmp)] <- ""
rtmp[] <- gsub("0\\.", "\\.", rtmp)
rownames(rtmp) <- paste0(1:8, ". ", c("SNS", "PSNS", "HPA", "Inflammation",
                                      "Cardiovascular", "Glucose", "Lipids",
                                      "AL"))
colnames(rtmp) <- paste0(1:8, ".")

write.table(rtmp, "clipboard", sep = "\t")


################################################################################
#                          Item Parameter Invariance                           #
################################################################################

ipv_extract <- function(x) {
  ##  x <- x$parameters$ci.stdyx.standardized
  if ("Group" %in% colnames(x)) {
    x2 <- x[grepl("\\.BY", x$paramHeader),
            c("paramHeader", "param", "low2.5", "est", "up2.5", "Group")]
  } else {
    x2 <- x[grepl("\\.BY", x$paramHeader),
            c("paramHeader", "param", "low2.5", "est", "up2.5")]
  }

  homa.repeat <- which(x2$paramHeader == "LIPID.BY" & x2$param == "P4HOMAIR")
  if (length(homa.repeat)) {
    x2 <- x2[-homa.repeat, ]
  }

  al.index <- which(x2$paramHeader %in% c("AL.BY"))
  x2[, "Side"] <- factor(ifelse(1:nrow(x2) %in% al.index,
                                "Allostatic Load", "System-Specific"),
                         levels = c("System-Specific", "Allostatic Load"))

  x2[, "paramHeader2"] <- gsub("\\.BY", "", x2[, "paramHeader"])
  tmp <- x2[-c(al.index), c("param", "paramHeader2")]
  x2[al.index, "paramHeader2"] <- x2[-c(al.index),
     c("paramHeader2")][match(x2[al.index, "param"],
                              x2[-c(al.index), c("param")])]

  if ("Group" %in% colnames(x)) {
    colnames(x2) <- c("paramHeader", "param", "LL", "Est", "UL", "Group", "Side", "System")
  } else {
    colnames(x2) <- c("paramHeader", "param", "LL", "Est", "UL", "Side", "System")
  }

  return(x2)
}

key.table <- data.frame(
  Labels = c(
    "RADJ_EPI", "RADJ_NOR",
    "AVGB_SD", "AVGB_RM", "AVGB_LF", "AVGB_HF", "RB4P1D",
    "RADJ_CRT", "B4BDHEAS",
    "RB4BCRP", "B4BIL6", "B4BFGN", "B4BSESEL", "B4BSICAM",
    "PULPRESS", "RB4P1GS",
    "RB4BGLUC", "B4BHA1C", "P4HOMAIR",
    "RB4PWHR", "B4BHDL", "B4BLDL", "RB4BTRIG"
    ),
  Labels2 = c(
    "E", "NE",
    "SDRR", "RMSSD", "LFHRV", "HFHRV", "Pulse Rate",
    "Cortisol", "DHEA-S",
    "CRP", "IL6", "Fibrinogen", "sE-Selectin","sICAM-1",
    "Pulse Pressure", "SBP",
    "Glucose", "HbA1c", "HOMA-IR",
    "WHR", "HDL", "LDL", "Triglycerides"
    ), stringsAsFactors = FALSE)

key.table$Number <- as.numeric(factor(key.table$Labels,
                                      levels = rev(key.table$Labels)))


ipinv <- readModels(target = "AL_bifac_ipv")

ipinv.all <- do.call(rbind, lapply(names(ipinv), function(n) {
  cbind(Model = n, ipv_extract(ipinv[[n]]$parameters$ci.stdyx.standardized))
}))

bifac.orig <- cbind(Model = "All", ipv_extract(m.allcov.clustering$Models$BiFac$results$parameters$ci.stdyx.standardized))

ipinv.all <- rbind(bifac.orig, ipinv.all)
ipinv.all$System <- factor(ipinv.all$System,
                           levels = c("LIPID", "GLUC", "CARD", "PARA", "SYM", "HPA", "INFL"),
                           labels = c("Lipids", "Glucose", "Cardiovascular",
                             "PSNS", "SNS", "HPA", "Inflammation"))

ipinv.all <- ipinv.all[order(ipinv.all$System, ipinv.all$Side, ipinv.all$Model), ]


ipinv.all$paramn <- as.numeric(factor(ipinv.all$param, levels = rev(key.table$Labels))) +
  ifelse(ipinv.all$Model == "All", .2, -.2)

ipinv.all$Model <- factor(ipinv.all$Model, levels = c(
                                             "All",
                                             "factor_allcov_clustering_bifac_nocard.out",
                                             "factor_allcov_clustering_bifac_nogluc.out",
                                             "factor_allcov_clustering_bifac_nohpa.out",
                                             "factor_allcov_clustering_bifac_noinfl.out",
                                             "factor_allcov_clustering_bifac_nolipid.out",
                                             "factor_allcov_clustering_bifac_nopara.out",
                                             "factor_allcov_clustering_bifac_nosym.out"),
                          labels = c("Bi-factor",
                            "No Cardiovascular",
                            "No Glucose",
                            "No HPA",
                            "No Inflammation",
                            "No Lipids",
                            "No PSNS",
                            "No SNS"))

p.ipinv <- ggplot(ipinv.all, aes(y = paramn, yend = paramn, x = LL, xend = UL, colour = Model,
                      shape = Model)) +
  scale_y_continuous("", breaks = key.table$Number, label = key.table$Labels2) +
  scale_shape_manual(values =  c("Bi-factor" = 17,
                            "No Cardiovascular" = 16,
                            "No Glucose" = 0,
                            "No HPA" = 3,
                            "No Inflammation" = 4,
                            "No Lipids" = 15,
                            "No PSNS" = 7,
                            "No SNS" = 8)) +
  scale_colour_manual(values =  c("Bi-factor" = "black",
                            "No Cardiovascular" = "#1b9e77",
                            "No Glucose" = "#e6ab02",
                            "No HPA" = "#7570b3",
                            "No Inflammation" = "#e7298a",
                            "No Lipids" = "#66a61e",
                            "No PSNS" = "#a6761d",
                            "No SNS" = "#d95f02")) +
  geom_segment() + geom_point(aes(x = Est)) +
  facet_grid(. ~ Side) +
  theme_bw() + theme(legend.key.width = unit(1, "cm")) +
  xlab("Standardized Loadings + 95% CI") +
  coord_cartesian(xlim = c(-1.05, 1.05))

png(filename = "item_parameter_invariance.png", width = 8, height = 5, units = "in", res = 1200)
print(p.ipinv)
dev.off()

##setEPS()
##postscript(file = "item_parameter_invariance.eps", width = 8, height = 5)
pdf(file = "figure_3_item_parameter_invariance.pdf", width = 8, height = 5)
print(p.ipinv)
dev.off()

#### Multiple Group Results ####

#### Medications
med.items.all <- ipv_extract(m.allcov.clustering$Models$MGMedBiFacConfig$results$parameters$ci.stdyx.standardized)

med.items.all$System <- factor(med.items.all$System,
  levels = c("LIPID", "GLUC", "CARD", "PARA", "SYM", "HPA", "INFL"),
  labels = c("Lipids", "Glucose", "Cardiovascular", "PSNS", "SNS", "HPA", "Inflammation"))

med.items.all <- med.items.all[order(med.items.all$System, med.items.all$Side, med.items.all$Group), ]

med.items.all$paramn <- as.numeric(factor(med.items.all$param,
  levels = rev(key.table$Labels))) +
  ifelse(med.items.all$Group == "NOMED", .2, -.2)

med.items.all$Group <- factor(med.items.all$Group, levels = c(
                                             "NOMED",
                                             "MED"),
                          labels = c("No Meds", "1+ Meds"))

p.med.items.all <- ggplot(med.items.all, aes(y = paramn, yend = paramn, x = LL, xend = UL, colour = Group,
                      shape = Group)) +
  scale_y_continuous("", breaks = key.table$Number, label = key.table$Labels2) +
  scale_shape_manual(values =  c("No Meds" = 17,
                            "1+ Meds" = 16)) +
  scale_colour_manual(values =  c("No Meds" = "black",
                            "1+ Meds" = "#e6ab02")) +
  geom_segment() + geom_point(aes(x = Est)) +
  facet_grid(. ~ Side) +
  theme_bw() + theme(legend.key.width = unit(1, "cm")) +
  xlab("Standardized Loadings + 95% CI") +
  coord_cartesian(xlim = c(-1.05, 1.05))

#### Sex
sex.items.all <- ipv_extract(m.allcov.clustering$Models$MGSexBiFacConfig$results$parameters$ci.stdyx.standardized)

sex.items.all$System <- factor(sex.items.all$System,
  levels = c("LIPID", "GLUC", "CARD", "PARA", "SYM", "HPA", "INFL"),
  labels = c("Lipids", "Glucose", "Cardiovascular", "PSNS", "SNS", "HPA", "Inflammation"))

sex.items.all <- sex.items.all[order(sex.items.all$System, sex.items.all$Side, sex.items.all$Group), ]

sex.items.all$paramn <- as.numeric(factor(sex.items.all$param,
  levels = rev(key.table$Labels))) +
  ifelse(sex.items.all$Group == "FEMALE", .2, -.2)

sex.items.all$Group <- factor(sex.items.all$Group, levels = c(
                                             "FEMALE",
                                             "MALE"),
                          labels = c("Female", "Male"))

p.sex.items.all <- ggplot(sex.items.all, aes(y = paramn, yend = paramn, x = LL, xend = UL, colour = Group,
                      shape = Group)) +
  scale_y_continuous("", breaks = key.table$Number, label = key.table$Labels2) +
  scale_shape_manual(values =  c("Female" = 17,
                            "Male" = 16)) +
  scale_colour_manual(values =  c("Female" = "black",
                            "Male" = "#e6ab02")) +
  geom_segment() + geom_point(aes(x = Est)) +
  facet_grid(. ~ Side) +
  theme_bw() + theme(legend.key.width = unit(1, "cm")) +
  xlab("Standardized Loadings + 95% CI") +
  coord_cartesian(xlim = c(-1.05, 1.05))


#### Race/Ethnicity
r.items.all <- ipv_extract(m.allcov.clustering$Models$MGRBiFacConfig$results$parameters$ci.stdyx.standardized)

r.items.all$System <- factor(r.items.all$System,
  levels = c("LIPID", "GLUC", "CARD", "PARA", "SYM", "HPA", "INFL"),
  labels = c("Lipids", "Glucose", "Cardiovascular", "PSNS", "SNS", "HPA", "Inflammation"))

r.items.all <- r.items.all[order(r.items.all$System, r.items.all$Side, r.items.all$Group), ]

r.items.all$paramn <- as.numeric(factor(r.items.all$param,
  levels = rev(key.table$Labels))) +
  ifelse(r.items.all$Group == "WHITE", .2, -.2)

r.items.all$Group <- factor(r.items.all$Group, levels = c(
                                             "WHITE",
                                             "NONWHITE"),
                          labels = c("White", "Other Ethnicity"))

p.r.items.all <- ggplot(r.items.all, aes(y = paramn, yend = paramn, x = LL, xend = UL, colour = Group,
                      shape = Group)) +
  scale_y_continuous("", breaks = key.table$Number, label = key.table$Labels2) +
  scale_shape_manual(values =  c("White" = 17,
                            "Other Ethnicity" = 16)) +
  scale_colour_manual(values =  c("White" = "black",
                            "Other Ethnicity" = "#e6ab02")) +
  geom_segment() + geom_point(aes(x = Est)) +
  facet_grid(. ~ Side) +
  theme_bw() + theme(legend.key.width = unit(1, "cm")) +
  xlab("Standardized Loadings + 95% CI") +
  coord_cartesian(xlim = c(-1.05, 1.05))



pdf(file = "supplementary_figure_2_loadings_mg_medications.pdf", width = 8, height = 5)
print(p.med.items.all)
dev.off()
pdf(file = "supplementary_figure_3_loadings_mg_race.pdf", width = 8, height = 5)
print(p.r.items.all)
dev.off()
pdf(file = "supplementary_figure_4_loadings_mg_sex.pdf", width = 8, height = 5)
print(p.sex.items.all)
dev.off()








l1.conf <- m.allcov.clustering$Models$BiFac$results$parameters$ci.stdyx.standardized
l1.conf <- l1.conf[grepl("\\.BY", l1.conf$paramHeader), c("paramHeader", "param", "low2.5", "est", "up2.5")]

ipinv.c <- lapply(ipinv)



############## Imputation ###############
ALplus <- update(m.body.allcov$AL,
  VARIABLE = ~ . + "
  idvariable is m2id;",
  ANALYSIS = ~ "
  ESTIMATOR = BAYES;
  PROCESSORS = 6;
  FBITERATIONS = 3000;
  THIN = 10;
  CHAINS = 6;",
  SAVEDATA = ~ "
  DATA IMPUTATION:
  NDATASETS = 200;
  SAVE = alplus_imp*.dat;
  SAVEDATA: FILE = alplus_plaus.dat;
  SAVE = FSCORES (200);
  FACTORS = sym hpa card gluc lipid infl para AL;",
  usevariables = c(m.body.allcov$AL$usevariables, "m2id"),
  rdata = fdat)

cd(base, pre <- "factor_allcov", num <- "_ALplus")
m.ALplus <- mplusModeler(ALplus, paste0(pre, num, ".dat"), run=TRUE)
summary(m.ALplus)

BiFacplus <- update(m.body.allcov$BiFac,
  VARIABLE = ~ . + "
  idvariable is m2id;",
  ANALYSIS = ~ "
  ESTIMATOR = BAYES;
  PROCESSORS = 6;
  FBITERATIONS = 5000;
  THIN = 10;
  CHAINS = 6;",
  SAVEDATA = ~ "
  DATA IMPUTATION:
  NDATASETS = 200;
  SAVE = bialplus_imp*.dat;
  SAVEDATA: FILE = bialplus_plaus.dat;
  SAVE = FSCORES (200);
  FACTORS = sym hpa card gluc lipid infl para F;",
  usevariables = c(m.body.allcov$BiFac$usevariables, "m2id"),
  rdata = fdat)

cd(base, pre <- "factor_allcov", num <- "_BiFacplus")
m.BiFacplus <- mplusModeler(BiFacplus, paste0(pre, num, ".dat"), run=TRUE)
summary(m.BiFacplus)


imputed.data.vars <- c(
  "Radj_epi", "Radj_nor", "avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf",
  "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam",
  "b4bsesel", "pulpress", "Rb4p1gs", "Rb4p1d", "b4bha1c",
  "Rb4bgluc", "p4homair", "b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr",
  "b1pgender", "b1page_m2", "m2id")
bi.vars <- c("bi.sym", "bi.hpa", "bi.card", "bi.gluc", "bi.lipid", "bi.infl", "bi.para", "F")
al.vars <- c("al.sym", "al.hpa", "al.card", "al.gluc", "al.lipid", "al.infl", "al.para", "AL")

fdat.merge <- fdat[rowSums(is.na(fdat[, unlist(vars)])) != 23, ]
#fdat.merge <- subset(fdat.merge, select = -match(imputed.data.vars[-(24:25)], colnames(fdat.merge)))

al.bi.data <- lapply(1:200, function(i) {
  al.d <- read.table(paste0("~/OneDrive/Projects/MIDUS/Setup/AL_Measurement/factor_allcov_alplus/alplus_imp", i, ".dat"), na.strings = "*")
  bi.d <- read.table(paste0("~/OneDrive/Projects/MIDUS/Setup/AL_Measurement/factor_allcov_BiFacplus/bialplus_imp", i, ".dat"), na.strings = "*")
  colnames(al.d) <- c(imputed.data.vars, al.vars)
  colnames(bi.d) <- c(imputed.data.vars, bi.vars)
  cbind(Imputation = i, fdat.merge, al.d[, al.vars], bi.d[, bi.vars])
})
