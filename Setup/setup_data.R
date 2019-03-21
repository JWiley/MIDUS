
if (TRUE) {
  source("setup_packages_functions.R")
  options(stringsAsFactors = FALSE)
}


################################################################################
##                                                                            ##
##                             Master Variable List                           ##
##                                                                            ##
################################################################################

mVars <- read.xlsx("midus_master_vars.xlsx", sheetIndex = 1)

## check for any duplicates
apply(mVars[, -(1:4)], 2, function(x) {x <- na.omit(x); if(anyDuplicated(x)) x[duplicated(x)]})


################################################################################
##                                                                            ##
##                             Loading Data                                   ##
##                                                                            ##
################################################################################

## MIDUS Wave 1
M1 <- fread("~/OneDrive/Projects/MIDUS/Data/ICPSR_02760/DS0001/02760-0001-Data.tsv",
            header=TRUE, sep = '\t', stringsAsFactors = FALSE)

M1[, A1PIDATE := as.Date(paste0(A1PI_YR, "-", A1PI_MO, "-15"), format = "%Y-%m-%d")]
M1[, A1PBWORK := ifelse(A1PB3A == 1, 1,
                   ifelse(A1PB3B == 1, 2,
                     ifelse(A1PB3C == 1, 3,
                       ifelse(A1PB3D == 1, 4,
                         ifelse(A1PB3E == 1, 5,
                           ifelse(A1PB3F == 1, 6,
                             ifelse(A1PB3G == 1, 7,
                               ifelse(A1PB3H == 1, 8,
                                 ifelse(A1PB3I == 1, 9,
                                   ifelse(A1PB3J == 1, 10,
                                      ifelse(A1PB3K == 1, 11, NA)))))))))))
   ]

v <- paste0("A1SC1", LETTERS[1:8])
M1[, (v) := lapply(.SD, cleanVar, inlist = 1:2), .SDcols = v]
M1[, A1SC1 := ifelse(rowMeans(M1[, v, with = FALSE] == 1, na.rm = TRUE) > 0,
                1L,
                ifelse(rowSums(is.na(M1[, v, with = FALSE])) == 8,
                  NA_integer_,
                  2L))]

varsIn(mVars$M1, M1)
M1 <- M1[, chooseVars(mVars$M1, drop = c("A1SLFEDI", "A1SDAYDI", "MFAbuse")), with = FALSE]

## MIDUS Wave 2 Project 1
M2 <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_04652/DS0001/04652-0001-Data.tsv",
            header=TRUE, sep = '\t', stringsAsFactors = FALSE)

varsIn(mVars$M2, M2)
M2 <- M2[, chooseVars(mVars$M2, drop = c("LifeStress", "LifeStressImpactS", "LifeStressImpactL")), with = FALSE]

## Milwaukee Sample
MA <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_22840/DS0001/22840-0001-Data-REST.tsv",
            header=TRUE, sep = '\t', stringsAsFactors = FALSE)
MA[, M2FAMNUM := M2ID]
MA[, SAMPLMAJ := 13] ## milwaukee
MA[, BACBWORK := ifelse(BACB3A == 1, 1,
                   ifelse(BACB3B == 1, 2,
                     ifelse(BACB3C == 1, 3,
                       ifelse(BACB3D == 1, 4,
                         ifelse(BACB3E == 1, 5,
                           ifelse(BACB3F == 1, 6,
                             ifelse(BACB3G == 1, 7,
                               ifelse(BACB3H == 1, 8,
                                 ifelse(BACB3I == 1, 9,
                                   ifelse(BACB3J == 1, 10,
                                      ifelse(BACB3K == 1, 11, NA)))))))))))
                            ]
MA[, BACBADL1 := NA_real_]
MA[, BACFDSOL := (ifelse(BACFDSPO < 7, BACFDSPO, NA_real_) +
                  ifelse(BACFDSNE < 7, (5 - BACFDSNE), NA_real_)) / 2]

varsIn(mVars$MA, MA)
MA <- MA[, chooseVars(mVars$MA, drop = c("LifeStress", "LifeStressImpactS", "LifeStressImpactL")), with = FALSE]

setnames(MA, names(MA), names(M2))

## Merge M2 and MA
M2 <- rbind(M2, MA)

## Daily Stress / Experiences (P3)
M2P2 <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_26841/DS0001/26841-0001-Data.tsv",
              header=TRUE, sep = '\t', stringsAsFactors = FALSE)
varsIn(mVars$M2P2, M2P2)
M2P2 <- M2P2[, chooseVars(mVars$M2P2), with = FALSE]

## MIDUS Wave 2 Cognitive Project (P3)
M2P3 <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_25281/DS0001/25281-0001-Data.tsv",
              header = TRUE, sep = "\t", stringsAsFactors=FALSE)
M2P3[, B3PIDATE := as.Date(paste0(B3PIDATE_YR, "-", B3PIDATE_MO, "-15"), format = "%Y-%m-%d")]

varsIn(mVars$M2P3, M2P3)
M2P3 <- M2P3[, chooseVars(mVars$M2P3), with = FALSE]

## Biomarker Project Sample (P4)
M2P4 <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_29282/DS0001/29282-0001-Data.tsv",
              header=TRUE, sep = '\t', stringsAsFactors = FALSE)
M2P4[, M2FAMNUM := ifelse(is.na(M2FAMNUM), M2ID, M2FAMNUM)]

varsIn(mVars$M2P4, M2P4)
M2P4 <- M2P4[, chooseVars(mVars$M2P4, drop = c("B4CTQTotal")), with = FALSE]


## MIDUS Wave 3 Project 1
M3 <- fread("~/Onedrive/Projects/MIDUS/Data/ICPSR_36346/DS0001/36346-0001-Data.tsv",
            header = TRUE, sep = "\t", stringsAsFactors=FALSE)
M3[, C1PIDATE := as.Date(paste0(C1PIDATE_YR, "-", C1PIDATE_MO, "-15"), format = "%Y-%m-%d")]

varsIn(mVars$M3, M3)
M3 <- M3[, chooseVars(mVars$M3), with = FALSE]

## MIDUS Wave 3 Project 3
M3P3 <- as.data.table(read.spss("~/Onedrive/Projects/MIDUS/Data/February2017/Data/M3_P3_BTACT_N2693_20160927.sav", to.data.frame=TRUE))
## M3P3[, C3PIDATE := as.Date(paste0(C3PIDATE_YR, "-", C3PIDATE_MO, "-15"), format = "%Y-%m-%d")]

varsIn(mVars$M3P3, M3P3)
M3P3 <- M3P3[, chooseVars(mVars$M3P3), with = FALSE]
M3P3[, (names(M3P3)[1:4]) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = names(M3P3)[1:4]]

## MIDUS 3 Mortality codes
M3Mort <- as.data.table(read.spss("~/OneDrive/Projects/MIDUS/Data/February2018/MIDUS_MortalityCauseData_N1382_20180307.sav",
                    to.data.frame = TRUE))
M3Mort[, M2FAMNUM := ifelse(is.na(M2FAMNUM), M2ID, M2FAMNUM)]
M3Mort[, DOD_M := as.integer(as.character(DOD_M))]
M3Mort[, DOD_Y := as.integer(as.character(DOD_Y))]
M3Mort[, DECEASED := NULL]
M3Mort[, DECEASED := 1L]
M3Mort[, SAMPLMAJ := as.integer(as.character(factor(SAMPLMAJ, levels = c("MAIN RDD", "SIBLING", "TWIN", "CITY OVERSAMPLE", "MILWAUKEE",
"BOSTON NEW"), labels = c("1", "2", "3", "4", "13", "999"))))]

varsIn(mVars$M3Mort, M3Mort)
M3Mort <- M3Mort[, chooseVars(mVars$M3Mort), with = FALSE]

## MIDUS 3 disposition codes
M3Disp <- fread("~/OneDrive/Projects/MIDUS/Data/ICPSR_36346/DS0003/36346-0003-Data.tsv",
                header = TRUE, sep = "\t", stringsAsFactors=FALSE)


## MIDUS Data from Teresa Seeman Group
tmp <- list(
  dat = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/p4subset_biology_3-20-12.dta"),
  demodat = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/demographics_for_JW.dta"),
  meddat = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/p4_med_flags_10-28-12.dta"),
  refdat = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/NEW_AL_measures_WITH_MEDS_12-19-12.dta"),
  refdatnomed = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/NEW_AL_measures_NO_MEDS_12-19-12.dta"),
  cog = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/btact.dta"),
  chronic = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/m2p4chronic_2_7_11.dta"),
  ses = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/ses.dta"),
  bone = read.dta("~/OneDrive/Projects/MIDUS/Data/seeman_data/ses_zallo_bone.dta"))

## Table 2: Components used to create Childhood Advantage Score m1chad
## Indicator Variable:	Recode applied to original variable:
## m1welf (on welfare as child)	RECODE OF VARIABLE a1pc14:
## 0 = yes
## 2 = no
## m1cses (financial level growing up)	RECODE OF VARIABLE a1se9:
## 0 = worse off than others
## 1 = same as others
## 2 = better off than others
## m1ped3 (parent highest education)	RECODE OF VARIABLE m1phed:
## 0 = < High School
## 1 = High School/GED
## 2 = Some College+
alpha(psych::polychoric(tmp$ses[, c("m1welf_all", "m1ped3_all", "m1cses_all")])$rho)
tmp$ses$m1chadv <- rowMeans(tmp$ses[, c("m1welf_all", "m1ped3_all", "m1cses_all")], na.rm = TRUE) * 3

## Table 4: Components used to create M2 Adult Advantage Score m2aadv
## Indicator Variable:	Recode applied to original variable:
## m2ed3 (participant education)	RECODE OF VARIABLE m2eddg:
## 0 = high school/GED
## 1 = some college/AA
## 2 = college degree or greater
## m2fpir3 (family-adjusted poverty to income ratio)	RECODE OF VARIABLE m2fpir:
## 0 = <300%
## 1 = 300-599%
## 2 = ≥600%
## m2qf1 (current financial situation)	RECODE OF VARIABLE b1sg1:
## 0 = worst
## 1 = average
## 2 = best
## m2qf6 (enough money to meet needs)	RECODE OF VARIABLE b1sg6:
## 0=not enough
## 1=just enough
## 2=more than enough
## m2qf7 (difficulty paying bills)	RECODE OF VARIABLE b1sg7:
## 0=very or somewhat difficult
## 1=not very difficult
## 2=not difficult at all
alpha(psych::polychoric(tmp$ses[, c("m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all")])$rho)
tmp$ses$m2aadv <- rowMeans(tmp$ses[, c("m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all")], na.rm = TRUE) * 5


TM2P4 <- merge(tmp$dat, tmp$meddat, by = "m2id", all = TRUE)
TM2P4 <- merge(TM2P4, tmp$demodat, by = "m2id", all = TRUE)
TM2P4 <- merge(TM2P4, tmp$chronic[, c("m2id", "p4majorconditions", "p4minorconditions", "p4sumburden")], by = "m2id", all = TRUE)
TM2P4 <- merge(TM2P4, tmp$bone[, c("m2id", "compression", "bending", "impact")], by = "m2id", all = TRUE)
TM2P4 <- merge(TM2P4, tmp$refdat[, c("m2id", "RXNNregAL")], by = "m2id", all = TRUE)
TM2P4 <- merge(TM2P4, tmp$refdatnomed[, c("m2id", "GNNregAL")], by = "m2id", all = TRUE)
TM2P4 <- as.data.table(TM2P4)

setnames(TM2P4, c("m2id", "m2famnum", "samplmaj"), c("M2ID", "M2FAMNUM", "SAMPLMAJ"))

varsIn(mVars$TM2P4, TM2P4)
TM2P4 <- TM2P4[, chooseVars(mVars$TM2P4), with = FALSE]
TM2P4[, SAMPLMAJ := NULL]
TM2P4[, M2FAMNUM := NULL]

TSES <- as.data.table(tmp$ses)
setnames(TSES, "m2id", "M2ID")
varsIn(mVars$TSES, TSES)
TSES <- TSES[, c(chooseVars(mVars$TSES), "m1chadv", "m2aadv"), with = FALSE]

TCOG <- as.data.table(tmp$cog)
setnames(TCOG, "m2id", "M2ID")
varsIn(mVars$TCOG, TCOG)
TCOG <- TCOG[, chooseVars(mVars$TCOG), with = FALSE]


################################################################################
##                                                                            ##
##                            Merging Data (Wide)                             ##
##                                                                            ##
################################################################################

dw <- merge(M1, M2, by = c("M2ID", "M2FAMNUM", "SAMPLMAJ"), all = TRUE)
dw <- merge(dw, M2P3[, -c("SAMPLMAJ", "M2FAMNUM"), with=FALSE], by = c("M2ID"), all = TRUE)
dw <- merge(dw, M2P4, by = c("M2ID", "M2FAMNUM", "SAMPLMAJ"), all = TRUE)
dw <- merge(dw, M3[, -"M2FAMNUM", with=FALSE], by = c("M2ID", "SAMPLMAJ"), all = TRUE)
dw <- merge(dw, M3P3[, -c("SAMPLMAJ", "M2FAMNUM"), with=FALSE], by = c("M2ID"), all = TRUE)
dw <- merge(dw, M3Mort, by = c("M2ID", "M2FAMNUM", "SAMPLMAJ"), all = TRUE)
dw <- merge(dw, TM2P4, by = c("M2ID"), all = TRUE)
dw <- merge(dw, TSES, by = c("M2ID"), all = TRUE)
dw <- merge(dw, TCOG, by = c("M2ID"), all.x = TRUE)

dim(dw)

################################################################################
##                                                                            ##
##                           Overall Birth Year and Age                       ##
##                                                                            ##
################################################################################

dw[, A1PBYEAR := cleanVar(A1PBYEAR, LL = 1900, 2000)]
dw[, B1PBYEAR := cleanVar(B1PBYEAR, LL = 1900, 2000)]
dw[, C1PBYEAR := cleanVar(C1PBYEAR, LL = 1900, 2000)]

## age
v <- c("A1PAGE_M2", "B1PAGE_M2", "C1PRAGE")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 95), .SDcols = v]

dw[, B1PIDATE := as.Date(B1PIDATE, format = "%d-%b-%Y")]

## summary(lm(A1PBYEAR ~ I(year(A1PIDATE) - A1PAGE_M2), data = dw))
## summary(lm(B1PBYEAR ~ I(year(B1PIDATE) - B1PAGE_M2), data = dw))
## summary(lm(C1PBYEAR ~ I(year(C1PIDATE) - C1PRAGE), data = dw))
## summary(dw[, B1PAGE_M2 - A1PAGE_M2])
## summary(dw[, C1PRAGE - B1PAGE_M2])
## summary(dw[, C1PRAGE - A1PAGE_M2])
## xtabs(~is.na(A1PAGE_M2) + is.na(B1PAGE_M2) + is.na(C1PRAGE), data = dw)

dw[, BirthYear := recursiveFillNA(
       A1PBYEAR, B1PBYEAR, C1PBYEAR,
       year(A1PIDATE) - A1PAGE_M2,
       year(B1PIDATE) - B1PAGE_M2,
       year(C1PIDATE) - C1PRAGE)]

## plot(BirthYear ~ I(year(A1PIDATE) - A1PAGE_M2), data =dw)
## plot(A1PAGE_M2 ~ I(year(A1PIDATE) - BirthYear), data =dw)
## plot(BirthYear ~ I(year(B1PIDATE) - B1PAGE_M2), data =dw)
## plot(BirthYear ~ I(year(C1PIDATE) - C1PRAGE), data =dw)

## age: note that the median and mean between M1 and M2 is 9 years
dw[, AGEM1 := recursiveFillNA(
       A1PAGE_M2,
       year(A1PIDATE) - BirthYear,
       B1PAGE_M2 - 9,
       C1PRAGE - 18)]

dw[, AGEM2 := recursiveFillNA(
       B1PAGE_M2,
       year(B1PIDATE) - BirthYear,
       A1PAGE_M2 + 9,
       C1PRAGE - 9)]

dw[, AGEM3 := recursiveFillNA(
       C1PRAGE,
       year(C1PIDATE) - BirthYear,
       A1PAGE_M2 + 18,
       B1PAGE_M2 + 9)]

## Fix one weird age
## plot(A1PAGE_M2 ~ I(year(A1PIDATE) - BirthYear), data = dw, col = (M2ID == 13807) + 2)
dw[M2ID==13807, AGEM1 := year(A1PIDATE) - BirthYear]
dw[M2ID==13807, AGEM2 := year(B1PIDATE) - BirthYear]
dw[M2ID==13807, AGEM3 := AGEM2 + 9]


################################################################################
##                                                                            ##
##                         Allostatic Load - Biomarkers                       ##
##                                                                            ##
################################################################################

med.vars <- c("fg_bpup", "fg_bpdwn", "fg_hrdwn", "fg_hrup",
  "fg_hpaup", "fg_sympdwn", "fg_sympup", "fg_paradwn",
  "fg_paraup", "fg_infldwn", "fg_influp", "fg_glucdwn",
  "fg_glucup", "fg_rxchol")

vars <- list(
  sympathetic = c("Radj_epi", "Radj_nor"),
  parasympathetic = c("avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf", "Rb4p1d"),
  hpa = c("Radj_crt", "b4bdheas"),
  inflammation = c("Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel"),
  cardiovascular = c("pulpress", "Rb4p1gs"),
  glucose = c("b4bha1c", "Rb4bgluc", "p4homair"),
  lipid = c("b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr")
)

vars.hr <- list(
  sympathetic = c("hr25aepi", "hr25anor"),
  parasympathetic = c("hr25avgsd", "hr25avgrm", "hr25avglf", "hr25avghf", "hr25pulse"),
  hpa = c("hr25acrt", "hr25dheas"),
  inflammation = c("hr25crp", "hr25il6", "hr25fgn", "hr25icam", "hr25esel"),
  cardiovascular = c("hr25ppress", "hr25sbp"),
  glucose = c("hr25ha1c", "hr25gluc", "hr25homair"),
  lipid = c("hr25ldl", "hr25hdl", "hr25trig", "hr25bmi", "hr25whr")
)

all(unlist(vars) %in% colnames(dw))
all(unlist(vars.hr) %in% colnames(dw))

dw[, unlist(vars) := lapply(.SD, function(x) {
  ifelse(x %in% c(999999, 99999, 9999, 999, 998), NA, x)
}), .SDcols = unlist(vars)]


dw[, c(
  "Radj_epi", "Radj_nor", "avgb_sd",
  "avgb_rm", "avgb_lf", "avgb_hf",
  "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6",
  "b4bsesel", "b4bha1c", "Rb4bgluc",
  "p4homair", "Rb4btrig") := .(
  log(Radj_epi), log(Radj_nor),
  log(avgb_sd), log(avgb_rm),
  log(avgb_lf), log(avgb_hf),
  log(Radj_crt), log(b4bdheas),
  log(Rb4bcrp), log(b4bil6),
  log(b4bsesel), log(b4bha1c),
  log(Rb4bgluc),  log(p4homair), log(Rb4btrig)
  )]

dw[, unlist(vars) := lapply(.SD, function(x) {
  as.numeric(scale(winsorizor(x, percentile = .005)))
}), .SDcols = unlist(vars)]


# multivariate normality (not really)
## mvqq(dw[, unlist(vars), with = FALSE])


## p.trans <- ggplot(reshape2::melt(d2.bio.seeman$fdat[, unlist(vars)]), aes(value)) +
##   geom_histogram() +
##   facet_wrap(~variable, scales="free")
## tmplong <- reshape2::melt(d2.bio.seeman$fdat[, c("b1page_m2", unlist(vars))], id.vars="b1page_m2")
## tmplong$age <- cut(tmplong$b1page_m2, breaks = quantile(d2.bio.seeman$fdat$b1page_m2), include.lowest=TRUE)
## p.trans <- ggplot(tmplong, aes(value, y = ..density..)) +
##   geom_histogram() +
##   facet_grid(age ~ variable)
## p.scat <- ggplot(tmplong, aes(b1page_m2, value)) +
##   geom_point(alpha = .1) + stat_smooth(size=1.5) +
##   facet_wrap(~ variable) + theme_bw()
## pdf("age_bio.pdf", width = 11, height = 8.5)
## print(p.scat)
## dev.off()
## pdf("bio_distribution.pdf", width = 11, height = 8.5)
## print(p.un + theme_bw())
## print(p.trans + theme_bw())
## dev.off()

dw[, white := as.integer(m1m2race == 1)]
dw[, Agecat := as.numeric(cut(b1page_m2, breaks = c(0, 45, 60, 100)))]
dw$ALMedGroup <- ifelse(
  rowSums(dw[, med.vars, with=FALSE]) == length(med.vars), NA_integer_,
  as.integer(rowSums(dw[, med.vars, with=FALSE], na.rm = TRUE) == 0))

################################################################################
##                                                                            ##
##                            PSRs D2 & DM Setup                              ##
##                                                                            ##
################################################################################

Items <- list()

Items$PSR <- rbind.data.frame(
## sense of control; 8 & 9 = missing
  cbind(Scale = "SOC",
        M2items = c(
          "B1SE4A", # LITTLE CAN DO TO CHANGE IMPORTANT THINGS
          "B1SE4B", # HELPLESS DEALING WITH PROBLEMS OF LIFE
          "B1SE4C", # DO JUST ABOUT ANYTHING I SET MY MIND TO
          "B1SE4D", # OTHS DETERMINE WHAT I CAN AND CANNOT DO
          "B1SE4E", # WHAT HAPPENS IN LIFE IS BEYOND MY CTRL
          "B1SE4F", # WHEN REALLY WANT SOMETHING, FIND WAY
          "B1SE4G", # MANY THINGS INTERFERE W/ WHAT I WANT DO
          "B1SE4H", # WHETHER I GET WHAT WANT IS IN OWN HANDS
          "B1SE4I", # LITTLE CONTROL OVER THINGS HAPPEN TO ME
          "B1SE4J", # REALLY NO WAY I CAN SOLVE PROBS I HAVE
          "B1SE4K", # FEEL PUSHED AROUND IN LIFE
          "B1SE4L"), # HAPPENS TO ME IN FUTURE DEPENDS ON ME
        MAitems = c(
          "BASC4A", # LITTLE CAN DO TO CHANGE IMPORTANT THINGS
          "BASC4B", # HELPLESS DEALING WITH PROBLEMS OF LIFE
          "BASC4C", # DO JUST ABOUT ANYTHING I SET MY MIND TO
          "BASC4D", # OTHS DETERMINE WHAT I CAN AND CANNOT DO
          "BASC4E", # WHAT HAPPENS IN LIFE IS BEYOND MY CTRL
          "BASC4F", # WHEN REALLY WANT SOMETHING, FIND WAY
          "BASC4G", # MANY THINGS INTERFERE W/ WHAT I WANT DO
          "BASC4H", # WHETHER I GET WHAT WANT IS IN OWN HANDS
          "BASC4I", # LITTLE CONTROL OVER THINGS HAPPEN TO ME
          "BASC4J", # REALLY NO WAY I CAN SOLVE PROBS I HAVE
          "BASC4K", # FEEL PUSHED AROUND IN LIFE
          "BASC4L")), # HAPPENS TO ME IN FUTURE DEPENDS ON ME
  cbind(Scale = "SelfEsteem",
        M2items = c(
          "B1SE4M", # NO BETTER/WORSE THAN OTHERS
          "B1SE4N", # TAKE POSITIVE ATTITUDE TOWARD SELF
          "B1SE4O", # FEEL NO GOOD AT ALL AT TIMES
          "B1SE4P", # ABLE TO DO THINGS AS WELL AS MOST PEOPLE
          "B1SE4Q", # WISH HAVE MORE RESPECT FOR MYSELF
          "B1SE4R", # ON THE WHOLE, I'M SATISFIED WITH MYSELF
          "B1SE4S"), # CERTAINLY FEEL USELESS AT TIMES
        MAitems = c(
          "BASC4M", # NO BETTER/WORSE THAN OTHERS
          "BASC4N", # TAKE POSITIVE ATTITUDE TOWARD SELF
          "BASC4O", # FEEL NO GOOD AT ALL AT TIMES
          "BASC4P", # ABLE TO DO THINGS AS WELL AS MOST PEOPLE
          "BASC4Q", # WISH HAVE MORE RESPECT FOR MYSELF
          "BASC4R", # ON THE WHOLE, I'M SATISFIED WITH MYSELF
          "BASC4S")), # CERTAINLY FEEL USELESS AT TIMES
  cbind(Scale = "Optimism",
        M2items = c(
          "B1SE10A", # IN UNCERTAIN TIMES, USUALLY EXPECT BEST
          "B1SE10B", # SOMETHING CAN GO WRONG FOR ME, IT WILL
          "B1SE10C", # OPTIMISTIC ABOUT MY FUTURE
          "B1SE10D", # HARDLY EVER EXPECT THINGS TO GO MY WAY
          "B1SE10E", # RARELY COUNT ON GOOD THINGS HAPPEN TO ME
          "B1SE10F"), # EXPECT MORE GOOD THINGS HAPPEN THAN BAD
        MAitems = c(
          "BASC10A", # IN UNCERTAIN TIMES, USUALLY EXPECT BEST
          "BASC10B", # SOMETHING CAN GO WRONG FOR ME, IT WILL
          "BASC10C", # OPTIMISTIC ABOUT MY FUTURE
          "BASC10D", # HARDLY EVER EXPECT THINGS TO GO MY WAY
          "BASC10E", # RARELY COUNT ON GOOD THINGS HAPPEN TO ME
          "BASC10F")), # EXPECT MORE GOOD THINGS HAPPEN THAN BAD
  cbind(Scale = "FamilySupport",
        M2items = c(
          "B1SJ2A", # FAMILY MEMBERS REALLY CARE ABOUT YOU
          "B1SJ2B", # FAMILY MEMBERS UNDERSTAND WAY YOU FEEL
          "B1SJ2C", # RELY ON FAMILY FOR HELP WITH PROBLEM
          "B1SJ2D"), # OPEN UP TO FAMILY ABOUT WORRIES
        MAitems = c(
          "BACJS2A", # FAMILY MEMBERS REALLY CARE ABOUT YOU
          "BACJS2B", # FAMILY MEMBERS UNDERSTAND WAY YOU FEEL
          "BACJS2C", # RELY ON FAMILY FOR HELP WITH PROBLEM
          "BACJS2D")), # OPEN UP TO FAMILY ABOUT WORRIES
  cbind(Scale = "FriendSupport",
        M2items = c(
          "B1SJ4A", # FRIENDS REALLY CARE ABOUT YOU
          "B1SJ4B", # FRIENDS UNDERSTAND WAY YOU FEEL
          "B1SJ4C", # RELY ON FRIENDS FOR HELP WITH PROBLEM
          "B1SJ4D"), # OPEN UP TO FRIENDS ABOUT WORRIES
        MAitems = c(
          "BACJS4A", # FRIENDS REALLY CARE ABOUT YOU
          "BACJS4B", # FRIENDS UNDERSTAND WAY YOU FEEL
          "BACJS4C", # RELY ON FRIENDS FOR HELP WITH PROBLEM
          "BACJS4D")), # OPEN UP TO FRIENDS ABOUT WORRIES
  cbind(Scale = "SpouseSupport",
        M2items = c(
          "B1SL11A", # SP REALLY CARES ABOUT YOU
          "B1SL11B", # SP UNDERSTANDS WAY YOU FEEL
          "B1SL11C", # SP APPRECIATES YOU
          "B1SL11D", # RELY SP FOR HELP WITH SERIOUS PROBLEM
          "B1SL11E", # OPEN UP TO SP ABOUT WORRIES
          "B1SL11F"), # CAN RELAX, BE YOURSELF AROUND SP
        MAitems = c(
          "BACLS11A", # SP REALLY CARES ABOUT YOU
          "BACLS11B", # SP UNDERSTANDS WAY YOU FEEL
          "BACLS11C", # SP APPRECIATES YOU
          "BACLS11D", # RELY SP FOR HELP WITH SERIOUS PROBLEM
          "BACLS11E", # OPEN UP TO SP ABOUT WORRIES
          "BACLS11F"))) # CAN RELAX, BE YOURSELF AROUND SP

table(Items$PSR$M2items %in% colnames(dw))

for (i in Items$PSR$M2items) {
  dw[, (i) := cleanVar(get(i), inlist = 1:6)]
}



################################################################################
##                                                                            ##
##                         Psychosocial Scale Scores                          ##
##                                                                            ##
################################################################################

droplist <- list(single = c(-1, 8, 9),
                 double = c(-1, 98, 99),
                 triple = c(-1, 996, 997, 998, 999),
                 quad = c(-1, 9996, 9997, 9998, 9999),
                 five = c(-1, 99996, 99997, 99998, 99999))

for (v in c("A1SAGENC", "A1SAGREE", "A1SCONS", "A1SEXTRA", "A1SNEURO",
"A1SOPEN", "A1SPWBA", "A1SPWBG", "A1SPWBE", "A1SPWBU", "A1SPWBS",
"A1SPWBR", "A1SMASTE", "A1SCONST", "A1SSWBSI", "A1SKINPO", "A1SFDSPO",
"A1SSPEMP", "B1SREINT", "B1SACTIV", "B1SPLAN", "B1SVENT", "B1SDENIA",
"B1SDISEN", "B1SFDCOP", "B1SAGENC", "B1SAGREE", "B1SCONS2", "B1SEXTRA",
"B1SNEURO", "B1SOPEN", "B1SPWBA2", "B1SPWBG2", "B1SPWBE2", "B1SPWBU2",
"B1SPWBS2", "B1SPWBR2", "B1SMASTE", "B1SCONST", "B1SSWBSI", "B1SKINPO",
"B1SFDSPO", "B1SSPEMP", "B1SESTEE", "B1SMPQCN", "B1SOPTIM", "B1SPESSI",
"C1SREINT", "C1SACTIV", "C1SPLAN", "C1SVENT", "C1SDENIA", "C1SDISEN",
"C1SFDCOP", "C1SAGENC", "C1SAGREE", "C1SCONS2", "C1SEXTRA", "C1SNEURO",
"C1SOPEN", "C1SPWBA2", "C1SPWBG2", "C1SPWBE2", "C1SPWBU2", "C1SPWBS2",
"C1SPWBR2", "C1SMASTE", "C1SCONST", "C1SSWBSI", "C1SKINPO", "C1SFDSPO",
"C1SSPEMP", "C1SESTEE", "C1SMPQCN", "C1SOPTIM", "C1SPESSI",
## new vars (TODO: check cleaning)
"A1SEMA", "A1SEFA", "B1SPWBR1", "B1SSWBSI", "B1SFAMSO",
       "B1SFDSOL", "B1SSPSOL", "B1SRELSU", "B1SSUFFI", "B1SRELCA",
       "B1SSPIRI", "B1SRELPR", "B1SSPRTE", "B1SMNDFU", "B1SINTER",
       "B1SPWBG2", "B1SPWBU1", "B1SDIREC", "B1SFORSG", "B1SINSGH", "B1SPERSI",
       "B1SSPCTR", "B1SSSCTR", "B1SREAPP", "B1SCSCSP", "B1SPRCOP",
       "B1SREINT", "B1SACTIV", "B1SPLAN"
)) {
  ddex <- ifelse(max(dw[[v]], na.rm = TRUE) < 10, 1,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 100, 2,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 1000, 3,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 10000, 4,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 100000, 5, NA_real_)))))

  if (is.na(ddex)) cat(sprintf("What?: %s", v))

  dw[, (v) := ifelse(get(v) %in% droplist[[c("single", "double", "triple", "quad", "five")[ddex]]],
                     NA_real_, get(v))]
}

################################################################################
##                                                                            ##
##                        Highest Parental Affection                          ##
##                                                                            ##
################################################################################

dw[, ParentalAffection := pmax(A1SEMA, A1SEFA, na.rm = TRUE)]

################################################################################
##                                                                            ##
##                             PA NA D2 & DM Setup                            ##
##                                                                            ##
################################################################################

Items$Mood <- rbind.data.frame(
  cbind(Scale = "NegAff",
        M2items = c(
          ## during past 30 days how much of the time did you feel...
          ## (1 = all of the time, 5 = none of the time)
          "B1SA24A", # so sad nothing could cheer you up
          "B1SA24B", # nervous
          "B1SA24C", # restless or fidgety
          "B1SA24D", # hopeless
          "B1SA24E", # that everything was an effort
          "B1SA24F", # worthless
          ##  "B1SA24G", # lonely ## drop
          "B1SA24H", # afraid
          "B1SA24I", # jittery
          "B1SA24J", # irritable
          "B1SA24K", # ashamed
          "B1SA24L", # upset
            "B1SA24M", # angry ## drop
            "B1SA24N" # frustrated ## drop
          ),
        MAitems = c(
          ## during past 30 days how much of the time did you feel...
          ## (1 = all of the time, 5 = none of the time)
          "BACAS24A", # so sad nothing could cheer you up
          "BACAS24B", # nervous
          "BACAS24C", # restless or fidgety
          "BACAS24D", # hopeless
          "BACAS24E", # that everything was an effort
          "BACAS24F", # worthless
          ##  "BACAS24G", # lonely ## drop
          "BACAS24H", # afraid
          "BACAS24I", # jittery
          "BACAS24J", # irritable
          "BACAS24K", # ashamed
          "BACAS24L", # upset
            "BACAS24M", # angry ## drop
            "BACAS24N" # frustrated ## drop
          )),
  cbind(Scale = "PosAff",
        M2items = c(
          ## during past 30 days how much of the time did you feel...
          ## (1 = all of the time, 5 = none of the time)
          "B1SA26A", # cheerful
          "B1SA26B", # in good spirits
          "B1SA26C", # extremely happy
          "B1SA26D", # calm and peaceful
          "B1SA26E", # satisfied
          "B1SA26F", # full of life
          ##  "B1SA26G", # close to others ## drop
          ##  "B1SA26H", # like you belong ## drop
          "B1SA26I", # enthusiastic
          "B1SA26J", # attentive
          "B1SA26K", # proud
          "B1SA26L", # active
            "B1SA26M" # confident ## drop
          ),
        MAitems = c(
          ## during past 30 days how much of the time did you feel...
          ## (1 = all of the time, 5 = none of the time)
          "BACAS26A", # cheerful
          "BACAS26B", # in good spirits
          "BACAS26C", # extremely happy
          "BACAS26D", # calm and peaceful
          "BACAS26E", # satisfied
          "BACAS26F", # full of life
          ##  "BACAS26G", # close to others ## drop
          ##  "BACAS26H", # like you belong ## drop
          "BACAS26I", # enthusiastic
          "BACAS26J", # attentive
          "BACAS26K", # proud
          "BACAS26L", # active
            "BACAS26M" # confident ## drop
          )))

table(Items$Mood$M2items %in% colnames(dw))

for (i in Items$Mood$M2items) {
  dw[, (i) := cleanVar(get(i), inlist = 1:6)]
}



################################################################################
##                                                                            ##
##                          Misc Items / Measures Setup                       ##
##                                                                            ##
################################################################################

#### depression ####
# depressed affect
Items$Depression <- c( ## formerly dep.items
  ###
  "B1PDEPAF", # 0 - 7 sum of items below
  # during two weeks in npast 12 months, when you felt sad, blue, or depressed, did you
  # lose interest in most things?
  # feel more tired out or low on energy than is usual?
  # lose your appetitte or appetite increased?
  # have more trouble falling asleep than usual?
  # have a lot more trouble concentrating than usual?
  # feel down on yourself, no good, or worthless?
  # think a lot about death?
  ###
  "B1PANHED",
  # anhedonia, sum of 6 items below
  # during two weeks when you lost interest in most things did you
  # feel more tired out or low on energy than is usual
  # los your appetite or appettite increased
  # have more trouble falling assleep than usual
  # have a lot more trouble concentrating than usual
  # feel down on yourself, no good, or worthless
  # think a lot about death
  ###
  # binary variable based on depressed affect and anhedonia
  "B1DEPRE",
  # binary depression variable
  "B1PDEPDX")

Items$Anxiety <- c( ## formerly anxiety.items
  # sum of 'most days' responses to the items
  "B1PANXIE",
  # how often over the past 12 months you
  # were restless because of your worry
  # were keyed up on edge or had a lot of nervous energy
  # were irritable becausew of your worry
  # had trouble falling asleep
  # had trouble staying asleep because of your worry
  # had trouble keeping your mind on what you were doing
  # had trouble remembering things ecause of your worry
  # were low on energy
  # tired easily because of your worry
  # had sore or aching muscles because of tension
  ### binary diagnosis
  "B1PANXTD"
)

# big five
Items$Personality <- c( ## personality.items
  "B1SNEURO", # 4 adjectives
  "B1SEXTRA", # 5 adjectives
  "B1SOPEN",  # 7 adjectives
  "B1SCONS2", # 5 adjectives
  "B1SAGREE"  # 5 adjectives
)


################################################################################
##                                  BMI / WHR                                 ##
################################################################################
dw[, c("A1SBMI", "A1SWSTHI", "B1SBMI", "B1SWSTHI", "C1SBMI", "C1SWSTHI") := .(
       cleanVar(A1SBMI, 10, 90),
       cleanVar(A1SWSTHI, 0, 4),
       cleanVar(B1SBMI, 10, 90),
       cleanVar(B1SWSTHI, 0, 4),
       cleanVar(C1SBMI, 10, 90),
       cleanVar(C1SWSTHI, 0, 4))]

################################################################################
##                                 Health Cleanup                             ##
################################################################################
##  Count of Chronic Conditions
dw[, c("A1SCHRON", "B1SCHRON", "C1SCHRON") := .(
       cleanVar(A1SCHRON, 0, 90),
       cleanVar(B1SCHRON, 0, 90),
       cleanVar(C1SCHRON, 0, 90))]

## subjective health
v <- c("A1PA4", "A1PA5", "A1PA6", "B1PA1", "B1PA2", "B1PA3", "C1PA1", "C1PA2", "C1PA3",
       "B1SA7B", "B1SA7C", "B1SA7D", "C1SA7B", "C1SA7C", "C1SA7D")

dw[, (v) := lapply(.SD, cleanVar, inlist = 1:5), .SDcols = v]

## Activities of Daily Living
v <- c("A1SBADL", "A1SIADL",
       "B1SBADL1", "B1SBADL2", "B1SIADL",
       "C1SBADL1", "C1SBADL2", "C1SIADL")
dw[, (v) := lapply(.SD, cleanVar, LL = 1, UL = 4), .SDcols = v]

## Cognitive Function
v <- c("B3TCOMPZ3", "B3TEMZ3", "B3TEFZ3", "C3TCOMPZ", "C3TEMZ", "C3TEFCZ")
dw[, (v) := lapply(.SD, cleanVar, LL = -6, UL = 6), .SDcols = v]


################################################################################
##                             Stress Network Vars                            ##
################################################################################
droplist <- list(single = c(8, 9),
                 double = c(98, 99),
                 triple = c(996, 997, 998, 999),
                 quad = c(9996, 9997, 9998, 9999),
                 five = c(99996, 99997, 99998, 99999))

for (v in c(
            "A1SS7", "B1PF7A",
            "A1SPIWOR", "A1SHOMET", "A1SPIHOM", "A1SKINNE", "A1SFDSNE", "A1SSPCRI",
            "A1SPIFAM", "A1SS13A", "A1SS13B", "A1SS13C", "A1SS13D", "A1SS13E",
            "A1SS13F", "A1SS13G", "A1SS13H", "A1SS13I", "A1SS13J", "A1SS13K",
            "A1SS14A", "A1SS14B", "A1SS14C", "A1SS14D", "A1SS14E", "A1SS14F",
            "A1SS14G", "A1SS14H", "A1SS14I",
            "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI",
            "B1SPIFAM", "B1SLFEDI", "B1SDAYDI",
            "B4QPS_PS",
            "C1SLFEDI", "C1SDAYDI", "C1SPIHOM", "C1SPIFAM"
            )) {
  ddex <- ifelse(max(dw[[v]], na.rm = TRUE) < 10, 1,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 100, 2,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 1000, 3,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 10000, 4,
                 ifelse(max(dw[[v]], na.rm = TRUE) < 100000, 5, NA)))))

  if (is.na(ddex)) cat(sprintf("What?: %s", v))

  dw[, (v) := ifelse(get(v) %in% droplist[[c("single", "double", "triple", "quad", "five")[ddex]]],
                     NA_real_, get(v))]
}


################################################################################
##                                                                            ##
##                         Stress Items / Measures Setup                      ##
##                                                                            ##
################################################################################

Items$Abuse = c(
  ## How often LIST A: Insulted you or swore at you; Sulked or refused to talk to you;
  ## Stomped out of the room; Did or said something to spite you; Threatened to hit you; Smashed or
  ## kicked something in anger
  "A1SE17A", ## Mother / woman
  "A1SE17B", ## Father / man
  "A1SE17C", ## Brothers
  "A1SE17D", ## Sisters
  "A1SE17E", ## Anyone Else
  ## LIST B: Pushed, grabbed, or shoved you; Slapped you; Threw something at you.
  "A1SE17F", ## Mother / woman
  "A1SE17G", ## Father / man
  "A1SE17H", ## Brothers
  "A1SE17I", ## Sisters
  "A1SE17J", ## Anyone Else
  ## LIST C: Kicked, bit, or hit you with a fist; Hit or tried to hit you with something; Beat you up; Choked
  ## you; Burned or scalded you.
  "A1SE17K", ## Mother / woman
  "A1SE17L", ## Father / man
  "A1SE17M", ## Brothers
  "A1SE17N", ## Sisters
  "A1SE17O") ## Anyone Else
Items$Abuse2 <- list(
  Parent = paste0("A1SE17", c("A", "B", "F", "G", "K", "L")),
  Sibling = paste0("A1SE17", c("C", "D", "H", "I", "M", "N")),
  Other = paste0("A1SE17", c("E", "J", "O")))

dw[, (Items$Abuse) := lapply(.SD, cleanVar, inlist = 1:4), .SDcols = Items$Abuse]


## alpha(d1[, Items$Abuse2$Parent, with = FALSE])
## alpha(d1[, Items$Abuse2$Sibling, with = FALSE])
## alpha(d1[, Items$Abuse2$Other, with = FALSE])

dw[, A1ParentAbuse := rowMeans(dw[, Items$Abuse2$Parent, with = FALSE], na.rm = TRUE) * -1 + 5]
dw[, A1ParentAbuse := ifelse(rowSums(is.na(dw[, Items$Abuse2$Parent, with = FALSE])) == 6, NA_real_, A1ParentAbuse)]

dw[, A1SiblingAbuse := rowMeans(dw[, Items$Abuse2$Sibling, with = FALSE], na.rm = TRUE) * -1 + 5]
dw[, A1SiblingAbuse := ifelse(rowSums(is.na(dw[, Items$Abuse2$Sibling, with = FALSE])) == 6, NA_real_, A1SiblingAbuse)]

dw[, A1OtherAbuse := rowMeans(dw[, Items$Abuse2$Other, with = FALSE], na.rm = TRUE) * -1 + 5]
dw[, A1OtherAbuse := ifelse(rowSums(is.na(dw[, Items$Abuse2$Other, with = FALSE])) == 3, NA_real_, A1SiblingAbuse)]

dw[, A1AllAbuse := rowMeans(dw[, Items$Abuse, with = FALSE], na.rm = TRUE) * -1 + 5]
dw[, A1AllAbuse := ifelse(rowSums(is.na(dw[, Items$Abuse, with = FALSE])) == 15, NA_real_, A1AllAbuse)]

dw[, A1SLFEDI := rowSums(dw[, paste0("A1SS13", LETTERS[1:11]), with = FALSE] > 0, na.rm = TRUE)]
dw[, A1SLFEDI := ifelse(rowSums(is.na(dw[, paste0("A1SS13", LETTERS[1:11]), with = FALSE])) == 11, NA_real_, A1SLFEDI)]

dw[, A1SDAYDI := rowMeans(5 - dw[, paste0("A1SS14", LETTERS[1:9]), with = FALSE], na.rm = TRUE) * 9]
dw[, A1SDAYDI := ifelse(is.nan(A1SDAYDI), NA_real_, A1SDAYDI)]


Items$Stress <- list(
  "A1PA4", "A1PA5", "A1PA6", "A1SCHRON",
  ParentAbuse = "A1ParentAbuse",
  SiblingAbuse = "A1SiblingAbuse",
  OtherAbuse = "A1OtherAbuse",
  piw = "A1SPIWOR", ## perceived inequality in work; alpha = .78
  pnq = "A1SHOMET", ## perceived neighborhood quality/health; alpha = .68
  pih = "A1SPIHOM", ## perceived inequality in home; alpha = .80
  famstrain = "A1SKINNE", ## family strain; alpha = .80
  friendstrain = "A1SFDSNE", ## friend strain; alpha = .79
  partnerstrain = "A1SSPCRI", ## partner strain
  pif = "A1SPIFAM", ## perceived inequality in family; alpha = .69
  lifetimediscrimination = "A1SLFEDI", ## lifetime discrimination
  dailydiscrimination = "A1SDAYDI" ## daily discrimination, alpha = .97
)


v <- c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:2), .SDcols = v]

## Initial Effect
v <- c(paste0("B1SE11", LETTERS[1:26], "3"), "B1SE11AY",
       paste0("B1SE11", LETTERS[1:26], "4"), "B1SE11AZ")
dw[, (v) := lapply(.SD, cleanVar, inlist = -2:2), .SDcols = v]


dw[, B1ChildLifeStress := rowSums(dw[, paste0("B1SE11", LETTERS[1:7]), with = FALSE] == 1, na.rm = TRUE)]
dw[, B1ChildLifeStress := ifelse(rowSums(is.na(dw[, paste0("B1SE11", LETTERS[1:7]),
                                              with = FALSE])) == 7, NA_real_, B1ChildLifeStress)]


dw[, B1LifeStress := rowSums(dw[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA"), with = FALSE] == 1, na.rm = TRUE)]
dw[, B1LifeStress := ifelse(rowSums(is.na(dw[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA"),
                                              with = FALSE])) == 27, NA_real_, B1LifeStress)]

dw[, B1LifeStressImpactS := rowSums(dw[, c(paste0("B1SE11", LETTERS[1:26], "3"), "B1SE11AY"), with = FALSE],
                                     na.rm = TRUE)]
dw[, B1LifeStressImpactS := ifelse(rowSums(is.na(dw[, c(paste0("B1SE11", LETTERS[1:26], "3"), "B1SE11AY"), with = FALSE])) == 27,
                                   NA_real_, B1LifeStressImpactS)]

dw[, B1LifeStressImpactL := rowSums(dw[, c(paste0("B1SE11", LETTERS[1:26], "4"), "B1SE11AZ"), with = FALSE],
                                     na.rm = TRUE)]
dw[, B1LifeStressImpactL := ifelse(rowSums(is.na(dw[, c(paste0("B1SE11", LETTERS[1:26], "4"), "B1SE11AZ"), with = FALSE])) == 27,
                                   NA_real_, B1LifeStressImpactL)]

Items$Stress2 <- list(
  "B1PA1", "B1PA2", "B1PA3", "B1SCHRON",
  "B1LifeStress", "B1LifeStressImpactS", "B1LifeStressImpactL",
  "B1SJOBDI", ## chronic job discrimination
  piw = "B1SPIWOR", ## perceived inequality in work
  pnq = "B1SHOMET", ## perceived neighborhood quality/health;
  pih = "B1SPIHOM", ## perceived inequality in home;
  famstrain = "B1SKINNE", ## family strain;
  friendstrain = "B1SFDSNE", ## friend strain;
  partnerstrain = "B1SSPCRI", ## partner strain
  pif = "B1SPIFAM", ## perceived inequality in family;
  lifetimediscrimination = "B1SLFEDI", ## paste0("A1SS13", LETTERS[1:11]), ## lifetime discrimination
  dailydiscrimination = "B1SDAYDI") ## paste0("A1SS14", LETTERS[1:9]) ## daily discrimination;

################################################################################
##                        MIDUS II Biomarker Project                          ##
################################################################################
v <- c("B4QCT_EA", "B4QCT_PA", "B4QCT_SA", "B4QCT_EN", "B4QCT_PN")
dw[, (v) := lapply(.SD, cleanVar, UL = 90), .SDcols = v]

## alpha(d2.p4[, c("B4QCT_EA", "B4QCT_PA", "B4QCT_SA", "B4QCT_EN", "B4QCT_PN")])

dw[, B4CTQTotal := rowMeans(dw[, c("B4QCT_EA", "B4QCT_PA", "B4QCT_SA", "B4QCT_EN", "B4QCT_PN"), with = FALSE],
                         na.rm = TRUE)]

Items$Stressp4 <- list(
"B4QPS_PS", ## perceived stress scale
"B4QCT_EA", ## emotional abuse
"B4QCT_PA", ## physical abuse
"B4QCT_SA", ## sexual abuse
"B4QCT_EN", ## emotional neglect
"B4QCT_PN", ## physical neglect
"B4QCT_MD", ## minimization/denial, used as a check
"B4CTQTotal" ## a total score from the CTQ subscales but excluding minimization/denial
)


################################################################################
##                             Demographic Cleanup                            ##
################################################################################

## nights worked away from home
v <- c("A1PB12", "B1PB13", "C1PB13")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 365), .SDcols = v]

## highest education
v <- c("A1PB1", "B1PB1", "C1PB1")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:12), .SDcols = v]

## current marital status
v <- c("A1PB17", "B1PB19", "C1PB19", "B4HMARR")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:6), .SDcols = v]

## current work status
v <- c("A1PBWORK", "B1PBWORK", "C1PB3WK")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 11), .SDcols = v]

## currently have health insurance
v <- c("A1SC1", "B1SC1", "C1SC1")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:2), .SDcols = v]

## sex
v <- c("A1PRSEX", "B1PRSEX", "C1PRSEX")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:2), .SDcols = v]

## overall sex
dw[, Sex := ifelse(is.na(A1PRSEX), ifelse(is.na(B1PRSEX), as.integer(C1PRSEX), as.integer(B1PRSEX)), as.integer(A1PRSEX))]



################################################################################
##                                      Alcohol                               ##
################################################################################

v <- c(
  paste0(c("A", "B", "C"), "1PA54"),
  paste0(c("A", "B", "C"), "1PA54A"),
  paste0(c("B", "C"), "1PA51"),
  paste0(c("B", "C"), "1PA51A"),
  "B4H38", "B4H39", "B4H34", "B4H35")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:6), .SDcols = v]

v <- c(
  paste0(c("A", "B", "C"), "1PA55"),
  paste0(c("A", "B", "C"), "1PA52"),
  paste0(c("B", "C"), "1PA53"),
  "B4H40", "B4H36", "B4H37")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 90), .SDcols = v]

v <- c("A1PA52", "B1PA49", "C1PA49")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 96), .SDcols = v]

v <- c("B1PA50", "C1PA50", "B4H33")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:3), .SDcols = v]

scoreAlcohol <- function(agefirst, atleast1, perweek, permonth, avedrinks, fiveplus, sex) {
  if (missing(atleast1)) {
    never <- as.integer(agefirst == 96 | perweek == 6 | permonth %in% c(4, 6))
  } else {
    never <- as.integer(agefirst == 96 | atleast1 %in% c(2, 3) | perweek == 6 | permonth %in% c(4, 6))
  }

  DaysPerWeek <- recode(perweek, "1 = 7; 2 = 5.5; 3 = 3.5; 4 = 1.5; 5 = -1; else = NA")
  DaysPerWeek2 <- recode(permonth, "1 = .875; 2 = .375; 3 = .125; else = NA")

  DaysPerWeek <- ifelse(DaysPerWeek == -1,
                        DaysPerWeek2,
                        DaysPerWeek)

  DrinksPerWeek <- DaysPerWeek * avedrinks

  Sex <- ifelse(sex == 1, "male", "female")

  if (missing(fiveplus)) {
    fiveplus <- 0
  }

  ifelse(never == 1 & !is.na(never), "None",
    ifelse(Sex == "male",
      ifelse(DrinksPerWeek > 14 | avedrinks > 4 | fiveplus > 0, "High",
      ifelse(DrinksPerWeek <= 14 & avedrinks <= 4 & fiveplus == 0, "Moderate", NA_character_)),
      ifelse(Sex == "female",
        ifelse(DrinksPerWeek > 7 | avedrinks > 3 | fiveplus > 0, "High",
        ifelse(DrinksPerWeek <= 7 & avedrinks <= 3 & fiveplus == 0, "Moderate", NA_character_)),
        NA_character_)))
}

dw[, A1WorstAlcohol := scoreAlcohol(agefirst = A1PA52, perweek = A1PA54, permonth = A1PA54A, avedrinks = A1PA55, sex = Sex)]
dw[, B1WorstAlcohol := scoreAlcohol(agefirst = B1PA49, perweek = B1PA54, permonth = B1PA54A, avedrinks = B1PA55, sex = Sex)]
dw[, B4WorstAlcohol := scoreAlcohol(agefirst = B1PA49, perweek = B4H38, permonth = B4H39, avedrinks = B4H40, sex = Sex)]
dw[, C1WorstAlcohol := scoreAlcohol(agefirst = C1PA49, perweek = C1PA54, permonth = C1PA54A, avedrinks = C1PA55, sex = Sex)]
dw[, B1CurrentAlcohol := scoreAlcohol(B1PA49, B1PA50, B1PA51, B1PA51A, B1PA52, B1PA53, Sex)]
dw[, B4CurrentAlcohol := scoreAlcohol(B1PA49, B4H33, B4H34, B4H35, B4H36, B4H37, Sex)]
dw[, C1CurrentAlcohol := scoreAlcohol(C1PA49, C1PA50, C1PA51, C1PA51A, C1PA52, C1PA53, Sex)]

## xtabs(~B1WorstAlcohol + B4WorstAlcohol, data = dw)
## xtabs(~B1CurrentAlcohol + B4CurrentAlcohol, data = dw)

################################################################################
##                                      Smoking                               ##
################################################################################

## age first smoked item (96 = never)
v <- c("A1PA41", "B1PA37", "C1PA37")
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 96), .SDcols = v]


## have you ever or do you regularly now items
v <- c("A1PA40", "B1PA38A", "B4H26", "C1PA38A",
       "A1PA43", "B1PA39", "B4H26A", "C1PA39",
       "B1PA44", "C1PA44", "B4H29")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:2), .SDcols = v]

scoreSmoke <- function(agefirst, ever, now) {
  never <- as.integer(agefirst == 96 | ever == 2)

  current <- ifelse(now == 1, 1, 0)
  ever <- ifelse(ever == 1, 1, 0)

  ifelse(never == 1 & !is.na(never), "never",
    ifelse(current == 1 & !is.na(current), "current",
      ifelse(ever == 1 & !is.na(ever), "past", NA_character_)))
}

dw[, A1Smoke := scoreSmoke(agefirst = A1PA41, ever = A1PA40, now = A1PA43)]
dw[, B1Smoke := scoreSmoke(agefirst = B1PA37, ever = B1PA38A, now = B1PA39)]
dw[, C1Smoke := scoreSmoke(agefirst = C1PA37, ever = C1PA38A, now = C1PA39)]

## consider: don't allow people who said smoking in A1 to be "never" in B1, etc.
## dw[, B1Smoke := ifelse(B1Smoke == "never" & A1Smoke %in% c("past", "current"), "past", B1Smoke)]
## dw[, C1Smoke := ifelse(C1Smoke == "never" & (A1Smoke %in% c("past", "current") | B1Smoke %in% c("past", "current")),
##                        "past", C1Smoke)

## xtabs(~A1Smoke + B1Smoke, data = dw)
## xtabs(~B1Smoke + C1Smoke, data = dw)

################################################################################
##                             Physical Activity                              ##
################################################################################

v <- c(
  paste0("A1SA", 18:21),
  paste0("B1SA30", LETTERS[1:6]),
  paste0("B1SA31", LETTERS[1:6]),
  paste0("B1SA32", LETTERS[1:6]),
  paste0("C1SA26", LETTERS[1:6]),
  paste0("C1SA27", LETTERS[1:6]),
  paste0("C1SA28", LETTERS[1:6]))
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:6), .SDcols = v]

alpha(as.data.frame(dw[, paste0("A1SA", 18:21), with = FALSE]))
alpha(as.data.frame(dw[, paste0("B1SA30", LETTERS[1:6]), with = FALSE]))
alpha(as.data.frame(dw[, paste0("B1SA31", LETTERS[1:6]), with = FALSE]))
alpha(as.data.frame(dw[, paste0("B1SA32", LETTERS[1:6]), with = FALSE]))

scoreActivity <- function(vig, mod, light) {
  dvig <- copy(dw[, vig, with = FALSE])
  dvig[, (vig) := lapply(.SD, function(x) (-x + 6) * 5), .SDcols = vig]

  dmod <- copy(dw[, mod, with = FALSE])
  dmod[, (mod) := lapply(.SD, function(x) (-x + 6) * 3), .SDcols = mod]


  if (!missing(light)) {
    dlight <- copy(dw[, light, with = FALSE])
    dlight[, (light) := lapply(.SD, function(x) (-x + 6) * 1), .SDcols = light]
    dall <- cbind(dvig, dmod, dlight)
  } else {
    dall <- cbind(dvig, dmod)
  }

  print(alpha(as.data.frame(dall)))

  rowMeans(dall, na.rm = TRUE)
}

dw[, A1PhysAct := scoreActivity(paste0("A1SA", 18:19), paste0("A1SA", 20:21))]

dw[, B1PhysAct := scoreActivity(
       paste0("B1SA30", LETTERS[1:6]),
       paste0("B1SA31", LETTERS[1:6]),
       paste0("B1SA32", LETTERS[1:6]))]

dw[, C1PhysAct := scoreActivity(
       paste0("C1SA26", LETTERS[1:6]),
       paste0("C1SA27", LETTERS[1:6]),
       paste0("C1SA28", LETTERS[1:6]))]


## dw[, A1PhysAct := winsorizor(A1PhysAct, percentile = .005)]
## dw[, B1PhysAct := winsorizor(B1PhysAct, percentile = .005)]
## dw[, C1PhysAct := winsorizor(C1PhysAct, percentile = .005)]
## hist(dw$A1PhysAct)
## hist(dw$B1PhysAct)
## hist(dw$C1PhysAct)


physact <- function(x, weights1 = "1 = 3; 2 = 2; 3 = 1;", weights2 = "1 = 5; 2 = 3; 3 = 1;") {

  anyact <- as.integer(dw$B4H25 == 1)

  tmp <- lapply(LETTERS[1:7], function(i) {
    freq <- dw[[paste0("B4H25", i, "FW")]]
    duration <- dw[[paste0("B4H25", i, "M")]]
    intensity <- dw[[paste0("B4H25", i, "I")]]
    freq <- ifelse(freq > 7, NA, freq)
    duration <- ifelse(duration > 1000, NA, duration)
    intensity <- ifelse(!intensity %in% 1:3, NA, intensity)
    res1 <- freq * recode(intensity, weights1)
    res2 <- freq * duration * recode(intensity, weights1)
    res3 <- freq * recode(intensity, weights2)
    res4 <- freq * duration * recode(intensity, weights2)
    data.frame(FreqInt = res1, FreqDurInt = res2,
               FreqIntW = res3, FreqDurIntW = res4)
  })

  data.table(
    AnyAct = anyact,
    FreqInt = ifelse(anyact == 2 | is.na(anyact),
                     NA,
                     rowSums(sapply(tmp, `[[`, "FreqInt"), na.rm = TRUE)),
    FreqDurInt = ifelse(anyact == 2 | is.na(anyact),
                     NA,
                     rowSums(sapply(tmp, `[[`, "FreqDurInt"), na.rm = TRUE)),
    FreqIntW = ifelse(anyact == 2 | is.na(anyact),
                     NA,
                     rowSums(sapply(tmp, `[[`, "FreqIntW"), na.rm = TRUE)),
    FreqDurIntW = ifelse(anyact == 2 | is.na(anyact),
                     NA,
                     rowSums(sapply(tmp, `[[`, "FreqDurIntW"), na.rm = TRUE))
    )
}

phystemp <- physact(copy(dw))
phystemp[, PhysAct := cut(FreqDurInt,
                     breaks = c(-Inf, 1, quantile(FreqDurInt[FreqDurInt > 0], probs = c(1/3, 2/3), na.rm = TRUE), Inf),
                     labels = 1:4, ordered_result = TRUE)]

setnames(phystemp, names(phystemp), paste0("B4", names(phystemp)))

dw <- cbind(dw, phystemp)

################################################################################
##                                                                            ##
##                              Self Report Sleep                             ##
##                                                                            ##
################################################################################
v <- c("B1SA57A", "B1SA57B", "B1SA58A", "B1SA58B", "B1SA59A", "B1SA59B",
       "C1SA53A", "C1SA53B", "C1SA54A", "C1SA54B", "C1SA55A", "C1SA55B")

dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 60), .SDcols = v]

dw[, B1WeekdaySleep := ifelse(is.na(B1SA57A) & is.na(B1SA57B),
                              NA_real_, rowSums(dw[, .(B1SA57A, B1SA57B/60)], na.rm = TRUE))]
dw[, B1WeekendSleep := ifelse(is.na(B1SA58A) & is.na(B1SA58B),
                              NA_real_, rowSums(dw[, .(B1SA58A, B1SA58B/60)], na.rm = TRUE))]

dw[, C1WeekdaySleep := ifelse(is.na(C1SA53A) & is.na(C1SA53B),
                              NA_real_, rowSums(dw[, .(C1SA53A, C1SA53B/60)], na.rm = TRUE))]
dw[, C1WeekendSleep := ifelse(is.na(C1SA54A) & is.na(C1SA54B),
                              NA_real_, rowSums(dw[, .(C1SA54A, C1SA54B/60)], na.rm = TRUE))]

cor(dw[, .(B1WeekdaySleep, B1WeekendSleep)], use = "pairwise.complete.obs")
dw[, B1Sleep := rowMeans(dw[, .(B1WeekdaySleep, B1WeekendSleep)], na.rm = TRUE)]
dw[, C1Sleep := rowMeans(dw[, .(C1WeekdaySleep, C1WeekendSleep)], na.rm = TRUE)]

## hist(dw$B1Sleep, breaks = 10)
## hist(dw$C1Sleep, breaks = 10)

dw[, B1SleepOnsetLatency := ifelse(is.na(B1SA59A) & is.na(B1SA59B),
                              NA_real_, rowSums(dw[, .(B1SA59A, B1SA59B/60)], na.rm = TRUE))]

dw[, C1SleepOnsetLatency := ifelse(is.na(C1SA55A) & is.na(C1SA55B),
                              NA_real_, rowSums(dw[, .(C1SA55A, C1SA55B/60)], na.rm = TRUE))]

v <- c("B1SA61B", "C1SA57B")
dw[, (v) := lapply(.SD, cleanVar, inlist = 1:5), .SDcols = v]

## winsorize, log, standardize and combine
cor(dw[, .(SOL = scale(log(winsorizor(B1SleepOnsetLatency, .01))), Freq = scale(B1SA61B))],
    use = "pairwise.complete.obs")

dw[, B1SSQ := as.vector(scale(log(winsorizor(B1SleepOnsetLatency, .01))) + scale(B1SA61B))]
dw[, C1SSQ := as.vector(scale(log(winsorizor(C1SleepOnsetLatency, .01))) + scale(C1SA57B))]


################################################################################
##                                                                            ##
##                                  Mortality                                 ##
##                                                                            ##
################################################################################


dw[, DDate := as.Date(paste0(DOD_Y, "-", DOD_M, "-15"), "%Y-%m-%d")]
dw[, DOD_Y := cleanVar(DOD_Y, LL = 1900, 2020)]
dw[, DECEASED := ifelse(is.na(DECEASED), 0L, 1L)]
dw[, LastAge := ifelse(is.na(DOD_Y) & (DECEASED == 0), 2017, DOD_Y) - BirthYear]
## ggplot(dw, aes(LastAge, colour = factor(DECEASED))) + geom_density()
## plot(survfit(Surv(LastAge, DECEASED) ~ factor(Sex), data = dw))


################################################################################
##                          Data Subset for Stress                            ##
################################################################################

## TODO: check race coding
dw[,
  RaceG3 := ifelse(is.na(A1SS7) & is.na(B1PF7A), "Missing", ifelse(is.na(A1SS7), B1PF7A, A1SS7))][,
  RaceG3 := ifelse(RaceG3 == 1, "White",
            ifelse(RaceG3 == 2, "AA",
            ifelse(RaceG3 %in% c(3, 4, 5, 6), "Other", "Missing")))][,
  RaceG3 := factor(ifelse(RaceG3 == "Missing", NA, RaceG3),
            levels = c("White", "AA", "Other"))]


## plot(SEMSummary(~., data = d1b[, unlist(stress)]))
## chordDiagram(cor(d1b[, unlist(stress)][, 1:40], use = "pairwise.complete.obs"), symmetric = TRUE)

## Nice Names
stresskey <- "
Var	Label
AGEM1	Age (M1)
AGEM2	Age (M2)
Sex	Sex (M1)
MFAbuse	Mother Father Abuse (M1)
B4CTQTotal	CTQ Total Score (M2P4)
B4QCT_MD	Minimization/Denial (M2P4)
A1SSPCRI	Partner Strain (M1)
B1SSPCRI	Partner Strain (M2)
A1SKINNE	Family Strain (M1)
B1SKINNE	Family Strain (M2)
A1SFDSNE	Friend Strain (M1)
B1SFDSNE	Friend Strain (M2)
B1SLFEDI	Lifetime Discrimination (M2)
B1SDAYDI	Daily Discrimination (M2)
A1SLFEDI	Lifetime Discrimination (M1)
A1SDAYDI	Daily Discrimination (M1)
B1SJOBDI	Chronic Job Discrimination (M2)
B4QPS_PS	Perceived Stress Scale (M2P4)
A1SPIWOR	Work Inequality (M1)
B1SPIWOR	Work Inequality (M2)
B1LifeStress	Stressful Life Events (M2)
B1LifeStressImpactS	Short Term Impact of SLEs (M2)
B1LifeStressImpactL	Long Term Impact of SLEs (M2)
B4QCT_SA	Sexual Abuse (M2P4)
B4QCT_PN	Physical Neglect (M2P4)
B4QCT_PA	Physical Abuse (M2P4)
B4QCT_EA	Emotional Abuse (M2P4)
B4QCT_EN	Emotional Neglect (M2P4)
A1SPIFAM	Family Inequality (M1)
B1SPIFAM	Family Inequality (M2)
A1SHOMET	Neighborhood Quality (M1)
A1SPIHOM	Home Inequality (M1)
B1SHOMET	Neighborhood Quality (M2)
B1SPIHOM	Home Inequality (M2)
A1SCHRON	Chronic Conditions (M1)
B1SCHRON	Chronic Conditions (M2)
A1PA6	SR Relative Health (M1)
B1PA3	SR Relative Health (M2)
B1PA1	SR Physical Health (M2)
B1PA2	SR Mental Health (M2)
A1PA4	SR Physical Health (M1)
A1PA5	SR Mental Health (M1)
dDayDi	Change Daily Discrimination
dFamIn	Change Family Inequality
dFamSt	Change Family Strain
dFriSt	Change Friend Strain
dHomIn	Change Home Inequality
dLfeDi	Change Lifetime Discrimination
dNeiQu	Change Neighborhood Quality
dParSt	Change Partner Strain
dWorIn	Change Work Inequality
b4bsesel	sE-Selectin (M2P4)
b4bsicam	sICAM (M2P4)
b4bil6	IL-6 (M2P4)
Rb4bcrp	CRP (M2P4)
b4bfgn	Fibrinogen (M2P4)
avgb_lf	LFHRV (M2P4)
avgb_rm	HRV RMSSD (M2P4)
avgb_hf	HFHRV (M2P4)
pulpress	Pulse Pressure (M2P4)
Rb4p1gs	SBP (M2P4)
b3tem	Episodic Memory (M2)
exec_fxn	Executive Function (M2)
LastAge	Age at Death/Censor (M2+)
"

stresskey2 <- read.table(textConnection(stresskey), header=TRUE, sep = "\t")
stresskey2$Timepoint <- gsub("(.*)\\((.*)\\)", "\\2", stresskey2$Label)
stresskey2$Label2 <- gsub("(.*)\\((.*)\\)", "\\1", stresskey2$Label)

stresskey2$TimepointCol <- ifelse(stresskey2$Timepoint == "M1",
                            "steelblue2",
                            ifelse(stresskey2$Timepoint == "M2",
                                   "lightgoldenrod", "brown1"))
stresskey2$Time <- ifelse(stresskey2$Timepoint == "M1", 1, 2)



################################################################################
##                        Create Variables for RCM Paper                      ##
################################################################################


## Missing Data General
dw[, B4QCESD := ifelse(B4QCESD == 98, NA, B4QCESD)]
dw[, B4QMA_D := ifelse(B4QMA_D == 98, NA, B4QMA_D)]
dw[, B4QMA_A := ifelse(B4QMA_A == 98, NA, B4QMA_A)]
dw[, B4QMA_LI := ifelse(B4QMA_LI == 98, NA, B4QMA_LI)]
dw[, B4QMA_AA := ifelse(B4QMA_AA == 98, NA, B4QMA_AA)]
dw[, B4QMA_PA := ifelse(B4QMA_PA == 98, NA, B4QMA_PA)]

## Missing Data PSQI Items
## bed partner
dw[, B4S9 := cleanVar(B4S9, inlist = 1:4)]

dw[, B4SSQ_GS := ifelse(B4SSQ_GS == 98, NA, B4SSQ_GS)] # global score
## also need to get component scores and one specific item to remake global
## score without enthusiasm and without subjective sleep quality
dw[, B4SSQ_S1 := ifelse(B4SSQ_S1 == 8, NA, B4SSQ_S1)] # SSQ
dw[, B4SSQ_S2 := ifelse(B4SSQ_S2 == 8, NA, B4SSQ_S2)] # latency
dw[, B4SSQ_S3 := ifelse(B4SSQ_S3 == 8, NA, B4SSQ_S3)] # duration
dw[, B4SSQ_S4 := ifelse(B4SSQ_S4 == 8, NA, B4SSQ_S4)] # efficiency
## if efficiency > 100% don't compute
dw[, B4SSQ_S4 := ifelse(B4SSQ_S4 == 4, 0, B4SSQ_S4)] # efficiency
dw[, B4SSQ_S5 := ifelse(B4SSQ_S5 == 8, NA, B4SSQ_S5)] # disturbances
dw[, B4SSQ_S6 := ifelse(B4SSQ_S6 == 8, NA, B4SSQ_S6)] # sleep meds
dw[, B4SSQ_S7 := ifelse(B4SSQ_S7 == 8, NA, B4SSQ_S7)] # daytime dysfunction
dw[, B4S8 := ifelse(B4S8 == 8, NA, B4S8)] # trouble staying awake

## Missing Data Diet / Nutrition Items
dw[, B4H16 := ifelse(B4H16 == 8, NA, B4H16)]
dw[, B4H17AF := ifelse(B4H17AF > 99, NA, B4H17AF)]
dw[, B4H17BF := ifelse(B4H17BF > 99, NA, B4H17BF)]
dw[, B4H17CF := ifelse(B4H17CF > 99, NA, B4H17CF)]
dw[, B4H18AF := ifelse(B4H18AF > 99, NA, B4H18AF)]
dw[, B4H18BF := ifelse(B4H18BF > 99, NA, B4H18BF)]
dw[, B4H18CF := ifelse(B4H18CF > 99, NA, B4H18CF)]
dw[, B4H19 := ifelse(B4H19 %in% c(5, 8), NA, B4H19)]
dw[, B4H20 := ifelse(B4H20 %in% c(5, 8), NA, B4H20)]
dw[, B4H21 := ifelse(B4H21 %in% c(5, 7, 8), NA, B4H21)]
dw[, B4H22 := ifelse(B4H22 %in% c(5, 8), NA, B4H22)]
dw[, B4H23A := ifelse(B4H23A == 8, NA, B4H23A)]
dw[, B4H23B := ifelse(B4H23B == 8, NA, B4H23B)]
dw[, B4H23C := ifelse(B4H23C == 8, NA, B4H23C)]
dw[, B4H23D := ifelse(B4H23D == 8, NA, B4H23D)]
dw[, B4H24 := ifelse(B4H24 == 8, NA, B4H24)]


## Diet Variables
## fast food + high fat meed
dw[, B4Diet := (B4H23B + B4H24) / 2]

## no water or 8+ water
## no vegetables
## 4+ sugar sweetened beverages per day
## no or 5+ whole grains
dw[, B4ExtremeDiet := (B4H19 %in% c(1)) +
       (B4H20 >= 3) +
      (B4H21 == 1) +
      (B4H22 %in% c(1, 4))]

## overall diet previous two, rescaled so each is 0 to 1 range
## then summed
dw[, B4OverallDiet :=
    (B4H23B-1)/4 +
      (B4H24-1)/4 +
      (B4H19 %in% c(1)) +
      (B4H20 >= 3) +
      (B4H21 == 1) +
      (B4H22 %in% c(1, 4))]

## PSQI with and without various items
dw[, B4PSQI1 := rowSums(dw[, paste0("B4SSQ_S", c(1, 2, 3, 4, 5, 6, 7)), with = FALSE])]
## PSQI - enthusiasm item
dw[, B4PSQI2 := rowSums(dw[, c(paste0("B4SSQ_S", c(1, 2, 3, 4, 5, 6)), "B4S8"), with = FALSE])]
## PSQI - enthusiasm item - SSQ
dw[, B4PSQI3 := rowSums(dw[, c(paste0("B4SSQ_S", c(2, 3, 4, 5, 6)), "B4S8"), with = FALSE])]

## count of not use seatbelt, not use helmet (if ride bike/motorcycle), excessive sun, and not wear sunscreen
## categorized into low / high unhealth behavior
dw[, B4RiskyBehaviors := as.integer(rowSums(cbind(B4H47 == 2, B4H48A == 2, B4H49 == 1, B4H50 == 2),
                                     na.rm = TRUE) > 2)]


dw[, (Items$Personality) := lapply(.SD, function(x) ifelse (x == 8, NA, x)),
   .SDcols = Items$Personality]

## included.variables <- c(
##   "Diet", "ExtremeDiet", "OverallDiet",
##   "PSQI1", "PSQI2", "PSQI3",
##   "Smoke", "CurrentSmoke",
##   "RiskyBehaviors", "CurrentAlcohol", "WorstAlcohol",
##   ## different variants and weights of physical activity composite
##   "FreqInt", "FreqDurInt", "FreqIntW", "FreqDurIntW", "PhysAct",
##   "B4QCESD", "B4QMA_D", "B4QMA_A", "B4QMA_LI", "B4QMA_AA", "B4QMA_PA")
## misc.d <- merge(rbind(d[, c("M2ID", "LifeStress")], dm[, c("M2ID", "LifeStress")]),
##                 demos, by.x = "M2ID", by.y = "m2id", all = TRUE)
## misc.d <- merge(misc.d, useses, by.x = "M2ID", by.y = "m2id", all = TRUE)
## misc.d <- merge(misc.d, chronic, by.x = "M2ID", by.y = "m2id", all = TRUE)
## misc.d <- merge(misc.d, p4dat[, c("M2ID", included.variables)], by = "M2ID", all = TRUE)
## misc.d.final <- misc.d[, c("M2ID", "m2famnum", "b1page_m2", "b1pgender",
##   "m1chadv", "m2aadv", "LifeStress",
##   "p4majorconditions", "p4minorconditions", "p4sumburden",
##   included.variables)]


################################################################################
##                             Stress Network PSRS                            ##
################################################################################

midus.psr.items <- read.csv("~/OneDrive/Projects/MIDUS/Projects/StressNetwork/midus_psr_items.csv", stringsAsFactors = FALSE)
midus.psr.items[] <- lapply(midus.psr.items, function(x) ifelse(nzchar(x), x, NA))
midus.psr.items <- as.data.table(within(midus.psr.items, {
  ItemAbbr <- paste0(Abbr, unlist(sapply(rle(Abbr)$lengths, function(x) 1:x)))
}))

## PSRF-SF
midus.psr.items[ItemAbbr %in% c("M2", "LC2", "SE2", "Opt2", "PLife2", "Con3", "Ext4",
                                "FamS2", "FriS2", "SpoS3", "SI2", "SI1")]

dw[, (midus.psr.items$MIDUS2) := lapply(.SD, cleanVar, LL = 0, UL = 7), .SDcols = midus.psr.items$MIDUS2]

midus.psr.items$ItemAbbr <- paste0("B1", midus.psr.items$ItemAbbr)

for (i in 1:nrow(midus.psr.items)) {
  dw[, (midus.psr.items$ItemAbbr[i]) := get(midus.psr.items$MIDUS2[i])]
}

v <- midus.psr.items[Abbr %in% c("FamS", "FriS", "SpoS")][["ItemAbbr"]]
dw[, (v) := lapply(.SD, cleanVar, LL = 0, UL = 4), .SDcols = v]

## reverse code items
dw[, B1LC2 := B1LC2 * -1]
dw[, B1SI1 := B1SI1 * -1]

## by default these will be negative, so negate to make higher = more resources
dw[, B1PSRQSFPR := as.vector(scale(-rowMeans(as.data.frame(lapply(dw[, c("B1M2", "B1LC2", "B1SE2", "B1Opt2",
  "B1PLife2", "B1Con3", "B1Ext4"), with = FALSE], scale)), na.rm = TRUE)))]

dw[, B1PSRQSFSR := as.vector(scale(-rowMeans(as.data.frame(lapply(dw[, c("B1FamS2",
  "B1FriS2", "B1SpoS3", "B1SI2", "B1SI1"), with = FALSE], scale)), na.rm = TRUE)))]

################################################################################
##                                                                            ##
##                         Winsorizing to remove outliers                     ##
##                                                                            ##
################################################################################

distp <- function(v, wins = c(.00, .005, .01), trans = I) {
  d <- melt(data.frame(
    V1 = scale(trans(winsorizor(dw[[v]], wins[1]))),
    V2 = scale(trans(winsorizor(dw[[v]], wins[2]))),
    V3 = scale(trans(winsorizor(dw[[v]], wins[3])))))

  ggplot(d, aes(value)) +
  geom_histogram(aes(y = ..density..)) +
    geom_density() +
    theme_cowplot() +
    facet_wrap(~variable, scales = "free", ncol = 1)
}

dwclean <- copy(dw)

## distp("B1PSRQSFPR", trans = I)

## do nothing
v000 <- c(
  "A1SSPCRI", "A1SKINNE", "A1SiblingAbuse",
  "B1SSPCRI", "A1PA4", "A1PA5", "A1PA6",
  "B1PA1", "B1PA2", "B1PA3",
  "exec_fxn", "BirthYear", "AGEM1", "AGEM2", "AGEM3",
  "B1SSQ", "C1SSQ", "m1chadv", "m2aadv"
  )
## winsorize lower and upper 0.5%
v005 <- c(
  "A1SFDSNE", "A1SLFEDI", "A1SPIWOR", "A1SPIFAM",
  "A1SPIHOM", "B1ChildLifeStress", "B1LifeStress",
  "B1LifeStressImpactS", "B1SJOBDI", "B1SPIFAM",
  "B1SLFEDI", "B4QPS_PS",
  "A1SCHRON", "B1SCHRON", "C1SCHRON",
  "b3tem", "A1SBMI", "B1SBMI", "C1SBMI",
  "A1SWSTHI", "B1PhysAct", "C1PhysAct",
  "B1PSRQSFPR", "B1PSRQSFSR",
  "B3TCOMPZ3", "B3TEMZ3", "C3TCOMPZ", "C3TEMZ", "C3TEFCZ"
)
## winsorize lower and upper 1%
v010 <- c(
  "A1SDAYDI", "A1SHOMET", "A1ParentAbuse", "B4CTQTotal",
  "B1LifeStressImpactL", "B1SPIWOR", "B1SHOMET",
  "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SDAYDI",
  "B1SWSTHI", "C1SWSTHI", "A1PhysAct", "B1Sleep",
  "C1Sleep"
)

dwclean[, (v005) := lapply(.SD, winsorizor, percentile = .005), .SDcols = v005]
dwclean[, (v010) := lapply(.SD, winsorizor, percentile = .010), .SDcols = v010]

################################################################################
##                            Create Change Variables                         ##
################################################################################

## create change scores (M2 - M1)
dw[, c("dParSt", "dFamSt", "dFriSt", "dLfeDi", "dDayDi", "dWorIn", "dFamIn", "dNeiQu", "dHomIn", "dSRPH ", "dSRMH ") := .(
  B1SSPCRI - A1SSPCRI,
  B1SKINNE - A1SKINNE,
  B1SFDSNE - A1SFDSNE,
  B1SLFEDI - A1SLFEDI,
  B1SDAYDI - A1SDAYDI,
  B1SPIWOR - A1SPIWOR,
  B1SPIFAM - A1SPIFAM,
  B1SHOMET - A1SHOMET,
  B1SPIHOM - A1SPIHOM,
  B1PA1 - A1PA4,
  B1PA2 - A1PA5
)]

dwclean[, c("dParSt", "dFamSt", "dFriSt", "dLfeDi", "dDayDi", "dWorIn", "dFamIn", "dNeiQu", "dHomIn", "dSRPH ", "dSRMH ") := .(
  B1SSPCRI - A1SSPCRI,
  B1SKINNE - A1SKINNE,
  B1SFDSNE - A1SFDSNE,
  B1SLFEDI - A1SLFEDI,
  B1SDAYDI - A1SDAYDI,
  B1SPIWOR - A1SPIWOR,
  B1SPIFAM - A1SPIFAM,
  B1SHOMET - A1SHOMET,
  B1SPIHOM - A1SPIHOM,
  B1PA1 - A1PA4,
  B1PA2 - A1PA5
)]


################################################################################
##                                                                            ##
##                                   NSDE 2                                   ##
##                                                                            ##
################################################################################

## only take people/days with cortisol and where they know their wake up time and woke up in morning
M2P2Cort <- M2P2[B2DCDAY==1 & B2DWAKE3 == 1]

M2P2Cort[, WakeTime := ifelse(B2DWAKE1 < 90, B2DWAKE1, NA) + ifelse(B2DWAKE2 < 90, B2DWAKE2, NA)/60]
## summary(M2P2Cort$WakeTime)
M2P2Cort[, WakeTime := ifelse((ifelse(B2DCORWT < 90, B2DCORWT, NA) - WakeTime) < -.25, NA, WakeTime)]
## prop.table(table(M2P2Cort[, .(A = I(abs(WakeTime - ifelse(B2DCORWT < 90, B2DCORWT, NA)) < .25))]$A, useNA = "always"))
M2P2Cort[, WakeTime := pmin(WakeTime, ifelse(B2DCORWT < 90, B2DCORWT, NA))]
## summary(M2P2Cort$WakeTime)

## hist(M2P2Cort$WakeTime, breaks = 30)

M2P2Cort[, paste0("CortTime", 1:4) := .(
      ifelse(B2DCORWT < 90, B2DCORWT, NA) - WakeTime,
      ifelse(B2DCORAT < 90, B2DCORAT, NA) - WakeTime,
      ifelse(B2DCORLT < 90, B2DCORLT, NA) - WakeTime,
      ifelse(B2DCORBT < 90, B2DCORBT, NA) - WakeTime)]
M2P2Cort[, c("B2DCORW", "B2DCORA", "B2DCORL", "B2DCORB") := .(
      ifelse(B2DCORW < 60, B2DCORW, NA),
      ifelse(B2DCORA < 60, B2DCORA, NA),
      ifelse(B2DCORL < 60, B2DCORL, NA),
      ifelse(B2DCORB < 60, B2DCORB, NA))]

M2P2Cort[, AllergyMeds := ifelse(B2DMED1 == 8, NA, as.integer(B2DMED1 == 1))]
M2P2Cort[, SteroidMeds := as.integer(ifelse(B2DMED2 == 8, NA, as.integer(B2DMED2 == 1)) +
             ifelse(B2DMED3 == 8, NA, as.integer(B2DMED3 == 1)) +
             ifelse(B2DMED4 == 8, NA, as.integer(B2DMED4 == 1)) > 0)]
M2P2Cort[, HormonalMeds := as.integer(ifelse(B2DMED5 == 8, NA, as.integer(B2DMED5 == 1)) +
                                      ifelse(B2DMED6 == 8, NA, as.integer(B2DMED6 == 1)) > 0)]

M2P2Cort[, CortMeds := as.integer(
             ifelse(B2DMED2 == 8, NA, as.integer(B2DMED2 == 1)) == 1 |
             ifelse(B2DMED3 == 8, NA, as.integer(B2DMED3 == 1)) == 1 |
             ifelse(B2DMED4 == 8, NA, as.integer(B2DMED4 == 1)) == 1 |
             ifelse(B2DMED5 == 8, NA, as.integer(B2DMED5 == 1)) == 1 |
             ifelse(B2DMED7 == 8, NA, as.integer(B2DMED7 == 1)) == 1)]

M2P2Cort[, Smoke := ifelse(B2DB2 == 999 | B2DB2 == 0, 0, ifelse(B2DB2 == 998, 2, 1))]
## table(M2P2Cort$Smoke)

M2P2Cort[, AnyPhysSymp := as.integer(B2DA_SYM == 1)]

M2P2Cortl <- melt(M2P2Cort,
           measure.vars = list(
             paste0("CortTime", 1:4),
             paste0("B2DCOR", c("W", "A", "L", "B"))),
           value.name = c("Time", "Cort"))

## xtabs(~I(Time < -.25) + variable, data = M2P2Cortl)
## xtabs(~I(Time > 24) + variable, data = M2P2Cortl)

M2P2Cortl <- M2P2Cortl[Time < 24 & !is.na(Time) & !is.na(Cort)]

## hist(M2P2Cortl$Time, breaks = 30)
## hist(M2P2Cortl$Cort, breaks = 30)

M2P2Cortl[, PeakTime := Time - Time[variable == 2], by = .(M2ID, B2DDAY)]
M2P2Cortl[, Day := as.integer(factor(B2DDAY)), by = M2ID]
M2P2Cortl[, CatTime := n(variable) - 1]

################################################################################
##                                                                            ##
##                                    SLEEP                                   ##
##                                                                            ##
################################################################################

dwl <- reshape(dw[, c("M2ID",
                      paste0("B4WR", 1:7, "SLT"), # rest sleep time
                      ## paste0("B4WS", 1:7, "SLT"), # sleep sleep time
                      paste0("B4WS", 1:7, "ST"), # sleep start time
                      paste0("B4WS", 1:7, "ET"), # sleep end time
                      paste0("B4WS", 1:7, "OL"), # sleep onset latency
                      paste0("B4WS", 1:7, "EFF"), # sleep efficiency
                      paste0("B4WS", 1:7, "WSO") # sleep wake after sleep onset
                      ), with = FALSE],
               varying = list(
                 paste0("B4WR", 1:7, "SLT"), # rest sleep time
                 ## paste0("B4WS", 1:7, "SLT"), # sleep sleep time
                 paste0("B4WS", 1:7, "ST"), # sleep start time
                 paste0("B4WS", 1:7, "ET"), # sleep end time
                 paste0("B4WS", 1:7, "OL"), # sleep onset latency
                 paste0("B4WS", 1:7, "EFF"), # sleep efficiency
                 paste0("B4WS", 1:7, "WSO") # sleep wake after sleep onset
               ),
               v.names = c("TST", "BT", "RT", "SOL", "SE", "WASO"),
               idvar = "M2ID", direction = "long")

dwl[, TST := ifelse(TST %in% c(9997, 9998, 9999), NA, TST)]
dwl[, SOL := ifelse(SOL %in% c(997, 998, 999), NA, SOL)]
dwl[, SE := ifelse(SE %in% c(997, 998, 999), NA, SE)]
dwl[, WASO := ifelse(WASO %in% c(997, 998, 999), NA, WASO)]


dwl <- within(dwl, {
    BT2 <- ftimediff(BT)
    RT2 <- ftimediff2(RT)
})


################################################################################
##                                                                            ##
##                           Measurement Models                               ##
##                                                                            ##
################################################################################

###### PSR Models ######

if (FALSE) {
setwd("~/Onedrive/Projects/MIDUS/Setup/PSR_Measurement/psr_imputed/")

m <- mplusObject(
VARIABLE = "
USEVARIABLES ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;
  IDVARIABLE = M2ID;
  CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L*-1;
  constraint BY B1SE4A@-1  B1SE4B*-1  B1SE4D*-1
    B1SE4E*-1  B1SE4G*-1  B1SE4J*-1  B1SE4K*-1  B1SE4I*-1;
  selfesteem BY B1SE4M*1  B1SE4N@-1
    B1SE4O*1  B1SE4P*-1  B1SE4Q*1  B1SE4R*-1  B1SE4S*1;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B*-1  B1SJ2C*-1  B1SJ2D*-1;
  friendsupt BY B1SJ4A@-1  B1SJ4B*-1  B1SJ4C*-1  B1SJ4D*-1;
  spousesupt BY B1SL11A@-1  B1SL11B*-1  B1SL11C*-1
    B1SL11D*-1  B1SL11E*-1  B1SL11F*-1;
  PR BY mastery*1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt*1  friendsupt*1  spousesupt*1;
  PR@1; SR@1;
  PR WITH SR*;
",
OUTPUT = "STDYX;",
usevariables = c(unlist(psr.items$M2items), "M2ID", "M2FAMNUM"),
rdata = dmerged[, c("M2ID", "M2FAMNUM", psr.items$M2items)])

m <- mplusModeler(m, "MIDUS_psrs.dat", run=TRUE)
m$results$summaries

m.imputed <- update(m,
ANALYSIS = ~ "
  ESTIMATOR = BAYES;
  PROCESSORS = 8;
  BSEED = 12345;",
SAVEDATA = ~ "
  DATA IMPUTATION:
  NDATASETS = 50;
  SAVE = psr_imp*.dat;
  SAVEDATA: FILE = psr_plaus.dat;
  SAVE = FSCORES (50);
  FACTORS = mastery constraint selfesteem optimism pessimism
   familysupt friendsupt spousesupt pr sr;")

# took about 20000 iterations
m.imputed <- mplusModeler(m.imputed, "MIDUS_psrs_imputed.dat", run=TRUE)

}

psr.imputed.vars <- Items$PSR$M2items
psr.factor.vars <- c("MASTERY", "CONSTRAINT", "SELFESTEEM", "OPTIMISM",
"PESSIMISM", "FAMILYSUPT", "FRIENDSUPT", "SPOUSESUPT", "PR", "SR")

psr.data <- lapply(1:50, function(i) {
  psr.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/Setup/PSR_Measurement/psr_imputed/psr_imp", i, ".dat"), na.strings = "*")
  colnames(psr.d) <- c(psr.imputed.vars, "M2ID", psr.factor.vars)
  out <- cbind(Imputation = i, psr.d[, c("M2ID", psr.factor.vars)])
  colnames(out)[3:12] <- paste0("B1", colnames(out)[3:12])
  return(out)
})

psr.factor.vars <- paste0("B1", psr.factor.vars)

## mean(sapply(psr.data, function(x) {with(x, cor(B1PR, B1SR))}))


###### PA NA Models ######

if (FALSE) {

setwd("~/Onedrive/Projects/MIDUS/Setup/PANA_Measurement/pana_imputed/")

m.pana <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
  ! NA items
  B1SA24A B1SA24B B1SA24C B1SA24D B1SA24E B1SA24F
  B1SA24H B1SA24I B1SA24J B1SA24K B1SA24L
  B1SA24M B1SA24N ! drop
  ! PA items
  B1SA26A B1SA26B B1SA26C B1SA26D B1SA26E
  B1SA26F B1SA26I B1SA26J B1SA26K B1SA26L
  B1SA26M ! drop
;

CATEGORICAL ARE
  ! NA items
  B1SA24A B1SA24B B1SA24C B1SA24D B1SA24E B1SA24F
  B1SA24H B1SA24I B1SA24J B1SA24K B1SA24L
  B1SA24M B1SA24N ! drop
  ! PA items
  B1SA26A B1SA26B B1SA26C B1SA26D B1SA26E
  B1SA26F B1SA26I B1SA26J B1SA26K B1SA26L
  B1SA26M ! drop
;
  IDVARIABLE = M2ID;
  CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  NA BY B1SA24A*-1 B1SA24B*-1 B1SA24C*-1 B1SA24D*-1 B1SA24E*-1 B1SA24F*-1
    B1SA24H*-1 B1SA24I*-1 B1SA24J*-1 B1SA24K*-1 B1SA24L*-1
  B1SA24M B1SA24N ! drop
  ;
  PA BY B1SA26A*-1 B1SA26B*-1 B1SA26C*-1 B1SA26D*-1 B1SA26E*-1
  B1SA26F*-1 B1SA26I*-1 B1SA26J*-1 B1SA26K*-1 B1SA26L*-1
  B1SA26M ! drop
  ;
  NA@1;
  PA@1;
",
OUTPUT = "STDYX;",
usevariables = c(mood.items$M2items, "M2ID", "M2FAMNUM"),
rdata = dmerged2)

m.pana <- mplusModeler(m.pana, "MIDUS_pana.dat", run=TRUE)

screenreg(m.pana, single.row = TRUE, type = "stdyx", param = "load")

m.pana.imputed <- update(m.pana,
ANALYSIS = ~ "
  ESTIMATOR = BAYES;
  PROCESSORS = 8;
  BSEED = 12345;",
SAVEDATA = ~ "
  DATA IMPUTATION:
  NDATASETS = 50;
  SAVE = pana_imp*.dat;
  SAVEDATA: FILE = pana_plaus.dat;
  SAVE = FSCORES (50);
  FACTORS = NA PA;")

# took about 5000 iterations
m.pana.imputed <- mplusModeler(m.pana.imputed, "MIDUS_pana_imputed.dat", run=TRUE)
}

pana.imputed.vars <- c(Items$Mood$M2items[c(1:11, 14:23)], "M2ID")
pana.factor.vars <- c("NegAff", "PosAff")

pana.data <- lapply(1:50, function(i) {
  pana.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/Setup/PANA_Measurement/pana_imputed/pana_imp", i, ".dat"), na.strings = "*")
  colnames(pana.d) <- c(pana.imputed.vars, pana.factor.vars)
  out <- cbind(Imputation = i, pana.d[, c("M2ID", pana.factor.vars)])
  colnames(out)[3:4] <- paste0("B1", colnames(out)[3:4])
  return(out)
})

pana.factor.vars <- paste0("B1", pana.factor.vars)


################################################################################
##                               Save Data Files                              ##
################################################################################

saveRDS(dw, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dw.RDS", compress = "xz")
saveRDS(dwclean, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dwclean.RDS", compress = "xz")
saveRDS(dwl, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dwl.RDS", compress = "xz")


################################################################################
#                                                                              #
#                              Imputed AL Scores                               #
#                                                                              #
################################################################################

imputed.data.vars <- c(
  "Radj_epi", "Radj_nor", "avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf",
  "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam",
  "b4bsesel", "pulpress", "Rb4p1gs", "Rb4p1d", "b4bha1c",
  "Rb4bgluc", "p4homair", "b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr",
  "b1pgender", "b1page_m2", "M2ID")
bi.vars <- c("bi.sym", "bi.hpa", "bi.card", "bi.gluc", "bi.lipid", "bi.infl", "bi.para", "F")

bi.data.nosbpgluc <- lapply(1:50, function(i) {
  bi.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/Setup/AL_Measurement/factor_allcov_BiFacplus_nosbpgluc/bialplus_imp", i, ".dat"), na.strings = "*")
  colnames(bi.d) <- c(imputed.data.vars, bi.vars)
  out <- cbind(Imputation = i, bi.d[, c("M2ID", bi.vars)])
  colnames(out)[3:ncol(out)] <- paste0("B4", colnames(out)[3:ncol(out)])
  return(out)
})

bi.data <- lapply(1:50, function(i) {
  bi.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/Setup/AL_Measurement/factor_allcov_BiFacplus/bialplus_imp", i, ".dat"), na.strings = "*")
  colnames(bi.d) <- c(imputed.data.vars, bi.vars)
  out <- cbind(Imputation = i, bi.d[, c("M2ID", bi.vars)])
  colnames(out)[3:ncol(out)] <- paste0("B4", colnames(out)[3:ncol(out)])
  return(out)
})


bi.vars <- paste0("B4", bi.vars)

psr_pana_bi.data <- lapply(1:50, function(i) {
    tmp <- merge(psr.data[[i]][, c("M2ID", "Imputation", psr.factor.vars)],
                 bi.data[[i]][, -which(colnames(bi.data[[i]]) %in% "Imputation")], , by = "M2ID", all=TRUE)
    tmp <- merge(tmp, pana.data[[i]][, -which(colnames(pana.data[[i]]) %in% "Imputation")], by = "M2ID", all = TRUE)
    as.data.table(tmp)
})

psr_pana_bi.data.nosbpgluc <- lapply(1:50, function(i) {
    tmp <- merge(psr.data[[i]][, c("M2ID", "Imputation", psr.factor.vars)],
                 bi.data.nosbpgluc[[i]][, -which(colnames(bi.data.nosbpgluc[[i]]) %in% "Imputation")], , by = "M2ID", all=TRUE)
    tmp <- merge(tmp, pana.data[[i]][, -which(colnames(pana.data[[i]]) %in% "Imputation")], by = "M2ID", all = TRUE)
    as.data.table(tmp)
})


names(psr_pana_bi.data[[1]])

################################################################################
##                               Save Data Files                              ##
################################################################################


saveRDS(psr_pana_bi.data, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_psrpanabi_imp.RDS", compress = "xz")
saveRDS(psr_pana_bi.data.nosbpgluc, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_psrpanabi_nosbpgluc_imp.RDS", compress = "xz")
saveRDS(M2P2Cort, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_M2P2Cort.RDS", compress = "xz")
saveRDS(M2P2Cortl, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_M2P2Cortl.RDS", compress = "xz")

################################################################################
##                             Create Imputed Data                            ##
################################################################################

if (FALSE) {
dwimp <- lapply(1:50, function(i) {
  merge(dw, psr_pana_bi.data[[i]], by = "M2ID", all = TRUE)
})

dwcleanimp <- lapply(1:50, function(i) {
  merge(dwclean, psr_pana_bi.data[[i]], by = "M2ID", all = TRUE)
})

dwlimp <- lapply(1:50, function(i) {
  merge(dwl, psr_pana_bi.data[[i]], by = "M2ID", all = TRUE)
})
}

## save(dw, dwclean, dwl, dwimp, dwcleanimp, dwlimp, M2P2Cort, M2P2Cortl, file = "~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data.RData", compress = "xz")


## tmpdat <- dw[!is.na(A1AllAbuse) & !is.na(ParentalAffection) & !is.na(LastAge) & !is.na(DECEASED), .(
##   A1AllAbuse, ParentalAffection, A1SEMA, A1SEFA,
##   B1SPWBR1, B1SSWBSI, B1SFAMSO, B1SFDSOL, B1SSPSOL, B1SRELSU, B1SSUFFI,
##   B1SRELCA, B1SSPIRI, B1SRELPR, B1SSPRTE, B1SMNDFU, B1SINTER, B1SPWBG2, B1SPWBU1,
##   B1SDIREC, B1SFORSG, B1SINSGH, B1SPERSI, B1SSPCTR, B1SSSCTR,
##   B1SREAPP, B1SCSCSP, B1SPRCOP, B1SREINT, B1SACTIV, B1SPLAN,
##   LastAge, DECEASED, AGEM1, AGEM2, Sex, RaceG3)]
## tmpdat <- tmpdat[rowSums(is.na(tmpdat)) < 27]
## write.dta(as.data.frame(tmpdat), file = "~/Desktop/MIDUS_gemma.dta", version = 12)
## summary(coxph(Surv(LastAge, DECEASED) ~ A1AllAbuse + Sex, data = dw))
## summary(coxph(Surv(LastAge, DECEASED) ~ A1AllAbuse + Sex, data = tmpdat))
## summary(coxph(Surv(LastAge, DECEASED) ~ Sex + A1AllAbuse * ParentalAffection, data = tmpdat))
## summary(coxph(Surv(LastAge, DECEASED) ~ Sex + ParentalAffection, data = tmpdat))
## summary(coxph(Surv(LastAge, DECEASED) ~ Sex + ParentalAffection, data = dw))
## summary(coxph(Surv(LastAge, DECEASED) ~ Sex + A1AllAbuse * B1SFDSOL, data = tmpdat))
## summary(coxph(Surv(LastAge, DECEASED) ~ Sex + A1AllAbuse * B1SFDSOL, data = dw))

