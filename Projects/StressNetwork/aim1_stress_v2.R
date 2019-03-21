source("../MIDUS-Setup/setup_packages_functions.R")
options(stringsAsFactors = FALSE, digits = 3)

library(powerSurvEpi)
powerEpiCont.default(n = 7103, theta = 1.10, sigma2 = 1, psi = .166, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 6708, theta = 1.10, sigma2 = 1, psi = .117, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 6708, theta = 1.10, sigma2 = 1, psi = .2, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 6000, theta = 1.10, sigma2 = 1, psi = .15, rho2 = 0, alpha = .05)


## code based off mice::pool.compare
pool.compare.null <- function (fit1) {
    m1 <- length(fit1$analyses)
    m <- m1
    est1 <- mice::pool(fit1)
    dimQ1 <- length(est1$qbar)
    dimQ2 <- dimQ1 - 0
    formula1 <- formula(fit1$analyses[[1]])
    Q <- diag(rep(1, dimQ1), ncol = dimQ1)
    Q <- matrix(Q[((dimQ1 - dimQ2 + 1):dimQ1), ], nrow = dimQ2,
                ncol = dimQ1)
    qbar <- Q %*% est1$qbar
    Ubar <- Q %*% est1$ubar %*% (t(Q))
    Bm <- Q %*% est1$b %*% (t(Q))
    rm <- (1 + 1/m) * sum(diag(Bm %*% (solve(Ubar))))/dimQ2
    Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar/(dimQ2 * (1 +
                                                         rm))
    v <- dimQ2 * (m - 1)
    if (v > 4)
        w <- 4 + (v - 4) * ((1 + (1 - 2/v) * (1/rm))^2)
    else w <- v * (1 + 1/dimQ2) * ((1 + 1/rm)^2)/2
    statistic <- list(Dm = Dm, rm = rm, df1 = dimQ2, df2 = w,
        pvalue = 1 - pf(Dm, dimQ2, w))
    return(statistic)
}

format.mortality <- function(object) {
  res <- summary(mice::pool(as.mira(object)))
  data.table(
    IV = rownames(res),
    Est = sprintf("%0.2f%s\n[%0.2f, %0.2f]",
                  exp(res[, "est"]),
                  star(res[, "Pr(>|t|)"]),
                  exp(res[, "lo 95"]),
                  exp(res[, "hi 95"])))
}

pool.cindex <- function(obj) {
  tanh(mean(atanh(sapply(1:50, function(i) summary(obj[[i]])$concordance[[1]] ))))
}


mVars <- as.data.table(read.xlsx("../MIDUS-Setup/midus_master_vars.xlsx", sheetIndex = 1))
## setkey(mVars, "Domain")

mVars[Domain == "Stress", .(Name, Abbr, M1)][!is.na(M1)]
mVars[Domain == "Stress", .(Name, Abbr, M2)][!is.na(M2)]

## not done, confusing
## dw[, A1Work := recode(A1PBWORK, "c(1, 2) = 'employed'; c(3, 4) = 'unemployed'; c(5) = 'retired'; c(6) = 'homemaker'")]

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
B1ChildLifeStress	Childhood Stressful Life Events (M2)
B1LifeStress	Stressful Life Events (M2)
B1LifeStressImpactS	Short Term Impact of SLEs (M2)
B1LifeStressImpactL	Long Term Impact of SLEs (M2)
B4QCT_SA	Sexual Abuse (M2P4)
B4QCT_PN	Physical Neglect (M2P4)
B4QCT_PA	Physical Abuse (M2P4)
B4QCT_EA	Emotional Abuse (M2P4)
B4QCT_EN	Emotional Neglect (M2P4)
A1ParentAbuse	Parent Abuse (M1)
A1SiblingAbuse	Sibling Abuse (M1)
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



dwclean <- readRDS("../MIDUS-Setup/midus_merged_data_dwclean.RDS")
psr_pana_bi.data <- readRDS("../MIDUS-Setup/midus_merged_data_psrpanabi_imp.RDS")
dwcleanimp <- readRDS("../MIDUS-Setup/stressnetwork_imputation/midus_stress_final_imputed.RDS")

v <- list(
  stress = c(
    ## M1
    "A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SHOMET", "A1SPIHOM",
    "A1ParentAbuse", "A1SiblingAbuse",
    ## M2 (Retrospective)
    "B1ChildLifeStress", "B4CTQTotal",
    "B1LifeStress", "B1LifeStressImpactS", "B1LifeStressImpactL",
    ## M2
    "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM",
    "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
    "B1SLFEDI", "B1SDAYDI", "B4QPS_PS",
    ## change variables
    "dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi",
    "dFriSt", "dFamSt", "dParSt"
  ),
  health.cat = c(
  "A1PA4", "A1PA5", "A1PA6",
  "B1PA1", "B1PA2", "B1PA3",
  "A1PDEPDX", "A1PANXTD",
  "B1PDEPDX", "B1PANXTD"
  ),
  health = c(
  "A1SCHRON", "B1SCHRON", "C1SCHRON", #"p4majorconditions",
  "b3tem", "exec_fxn",
  "Radj_epi", "Radj_nor", "avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf",
  "Rb4p1d", "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6", "b4bfgn",
  "b4bsicam", "b4bsesel", "pulpress", "Rb4p1gs", "b4bha1c", "Rb4bgluc",
  "p4homair", "b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr",
  ## mood
  "B1NegAff", "B1PosAff", "B1PR", "B1SR",
  ## AL and systems
  "B4F", "B4bi.sym", "B4bi.hpa", "B4bi.card",
  "B4bi.gluc", "B4bi.lipid", "B4bi.infl", "B4bi.para"
  ),
  cov = c(
    "A1SBMI", "B1SBMI", "C1SBMI",
    "A1SWSTHI", "B1SWSTHI", "C1SWSTHI",
    "BirthYear", "AGEM1", "AGEM2",
    ## health behaviors
    "A1PhysAct", "B1PhysAct", "C1PhysAct",
    "B1Sleep", "B1SSQ", "C1Sleep", "C1SSQ",
    ## SES
    "m1chadv", "m2aadv",
    ## psychosocial resources / personality / coping
    "B1SREINT", "B1SACTIV", "B1SPLAN", "B1SVENT", "B1SDENIA",
    "B1SDISEN", "B1SFDCOP", "B1SAGENC", "B1SAGREE", "B1SCONS2", "B1SEXTRA",
    "B1SNEURO", "B1SOPEN", "B1SPWBA2", "B1SPWBG2", "B1SPWBE2", "B1SPWBU2",
    "B1SPWBS2", "B1SPWBR2", "B1SMASTE", "B1SCONST", "B1SSWBSI", "B1SKINPO",
    "B1SFDSPO", "B1SSPEMP", "B1SESTEE", "B1SMPQCN", "B1SOPTIM", "B1SPESSI"
  ),
  cov.cat = c(
    "Sex", "RaceG3",
    ## health insurance
    "A1SC1", "B1SC1", "C1SC1",
    ## health behaviors
    "A1Smoke", "B1Smoke", "C1Smoke",
    "B1CurrentAlcohol", "A1WorstAlcohol", "B1WorstAlcohol", "C1CurrentAlcohol", "C1WorstAlcohol",
    ## bed partner
    #"B4S9",
    ## education & work status
    "A1PB1", "B1PB1", "C1PB1",
    "A1PBWORK", "B1PBWORK", "C1PB3WK",
    ## SES
    "m1welf_all", "m1ped3_all", "m1cses_all",
    "m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all"
  ),
  psr = c("B1PSRQSFPR", "B1PSRQSFSR"),
  noimp = c("M2ID", "M2FAMNUM", "LastAge", "DECEASED")
)

## only includes MIDUS participants with some stress or health measure
nrow(dwcleanimp[[1]]) ## total

useIDs <- dwcleanimp[[1]][["M2ID"]]

dwclean.use <- dwclean[match(useIDs, dwclean$M2ID)]
table(dwclean.use$M2ID %in% useIDs)

dwclean.use$PAfterM2 <- rowMeans(sapply(dwcleanimp, function(d) with(d, LastAge > AGEM2 | DECEASED == 0)))

table(dwclean.use$PAfterM2)


for (i in 1:50) {
  dwcleanimp[[i]]$PAfterM2 <- dwclean.use$PAfterM2
}

################################################################################
##                                                                            ##
##                            Descriptive Statistics                          ##
##                                                                            ##
################################################################################

egltable(c(
  "A1SLFEDI", "B1SLFEDI",
  "A1SDAYDI", "B1SDAYDI", "B1SJOBDI",
  "A1SPIWOR", "B1SPIWOR",
  "A1SPIHOM", "B1SPIHOM",
  "A1SPIFAM", "B1SPIFAM",
  "B4QPS_PS",
  "A1SKINNE", "B1SKINNE",
  "A1SFDSNE", "B1SFDSNE",
  "A1SSPCRI", "B1SSPCRI",
  "B1LifeStress"
  ),
  data = as.data.frame(dwclean.use), parametric=FALSE)

egltable(c(
  "Sex", "m1ped3_all",
  "A1PB1", "B1PB1",
  "A1PBWORK", "B1PBWORK",
  "A1Smoke", "B1Smoke",
  "A1WorstAlcohol", "B1WorstAlcohol",
  "A1SWSTHI", "B1SWSTHI",
  "AGEM1", "AGEM2"),
  data = as.data.frame(dwclean.use), parametric=TRUE, strict=FALSE)


################################################################################
##                                                                            ##
##                                Exploratory                                 ##
##                                                                            ##
################################################################################


tmplongdat <- melt(as.data.frame(scale(dwclean.use[, c("M2ID", grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE)), with = FALSE])), id.vars = "M2ID")

tmplongdat$variable <- factor(tmplongdat$variable,
                              levels = levels(tmplongdat$variable),
                              labels = stresskey2$Label[match(levels(tmplongdat$variable), stresskey2$Var)])

ggplot(tmplongdat,
       aes(value)) +
  geom_histogram(aes(y = ..density..), bins = 20) +
  geom_density() +
  theme_cowplot() +
  facet_wrap(~variable, scales = "free") + ggtitle("Stress Meausures for MIDUS 1 (A) and MIDUS 2 (B)") +
  xlab("Z Scores")

napprox <- colSums(!is.na(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE]))

r.pearson <- cor(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE],
                 use = "pairwise.complete.obs", method = "pearson")
r.spearman <- cor(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE],
                 use = "pairwise.complete.obs", method = "spearman")

colnames(r.pearson) <- rownames(r.pearson) <- stresskey2$Label[match(colnames(r.pearson), stresskey2$Var)]

colnames(r.spearman) <- rownames(r.spearman) <- stresskey2$Label[match(colnames(r.spearman), stresskey2$Var)]

hist((r.pearson - r.spearman)[lower.tri(r.pearson)], breaks = 30,
     xlab = "Difference between Pearson and Spearman",
     main = "Histogram of Correlation Differences")

corplot(r.pearson[1:11, 1:11], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Pearson")

corplot(r.pearson[-(1:11), -(1:11)], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Pearson")



hist(sapply(1:50, function(i) {
alpha(scale(as.data.table(dwcleanimp[[i]])[, .(A1SLFEDI, A1SDAYDI,
                                A1SPIWOR, A1SPIHOM, A1SPIFAM)]))$total$std.alpha
}))


################################################################################
##                                                                            ##
##                       Stress Grouping / Clustering                         ##
##                                                                            ##
################################################################################


alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI,
                                               A1SPIWOR, A1SPIHOM, A1SPIFAM)]))

alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI)]))

alpha(scale(dwclean.use[, .(A1SPIWOR, A1SPIHOM, A1SPIFAM)]))


alpha(scale(dwclean.use[, .(A1SKINNE, A1SFDSNE, A1SSPCRI)]))

alpha(scale(dwclean.use[, .(B1SLFEDI, B1SDAYDI, B1SJOBDI,
                      B1SPIWOR, B1SPIHOM, B1SPIFAM)]))
alpha(scale(dwclean.use[, .(B4QPS_PS, B1SKINNE, B1SFDSNE, B1SSPCRI)]))

m1.cfa1 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m1_stresscfa1.dat", run = 1L)

summary(m1.cfa1)
coef(m1.cfa1, type = "stdyx", param = "loading")
screenreg(m1.cfa1, type = "stdyx", param = "loading", single.row = TRUE)

m1.cfa2 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m1_stresscfa2.dat", run = 1L)
summary(m1.cfa2)
coef(m1.cfa2, type = "stdyx", param = "loading")
screenreg(m1.cfa2, type = "stdyx", param = "loading", single.row = TRUE)

dwclean.use.m1.cat <- copy(dwclean.use[, m1.cfa1$usevariables, with=FALSE])
dwclean.use.m1.cat[, (colnames(dwclean.use.m1.cat)) :=
                       lapply(.SD, function(x) {
                         cut(x, breaks = unique(quantile(x,
                           probs = seq(0, 1, by = .2),
                           na.rm = TRUE)),
                           include.lowest = TRUE, ordered_result = TRUE)
                       })
                   ]


m1.cfa3 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa3.dat", run = 1L)
coef(m1.cfa3, type = "stdyx", param = "loading")

m1.cfa3b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa3b.dat", run = 1L)
m1.cfa3b$results$summaries
coef(m1.cfa3b, type = "stdyx", param = "loading")

m1.cfa4 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa4.dat", run = 1L)
coef(m1.cfa4, type = "stdyx", param = "loading")

m1.cfa4b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa4b.dat", run = 1L)
m1.cfa4b$results$summaries


screenreg(list(
  M1 = extract(m1.cfa1, type = "stdyx", param = "loading"),
  M1CatML = extract(m1.cfa3, type = "stdyx", param = "loading"),
  M1CatWLSMV = extract(m1.cfa3b, type = "stdyx", param = "loading")
  ))

summary(m1.cfa1)
m1.cfa3b$results$summaries

screenreg(list(
  M1Alt = extract(m1.cfa2, type = "stdyx", param = "loading"),
  M1AltCatML = extract(m1.cfa4, type = "stdyx", param = "loading"),
  M1AltCatWLSMV = extract(m1.cfa4b, type = "stdyx", param = "loading")
  ))

summary(m1.cfa2)
m1.cfa4b$results$summaries



m2.cfa1 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = subset(dwclean.use, PAfterM2==1)), dataout = "m2_stresscfa1.dat", run = 1L)
summary(m2.cfa1)
coef(m2.cfa1, type = "stdyx", param = "loading")
screenreg(m2.cfa1, type = "stdyx", param = "loading", single.row = TRUE)

m2.cfa2 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = subset(dwclean.use, PAfterM2==1)), dataout = "m2_stresscfa2.dat", run = 1L)
summary(m2.cfa2)
coef(m2.cfa2, type = "stdyx", param = "loading")
screenreg(m2.cfa2, type = "stdyx", param = "loading", single.row = TRUE)

dwclean.use.m2.cat <- copy(dwclean.use[, m2.cfa1$usevariables, with=FALSE])
dwclean.use.m2.cat[, (colnames(dwclean.use.m2.cat)) :=
                       lapply(.SD, function(x) {
                         cut(x, breaks = unique(quantile(x,
                           probs = seq(0, 1, by = .2),
                           na.rm = TRUE)),
                           include.lowest = TRUE, ordered_result = TRUE)
                       })
                   ]


m2.cfa3 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa3.dat", run = 1L)
coef(m2.cfa3, type = "stdyx", param = "loading")

m2.cfa3b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa3b.dat", run = 1L)
m2.cfa3b$results$summaries
coef(m2.cfa3b, type = "stdyx", param = "loading")

m2.cfa4 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa4.dat", run = 1L)
coef(m2.cfa4, type = "stdyx", param = "loading")

m2.cfa4b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa4b.dat", run = 1L)
m2.cfa4b$results$summaries

screenreg(list(
  M2 = extract(m2.cfa1, type = "stdyx", param = "loading"),
  M2CatML = extract(m2.cfa3, type = "stdyx", param = "loading"),
  M2CatWLSMV = extract(m2.cfa3b, type = "stdyx", param = "loading")
  ))

screenreg(list(
  M2Alt = extract(m2.cfa2, type = "stdyx", param = "loading"),
  M2AltCatML = extract(m2.cfa4, type = "stdyx", param = "loading"),
  M2AltCatWLSMV = extract(m2.cfa4b, type = "stdyx", param = "loading")
  ))


## LCA ### NOTE THAT THIS DID NOT WORK WELL ###

m2.lca <- lapply(1:10, function(k) {
  resvars <- do.call(paste, c(lapply(1:k, function(i) {
    gsub("%i%", i, "
   %C#%i%%
   [B1SLFEDI* B1SDAYDI* B1SJOBDI*
    B1SPIWOR* B1SPIHOM* B1SPIFAM*
    B4QPS_PS* B1SKINNE* B1SFDSNE* B1SSPCRI*];
   B1SLFEDI* B1SDAYDI* B1SJOBDI*
    B1SPIWOR* B1SPIHOM* B1SPIFAM*
    B4QPS_PS* B1SKINNE* B1SFDSNE* B1SSPCRI*;
  ")
  }), collapse = "\n"))

  mplusModeler(mplusObject(
    VARIABLE = gsub("%k%", k, "
    CLASSES = c (%k%);
  "),
  ANALYSIS = "
   TYPE = MIXTURE;
   ESTIMATOR = BAYES;
   BITERATIONS = 20000 (4000);
   STARTS = 600 120;
   STVALUES = ML;
   PROCESSORS = 8;
   ",
  MODEL = "
   %OVERALL%
   [B1SLFEDI* B1SDAYDI* B1SJOBDI*
    B1SPIWOR* B1SPIHOM* B1SPIFAM*
    B4QPS_PS* B1SKINNE* B1SFDSNE* B1SSPCRI*];
   B1SLFEDI* B1SDAYDI* B1SJOBDI*
    B1SPIWOR* B1SPIHOM* B1SPIFAM*
    B4QPS_PS* B1SKINNE* B1SFDSNE* B1SSPCRI*;"
,
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = sprintf("m2_stress_lca%s.dat", as.character(k)), run = 1L)
})


m4 <- readModels("m2_stress_lca4.out")
tmp <- coef(m4, type = "stdyx", param = "expectation")
tmp$Class <- gsub("C_([1-6]).*", "\\1", tmp$Label)
tmp$Var <- substr(tmp$Label, 5, 12)
v <- c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI", "B1SPIWOR", "B1SPIFAM",
       "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B4QPS_PS")
tmp$Variable <- factor(tmp$Var,
                       levels = v,
                       labels = stresskey2$Label[match(v, stresskey2$Var)])

ggplot(tmp, aes(as.numeric(Variable), est, shape = Class, colour = Class)) +
  geom_line() + geom_point() +
  scale_x_continuous("", breaks = 1:10, labels = levels(tmp$Variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



m4 <- readModels("m2_stress_lca10.out")
tmp <- coef(m4, type = "stdyx", param = "expectation")
tmp$Class <- gsub("C_([0-9]*).*", "\\1", tmp$Label)
tmp$Var <- ifelse(tmp$Class == 10,
                  substr(tmp$Label, 6, 13),
                  substr(tmp$Label, 5, 12))
v <- c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI", "B1SPIWOR", "B1SPIFAM",
       "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B4QPS_PS")
tmp$Variable <- factor(tmp$Var,
                       levels = v,
                       labels = stresskey2$Label[match(v, stresskey2$Var)])

ggplot(tmp, aes(as.numeric(Variable), est, colour = Class)) +
  geom_line() + geom_point() +
  scale_x_continuous("", breaks = 1:10, labels = levels(tmp$Variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


m2.lca[[1]]$results$summaries$Entropy <- 1

coef(m2.lca[[1]]$results, type = "stdyx", params = "expectation")

tmp <- coef(m2.lca[[4]]$results, type = "stdyx", params = "expectation")
## tmp$Class <- factor(gsub("C_[1-4]tmp$Label"))

m2.lca.fits <- do.call(rbind, lapply(1:10, function(i) {
  if (i < 7) {
  cbind(NClasses = i,
        m2.lca[[i]]$results$summaries[, c("Parameters", "AIC", "BIC", "aBIC", "AICC", "Entropy")],
        SmallestClass = min(m2.lca[[i]]$results$class_counts$mostLikely[, "count"]))
  } else {
  cbind(NClasses = i,
        m2.lcab[[i-6]]$results$summaries[, c("Parameters", "AIC", "BIC", "aBIC", "AICC", "Entropy")],
        SmallestClass = min(m2.lcab[[i-6]]$results$class_counts$mostLikely[, "count"]))
    }
}))

m2.lca.fitsl <- melt(m2.lca.fits, id.vars = "NClasses")

ggplot(subset(m2.lca.fitsl, NClasses != 1), aes(NClasses, value)) +
  geom_line() + geom_point() +
  facet_wrap(~variable, scales = "free") +
  theme_cowplot()

m2.lca3 <- mplusModeler(mplusObject(
  VARIABLE = "
    CLASSES = c (3);
  ",
  ANALYSIS = "
   TYPE = MIXTURE;
   ESTIMATOR = MLR;
   PROCESSORS = 2;
   ",
  MODEL = "
   %OVERALL%
   [B1SLFEDI B1SDAYDI B1SJOBDI
    B1SPIWOR B1SPIHOM B1SPIFAM
    B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI];
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m2_stress_lca3.dat", run = 1L)

summary(m2.cfa1)
coef(m2.cfa1, type = "stdyx", param = "loading")



################################################################################
##                                                                            ##
##                              Mortality Model                               ##
##                                                                            ##
################################################################################

## covariates
use.cov.m1stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all"),
 c("A1PB1", "A1PBWORK"),
 c("A1Smoke", "A1WorstAlcohol", "A1PhysAct", "A1SWSTHI"))

use.cov.m2stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all"),
 c("B1PB1", "B1PBWORK"),
 c("B1Smoke", "B1WorstAlcohol", "B1PhysAct", "B1SWSTHI"))


m1.stress.vars <- list("",
  c("A1SLFEDI", "A1SDAYDI"),
  c("A1SPIWOR", "A1SPIHOM", "A1SPIFAM"),
  c("A1SKINNE", "A1SFDSNE", "A1SSPCRI"))

m2.stress.vars.full <- list("",
  c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI"),
  c("B1SPIWOR", "B1SPIHOM", "B1SPIFAM"),
  c("B4QPS_PS", "B1SKINNE", "B1SFDSNE", "B1SSPCRI"),
  "B1LifeStress")

m2.stress.vars.reduced <- list("",
  c("B1SLFEDI", "B1SDAYDI"),
  c("B1SPIWOR", "B1SPIHOM", "B1SPIFAM"),
  c("B1SKINNE", "B1SFDSNE", "B1SSPCRI"))


dwcleanimp <- lapply(dwcleanimp, function(d) {
  d <- within(d, {
    DECEASED <- as.integer(DECEASED == "1")
    A1PB1 <- as.integer(as.character(A1PB1))
    B1PB1 <- as.integer(as.character(B1PB1))
    A1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
    B1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
  })
  d <- within(d, {
    A1PB1 <- recode(A1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
    B1PB1 <- recode(B1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
  })
  d <- d[, unique(unlist(c(use.cov.m1stress[-1], use.cov.m2stress[-1], m1.stress.vars[-1], m2.stress.vars.full[-1],
                           "LastAge", "DECEASED", "M2FAMNUM", "M2ID", "PAfterM2", "AGEM2")))]
  return(d)
})


m1.stress.vars <- lapply(m1.stress.vars, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
m2.stress.vars.full <- lapply(m2.stress.vars.full, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
m2.stress.vars.reduced <- lapply(m2.stress.vars.reduced, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")


## for random forest
## indices <- subset(expand.grid(cov = 1:4, iv = 1:5), iv > 1 | cov > 1)
indices <- expand.grid(cov = 1:4, iv = 1:5)
indices <- subset(expand.grid(cov = 1:4, iv = 1:5), iv > 1 | cov > 1)

detach("package:caret")
indices$m1f <- sapply(1:nrow(indices), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM)%s%s",
          paste(unlist(use.cov.m1stress[1:indices$cov[n]]), collapse = " + "),
          paste0(ifelse(indices$iv[n]>1, " + ", ""),
                 paste(if(indices$iv[n] <= length(m1.stress.vars)) m1.stress.vars[[indices$iv[n]]] else unlist(m1.stress.vars[-1]),
                collapse = " + ")))
})

indices$m2f.reduced <- sapply(1:nrow(indices), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM)%s%s",
          paste(unlist(use.cov.m2stress[1:indices$cov[n]]), collapse = " + "),
          paste0(ifelse(indices$iv[n]>1, " + ", ""),
          paste(if(indices$iv[n] <= length(m2.stress.vars.reduced)) m2.stress.vars.reduced[[indices$iv[n]]] else unlist(m2.stress.vars.reduced[-1]),
                collapse = " + ")))
})


indices2 <- expand.grid(cov = 1:4, iv = 1:6)
indices2 <- subset(expand.grid(cov = 1:4, iv = 1:6), iv > 1 | cov > 1)

indices2$m2f.full <- sapply(1:nrow(indices2), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM)%s%s",
          paste(unlist(use.cov.m2stress[1:indices2$cov[n]]), collapse = " + "),
          paste0(ifelse(indices2$iv[n]>1, " + ", ""),
                 paste(if(indices2$iv[n] <= length(m2.stress.vars.full)) m2.stress.vars.full[[indices2$iv[n]]] else unlist(m2.stress.vars.full[-1]),
                collapse = " + ")))
})


mortality.coxph.results <- list(
  m1f = c(NA, lapply(1:nrow(indices), function(n) {
    lapply(1:50, function(i) {
      coxph(as.formula(indices$m1f[n]), data = dwcleanimp[[i]])
    })
  })),
  m2f.full = c(NA, lapply(1:nrow(indices2), function(n) {
    lapply(1:50, function(i) {
      coxph(as.formula(indices2$m2f.full[n]), data = subset(dwcleanimp[[i]], PAfterM2 == 1))
    })
  }))## ,
  ## m2f.reduced = c(NA, lapply(1:nrow(indices), function(n) {
  ##   lapply(1:50, function(i) {
  ##     coxph(as.formula(indices$m2f.reduced[n]), data = subset(dwcleanimp[[i]], PAfterM2 == 1))
  ##   })
  ## }))
)

library(rms)

m1.stressvars <- c("A1SLFEDI", "A1SDAYDI",
  "A1SPIWOR", "A1SPIHOM", "A1SPIFAM",
  "A1SKINNE", "A1SFDSNE", "A1SSPCRI")

m1.newdata.predictionsB <- lapply(1:50, function(i) {
  tmpdat1 <- tmpdat2 <- dwcleanimp[[i]]

  for (v in m1.stressvars) {
    tmpdat1[[v]] <- quantile(tmpdat1[[v]], probs = .25, na.rm = TRUE)
    tmpdat2[[v]] <- quantile(tmpdat2[[v]], probs = .75, na.rm = TRUE)
  }

  tmpdat1$LastAge <- 70
  tmpdat2$LastAge <- 70

  yhat1 <- 1 - predict(
    mortality.coxph.results$m1f[[20]][[i]],
    newdata = tmpdat1,
    type = "lp")
  yhat2 <- 1 - predict(
    mortality.coxph.results$m1f[[20]][[i]],
    newdata = tmpdat2,
    type = "lp")

  data.table(Low = yhat1, High = yhat2, Diff = yhat2 - yhat1, Imp = i)
})

m1.newdata.predictionsB <- do.call(rbind, m1.newdata.predictionsB)
m1.newdata.predictionsB[!duplicated(Imp), .(mean(Low/High))]
m1.newdata.predictionsB[, .(Mean = mean(Diff), Mdn = median(Diff))]


m1.newdata.predictions <- lapply(1:50, function(i) {
  ddist <- datadist(dwcleanimp[[i]])$limits
  lastage <- seq(from = 60, to = 95, length.out = 250)

  tmpnew2 <- tmpnew <- ddist["Adjust to", ]
  tmpnew[, m1.stressvars] <- ddist["Low:effect", m1.stressvars]
  tmpnew2[, m1.stressvars] <- ddist["High:effect", m1.stressvars]
  tmpnew <- do.call(rbind, rep(list(tmpnew), 250))
  tmpnew2 <- do.call(rbind, rep(list(tmpnew2), 250))

  tmpnew$LastAge <- tmpnew2$LastAge <- lastage

  tmpnew$Yhat <- 1 - predict(
                            mortality.coxph.results$m1f[[20]][[i]],
                            newdata = tmpnew,
                            type = "expected")

  tmpnew2$Yhat <- 1 - predict(
                             mortality.coxph.results$m1f[[20]][[i]],
                             newdata = tmpnew2,
                             type = "expected")

  cbind(rbind(
    cbind(tmpnew[, c("LastAge", "Yhat")], Stress = "Low"),
    cbind(tmpnew2[, c("LastAge", "Yhat")], Stress = "High")),
    Imp = i)
})

m1.newdata.predictions.long <- as.data.table(do.call(rbind, m1.newdata.predictions))

agediff <- do.call(rbind, lapply(seq(from = .1, to = .9, by = .01), function(vi) {
  data.table(SurvivalPerc = vi,
             Years = m1.newdata.predictions.long[,
                                                 .(Age = LastAge[which.min(abs(Yhat - vi))],
                                                   Yhat = Yhat[which.min(abs(Yhat - vi))]),
                                                 by = .(Stress, Imp)][,
                                                                      .(Age = mean(Age),
                                                                        Yhat = mean(Yhat)),
                                                                      by = Stress][, diff(Age)])
}))

ggplot(agediff, aes(SurvivalPerc, Years)) +
  stat_smooth(se=FALSE, size = 1.5, span = .25, colour = "black") +
  scale_x_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(xlim = c(0, 1.02), ylim = c(-4.5, -2), expand = FALSE) +
  xlab("Percent Survival") +
  ylab("Years Less in High vs. Low Stress") +
  theme(legend.position = "none")


m1.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .5))], Yhat = Yhat[which.min(abs(Yhat - .5))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]

m1.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .75))], Yhat = Yhat[which.min(abs(Yhat - .75))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]

m1.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .25))], Yhat = Yhat[which.min(abs(Yhat - .25))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]

m1.newdata.predictions.long <- m1.newdata.predictions.long[, .(
  YhatLL = pmax(min(Yhat), 0),
  YhatUL = pmax(max(Yhat), 0),
  Yhat = pmax(mean(Yhat), 0)), by = .(LastAge, Stress)]

ggplot(m1.newdata.predictions.long, aes(LastAge, Yhat, linetype = Stress)) +
  ## geom_ribbon(aes(ymin = YhatLL, ymax = YhatUL, fill = Stress), alpha=.25) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(ylim = c(0, 1), xlim = c(60, 95), expand = FALSE) +
  xlab("Age at Death (years)") +
  ylab("Percent Survival") +
  theme(legend.position = c(.1, .1), legend.key.width = unit(2, "cm"))

ggplot(m1.newdata.predictions.long, aes(LastAge, Yhat, linetype = Stress)) +
  ## geom_ribbon(aes(ymin = YhatLL, ymax = YhatUL, fill = Stress), alpha=.25) +
  geom_line(size = 1) +
  geom_text(aes(x = 80, y = .55, label = "High Stress"), size = 5) +
  geom_text(aes(x = 80, y = .95, label = "Low Stress"), size = 5) +
  geom_text(aes(x = 60.5, y = .05, label = "High vs. Low Stress, p < .001"), hjust = 0, size = 5) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(ylim = c(0, 1), xlim = c(60, 95), expand = FALSE) +
  xlab("Age at Death (years)") +
  ylab("Percent Survival") +
  theme(legend.position = "none")

m2.full.stressvars <- c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
                        "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
                        "B4QPS_PS", "B1SKINNE", "B1SFDSNE", "B1SSPCRI",
                        "B1LifeStress")

m2.newdata.predictions <- lapply(1:50, function(i) {
  ddist <- datadist(subset(dwcleanimp[[i]], PAfterM2 > .9))$limits
  lastage <- seq(from = 60, to = 95, length.out = 250)

  tmpnew2 <- tmpnew <- ddist["Adjust to", ]
  tmpnew[, m2.full.stressvars] <- ddist["Low:effect", m2.full.stressvars]
  tmpnew2[, m2.full.stressvars] <- ddist["High:effect", m2.full.stressvars]

  tmpnew <- do.call(rbind, rep(list(tmpnew), 250))
  tmpnew2 <- do.call(rbind, rep(list(tmpnew2), 250))

  tmpnew2$LastAge <- tmpnew$LastAge <- lastage

  tmpnew$Yhat <- 1 - predict(
                       mortality.coxph.results$m2f.full[[24]][[i]],
                       newdata = tmpnew,
                       type = "expected")

  tmpnew2$Yhat <- 1 - predict(
                        mortality.coxph.results$m2f.full[[24]][[i]],
                        newdata = tmpnew2,
                        type = "expected")

  cbind(rbind(
    cbind(tmpnew[, c("LastAge", "Yhat")], Stress = "Low"),
    cbind(tmpnew2[, c("LastAge", "Yhat")], Stress = "High")),
    Imp = i)
})

m2.newdata.predictions.long <- as.data.table(do.call(rbind, m2.newdata.predictions))

m2.agediff <- do.call(rbind, lapply(seq(from = .1, to = .9, by = .01), function(vi) {
  data.table(SurvivalPerc = vi,
             Years = m2.newdata.predictions.long[,
                                                 .(Age = LastAge[which.min(abs(Yhat - vi))],
                                                   Yhat = Yhat[which.min(abs(Yhat - vi))]),
                                                 by = .(Stress, Imp)][,
                                                                      .(Age = mean(Age),
                                                                        Yhat = mean(Yhat)),
                                                                      by = Stress][, diff(Age)])
}))

ggplot(m2.agediff, aes(SurvivalPerc, Years)) +
  stat_smooth(se=FALSE, size = 1.5, span = .25, colour = "black") +
  scale_x_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(xlim = c(0, 1.02), ylim = c(-5.1, -.9), expand = FALSE) +
  xlab("Percent Survival") +
  ylab("Years Less in High vs. Low Stress") +
  theme(legend.position = "none")


m2.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .5))], Yhat = Yhat[which.min(abs(Yhat - .5))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]

m2.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .75))], Yhat = Yhat[which.min(abs(Yhat - .75))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]

m2.newdata.predictions.long[, .(Age = LastAge[which.min(abs(Yhat - .25))], Yhat = Yhat[which.min(abs(Yhat - .25))]), by = .(Stress, Imp)][,
  .(Age = mean(Age), Yhat = mean(Yhat)), by = Stress][, diff(Age)]


m2.newdata.predictions.long <- m2.newdata.predictions.long[, .(
  YhatLL = pmax(min(Yhat), 0),
  YhatUL = pmax(max(Yhat), 0),
  Yhat = pmax(mean(Yhat), 0)), by = .(LastAge, Stress)]

ggplot(m2.newdata.predictions.long, aes(LastAge, Yhat, linetype = Stress)) +
  #geom_ribbon(aes(ymin = YhatLL, ymax = YhatUL), alpha=.25) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(ylim = c(0, 1), xlim = c(60, 95), expand = FALSE) +
  xlab("Age at Death (years)") +
  ylab("Percent Survival") +
  theme(legend.position = c(.1, .1), legend.key.width = unit(2, "cm"))

ggplot(m2.newdata.predictions.long, aes(LastAge, Yhat, linetype = Stress)) +
  ## geom_ribbon(aes(ymin = YhatLL, ymax = YhatUL, fill = Stress), alpha=.25) +
  geom_line(size = 1) +
  geom_text(aes(x = 80, y = .65, label = "High Stress"), size = 5) +
  geom_text(aes(x = 80, y = .98, label = "Low Stress"), size = 5) +
  geom_text(aes(x = 60.5, y = .05, label = "High vs. Low Stress, p = .071"), hjust = 0, size = 5) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(ylim = c(0, 1), xlim = c(60, 95), expand = FALSE) +
  xlab("Age at Death (years)") +
  ylab("Percent Survival") +
  theme(legend.position = "none")


m1m2.newdata.predictions.long <- rbind(
  cbind(m1.newdata.predictions.long, Time = "MIDUS 1 (p = .001)"),
  cbind(m2.newdata.predictions.long, Time = "MIDUS 2 (p = .071)"))

p.predicted <- ggplot(m1m2.newdata.predictions.long, aes(LastAge, Yhat, linetype = Stress)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() +
  coord_cartesian(ylim = c(0, 1), xlim = c(60, 95), expand = FALSE) +
  xlab("Age (years)") +
  ylab("Percent Survival") +
  facet_wrap(~Time) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

pdf(file = "m1m2_highlowstress_predictedsurvival.pdf", width = 8, height = 4.5)
print(p.predicted)
dev.off()

###### MIDUS 1 Results ######
cbind(
  IV = 2:20,
  C = sapply(2:20, function(k) pool.cindex(mortality.coxph.results$m1f[[k]])))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare.null(as.mira(m1f[[5]])),
       iv3 = pool.compare.null(as.mira(m1f[[9]])),
       iv4 = pool.compare.null(as.mira(m1f[[13]])),
       iv5 = pool.compare.null(as.mira(m1f[[17]]))))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m1f[[6]]),  as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m1f[[10]]), as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m1f[[14]]), as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m1f[[18]]), as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m1f[[7]]),  as.mira(m1f[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m1f[[11]]), as.mira(m1f[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m1f[[15]]), as.mira(m1f[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m1f[[19]]), as.mira(m1f[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m1f[[8]]),  as.mira(m1f[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m1f[[12]]), as.mira(m1f[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m1f[[16]]), as.mira(m1f[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m1f[[20]]), as.mira(m1f[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

mortality.coxph.results.m1f.m1coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m1f[[5]]),
       format.mortality(m1f[[9]]),
       format.mortality(m1f[[13]]),
       format.mortality(m1f[[17]])))

mortality.coxph.results.m1f.m2coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m1f[[6]]),
       format.mortality(m1f[[10]]),
       format.mortality(m1f[[14]]),
       format.mortality(m1f[[18]])))

mortality.coxph.results.m1f.m3coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m1f[[7]]),
       format.mortality(m1f[[11]]),
       format.mortality(m1f[[15]]),
       format.mortality(m1f[[19]])))

mortality.coxph.results.m1f.m4coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m1f[[8]]),
       format.mortality(m1f[[12]]),
       format.mortality(m1f[[16]]),
       format.mortality(m1f[[20]])))

write.xlsx(cbind(
  mortality.coxph.results.m1f.m1coef,
  Est2 = mortality.coxph.results.m1f.m2coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m1f.m3coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m1f.m4coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est]),
  file = "c:/Users/Joshua/OneDrive/Projects/StressNetwork/aim1_stress_mortality_v2.xlsx",
  sheetName = "M1",
  row.names = FALSE, showNA=FALSE)

###### MIDUS 2 Reduced Results ######

with(mortality.coxph.results,
     list(
       iv2 = pool.compare.null(as.mira(m2f.reduced[[5]])),
       iv3 = pool.compare.null(as.mira(m2f.reduced[[9]])),
       iv4 = pool.compare.null(as.mira(m2f.reduced[[13]])),
       iv5 = pool.compare.null(as.mira(m2f.reduced[[17]]))))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.reduced[[6]]),  as.mira(m2f.reduced[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.reduced[[10]]), as.mira(m2f.reduced[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.reduced[[14]]), as.mira(m2f.reduced[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.reduced[[18]]), as.mira(m2f.reduced[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.reduced[[7]]),  as.mira(m2f.reduced[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.reduced[[11]]), as.mira(m2f.reduced[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.reduced[[15]]), as.mira(m2f.reduced[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.reduced[[19]]), as.mira(m2f.reduced[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.reduced[[8]]),  as.mira(m2f.reduced[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.reduced[[12]]), as.mira(m2f.reduced[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.reduced[[16]]), as.mira(m2f.reduced[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.reduced[[20]]), as.mira(m2f.reduced[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

mortality.coxph.results.m2f.reduced.m1coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.reduced[[5]]),
       format.mortality(m2f.reduced[[9]]),
       format.mortality(m2f.reduced[[13]]),
       format.mortality(m2f.reduced[[17]])))

mortality.coxph.results.m2f.reduced.m2coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.reduced[[6]]),
       format.mortality(m2f.reduced[[10]]),
       format.mortality(m2f.reduced[[14]]),
       format.mortality(m2f.reduced[[18]])))

mortality.coxph.results.m2f.reduced.m3coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.reduced[[7]]),
       format.mortality(m2f.reduced[[11]]),
       format.mortality(m2f.reduced[[15]]),
       format.mortality(m2f.reduced[[19]])))

mortality.coxph.results.m2f.reduced.m4coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.reduced[[8]]),
       format.mortality(m2f.reduced[[12]]),
       format.mortality(m2f.reduced[[16]]),
       format.mortality(m2f.reduced[[20]])))

write.xlsx(cbind(
  mortality.coxph.results.m2f.reduced.m1coef,
  Est2 = mortality.coxph.results.m2f.reduced.m2coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m2f.reduced.m3coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m2f.reduced.m4coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est]),
  file = "c:/Users/Joshua/OneDrive/Projects/StressNetwork/aim1_stress_mortality_v2.xlsx",
  sheetName = "M2 Reduced",
  row.names = FALSE, showNA=FALSE, append = TRUE)





###### MIDUS 2 Full Results ######

cbind(
  IV = 2:24,
  C = sapply(2:24, function(k) pool.cindex(mortality.coxph.results$m2f.full[[k]])))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare.null(as.mira(m2f.full[[5]])),
       iv3 = pool.compare.null(as.mira(m2f.full[[9]])),
       iv4 = pool.compare.null(as.mira(m2f.full[[13]])),
       iv5 = pool.compare.null(as.mira(m2f.full[[17]])),
       iv6 = pool.compare.null(as.mira(m2f.full[[21]]))))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[6]]),  as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[10]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[14]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[18]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv6 = pool.compare(as.mira(m2f.full[[22]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[7]]),  as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[11]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[15]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[19]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv6 = pool.compare(as.mira(m2f.full[[23]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[8]]),  as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[12]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[16]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[20]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv6 = pool.compare(as.mira(m2f.full[[24]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

mortality.coxph.results.m2f.full.m1coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[5]]),
       format.mortality(m2f.full[[9]]),
       format.mortality(m2f.full[[13]]),
       format.mortality(m2f.full[[17]]),
       format.mortality(m2f.full[[21]])))

mortality.coxph.results.m2f.full.m2coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[6]]),
       format.mortality(m2f.full[[10]]),
       format.mortality(m2f.full[[14]]),
       format.mortality(m2f.full[[18]]),
       format.mortality(m2f.full[[22]])))

mortality.coxph.results.m2f.full.m3coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[7]]),
       format.mortality(m2f.full[[11]]),
       format.mortality(m2f.full[[15]]),
       format.mortality(m2f.full[[19]]),
       format.mortality(m2f.full[[23]])))

mortality.coxph.results.m2f.full.m4coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[8]]),
       format.mortality(m2f.full[[12]]),
       format.mortality(m2f.full[[16]]),
       format.mortality(m2f.full[[20]]),
       format.mortality(m2f.full[[24]])))

write.xlsx(cbind(
  mortality.coxph.results.m2f.full.m1coef,
  Est2 = mortality.coxph.results.m2f.full.m2coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m2f.full.m3coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m2f.full.m4coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est]),
  file = "c:/Users/Joshua/OneDrive/Projects/StressNetwork/aim1_stress_mortality_v2.xlsx",
  sheetName = "M2 Full",
  row.names = FALSE, showNA=FALSE, append = TRUE)



################################################################################
##                                                                            ##
##                        Mortality RF Survival Model                         ##
##                                                                            ##
################################################################################

###### Unadjusted Model
cl <- makeCluster(10)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-01-07", R.version = "3.3.2")
  library(data.table)
  library(randomForest)
  library(randomForestSRC)
  library(ggRandomForests)
  library(pec)
})
clusterExport(cl, c("indices", "dwcleanimp"))

rfs.res1 <- parLapplyLB(cl, 1:50, function(i) {
  set.seed(12345)
  rfsrc(update(as.formula(indices$m1f[16]), . ~ . - cluster(M2FAMNUM)),
        data = dwcleanimp[[i]],
        ntree = 1000, nsplit = 10,
        ntime = 50, nodesize = 5,
        seed = -12345L,
        importance = "permute", forest = TRUE)
})
saveRDS(rfs.res1, file = "~/OneDrive/Projects/StressNetwork/aim1/rfs_res1.RDS")
stopCluster(cl)
rm(rfs.res1); gc()

rfs.res1 <- readRDS("~/OneDrive/Projects/StressNetwork/aim1/rfs_res1.RDS")

tanh(summary(atanh(1 - sapply(rfs.res1, function(x) x$err.rate[1000]))))
summary(1 - sapply(rfs.res1, function(x) x$err.rate[1000]))


rfs.res1.mimp <- pmax(sort(rowMeans(sapply(rfs.res1, function(x) x$importance/max(x$importance)))), 0)
rfs.res1.mimp <- data.table(V = names(rfs.res1.mimp), "MIDUS 1 - Model 1" = as.vector(rfs.res1.mimp))

## print(vimp(rfs.res1[[1]], c("A1SLFEDI", "A1SDAYDI",
##                             "A1SPIWOR", "A1SPIHOM", "A1SPIFAM",
##                             "A1SKINNE", "A1SFDSNE", "A1SSPCRI"),
##   joint = TRUE, importance = "permute.ensemble")$importance)

###### Adjusted Model
cl <- makeCluster(10)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-01-07", R.version = "3.3.2")
  library(data.table)
  library(randomForest)
  library(randomForestSRC)
  library(ggRandomForests)
  library(pec)
})
clusterExport(cl, c("indices", "dwcleanimp"))

rfs.res4 <- parLapplyLB(cl, 1:50, function(i) {
  set.seed(12345)
  rfsrc(update(as.formula(indices$m1f[19]), . ~ . - cluster(M2FAMNUM)),
        data = dwcleanimp[[i]],
        ntree = 1000, nsplit = 10,
        ntime = 50, nodesize = 5,
        seed = -12345L,
        importance = "permute", forest = TRUE)
})
saveRDS(rfs.res4, file = "~/OneDrive/Projects/StressNetwork/aim1/rfs_res4.RDS")
stopCluster(cl)
rm(rfs.res4); gc()

rfs.res4 <- readRDS("~/OneDrive/Projects/StressNetwork/aim1/rfs_res4.RDS")

tanh(summary(atanh(1 - sapply(rfs.res4, function(x) x$err.rate[1000]))))
summary(1 - sapply(rfs.res4, function(x) x$err.rate[1000]))


rfs.res4.mimp <- pmax(sort(rowMeans(sapply(rfs.res4, function(x) x$importance/max(x$importance)))), 0)
rfs.res4.mimp <- data.table(V = names(rfs.res4.mimp), "MIDUS 1 - Model 4" = as.vector(rfs.res4.mimp))

rfs.resall.mimp <- melt(merge(rfs.res1.mimp, rfs.res4.mimp, by = "V", all = TRUE),
     id.vars = "V", variable.name = "Model")

rfs.resall.mimp <- merge(rfs.resall.mimp, rbind(stresskey2[, c("Var", "Label2")],
                             data.frame(Var = c("A1PB1", "A1PBWORK", "A1PhysAct", "A1SWSTHI", "A1Smoke",
                                                "A1WorstAlcohol", "RaceG3", "m1ped3_all", "m1welf_all"),
                                        Label2 = c("Education", "Employed", "Physical Activity", "Waist-to-Hip Ratio", "Smoking Status",
                                                   "Worst Alcohol", "Race", "Parent Education", "On Welfare in Childhood"))),
      by.x = "V", by.y = "Var", all.x = TRUE)

rfs.resall.mimp[, Label2 := gsub("\\s*$", "\\2", Label2)]

rfs.resall.mimp[, V := factor(V, levels = rfs.resall.mimp[Model=="MIDUS 1 - Model 4", max(value, na.rm = TRUE), by = V][order(V1), V])]
rfs.resall.mimp[, Label2 := factor(Label2, levels = rfs.resall.mimp[Model=="MIDUS 1 - Model 4", max(value, na.rm = TRUE), by = Label2][order(V1), Label2])]


## rfs.resall.pimp <- ggplot(rfs.resall.mimp, aes(Label2, value)) +
##   geom_bar(aes(Label2, V1), data = rfs.resall.mimp[, max(value, na.rm = TRUE), by = Label2], stat = "identity", width = .1, fill = "grey60") +
##   geom_point(aes(shape = Model), size = 2) +
##   coord_flip() +
##   scale_y_continuous(labels = percent) +
##   theme_cowplot() +
##   xlab("") + ylab("Relative Variable Importances") +
##   theme(legend.position = c(.9, .1))

rfs.resall.pimp <- ggplot(rfs.resall.mimp, aes(Label2, value)) +
  geom_bar(, stat = "identity", width = .1, fill = "grey60") +
  geom_point(, size = 2) + facet_wrap(~Model) +
  coord_flip(ylim = c(-.02, 1.02), xlim = c(.7, 18.3), expand=FALSE) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() + theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = .5)) +
  xlab("") + ylab("Relative Variable Importances")

## jointimp.rfs.res4 <- lapply(rfs.res4, function(x) vimp(x, c("A1SLFEDI", "A1SDAYDI",
##                             "A1SPIWOR", "A1SPIHOM", "A1SPIFAM",
##                             "A1SKINNE", "A1SFDSNE", "A1SSPCRI"),
##   joint = TRUE, importance = "permute.ensemble"))
## print(vimp(rfs.res4[[1]], c("A1SPIHOM", "A1SFDSNE", "A1SLFEDI"),
##            joint = TRUE, importance = "permute.ensemble")$importance)


###### Unadjusted Model M2
cl <- makeCluster(10)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-01-07", R.version = "3.3.2")
  library(data.table)
  library(randomForest)
  library(randomForestSRC)
  library(ggRandomForests)
  library(pec)
})
clusterExport(cl, c("indices2", "dwcleanimp"))

rfs.m2.res1 <- parLapplyLB(cl, 1:50, function(i) {
  set.seed(12345)
  rfsrc(update(as.formula(indices2$m2f.full[20]), . ~ . - cluster(M2FAMNUM)),
        data = subset(dwcleanimp[[i]], PAfterM2 == 1),
        ntree = 1000, nsplit = 10,
        ntime = 50, nodesize = 5,
        seed = -12345L,
        importance = "permute", forest = TRUE)
})
saveRDS(rfs.m2.res1, file = "~/OneDrive/Projects/StressNetwork/aim1/rfs_m2_res1.RDS")
stopCluster(cl)
rm(rfs.m2.res1); gc()

rfs.m2.res1 <- readRDS("~/OneDrive/Projects/StressNetwork/aim1/rfs_m2_res1.RDS")


tanh(summary(atanh(1 - sapply(rfs.m2.res1, function(x) x$err.rate[1000]))))


rfs.m2.res1.mimp <- pmax(sort(rowMeans(sapply(rfs.m2.res1, function(x) x$importance/max(x$importance)))), 0)
rfs.m2.res1.mimp <- data.table(V = names(rfs.m2.res1.mimp), "MIDUS 2 - Model 1" = as.vector(rfs.m2.res1.mimp))


###### Adjusted Model M2
cl <- makeCluster(10)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-01-07", R.version = "3.3.2")
  library(data.table)
  library(randomForest)
  library(randomForestSRC)
  library(ggRandomForests)
  library(pec)
})
clusterExport(cl, c("indices2", "dwcleanimp"))

rfs.m2.res4 <- parLapplyLB(cl, 1:50, function(i) {
  set.seed(12345)
  rfsrc(update(as.formula(indices2$m2f.full[23]), . ~ . - cluster(M2FAMNUM)),
        data = subset(dwcleanimp[[i]], PAfterM2 == 1),
        ntree = 1000, nsplit = 10,
        ntime = 50, nodesize = 5,
        seed = -12345L,
        importance = "permute", forest = TRUE)
})
## saveRDS(rfs.m2.res4, file = "~/OneDrive/Projects/StressNetwork/aim1/rfs_m2_res4.RDS")
stopCluster(cl)
rm(rfs.m2.res4); gc()

rfs.m2.res4 <- readRDS("~/OneDrive/Projects/StressNetwork/aim1/rfs_m2_res4.RDS")

summary(1 - sapply(rfs.m2.res4, function(x) x$err.rate[1000]))
tanh(summary(atanh(1 - sapply(rfs.m2.res4, function(x) x$err.rate[1000]))))

rfs.m2.res4.mimp <- pmax(sort(rowMeans(sapply(rfs.m2.res4, function(x) x$importance/max(x$importance)))), 0)
rfs.m2.res4.mimp <- data.table(V = names(rfs.m2.res4.mimp), "MIDUS 2 - Model 4" = as.vector(rfs.m2.res4.mimp))

rfs.m2.resall.mimp <- melt(merge(rfs.m2.res1.mimp, rfs.m2.res4.mimp, by = "V", all = TRUE),
     id.vars = "V", variable.name = "Model")

rfs.m2.resall.mimp <- merge(rfs.m2.resall.mimp, rbind(stresskey2[, c("Var", "Label2")],
                             data.frame(Var = c("B1PB1", "B1PBWORK", "B1PhysAct", "B1SWSTHI", "B1Smoke",
                                                "B1WorstAlcohol", "RaceG3", "m1ped3_all", "m1welf_all"),
                                        Label2 = c("Education", "Employed", "Physical Activity", "Waist-to-Hip Ratio", "Smoking Status",
                                                   "Worst Alcohol", "Race", "Parent Education", "On Welfare in Childhood"))),
      by.x = "V", by.y = "Var", all.x = TRUE)

rfs.m2.resall.mimp[, Label2 := gsub("\\s*$", "", Label2)]

rfs.m2.resall.mimp[, V := factor(V, levels = rfs.m2.resall.mimp[Model=="MIDUS 2 - Model 4", max(value, na.rm = TRUE), by = V][order(V1), V])]
rfs.m2.resall.mimp[, Label2 := factor(Label2, levels = rfs.m2.resall.mimp[Model=="MIDUS 2 - Model 4", max(value, na.rm = TRUE), by = Label2][order(V1), Label2])]


## rfs.m1m2.resall.mimp <- rbind(cbind(MIDUS = "1", rfs.resall.mimp),
##                               cbind(MIDUS = "2", rfs.m2.resall.mimp))
## rfs.m1m2.resall.mimp[, V := factor(V, levels = rfs.m1m2.resall.mimp[, max(value, na.rm = TRUE), by = V][order(V1), V])]
## rfs.m1m2.resall.mimp[, Label2 := factor(Label2, levels = rfs.m1m2.resall.mimp[, max(value, na.rm = TRUE), by = Label2][order(V1), Label2])]
## ggplot(rfs.m1m2.resall.mimp, aes(Label2, value)) +
##   geom_bar(stat = "identity", width = .1, fill = "grey60") +
##   geom_point(size = 2) + facet_grid(MIDUS~Model) +
##   coord_flip() +
##   scale_y_continuous(labels = percent) +
##   theme_cowplot() +
##   xlab("") + ylab("Relative Variable Importances") +
##   theme(legend.position = c(.9, .1))


## rfs.m2.resall.pimp <- ggplot(rfs.m2.resall.mimp, aes(Label2, value)) +
##   geom_bar(aes(Label2, V1), data = rfs.m2.resall.mimp[, max(value, na.rm = TRUE), by = Label2], stat = "identity", width = .1, fill = "grey60") +
##   geom_point(aes(shape = Model), size = 2) +
##   coord_flip() +
##   scale_y_continuous(labels = percent) +
##   theme_cowplot() +
##   xlab("") + ylab("Relative Variable Importances") +
##   theme(legend.position = c(.9, .1))


rfs.m2.resall.pimp <- ggplot(rfs.m2.resall.mimp, aes(Label2, value)) +
  geom_bar(stat = "identity", width = .1, fill = "grey60") +
  geom_point(size = 2) + facet_wrap(~Model) +
  coord_flip(ylim = c(-.02, 1.02), xlim = c(.7, 21.3), expand=FALSE) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() + theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = .5)) +
  xlab("") + ylab("Relative Variable Importances")


save_plot(filename = "rfs_m1m2_variable_importances.pdf",
          plot_grid(rfs.resall.pimp, rfs.m2.resall.pimp, ncol = 1, align = "hv"),
          nrow = 2,
          base_height = 4, base_aspect_ratio = 2)
