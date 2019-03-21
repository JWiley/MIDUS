source("../MIDUS-Setup/setup_packages_functions.R")
options(stringsAsFactors = FALSE, digits = 3)

mVars <- as.data.table(read.xlsx("../MIDUS-Setup/midus_master_vars.xlsx", sheetIndex = 1))
## setkey(mVars, "Domain")

mVars[Domain == "Stress", .(Name, Abbr, M1)][!is.na(M1)]
mVars[Domain == "Stress", .(Name, Abbr, M2)][!is.na(M2)]

## not done, confusing
## dw[, A1Work := recode(A1PBWORK, "c(1, 2) = 'employed'; c(3, 4) = 'unemployed'; c(5) = 'retired'; c(6) = 'homemaker'")]

## Nice Names
stresskey <- "
Var	Label
A1PAGE_M2	Age (M1)
B1PAGE_M2	Age (M2)
AGEM1	Age (M1)
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
    "BirthYear", "AGEM1", "B1PAGE_M2",
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
  ANALYSIS = "
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

m1.cfa2 <- mplusModeler(mplusObject(
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ANALYSIS = "
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m2_stresscfa1.dat", run = 1L)
summary(m2.cfa1)
coef(m2.cfa1, type = "stdyx", param = "loading")

m2.cfa2 <- mplusModeler(mplusObject(
  ANALYSIS = "
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m2_stresscfa2.dat", run = 1L)
summary(m2.cfa2)
coef(m2.cfa2, type = "stdyx", param = "loading")


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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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
  ",
  ANALYSIS = "
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


## LCA

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
 c("A1Smoke", "A1WorstAlcohol", "A1PhysAct", "A1SBMI"))

use.cov.m2stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all"),
 c("B1PB1", "B1PBWORK"),
 c("B1Smoke", "B1WorstAlcohol", "B1PhysAct", "B1SBMI"))


m1.stress.vars <- list("",
  c("A1SLFEDI", "A1SDAYDI"),
  c("A1SPIWOR", "A1SPIHOM", "A1SPIFAM"),
  c("A1SKINNE", "A1SFDSNE", "A1SSPCRI"))

m2.stress.vars.full <- list("",
  c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI"),
  c("B1SPIWOR", "B1SPIHOM", "B1SPIFAM"),
  c("B4QPS_PS", "B1SKINNE", "B1SFDSNE", "B1SSPCRI"))

m2.stress.vars.reduced <- list("",
  c("B1SLFEDI", "B1SDAYDI"),
  c("B1SPIWOR", "B1SPIHOM", "B1SPIFAM"),
  c("B1SKINNE", "B1SFDSNE", "B1SSPCRI"))

m1.stress.vars <- lapply(m1.stress.vars, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
m2.stress.vars.full <- lapply(m2.stress.vars.full, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
m2.stress.vars.reduced <- lapply(m2.stress.vars.reduced, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")


dwcleanimp <- lapply(dwcleanimp, function(d) {
  d <- within(d, {
    DECEASED <- as.integer(DECEASED == "1")
    A1PB1 <- as.integer(as.character(A1PB1))
    B1PB1 <- as.integer(as.character(B1PB1))
    A1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
    B1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
  })
  within(d, {
    A1PB1 <- recode(A1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
    B1PB1 <- recode(B1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
  })
})

## for random forest
## indices <- subset(expand.grid(cov = 1:4, iv = 1:5), iv > 1 | cov > 1)
indices <- expand.grid(cov = 1:4, iv = 1:5)

indices$m1f <- sapply(1:nrow(indices), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ 1%s%s",
          paste(unlist(use.cov.m1stress[1:indices$cov[n]]), collapse = " + "),
          paste0(ifelse(indices$iv[n]>1, " + ", ""),
                 paste(if(indices$iv[n] <= length(m1.stress.vars)) m1.stress.vars[[indices$iv[n]]] else unlist(m1.stress.vars[-1]),
                collapse = " + ")))
})

indices$m2f.full <- sapply(1:nrow(indices), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ 1%s%s",
          paste(unlist(use.cov.m2stress[1:indices$cov[n]]), collapse = " + "),
          paste0(ifelse(indices$iv[n]>1, " + ", ""),
                 paste(if(indices$iv[n] <= length(m2.stress.vars.full)) m2.stress.vars.full[[indices$iv[n]]] else unlist(m2.stress.vars.full[-1]),
                collapse = " + ")))
})

indices$m2f.reduced <- sapply(1:nrow(indices), function(n) {
  sprintf("Surv(LastAge, DECEASED) ~ 1%s%s",
          paste(unlist(use.cov.m2stress[1:indices$cov[n]]), collapse = " + "),
          paste0(ifelse(indices$iv[n]>1, " + ", ""),
          paste(if(indices$iv[n] <= length(m2.stress.vars.reduced)) m2.stress.vars.reduced[[indices$iv[n]]] else unlist(m2.stress.vars.reduced[-1]),
                collapse = " + ")))
})

mortality.coxph.results <- list(
  m1f = lapply(1:20, function(n) {
    lapply(1:50, function(i) {
      coxph(as.formula(indices$m1f[n]), data = dwcleanimp[[i]])
    })
  }),
  m2f.full = lapply(1:20, function(n) {
    lapply(1:50, function(i) {
      coxph(as.formula(indices$m2f.full[n]), data = dwcleanimp[[i]])
    })
  }),
  m2f.reduced = lapply(1:20, function(n) {
    lapply(1:50, function(i) {
      coxph(as.formula(indices$m2f.reduced[n]), data = dwcleanimp[[i]])
    })
  }))

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

###### MIDUS 1 Results ######

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

write.table(cbind(
  mortality.coxph.results.m1f.m1coef,
  Est2 = mortality.coxph.results.m1f.m2coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m1f.m3coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m1f.m4coef[IV %in% mortality.coxph.results.m1f.m1coef$IV, Est]),
  file = "clipboard", row.names = FALSE, sep = "\t")



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

write.table(cbind(
  mortality.coxph.results.m2f.reduced.m1coef,
  Est2 = mortality.coxph.results.m2f.reduced.m2coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m2f.reduced.m3coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m2f.reduced.m4coef[IV %in% mortality.coxph.results.m2f.reduced.m1coef$IV, Est]),
  file = "clipboard", row.names = FALSE, sep = "\t")





###### MIDUS 2 Full Results ######

with(mortality.coxph.results,
     list(
       iv2 = pool.compare.null(as.mira(m2f.full[[5]])),
       iv3 = pool.compare.null(as.mira(m2f.full[[9]])),
       iv4 = pool.compare.null(as.mira(m2f.full[[13]])),
       iv5 = pool.compare.null(as.mira(m2f.full[[17]]))))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[6]]),  as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[10]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[14]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[18]]), as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[7]]),  as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[11]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[15]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[19]]), as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

with(mortality.coxph.results,
     list(
       iv2 = pool.compare(as.mira(m2f.full[[8]]),  as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv3 = pool.compare(as.mira(m2f.full[[12]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv4 = pool.compare(as.mira(m2f.full[[16]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")],
       iv5 = pool.compare(as.mira(m2f.full[[20]]), as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]))

mortality.coxph.results.m2f.full.m1coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[5]]),
       format.mortality(m2f.full[[9]]),
       format.mortality(m2f.full[[13]]),
       format.mortality(m2f.full[[17]])))

mortality.coxph.results.m2f.full.m2coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[6]]),
       format.mortality(m2f.full[[10]]),
       format.mortality(m2f.full[[14]]),
       format.mortality(m2f.full[[18]])))

mortality.coxph.results.m2f.full.m3coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[7]]),
       format.mortality(m2f.full[[11]]),
       format.mortality(m2f.full[[15]]),
       format.mortality(m2f.full[[19]])))

mortality.coxph.results.m2f.full.m4coef <- with(mortality.coxph.results,
     rbind(
       format.mortality(m2f.full[[8]]),
       format.mortality(m2f.full[[12]]),
       format.mortality(m2f.full[[16]]),
       format.mortality(m2f.full[[20]])))

write.table(cbind(
  mortality.coxph.results.m2f.full.m1coef,
  Est2 = mortality.coxph.results.m2f.full.m2coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est],
  Est3 = mortality.coxph.results.m2f.full.m3coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est],
  Est4 = mortality.coxph.results.m2f.full.m4coef[IV %in% mortality.coxph.results.m2f.full.m1coef$IV, Est]),
  file = "clipboard", row.names = FALSE, sep = "\t")













































saveRDS(list(indices = indices, impdata = dwcleanimp), file = "~/Desktop/aim1_stress.RDS")

system.time(test <- lapply(1:50, function(i) {
  coxph(as.formula(indices$m1f[19]), data = dwcleanimp[[i]])
}))

system.time(test2 <- lapply(1:50, function(i) {
  coxph(as.formula(indices$m1f[3]), data = dwcleanimp[[i]])
}))

pool.compare(
  as.mira(test),
  as.mira(test2))

summary(mice::pool(as.mira(test)))

summary(test[[1]])

rfsrc.obj <- rfsrc(as.formula(indices$m1f[1]), data = within(dwcleanimp[[1]], {DECEASED <- as.integer(DECEASED == "1")}), forest = TRUE)


f.test <- Surv(LastAge, DECEASED == "1") ~ RaceG3
rfsrc.obj <- rfsrc(f.test, data = dwcleanimp[[1]])
rfsrc.obj$err.rate[rfsrc.obj$ntree]

summary(coxph(f.test, data = dwcleanimp[[1]]))

summary(coxph(Surv(LastAge, DECEASED == "1") ~ 1, data = dwcleanimp[[1]]))


alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI,
                                               A1SPIWOR, A1SPIHOM, A1SPIFAM)]))

alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI)]))

alpha(scale(dwclean.use[, .(A1SPIWOR, A1SPIHOM, A1SPIFAM)]))


alpha(scale(dwclean.use[, .(A1SKINNE, A1SFDSNE, A1SSPCRI)]))

alpha(scale(dwclean.use[, .(B1SLFEDI, B1SDAYDI, B1SJOBDI,
                      B1SPIWOR, B1SPIHOM, B1SPIFAM)]))
alpha(scale(dwclean.use[, .(B4QPS_PS, B1SKINNE, B1SFDSNE, B1SSPCRI)]))




        ## compute out-of-bag C-index for cox regression and compare to rfsrc
       rfsrc.obj <- rfsrc(surv.f, pbc.na, nsplit = 10)
       cat("out-of-bag Cox Analysis ...", "\n")
       cox.err <- sapply(1:100, function(b) {
         if (b%%10 == 0) cat("cox bootstrap:", b, "\n")
         train <- sample(1:nrow(pbc.na), nrow(pbc.na), replace = TRUE)
         cox.obj <- tryCatch({coxph(surv.f, pbc.na[train, ])}, error=function(ex){NULL})
         if (is.list(cox.obj)) {
           rcorr.cens(predict(cox.obj, pbc.na[-train, ]),
                      Surv(pbc.na$days[-train],
                      pbc.na$status[-train]))[1]
         } else NA
       })
       cat("\n\tOOB error rates\n\n")
       cat("\tRSF            : ", rfsrc.obj$err.rate[rfsrc.obj$ntree], "\n")
       cat("\tCox regression : ", mean(cox.err, na.rm = TRUE), "\n")















corplot(r.pearson, plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Pearson")

corplot(r.spearman, plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Spearman")

corplot(r.pearson - r.spearman, plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


corplot(r.pearson[1:11, 1:11], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

corplot(r.spearman[-(1:11), -(1:11)], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

fa.parallel(r.spearman, n.obs = 1250)
fa.parallel(r.pearson, n.obs = 1250)

fa.parallel(r.spearman[1:11, 1:11], n.obs = 1250)
fa.parallel(r.pearson[1:11, 1:11], n.obs = 1250)

fa.parallel(r.spearman[-(1:11), -(1:11)], n.obs = 1250)
fa.parallel(r.pearson[-(1:11), -(1:11)], n.obs = 1250)







pdf("~/Desktop/dist.pdf", width = 30, height = 20)
ggplot(melt(as.data.frame(scale(winsorizor(dw.use[, -c("M2FAMNUM", "DECEASED", v$cov.cat, v$health.cat), with = FALSE], .00))), id.vars = "M2ID"),
       aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  theme_cowplot() +
  facet_wrap(~variable, scales = "free_y") + ggtitle("none")

ggplot(melt(as.data.frame(scale(winsorizor(dw.use[, -c("M2FAMNUM", "DECEASED", v$cov.cat, v$health.cat), with = FALSE], .005))), id.vars = "M2ID"),
       aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  theme_cowplot() +
  facet_wrap(~variable, scales = "free_y") + ggtitle(".005")

ggplot(melt(as.data.frame(scale(winsorizor(dw.use[, -c("M2FAMNUM", "DECEASED", v$cov.cat, v$health.cat), with = FALSE], .01))), id.vars = "M2ID"),
       aes(value)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  theme_cowplot() +
  facet_wrap(~variable, scales = "free_y") + ggtitle(".001")

dev.off()

v$health <- c(v$health, "B4F")
"B1NegAff", "B1PosAff", "B1PR", "B1SR",
"B4bi.sym", "B4bi.hpa", "B4bi.card", "B4bi.gluc", "B4bi.lipid", "B4bi.infl", "B4bi.para",

dwimp.use <- lapply(dwimp, function(d) {
  out <- cbind(
    d[, c(v$noimp, v$cov.cat, v$health.cat), with = FALSE],
    winsorizor(d[, unlist(v), with = FALSE][, -c(v$noimp, v$cov.cat, v$health.cat), with = FALSE], .005))[
    M2ID %in% useIDs]
  out[, c(v$cov.cat, v$health.cat, "DECEASED") := lapply(.SD, as.factor), .SDcols = c(v$cov.cat, v$health.cat, "DECEASED")]
  return(out)
})

rm(dwimp)
rm(dwlimp)
rm(M2P3Cort)
rm(M2P3Cortl)
gc()

## set number of cores to use for random forests
##options(rf.cores = 40)
##preloaded <- TRUE


## summary(m.cox <- coxph(Surv(LastAge, DECEASED) ~ MFAbuse, data = d.all))
## m <- rfsrc(Surv(LastAge, DECEASED) ~ MFAbuse, data = d.all)
## set.seed(1234)

## prederr <- pec(list(m.cox, m), data = na.omit(d.all[, c("LastAge", "DECEASED", "MFAbuse")]),
##                formula = Surv(LastAge, DECEASED) ~ 1, splitMethod = "bootcv", B = 10)

## summary(coxph(Surv(LastAge, DECEASED) ~ b4bil6, data = d.all))
## summary(coxph(Surv(LastAge, DECEASED) ~ dNeiQu, data = d.all))

## summary(coxph(Surv(LastAge, DECEASED) ~ winsorizor(LifeStress, .005), data = d.all))
## summary(coxph(Surv(LastAge, DECEASED) ~ winsorizor(LifeStress, .005) + winsorizor(LifeStressImpactS, .005), data = d.all))
## summary(coxph(Surv(LastAge, DECEASED) ~ winsorizor(LifeStressImpactS, .005) +
##             I(winsorizor(LifeStressImpactS, .005)^2), data = d.all))

## summary(coxph(Surv(LastAge, DECEASED) ~ winsorizor(LifeStress, .005) +
##                 winsorizor(LifeStressImpactS, .005) +
##             I(winsorizor(LifeStressImpactS, .005)^2), data = d.all))


## summary(gam(LastAge ~ b4bil6, data = d.all, family = cox.ph(link = "identity"), weights = DECEASED))
## summary(gam(LastAge ~ s(b4bil6, k = 6), data = d.all, family = cox.ph(link = "identity"), weights = DECEASED))
## plot(gam(LastAge ~ s(b4bil6, k = 6), data = d.all, family = cox.ph(link = "identity"), weights = DECEASED))
## plot(gam(LastAge ~ s(b3tem, k = 20), data = d.all, family = cox.ph(link = "identity"), weights = DECEASED))
## summary(gam(LastAge ~ Sex + s(b4bil6, k = 6, by = Sex), data = within(d.all, {Sex <- factor(Sex)}), family = cox.ph(link = "identity"), weights = DECEASED))


##     "A1SS13A", "A1SS13B", "A1SS13C", "A1SS13D", "A1SS13E", "A1SS13F",
##     "A1SS13G", "A1SS13H", "A1SS13I", "A1SS13J", "A1SS13K", "A1SS14A",
##     "A1SS14B", "A1SS14C", "A1SS14D", "A1SS14E", "A1SS14F", "A1SS14G",
## "A1SS14H", "A1SS14I",    "dSRMH", "dSRPH",
##     "b4bfgn", "b4bsicam", "b4bsesel",
##     "pulpress", "Rb4p1gs", "avgb_rm", "avgb_lf", "avgb_hf",
##     "b4bha1c", "Rb4bgluc", "p4homair", "C1SCHRON",
## "B4QCT_EA", "B4QCT_PA", "B4QCT_SA", "B4QCT_EN", "B4QCT_PN", "B4QCT_MD",
## "B1PIDATE", "DDate", "DOD_Y",


if (FALSE) {
## need to limit due to memory constraints
cl <- makeCluster(10)
## registerDoParallel(cl)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2016-04-01", R.version = "3.2.4")
  library(mice)
  library(randomForest)
  ## library(caret)
  ## library(ipred)
  })

imputation_seeds <- c(
  403L, 2L, 2118700268L, 1567504751L, -161759579L, -1822093220L,
  1093060658L, 1258159917L, -250049049L, -1470426866L,
  1741777492L, 961311575L, -1409058995L, 1939344100L, -158264630L,
  -1756812491L, 1217126799L, -350005610L, -1430768064L, -416067837L,
  916506401L, -2086039536L, -526825490L, -397482087L, 612794411L,
  -2030119654L, 1258444764L, 1217690079L, 1365220789L, -1816351924L,
  -691467870L, -631813891L, -367962633L, -826543330L, -979915304L,
  -1506474181L, -528224039L, -1726899096L, -1372003946L, -499687151L,
  -698756413L, -65410350L, -866952348L, 569921191L, 1393833981L,
  -1039109644L, -936870182L, 1649180709L, -1932571329L, 1391297222L
)

  pmat <- (1 - diag(1, ncol(dwimp.use[[1]])))
  colnames(pmat) <- rownames(pmat) <- colnames(dwimp.use[[1]])
  ## do not impute ID, family number, or last age
  pmat[1:3, ] <- 0
  pmat[, 1:3] <- 0

clusterExport(cl, c("dwimp.use", "imputation_seeds", "pmat"))


imputed <- parLapplyLB(cl, 1:50, function(i) {
  set.seed(imputation_seeds[i])
  ## now do the imputation
  mice(
    dwimp.use[[i]],
    m = 1,
    method = "rf",
    predictorMatrix = pmat,
    maxit = 25,
    seed = imputation_seeds[i],
    ntree = 10
  )
})
saveRDS(imputed, file = "midus_stress_imputed.RDS")

  final_imputed <- lapply(imputed, mice::complete, 1)

## for (i in 2:length(imputed)) {
##   final_imputed <- ibind(final_imputed, imputed[[i]])
## }
## final_imputed <- cbind.mids(final_imputed, tmp.d.all[, noimpvars])

saveRDS(final_imputed, file = "midus_stress_final_imputed.RDS")
}

final_imputed <- readRDS("midus_stress_final_imputed.RDS")

vn <- dimnames(imputedr1[[1]]$chainMean)[[1]]

chainMeans <- do.call(rbind, lapply(vn, function(v) {
  melt(cbind.data.frame(Iteration = 1:20, Var = v,
             as.data.frame(sapply(imputedr1, function(x) x$chainMean[v, ,1]))), id.vars = c("Iteration", "Var"))
}))

ggplot(chainMeans, aes(Iteration, value, colour = variable, linetype = variable)) +
  geom_line() +
  theme_cowplot() +
  facet_wrap(~Var, scales = "free_y")

chainSDs <- do.call(rbind, lapply(vn, function(v) {
  melt(cbind.data.frame(Iteration = 1:20, Var = v,
             as.data.frame(sapply(imputedr1, function(x) sqrt(x$chainVar[v, ,1])))), id.vars = c("Iteration", "Var"))
}))

ggplot(chainSDs, aes(Iteration, value, colour = variable, linetype = variable)) +
  geom_line() +
  theme_cowplot() +
  facet_wrap(~Var, scales = "free_y")

## ggplot(subset(chainMeans, Iteration > 10), aes(Iteration, value, colour = variable, linetype = variable)) +
##   geom_line() +
##   theme_cowplot() +
##   facet_wrap(~Var, scales = "free_y")

## dchainMeans <- as.data.table(chainMeans)
## dchainMeans[, CM := mean(value), by = .(Var, variable)]
## dchainMeans[, .(Total = sd(value), Within = sd(value - CM)), by = Var][, .(Ratio = Total / Within), by = Var]


## h2oServer <- h2o.init(max_mem_size = "16g", nthreads = 10)
## brel <- ml_rel_mat(tmp.d.all, h2oServer)
## saveRDS(brel, file = "brel.RDS")
## brel <- readRDS("brel.RDS")

covariates <- c("Sex", "RaceG3", "AGE")
covs.ignore <- c("BirthYear", "A1SBMI", "B1SBMI", "A1SWSTHI", "B1SWSTHI")

xvars <- c(
  "MFAbuse", "A1SPIWOR",
  "A1SHOMET", "A1SPIHOM", "A1SKINNE", "A1SFDSNE", "A1SSPCRI", "A1SPIFAM",
  "A1SLFEDI", "A1SDAYDI",
  "LifeStress", "LifeStressImpactS", "LifeStressImpactL",
  "B1SJOBDI", "B1SPIWOR",
  "B1SHOMET", "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
  "B1SLFEDI", "B1SDAYDI",
  "B4QPS_PS", "CTQTotal",
  "dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi",
  "dFriSt", "dFamSt", "dParSt"
)

yvars <- c(
  "B1SCHRON", "b3tem", "exec_fxn", "Rb4bcrp", "b4bil6", "B1PA1", "B1PA2", "B1PA3",
  "LastAge"
)
yvars.ignore <- c("A1SCHRON", "A1SS7", "A1PA4", "A1PA5", "A1PA6", "B1PF7A", "DECEASED")


yvar.info <- data.frame(
  Vars = c(
    "A1SS7", "B1PF7A",
    "A1PA6", "B1PA3",
    "A1PA4", "B1PA1",
    "A1PA5", "B1PA2",
    "A1SCHRON", "B1SCHRON",
    "b4bsesel", "b4bsicam", "b4bil6", "Rb4bcrp", "b4bfgn",
    "avgb_lf", "avgb_rm", "avgb_hf",
    "pulpress", "Rb4p1gs",
    "b3tem", "exec_fxn", "LastAge", "DECEASED"),
  Labels = c(
    "Self-rated Health (M1)", "Self-rated Health (M2)",
    "Self-rated Relative Health (M1)", "Self-rated Relative Health (M2)",
    "Self-rated Physical Health (M1)", "Self-rated Physical Health (M2)",
    "Self-rated Mental Health (M1)", "Self-rated Mental Health (M2)",
    "Chronic Conditions (M1)", "Chronic Conditions (M2)",
    "sE-Selectin (M2P4)", "sICAM (M2P4)", "IL-6 (M2P4)", "CRP (M2P4)", "Fibrinogen (M2P4)",
    "LFHRV (M2P4)", "HRV RMSSD (M2P4)", "HFHRV (M2P4)",
    "Pulse Pressure (M2P4)", "SBP (M2P4)",
    "Episodic Memory (M2)", "Executive Function (M2)", "Mortality", "Mortality"),
  Type = c(rep("ordered", 8), rep("count", 2), rep("continuous", 12), rep("survival", 2)),
  Levels = c(6, 7, rep(5, 6), rep(NA, 16)))
yvar.info$Time <- ifelse(grepl("\\(M1\\)", yvar.info$Labels), 1, 2)

## testit <- mygam.fit("B1SCHRON", "~ s(A1SBMI, k = 5)", singleandpred=TRUE)
## testit2 <- mygam.fit("B1SCHRON", "~ s(A1SBMI, k = 5)", covs=TRUE, singleandpred=TRUE)

mygam.fit <- function(dv, form, impdata = final_imputed, impnum = 1, rawdata = tmp.d.all, info = yvar.info, topk = 5, minp = .001, maxp = .1, select = TRUE, covs = FALSE, singleandpred = FALSE, ...) {

  if (covs) {
    final_form <- paste0(dv, form, "+ Sex + RaceG3 + s(AGE, k = 5)")
    final_form_covs_only <- paste0(dv, " ~ Sex + RaceG3 + s(AGE, k = 5)")
    mainiv <- all.vars(as.formula(form))
  } else {
    final_form <- paste0(dv, form)
  }
  usedata <- complete(impdata, impnum)[!is.na(rawdata[[dv]]), ]

  if (info[which(info$Vars == dv), "Type"] == "ordered") {
    stopifnot(length(unique(usedata[[dv]])) == info[which(info$Vars == dv), "Levels"])

    ## usedata[[dv]] <- as.integer(factor(usedata[[dv]], levels = sort(unique(usedata[[dv]])), ordered = TRUE))

    usedata[[dv]] <- as.integer(usedata[[dv]])

    obj <- gam(
      formula = as.formula(final_form),
      family = ocat(R = info[which(info$Vars == dv), "Levels"]),
      data = usedata,
      method = "REML",
      select = select, ...)

    objcovonly <- gam(
      formula = as.formula(final_form_covs_only),
      family = ocat(R = info[which(info$Vars == dv), "Levels"]),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$dev.expl
    perfcovonly <- summary(objcovonly)$dev.expl


  } else if (info[which(info$Vars == dv), "Type"] == "count") {
    usedata[[dv]] <- as.integer(factor(usedata[[dv]], levels = sort(unique(usedata[[dv]])), ordered = TRUE)) - 1

    obj <- gam(
      formula = as.formula(final_form),
      family = nb(),
      data = usedata,
      method = "REML",
      select = select, ...)

    objcovonly <- gam(
      formula = as.formula(final_form_covs_only),
      family = nb(),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$r.sq
    perfcovonly <- summary(objcovonly)$r.sq

  } else if (info[which(info$Vars == dv), "Type"] == "survival") {
    obj <- gam(
      formula = as.formula(final_form),
      family = cox.ph(link = "identity"),
      data = usedata,
      method = "REML",
      select = select,
      weights = DECEASED,
      ...)

    objcovonly <- gam(
      formula = as.formula(final_form_covs_only),
      family = cox.ph(link = "identity"),
      data = usedata,
      method = "REML",
      select = select,
      weights = DECEASED,
      ...)

    perf <- summary(obj)$dev.expl
    perfcovonly <- summary(objcovonly)$dev.expl

  } else {
    obj <- gam(
      formula = as.formula(final_form),
      data = usedata,
      method = "REML",
      select = select, ...)

    objcovonly <- gam(
      formula = as.formula(final_form_covs_only),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$r.sq
    perfcovonly <- summary(objcovonly)$r.sq

  }

  if (singleandpred) {
    if (covs) {
      vars <- all.vars(formula(obj))
      usedata <- usedata[, unique(c(vars, "DECEASED"))]
      usedata$AGE <- 0 ## mean age roughly since standardized
      usedata$Sex <- factor("Female", levels = c("Female", "Male"))
      usedata$RaceG3 <- factor("White", levels = c("White", "AA", "Other"))

      ivrange <- range(usedata[, mainiv], na.rm = TRUE)
      newdata <- usedata[rep(1, 250), ]
      newdata[, mainiv] <- seq(from = ivrange[1], to = ivrange[2], length.out = 250)
      newdata$LastAge <- 65
      yhat <- predict(obj, newdata = newdata, type = "response", se = FALSE)
      ivvars <- vars[-1]
      if (is.matrix(yhat)) {
        vars <- c(paste0(vars[1], "_", 1:ncol(yhat)), ivvars)
      }
      yhat <- cbind(as.data.frame(yhat), newdata[, ivvars])
      colnames(yhat) <- vars

    } else {
      vars <- all.vars(formula(obj))
      ivrange <- range(usedata[, vars[2]], na.rm = TRUE)
      newdata <- usedata[rep(1, 250), ]
      newdata[, vars[2]] <- seq(from = ivrange[1], to = ivrange[2], length.out = 250)
      newdata$LastAge <- 65
      yhat <- predict(obj, newdata = newdata, type = "response", se = FALSE)
      ivvars <- vars[-1]
      if (is.matrix(yhat)) {
        vars <- c(paste0(vars[1], "_", 1:ncol(yhat)), ivvars)
      }
      yhat <- cbind(as.data.frame(yhat), newdata[, ivvars])
      colnames(yhat) <- vars
    }
  } else {
    yhat <- NA
  }

  tab <- as.data.frame(summary(obj)$s.table)
  tab <- cbind(Vars = gsub("s\\((.*)\\)", "\\1", rownames(tab)),
               tab)
  rownames(tab) <- NULL

  colnames(tab) <- c("Vars", "edf", "Refdf", "statistic", "p")

  if (covs) {
    tab <- subset(tab, Vars != "AGE")
  }

  if (length(tab$edf) <= topk) {
    minedf <- -.0001
  } else {
    minedf <- tab$edf[which(rank(tab$edf) == (length(tab$edf) - topk))]
  }

  usetab <- droplevels(subset(tab, (p < minp) | ((p < maxp) & (edf > minedf))))

  return(list(
    Model = obj, Table = tab, Refined = usetab,
    Performance = perf, CovPerformance = ifelse(covs, perfcovonly, NA),
    Type = ifelse(info[which(info$Vars == dv), "Type"] %in% c("ordered", "survival"), "Deviance", "R2"),
    Yhat = yhat))
}


cl <- makeCluster(40)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2016-03-15", R.version = "3.2.4")
  library(mgcv)
  library(mice)
})

## indmultires <- function(dv, iv, k = 5) {
##   env <- environment()

##   form <- paste0(" ~ ", paste0(paste0("s(", iv, ", k = ", k, ")"), collapse = " + "))
##   ## clusterExport(cl, "form", envir = env)

##   ires <- lapply(1:10, function(i) mygam.fit(dv, form, impnum = i, select = FALSE, singleandpred=TRUE))

##   tmp <- do.call(rbind, lapply(ires, function(x) x$Refined))


##   outd <- data.frame(DV = dv, IV = iv,
##              Performance = tanh(mean(atanh(sapply(ires, function(x) x$Performance)))),
##              P = mean(tmp$p))

##   list(Data = outd, Yhat = lapply(ires, function(x) x$Yhat))
## }

## clusterExport(cl, c("mygam.fit", "final_imputed", "tmp.d.all", "yvar.info", "stresskey2", "indmultires", "xvars", "yvars"))


## ires.all <- parLapplyLB(cl, yvars, function(dv) {
##   tmpx <- xvars[-(1:5)]
##   xtime <- stresskey2$Time[match(tmpx, stresskey2$Var)]
##   dvtime <- yvar.info$Time[yvar.info$Vars == dv]
##   usex <- tmpx[xtime <= dvtime]

##   lapply(usex, function(iv) {
##     indmultires(dv, iv, k = ifelse(iv == "B4QCT_MD", 4, 5))
##   })
## })

## ires.all.plots <- lapply(1:length(yvars), function(iy) {
##   useres <- ires.all[[iy]]
##   dvvar <- as.character(useres[[1]]$Data$DV)

##   testit2 <- do.call(rbind, lapply(1:length(useres), function(i) {
##     tmpout <- cbind(
##       P = useres[[i]]$Data$P,
##       IV = useres[[i]]$Data$IV,
##       melt(Reduce(`+`, useres[[i]]$Yhat) / length(useres[[i]]$Yhat),
##            id.vars = useres[[i]]$Data$IV))

##     colnames(tmpout) <- c("P", "IV", "IVValue", "DVLevel", "DVValue")

##     return(tmpout)
##   }))

##   testit2$DVLevel <- gsub("(.*_)(.)", "\\2", testit2$DVLevel)
##   testit2$IV <- stresskey2$Label[match(testit2$IV, stresskey2$Var)]
##   testit2 <- testit2[order(testit2$P), ]
##   testit2$IV2 <- sprintf("%s, log10 p %s", testit2$IV, ifelse(is.na(testit2$P), "> -1.00", paste0("= ", format(log10(testit2$P), digits = 2, nsmall = 2))))
##   testit2$IV2 <- factor(testit2$IV2, levels = unique(testit2$IV2))

##   if (length(unique(testit2$DVLevel)) > 1) {
##   pout <- ggplot(testit2, aes(IVValue, DVValue, fill = DVLevel)) +
##     geom_area() +
##     facet_wrap(~IV2, ncol = 3, scales = "free") +
##     theme_classic() +
##     xlab("") +
##     ylab(yvar.info$Labels[yvar.info$Vars == dvvar]) +
##     ggtitle(yvar.info$Labels[yvar.info$Vars == dvvar])
##   } else {
##   pout <- ggplot(testit2, aes(IVValue, DVValue)) +
##     geom_line() +
##     facet_wrap(~IV2, ncol = 3, scales = "free") +
##     theme_classic() +
##     xlab("") +
##     ylab(yvar.info$Labels[yvar.info$Vars == dvvar]) +
##     ggtitle(yvar.info$Labels[yvar.info$Vars == dvvar])
##   }

## return(pout)
## })


## pdf("MIDUS_Stress_univariate_time1.pdf", width = 4 * 3, height = 3 * 3)
## for (i in which(yvar.info$Time[match(yvars, yvar.info$Vars)] == 1)) {
##   print(ires.all.plots[[i]])
## }
## dev.off()

## pdf("MIDUS_Stress_univariate_time2.pdf", width = 4 * 3, height = 3 * 9)
## for (i in which(yvar.info$Time[match(yvars, yvar.info$Vars)] == 2)) {
##   print(ires.all.plots[[i]])
## }
## dev.off()


indmultirescov <- function(dv, iv, k = 5) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", iv, ", k = ", k, ")"), collapse = " + "))
  ## clusterExport(cl, "form", envir = env)

  ires <- lapply(1:20, function(i) mygam.fit(dv, form, impnum = i, select = FALSE, covs=TRUE, singleandpred=TRUE))

  tmp <- do.call(rbind, lapply(ires, function(x) x$Refined))

  outd <- data.frame(DV = dv, IV = iv,
             Performance = tanh(mean(atanh(sapply(ires, function(x) x$Performance)))),
             CovPerformance = tanh(mean(atanh(sapply(ires, function(x) x$CovPerformance)))),
             dPerformance = mean(sapply(ires, function(x) x$Performance - x$CovPerformance)),
             P = mean(tmp$p),
             Positive = mean(sapply(ires, function(x) x$Yhat[nrow(x$Yhat), 1] - x$Yhat[1, 1])))

  list(Data = outd, Yhat = lapply(ires, function(x) x$Yhat))
}

multirescov <- function(dv, iv, k = 5) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", iv, ", k = ", k, ")"), collapse = " + "))
  ## clusterExport(cl, "form", envir = env)

  ires <- lapply(1:20, function(i) mygam.fit(dv, form, impnum = i,
                                             select = FALSE, covs=TRUE,
                                             singleandpred=FALSE))

  tmp <- do.call(rbind, lapply(ires, function(x) x$Refined))

  outd <- data.frame(
    DV = dv, IV = paste(iv, collapse = ", "),
    Performance = tanh(mean(atanh(sapply(ires, function(x) x$Performance)))),
    CovPerformance = tanh(mean(atanh(sapply(ires, function(x) x$CovPerformance)))),
    dPerformance = mean(sapply(ires, function(x) x$Performance - x$CovPerformance)))

  list(Data = outd, Models = ires)
}


irescov.all <- lapply(1, function(junk) {
  dv <- "B1SCHRON"
  iv <- "A1SPIWOR"
  indmultirescov(dv, iv, k = ifelse(iv == "B4QCT_MD", 4, 5))
})

univariate.pairs <- expand.grid(IV = xvars, DV = yvars, stringsAsFactors = FALSE)
multivariate.pairs <- list(DV = yvars,
                           IV = rep(list(xvars), length(yvars)))


clusterExport(cl, c("mygam.fit", "final_imputed", "tmp.d.all",
                    "yvar.info", "stresskey2",
                    "indmultirescov", "multirescov",
                    "xvars", "yvars",
                    "univariate.pairs", "multivariate.pairs"))


irescov.all <- parLapplyLB(cl, 1:nrow(univariate.pairs), function(rownum) {
  dv <- univariate.pairs$DV[rownum]
  iv <- univariate.pairs$IV[rownum]
  indmultirescov(dv, iv, k = ifelse(iv == "B4QCT_MD", 4, 5))
})
saveRDS(irescov.all, file = "irescov_all.RDS")
irescov.all <- readRDS("irescov_all.RDS")

irescov.all.summaries <- do.call(rbind, lapply(irescov.all, function(x) x$Data))

## reverse IVs as needed
irescov.all.summaries$Positive <- irescov.all.summaries$Positive * ifelse(
  irescov.all.summaries$IV %in% c("A1SHOMET", "B1SHOMET", "dHomIn",
                                  "LifeStressImpactS", "LifeStressImpactL"), -1, 1)

## reverse DVs as needed
## for self rated health, using first category, which is the "best" so higher = better
## lastage is really a survivor function, so higher = better
irescov.all.summaries$Positive <- irescov.all.summaries$Positive * ifelse(
  irescov.all.summaries$DV %in% c("B1SCHRON", "Rb4bcrp", "b4bil6"), -1, 1)

saveRDS(irescov.all.summaries, file = "irescov_all_summaries.RDS")
irescov.all.summaries <- readRDS("irescov_all_summaries.RDS")

irescov.all.dPerformance <- dcast(within(irescov.all.summaries, {
  dPerformance <- pmax(dPerformance, 0) * sign(Positive) * 100
})[, c("DV", "IV", "dPerformance")], IV ~ DV)
dput(irescov.all.dPerformance)

irescov.all.dPerformance <- structure(list(IV = structure(1:34, .Label = c("MFAbuse", "A1SPIWOR",
"A1SHOMET", "A1SPIHOM", "A1SKINNE", "A1SFDSNE", "A1SSPCRI", "A1SPIFAM",
"A1SLFEDI", "A1SDAYDI", "LifeStress", "LifeStressImpactS", "LifeStressImpactL",
"B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM", "B1SKINNE", "B1SFDSNE",
"B1SSPCRI", "B1SPIFAM", "B1SLFEDI", "B1SDAYDI", "B4QPS_PS", "CTQTotal",
"dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi", "dFriSt",
"dFamSt", "dParSt"), class = "factor"), B1SCHRON = c(-1.41156475766525,
-1.12390028309812, -1.61211582157034, -1.65018146365394, -2.2415609907147,
-1.22381504605558, -0.953692610833015, -2.17081132497301, -0.494749254195066,
-0.966853010924398, -6.83188878350207, -6.07250667253866, -4.33218305606468,
-0.469730326549752, -1.11456155112167, -2.13947747079457, -2.1665762359373,
-3.337802549719, -1.68199788763962, -1.27564109437553, -3.57027729354089,
-2.36017947728839, -2.27342292756974, -2.57493576668494, -2.83075519839604,
0.658895548639568, -0.33221719236284, -0.570007563799157, -0.143804918472146,
-1.0344197175803, -1.28802316271132, -0.0472991049140864, -0.276421882690667,
0.209261868845041), b3tem = c(-0.134716269151127, -0.292155390059938,
-0.322736910284588, -0.238855759219366, -0.120729700223369, -0.150203269230975,
0, -0.557368387055314, 0.00190326282703546, -0.129900607717344,
-0.253728246788302, 0.496739165115277, -0.13788132357793, -0.0290298661956562,
-0.398452952269633, -0.626981606079007, -0.487217636592049, -0.273698736450984,
-0.313008063366096, -0.00225374338893913, -0.386936167357118,
0.0320316574223906, -0.119479131742454, -0.181341155656969, -0.227614621278827,
0.0862088623627644, 0.0916776136155328, 0.0365286633206047, -0.0425274279666071,
-0.113537100562431, 0.0514576771182568, 0.168071160438411, 0.0945524010379056,
-0.0202130511001081), exec_fxn = c(-0.187853324409932, -1.29263895970538,
-0.280006405853981, -0.325952897060268, -0.228087819776216, -0.361710231674637,
-0.143779200056683, -1.10160318003436, -0.0705114645811522, -0.716737510474509,
-0.24742318573847, 1.11187213469549, -0.416186885827559, -0.244946688192002,
-1.17118462899099, -0.612362711260781, -0.612772027997829, -0.21099481936193,
-0.54969313360135, -0.11002827236302, -0.784286987226658, 0,
-0.759933662351384, -0.399417072760214, -0.292126632051785, 0.038193874595176,
0.297500668100054, 0.144999044315107, -0.0787488220584792, -0.609592193490536,
0, -0.167911506276828, 0.110358410365787, -0.151080185628019),
    Rb4bcrp = c(-0.0592252480348071, -0.120549837149678, -0.0618463854953416,
    -0.640817559054228, -0.155601331876183, -0.3225217361594,
    0, -0.125573738065476, 0.283782557517052, -0.214911507871411,
    -1.45502226294511, 1.50566292927654, -0.626251502909609,
    -0.0383187125346834, -0.291044066523874, -0.121751559980887,
    -0.0405622183494009, -0.351442739905933, 0, 0.093390594571105,
    -0.0465952408062348, -0.318657192165438, -0.594465569803908,
    0, -0.518536825510866, -0.290892210802314, 0.104419388569298,
    0, -0.250266049005702, 0.435241098837943, -0.150833128837685,
    0.0786622069377046, 0.0212722743552102, 0.00208289828906638
    ), b4bil6 = c(0, -0.253478486049379, -0.0405072763998165,
    -0.256929158888204, -0.131003452586124, 0.0407376255802538,
    0, -0.233698245115015, 0.0728664375771737, -0.131422341611749,
    -1.39607499115305, 1.76215177702626, -0.657236959545106,
    0, -0.759802943005781, -0.238396114872322, -0.804296535038241,
    -1.057384454543, 0, -0.237755815004075, -0.246627913965382,
    -0.386401166004849, -0.153164876220553, -0.128450296575281,
    -0.734542436282117, 0.351314116273893, 0, -0.0452946434877716,
    -0.150973827597322, 0.241704570543418, -0.0689436406300592,
    0, 0, -0.0374306295016763), B1PA1 = c(-0.343701860826531,
    -0.786219368300082, -0.517916056608531, -0.930540854044835,
    -0.48060515781958, -0.361931321901509, -0.194382773111394,
    -1.12874143724484, -0.0515359705980181, -0.449089794356972,
    -0.928744823032608, -0.763092673329857, -0.895829805747536,
    -0.225314386594257, -0.802169448577585, -0.859571846586549,
    -1.29904958115784, -0.573694208322237, -0.369141461741108,
    -0.201492723685271, -1.29677069470101, -0.168820790077864,
    -0.60152093612197, -0.874494536939444, -0.716105426648617,
    0.394169643431571, 0.249727141887457, -0.2118117378663, -0.123791523013518,
    -0.1846928883946, -0.123829473682689, 0.0776133185305226,
    0.038055686717495, 0.0423841647423475), B1PA2 = c(-0.460059636864223,
    -0.972384352612945, -0.586446410088712, -1.29741414021833,
    -0.844323434352458, -0.558623909413306, -0.551710990625195,
    -1.34414207586653, -0.0924425607366065, -0.652381383568101,
    -1.04664602181734, -1.06300522036663, -1.29787358605505,
    -0.369839139092314, -1.1887568681486, -0.851948601150237,
    -1.88746423491175, -1.18138205010819, -0.645102228389003,
    -0.668004497200135, -1.78623482497607, -0.118304362140889,
    -0.829829877910779, -1.59281163037925, -0.958200689135624,
    0.554497380167516, 0.134293929340045, -0.216814386107804,
    -0.0971377701492988, 0.212648750009537, -0.0949510932766357,
    -0.0284404576908712, -0.10184520161971, -0.0255088470122478
    ), B1PA3 = c(-0.163735048541756, -0.472613333329874, -0.446486506129606,
    -0.578890738724606, -0.296163164354804, -0.0709335963515695,
    -0.118803507854963, -0.681643240821098, -0.00068068197450095,
    -0.154782007912534, -0.336460621921656, -0.272761848415174,
    -0.48385383058356, -0.153955213509907, -0.544744748730172,
    -0.583268377166395, -0.812995033602633, -0.351578821951015,
    -0.150851723519024, -0.128184401378101, -0.717141588062279,
    -0.052765561653068, -0.259252592823172, -0.659378806542959,
    -0.415860004955292, 0.167201507815849, 0.150542845572644,
    -0.07979035420664, 0.0699276479991126, -0.0209768261922683,
    -0.0382166420293324, 0.0447560095961786, -0.00119049849270928,
    0.00468634353954769), LastAge = c(-0.16282869133049, -0.15788348407615,
    -0.355633263570828, -0.463710476136805, -0.280415311966562,
    -0.380197438418259, -0.056085547417102, -0.245189243349324,
    -0.040277174514743, -0.07398425351752, -0.591356945968212,
    -0.569133946720783, -0.697074253245761, -0.0331238068597069,
    -0.182188454345994, -0.293242062996694, -0.28712191176023,
    -0.0994028802217156, -0.0605815873861914, -0.0813045940703728,
    -0.395218821124351, -0.00865200217041048, -0.0993263300133219,
    -0.37472619601049, -0.294068698332152, 0.204780528009099,
    0.627617916734158, -0.187467339199685, -0.0334027112414865,
    -0.19923251469821, -0.0011219610788326, 0.0737216805538889,
    0.06859621531851, -0.120146214012655)), .Names = c("IV",
"B1SCHRON", "b3tem", "exec_fxn", "Rb4bcrp", "b4bil6", "B1PA1",
"B1PA2", "B1PA3", "LastAge"), row.names = c(NA, -34L), class = "data.frame")


library(heatmap3)


rownames(irescov.all.dPerformance) <- as.character(stresskey2$Label[match(as.character(irescov.all.dPerformance$IV), stresskey2$Var)])
irescov.all.dPerformance$IV <- NULL
colnames(irescov.all.dPerformance) <- stresskey2$Label2[match(colnames(irescov.all.dPerformance), stresskey2$Var)]

pdf(file = "midus_individual_heatmap.pdf", width = 12, height = 10)
heatmap3(irescov.all.dPerformance,
         ##balance = TRUE,
         ##distfun = function(x) as.dist(1 - t(x)),
         col = colorRampPalette(c("navy", "white", "firebrick3"))(1024),
         ## RowSideColors = key2$TimepointCol[rnindex],
         ## ColSideColors = key2$TimepointCol[cnindex],
         margins = c(12, 12),
         ## legendfun = function() {
         ##   showLegend(
         ##     legend = c("M I", "M II", "M II P4"),
         ##     col = c("steelblue2", "lightgoldenrod", "brown1"),
         ##     cex = 1.5)
         ## },
         balanceColor = TRUE, method="ward.D2",
         scale = "none",
         main = "MIDUS Stress and Health")
dev.off()




multirescov.all <- parLapplyLB(cl, 1:length(multivariate.pairs$DV), function(rownum) {
  dv <- multivariate.pairs$DV[rownum]
  iv <- multivariate.pairs$IV[[rownum]]
  multirescov(dv, iv)
})
saveRDS(multirescov.all, file = "multirescov_all.RDS")

multirestab <- cbind.data.frame(
  multirescov.all[[1]][[2]][[1]]$Table[,1],
sapply(1:9, function(i) {
  log10(rowMeans(do.call(cbind, lapply(1:20, function(imp) multirescov.all[[i]][[2]][[imp]]$Table$p))))
}), stringsAsFactors = FALSE)
colnames(multirestab) <- c("Stress", yvars)

multiresrank <- cbind.data.frame(Stress = multirestab[,1], apply(multirestab[,-1], 2, rank))

dput(multiresrank[order(rowMeans(multiresrank[,-1])), ])

orderedmultiresrank <- structure(list(Stress = structure(c(34L, 32L, 33L, 21L, 22L,
6L, 1L, 19L, 8L, 4L, 23L, 18L, 15L, 24L, 12L, 29L, 3L, 13L, 5L,
35L, 11L, 31L, 17L, 16L, 25L, 7L, 28L, 27L, 9L, 2L, 20L, 30L,
26L, 14L), .Label = c("A1SDAYDI", "A1SFDSNE", "A1SHOMET", "A1SKINNE",
"A1SLFEDI", "A1SPIFAM", "A1SPIHOM", "A1SPIWOR", "A1SSPCRI", "AGE",
"B1SDAYDI", "B1SFDSNE", "B1SHOMET", "B1SJOBDI", "B1SKINNE", "B1SLFEDI",
"B1SPIFAM", "B1SPIHOM", "B1SPIWOR", "B1SSPCRI", "B4QPS_PS", "CTQTotal",
"dDayDi", "dFamIn", "dFamSt", "dFriSt", "dHomIn", "dLfeDi", "dNeiQu",
"dParSt", "dWorIn", "LifeStress", "LifeStressImpactL", "LifeStressImpactS",
"MFAbuse"), class = "factor"), B1SCHRON = c(4, 1, 2, 3, 6, 14,
9, 25, 8, 11, 13, 5, 12, 15, 16, 24, 30, 10, 27, 7, 32, 21, 19,
34, 31, 29, 18, 23, 20, 28, 17, 22, 26, 33), b3tem = c(1, 2,
3, 24, 17, 23, 9, 11, 30, 21, 25, 26, 8, 18, 7, 4, 5, 20, 33,
27, 34, 32, 10, 14, 15, 19, 6, 16, 12, 28, 31, 29, 22, 13), exec_fxn = c(1,
11, 3, 15, 21, 9, 4, 19, 2, 12, 5, 13, 29, 30, 7, 8, 18, 14,
16, 23, 32, 34, 22, 6, 24, 28, 17, 25, 26, 31, 27, 10, 33, 20
), Rb4bcrp = c(2, 1, 34, 4, 16, 15, 21, 6, 8, 14, 5, 25, 10,
22, 23, 27, 7, 30, 19, 29, 3, 9, 26, 31, 20, 12, 32, 13, 28,
11, 17, 18, 24, 33), b4bil6 = c(1, 2, 21, 34, 5, 13, 28, 7, 24,
18, 10, 14, 4, 22, 20, 12, 15, 19, 8, 3, 33, 17, 16, 6, 27, 31,
9, 30, 32, 25, 26, 29, 23, 11), B1PA1 = c(3, 4, 1, 2, 6, 5, 8,
18, 7, 10, 17, 9, 23, 14, 11, 21, 13, 16, 12, 30, 31, 27, 34,
33, 19, 29, 32, 24, 28, 25, 15, 20, 22, 26), B1PA2 = c(3, 4,
1, 2, 8, 7, 13, 19, 5, 14, 29, 6, 10, 12, 21, 20, 23, 24, 31,
28, 11, 15, 30, 25, 16, 9, 34, 22, 27, 17, 18, 32, 26, 33), B1PA3 = c(3,
6, 1, 2, 4, 5, 19, 9, 8, 16, 14, 13, 30, 7, 22, 33, 18, 10, 15,
12, 11, 24, 25, 32, 27, 17, 34, 28, 23, 20, 31, 29, 21, 26),
    LastAge = c(11, 1, 6, 3, 12, 15, 8, 5, 30, 10, 9, 24, 19,
    7, 23, 2, 26, 34, 20, 31, 4, 13, 16, 17, 21, 28, 22, 27,
    14, 29, 32, 25, 18, 33)), .Names = c("Stress", "B1SCHRON",
"b3tem", "exec_fxn", "Rb4bcrp", "b4bil6", "B1PA1", "B1PA2", "B1PA3",
"LastAge"), row.names = c(12L, 11L, 13L, 24L, 25L, 8L, 10L, 15L,
2L, 5L, 30L, 17L, 18L, 28L, 19L, 27L, 3L, 16L, 9L, 1L, 23L, 29L,
21L, 22L, 33L, 4L, 31L, 26L, 7L, 6L, 20L, 34L, 32L, 14L), class = "data.frame")

rownames(orderedmultiresrank) <- as.character(stresskey2$Label[match(as.character(orderedmultiresrank$Stress), stresskey2$Var)])
orderedmultiresrank$Stress <- NULL
colnames(orderedmultiresrank) <- stresskey2$Label2[match(colnames(orderedmultiresrank), stresskey2$Var)]

write.table(orderedmultiresrank, file= "clipboard", sep = "\t")




irescov.all.plots <- lapply(1:length(yvars), function(iy) {
  useres <- irescov.all[[iy]]
  dvvar <- as.character(useres[[1]]$Data$DV)

  testit2 <- do.call(rbind, lapply(1:length(useres), function(i) {
    useres[[i]]$Yhat <- lapply(useres[[i]]$Yhat, function(d) subset(d, select = -c(Sex, RaceG3, AGE)))
    tmpout <- cbind(
      P = useres[[i]]$Data$P,
      IV = useres[[i]]$Data$IV,
      melt(Reduce(`+`, useres[[i]]$Yhat) / length(useres[[i]]$Yhat),
           id.vars = useres[[i]]$Data$IV))

    colnames(tmpout) <- c("P", "IV", "IVValue", "DVLevel", "DVValue")

    return(tmpout)
  }))

  testit2$DVLevel <- gsub("(.*_)(.)", "\\2", testit2$DVLevel)
  testit2$IV <- stresskey2$Label[match(testit2$IV, stresskey2$Var)]
  testit2 <- testit2[order(testit2$P), ]
  testit2$IV2 <- sprintf("%s, log10 p %s", testit2$IV, ifelse(is.na(testit2$P), "> -1.00", paste0("= ", format(log10(testit2$P), digits = 2, nsmall = 2))))
  testit2$IV2 <- factor(testit2$IV2, levels = unique(testit2$IV2))

  if (length(unique(testit2$DVLevel)) > 1) {
  pout <- ggplot(testit2, aes(IVValue, DVValue, fill = DVLevel)) +
    geom_area() +
    facet_wrap(~IV2, ncol = 3, scales = "free") +
    theme_classic() +
    xlab("") +
    ylab(yvar.info$Labels[yvar.info$Vars == dvvar]) +
    ggtitle(yvar.info$Labels[yvar.info$Vars == dvvar])
  } else {
  pout <- ggplot(testit2, aes(IVValue, DVValue)) +
    geom_line() +
    facet_wrap(~IV2, ncol = 3, scales = "free") +
    theme_classic() +
    xlab("") +
    ylab(yvar.info$Labels[yvar.info$Vars == dvvar]) +
    ggtitle(yvar.info$Labels[yvar.info$Vars == dvvar])
  }

return(pout)
})


pdf("MIDUS_Stress_univariate_cov_time1.pdf", width = 4 * 3, height = 3 * 3)
for (i in which(yvar.info$Time[match(yvars, yvar.info$Vars)] == 1)) {
  print(irescov.all.plots[[i]])
}
dev.off()

pdf("MIDUS_Stress_univariate_cov_time2.pdf", width = 4 * 3, height = 3 * 9)
for (i in which(yvar.info$Time[match(yvars, yvar.info$Vars)] == 2)) {
  print(irescov.all.plots[[i]])
}
dev.off()













###### Not correct!!!!!!!!!! ##################
multirescov <- function(dv, iv, k = 5) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", iv, ", k = ", k, ")"), collapse = " + "))
  ## clusterExport(cl, "form", envir = env)

  ires <- lapply(1:10, function(i) mygam.fit(dv, form, impnum = i, select = FALSE, covs=TRUE, singleandpred=FALSE))

  tmp <- do.call(rbind, lapply(ires, function(x) x$Refined))
  outd <- data.frame(DV = dv, IV = iv,
             Performance = tanh(mean(atanh(sapply(ires, function(x) x$Performance)))),
             P = mean(tmp$p))

  list(Data = outd, Yhat = lapply(ires, function(x) x$Yhat))
}

clusterExport(cl, c("mygam.fit", "final_imputed", "tmp.d.all", "yvar.info", "stresskey2", "indmultires", "indmultirescov", "multirescov", "xvars", "yvars"))


mrescov.all <- parLapplyLB(cl, yvars, function(dv) {
  tmpx <- xvars[-(1:5)]
  xtime <- stresskey2$Time[match(tmpx, stresskey2$Var)]
  dvtime <- yvar.info$Time[yvar.info$Vars == dv]
  usex <- tmpx[xtime <= dvtime]
  multirescov(dv, iv = usex, k = ifelse(usex == "B4QCT_MD", 4, 5))
})





testit2 <- do.call(rbind, lapply(1:27, function(i) {
  tmpout <- cbind(
    P = ires.all[[1]][[i]]$Data$P,
    IV = xvars[-(1:5)][i], melt(testit[[1]][[i]], id.vars = xvars[-(1:5)][i]))
  colnames(tmpout) <- c("P", "IV", "IVValue", "DVLevel", "DVValue")
  return(tmpout)
}))
testit2$DVLevel <- gsub("(.*_)(.)", "\\2", testit2$DVLevel)
testit2$IV <- stresskey2$Label[match(testit2$IV, stresskey2$Var)]
testit2 <- testit2[order(testit2$P), ]
testit2$IV2 <- sprintf("%s, log10 p = %0.2f", testit2$IV, log10(testit2$P))
testit2$IV2 <- factor(testit2$IV2, levels = unique(testit2$IV2))

ggplot(testit2, aes(IVValue, DVValue, fill = DVLevel)) +
  geom_area() +
  facet_wrap(~IV2, ncol = 3, scales = "free") +
  theme_classic() +
  xlab("") +
  ylab(yvar.info$Labels[yvar.info$Vars == yvars[1]])



ires.all <- lapply(yvars[10], function(dv) {
    lapply(xvars[-(1:5)], function(iv) {
      indmultires(dv, iv, k = ifelse(iv == "B4QCT_MD", 4, 5))
    })
})

ires.all <- lapply(yvars[10], function(dv) {
  c(
    lapply(xvars[-(1:5)][4:5], function(iv) {
      indmultires(dv, iv, k = 5)
    }),
    lapply(xvars[-(1:5)][27], function(iv) {
      indmultires(dv, iv, k = 4)
    }))
})



ires.all.dat <- do.call(rbind, lapply(ires.all, function(d) {
  tmp <- do.call(rbind, d)
  ## top 5
  subset(tmp, (P < .10) & ((rank(Performance) > 22) | (P < .001)))
}))

for (i in 1:nrow(ires.all.dat)) {
  individual.result.mat[ires.all.dat$IV[i], ires.all.dat$DV[i]] <- sprintf("%0.1f%% (%s)",
  ires.all.dat$Performance[i] * 100,
  ifelse(
    ires.all.dat$P[i] < .0001, "<.0001",
    paste0("", format(round(ires.all.dat$P[i],4), digits = 4, nsmall = 4, scientific=4))))
}

colnames(individual.result.mat) <- key2$Label[match(colnames(individual.result.mat), key2$Var)]
rownames(individual.result.mat) <- c("Total", "Reduced", as.character(key2$Label[match(rownames(individual.result.mat), key2$Var)])[-(1:2)])

write.table(individual.result.mat[-(1:2), ], file = "clipboard", sep = "\t")





mywrapper <- function(dv, k = 5, ...) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", xvars[-(1:2)], ", k = ", k, ", bs = 'ts')"), collapse = " + "))
  clusterExport(cl, "form", envir = env)

  res <- parLapplyLB(cl, 1:10, function(i) mygam.fit(dv, form, impnum = i, ...))
  res.freq <- table(droplevels(unlist(lapply(res, function(x) x$Refined$Vars))))
  res.topvars <- names(res.freq)[res.freq >= 6]

  reducedform <- paste0(" ~ ", paste0(paste0("s(", res.topvars, ", k = ", k, ")"), collapse = " + "))
  clusterExport(cl, "reducedform", envir = env)
  reducedres <- parLapplyLB(cl, 1:10, function(i) mygam.fit(dv, reducedform, impnum = i, select = FALSE, ...))

  return(list(Results = res, Freq = res.freq, TopVars = res.topvars, ReducedResults = reducedres))
}

multires <- function(dv, ...) {
  mres <- mywrapper(dv, ...)
  mres.topvars <- mres$TopVars

  mres.topvars <- data.frame(
    Vars = mres.topvars,
    P = sapply(mres.topvars, function(v) {
      tmp <- do.call(rbind, lapply(mres$Results, function(x) x$Refined))
      mean(subset(tmp, Vars == v)$p)
    }))

  total.perf <- tanh(mean(atanh(sapply(mres$Results, function(x) x$Performance))))
  reduced.perf <- tanh(mean(atanh(sapply(mres$ReducedResults, function(x) x$Performance))))

  list(V = mres.topvars,
       TPerf = total.perf,
       RPerf = reduced.perf,
       All = mres)
}

mres.all <- lapply(yvars[1:5], multires)
mres.all2 <- lapply(yvars[6:8], multires)
mres.all3 <- lapply(yvars[9:11], multires)
mres.all4 <- lapply(yvars[12:14], multires)
mres.all5 <- lapply(yvars[15:17], multires)
mres.all6 <- lapply(yvars[18:20], multires, k = 4)

mres.all.final <- c(mres.all, mres.all2, mres.all3, mres.all4, mres.all5, mres.all6)
names(mres.all.final) <- yvars

multivariate.result.mat <- individual.result.mat <- matrix("", ncol = length(yvars), nrow = length(xvars) - 2 + 2,
                                                           dimnames = list(c("Total", "Reduced", xvars[-(1:2)]), yvars))

for (v in yvars) {
  multivariate.result.mat["Total", v] <- sprintf("%0.1f%%", mres.all.final[[v]]$TPerf * 100)
  multivariate.result.mat["Reduced", v] <- sprintf("%0.1f%%", mres.all.final[[v]]$RPerf * 100)

  multivariate.result.mat[match(mres.all.final[[v]]$V$Vars, rownames(multivariate.result.mat)), v] <- ifelse(
    mres.all.final[[v]]$V$P < .0001, "<.0001",
    paste0("", format(round(mres.all.final[[v]]$V$P,4), digits = 4, nsmall = 4, scientific=4)))
}

colnames(multivariate.result.mat) <- key2$Label[match(colnames(multivariate.result.mat), key2$Var)]
rownames(multivariate.result.mat) <- c("Total", "Reduced", as.character(key2$Label[match(rownames(multivariate.result.mat), key2$Var)])[-(1:2)])

write.table(multivariate.result.mat, file = "clipboard", sep = "\t")






form <- paste0("Rb4bcrp ~ Sex + s(AGE, k = 5, bs = 'ts') + ", paste0(paste0("s(", xvars[-(1:2)], ", k = 5, bs = 'ts')"), collapse = " + "))

form <- paste0("as.integer(factor(B1PA1, levels = sort(unique(B1PA1)), ordered = TRUE)) ~ Sex + s(AGE, k = 5, bs = 'ts') + ", paste0(paste0("s(", xvars[-(1:2)], ", k = 5, bs = 'ts')"), collapse = " + "))

gamcv <- function(formula, data, k = 5, seed = 1234, ...) {
  set.seed(seed)
  i <- createFolds(1:nrow(data), k = k)

  do.call(rbind, lapply(1:k, function(j) {
    m <- gam(formula, data = data[-i[[j]], ], ...)
    dv <- colnames(model.frame(m))[1]
    data.frame(Y = data[i[[j]], dv],
               Yhat = predict(m, newdata = data[i[[j]], ]))
  }))
}

testit <- gamcv(
  as.formula(form),
  data = complete(final_imputed, 1)[!is.na(tmp.d.all$Rb4bcrp), ],
  k = 5, seed = 1234)

library(mgcv)
summary(gam1 <- gam(as.formula(form),
            data = complete(final_imputed, 1)[!is.na(tmp.d.all$Rb4bcrp), ],
            method = "REML", select = TRUE))

summary(gam1 <- gam(as.formula(form),
                    data = complete(final_imputed, 1)[!is.na(tmp.d.all$B1PA1), ],
                    family = ocat(R = 5),
            method = "REML", select = TRUE))


tmpdat <- complete(final_imputed, 1)[!is.na(tmp.d.all$Rb4bcrp), ]
caret1 <- train(
  x = tmpdat[, xvars],
  y = tmpdat[, yvars[11]],
  method = "gam",
  metric = "Rsquared",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(select = TRUE, method = "GCV.Cp"),
  preProc = c("center", "scale"))


form2 <- paste0("Rb4bcrp ~ Sex + ", paste0(paste0("s(", gsub("s\\((.*)\\)", "\\1", rownames(subset(as.data.frame(summary(gam1)$s.table), edf > .5)))
                                                , ", k = 3)"), collapse = " + "))

summary(gam2 <- gam(as.formula(form2),
            data = complete(final_imputed, 1)[!is.na(tmp.d.all$Rb4bcrp), ],
            method = "REML", select = TRUE))



h2odat <- as.h2o(tmp.d.all, "midusd")

rf1 <- h2o.randomForest(x = xvars,
                 y = yvars[11],
                 h2odat,
                 ntrees = 1000,
                 max_depth = 10,
                 nfolds = 5,
                 fold_assignment = "Random")
rf1

ggplot(h2o.varimp(rf1), aes(factor(variable, levels = variable), (percentage))) +
  geom_point() +
  coord_flip()


rf2 <- h2o.randomForest(x = c("LifeStress", "AGE", "B4QPS_PS", "B1SLFEDI"),
                 y = yvars[11],
                 h2odat,
                 ntrees = 1000,
                 max_depth = 5,
                 nfolds = 10,
                 fold_assignment = "Random")
rf2

gbm1 <- h2o.gbm(x = xvars,
                y = yvars[5],
                h2odat,
                ntrees = 100,
                max_depth = 5,
                nfolds = 5,
                fold_assignment = "Random")

deep1 <- h2o.deeplearning(
  x = xvars,
  y = yvars[11],
  h2odat,
  activation = "RectifierWithDropout",
  hidden = c(80, 160, 160),
  hidden_dropout = c(.5, .5, .5),
  variable_importances = TRUE,
  nfolds = 10,
  fold_assignment = "Random")
deep1

ggplot(h2o.varimp(deep1), aes(factor(variable, levels = variable), (percentage))) +
  geom_point() +
  coord_flip()

deep2 <- h2o.deeplearning(
  x = c("LifeStress", "AGE", "B1PA1", "B4QPS_PS", "B1SLFEDI", "B1PA3"),
  y = yvars[5],
  h2odat,
  activation = "RectifierWithDropout",
  hidden = c(50, 100, 100),
  hidden_dropout = c(.5, .5, .5),
  variable_importances = TRUE,
  nfolds = 10,
  fold_assignment = "Random")
deep2

ggplot(h2o.varimp(deep2), aes(factor(variable, levels = variable), (percentage))) +
  geom_point() +
  coord_flip()


summary(gam(Rb4bcrp ~ s(LifeStress, k = 8) + s(B4QPS_PS, k = 8) + s(B1SLFEDI, k = 8), data = tmp.d.all, method = "REML", select = TRUE))


form <- paste0("Rb4bcrp ~ ",
               paste0(paste0("s(", xvars[-length(xvars)], ", k = 3)"), collapse = " + "))

summary(gam1 <- gam(as.formula(form), data = complete(final_imputed, 1)[!is.na(tmp.d.all2$Rb4bcrp), ], method = "REML", select = TRUE))
summary(gam1 <- gam(as.formula(form), data = complete(final_imputed, 2)[!is.na(tmp.d.all2$Rb4bcrp), ], method = "REML", select = TRUE))




library(heatmap3)
dvs <- c(
  "A1PA4", "A1PA5", "A1PA6", "A1SCHRON",
  "B1PA1", "B1PA2", "B1PA3", "B1SCHRON",
  "b3tem", "exec_fxn",
  "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel",
  "pulpress", "Rb4p1gs", "avgb_rm", "avgb_lf", "avgb_hf")

tmp <- as.matrix(brel$RelMat)
tmp[tmp < 0] <- 0

tmp <- tmp[-which(rownames(tmp) %in% dvs), dvs]
cnindex <- match(colnames(tmp), key2$Var)
rnindex <- match(rownames(tmp), key2$Var)

colnames(tmp) <- key2$Label2[cnindex]
rownames(tmp) <- key2$Label2[rnindex]

pdf(file = "midus_heatmap.pdf", width = 8, height = 8)
heatmap3(sqrt(tmp),
         ##balance = TRUE,
         ##distfun = function(x) as.dist(1 - t(x)),
         col = colorRampPalette(c("navy", "white", "firebrick3"), bias = 2)(1024),
         RowSideColors = key2$TimepointCol[rnindex],
         ColSideColors = key2$TimepointCol[cnindex],
         margins = c(12, 12),
         legendfun = function() {
           showLegend(
             legend = c("M I", "M II", "M II P4"),
             col = c("steelblue2", "lightgoldenrod", "brown1"),
             cex = 1.5)
         },
         scale = "none",
         main = "MIDUS Stress and Health")
dev.off()

pdf(file = "midus_heatmap2.pdf", width = 8, height = 8)
heatmap3(sqrt(tmp),
         ##balance = TRUE,
         ##distfun = function(x) as.dist(1 - t(x)),
         col = colorRampPalette(c("navy", "white", "firebrick3"), bias = 2)(1024),
         RowSideColors = key2$TimepointCol[rnindex],
         ColSideColors = key2$TimepointCol[cnindex],
         margins = c(12, 12),
         ## legendfun = function() {
         ##   showLegend(
         ##     legend = c("M I", "M II", "M II P4"),
         ##     col = c("steelblue2", "lightgoldenrod", "brown1"),
         ##     cex = 1.5)
         ## },
         scale = "none",
         main = "MIDUS Stress and Health")
dev.off()

dev.new()
## chordDiagram(symmat)
## tmpdf <- testit[[2]][, c("x", "y", "Perf")]
## colnames(tmpdf) <- c("from", "to", "value")
## chordDiagram(tmpdf)

chordDiagram(brel$RelMat,
             symmetric = TRUE,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.3))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
              niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important



