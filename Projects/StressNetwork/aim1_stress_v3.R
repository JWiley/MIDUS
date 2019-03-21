dir.setup <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Setup/"
dir.data <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Data/Processed/"
dir.imp <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Setup/stressnetwork_imputation/"
dir.proj <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Projects/StressNetwork/"

dir.setup <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
dir.data <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
dir.imp <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
dir.proj <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"

## hack to make rJava work on ubuntu RStudio server
if (identical(Sys.getenv("RSTUDIO"), "1")) {
  dyn.load("/usr/lib/jvm/java-9-oracle/lib/server/libjvm.so")
}

source(file.path(dir.setup, "setup_packages_functions.R"))
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


mVars <- as.data.table(read.xlsx(file.path(dir.setup,
  "midus_master_vars.xlsx"), sheetIndex = 1))
## setkey(mVars, "Domain")

mVars[!is.na(Domain) & Domain == "Stress", .(Name, Abbr, M1)][!is.na(M1)]
mVars[!is.na(Domain) & Domain == "Stress", .(Name, Abbr, M2)][!is.na(M2)]

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



dwclean <- readRDS(file.path(dir.data, "midus_merged_data_dwclean.RDS"))
psr_pana_bi.data <- readRDS(file.path(dir.data, "midus_merged_data_psrpanabi_imp.RDS"))
dwcleanimp <- readRDS(file.path(dir.imp, "midus_stress_final_imputed.RDS"))

v <- list(
  stress = c(
    ## M1
    "A1SLFEDI", "A1SDAYDI",
    "A1SPIWOR", "A1SHOMET", "A1SPIHOM", "A1SPIFAM",
    "A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1ParentAbuse", "A1SiblingAbuse",
    ## M2 (Retrospective)
    "B1ChildLifeStress", "B4CTQTotal",
    "B1LifeStress", "B1LifeStressImpactS", "B1LifeStressImpactL",
    ## M2
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM", "B1SHOMET",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS",
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


dwclean.use$CumStressM1 <- rowSums(apply(dwclean.use[, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
                                                         "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) as.vector(scale(x))))
dwclean.use$CumStressM2 <- rowSums(apply(dwclean.use[, c(
  "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
  "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
  "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
  "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

dwclean.use$DiscM1 <- rowSums(apply(dwclean.use[, c("A1SLFEDI", "A1SDAYDI")], 
                                    2, function(x) as.vector(scale(x))))
dwclean.use$SocM1 <- rowSums(apply(dwclean.use[, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE")], 
                                   2, function(x) as.vector(scale(x))))
dwclean.use$IneqM1 <- rowSums(apply(dwclean.use[, c("A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 
                                    2, function(x) as.vector(scale(x))))

dwclean.use$DiscM2 <- rowSums(apply(dwclean.use[, c(
  "B1SLFEDI", "B1SDAYDI", "B1SJOBDI")], 2, function(x) as.vector(scale(x))))
dwclean.use$SocM2 <- rowSums(apply(dwclean.use[, c(
  "B1SSPCRI", "B1SKINNE", "B1SFDSNE")], 2, function(x) as.vector(scale(x))))
dwclean.use$IneqM2 <- rowSums(apply(dwclean.use[, c(
  "B1SPIWOR", "B1SPIHOM", "B1SPIFAM")], 2, function(x) as.vector(scale(x))))

dwclean.use$CumStressBM1 <- rowSums(apply(dwclean.use[, c("DiscM1", "IneqM1", "SocM1")], 
                                          2, function(x) as.vector(scale(x))))
dwclean.use$CumStressBM2 <- rowSums(apply(dwclean.use[, c(
  "DiscM2", "IneqM2", "SocM2",
  "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

dwclean.use$CumStressBaltM2 <- rowSums(apply(dwclean.use[, c(
  "DiscM2", "IneqM2", "SocM2",
  "B1LifeStress")], 2, function(x) as.vector(scale(x))))


dwclean.use$CumStressBM1 <- (dwclean.use$CumStressBM1 - 
                               quantile(dwclean.use$CumStressBM1, probs = .25,
                                        na.rm = TRUE)) / IQR(dwclean.use$CumStressBM1, na.rm = TRUE)

dwclean.use$DiscM1 <- (dwclean.use$DiscM1 - 
  quantile(dwclean.use$DiscM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$DiscM1, na.rm = TRUE)
dwclean.use$IneqM1 <- (dwclean.use$IneqM1 - 
  quantile(dwclean.use$IneqM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$IneqM1, na.rm = TRUE)
dwclean.use$SocM1 <- (dwclean.use$SocM1 - 
  quantile(dwclean.use$SocM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$SocM1, na.rm = TRUE)


dwclean.use$CumStressBM2 <- (dwclean.use$CumStressBM2 - 
  quantile(dwclean.use$CumStressBM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$CumStressBM2, na.rm = TRUE)
dwclean.use$CumStressBaltM2 <- (dwclean.use$CumStressBaltM2 - 
  quantile(dwclean.use$CumStressBaltM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$CumStressBaltM2, na.rm = TRUE)
dwclean.use$DiscM2 <- (dwclean.use$DiscM2 - 
  quantile(dwclean.use$DiscM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$DiscM2, na.rm = TRUE)
dwclean.use$IneqM2 <- (dwclean.use$IneqM2 - 
  quantile(dwclean.use$IneqM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$IneqM2, na.rm = TRUE)
dwclean.use$SocM2 <- (dwclean.use$SocM2 - 
  quantile(dwclean.use$SocM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$SocM2, na.rm = TRUE)

dwclean.use$B4QPS_PS <- (dwclean.use$B4QPS_PS - 
  quantile(dwclean.use$B4QPS_PS, probs = .25, na.rm = TRUE)) / 
  IQR(dwclean.use$B4QPS_PS, na.rm = TRUE)
dwclean.use$B1LifeStress <- (dwclean.use$B1LifeStress - 
  quantile(dwclean.use$B1LifeStress, probs = .25, na.rm = TRUE)) / 
  IQR(dwclean.use$B1LifeStress, na.rm = TRUE)

# https://link.springer.com/article/10.1007/s10654-018-0375-y

for (i in 1:50) {
  dwcleanimp[[i]]$PAfterM2 <- dwclean.use$PAfterM2
}


for (i in 1:50) {
  dwcleanimp[[i]]$CumStressM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$CumStressM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

  dwcleanimp[[i]]$CumStressQ50M1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) x > median(x, na.rm = TRUE)))
  dwcleanimp[[i]]$CumStressQ50M2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) x > median(x, na.rm = TRUE)))

  dwcleanimp[[i]]$CumStressQ75M1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) x > quantile(x, probs = .75, na.rm = TRUE)))
  dwcleanimp[[i]]$CumStressQ75M2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) x > quantile(x, probs = .75, na.rm = TRUE)))
  
  dwcleanimp[[i]]$DiscM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SLFEDI", "A1SDAYDI")], 
                                          2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$SocM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE")], 
                                          2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$IneqM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 
                                         2, function(x) as.vector(scale(x))))
  
  dwcleanimp[[i]]$DiscM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$SocM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$IneqM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM")], 2, function(x) as.vector(scale(x))))
  
  
  dwcleanimp[[i]]$CumStressBM1 <- rowSums(apply(dwcleanimp[[i]][, 
    c("DiscM1", "IneqM1", "SocM1")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$CumStressBM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "DiscM2", "IneqM2", "SocM2",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))
}

## covariates
use.cov.m1stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all", "A1PB1", "A1PBWORK"),
 c("A1Smoke", "A1WorstAlcohol", "A1PhysAct", "A1SWSTHI"),
 c("A1PDEPDX", "A1PANXTD"))

use.cov.m2stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all", "B1PB1", "B1PBWORK"),
 c("B1Smoke", "B1WorstAlcohol", "B1PhysAct", "B1SWSTHI"),
 c("B1PDEPDX", "B1PANXTD"))


m1.stress.vars <- list("",
  c("DiscM1"),
  c("IneqM1"),
  c("SocM1"),
  c("CumStressBM1"))

m2.stress.vars.full <- list("",
  c("DiscM2"),
  c("IneqM2"),
  c("SocM2"),
  c("B4QPS_PS"),
  "B1LifeStress",
  "CumStressBM2")

## used for predictions later
m1.stressvars <- c("DiscM1", "IneqM1", "SocM1")
m2.full.stressvars <- c("DiscM2",
                        "IneqM2", "SocM2", 
                        "B4QPS_PS",
                        "B1LifeStress")


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
                           "LastAge", "DECEASED", "M2FAMNUM", "M2ID", "PAfterM2", "AGEM2", "AGEM1",
                           "A1SCHRON", "B1SCHRON",
                           "CumStressM1", "CumStressM2",
                           "CumStressBM1", "CumStressBM2")))]
  d$CumStressBM1 <- (d$CumStressBM1 - quantile(d$CumStressBM1, probs = .25)) / IQR(d$CumStressBM1)
  d$DiscM1 <- (d$DiscM1 - quantile(d$DiscM1, probs = .25)) / IQR(d$DiscM1)
  d$IneqM1 <- (d$IneqM1 - quantile(d$IneqM1, probs = .25)) / IQR(d$IneqM1)
  d$SocM1 <- (d$SocM1 - quantile(d$SocM1, probs = .25)) / IQR(d$SocM1)
  
  d$CumStressBM2 <- (d$CumStressBM2 - quantile(d$CumStressBM2, probs = .25)) / IQR(d$CumStressBM2)
  d$DiscM2 <- (d$DiscM2 - quantile(d$DiscM2, probs = .25)) / IQR(d$DiscM2)
  d$IneqM2 <- (d$IneqM2 - quantile(d$IneqM2, probs = .25)) / IQR(d$IneqM2)
  d$SocM2 <- (d$SocM2 - quantile(d$SocM2, probs = .25)) / IQR(d$SocM2)
  d$B4QPS_PS <- (d$B4QPS_PS - quantile(d$B4QPS_PS, probs = .25)) / IQR(d$B4QPS_PS)
  d$B1LifeStress <- (d$B1LifeStress - quantile(d$B1LifeStress, probs = .25)) / IQR(d$B1LifeStress)
  
  return(d)
})


################################################################################
##                                                                            ##
##                            Descriptive Statistics                          ##
##                                                                            ##
################################################################################

## follow-up years MIDUS 1
print(mean(dwclean.use$LastAge - dwclean.use$AGEM1), digits = 4)
## follow-up years MIDUS 2
print(mean(dwclean.use$LastAge - dwclean.use$AGEM2), digits = 3)

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
  "AGEM1", "AGEM2", "A1PDEPDX", "B1PDEPDX"),
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





################################################################################
##                                                                            ##
##                     Mortality Model M1 - M2 Automated                      ##
##                                                                            ##
################################################################################

cov.list.m1 <- list(
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK + 
   A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK + 
   A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI + 
   A1PDEPDX + A1PANXTD + A1SCHRON")
cov.list.m2 <- list(
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK + 
   B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK + 
   B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
   B1PDEPDX + B1PANXTD + B1SCHRON")

m1 <- mclapply(c("CumStressBM1", "DiscM1", "IneqM1", "SocM1"), 
               function(v) {
                 runStress(v, cov = cov.list.m1, wave = c(0, 1))
               }, 
               mc.cores = 4L)

cumu.m1 <- runStress("CumStressBM1", cov = cov.list.m1, wave = c(0, 1))
disc.m1 <- runStress("DiscM1", wave = c(0, 1))
ineq.m1 <- runStress("IneqM1", wave = c(0, 1))
soci.m1 <- runStress("SocM1", wave = c(0, 1))


cumu.m2 <- runStress("CumStressBM2", cov = , wave = c(1))

ggplot(data.table(
  Age = rep(disc.m1$NNT$Age, 4),
  Type = rep(c("Cumulative", "Discrimination", "Inequality", "Social Strain"), each = length(disc.m1$NNT$Age)),
  NNT = c(
    cumu.m1$NNT$NNT,
    disc.m1$NNT$NNT,
    ineq.m1$NNT$NNT,
    soci.m1$NNT$NNT))[Age>=50],
       aes(Age, NNT, linetype = Type)) + 
  geom_line() + 
  scale_y_continuous("Number Needed to Treat", breaks = c(10, 25, 50, 100, 200, 400, 600, 800, 1000)) + 
  theme(legend.position = c(0, .15), legend.key.width = unit(1.5, "cm")) + 
  coord_trans(y = "log10", 
              limx = c(50, 87), 
              limy = c(10, 1000)) + 
  # scale_y_log10() +
  NULL




NNT <- function(est) {
  p <- 1 - est[, 1]
  p1 <- 1 - est[, 2]
  1 / (p - p1)
}

runStress <- function(stressvar, cov, wave = c(0, 1)) {

m <- coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)), 
    data = subset(dwclean.use, PAfterM2 %in% wave))

hr <- data.table(
  Model = "Complete Case, Model 1",
  Order = 1,
  HR = exp(coef(m)),
  LL = exp(confint(m))[,1],
  UL = exp(confint(m))[,2],
  P = coef(summary(m))[, "Pr(>|z|)"])

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)), 
    data = dat)
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 1",
  Order = 2, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = "gaussian",
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[1]])))))
  
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 2 MSCM",
  Order = 3, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[1]], stressvar)), data = dat)
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 2 Covariates",
  Order = 4, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = "gaussian",
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[2]])))))
  
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 3 MSCM",
  Order = 5, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[2]], stressvar)), data = dat)
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 3 Covariates",
  Order = 6, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = "gaussian",
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])))))
  
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 4 MSCM",
  Order = 7, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[3]], stressvar)), data = dat)
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 4 Covariates",
  Order = 8, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = "gaussian",
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])))))
  
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[3]], stressvar)), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr <- rbind(hr, data.table(
  Model = "Imputed, Model 4 MSCM + Covariates",
  Order = 9, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


p <- ggplot(hr, aes(Order)) + 
  geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
  scale_x_continuous(breaks = 1:9, labels = hr$Model) + 
  geom_hline(yintercept = 1, colour = "grey50") + 
  coord_flip() + 
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank())

## Effect Size
temp <- lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = "gaussian",
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])))))
  
  m <- coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ %s", stressvar)), 
    data = dat, weights = winsorizor(w$ipw.weights, .01),
    ties = "breslow")
  
  ps <- eval(substitute(
    stdCoxph(m, data = as.data.frame(dat), 
                 X = stressvar, 
                 x = c(NA, median(dat[[stressvar]])), 
                 t = 49:88, clusterid = "M2FAMNUM", 
                 subsetnew = EXP > quantile(EXP, probs = .75)),
    list(EXP = as.symbol(stressvar))))
  NNT(ps$est)
})

nnt.m4 <- data.table(
  Age = 49:88, 
  NNT = Reduce(`+`, temp)/50)

return(list(
  HRs = hr,
  Plot = p,
  NNT = nnt.m4))
}



################################################################################
##                                                                            ##
##                            Mortality Model M1                              ##
##                                                                            ##
################################################################################

detach(package:caret)

m <- coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM1, 
           data = dwclean.use)

hr.m1 <- data.table(
  Model = "Complete Case, Model 1",
  Order = 1,
  HR = exp(coef(m)),
  LL = exp(confint(m))[,1],
  UL = exp(confint(m))[,2],
  P = coef(summary(m))[, "Pr(>|z|)"])

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM1, data = dat)
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 1",
  Order = 2, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM1,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + A1PB1 + A1PBWORK,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM1, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 2 MSCM",
  Order = 3, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + A1PB1 + A1PBWORK + 
          CumStressBM1, data = dat)
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 2 Covariates",
  Order = 4, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))




m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM1,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + A1PB1 + A1PBWORK + 
      A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM1, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 3 MSCM",
  Order = 5, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + A1PB1 + A1PBWORK + 
          A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI + 
          CumStressBM1, data = dat)
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 3 Covariates",
  Order = 6, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM1,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + A1PB1 + A1PBWORK + 
      A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI + 
      A1PDEPDX + A1PANXTD + A1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM1, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 4 MSCM",
  Order = 7, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + A1PB1 + A1PBWORK + A1Smoke + A1WorstAlcohol + A1PhysAct + 
          A1SWSTHI + A1PDEPDX + A1PANXTD + A1SCHRON + 
          CumStressBM1, data = dat)
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 4 Covariates",
  Order = 8, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM1,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + A1PB1 + A1PBWORK + A1Smoke + A1WorstAlcohol + A1PhysAct + 
      A1SWSTHI + A1PDEPDX + A1PANXTD + A1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + A1PB1 + A1PBWORK + A1Smoke + A1WorstAlcohol + A1PhysAct + 
          A1SWSTHI + A1PDEPDX + A1PANXTD + A1SCHRON + 
          CumStressBM1, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m1 <- rbind(hr.m1, data.table(
  Model = "Imputed, Model 4 MSCM + Covariates",
  Order = 9, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


ggplot(hr.m1, aes(Order)) + 
  geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
  scale_x_continuous(breaks = 1:9, labels = hr.m1$Model) + 
  geom_hline(yintercept = 1, colour = "grey50") + 
  coord_flip() + 
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank())



## Effect Size

NNT <- function(est) {
  p <- 1 - est[, 1]
  p1 <- 1 - est[, 2]
  1 / (p - p1)
}

temp <- lapply(dwcleanimp, function(dat) {
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM1,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + A1PB1 + A1PBWORK + 
      A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI + 
      A1PDEPDX + A1PANXTD + A1SCHRON,
    data = dat)
  
  m <- coxph(Surv(LastAge, DECEASED) ~ CumStressBM1, 
             data = dat, weights = winsorizor(w$ipw.weights, .01),
             ties = "breslow")
  
  ps <- stdCoxph(m, data = as.data.frame(dat), 
                 X = "CumStressBM1", 
                 x = c(NA, median(dat$CumStressBM1)), 
                 t = 49:87, clusterid = "M2FAMNUM", 
                 subsetnew = CumStressBM1 > quantile(CumStressBM1, probs = .75))
  NNT(ps$est)
})

ggplot(data.table(
  Age = 49:87,
  NNT = Reduce(`+`, temp)/50),
  aes(Age, NNT)) + 
  geom_line()


################################################################################
##                                                                            ##
##                            Mortality Model M2                              ##
##                                                                            ##
################################################################################

m <- coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM2, 
           data = dwclean.use[PAfterM2 == 1])
hr.m2 <- data.table(
  Model = "Complete Case, Model 1",
  Order = 0,
  HR = exp(coef(m)),
  LL = exp(confint(m))[,1],
  UL = exp(confint(m))[,2],
  P = coef(summary(m))[, "Pr(>|z|)"])

m <- coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBaltM2, 
           data = dwclean.use[PAfterM2 == 1])
hr.m2 <- rbind(hr.m2, data.table(
  Model = "Complete Case Alt, Model 1",
  Order = 1,
  HR = exp(coef(m)),
  LL = exp(confint(m))[,1],
  UL = exp(confint(m))[,2],
  P = coef(summary(m))[, "Pr(>|z|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM2, data = dat)
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 1",
  Order = 2, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + B1PB1 + B1PBWORK,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM2, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 2 MSCM",
  Order = 3, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + B1PB1 + B1PBWORK + 
          CumStressBM2, data = dat)
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 2 Covariates",
  Order = 4, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + B1PB1 + B1PBWORK + 
      B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM2, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 3 MSCM",
  Order = 5, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + B1PB1 + B1PBWORK + 
          B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
          CumStressBM2, data = dat)
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 3 Covariates",
  Order = 6, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + B1PB1 + B1PBWORK + 
      B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
      B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + CumStressBM2, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 4 MSCM",
  Order = 7, 
  HR = exp(m[, "est"]),
  LL = exp(m[, "lo 95"]),
  UL = exp(m[, "hi 95"]),
  P = m[, "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + 
          B1SWSTHI + B1PDEPDX + B1PANXTD + B1SCHRON + 
          CumStressBM1, data = dat)
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 4 Covariates",
  Order = 8, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + 
      B1SWSTHI + B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + 
          m1welf_all + B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + 
          B1SWSTHI + B1PDEPDX + B1PANXTD + B1SCHRON + 
          CumStressBM2, 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

hr.m2 <- rbind(hr.m2, data.table(
  Model = "Imputed, Model 4 MSCM + Covariates",
  Order = 9, 
  HR = exp(m[nrow(m), "est"]),
  LL = exp(m[nrow(m), "lo 95"]),
  UL = exp(m[nrow(m), "hi 95"]),
  P = m[nrow(m), "Pr(>|t|)"]))


ggplot(hr.m2, aes(Order)) + 
  geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
  scale_x_continuous(breaks = 0:9, labels = hr.m2$Model) + 
  geom_hline(yintercept = 1, colour = "grey50") + 
  coord_flip() + 
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank())



plot_grid(
  ggplot(hr.m1, aes(Order)) + 
    geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
    scale_x_continuous(breaks = 0:9, labels = c("", paste0(hr.m1$Model, ", ", formatPval(hr.m1$P, includeP = TRUE)))) + 
    geom_hline(yintercept = 1, colour = "grey50") + 
    coord_flip(ylim = c(1, 1.5), xlim = c(0, 9)) + 
    theme(axis.line = element_blank(),
          axis.ticks.y = element_blank()),
  ggplot(hr.m2, aes(Order)) + 
    geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
    scale_x_continuous(breaks = 0:9, labels = paste0(hr.m2$Model, ", ", formatPval(hr.m2$P, includeP = TRUE))) + 
    geom_hline(yintercept = 1, colour = "grey50") + 
    coord_flip(ylim = c(1, 1.5), xlim = c(0, 9)) + 
    theme(axis.line = element_blank(),
          axis.ticks.y = element_blank()), ncol = 2)


write.csv(
  hr.m1[, .(
    Model = Model,
    HR = format(round(HR, 2), nsmall = 2),
    LL = format(round(LL, 2), nsmall = 2),
    UL = format(round(UL, 2), nsmall = 2),
    P = formatPval(P))],
  file = file.path(dir.proj, "hazardratios_midus1.csv"),
  row.names = FALSE)

write.csv(hr.m2[, .(
  Model = Model,
  HR = format(round(HR, 2), nsmall = 2),
  LL = format(round(LL, 2), nsmall = 2),
  UL = format(round(UL, 2), nsmall = 2),
  P = formatPval(P))],
  file = file.path(dir.proj, "hazardratios_midus2.csv"),
  row.names = FALSE)


temp2 <- lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + 
      m1welf_all + B1PB1 + B1PBWORK + 
      B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
      B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  
  m <- coxph(Surv(LastAge, DECEASED) ~ CumStressBM2, 
             data = dat, weights = winsorizor(w$ipw.weights, .01),
             ties = "breslow")
  
  ps <- stdCoxph(m, data = as.data.frame(dat), 
                 X = "CumStressBM2", 
                 x = c(NA, median(dat$CumStressBM2)), 
                 t = 49:87, clusterid = "M2FAMNUM", 
                 subsetnew = CumStressBM2 > quantile(CumStressBM2, probs = .75))
  NNT(ps$est)
})

nnt.m1m2 <- data.table(
  Age = c(49:87, 49:87),
  NNT = c(Reduce(`+`, temp)/50, Reduce(`+`, temp2)/50), 
  Wave = rep(c("MIDUS 1", "MIDUS 2"), each = 39))

ggplot(nnt.m1m2[Age>=50],
       aes(Age, NNT, linetype = Wave)) + 
  geom_line() + 
  scale_y_continuous("Number Needed to Treat", breaks = c(10, 25, 50, 100, 200, 400, 600, 800)) + 
  theme(legend.position = c(0, .1)) + 
  coord_trans(y = "log10", 
              limx = c(50, 87), 
              limy = c(10, 850)) + 
  # scale_y_log10() +
  NULL

























summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + 
          scale(CumStressBM2), data = dat)
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + m1welf_all +
      B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
      B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + scale(CumStressBM2), 
        data = dat, weights = winsorizor(w$ipw.weights, .00))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + m1welf_all +
      B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
      B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + scale(CumStressBM2), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all +
          B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
          B1PDEPDX + B1PANXTD + B1SCHRON +
          scale(CumStressBM2), data = subset(dat, PAfterM2 == 1))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  dat <- subset(dat, PAfterM2 == 1)
  ## weights
  w <- ipwpoint(
    exposure = CumStressBM2,
    family = "gaussian",
    numerator = ~ 1,
    denominator = ~ Sex + RaceG3 + m1ped3_all + m1welf_all +
      B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
      B1PDEPDX + B1PANXTD + B1SCHRON,
    data = dat)
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all +
          B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + 
          B1PDEPDX + B1PANXTD + B1SCHRON +
          scale(CumStressBM2), 
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))))







coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + scale(CumStressBM2), 
      data = dwclean.use)

coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + scale(CumStressBaltM2), 
      data = dwclean.use)






summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 +
          A1PBWORK + A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI +
          A1SCHRON +
          I(CumStressM1), data = dat)
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 +
          A1PBWORK + A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI +
          A1SCHRON +
          I(CumStressQ75M1/3), data = dat)
}))))


summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + I(CumStressM2/3), data = subset(dat, PAfterM2 == 1))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + I(CumStressBM2/3), data = subset(dat, PAfterM2 == 1))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all +
          B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI + B1PDEPDX + B1PANXTD +
          ##B1SCHRON +
          scale(CumStressBM2), data = subset(dat, PAfterM2 == 1))
}))))

summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  coxph(Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + Sex + RaceG3 + m1ped3_all + m1welf_all +
          B1PB1 + B1PBWORK + B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI +
          B1SCHRON +
          I(CumStressQ75M2/3), data = subset(dat, PAfterM2 == 1))
}))))


m1.stress.vars <- lapply(m1.stress.vars, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
m2.stress.vars.full <- lapply(m2.stress.vars.full, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")
# m2.stress.vars.reduced <- lapply(m2.stress.vars.reduced, function(x) if (nzchar(x[1])) sprintf("scale(%s)", x) else "")


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

indices2 <- expand.grid(cov = 1:4, iv = 1:7)
indices2 <- subset(expand.grid(cov = 1:4, iv = 1:7), iv > 1 | cov > 1)

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
saveRDS(mortality.coxph.results,
        file = file.path(dir.proj, "mortality_coxph_results.RDS"),
        compress = "xz")


library(rms)


## fully adjusted model all stress variables M1
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

## adjusted model, all stress varaibles M1
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
  coord_cartesian(xlim = c(0, 1.02), ylim = c(-2.5, -1), expand = FALSE) +
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


m2.newdata.predictions <- lapply(1:50, function(i) {
  ddist <- datadist(subset(dwcleanimp[[i]], PAfterM2 == 1))$limits
  lastage <- seq(from = 60, to = 95, length.out = 250)

  tmpnew2 <- tmpnew <- ddist["Adjust to", ]
  tmpnew[, m2.full.stressvars] <- ddist["Low:effect", m2.full.stressvars]
  tmpnew2[, m2.full.stressvars] <- ddist["High:effect", m2.full.stressvars]

  tmpnew <- do.call(rbind, rep(list(tmpnew), 250))
  tmpnew2 <- do.call(rbind, rep(list(tmpnew2), 250))

  tmpnew2$LastAge <- tmpnew$LastAge <- lastage

  tmpnew$Yhat <- 1 - predict(
                       mortality.coxph.results$m2f.full[[28]][[i]],
                       newdata = tmpnew,
                       type = "expected")

  tmpnew2$Yhat <- 1 - predict(
                        mortality.coxph.results$m2f.full[[28]][[i]],
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
  cbind(m1.newdata.predictions.long, Time = "MIDUS 1 (p = .010)"),
  cbind(m2.newdata.predictions.long, Time = "MIDUS 2 (p = .027)"))

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
mortality.coxph.results.m1f.c <- cbind(
  IV = 2:20,
  C = sapply(2:20, function(k) pool.cindex(mortality.coxph.results$m1f[[k]])))

modindex <- c(5, 9, 13, 17)

mortality.coxph.results.m1f.multivariate <- with(mortality.coxph.results,
  rbind(
    cbind(Model = "1", as.data.table(do.call(rbind, lapply(modindex, function(i){
      pool.compare.null(as.mira(m1f[[i]]))
    })))),
    cbind(Model = "2", as.data.table(do.call(rbind, lapply(modindex + 1, function(i){
      pool.compare(as.mira(m1f[[i]]),  as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    })))),
    cbind(Model = "3", as.data.table(do.call(rbind, lapply(modindex + 2, function(i){
      pool.compare(as.mira(m1f[[i]]),  as.mira(m1f[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    })))),
    cbind(Model = "4", as.data.table(do.call(rbind, lapply(modindex + 3, function(i){
      pool.compare(as.mira(m1f[[i]]),  as.mira(m1f[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    }))))
  ))

mortality.coxph.results.m1f.mcoef <- with(mortality.coxph.results, {
  tmp <- do.call(rbind, lapply(modindex + 0, 
                               function(i) format.mortality(m1f[[i]])))
  cbind(tmp,
        Est2 = do.call(rbind, lapply(modindex + 1, 
          function(i) format.mortality(m1f[[i]])))[IV %in% tmp$IV, Est],
        Est3 = do.call(rbind, lapply(modindex + 2, 
          function(i) format.mortality(m1f[[i]])))[IV %in% tmp$IV, Est],
        Est4 = do.call(rbind, lapply(modindex + 3, 
          function(i) format.mortality(m1f[[i]])))[IV %in% tmp$IV, Est])
})

write.xlsx(mortality.coxph.results.m1f.mcoef,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M1 Full",
  row.names = FALSE, showNA=FALSE, append = FALSE)

write.xlsx(
  mortality.coxph.results.m1f.multivariate,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M1 Multi",
  row.names = FALSE, showNA=FALSE, append = TRUE)

write.xlsx(
  mortality.coxph.results.m1f.c,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M1 C Indices",
  row.names = FALSE, showNA=FALSE, append = TRUE)

###### MIDUS 2 Full Results ######

mortality.coxph.results.m2f.full.c <- cbind(
  IV = 2:28,
  C = sapply(2:28, function(k) pool.cindex(mortality.coxph.results$m2f.full[[k]])))

modindex <- c(5, 9, 13, 17, 21, 25)

mortality.coxph.results.m2f.full.multivariate <- with(mortality.coxph.results,
  rbind(
    cbind(Model = "1", as.data.table(do.call(rbind, lapply(modindex, function(i){
       pool.compare.null(as.mira(m2f.full[[i]]))
     })))),
    cbind(Model = "2", as.data.table(do.call(rbind, lapply(modindex + 1, function(i){
       pool.compare(as.mira(m2f.full[[i]]),  as.mira(m2f.full[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    })))),
    cbind(Model = "3", as.data.table(do.call(rbind, lapply(modindex + 2, function(i){
      pool.compare(as.mira(m2f.full[[i]]),  as.mira(m2f.full[[3]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    })))),
    cbind(Model = "4", as.data.table(do.call(rbind, lapply(modindex + 3, function(i){
      pool.compare(as.mira(m2f.full[[i]]),  as.mira(m2f.full[[4]]))[c("Dm", "rm", "df1", "df2", "pvalue")]
    }))))
))

mortality.coxph.results.m2f.full.mcoef <- with(mortality.coxph.results, {
  tmp <- do.call(rbind, lapply(modindex + 0, 
         function(i) format.mortality(m2f.full[[i]])))
  cbind(tmp,
    Est2 = do.call(rbind, lapply(modindex + 1, 
    function(i) format.mortality(m2f.full[[i]])))[IV %in% tmp$IV, Est],
    Est3 = do.call(rbind, lapply(modindex + 2, 
    function(i) format.mortality(m2f.full[[i]])))[IV %in% tmp$IV, Est],
    Est4 = do.call(rbind, lapply(modindex + 3, 
    function(i) format.mortality(m2f.full[[i]])))[IV %in% tmp$IV, Est])
  })


write.xlsx(mortality.coxph.results.m2f.full.mcoef,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M2 Full",
  row.names = FALSE, showNA=FALSE, append = TRUE)

write.xlsx(
  mortality.coxph.results.m2f.full.multivariate,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M2 Full Multi",
  row.names = FALSE, showNA=FALSE, append = TRUE)

write.xlsx(
  mortality.coxph.results.m2f.full.c,
  file = file.path(dir.proj, "aim1_stress_mortality_v2.xlsx"),
  sheetName = "M2 Full C Indices",
  row.names = FALSE, showNA=FALSE, append = TRUE)


################################################################################
##                                                                            ##
##                        Mortality RF Survival Model                         ##
##                                                                            ##
################################################################################

#### Fit the models on AWS ####

if (FALSE) {
  library(checkpoint)
  checkpoint("2018-01-01", R.version = "3.4.3", 
             checkpointLocation = "~/EncDB",
             scanForPackages = FALSE)
  library(data.table)
  # library(randomForest)
  library(randomForestSRC)
  # library(ggRandomForests)
  library(pec)
  library(parallel)

  ## unadjusted, M1
  setMKLthreads(n = 1L)
  options(rf.cores = 4L)
  options(mc.cores = 4L)
  
  ntimepts <- as.vector(quantile(dwclean.use$LastAge, 
                                 probs = seq(from = 0, to = 1, 
                                             length.out = 40)))
  
  
  rfs.res1 <- lapply(1:25, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices$m1f[12]), . ~ . - cluster(M2FAMNUM)),
          data = dwcleanimp[[i]],
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = 12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 2, mc.allow.recursive = FALSE)
  saveRDS(rfs.res1, file = file.path(dir.proj, "aim1/rfs_res1.RDS"))
  rm(rfs.res1); gc()
  
  rfs.res1b <- lapply(26:50, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices$m1f[12]), . ~ . - cluster(M2FAMNUM)),
          data = dwcleanimp[[i]],
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = 12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 2, mc.allow.recursive = FALSE)
  saveRDS(rfs.res1b, file = file.path(dir.proj, "aim1/rfs_res1b.RDS"))
  rm(rfs.res1b); gc()
  
  
  ## fully adjusted, M1
  rfs.res4 <- lapply(1:18, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices$m1f[19]), . ~ . - cluster(M2FAMNUM)),
          data = dwcleanimp[[i]],
          ntree = 1000, nsplit = 10,
          ntime = ntimepts, nodesize = 5,
          seed = 12345L, split.depth = "by.tree",
          importance = "permute", forest = TRUE)
  })
  saveRDS(rfs.res4, file = file.path(dir.proj, "aim1/rfs_res4.RDS"))
  rm(rfs.res4); gc()
  rfs.res4b <- lapply(19:36, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices$m1f[19]), . ~ . - cluster(M2FAMNUM)),
          data = dwcleanimp[[i]],
          ntree = 1000, nsplit = 10,
          ntime = ntimepts, nodesize = 5,
          seed = 12345L, split.depth = "by.tree",
          importance = "permute", forest = TRUE)
  })
  saveRDS(rfs.res4b, file = file.path(dir.proj, "aim1/rfs_res4b.RDS"))
  rm(rfs.res4b); gc()
  rfs.res4c <- lapply(37:50, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices$m1f[19]), . ~ . - cluster(M2FAMNUM)),
          data = dwcleanimp[[i]],
          ntree = 1000, nsplit = 10,
          ntime = ntimepts, nodesize = 5,
          seed = 12345L, split.depth = "by.tree",
          importance = "permute", forest = TRUE)
  })
  saveRDS(rfs.res4c, file = file.path(dir.proj, "aim1/rfs_res4c.RDS"))
  rm(rfs.res4c); gc()  
  
  ## unadjusted M2
  rfs.m2.res1 <- lapply(1:18, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[15]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4, mc.allow.recursive = FALSE)
  saveRDS(rfs.m2.res1, file = file.path(dir.proj, "aim1/rfs_m2_res1.RDS"))
  rm(rfs.m2.res1); gc()
  rfs.m2.res1b <- lapply(19:36, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[15]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4, mc.allow.recursive = FALSE)
  saveRDS(rfs.m2.res1b, file = file.path(dir.proj, "aim1/rfs_m2_res1b.RDS"))
  rm(rfs.m2.res1b); gc()
  rfs.m2.res1c <- lapply(37:50, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[15]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4, mc.allow.recursive = FALSE)
  saveRDS(rfs.m2.res1c, file = file.path(dir.proj, "aim1/rfs_m2_res1c.RDS"))
  rm(rfs.m2.res1c); gc()
  
  rfs.m2.res4 <- lapply(1:18, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[17]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4)
  saveRDS(rfs.m2.res4, file = file.path(dir.proj, "aim1/rfs_m2_res4.RDS"))
  rm(rfs.m2.res4); gc()
  rfs.m2.res4b <- lapply(19:36, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[17]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4)
  saveRDS(rfs.m2.res4b, file = file.path(dir.proj, "aim1/rfs_m2_res4b.RDS"))
  rm(rfs.m2.res4b); gc()
  rfs.m2.res4c <- lapply(37:50, function(i) {
    set.seed(12345)
    rfsrc(update(as.formula(indices2$m2f.full[17]), . ~ . - cluster(M2FAMNUM)),
          data = subset(dwcleanimp[[i]], PAfterM2 == 1),
          ntree = 1000, nsplit = 10,
          ntime = 50, nodesize = 5,
          seed = -12345L,
          importance = "permute", forest = TRUE)
  })#, mc.cores = 4)
  saveRDS(rfs.m2.res4c, file = file.path(dir.proj, "aim1/rfs_m2_res4c.RDS"))
  rm(rfs.m2.res4c); gc()  

  rfs.res1 <- readRDS(file.path(dir.proj, "aim1/rfs_res1.RDS"))
  rfs.res1b <- readRDS(file.path(dir.proj, "aim1/rfs_res1b.RDS"))
  saveRDS(c(rfs.res1, rfs.res1b), 
          file.path(dir.proj, "aim1/rfs_res1_all.RDS"), 
          compress = "xz")

  rfs.res4 <- readRDS(file.path(dir.proj, "aim1/rfs_res4.RDS"))
  rfs.res4b <- readRDS(file.path(dir.proj, "aim1/rfs_res4b.RDS"))
  rfs.res4c <- readRDS(file.path(dir.proj, "aim1/rfs_res4c.RDS"))
  saveRDS(c(rfs.res4, rfs.res4b, rfs.res4c), 
          file.path(dir.proj, "aim1/rfs_res4_all.RDS"), 
          compress = "xz")

  
  rfs.m2.res1 <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res1.RDS"))
  rfs.m2.res1b <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res1b.RDS"))
  rfs.m2.res1c <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res1c.RDS"))
  saveRDS(c(rfs.m2.res1, rfs.m2.res1b, rfs.m2.res1c), 
          file.path(dir.proj, "aim1/rfs_m2_res1_all.RDS"), 
          compress = "xz")

  rfs.m2.res4 <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res4.RDS"))
  rfs.m2.res4b <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res4b.RDS"))
  rfs.m2.res4c <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res4c.RDS"))
  saveRDS(c(rfs.m2.res4, rfs.m2.res4b, rfs.m2.res4c), 
          file.path(dir.proj, "aim1/rfs_m2_res4_all.RDS"), 
          compress = "xz")
  
} else {
  rfs.res1 <- readRDS(file.path(dir.proj, "aim1/rfs_res1_all.RDS"))
  rfs.res4 <- readRDS(file.path(dir.proj, "aim1/rfs_res4.RDS"))
  rfs.m2.res1 <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res1.RDS"))
  rfs.m2.res4 <- readRDS(file.path(dir.proj, "aim1/rfs_m2_res4.RDS"))

}

tanh(summary(atanh(1 - sapply(rfs.res1, function(x) x$err.rate[1000]))))
summary(1 - sapply(rfs.res1, function(x) x$err.rate[1000]))


rfs.res1.mimp <- pmax(sort(rowMeans(sapply(rfs.res1,
  function(x) x$importance/max(x$importance)))), -Inf)
rfs.res1.mimp <- data.table(V = names(rfs.res1.mimp),
  "MIDUS 1 - Model 1" = as.vector(rfs.res1.mimp))

###### Adjusted Model
tanh(summary(atanh(1 - sapply(rfs.res4, function(x) x$err.rate[1000]))))
summary(1 - sapply(rfs.res4, function(x) x$err.rate[1000]))


rfs.res4.mimp <- pmax(sort(rowMeans(sapply(rfs.res4,
  function(x) x$importance/max(x$importance)))), 0)
rfs.res4.mimp <- data.table(V = names(rfs.res4.mimp),
  "MIDUS 1 - Model 4" = as.vector(rfs.res4.mimp))

rfs.resall.mimp <- melt(merge(rfs.res1.mimp, rfs.res4.mimp, by = "V", all = TRUE),
     id.vars = "V", variable.name = "Model")

rfs.resall.mimp <- merge(rfs.resall.mimp, rbind(stresskey2[, c("Var", "Label2")],
     data.frame(Var = c("A1PB1", "A1PBWORK", "A1PhysAct", "A1SWSTHI", "A1Smoke",
                        "A1WorstAlcohol", "RaceG3", "m1ped3_all", "m1welf_all"),
                Label2 = c("Education", "Employed", "Physical Activity",
                           "Waist-to-Hip Ratio", "Smoking Status",
                           "Worst Alcohol", "Race", "Parent Education",
                           "On Welfare in Childhood"))),
     by.x = "V", by.y = "Var", all.x = TRUE)

rfs.resall.mimp[, Label2 := gsub("\\s*$", "\\2", Label2)]

rfs.resall.mimp[, V := factor(V, levels = rfs.resall.mimp[Model=="MIDUS 1 - Model 4", max(value, na.rm = TRUE), by = V][order(V1), V])]
rfs.resall.mimp[, Label2 := factor(Label2, levels = rfs.resall.mimp[Model=="MIDUS 1 - Model 4", max(value, na.rm = TRUE), by = Label2][order(V1), Label2])]


rfs.resall.pimp <- ggplot(rfs.resall.mimp, aes(Label2, value)) +
  geom_bar(, stat = "identity", width = .1, fill = "grey60") +
  geom_point(, size = 2) + facet_wrap(~Model) +
  coord_flip(ylim = c(-.02, 1.02), xlim = c(.7, 18.3), expand=FALSE) +
  scale_y_continuous(labels = percent) +
  theme_cowplot() + theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = .5)) +
  xlab("") + ylab("Relative Variable Importances")


###### Unadjusted Model M2
tanh(summary(atanh(1 - sapply(rfs.m2.res1, function(x) x$err.rate[1000]))))


rfs.m2.res1.mimp <- pmax(sort(rowMeans(sapply(rfs.m2.res1, function(x) x$importance/max(x$importance)))), 0)
rfs.m2.res1.mimp <- data.table(V = names(rfs.m2.res1.mimp), "MIDUS 2 - Model 1" = as.vector(rfs.m2.res1.mimp))


###### Adjusted Model M2
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
