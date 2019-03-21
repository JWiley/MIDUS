library(checkpoint)
checkpoint("2016-07-07", R.version = "3.2.5", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)

library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(JWileymisc)
library(pscore)
library(MplusAutomation)
library(texreg)
library(foreign)
library(survival)
library(data.table)
## library(mgcv)


load("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data.RData")
base <- "c:/Users/Joshua/OneDrive/Projects/MIDUS/JW-MIDUS-AL"
base <- "c:/Users/jwile/OneDrive/Projects/MIDUS/JW-MIDUS-AL"

## source("~/Google Drive/RCode/utilities.R")
n <- function(x) as.numeric(as.character(x))
z <- function(x) as.vector(scale(x))

usev <- c("M2ID", "M2FAMNUM", "B1PAGE_M2", "Sex", "RaceG3",
          "B1NegAff", "B1PosAff", "m1chadv", "m2aadv", "B1PR", "B1SR",
          "B1Smoke", "B1CurrentAlcohol", "B1SC1", "B1Sleep", "B1SSQ",
          "B1LifeStress", "B1PhysAct",
          "B4F", "B4bi.sym", "B4bi.hpa", "B4bi.card", "B4bi.gluc", "B4bi.lipid", "B4bi.infl", "B4bi.para",
          "B1SCHRON", "C1SCHRON")

dwimp2 <- lapply(dwimp, function(d) {
  d <- copy(d[!is.na(B1SCHRON) | !is.na(C1SCHRON) | !is.na(B1Smoke) |
    !is.na(B1CurrentAlcohol) | !is.na(m1chadv) | !is.na(B1PhysAct) |
    !is.na(B1Sleep) | !is.na(B1SSQ) | !is.na(B1LifeStress) |
    !is.na(B1SC1)][, usev, with = FALSE])

  d[, B1Sleep := as.vector(winsorizor(B1Sleep, .01))]
  d[, B1LifeStress := as.vector(winsorizor(B1LifeStress, .01))]
  d[, B1PhysAct := as.vector(winsorizor(B1PhysAct, .01))]
  d[, B1SCHRON := as.vector(winsorizor(B1SCHRON, .01))]
  d[, C1SCHRON := as.vector(winsorizor(C1SCHRON, .01))]

  d[, Sex := factor(Sex, levels = 1:2)]
  d[, B1Smoke := as.integer(factor(B1Smoke, levels = c("never", "past", "current")))]
  d[, B1CurrentAlcohol := as.integer(factor(B1CurrentAlcohol, levels = c("None", "Moderate", "High")))]
  d[, B1SC1 := factor(B1SC1, levels = 1:2)]
  ## d[, B1LifeStress := factor(B1LifeStress, levels = 0:12, ordered = TRUE)]
  ## d[, B1SCHRON := factor(B1SCHRON, levels = 0:11, ordered = TRUE)]
  ## d[, C1SCHRON := factor(C1SCHRON, levels = 0:14, ordered = TRUE)]

  return(as.data.frame(d))
})

str(dwimp2[[1]])

rm(dwlimp)
rm(dwimp)
gc()

if (FALSE) {
seeds <- c(-1226005728L, 1751625633L, 1342183529L, -583252729L, -1112245536L,
-41916953L, -213638468L, -1125333389L, 1775951578L, -1797755070L,
-977714264L, 738401316L, -803312477L, 1300503099L, 1505697181L,
-1744750013L, -1191727168L, 226266328L, -903637861L, 1122111946L,
-1093654340L, -908151236L, -1542131728L, 712837057L, -1964088648L,
419458900L, 948236076L, 728038566L, -1087743534L, -1346240534L,
1810822272L, 1160497026L, 93604826L, -1974790688L, -277938328L,
1502711951L, -223446146L, 410671500L, -244321421L, -1765858680L,
540973963L, -1217792818L, 1793994722L, 627312336L, 1253686421L,
-1789327279L, -1967771486L, 973391592L, -1469498064L, -2015266711L)

pmat <- (1 - diag(1, ncol(dwimp2[[1]])))
colnames(pmat) <- rownames(pmat) <- colnames(dwimp2[[1]])
pmat[, 1:2] <- 0
pmat[1:2, ] <- 0

library(parallel)
library(mice)
library(randomForest)
cl <- makeCluster(10)
clusterEvalQ(cl, {
  library(mice)
  library(randomForest)
})
clusterExport(cl, c("dwimp2", "seeds", "pmat"))

impres <- parLapply(cl, 1:50, function(i) {
  set.seed(seeds[i])
  complete(mice(
    data = dwimp2[[i]],
    m = 1,
    predictorMatrix = pmat,
    method = "rf",
    maxit = 25,
    seed = seeds[i],
    ntrees = 20,
    nodesize = 15), 1)
})
save(impres, file = "midus_rcm_imputed.RData")
}

load("midus_rcm_imputed.RData")

## table(dwimp2[[1]]$B1CurrentAlcohol, useNA = "always")
## table(complete(impres[[1]], 1)$B1CurrentAlcohol, useNA = "always")
## str(complete(impres[[1]], 1))
## table(colSums(is.na(complete(impres[[5]], 1))))


dwimp3 <- lapply(impres, function(d) {
  d <- as.data.table(d)
  d[, B1Smoke := factor(B1Smoke, levels = 1:3, labels = c("never", "past", "current"))]
  d[, B1CurrentAlcohol := factor(B1CurrentAlcohol, levels = 1:3, labels = c("None", "Moderate", "High"))]

  d[, B1PAGE_M2 := (B1PAGE_M2 - 55) / 10]
  d[, Female := as.integer(Sex == "2")]
  d[, B1NegAff := z(B1NegAff * -1)]
  d[, B1PosAff := z(B1PosAff * -1)]
  d[, m1chadv := z(m1chadv)]
  d[, m2aadv := z(m2aadv)]
  d[, dSES := z(m2aadv - m1chadv)]
  d[, B1PR := z(B1PR)]
  d[, B1SR := z(B1SR)]
  d[, B1PSR := z(B1PR + B1SR)]
  d[, B1SmokePast := as.integer(B1Smoke == "past")]
  d[, B1SmokeCurrent := as.integer(B1Smoke == "current")]
  d[, B1AlcoholHeavy := as.integer(B1CurrentAlcohol == "High")]
  d[, B1AlcoholModerate := as.integer(B1CurrentAlcohol == "Moderate")]
  d[, B1SC1 := as.integer(B1SC1 == "1")]
  d[, B1Sleep := z(B1Sleep)]
  d[, B1SSQ := z(B1SSQ)]

  d[, B1PhysAct := z(B1PhysAct)]
  d[, B4F := z(B4F)]
  d[, B4bi.sym := z(B4bi.sym)]
  d[, B4bi.hpa := z(B4bi.hpa)]
  d[, B4bi.card := z(B4bi.card)]
  d[, B4bi.gluc := z(B4bi.gluc)]
  d[, B4bi.lipid := z(B4bi.lipid)]
  d[, B4bi.infl := z(B4bi.infl)]
  d[, B4bi.para := z(B4bi.para)]
  d[, RAA := as.integer(RaceG3 == "AA")]
  d[, ROther := as.integer(RaceG3 == "Other")]

  # dropping dummy coded factors
  d[, Sex := NULL]
  d[, B1Smoke := NULL]
  d[, B1CurrentAlcohol := NULL]
  d[, RaceG3 := NULL]
return(d)
})

t(dwimp3[[1]][, lapply(.SD, function(x) mean(!is.na(x)))])

## summary(m <- gam(C1SCHRON ~ B1SCHRON + B1PAGE_M2 + Female + B1SC1 + RAA + ROther +
##                    B1SmokePast + B1SmokeCurrent +
##                    B1AlcoholHeavy + B1AlcoholModerate +
##                    B1PhysAct +
##                    B1SSQ + I(B1SSQ^2) + B1Sleep + I(B1Sleep^2) +
##                    B1LifeStress + B1NegAff + B1PSR + (B1PAGE_M2 + Female) * B1PSR +
##                    (m1chadv + dSES) * B1PSR + B4F * B1PSR, data = dwimp2[[1]], family = poisson()))

## plot(m)

## summary(m <- gam(C1SCHRON ~ B1SCHRON + B1PAGE_M2 + Female + B1SC1 +
##                    B1SmokePast + B1SmokeCurrent +
##                    B1AlcoholHeavy + B1AlcoholModerate +
##                    s(B1SSQ, k = 5) +
##                    B1Sleep + I(B1Sleep^2), data = dwimp2[[1]], family = poisson()))
## plot(m)


## summary(m <- gam(C1SCHRON ~ B1SCHRON + s(B1Sleep, k = 5), data = dwimp2[[1]], family = poisson()))
## plot(m)

## summary(m <- gam(C1SCHRON ~ B1SCHRON + B1Sleep + I(B1Sleep^2), data = dwimp2[[1]], family = poisson()))
## plot(m)

## egltable(c("b1pgender", "b1page_m2", "m1chadv", "dSES", "PSQI3", "CurrentSmoke", "RiskyBehaviors", "CurrentAlcohol", "PhysAct", "OverallDiet"),
##          data = within(p4final[[1]], {
##                                 b1page_m2 <- b1page_m2 * 10 + 55
##                                 PhysAct <- factor(PhysAct)
##                                       }))
## round(quantile(p4final[[1]]$LifeStress, probs = c(.5, .25, .75), na.rm = TRUE), 2)
## round(quantile(p4final[[1]]$p4majorconditions, probs = c(.5, .25, .75), na.rm = TRUE), 2)
## ldat <- do.call(rbind, p4final)
## egltable(c("PSR", "NegAff", "F"), data = ldat)

## p4final <- lapply(p4final, function(d) {
##   d$NegAff <- d$NegAff * -1
##   d$PosAff <- d$PosAff * -1
##   d$b1page_m2 <- (d$b1page_m2) / 10;
##   d$m1chadv <- as.vector((d$m1chadv))
##   d$m2aadv <- as.vector(scale(d$m2aadv)) - as.vector(scale(d$m1chadv))
##   d$b1pgender <- as.integer(d$b1pgender) - 1
##   d$Smoke <- as.integer(d$Smoke) - 1
##   d$PhysAct <- as.integer(d$PhysAct) - 1
##   return(d)
## })

## round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][, m$usevariables], as.numeric)))))/50, 2)
## round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][p4final[[1]]$b1pgender == "female", m$usevariables], as.numeric)))))/50, 2)
## round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][p4final[[1]]$b1pgender == "male", m$usevariables], as.numeric)))))/50, 2)


cd(base, "midus_rcm_", "imputed")

## junk <- lapply(1:50, function(i) {
##   invisible(prepareMplusData(p4final[[i]], file = paste0("imp", i, ".dat"),
##     inpfile = FALSE))
## })
## cat(paste0("imp", 1:50, ".dat"), file = "implist.dat", sep = "\n")

m <- mplusObject(TITLE = "RCM",
VARIABLE = "
  USEVARIABLES =
    !covariates
    B1PAGE_M2 RAA ROther Female B1SC1
    ! SES and stress and PSRs
    m1chadv dSES B1LifeStress B1PSR
    ! negative affect
    B1NegAff
    ! health behaviors
    B1SmokePast B1SmokeCurrent
    B1AlcoholHeavy B1AlcoholModerate
    B1Sleep B1SSQ B1PhysAct
    ! allostatic load
    B4F
    ! chronic conditions
    C1SCHRON B1SCHRON
    ! created variables
    PSRxSES PSRxdSES
    !PSRxSex PSRxAge
    !PSRxStress
    B1Sleep2 !B1SSQ2
  ;

  IDVARIABLE = M2ID;
  CLUSTER = M2FAMNUM;
  COUNT =
    B1LifeStress (nb)
    B1SCHRON (nb)
    C1SCHRON (nb)
  ;
  CATEGORICAL =
    B1SmokePast B1SmokeCurrent
    B1AlcoholHeavy B1AlcoholModerate
  ;

  DEFINE:
    !PSRxStress = B1LifeStress * B1PSR;
    PSRxSES = m1chadv * B1PSR;
    PSRxdSES = dSES * B1PSR;
    !PSRxSex = Female * B1PSR;
    !PSRxAge = B1PSR * B1PAGE_M2;
    B1Sleep2 = B1Sleep * B1Sleep;
    !B1SSQ2 = B1SSQ * B1SSQ;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 2;
",
MODEL = "
  ! Step 1
    B1LifeStress ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a1a)
      dSES (a1b)
    ;

    B1PSR ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a2a)
      dSES (a2b)
    ;

    ! Step 2
    B1NegAff ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a3a)
      dSES (a3b)
      B1PSR (b3)
      B1LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      !PSRxAge
    ;

    ! Step 3
    B1Sleep ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a4a)
      dSES (a4b)
      B1PSR (b4)
      B1LifeStress (c4)
      B1NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      !PSRxdSES (ab4b)
      !PSRxSex
      !PSRxAge
    ;

    B1SmokePast ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a5a)
      dSES (a5b)
      B1PSR (b5)
      B1LifeStress (c5)
      B1NegAff (d5)
      !PSRxStress
      !PSRxSES (ab5a)
      !PSRxdSES (ab5b)
      !PSRxSex
      !PSRxAge
    ;

    B1SmokeCurrent ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a6a)
      dSES (a6b)
      B1PSR (b6)
      B1LifeStress (c6)
      B1NegAff (d6)
      !PSRxStress
      !PSRxSES (ab6a)
      !PSRxdSES (ab6b)
      !PSRxSex
      !PSRxAge
    ;

    B1AlcoholHeavy ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a7a)
      dSES (a7b)
      B1PSR (b7)
      B1LifeStress (c7)
      B1NegAff (d7)
      !PSRxStress
      !PSRxSES (ab7a)
      !PSRxdSES (ab7b)
      !PSRxSex
      !PSRxAge
    ;

    B1AlcoholModerate ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a8a)
      dSES (a8b)
      B1PSR (b8)
      B1LifeStress (c8)
      B1NegAff (d8)
      !PSRxStress
      !PSRxSES (ab8a)
      !PSRxdSES (ab8b)
      !PSRxSex
      !PSRxAge
    ;

    B1SSQ ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a9a)
      dSES (a9b)
      B1PSR (b9)
      B1LifeStress (c9)
      B1NegAff (d9)
      !PSRxStress
      !PSRxSES (ab9a)
      PSRxdSES (ab9b)
      !PSRxSex
      !PSRxAge
    ;

    B1PhysAct ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv (a12a)
      dSES (a12b)
      B1PSR (b12)
      B1LifeStress (c12)
      B1NegAff (d12)
      !PSRxStress
      !PSRxSES (ab12a)
      !PSRxdSES (ab12b)
      !PSRxSex
      !PSRxAge
    ;

    ! Step 4
    B4F ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv
      dSES
      m1chadv
      dSES
      B1PSR
      B1LifeStress
      B1NegAff

      B1SmokePast B1SmokeCurrent
      B1AlcoholHeavy B1AlcoholModerate
      B1Sleep B1SSQ B1PhysAct
      B1Sleep2 !B1SSQ2

      !PSRxStress
      !PSRxSES (ab13a)
      !PSRxdSES (ab13b)
      !PSRxSex
      !PSRxAge
    ;

    ! Step 5
    B1SCHRON ON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv
      dSES
      m1chadv
      dSES
      B1PSR
      B1LifeStress
      B1NegAff

      B1SmokePast B1SmokeCurrent
      B1AlcoholHeavy B1AlcoholModerate
      B1Sleep B1SSQ B1PhysAct
      B1Sleep2 !B1SSQ2

      B4F

      !PSRxStress
      !PSRxSES (ab14a)
      !PSRxdSES (ab14b)
      !PSRxSex
      !PSRxAge
    ;

    C1SCHRON ON
      B1SCHRON
      B1PAGE_M2 RAA ROther Female B1SC1
      m1chadv
      dSES
      m1chadv
      dSES
      B1PSR
      B1LifeStress
      B1NegAff

      B1SmokePast B1SmokeCurrent
      B1AlcoholHeavy B1AlcoholModerate
      B1Sleep B1SSQ B1PhysAct
      !B1Sleep2 B1SSQ2

      B4F

      !PSRxStress
      !PSRxSES (ab15a)
      !PSRxdSES (ab15b)
      !PSRxSex
      !PSRxAge
    ;

",
  OUTPUT = "
   !STDYX;
   !CINTERVAL;",
usevariables = colnames(dwimp3[[1]]),
rdata = dwimp3[1:50], imputed = TRUE)

m <- mplusModeler(m, dataout = "rcm_int.dat", run = 0L)
screenreg(m, single.row=TRUE)




mtest1 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      a1a = 0;
      a1b = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test1.dat", run = 1L)

mtest2 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      a2a = 0;
      a2b = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test2.dat", run = 1L)


mtest3 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      a4a = 0;
      a4b = 0;
      a5a = 0;
      a5b = 0;
      a6a = 0;
      a6b = 0;
      a7a = 0;
      a7b = 0;
      a8a = 0;
      a8b = 0;
      a9a = 0;
      a9b = 0;
      a10a = 0;
      a10b = 0;
      a11a = 0;
      a11b = 0;
      a12a = 0;
      a12b = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test3.dat", run = 1L)

mtest4 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      d4 = 0;
      d5 = 0;
      d6 = 0;
      d7 = 0;
      d8 = 0;
      d9 = 0;
      d10 = 0;
      d11 = 0;
      d12 = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test4.dat", run = 1L)


mtest5 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      b4 = 0;
      b5 = 0;
      b6 = 0;
      b7 = 0;
      b8 = 0;
      b9 = 0;
      b10 = 0;
      b11 = 0;
      b12 = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test5.dat", run = 1L)

mtest6 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      a13a = 0;
      a13b = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test6.dat", run = 1L)

mtest7 <- mplusModeler(update(m,
  MODEL = ~ . +
    "MODEL TEST:
      e13 = 0;
      f13 = 0;
      g13 = 0;
      h13 = 0;
      i13 = 0;
      j13 = 0;
      k13 = 0;
      l13 = 0;
      m13 = 0;
    "
    ),
  dataout = "psr_al_final_interactions6_test7.dat", run = 1L)







mses <- mplusObject(TITLE = "PSR AL Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES
      p4majorconditions
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = p4majorconditions;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
      ! Step 5
      p4majorconditions ON
      m1chadv (a13a)
      dSES (a13b)
      ;

",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

mses <- mplusModeler(mses, dataout = "psr_al_final_sesout.dat", run = 1L)
screenreg(mses, single.row=TRUE, digits = 3)





mpsr <- mplusObject(TITLE = "PSR AL Model",
VARIABLE = "
   USEVARIABLES =
      PSR
      F
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
      ! Step 5
      F ON
        PSR;
      ;

",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

mpsr <- mplusModeler(mpsr, dataout = "psr_al_final_psrout.dat", run = 1L)
screenreg(mpsr, single.row=TRUE, digits = 3)


##{{{ Inflammation

m.infl <- mplusObject(TITLE = "PSR Inflammation Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_infl
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_infl ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_infl (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.infl <- mplusModeler(m.infl, dataout = "psr_infl_final_interactions6.dat", run = 1L)

screenreg(m.infl, single.row=TRUE)


##}}}


##{{{ Parasympathetic

m.para <- mplusObject(TITLE = "PSR Para Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_para
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_para ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_para (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.para <- mplusModeler(m.para, dataout = "psr_para_final_interactions6.dat", run = 1L)

screenreg(m.para, single.row=TRUE)


##}}}


##{{{ Sympathetic

m.sym <- mplusObject(TITLE = "PSR Sym Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_sym
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_sym ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_sym (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.sym <- mplusModeler(m.sym, dataout = "psr_sym_final_interactions6.dat", run = 1L)

screenreg(m.sym, single.row=TRUE)


##}}}


##{{{ HPA

m.hpa <- mplusObject(TITLE = "PSR HPA Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_hpa
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_hpa ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_hpa (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.hpa <- mplusModeler(m.hpa, dataout = "psr_hpa_final_interactions6.dat", run = 1L)

screenreg(m.hpa, single.row=TRUE)

##}}}


##{{{ Card

m.card <- mplusObject(TITLE = "PSR Card Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_card
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_card ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_card (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.card <- mplusModeler(m.card, dataout = "psr_card_final_interactions6.dat", run = 1L)

screenreg(m.card, single.row=TRUE)

##}}}


##{{{ Glucose

m.gluc <- mplusObject(TITLE = "PSR Gluc Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_gluc
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_gluc ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_gluc (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.gluc <- mplusModeler(m.gluc, dataout = "psr_gluc_final_interactions6.dat", run = 1L)

screenreg(m.gluc, single.row=TRUE)

##}}}

##{{{ Lipids

m.lipid <- mplusObject(TITLE = "PSR Lipid Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      bi_lipid
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      bi_lipid ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      bi_lipid (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.lipid <- mplusModeler(m.lipid, dataout = "psr_lipid_final_interactions6.dat", run = 1L)

screenreg(m.lipid, single.row=TRUE)

##}}}




##{{{ Psychological Resources

m.psych <- mplusObject(TITLE = "Psych Res AL Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PR LifeStress
      b1page_m2
      NegAff
      F
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PRxSES PRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PRxSES = m1chadv * PR;
    PRxdSES = dSES * PR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PRxSES PRxdSES];
  PRxSES WITH PRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PR (b3)
      LifeStress (c3)
      !PSRxStress
      PRxSES (ab3a)
      PRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      PRxSES (ab4a)
      PRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      F ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      F (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.psych <- mplusModeler(m.psych, dataout = "psychres_al_final_interactions6.dat", run = 1L)
screenreg(m.psych, single.row=TRUE)

##}}}



##{{{ Social Resources

m.soc <- mplusObject(TITLE = "Soc Res AL Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES SR LifeStress
      b1page_m2
      NegAff
      F
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      SRxSES SRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    SRxSES = m1chadv * SR;
    SRxdSES = dSES * SR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [SRxSES SRxdSES];
  SRxSES WITH SRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    SR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    NegAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      SR (b3)
      LifeStress (c3)
      !PSRxStress
      SRxSES (ab3a)
      SRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      SR (b4)
      LifeStress (c4)
      NegAff (d4)
      !PSRxStress
      SRxSES (ab4a)
      SRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      SR (b5)
      LifeStress (c5)
      NegAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      SR (b6)
      LifeStress (c6)
      NegAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      SR (b7)
      LifeStress (c7)
      NegAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      SR (b8)
      LifeStress (c8)
      NegAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      SR (b9)
      LifeStress (c9)
      NegAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      SR (b10)
      LifeStress (c10)
      NegAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      SR (b11)
      LifeStress (c11)
      NegAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      F ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      SR (b12)
      LifeStress (c12)
      NegAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      SR (b13)
      LifeStress (c13)
      NegAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      F (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.soc <- mplusModeler(m.soc, dataout = "socres_al_final_interactions6.dat", run = 1L)

screenreg(m.soc, single.row=TRUE)

##}}}




##{{{ using positive mood instead of negative mood
m.pm <- mplusObject(TITLE = "PSR AL Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      PosAff
      F
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress p4majorconditions;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    PSRxSES = m1chadv * PSR;
    PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 8;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  [PSRxSES PSRxdSES];
  PSRxSES WITH PSRxdSES*;

  ! Step 1
    LifeStress ON
      sex b1page_m2
      m1chadv (a1a)
      dSES (a1b)
      ;

    PSR ON
      sex b1page_m2
      m1chadv (a2a)
      dSES (a2b)
      ;

    ! Step 2
    PosAff ON
      sex b1page_m2
      m1chadv (a3a)
      dSES (a3b)
      PSR (b3)
      LifeStress (c3)
      !PSRxStress
      PSRxSES (ab3a)
      PSRxdSES (ab3b)
      !PSRxSex
      ;

    ! Step 3
      PSQI3 ON
      sex b1page_m2
      m1chadv (a4a)
      dSES (a4b)
      PSR (b4)
      LifeStress (c4)
      PosAff (d4)
      !PSRxStress
      PSRxSES (ab4a)
      PSRxdSES (ab4b)
      !PSRxSex
      ;

      CurrentSmoke ON
      sex b1page_m2
      m1chadv (a5a)
      dSES (a5b)
      PSR (b5)
      LifeStress (c5)
      PosAff (d5)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      RiskyBehaviors ON
      sex b1page_m2
      m1chadv (a6a)
      dSES (a6b)
      PSR (b6)
      LifeStress (c6)
      PosAff (d6)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      CANone ON
      sex b1page_m2
      m1chadv (a7a)
      dSES (a7b)
      PSR (b7)
      LifeStress (c7)
      PosAff (d7)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAFrequent ON
      sex b1page_m2
      m1chadv (a8a)
      dSES (a8b)
      PSR (b8)
      LifeStress (c8)
      PosAff (d8)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;
      CAHeavy ON
      sex b1page_m2
      m1chadv (a9a)
      dSES (a9b)
      PSR (b9)
      LifeStress (c9)
      PosAff (d9)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


      PhysAct ON
      sex b1page_m2
      m1chadv (a10a)
      dSES (a10b)
      PSR (b10)
      LifeStress (c10)
      PosAff (d10)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

      ! use OverallDietsq ?
      OverallDiet ON
      sex b1page_m2
      m1chadv (a11a)
      dSES (a11b)
      PSR (b11)
      LifeStress (c11)
      PosAff (d11)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;


    ! Step 4
      F ON
      sex b1page_m2
      m1chadv (a12a)
      dSES (a12b)
      PSR (b12)
      LifeStress (c12)
      PosAff (d12)
      PSQI3 (e12)
      CurrentSmoke (f12)
      RiskyBehaviors (g12)
      CANone (h12)
      CAFrequent (i12)
      CAHeavy (j12)
      PhysAct (k12)
      OverallDiet (l12)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
     ;

      ! Step 5
      p4majorconditions ON
      sex b1page_m2
      m1chadv (a13a)
      dSES (a13b)
      PSR (b13)
      LifeStress (c13)
      PosAff (d13)
      PSQI3 (e13)
      CurrentSmoke (f13)
      RiskyBehaviors (g13)
      CANone (h13)
      CAFrequent (i13)
      CAHeavy (j13)
      PhysAct (k13)
      OverallDiet (l13)
      F (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     MODEL CONSTRAINT:
       NEW(ind1 tot1 ind2);
       ind1 = e12 * m13;
       tot1 = ind1 + e13;
       ind2 = ind1 * b4;
",
  OUTPUT = "
   CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m.pm <- mplusModeler(m.pm, dataout = "psr_al_final_interactions6_pm.dat", run = 1L)
screenreg(m.pm, single.row=TRUE)

##}}}
