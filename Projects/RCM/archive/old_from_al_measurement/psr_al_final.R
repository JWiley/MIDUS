library(checkpoint)
checkpoint("2016-04-01", R.version = "3.2.4", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)

library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(JWileymisc)
library(MplusAutomation)
library(texreg)
library(foreign)
library(survival)

source("~/Google Drive/RCode/utilities.R")
n <- function(x) as.numeric(as.character(x))

load("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data.RData")

p4final <- lapply(p4final, function(d) {
d <- within(d, {
   Diet <- n(Diet)
   ExtremeDiet <- n(ExtremeDiet)
   OverallDiet <- n(OverallDiet)
   CurrentSmoke <- as.integer(CurrentSmoke == 1)
   RiskyBehaviors <- as.integer(RiskyBehaviors == 1)
   PhysAct <- n(PhysAct)
   Milwaukee <- as.integer(is.na(m2famnum))
   m2famnum <- ifelse(is.na(m2famnum), M2ID, m2famnum)

   b1pgender <- as.integer(b1pgender == "female")
   b1page_m2 <- (b1page_m2 - 55) / 10;

   NegAff <- NegAff * -1
   PosAff <- PosAff * -1

   m1chadv <- as.vector(scale(m1chadv))
   m2aadv <- as.vector(scale(m2aadv))
   dSES <- m2aadv - m1chadv

   ##CurAlc2 <- CurrentAlcohol^2
   CANone <- as.integer(CurrentAlcohol == "None")
   CAInfrequent <- as.integer(CurrentAlcohol == "Infrequent<4")
   CAFrequent <- as.integer(CurrentAlcohol == "Frequent<4")
   CAHeavy <- as.integer(CurrentAlcohol == "Heavy")

   PSR <- as.vector(scale(scale(PR) + scale(SR)))
  })

return(d)
})


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

round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][, m$usevariables], as.numeric)))))/50, 2)
round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][p4final[[1]]$b1pgender == "female", m$usevariables], as.numeric)))))/50, 2)
round(Reduce(`+`, lapply(1:50, function(i) cor(as.data.frame(lapply(p4final[[i]][p4final[[1]]$b1pgender == "male", m$usevariables], as.numeric)))))/50, 2)

cd(base, "psr_hba1c_grant_", "imputed")

m <- mplusObject(TITLE = "PSR HbA1c Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      b4bha1c
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      !p4majorconditions
      sex
      !PSRxStress
      PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress;
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
      b4bha1c ON
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
",
  OUTPUT = "
   CINTERVAL; STDYX;",
usevariables = colnames(p4final[[1]]),
rdata = p4final, imputed = TRUE)

m <- mplusModeler(m, dataout = "psr_hba1c_final_interactions6.dat", run = 1L)
screenreg(m, single.row=TRUE)



cd(base, "psr_al_final_mort_", "imputed")

## junk <- lapply(1:50, function(i) {
##   invisible(prepareMplusData(p4final[[i]], file = paste0("imp", i, ".dat"),
##     inpfile = FALSE))
## })
## cat(paste0("imp", 1:50, ".dat"), file = "implist.dat", sep = "\n")

m <- mplusObject(TITLE = "PSR AL Model",
VARIABLE = "
   USEVARIABLES =
      m1chadv dSES PSR LifeStress
      b1page_m2
      NegAff
      F
      PSQI3 CurrentSmoke RiskyBehaviors
      CANone CAFrequent CAHeavy PhysAct
      OverallDiet
      !p4majorconditions
      LastAge DECEASED
      sex
      !PSRxStress
      !PSRxSES PSRxdSES
 !     PSRxSex
      ;

  IDVARIABLE = M2ID;
    CLUSTER = m2famnum;
    COUNT = LifeStress
      !p4majorconditions
    ;
    CATEGORICAL = CANone CAFrequent CAHeavy
       CurrentSmoke PhysAct RiskyBehaviors;
    SURVIVAL = LastAge (55*1);
    TIMECENSORED = DECEASED (1 = NOT 0 = RIGHT);

    DEFINE:
    Sex = b1pgender;
    !PSRxStress = LifeStress * PSR;
    !SRxSES = m1chadv * PSR;
    !PSRxdSES = dSES * PSR;
!    PSRxSex = Sex * PSR;

",
ANALYSIS = "
  TYPE = COMPLEX;
  Estimator = MLR;
  PROCESSORS = 2;
  !MCONVERGENCE  = .000005;
  !H1CONVERGENCE = .000005;
  !LOGCRITERION  = .000005;
  !RLOGCRITERION = .000005;
  !ITERATIONS = 15000;
  !MITERATIONS = 7500;
  !H1ITERATIONS = 15000;
  INTEGRATION = MONTECARLO (500);
  BASEHAZARD = ON;
",
MODEL = "
!  [PSRxStress PSRxSES PSRxdSES PSRxSex];
!  PSRxStress WITH PSRxSES* PSRxdSES* PSRxSex*;
!  PSRxSES WITH PSRxdSES* PSRxSex*;
!  PSRxdSES WITH PSRxSex*;

  ![PSRxSES PSRxdSES];
  !PSRxSES WITH PSRxdSES*;

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
      !PSRxSES (ab3a)
      !PSRxdSES (ab3b)
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
      !PSRxSES (ab4a)
      !PSRxdSES (ab4b)
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
      F ON
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
      LastAge ON
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
      F (m13)
      !PSRxStress
      !PSRxSES
      !PSRxdSES
      !PSRxSex
      ;

     !MODEL CONSTRAINT:
     !  NEW(ind1 tot1 ind2);
     !  ind1 = e12 * m13;
     !  tot1 = ind1 + e13;
     !  ind2 = ind1 * b4;
",
  OUTPUT = "
   !CINTERVAL;",
usevariables = colnames(p4final[[1]]),
rdata = p4final[1:5], imputed = TRUE)

m <- mplusModeler(m, dataout = "psr_al_final_interactions6.dat", run = 1L)
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
