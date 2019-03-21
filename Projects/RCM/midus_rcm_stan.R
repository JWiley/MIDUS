##{{{ setup code

library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)

## devtools::install_github("ElkhartGroup/varian")

library(rstan)
library(shinystan)
library(rstanarm)
library(MASS)
library(mice)
library(VGAM)
library(abind)
library(varian)
library(JWileymisc)
library(pscore)
library(aod)

rstan_options(auto_write = TRUE)
options(mc.cores = 2)


n <- function(x) as.numeric(as.character(x))
z <- function(x) as.vector(scale(x))
#ziqr <- function(x) as.vector((x - quantile(x, .25, na.rm=TRUE))/diff(quantile(x, c(.25, .75), na.rm = TRUE)))
ziqr <- z

## dwcleanimp <- readRDS("midus_rcm_final_imputed.RDS")
## dwclean <- readRDS("../midus_merged_data_dwclean.RDS")

dwcleanimp <- readRDS("~/OneDrive/Projects/MIDUS-Setup/rcm_imputation/midus_rcm_final_imputed.RDS")
dwclean <- readRDS("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data_dwclean.RDS")

## use cases that were present in M2 or Milwaukee
usedex <- !is.na(dwclean$B1Smoke)

dwclean2 <- as.data.frame(dwclean[usedex])

dwcleanimp <- lapply(1:length(dwcleanimp), function(i) {
  dwcleanimp <- as.data.table(dwcleanimp[[i]])
  dwcleanimp <- dwcleanimp[M2ID %in% dwclean2$M2ID]
  dwcleanimp[, B1PAGE_M2 := ziqr(B1PAGE_M2)]
  dwcleanimp[, Female := as.integer(Sex == "2")]
  dwcleanimp[, B1NegAff := ziqr(B1NegAff * -1)]
  dwcleanimp[, B1PosAff := ziqr(B1PosAff * -1)]
  dwcleanimp[, m1chadv := ziqr(m1chadv)]
  dwcleanimp[, m2aadv := ziqr(m2aadv)]
  dwcleanimp[, dSES := z(z(m2aadv) - z(m1chadv))]
  dwcleanimp[, B1PR := ziqr(B1PR)]
  dwcleanimp[, B1SR := ziqr(B1SR)]
  dwcleanimp[, B1PSR := ziqr(z(B1PR) + z(B1SR))]
  dwcleanimp[, B1Smoke := relevel(B1Smoke, ref = "never")]
  dwcleanimp[, B1SmokePast := as.integer(B1Smoke == "past")]
  dwcleanimp[, B1SmokeCurrent := as.integer(B1Smoke == "current")]
  dwcleanimp[, B1CurrentAlcohol := relevel(B1CurrentAlcohol, ref = "None")]
  dwcleanimp[, B1AlcoholHeavy := as.integer(B1CurrentAlcohol == "High")]
  dwcleanimp[, B1AlcoholModerate := as.integer(B1CurrentAlcohol == "Moderate")]
  dwcleanimp[, B1SC1 := as.integer(B1SC1 == "1")]
  dwcleanimp[, B1Sleep := ziqr(B1Sleep)]
  dwcleanimp[, B1SSQ := ziqr(B1SSQ)]
  dwcleanimp[, B1PhysAct := ziqr(B1PhysAct)]
  dwcleanimp[, B4F := ziqr(B4F)]
  dwcleanimp[, B4bi.sym := ziqr(B4bi.sym)]
  dwcleanimp[, B4bi.hpa := ziqr(B4bi.hpa)]
  dwcleanimp[, B4bi.card := ziqr(B4bi.card)]
  dwcleanimp[, B4bi.gluc := ziqr(B4bi.gluc)]
  dwcleanimp[, B4bi.lipid := ziqr(B4bi.lipid)]
  dwcleanimp[, B4bi.infl := ziqr(B4bi.infl)]
  dwcleanimp[, B4bi.para := ziqr(B4bi.para)]
  dwcleanimp[, RaceG3 := relevel(RaceG3, ref = "White")]
  dwcleanimp[, RAA := as.integer(RaceG3 == "AA")]
  dwcleanimp[, ROther := as.integer(RaceG3 == "Other")]
  return(dwcleanimp)
})




##}}}

################################################################################
##                                                                            ##
##                          Descriptive Statistics                            ##
##                                                                            ##
################################################################################

egltable(c("Sex", "B1PAGE_M2", "RaceG3", "B1SC1",
           "m1chadv", "m2aadv", "B1LifeStress", "B1SLFEDI", "B1PSR", "B1NegAff",
           "B1Sleep", "B1SSQ", "B1Smoke", "B1CurrentAlcohol", "B1PhysAct",
#           "B4F", "B4bi.sym",
           "B1SCHRON", "C1SCHRON"
           ), data = is.na(within(dwclean2, {
  Sex <- factor(Sex)
  B1SC1 <- factor(B1SC1)
  B1PSR <- 0
  B1NegAff <- 0
})))

egltable(c("Sex", "B1PAGE_M2", "RaceG3", "B1SC1",
           "m1chadv", "m2aadv", "B1LifeStress", "B1PSR", "B1NegAff",
           "B1Sleep", "B1SSQ", "B1Smoke", "B1CurrentAlcohol", "B1PhysAct",
#           "B4F", "B4bi.sym",
           "B1SCHRON", "C1SCHRON"
           ), data = within(dwclean2, {
  Sex <- factor(Sex)
  B1SC1 <- factor(B1SC1)
  B1PSR <- 0
  B1NegAff <- 0
}))

egltable(c("B1LifeStress", "B1SLFEDI", "B1SCHRON", "C1SCHRON"), data = dwclean2, parametric = FALSE)



################################################################################
##                                                                            ##
##                            Exploratory Work                                ##
##                                                                            ##
################################################################################

mys <- function(object) {
  s <- summary(pool(as.mira(object)))#[, 1:7]
  cbind.data.frame(s, S = star(s[,5], TRUE))
}

options(width = 180, digits = 3)

## Life Stress
if (FALSE) {
m1 <- lapply(dwcleanimp, function(d) {
  glm.nb(B1LifeStress ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
           (m1chadv + m2aadv),
         data = d)
})

mys(m1)

m1final <- lapply(dwcleanimp, function(d) {
  glm.nb(B1LifeStress ~ B1SC1 + RaceG3 + Female + B1PAGE_M2 +
           m1chadv + m2aadv + B1PAGE_M2:m2aadv,
         data = d)
})
mys(m1final)
}

X1 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + RaceG3 + Female + B1PAGE_M2 +
                     m1chadv + m2aadv + B1PAGE_M2:m2aadv,
               data = d)
})
y1 <- lapply(dwcleanimp, `[[`, "B1LifeStress")

## Lifetime Discrimination
if (FALSE) {
m2 <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SLFEDI ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
           (m1chadv + m2aadv),
         data = d)
})

mys(m2)

m2final <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SLFEDI ~ B1SC1 + RaceG3 + Female + B1PAGE_M2 +
           m1chadv + m2aadv + B1PAGE_M2:m2aadv + RaceG3:m2aadv,
         data = d)
})
mys(m2final)

}

X2 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + RaceG3 + Female + B1PAGE_M2 +
                     m1chadv + m2aadv + B1PAGE_M2:m2aadv + RaceG3:m2aadv,
               data = d)
})
y2 <- lapply(dwcleanimp, `[[`, "B1SLFEDI")


## PSRs
if (FALSE) {
m3 <- lapply(dwcleanimp, function(d) {
  lm(B1PSR ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
           (m1chadv + m2aadv),
         data = d)
})
mys(m3)

m3 <- lapply(dwcleanimp, function(d) {
  lm(B1PSR ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) +
           (m1chadv * m2aadv),
         data = d)
})
mys(m3)

m3final <- lapply(dwcleanimp, function(d) {
  lm(B1PSR ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
           m1chadv + m2aadv + Female:m2aadv,
         data = d)
})
mys(m3final)
pool.r.squared(as.mira(m3final))
}

X3 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
                 m1chadv + m2aadv + Female:m2aadv,
               data = d)
})
y3 <- lapply(dwcleanimp, `[[`, "B1PSR")

## NegAff
if (FALSE) {
m4 <- lapply(dwcleanimp, function(d) {
  lm(B1NegAff ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR) +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI) * B1PSR,
     data = d)
})
mys(m4)
pool.r.squared(as.mira(m4))

m4final <- lapply(dwcleanimp, function(d) {
  lm(B1NegAff ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR +
       RaceG3:m2aadv + RaceG3:B1PSR + m2aadv:B1PSR,
     data = d)
})
mys(m4final)
pool.r.squared(as.mira(m4final))

}

X4 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR +
       RaceG3:m2aadv + RaceG3:B1PSR + m2aadv:B1PSR,
       data = d)
})
y4 <- lapply(dwcleanimp, `[[`, "B1NegAff")


## Sleep
if (FALSE) {
m5 <- lapply(dwcleanimp, function(d) {
  lm(B1Sleep ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m5)
pool.r.squared(as.mira(m5))

m5final <- lapply(dwcleanimp, function(d) {
  lm(B1Sleep ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       RaceG3:B1NegAff + Female:m2aadv,
     data = d)
})
mys(m5final)
pool.r.squared(as.mira(m5final))
}

X5 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       RaceG3:B1NegAff + Female:m2aadv,
       data = d)
})
y5 <- lapply(dwcleanimp, `[[`, "B1Sleep")


## Sleep Quality
if (FALSE) {
m6 <- lapply(dwcleanimp, function(d) {
  lm(B1SSQ ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m6)
pool.r.squared(as.mira(m6))

m6final <- lapply(dwcleanimp, function(d) {
  lm(B1SSQ ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1NegAff:B1PSR,
     data = d)
})
mys(m6final)
pool.r.squared(as.mira(m6final))
}

X6 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1NegAff:B1PSR,
       data = d)
})
y6 <- lapply(dwcleanimp, `[[`, "B1SSQ")


## Smoking (Past and Current)
if (FALSE) {
m7 <- lapply(dwcleanimp, function(d) {
  vglm(B1Smoke ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
         (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
       family = multinomial,
       data = d)
})
mys(m7)

m7 <- lapply(dwcleanimp, function(d) {
  vglm(relevel(B1Smoke, ref = "past") ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
         (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
       family = multinomial,
       data = d)
})
mys(m7)

m7 <- lapply(dwcleanimp, function(d) {
  vglm(factor(B1Smoke, levels = c("current", "past", "never")) ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m2aadv + RaceG3:m1chadv + Female:m1chadv + Female:B1LifeStress,
       family = multinomial,
       data = d)
})
mys(m7)

m7 <- lapply(dwcleanimp, function(d) {
  vglm(factor(B1Smoke, levels = c("current", "past", "never")) ~ 1,
       family = multinomial,
       data = d)
})
mys(m7)


}

X7 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m2aadv + Female:m1chadv + Female:B1LifeStress,
       data = d)
})
y7 <- lapply(dwcleanimp, `[[`, "B1Smoke")

## Alcohol (Heavy and Moderate)
## always identical design matrix for alcohol
if (FALSE) {
m8 <- lapply(dwcleanimp, function(d) {
  vglm(B1CurrentAlcohol ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
         (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
       family = multinomial,
       data = d)
})
mys(m8)

m8 <- lapply(dwcleanimp, function(d) {
  vglm(relevel(B1CurrentAlcohol, ref = "Moderate") ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
         (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
       family = multinomial,
       data = d)
})
mys(m8)

m8final <- lapply(dwcleanimp, function(d) {
  vglm(relevel(B1CurrentAlcohol, ref = "Moderate") ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m2aadv + RaceG3:m1chadv + RaceG3:m2aadv + RaceG3:B1LifeStress + Female:m2aadv,
       family = multinomial,
       data = d)
})
mys(m8final)

m8final <- lapply(dwcleanimp, function(d) {
  vglm(B1CurrentAlcohol ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m2aadv + RaceG3:m1chadv + RaceG3:m2aadv + RaceG3:B1LifeStress + Female:m2aadv,
       family = multinomial,
       data = d)
})
mys(m8final)

}

X8 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m2aadv + RaceG3:m1chadv + RaceG3:m2aadv + RaceG3:B1LifeStress + Female:m2aadv,
       data = d)
})
y8 <- lapply(dwcleanimp, `[[`, "B1CurrentAlcohol")


## Physical Activity
if (FALSE) {
m9 <- lapply(dwcleanimp, function(d) {
  lm(B1PhysAct ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m9)
pool.r.squared(as.mira(m9))

m9final <- lapply(dwcleanimp, function(d) {
  lm(B1PhysAct ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1PAGE_M2:m1chadv,
     data = d)
})
mys(m9final)
pool.r.squared(as.mira(m9final))

}

X9 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
                 m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
                 B1PAGE_M2:m1chadv,
       data = d)
})
y9 <- lapply(dwcleanimp, `[[`, "B1PhysAct")


## Allostatic Load
if (FALSE) {
m10 <- lapply(dwcleanimp, function(d) {
  lm(B4F ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m10)
pool.r.squared(as.mira(m10))

m10 <- lapply(dwcleanimp, function(d) {
  lm(B4F ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m10)
pool.r.squared(as.mira(m10))

m10final <- lapply(dwcleanimp, function(d) {
  lm(B4F ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + ziqr(B1LifeStress) + ziqr(B1SLFEDI) + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m10final)
pool.r.squared(as.mira(m10final))
}

X10 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ + B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
y10 <- lapply(dwcleanimp, `[[`, "B4F")


## Inflammation
if (FALSE) {
m11 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.infl ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m11)
pool.r.squared(as.mira(m11))

m11 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.infl ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m11)
pool.r.squared(as.mira(m11))

m11final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.infl ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m11final)
pool.r.squared(as.mira(m11final))
}

X11 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
y11 <- lapply(dwcleanimp, `[[`, "B4bi.infl")


## Parasympathetic nervous system
if (FALSE) {
m12 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.para ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m12)
pool.r.squared(as.mira(m12))

m12 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.para ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m12)
pool.r.squared(as.mira(m12))

m12final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.para ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m12final)
pool.r.squared(as.mira(m12final))


X12 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y12 <- lapply(dwcleanimp, `[[`, "B4bi.para")


## Metabolic Lipids
if (FALSE) {
m13 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.lipid ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m13)
pool.r.squared(as.mira(m13))

m13 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.lipid ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m13)
pool.r.squared(as.mira(m13))

m13final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.lipid ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m13final)
pool.r.squared(as.mira(m13final))

X13 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y13 <- lapply(dwcleanimp, `[[`, "B4bi.lipid")


## Metabolic Glucose
if (FALSE) {
m14 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.gluc ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m14)
pool.r.squared(as.mira(m14))

m14 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.gluc ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m14)
pool.r.squared(as.mira(m14))

m14final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.gluc ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m14final)
pool.r.squared(as.mira(m14final))

X14 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y14 <- lapply(dwcleanimp, `[[`, "B4bi.gluc")


## HPA
if (FALSE) {
m15 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.hpa ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m15)
pool.r.squared(as.mira(m15))

m15 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.hpa ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m15)
pool.r.squared(as.mira(m15))

m15final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.hpa ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m15final)
pool.r.squared(as.mira(m15final))

X15 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y15 <- lapply(dwcleanimp, `[[`, "B4bi.hpa")


## Sympathetic Nervous System
if (FALSE) {
m16 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.sym ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m16)
pool.r.squared(as.mira(m16))

m16 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.sym ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m16)
pool.r.squared(as.mira(m16))

m16final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.sym ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m16final)
pool.r.squared(as.mira(m16final))

X16 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y16 <- lapply(dwcleanimp, `[[`, "B4bi.sym")


## Cardiovascular (BP)
if (FALSE) {
m17 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.card ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m17)
pool.r.squared(as.mira(m17))

m17 <- lapply(dwcleanimp, function(d) {
  lm(B4bi.card ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m17)
pool.r.squared(as.mira(m17))

m17final <- lapply(dwcleanimp, function(d) {
  lm(B4bi.card ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
     data = d)
})
mys(m17final)
pool.r.squared(as.mira(m17final))

X17 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct,
       data = d)
})
}

y17 <- lapply(dwcleanimp, `[[`, "B4bi.card")


## M2 Chronic Conditions
if (FALSE) {
m18.a <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv, data = d)
})
mys(m18.a)


m18 <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SCHRON ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR,
     data = d)
})
mys(m18)

m18 <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc +
       RaceG3:B1SLFEDI,
     data = d)
})
mys(m18)

m18final <- lapply(dwcleanimp, function(d) {
  glm.nb(B1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc +
       RaceG3:B1SLFEDI,
     data = d)
})
mys(m18final)
}

X18 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc +
       RaceG3:B1SLFEDI,
       data = d)
})
y18 <- lapply(dwcleanimp, `[[`, "B1SCHRON")


## M3 Chronic Conditions
if (FALSE) {
m19.a <- lapply(dwcleanimp, function(d) {
  glm.nb(C1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1SCHRON, data = d)
})
mys(m19.a)


m19 <- lapply(dwcleanimp, function(d) {
  glm.nb(C1SCHRON ~ B1SC1 + (B1PAGE_M2 + RaceG3 + Female) *
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff) +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc +
       (m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1NegAff) * B1PSR + B1SCHRON,
     data = d)
})
mys(m19)

m19 <- lapply(dwcleanimp, function(d) {
  glm.nb(C1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + I(B1Sleep^2) + B1SSQ + I(B1SSQ^2) +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc + B1SCHRON,
     data = d)
})
mys(m19)

m19final <- lapply(dwcleanimp, function(d) {
  glm.nb(C1SCHRON ~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc + B1SCHRON,
     data = d)
})

mys(m19final)
}

X19 <- lapply(dwcleanimp, function(d) {
  model.matrix(~ B1SC1 + B1PAGE_M2 + RaceG3 + Female +
       m1chadv + m2aadv + B1LifeStress + B1SLFEDI + B1PSR + B1NegAff +
       B1Sleep + B1SSQ +
       B1Smoke + B1CurrentAlcohol + B1PhysAct +
       B4F + B4bi.infl + B4bi.para + B4bi.hpa + B4bi.sym + B4bi.card + B4bi.lipid + B4bi.gluc + B1SCHRON,
       data = d)
})
y19 <- lapply(dwcleanimp, `[[`, "C1SCHRON")


## mys(lapply(dwcleanimp, function(d) {
##   glm.nb(C1SCHRON ~ B1SCHRON + z(B4F),
##          data = d[!is.na(dwclean$GNNregAL)])
## }))
## mys(lapply(dwcleanimp, function(d) {
##   glm.nb(C1SCHRON ~ B1SCHRON + z(dwclean[!is.na(GNNregAL)]$GNNregAL),
##          data = d[!is.na(dwclean$GNNregAL)])
## }))


## Main MIDUS RCM Model

midus_rcm_mod <- "
data {
  int<lower=1> N; // the number of observations
  int<lower=1> J; // the number of dependent variables
  int K[J]; // the number of columns in the model matrix for each DV

  // LifeStress
  int<lower=0> y1[N]; // the outcome
  matrix[N, K[1]] X1; // the model matrix

  // Lifetime Discrimination
  int<lower=0> y2[N];
  matrix[N, K[2]] X2;

  // PSRs
  real y3[N];
  matrix[N, K[3]] X3;

  // NegAff
  real y4[N];
  matrix[N, K[4]] X4;

  // Sleep (duration)
  real y5[N];
  matrix[N, K[5]] X5;

  // Sleep Quality
  real y6[N];
  matrix[N, K[6]] X6;

  // Current Smoking Status (never, past, current)
  int<lower=1, upper=3> y7[N];
  vector[K[7]] X7[N];

  // Current Alcohol (none, moderate, heavy)
  int<lower=1, upper=3> y8[N];
  vector[K[8]] X8[N];

  // Physical Activity
  real y9[N];
  matrix[N, K[9]] X9;

  // Allostatic Load
  real y10[N];
  matrix[N, K[10]] X10;

  // Physiological Systems Use Same
  // Design Matrix (X11)
  matrix[N, K[11]] X11;
  real y11[N]; // Inflammation
  real y12[N]; // PSNS
  real y13[N]; // Lipids
  real y14[N]; // Glucose
  real y15[N]; // HPA
  real y16[N]; // SNS
  real y17[N]; // Cardiovascular (BP)

  // M2 Chronic Conditions
  int<lower=0> y18[N];
  matrix[N, K[18]] X18;

  // M3 Chronic Conditions
  int<lower=0> y19[N];
  matrix[N, K[19]] X19;
}

transformed data {
  // Special Setup for Multinomial
  row_vector[K[7]] zeros;
  row_vector[K[8]] zeros2;

  zeros = rep_row_vector(0, K[7]);
  zeros2 = rep_row_vector(0, K[8]);
}

parameters {
  // Life Stress
  vector[K[1]] b1; // the regression parameters
  real<lower=0> phi1; // the (over) dispersion parameters

  // Lifetime Discrimination
  vector[K[2]] b2;
  real<lower=0> phi2;

  // PSRs
  vector[K[3]] b3;
  real<lower=0> e3;

  // NegAff
  vector[K[4]] b4;
  real<lower=0> e4;

  // Sleep Duration
  vector[K[5]] b5;
  real<lower=0> e5;

  // Sleep Quality
  vector[K[6]] b6;
  real<lower=0> e6;

  // Current Smoking
  matrix[3 - 1, K[7]] b7;

  // Current Alcohol
  matrix[3 - 1, K[8]] b8;

  // Physical Activity
  vector[K[9]] b9;
  real<lower=0> e9;

  // Allostatic Load
  vector[K[10]] b10;
  real<lower=0> e10;

  // Inflammation
  vector[K[11]] b11;
  real<lower=0> e11;

  // PSNS
  vector[K[12]] b12;
  real<lower=0> e12;

  // Lipids
  vector[K[13]] b13;
  real<lower=0> e13;

  // Glucose
  vector[K[14]] b14;
  real<lower=0> e14;

  // HPA
  vector[K[15]] b15;
  real<lower=0> e15;

  // SNS
  vector[K[16]] b16;
  real<lower=0> e16;

  // Cardiovascular (BP)
  vector[K[17]] b17;
  real<lower=0> e17;

  // M2 Chronic Conditions
  vector[K[18]] b18;
  real<lower=0> phi18;

  // M3 Chronic Conditions
  vector[K[19]] b19;
  real<lower=0> phi19;
}

transformed parameters {
  // minus two because of smoking and alcohol
  // which are handled specially
  matrix[N, J - 2] mu;
  matrix[3, K[7]] beta7;
  matrix[3, K[8]] beta8;

  // exponentiate counts
  // leave linear on identity
  mu[, 1] = exp(X1 * b1); // Life Stress
  mu[, 2] = exp(X2 * b2); // Lifetime Discrimination

  mu[, 3] = X3 * b3; // PSRs
  mu[, 4] = X4 * b4; // NegAff
  mu[, 5] = X5 * b5; // Sleep Duration
  mu[, 6] = X6 * b6; // Sleep Quality

  beta7 = append_row(zeros, b7); // Smoking
  beta8 = append_row(zeros2, b8); // Alcohol

  mu[,  9-2] = X9 * b9; // Physical Activity
  mu[, 10-2] = X10 * b10; // Allostatic Load
  mu[, 11-2] = X11 * b11; // Inflammation
  mu[, 12-2] = X11 * b12; // PSNS
  mu[, 13-2] = X11 * b13; // Lipids
  mu[, 14-2] = X11 * b14; // Glucose
  mu[, 15-2] = X11 * b15; // HPA
  mu[, 16-2] = X11 * b16; // SNS
  mu[, 17-2] = X11 * b17; // Cardiovascular (BP)

  mu[, 18-2] = exp(X18 * b18); // M2 Chronic Conditions
  mu[, 19-2] = exp(X19 * b19); // M3 Chronic Conditions
}

model {
  // use normal(0, 5) for counts and multinomial
  b1 ~ normal(0, 5);
  b2 ~ normal(0, 5);

  // use normal(0, 10) for continuous
  b3 ~ normal(0, 10);
  b4 ~ normal(0, 10);
  b5 ~ normal(0, 10);
  b6 ~ normal(0, 10);

  // to_vector for multinomial
  to_vector(b7) ~ normal(0, 5);
  to_vector(b8) ~ normal(0, 5);

  // continuous
  b9 ~ normal(0, 10);
  b10 ~ normal(0, 10);
  b11 ~ normal(0, 10);
  b12 ~ normal(0, 10);
  b13 ~ normal(0, 10);
  b14 ~ normal(0, 10);
  b15 ~ normal(0, 10);
  b16 ~ normal(0, 10);
  b17 ~ normal(0, 10);

  // chronic conditions
  b18 ~ normal(0, 5);
  b19 ~ normal(0, 5);

  y1 ~ neg_binomial_2(mu[, 1], phi1);
  y2 ~ neg_binomial_2(mu[, 2], phi2);

  y3 ~ normal(mu[, 3], e3);
  y4 ~ normal(mu[, 4], e4);
  y5 ~ normal(mu[, 5], e5);
  y6 ~ normal(mu[, 6], e6);

  for (n in 1:N) {
    y7[n] ~ categorical_logit(beta7 * X7[n]);
    y8[n] ~ categorical_logit(beta8 * X8[n]);
  }

  y9 ~ normal(mu[,   9-2], e9);
  y10 ~ normal(mu[, 10-2], e10);
  y11 ~ normal(mu[, 11-2], e11);
  y12 ~ normal(mu[, 12-2], e12);
  y13 ~ normal(mu[, 13-2], e13);
  y14 ~ normal(mu[, 14-2], e14);
  y15 ~ normal(mu[, 15-2], e15);
  y16 ~ normal(mu[, 16-2], e16);
  y17 ~ normal(mu[, 17-2], e17);

  y18 ~ neg_binomial_2(mu[, 18-2], phi18);
  y19 ~ neg_binomial_2(mu[, 19-2], phi19);
}
/*
generated quantities {
 vector[N] y1_rep;
 vector[N] y2_rep;
 vector[N] y3_rep;
 vector[N] y4_rep;
 vector[N] y5_rep;
 vector[N] y6_rep;


vector[N] y9_rep;
 vector[N] y10_rep;
 vector[N] y11_rep;
 vector[N] y12_rep;
 vector[N] y13_rep;
 vector[N] y14_rep;
 vector[N] y15_rep;
 vector[N] y16_rep;
 vector[N] y17_rep;

 vector[N] y18_rep;
 vector[N] y19_rep;

 for (n in 1:N) {
  // posterior draws to get posterior predictive checks

  y1_rep[n] = neg_binomial_2_rng(mu[n, 1], phi1);
  y2_rep[n] = neg_binomial_2_rng(mu[n, 2], phi2);

  y3_rep[n] = normal_rng(mu[n, 3], e3);
  y4_rep[n] = normal_rng(mu[n, 4], e4);
  y5_rep[n] = normal_rng(mu[n, 5], e5);
  y6_rep[n] = normal_rng(mu[n, 6], e6);


  y9_rep[n] = normal_rng(mu[n,   9-2], e9);
  y10_rep[n] = normal_rng(mu[n, 10-2], e10);
  y11_rep[n] = normal_rng(mu[n, 11-2], e11);
  y12_rep[n] = normal_rng(mu[n, 12-2], e12);
  y13_rep[n] = normal_rng(mu[n, 13-2], e13);
  y14_rep[n] = normal_rng(mu[n, 14-2], e14);
  y15_rep[n] = normal_rng(mu[n, 15-2], e15);
  y16_rep[n] = normal_rng(mu[n, 16-2], e16);
  y17_rep[n] = normal_rng(mu[n, 17-2], e17);


  y18_rep[n] = neg_binomial_2_rng(mu[n, 18-2], phi18);
  y19_rep[n] = neg_binomial_2_rng(mu[n, 19-2], phi19);
 }
}
*/
"

seeds <- c(403L, 2L, 90220047L, 656774934L, 1385624000L, 90329475L, 1516455841L,
628041616L, -723338130L, 1133406745L, -1257483605L, 357426842L,
-1649200548L, 675131231L, 758743349L, -779689780L, 131115554L,
-747355011L, 932601207L, 469881502L, -174128808L, 1704761531L,
907634265L, -1582443032L, -1739894762L, -665379951L, 206300227L,
1006721874L, 742572004L, 1834524199L, 639301245L, 87074676L,
1969822554L, 1956670885L, -551138881L, 42000966L, -889098032L,
369101555L, -989254639L, 834345536L, -1014759906L, -993781527L,
705362459L, 1587259946L, -1582777012L, 365944591L, -65813947L,
-1635032068L, 804657746L, 1165182925L)

m.comp <-   stan_model(
  model_code = midus_rcm_mod,
  save_dso = TRUE,
  auto_write = TRUE)

N <- nrow(X1[[1]])
J <- 19L
K <- c(ncol(X1[[1]]), ncol(X2[[1]]), ncol(X3[[1]]),
          ncol(X4[[1]]), ncol(X5[[1]]), ncol(X6[[1]]),
          ncol(X7[[1]]), ncol(X8[[1]]), ncol(X9[[1]]),
          ncol(X10[[1]]),
          ## re-use for all systems (x7)
          rep(ncol(X11[[1]]), 7),
          ncol(X18[[1]]), ncol(X19[[1]])
       )
stan.parameters <- c(
  "b1","phi1",
  "b2","phi2",

  "b3", "e3",
  "b4", "e4",
  "b5", "e5",
  "b6", "e6",

  "b7", "b8",

  "b9", "e9",
  "b10", "e10",
  "b11", "e11",
  "b12", "e12",
  "b13", "e13",
  "b14", "e14",
  "b15", "e15",
  "b16", "e16",
  "b17", "e17",

  "b18","phi18", #"y18_rep",
  "b19","phi19"#, "y19_rep"
)

save(
  midus_rcm_mod,
  J, K, m.comp, N, seeds, stan.parameters,
  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X18, X19,
  y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11,
  y12, y13, y14, y15, y16, y17, y18, y19,
  file = "../midus_rcm_stan_data.RData")

load("midus_rcm_stan_data.RData")

## rcm.fit <- lapply(1:1, function(i) {
##   stan.data <- list(
##     N = N,
##     J = J,
##     K = K,
##     X1 = X1[[i]], y1 = as.integer(y1[[i]]),
##     X2 = X2[[i]], y2 = as.integer(y2[[i]]),
##     X3 = X3[[i]], y3 = y3[[i]],
##     X4 = X4[[i]], y4 = y4[[i]],
##     X5 = X5[[i]], y5 = y5[[i]],
##     X6 = X6[[i]], y6 = y6[[i]],
##     X7 = X7[[i]], y7 = as.integer(y7[[i]]),
##     X8 = X8[[i]], y8 = as.integer(y8[[i]]),
##     X9 = X9[[i]], y9 = y9[[i]],
##     X10 = X10[[i]], y10 = y10[[i]],
##     ## re-use for all systems (x7)
##     X11 = X11[[i]],
##     y11 = y11[[i]], y12 = y12[[i]], y13 = y13[[i]], y14 = y14[[i]],
##     y15 = y15[[i]], y16 = y16[[i]], y17 = y17[[i]],
##     X18 = X18[[i]], y18 = as.integer(y18[[i]]),
##     X19 = X19[[i]], y19 = as.integer(y19[[i]])
##   )

##   out <- sampling(
##     m.comp,
##     data = stan.data,
##     pars = stan.parameters,
##     chains = 2, warmup = 5, init = "random",
##     iter = 20, thin = 1, seed = seeds[i],
##     cores = getOption("mc.cores", 1L))
##   saveRDS(out, file = sprintf("midus_rcm_fit_%d.RDS", i))
##   return(out)
## })
## saveRDS(rcm.fit, file = "stanfit.RDS")

rcm.fit <- readRDS("midus_rcm_fit_all.RDS")

launch_shinystan(rcm.fit[[1]])

tmp <- lapply(rcm.fit, extract, permute = TRUE)
tmp1 <- extract(rcm.fit[[1]], permute = TRUE)

tmp <- lapply(1:length(tmp[[1]]), function(i) {
  do.call(abind, c(lapply(tmp, function(x) x[[i]]), along = 1))
})
names(tmp) <- names(tmp1)
str(tmp)

formatRes <- function(b, X, trans = I) {
  res <- do.call(rbind.data.frame, apply(b, 2, function(x) param_summary(x, trans = trans)))
  rownames(res) <- colnames(X[[1]])
  as.data.frame(param_summary_format(res, pretty = TRUE), stringsAsFactors = FALSE)
}

mergerator <- function(index) {
  `%merge%` <- function(e1, e2) merge(e1, e2, all = TRUE)

  Reduce(`%merge%`, lapply(index, function(i) {
    x <- res.all.f[[i]]
    tmp <- do.call(rbind, strsplit(x[, 1], ", p "))
    tmp[, 2] <- gsub("= ", "", tmp[, 2])

    out <- as.data.table(cbind(IV = rownames(x), tmp))
    setnames(out, names(out), c("IV", paste0(c("b_e", "b_p"), i)))
    setkey(out, IV)
    out
  }))
}

jointTest <- function(x) {
  ## out <- Reduce(`&`, lapply(1:ncol(x), function(i) {
  ##   m <- mean(x[,i], na.rm = TRUE)
  ##   if (m > 0) x[, i] <= 0 else x[, i] >= 0
  ## }))
  ## min(prop.table(table(out)))^(1/ncol(x)) * 2
  wald.test(cov(x), colMeans(x), Terms = 1:ncol(x))
}

#### Should all be ~ 1.0
## jointTest(cbind(rnorm(1e5)))
## jointTest(cbind(rnorm(1e5), rnorm(1e5)))
## jointTest(cbind(rnorm(1e5), rnorm(1e5), rnorm(1e5)))
## jointTest(cbind(rnorm(1e5), rnorm(1e5), rnorm(1e5), rnorm(1e5)))

options(digits = 2, stringsAsFactors = FALSE)

res.all.f <- list(
  b1 = formatRes(tmp$b1, X1, exp), ## life stress
  b2 = formatRes(tmp$b2, X2, exp), ## lifetime discrimination
  b3 = formatRes(tmp$b3, X3), ## PSRs
  b4 = formatRes(tmp$b4, X4), ## negative affect
  b5 = formatRes(tmp$b5, X5), ## sleep duration
  b6 = formatRes(tmp$b6, X6), ## sleep quality
  b7a = formatRes(tmp$b7[, 1, ], X7, exp), ## current smoking (multinomial)
  b7b = formatRes(tmp$b7[, 2, ], X7, exp), ## current smoking (multinomial)
  b8a = formatRes(tmp$b8[, 1, ], X8, exp), ## current alcohol (multinomial)
  b8b = formatRes(tmp$b8[, 2, ], X8, exp), ## current alcohol (multinomial)
  b9  = formatRes(tmp$b9,  X9),  ## physical activity
  b10 = formatRes(tmp$b10, X10), ## AL
  b11 = formatRes(tmp$b11, X11), ## inflammation
  b12 = formatRes(tmp$b12, X11), ## PSNS
  b13 = formatRes(tmp$b13, X11), ## lipids
  b14 = formatRes(tmp$b14, X11), ## glucose
  b15 = formatRes(tmp$b15, X11), ## HPA
  b16 = formatRes(tmp$b16, X11), ## SNS
  b17 = formatRes(tmp$b17, X11), ## cardiovascular (BP)
  b18 = formatRes(tmp$b18, X18, exp), ## M2 Chronic Conditions
  b19 = formatRes(tmp$b19, X19, exp)) ## M3 Chronic Conditions


res.ps <- mergerator(1:4)
write.table(res.ps, file = "clipboard", sep = "\t", na="", row.names = FALSE)

res.behav <- mergerator(5:11)
write.table(res.behav, file = "clipboard", sep = "\t", na="", row.names = FALSE)

res.bio <- mergerator(12:19)
write.table(res.bio, file = "clipboard", sep = "\t", na="", row.names = FALSE)

res.cond <- mergerator(20:21)
write.table(res.cond, file = "clipboard", sep = "\t", na="", row.names = FALSE)

## SES -> Stress
colnames(X1[[1]])
colnames(X2[[1]])
jointTest(tmp$b2[, 7:8])
jointTest(cbind(tmp$b1[, 7:8], tmp$b2[, 7:8]))

## SES -> PSRs
colnames(X3[[1]])
jointTest(tmp$b3[, 7:8])

## Stress -> NA
colnames(X4[[1]])
jointTest(tmp$b4[, 9:10])

## SES -> Behaviors
colnames(X5[[1]])
colnames(X6[[1]])
colnames(X7[[1]])
colnames(X8[[1]])
colnames(X9[[1]])

jointTest(cbind(
  tmp$b5[, 7:8],
  tmp$b6[, 7:8],
  tmp$b7[, 1, 7:8],
  tmp$b7[, 2, 7:8],
  tmp$b8[, 1, 7:8],
  tmp$b8[, 2, 7:8],
  tmp$b9[, 7:8]))

## NA -> Behaviors
colnames(X5[[1]])
colnames(X6[[1]])
colnames(X7[[1]])
colnames(X8[[1]])
colnames(X9[[1]])

jointTest(cbind(
  tmp$b5[, 12],
  tmp$b6[, 12],
  tmp$b7[, 1, 12],
  tmp$b7[, 2, 12],
  tmp$b8[, 1, 12],
  tmp$b8[, 2, 12],
  tmp$b9[, 12]))

## PSRs -> Behaviors
colnames(X5[[1]])
colnames(X6[[1]])
colnames(X7[[1]])
colnames(X8[[1]])
colnames(X9[[1]])

jointTest(cbind(
  tmp$b5[, 11],
  tmp$b6[, 11],
  tmp$b7[, 1, 11],
  tmp$b7[, 2, 11],
  tmp$b8[, 1, 11],
  tmp$b8[, 2, 11],
  tmp$b9[, 11]))

## Stress -> Behaviors
colnames(X5[[1]])
colnames(X6[[1]])
colnames(X7[[1]])
colnames(X8[[1]])
colnames(X9[[1]])

jointTest(cbind(
  tmp$b5[, 9:10],
  tmp$b6[, 9:10],
  tmp$b7[, 1, 9:10],
  tmp$b7[, 2, 9:10],
  tmp$b8[, 1, 9:10],
  tmp$b8[, 2, 9:10],
  tmp$b9[, 9:10]))


## SES -> AL
colnames(X10[[1]])
jointTest(tmp$b10[, 7:8])

## Stress -> AL
colnames(X10[[1]])
jointTest(tmp$b10[, 9:10])

## SES -> M2 Conditions
colnames(X18[[1]])
jointTest(tmp$b18[, 7:8])

## Behavioral -> M2 Conditions
colnames(X18[[1]])
jointTest(tmp$b18[, 13:20])



## M2 Conditions
colnames(X18[[1]])
jointTest(tmp$b18[, 7:8]) ## SES
jointTest(tmp$b18[, 9:10]) ## stress
jointTest(tmp$b18[, 13:20]) ## health behaviors
jointTest(tmp$b18[, 21:28]) ## bio

## M3 Conditions
colnames(X19[[1]])
jointTest(tmp$b19[, 7:8]) ## SES
jointTest(tmp$b19[, 9:10]) ## stress
jointTest(tmp$b19[, 13:19]) ## health behaviors
jointTest(tmp$b19[, 20:27]) ## bio


pvalues.res <- c(
  SES.Stress = jointTest(cbind(tmp$b1[, 7:8], tmp$b2[, 7:8]))$result$chi2[["P"]], ## SES -> STRESS
  SES.PSRs = jointTest(tmp$b3[, 7:8])$result$chi2[["P"]], ## SES -> PSRs
  Stress.NA = jointTest(tmp$b4[, 9:10])$result$chi2[["P"]], # Stress -> NA
  PSRxStress.NA = 1, ## PSRs did not buffer Stress -> NA
  SES.HB = jointTest(cbind(
    tmp$b5[, 7:8],
    tmp$b6[, 7:8],
    tmp$b7[, 1, 7:8],
    tmp$b7[, 2, 7:8],
    tmp$b8[, 1, 7:8],
    tmp$b8[, 2, 7:8],
    tmp$b9[, 7:8]))$result$chi2[["P"]], ## SES -> Health Behaviors
  NAxHB = jointTest(cbind(
    tmp$b5[, 12],
    tmp$b6[, 12],
    tmp$b7[, 1, 12],
    tmp$b7[, 2, 12],
    tmp$b8[, 1, 12],
    tmp$b8[, 2, 12],
    tmp$b9[, 12]))$result$chi2[["P"]],## NA -> Behaviors
  PSRs.HB = jointTest(cbind(
    tmp$b5[, 11],
    tmp$b6[, 11],
    tmp$b7[, 1, 11],
    tmp$b7[, 2, 11],
    tmp$b8[, 1, 11],
    tmp$b8[, 2, 11],
    tmp$b9[, 11]))$result$chi2[["P"]], ## PSRs -> Behaviors
  SES.AL = jointTest(tmp$b10[, 7:8])$result$chi2[["P"]], ## SES -> AL
  PSRs.AL = jointTest(tmp$b10[, 11, drop=FALSE])$result$chi2[["P"]], ## PSR -> AL
  NA.AL = jointTest(tmp$b10[, 12, drop=FALSE])$result$chi2[["P"]], ## NA -> AL
  SES.M2 = jointTest(tmp$b18[, 7:8])$result$chi2[["P"]], ## SES -> M2
  HB.M2 = jointTest(tmp$b18[, 13:20])$result$chi2[["P"]], ## health behaviors -> M2
  AL.M2 = jointTest(tmp$b18[, 21, drop=FALSE])$result$chi2[["P"]], ## bio  -> M2
  SES.M3 = jointTest(tmp$b19[, 7:8])$result$chi2[["P"]], ## SES -> M3
  HB.M3 = jointTest(tmp$b19[, 13:19])$result$chi2[["P"]], ## health behaviors -> M3
  AL.M3 = jointTest(tmp$b19[, 20, drop=FALSE])$result$chi2[["P"]]) ## bio -> M3

  Stress.HB = jointTest(cbind(
    tmp$b5[, 9:10],
    tmp$b6[, 9:10],
    tmp$b7[, 1, 9:10],
    tmp$b7[, 2, 9:10],
    tmp$b8[, 1, 9:10],
    tmp$b8[, 2, 9:10],
    tmp$b9[, 9:10]))$result$chi2[["P"]], ## Stress -> Behaviors
  Stress.AL = jointTest(tmp$b10[, 9:10])$result$chi2[["P"]], ## Stress -> AL
Stress.M2 = jointTest(tmp$b18[, 9:10])$result$chi2[["P"]], ## stress -> M2
Stress.M3 = jointTest(tmp$b19[, 9:10])$result$chi2[["P"]], ## stress -> M3

as.matrix(formatPval(p.adjust(pvalues.res, method = "BH"))) ## used this
as.matrix(formatPval(p.adjust(pvalues.res, method = "holm")))
