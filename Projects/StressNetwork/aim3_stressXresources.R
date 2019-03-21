## set number of cores to use for random forests
## options(rf.cores = 40)
source("projectwide_setup.R")
options(width = 80)




  summary(m0 <- gam(LastAge ~ #s(PR, k = 10) + s(SR, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED))

  summary(m1 <- gam(LastAge ~ s(PR, k = 10) + s(SR, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED))

  summary(m1b <- gam(LastAge ~ s(PRSF, k = 10) + s(SRSF, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED))


anova(m0, m1, test = "LRT")

anova(m1, m1b)

testit <- function(f = "LastAge ~ s(AGE, k = 5) + Sex + RaceG3", v1 = "LifeStress", v2 = "PR", k1 = 10, k2 = 10) {
  f1 <- paste0(f, sprintf(" + s(%s, k = %d)", v1, k1))
  f2 <- paste0(f, sprintf(" + s(%s, k = %d) + s(%s, k = %d)", v1, k1, v2, k2))
  f3 <- paste0(f, sprintf(" + s(%s, k = %d) + s(%s, k = %d) + ti(%s, %s, k = %d)", v1, k1, v2, k2, v1, v2, k1))
  f4 <- paste0(f, sprintf(" + te(%s, %s, k = %d)", v1, v2, k1))

  m1 <- gam(as.formula(f1),
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED)
  m2 <- gam(as.formula(f2),
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED)
  m3 <- gam(as.formula(f3),
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED)
  m4 <- gam(as.formula(f4),
            data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
            family = cox.ph(link = "identity"), weights = DECEASED)

  list(m1, m2, m3, m4,
       T1 = anova(m1, m2, test = "LRT"),
       T2 = anova(m2, m3, test = "LRT"),
       T3 = anova(m2, m4, test = "LRT"))
}

testit2 <- function(f = "Rb4bcrp ~ s(AGE, k = 5) + Sex + RaceG3",
                    v1 = "LifeStress", v2 = "PR", k1 = 10, k2 = 10,
                    dat = subset(tmp.d.all, !is.na(Rb4bcrp) & !is.na(PR) & !is.na(PRSF))) {
  f1 <- paste0(f, sprintf(" + s(%s, k = %d)", v1, k1))
  f2 <- paste0(f, sprintf(" + s(%s, k = %d) + s(%s, k = %d)", v1, k1, v2, k2))
  f3 <- paste0(f, sprintf(" + s(%s, k = %d) + s(%s, k = %d) + ti(%s, %s, k = %d)", v1, k1, v2, k2, v1, v2, k1))
  f4 <- paste0(f, sprintf(" + te(%s, %s, k = %d)", v1, v2, k1))

  m1 <- gam(as.formula(f1), data = dat)
  m2 <- gam(as.formula(f2), data = dat)
  m3 <- gam(as.formula(f3), data = dat)
  m4 <- gam(as.formula(f4), data = dat)

  list(m1, m2, m3, m4,
       T1 = anova(m1, m2, test = "LRT"),
       T2 = anova(m2, m3, test = "LRT"),
       T3 = anova(m2, m4, test = "LRT"))
}


lifestresspr <- testit(v1 = "LifeStress", v2 = "PR", k1 = 10, k2 = 10)
lifestresspr[5:7]

lifestresssr <- testit(v1 = "LifeStress", v2 = "SR", k1 = 10, k2 = 10)
lifestresssr[5:7]

pss.pr <- testit(v1 = "B4QPS_PS", v2 = "PR", k1 = 10, k2 = 10)
pss.pr[5:7]

pss.sr <- testit(v1 = "B4QPS_PS", v2 = "SR", k1 = 10, k2 = 10)
pss.sr[5:7]

dneiqu.pr <- testit(v1 = "dNeiQu", v2 = "PR", k1 = 10, k2 = 10)
dneiqu.pr[5:7]

dneiqu.sr <- testit(v1 = "dNeiQu", v2 = "SR", k1 = 10, k2 = 10)
dneiqu.sr[5:7]


crplifestresspr <- testit2(v1 = "LifeStress", v2 = "PR", k1 = 10, k2 = 10)
crplifestresspr[5:7]

crplifestresssr <- testit2(v1 = "LifeStress", v2 = "SR", k1 = 10, k2 = 10)
crplifestresssr[5:7]

crplifestressipr <- testit2(v1 = "LifeStressImpactS", v2 = "PR", k1 = 10, k2 = 10)
crplifestressipr[5:7]

crplifestressisr <- testit2(v1 = "LifeStressImpactS", v2 = "SR", k1 = 10, k2 = 10)
crplifestressisr[5:7]

(crpdaipr <- testit2(v1 = "B1SDAYDI", v2 = "PR", k1 = 10, k2 = 10))[5:7]
(crpdaisr <- testit2(v1 = "B1SDAYDI", v2 = "SR", k1 = 10, k2 = 10))[5:7]



summary(m0 <- gam(,
                  data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
                  family = cox.ph(link = "identity"), weights = DECEASED))
summary(m1 <- gam(LastAge ~ s(LifeStress, k = 10) + s(PR, k = 10) +
                    s(AGE, k = 5) + Sex + RaceG3,
                  data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
                  family = cox.ph(link = "identity"), weights = DECEASED))
summary(m2 <- gam(LastAge ~ s(LifeStress, k = 10) + s(PR, k = 10) +
                    ti(LifeStress, PR, k = 10) +
                    s(AGE, k = 5) + Sex + RaceG3,
                  data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
                  family = cox.ph(link = "identity"), weights = DECEASED))
summary(m3 <- gam(LastAge ~ te(LifeStress, PR, k = 10) +
                    s(AGE, k = 5) + Sex + RaceG3,
                  data = subset(tmp.d.all, !is.na(LastAge) & !is.na(PR) & !is.na(PRSF)),
                  family = cox.ph(link = "identity"), weights = DECEASED))

anova(m0, m1, test = "LRT")
anova(m1, m2, test = "LRT")
anova(m1, m3, test = "LRT")



  summary(gam(LastAge ~ s(PRSF, k = 10) + s(SRSF, k = 10) +
                s(AGE, k = 5) + Sex + RaceG3,
              data = tmp.d.all,
              family = cox.ph(link = "identity"), weights = DECEASED))


  summary(m1 <- gam(LastAge ~ s(LifeStress, k = 5) + s(PR, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = tmp.d.all,
            family = cox.ph(link = "identity"), weights = DECEASED))

  summary(m2 <- gam(LastAge ~ s(LifeStress, k = 5) + s(PR, k = 10) +
                      ti(LifeStress, PR, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = tmp.d.all,
            family = cox.ph(link = "identity"), weights = DECEASED))

  summary(m3 <- gam(LastAge ~ te(LifeStress, PR, k = 10) +
              s(AGE, k = 5) + Sex + RaceG3,
            data = tmp.d.all,
            family = cox.ph(link = "identity"), weights = DECEASED))
anova(m1, m2, test = "LRT")

