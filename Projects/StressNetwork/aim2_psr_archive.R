source("projectwide_setup.R")
options(width = 150)

##{{{ Setup

## Some potentially relevant codes to make an Age year at death variable

## ## Add Mortality Data in
## ## make overall birth year
## d1byr <- within(d1[, c("M2ID", "A1PBYEAR")], {
##   A1PBYEAR <- ifelse(A1PBYEAR == 9998, NA, A1PBYEAR)
## })
## ## d2byr <- within(d2[, c("M2ID", "B1PBYEAR")], {
## ##   B1PBYEAR <- ifelse(B1PBYEAR == 9998, NA, B1PBYEAR)
## ## })
## dmbyr <- dm[, c("M2ID", "BACBYR")]
## colnames(d1byr) <- colnames(dmbyr) <- c("M2ID", "BirthYear")
## d.all <- merge(d.all, rbind(d1byr, dmbyr), by = "M2ID", all = TRUE)
## d.all <- merge(d.all, d3.mort[, c("M2ID", "DOD_Y")], all = TRUE)

## d.all$DECEASED <- ifelse(d.all$M2ID %in% d3.mort$M2ID, 1, 0)
## d.all$LastAge <- ifelse(is.na(d.all$DOD_Y) & (d.all$DECEASED == 0), 2015, d.all$DOD_Y) - d.all$BirthYear


## midus3.mort <- read.spss("~/OneDrive/Projects/MIDUS/ICPSR_36346/DS0001/MIDUS_MortalityData_20151007.sav",
##                          to.data.frame = TRUE)
## midus3.mort <- within(midus3.mort, {
##   DDate <- as.Date(paste0(DOD_Y, "-", DOD_M, "-15"), "%Y-%m-%d")
##   DSurv <- Surv(as.numeric(DDate - as.Date("1995-03-01")), I(DECEASED == "YES"))
## })
## plot(survfit(DSurv ~ 1, data = midus3.mort))


midus.psr.items <- read.csv("midus_psr_items.csv", stringsAsFactors = FALSE)
midus.psr.items[] <- lapply(midus.psr.items, function(x) ifelse(nzchar(x), x, NA))
midus.psr.items <- within(midus.psr.items, {
  ItemAbbr <- paste0(Abbr, unlist(sapply(rle(Abbr)$lengths, function(x) 1:x)))
})

table(na.omit(midus.psr.items$MIDUS1) %in% colnames(d1))
table(midus.psr.items$MIDUS2 %in% colnames(d2))
table(midus.psr.items$Milwaukee %in% colnames(dm))


## Item Distribution Plots
m2.items <- melt(d2[, midus.psr.items$MIDUS2])
m2.items <- merge(m2.items, midus.psr.items[, c("Scale", "MIDUS2")],
                  by.x = "variable", by.y = "MIDUS2", all = TRUE)

## ggplot(na.omit(m2.items), aes(variable, fill = factor(value))) +
##   geom_bar() +
##   facet_wrap(~Scale, scales = "free", ncol = 1)
## ggplot(subset(na.omit(m2.items), value <= 7), aes(factor(value))) +
##   geom_bar() +
##   facet_wrap(~ variable)
## str(m2.items)

## Setup
m2.items <- d2[, midus.psr.items$MIDUS2]
ma.items <- dm[, midus.psr.items$Milwaukee]
colnames(m2.items) <- colnames(ma.items) <- midus.psr.items$ItemAbbr
m2a.items <- rbind(m2.items, ma.items)

m2a.items[] <- lapply(m2a.items, function(x) ifelse(x > 7, NA, x))

m2a.items[subset(midus.psr.items, Abbr %in% c("FamS", "FriS", "SpoS"))$ItemAbbr] <- lapply(m2a.items[, subset(midus.psr.items, Abbr %in% c("FamS", "FriS", "SpoS"))$ItemAbbr],
                                                                                           function(x) ifelse(x > 4, NA, x))

m2a.items$M2ID <- c(d2$M2ID, dm$M2ID)
m2a.items$M2FAMNUM <- c(d2$M2FAMNUM, dm$M2FAMNUM)


## plot(SEMSummary(~ ., data = m2a.items[, midus.psr.items$ItemAbbr], use = "pairwise.complete.obs"))
## ## PCA
## screeplot(prcomp(na.omit(m2a.items[, midus.psr.items$ItemAbbr]), scale.=TRUE),
##           npcs = 20,
##           type = "lines")
## abline(h = 1)

## plot(princomp(na.omit(m2a.items[, subset(midus.psr.items, !Abbr %in% c("Neu", "Ope", "Agr"))$ItemAbbr]), cor=TRUE))

## plot(princomp(na.omit(m2a.items[, subset(midus.psr.items, !Abbr %in% c("Neu", "Ope", "Agr", "Con", "Ext"))$ItemAbbr]), cor=TRUE))

## plot(princomp(na.omit(m2a.items[, subset(midus.psr.items, Abbr %in% c("M", "LC", "SE", "Opt", "Pess", "FamS", "FriS", "SpoS", "SI"))$ItemAbbr]), cor=TRUE))

tmp.items <- subset(midus.psr.items, Abbr %in% c("M", "LC", "SE", "Opt", "Pess", "FamS", "FriS", "SpoS", "SI", "Con", "Ext", "PLife", "Ctrl"))
tmpdat <- m2a.items[, c("M2ID", "M2FAMNUM", tmp.items$ItemAbbr)]

##tmpdat$Died <- ifelse(tmpdat$M2ID %in% midus3.mort$M2ID, 1, 0)
tmpdat$AGE <- c(d2$B1PAGE_M2, dm$BACRAGE)

## drop people missing all
nrow(tmpdat)
dropindex <- rowMeans(is.na(subset(tmpdat, select = -c(M2ID, M2FAMNUM, AGE))))
table(dropindex == 1)
tmpdat <- tmpdat[dropindex < 1, ]
nrow(tmpdat)

## do not want to development data to have members from same family
tmpdat <- tmpdat[order(tmpdat$M2FAMNUM, tmpdat$M2ID), ]
index <- unlist(lapply(rle(tmpdat$M2FAMNUM)$lengths, function(x) 1:x))
table(index)

## validation
tmpdat.valid <- subset(tmpdat, index == 2)
## development
tmpdat <- subset(tmpdat, index == 1)

if (FALSE) {
tmpdat <- merge(tmpdat, midus3.mort[, c("M2ID", "DDate")], by = "M2ID", all.x = TRUE)
tmpdat$BYR <- c(d2$B1PBYEAR, dm$BACBYR)
tmpdat$BYR <- ifelse(tmpdat$BYR > 2000, NA, tmpdat$BYR)
tmpdat$DAge <- ifelse(tmpdat$M2ID %in% midus3.mort$M2ID,
                      tmpdat$DDate - as.Date(paste0(tmpdat$BYR, "-06-01")),
                      as.Date("2015-05-15") - as.Date(paste0(tmpdat$BYR, "-06-01")))/365.25

summary(coxph(Surv(DAge, Died) ~ AGE, data = tmpdat))

summary(coxph(Surv(DAge, Died) ~ BYR, data = tmpdat))
summary(glm(Died ~ AGE, data = tmpdat,family = binomial()))
summary(glm(Died ~ BYR, data = tmpdat,family = binomial()))
plot(survfit(Surv(DAge, Died) ~ factor(AGE > median(AGE, na.rm = TRUE)), data = tmpdat))
}


## plot(SEMSummary(~ ., data = tmpdat[,-which(colnames(tmpdat) %in% c("M2ID", "M2FAMNUM", "Died", "AGE"))], use = "pairwise.complete.obs"))
## plot(SEMSummary(~ ., data = tmpdat[,-which(colnames(tmpdat) %in% c("M2ID", "M2FAMNUM", "Died", "AGE"))], use = "pairwise.complete.obs"), order = "asis")
## plot(SEMSummary(~ ., data = tmpdat[,-(1:2)], use = "pairwise.complete.obs"), order = "asis")

cd(base, pre <- "psr_", num <- "imputed")
##}}}


##{{{ Preliminary Analyses
m1efa <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
 Ctrl1 Ctrl2 Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
 Ctrl1 Ctrl2 Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = EFA 1 16;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m1efafit <- mplusModeler(m1efa, "MIDUS_PSR_efa1to16.dat", run=TRUE)
m1efafit$results$summaries

m1efares <- data.frame(
  NFactors = 1:16,
  CFI = c(.641, .737, .793, .834, .865, .893, .914, NA, .946, .962, .970, .977, .981, .984, NA, .988),
  RMSEA = c(.095, .083, .075, .068, .063, .057, .052, NA, .043, .037, .033, .030, .028, .026, NA, .023),
  RMSEALL = c(.095, .082, .074, .068, .062, .056, .051, NA, .042, .036, .032, .029, .027, .025, NA, .022),
  RMSEAUL = c(.096, .084, .076, .069, .063, .058, .053, NA, .043, .037, .034, .031, .028, .027, NA, .024),
  SRMR = c(.117, .095, .078, .062, .055, .045, .038, NA, .027, .022, .020, .018, .016, .015, NA, .013),
  NParams = c(62, 123, 183, 242, 300, 357, 416, NA, 522, 575, 627, 678, 728, 777, NA, 872),
  Chi2 = c(62179.004, 45980.306, 36569.554, 29588.096, 24311.617, 19540.365, 15913.642, NA, 10368.973, 7687.703, 6332.385, 5143.881, 4366.977, 3855.568, NA, 2982.589),
  DF = c(1829, 1768, 1708, 1649, 1591, 1534, 1478, NA, 1369, 1316, 1264, 1213, 1163, 1114, NA, 1019)
)

write.table(m1efares, file = "clipboard", sep = "\t")

m1efares.long <- melt(m1efares[, c("NFactors", "CFI", "RMSEA", "SRMR")], id.vars = "NFactors")
m1efares.long$variable <- factor(m1efares.long$variable, levels = c("CFI", "RMSEA", "SRMR"), labels = c("1 - CFI", "RMSEA", "SRMR"))

ggplot(na.omit(m1efares.long), aes(NFactors, value, linetype = variable, shape = variable)) +
  geom_line() + geom_point() +
  scale_linetype("") + scale_shape("") +
  scale_x_continuous(breaks = 1:16) +
  theme_classic() +
  xlab("Number of Factors") +
  ylab("Performance") +
  ggtitle("1 - 16 Factor EFA Results") +
  theme(legend.key.width = unit(1, "cm"))



## m1efa2 <- mplusObject(
## VARIABLE = "
## USEVARIABLES ARE
##  M1 M2 M3 M4
##  LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
##  SE1 SE2 SE3 SE4 SE5 SE6 SE7
##  Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
##  PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
##  Ctrl1 Ctrl2 Ctrl3
##  Con1 Con2 Con3 Con4 Con5
##  Ext1 Ext2 Ext3 Ext4 Ext5
##  FamS1 FamS2 FamS3 FamS4
##  FriS1 FriS2 FriS3 FriS4
##  SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
##  SI1 SI2 SI3;

## CATEGORICAL ARE
##  M1 M2 M3 M4
##  LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
##  SE1 SE2 SE3 SE4 SE5 SE6 SE7
##  Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
##  PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
##  Ctrl1 Ctrl2 Ctrl3
##  Con1 Con2 Con3 Con4 Con5
##  Ext1 Ext2 Ext3 Ext4 Ext5
##  FamS1 FamS2 FamS3 FamS4
##  FriS1 FriS2 FriS3 FriS4
##  SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
##  SI1 SI2 SI3;

## IDVARIABLE = M2ID;
## CLUSTER = M2FAMNUM;
## ",
## ANALYSIS = "
##   TYPE = COMPLEX EFA 2 16;
##   ROTATION = BI-GEOMIN (oblique);
##   ESTIMATOR = WLSMV;
##   PROCESSORS = 4;",
## MODEL = "
## ",
## OUTPUT = "STDYX;",
## usevariables = colnames(tmpdat),
## rdata = tmpdat)

## m1efafit2 <- mplusModeler(m1efa2, "MIDUS_PSR_cfa1efa2.dat", run=TRUE)


m1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
 Ctrl1 Ctrl2 Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 PLife1 PLife2 PLife3 PLife4 PLife5 PLife6 PLife7
 Ctrl1 Ctrl2 Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1 SE1*0;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6 PLife7 PLife1;
 Ctrl BY Ctrl1@1 Ctrl2 Ctrl3;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m1fit <- mplusModeler(m1, "MIDUS_PSR_cfa1.dat", run=TRUE)
m1fit$results$summaries

subset(coef(m1fit$results, type = "stdyx", param = "loading"), abs(est) < .4)

m1b <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6;
 Ctrl BY Ctrl1@1 Ctrl2;
 Ctrl@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m1bfit <- mplusModeler(m1b, "MIDUS_PSR_cfa1b.dat", run=TRUE)
m1bfit$results$summaries

subset(coef(m1bfit$results, type = "stdyx", param = "loading"), abs(est) < .4)

m2 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6 ;
 Ctrl BY Ctrl1@1 Ctrl2;
 Ctrl@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M LC SE Opt Pess PLife Ctrl Con EXT;
 SR BY FamS FriS SpoS SI;
 PR WITH SR*;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2fit <- mplusModeler(m2, "MIDUS_PSR_cfa2.dat", run=TRUE)
m2fit$results$summaries


m2b <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6 ;
 Ctrl BY Ctrl1@1 Ctrl2;
 Ctrl@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;

 PSR BY M LC SE Opt Pess PLife Ctrl Con EXT
   FamS FriS SpoS SI;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2bfit <- mplusModeler(m2b, "MIDUS_PSR_cfa2b.dat", run=TRUE)
m2bfit$results$summaries

coef(m2fit$results, type = "stdyx", param = "loading")
coef(m2bfit$results, type = "stdyx", param = "loading")



m2c <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6 ;
 !Ctrl BY Ctrl1@1 Ctrl2;
 !Ctrl@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M LC SE Opt Pess PLife Con EXT;
 SR BY FamS FriS SpoS SI;
 PR WITH SR*;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2cfit <- mplusModeler(m2c, "MIDUS_PSR_cfa2c.dat", run=TRUE)
m2cfit$results$summaries
##}}}

##{{{ Full Form Analyses and Validation
m2c2<- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3
 !M4
 LC2 LC5 LC7
 !LC1 LC3 LC4 LC6 LC8
 !SE1
 SE2 SE4 SE6
 !SE3 SE5 SE7
 Opt1 Opt2 Opt3
 !Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife5
 !PLife4 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con2 Con3 Con5
 !Con1 Con4
 Ext1 Ext3 Ext4
 !Ext2 Ext5
 FamS1 FamS2 FamS4
 !FamS3
 FriS1 FriS2 FriS4
 !FriS3
 SpoS1 SpoS2 SpoS3
 !SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3
 !M4
 LC2 LC5 LC7
 !LC1 LC3 LC4 LC6 LC8
 !SE1
 SE2 SE4 SE6
 !SE3 SE5 SE7
 Opt1 Opt2 Opt3
 !Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife5
 !PLife4 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con2 Con3 Con5
 !Con1 Con4
 Ext1 Ext3 Ext4
 !Ext2 Ext5
 FamS1 FamS2 FamS4
 !FamS3
 FriS1 FriS2 FriS4
 !FriS3
 SpoS1 SpoS2 SpoS3
 !SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3;
 LC BY LC2@-1 LC5*-1 LC7*-1;
 SE BY SE2@1 SE4*1 SE6*1;
 Opt BY Opt1 Opt2 Opt3;
 !Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife5;
 Con BY Con2@1 Con3 Con5;
 EXT BY Ext1@1 Ext3 Ext4;
 FamS BY FamS1 FamS2 FamS4;
 FriS BY FriS1 FriS2 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M LC SE Opt !Pess
   PLife Con EXT;
 SR BY FamS FriS SpoS SI;
 PSR BY PR SR;
 PSR@1;
 !PR WITH SR*;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2c2fit <- mplusModeler(m2c2, "MIDUS_PSR_cfa2c2.dat", run=TRUE)
m2c2fit$results$summaries

##}}}

## m2c2sa<- mplusObject(
## VARIABLE = "
## USEVARIABLES ARE
##  M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
##  FamS2 FriS2 SpoS3 SI2;

## CATEGORICAL ARE
##  M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
##  FamS2 FriS2 SpoS3 SI2;

## IDVARIABLE = M2ID;
## !CLUSTER = M2FAMNUM;
## ",
## ANALYSIS = "
##   !TYPE = COMPLEX;
##   ESTIMATOR = WLSMV;
##   PROCESSORS = 2;",
## MODEL = "
##  PR@1;
##  PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*
##  !;
##  !SR@1;
##  !SR BY
##  FamS2*1 FriS2*1 SpoS3*1 SI2*1;

##  !PR WITH SR*;
## ",
## OUTPUT = "STDYX;",
## PLOT = "TYPE IS PLOT2;",
## usevariables = colnames(tmpdat),
## rdata = tmpdat)
## m2c2safit <- mplusModeler(m2c2sa, "MIDUS_PSR_cfa2c2sa.dat", run=TRUE)
## m2c2safit$results$summaries
## coef(m2c2safit$results, type = "stdyx", param = "loading")


## m2c2sb<- mplusObject(
## VARIABLE = "
## USEVARIABLES ARE
##  M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
##  FamS2 FriS2 SpoS3 SI2;

## CATEGORICAL ARE
##  M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
##  FamS2 FriS2 SpoS3 SI2;

## IDVARIABLE = M2ID;
## !CLUSTER = M2FAMNUM;
## ",
## ANALYSIS = "
##   !TYPE = COMPLEX;
##   ESTIMATOR = WLSMV;
##   PROCESSORS = 2;",
## MODEL = "
##  PR@1;
##  PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*;
##  SR@1;
##  SR BY FamS2*1 FriS2*1 SpoS3*1 SI2*1;

##  PR WITH SR*;
## ",
## OUTPUT = "STDYX;",
## PLOT = "TYPE IS PLOT2;",
## usevariables = colnames(tmpdat),
## rdata = tmpdat)
## m2c2sbfit <- mplusModeler(m2c2sb, "MIDUS_PSR_cfa2c2sb.dat", run=TRUE)
## m2c2sbfit$results$summaries
## coef(m2c2sbfit$results, type = "stdyx", param = "loading")
## coef(m2c2sbfit$results, type = "stdyx")[1:12, ]

m2c2sc <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

CATEGORICAL ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

IDVARIABLE = M2ID;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 PR@1;
 PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*;
 SR@1;
 SR BY FamS2*1 FriS2 SpoS3 SI2 SI1;

 PSR BY PR SR;
 PSR@1;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2c2scfit <- mplusModeler(m2c2sc, "MIDUS_PSR_cfa2c2sc.dat", run=TRUE)
m2c2scfit$results$summaries

coef(m2c2scfit$results, type = "stdyx", param = "loading")
coef(m2c2scfit$results, type = "stdyx")[1:13, ]



alpha(polychoric(na.omit(tmpdat[, c("M2", "LC2", "SE2", "Opt2", "PLife2", "Con3", "Ext4")]), global = FALSE)$rho, check.keys = TRUE)

## alpha(polychoric(na.omit(tmpdat[, c("FamS2", "FriS2", "SpoS3", "SI2")]), global = FALSE)$rho, check.keys = TRUE)
alpha(polychoric(na.omit(tmpdat[, c("FamS2", "FriS2", "SpoS3", "SI2", "SI1")]), global = FALSE)$rho, check.keys = TRUE)

alpha(polychoric(na.omit(tmpdat[, c("FamS1", "FamS2", "FriS1", "FriS2", "SpoS1", "SpoS3", "SI1", "SI2")]), global = FALSE)$rho, check.keys = TRUE)
alpha(cor(with(na.omit(tmpdat[, c("FamS1", "FamS2", "FriS1", "FriS2", "SpoS1", "SpoS3", "SI1", "SI2")]), {
  data.frame(Fam = FamS1 + FamS2,
             Fri = FriS1 + FriS2,
             Spo = SpoS1 + SpoS3,
             SI = SI2 - SI1)
})), check.keys = TRUE)
corplot(polychoric(na.omit(tmpdat[, c("FamS1", "FamS2", "FamS4", "FriS1", "FriS2", "FriS4", "SpoS1", "SpoS2", "SpoS3", "SI1", "SI2", "SI3")]), global = FALSE)$rho, order = "asis")


alpha(polychoric(na.omit(tmpdat[, c("M2", "LC2", "SE2", "Opt2", "PLife2", "Con3", "Ext4", "FamS2", "FriS2", "SpoS3", "SI2")]), global = FALSE)$rho, check.keys = TRUE)


alpha(cor(with(na.omit(tmpdat[, c("FamS1", "FamS2", "FriS1", "FriS2", "SpoS1", "SpoS3", "SI1", "SI2")]), {
  data.frame(Fam = scale(FamS1) + scale(FamS2),
             Fri = scale(FriS1) + scale(FriS2),
             Spo = scale(SpoS1) + scale(SpoS3),
             SI = scale(SI2) - scale(SI1))
})), check.keys = TRUE)



alpha(polychoric(na.omit(tmpdat[, c("FamS2", "FriS2", "SpoS3", "SI2", "SI1")]), global = FALSE)$rho, check.keys = TRUE)







##{{{ Archive

m2d <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3 M4;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt BY Opt1 Opt2 Opt3;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;
 PLife BY PLife2@1 PLife3 PLife4 PLife5 PLife6 ;
 !Ctrl BY Ctrl1@1 Ctrl2;
 !Ctrl@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;
 EXT BY Ext1@1 Ext2 Ext3 Ext4 Ext5;
 FamS BY FamS1 FamS2 FamS3 FamS4;
 FriS BY FriS1 FriS2 FriS3 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6;
 SI BY SI1@-1 SI2*1 SI3*1;

 PSR BY M LC SE Opt Pess PLife Con EXT
   FamS FriS SpoS SI;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2dfit <- mplusModeler(m2d, "MIDUS_PSR_cfa2d.dat", run=TRUE)
m2dfit$results$summaries

coef(m2cfit$results, type = "stdyx", param = "loading")
coef(m2dfit$results, type = "stdyx", param = "loading")




m0 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
  G@1;
   G BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5
   FamS1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m0fit <- mplusModeler(m0, "MIDUS_PSR_cfa0.dat", run=TRUE)
m0fit$results$summaries




m3 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M@1;
 M BY M1* M2 M3 M4;
 LC@1;
 LC BY LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE@1;
 SE BY SE2*1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt@1;
 Opt BY Opt1*1 Opt2*1 Opt3*1;
 Pess@1;
 Pess BY Pess1*-1 Pess2*-1 Pess3*-1;
 PLife@1;
 PLife BY PLife2*1 PLife3 PLife4 PLife5 PLife6 ;
 !Ctrl@1;
 !Ctrl BY Ctrl1@1 Ctrl2;
 Con@1;
 Con BY Con1*1 Con2 Con3 Con4 Con5;
 Ext@1;
 Ext BY Ext1*1 Ext2 Ext3 Ext4 Ext5;

 FamS@1;
 FamS BY FamS1*1 FamS2*1 FamS3*1 FamS4*1;
 FriS@1;
 FriS BY FriS1*1 FriS2*1 FriS3*1 FriS4*1;
 SpoS@1;
 SpoS BY SpoS1*1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;
 SI@1;
 SI BY SI1*-1 SI2*1 SI3*1;

  G@1;
   G BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   !Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5
   FamS1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

 G WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

M WITH LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

LC WITH SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
SE WITH Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
Opt WITH Pess@0
  PLife@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0
  SI@0;
Pess WITH PLife@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
PLife WITH Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Con WITH Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Ext WITH  FamS@0 FriS@0 SpoS@0 SI@0;
FamS WITH FriS@0 SpoS@0 SI@0;
FriS WITH SpoS@0 SI@0;
SpoS WITH SI@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3fit <- mplusModeler(m3, "MIDUS_PSR_cfa3.dat", run=TRUE)
m3fit$results$summaries


m3b <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 !Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
!CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  !TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M@1;
 M BY M1* M2 M3 M4;
 LC@1;
 LC BY LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;
 SE@1;
 SE BY SE2*1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;
 Opt@1;
 Opt BY Opt1*1 Opt2*1 Opt3*1;
 Pess@1;
 Pess BY Pess1*-1 Pess2*-1 Pess3*-1;
 PLife@1;
 PLife BY PLife2*1 PLife3 PLife4 PLife5 PLife6 ;
 !Ctrl@1;
 !Ctrl BY Ctrl1@1 Ctrl2;
 Con@1;
 Con BY Con1*1 Con2 Con3 Con4 Con5;
 Ext@1;
 Ext BY Ext1*1 Ext2 Ext3 Ext4 Ext5;

 FamS@1;
 FamS BY FamS1*1 FamS2*1 FamS3*1 FamS4*1;
 FriS@1;
 FriS BY FriS1*1 FriS2*1 FriS3*1 FriS4*1;
 SpoS@1;
 SpoS BY SpoS1*1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;
 SI@1;
 SI BY SI1*-1 SI2*1 SI3*1;

  PR@1;
   PR BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   !Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5;

  SR@1;
   SR BY FamS1* FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

 PR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
 SR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

 PR WITH SR*;

M WITH LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

LC WITH SE@0 Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
SE WITH Opt@0
  Pess@0 PLife@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
Opt WITH Pess@0
  PLife@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0
  SI@0;
Pess WITH PLife@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
PLife WITH Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Con WITH Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Ext WITH  FamS@0 FriS@0 SpoS@0 SI@0;
FamS WITH FriS@0 SpoS@0 SI@0;
FriS WITH SpoS@0 SI@0;
SpoS WITH SI@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3bfit <- mplusModeler(m3b, "MIDUS_PSR_cfa3b.dat", run=TRUE)
m3bfit$results$summaries
coef(m3bfit$results, type = "stdyx", param = "loading")
coef(m3bfit$results, type = "stdyx", param = "un")





L <- coef(m2cfit$results, type = "stdyx", param = "loading")
L <- cbind(L, as.data.frame(apply(do.call(rbind, strsplit(L$Label, "<-")), 2, function(x) gsub(" ", "", x))))
L1 <- L[1:56, c("Label", "est", "V2")]
L2 <- L[57:68, c("est", "V1")]
L3 <- merge(L1, L2, by.x = "V2", by.y = "V1", all = TRUE)
L3$WEst <- with(L3, est.x * est.y)

















m3b <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M@1;
 M BY M1* M2 M3 M4;
 LC@1;
 LC BY LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;

 SE@1;
 SE BY SE2*1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;

 Opt@1;
 Opt BY Opt1*1 Opt2*1 Opt3*1;

 Pess@1;
 Pess BY Pess1*-1 Pess2*-1 Pess3*-1;

 PLife@1;
 PLife BY PLife2*1 PLife3 PLife4 PLife5 PLife6 ;

 Ctrl@1;
 Ctrl BY Ctrl1@1 Ctrl2;

 Con@1;
 Con BY Con1*1 Con2 Con3 Con4 Con5;

 Ext@1;
 Ext BY Ext1*1 Ext2 Ext3 Ext4 Ext5;


 FamS@1;
 FamS BY FamS1*1 FamS2*1 FamS3*1 FamS4*1;

 FriS@1;
 FriS BY FriS1*1 FriS2*1 FriS3*1 FriS4*1;

 SpoS@1;
 SpoS BY SpoS1*1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;

 SI@1;
 SI BY SI1*-1 SI2*1 SI3*1;

  G@1;
   G BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5
   FamS1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

 G WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3bfit <- mplusModeler(m3b, "MIDUS_PSR_cfa3b.dat", run=TRUE)
m3bfit$results$summaries
m3bfit <- readModels("MIDUS_PSR_cfa3b.out")


m3c <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 !M@1;
 M BY M1@1 M2 M3 M4;
 !LC@1;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;

 !SE@1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;

 !Opt@1;
 Opt BY Opt1@1 Opt2*1 Opt3*1;

 !Pess@1;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;

 !PLife@1;
 PLife BY PLife2@1 PLife3*-1 PLife4*-1 PLife5*1 PLife6*1;

 !Ctrl@1;
 Ctrl BY Ctrl1@1 Ctrl2;

 !Con@1;
 Con BY Con1@1 Con2 Con3 Con4 Con5;

 !Ext@1;
 Ext BY Ext1@1 Ext2 Ext3 Ext4 Ext5;


 !FamS@1;
 FamS BY FamS1@1 FamS2*1 FamS3*1 FamS4*1;

 !FriS@1;
 FriS BY FriS1@1 FriS2*1 FriS3*1 FriS4*1;

 !SpoS@1;
 SpoS BY SpoS1@1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;

 !SI@1;
 SI BY SI1@-1 SI2*1 SI3*1;

  G@1;
   G BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5
   FamS1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

 G WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

 PR BY SE@1 M*1 LC*1 Opt*1 Pess*1 PLife*1 Ctrl*1 Con*1 EXT*1;
 SR BY FamS@1 FriS*1 SpoS*1 SI*1;
 !PR@1;
 !SR@1;
 PR WITH SR@0;
 G WITH PR@0 SR@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3cfit <- mplusModeler(m3c, "MIDUS_PSR_cfa3c.dat", run=TRUE)
m3cfit$results$summaries

m3d <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M@1;
 M BY M1* M2 M3 M4;
 LC@1;
 LC BY LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;

 SE@1;
 SE BY SE2*1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;

 Opt@1;
 Opt BY Opt1*1 Opt2*1 Opt3*1;

 Pess@1;
 Pess BY Pess1*-1 Pess2*-1 Pess3*-1;

 PLife@1;
 PLife BY PLife2*1 PLife3 PLife4 PLife5 PLife6 ;

 Ctrl@1;
 Ctrl BY Ctrl1@1 Ctrl2;

 Con@1;
 Con BY Con1*1 Con2 Con3 Con4 Con5;

 Ext@1;
 Ext BY Ext1*1 Ext2 Ext3 Ext4 Ext5;


 FamS@1;
 FamS BY FamS1*1 FamS2*1 FamS3*1 FamS4*1;

 FriS@1;
 FriS BY FriS1*1 FriS2*1 FriS3*1 FriS4*1;

 SpoS@1;
 SpoS BY SpoS1*1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;

 SI@1;
 SI BY SI1*-1 SI2*1 SI3*1;

  GPR@1;
   GPR BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5;

  GSR@1;
   GSR BY FamS1*1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

  GPR WITH GSR*;

 GPR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

 GSR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

M WITH LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

LC WITH SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
SE WITH Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;
Opt WITH Pess@0
  PLife@0 Ctrl@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0
  SI@0;
Pess WITH PLife@0 Ctrl@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
PLife WITH Ctrl@0 Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Ctrl WITH Con@0 Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Con WITH Ext@0
  FamS@0 FriS@0 SpoS@0 SI@0;
Ext WITH  FamS@0 FriS@0 SpoS@0 SI@0;
FamS WITH FriS@0 SpoS@0 SI@0;
FriS WITH SpoS@0 SI@0;
SpoS WITH SI@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3dfit <- mplusModeler(m3d, "MIDUS_PSR_cfa3d.dat", run=TRUE)
m3dfit$results$summaries
m3dfit <- readModels("MIDUS_PSR_cfa3d.out")

m3e <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 !SE1
 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 !PLife1 PLife7
 PLife2 PLife3 PLife4 PLife5 PLife6
 Ctrl1 Ctrl2
 !Ctrl3
 Con1 Con2 Con3 Con4 Con5
 Ext1 Ext2 Ext3 Ext4 Ext5
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M@1;
 M BY M1* M2 M3 M4;
 LC@1;
 LC BY LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;

 SE@1;
 SE BY SE2*1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1;

 Opt@1;
 Opt BY Opt1*1 Opt2*1 Opt3*1;

 Pess@1;
 Pess BY Pess1*-1 Pess2*-1 Pess3*-1;

 PLife@1;
 PLife BY PLife2*1 PLife3 PLife4 PLife5 PLife6 ;

 Ctrl@1;
 Ctrl BY Ctrl1@1 Ctrl2;

 Con@1;
 Con BY Con1*1 Con2 Con3 Con4 Con5;

 Ext@1;
 Ext BY Ext1*1 Ext2 Ext3 Ext4 Ext5;


 FamS@1;
 FamS BY FamS1*1 FamS2*1 FamS3*1 FamS4*1;

 FriS@1;
 FriS BY FriS1*1 FriS2*1 FriS3*1 FriS4*1;

 SpoS@1;
 SpoS BY SpoS1*1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;

 SI@1;
 SI BY SI1*-1 SI2*1 SI3*1;

  GPR@1;
   GPR BY M1* M2 M3 M4
   LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
   SE2 SE3 SE4 SE5 SE6 SE7
   Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
   PLife2 PLife3 PLife4 PLife5 PLife6
   Ctrl1 Ctrl2
   Con1 Con2 Con3 Con4 Con5
   Ext1 Ext2 Ext3 Ext4 Ext5;

  GSR@1;
   GSR BY FamS1*1 FamS2 FamS3 FamS4
   FriS1 FriS2 FriS3 FriS4
   SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
   SI1*-1 SI2*1 SI3*1;

  GPR WITH GSR*;

 GPR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

 GSR WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 PLife@0 Ctrl@0 Con@0
  Ext@0 FamS@0 FriS@0 SpoS@0
  SI@0;

",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m3efit <- mplusModeler(m3e, "MIDUS_PSR_cfa3e.dat", run=TRUE)
m3efit$results$summaries



m4s1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt3 Pess2
 PLife3 Con2 Ext4
 FamS2 FriS1 SpoS3 SI1;

CATEGORICAL ARE
 M2 LC2 SE2 Opt3 Pess2
 PLife3 Con2 Ext4
 FamS2 FriS1 SpoS3 SI1;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 GPR@1;
 GPR BY M2*1 LC2*-1 SE2 Opt3 Pess2*-1
 PLife3 Con2 Ext4
! GSR@1;
! GSR BY
 FamS2*1 FriS1 SpoS3 SI1;

 !GPR WITH GSR*;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m4s1fit <- mplusModeler(m4s1, "MIDUS_PSR_cfa4s1.dat", run=TRUE)

m4s1fit$results$summaries
coef(m4s1fit$results, type = "stdyx")



m4s2 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS2 FriS1 SpoS2 SI1;

CATEGORICAL ARE
 M2 LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS2 FriS1 SpoS2 SI1;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 GPR@1;
 GPR BY M2*1 LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS2 FriS1 SpoS2 SI1;

",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m4s2fit <- mplusModeler(m4s2, "MIDUS_PSR_cfa4s2.dat", run=TRUE)

m4s2fit$results$summaries
coef(m4s2fit$results, type = "stdyx")


tmpdattest <- tmpdat[, c("M2", "LC2", "SE2", "Opt2", "PLife5", "Con2", "Ext4", "FamS2", "FriS1", "SpoS2", "SI1", "M2ID", "M2FAMNUM")]
tmpdattest <- tmpdattest[rowSums(is.na(tmpdattest)) < (ncol(tmpdattest) - 2), ]

rmat <- polychoric(na.omit(tmpdat[, c("M2", "LC2", "SE2", "Opt2", "PLife5", "Con2", "Ext4", "FamS2", "FriS1", "SpoS2", "SI1")]))

lavaantest <- sem("
  GPR =~ M2 + LC2 + SE2 + Opt2 +
   PLife5 + Con2 + Ext4 + FamS2 + FriS1 +
   SpoS2 + SI1", data = as.data.frame(lapply(na.omit(tmpdat[, c("M2", "LC2", "SE2", "Opt2", "PLife5", "Con2", "Ext4", "FamS2", "FriS1", "SpoS2", "SI1")]), factor, ordered = TRUE)),
  meanstructure = FALSE)

lavaantest <- sem("
  GPR =~ M2 + LC2 + SE2 + Opt2 +
   PLife5 + Con2 + Ext4 + FamS2 + FriS1 +
   SpoS2 + SI1", sample.cov = rmat$rho, sample.nobs = rmat$n.obs)
summary(lavaantest, fit.measures = TRUE)


m4s3 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS1 FamS2
 FriS1 FriS2
 SpoS2 SpoS3
 SI1 SI2;

CATEGORICAL ARE
 M2 LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS1 FamS2
 FriS1 FriS2
 SpoS2 SpoS3
 SI1 SI2;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 GPR@1;
 GPR BY M2* LC2 SE2 Opt2
 PLife5 Ctrl1 Con2 Ext4
 FamS1 FamS2
 FriS1 FriS2
 SpoS2 SpoS3
 SI1 SI2;
 FamS1 WITH FamS2*;
 FriS1 WITH FriS2*;
 SpoS2 WITH SpoS3*;
 SI1 WITH SI2*;

",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m4s3fit <- mplusModeler(m4s3, "MIDUS_PSR_cfa4s3.dat", run=TRUE)

m4s3fit$results$summaries
coef(m4s3fit$results, type = "stdyx")




tmpcfares <- do.call(rbind, list(
  M0 = m0fit$results$summaries,
  M1 = m1fit$results$summaries,
  M1B = m1bfit$results$summaries,
  M1C = m1cfit$results$summaries,
  M2 = m2fit$results$summaries,
  M3A = m3fit$results$summaries,
  M3B = m3bfit$results$summaries,
  M3C = m3cfit$results$summaries,
  M3D = m3dfit$results$summaries,
  M3E = m3efit$results$summaries))[, c(7, 8, 9, 10, 14, 16, 17, 18, 20)]

write.table(with(tmpcfares, data.frame(
  CFI = CFI,
  RMSEA = sprintf("%0.3f [%0.3f, %0.3f]", RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB),
  WRMR = WRMR,
  Parameters = Parameters,
  Chi2 = ChiSqM_Value,
  P = ChiSqM_PValue
)), file = "clipboard", sep = "\t", row.names = FALSE)

##compareModels(m3bfit$results, m3cfit$results, equalityMargin = c(param = .1, pvalue = .01), diffTest = TRUE)

coef(m3bfit$results, type = "stdyx", param = "loading")
coef(m3dfit$results, type = "stdyx", param = "loading")


tmploadings <- cbind(coef(m3bfit$results, type = "stdyx", param = "loading"), coef(m3fit$results, type = "stdyx", param = "loading"))
cor(tmploadings[, 2], tmploadings[, 6])

plot((tmploadings[, 2] + tmploadings[, 6])/2, tmploadings[, 2] - tmploadings[, 6])
abline(h = -.2)
abline(h = .2)





corevars <- colnames(subset(tmpdat, select = -c(M2ID, M2FAMNUM, AGE, SE1, PLife1, PLife7, Ctrl3)))

  sapply(tmpdat[, corevars], function(x) {
    y <- prop.table(table(x))
    max(y) - min(y)
  })

## not much differences between difference in min max and SD
## cor(
##   sapply(tmpdat[, corevars], function(x) {
##     y <- prop.table(table(x))
##     max(y) - min(y)
##   }),
##   sapply(tmpdat[, corevars], function(x) {
##     y <- prop.table(table(x))
##     sd(y)
##   }))























































m2c <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3 M4
 LC1 LC2 LC3 LC4 LC5 LC6 LC7 LC8
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1 Pess2 Pess3
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1 SI2 SI3;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 !M@1;
 M BY M1@1 M2 M3 M4;
 !LC@1;
 LC BY LC1@-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1;

 !SE@1;
 SE BY SE2@1 SE3*-1 SE4*1 SE5*-1 SE6*1 SE7*-1 SE1*0;

 !Opt@1;
 Opt BY Opt1@1 Opt2*1 Opt3*1;

 !Pess@1;
 Pess BY Pess1@-1 Pess2*-1 Pess3*-1;

 !FamS@1;
 FamS BY FamS1@1 FamS2*1 FamS3*1 FamS4*1;

 !FriS@1;
 FriS BY FriS1@1 FriS2*1 FriS3*1 FriS4*1;

 !SpoS@1;
 SpoS BY SpoS1@1 SpoS2*1 SpoS3*1 SpoS4*1 SpoS5*1 SpoS6*1;

 !SI@1;
 SI BY SI1@-1 SI2*1 SI3*1;

 G@1;
 G BY M1* M2 M3 M4
 LC1*-1 LC2*-1 LC3*-1 LC4*-1 LC5*-1 LC6*-1 LC7*-1 LC8*-1
 SE1 SE2 SE3 SE4 SE5 SE6 SE7
 Opt1 Opt2 Opt3 Pess1*-1 Pess2*-1 Pess3*-1
 FamS1 FamS2 FamS3 FamS4
 FriS1 FriS2 FriS3 FriS4
 SpoS1 SpoS2 SpoS3 SpoS4 SpoS5 SpoS6
 SI1*-1 SI2*1 SI3*1;

 G WITH M@0 LC@0 SE@0 Opt@0
  Pess@0 FamS@0 FriS@0 SpoS@0
  SI@0;

  !PR@1;
  PR BY M@1 LC*1 SE*1 Opt*1 Pess*1;

  !SR@1;
  SR BY FamS@1 FriS*1 SpoS*1 SI*1;

  PR WITH SR@0;

  G WITH PR@0 SR@0;
!M WITH LC@0 SE@0 Opt@0
!  Pess@0 FamS@0 FriS@0 SpoS@0
!  SI@0;
!LC WITH SE@0 Opt@0
!  Pess@0 FamS@0 FriS@0 SpoS@0
!  SI@0;
!SE WITH Opt@0
!  Pess@0 FamS@0 FriS@0 SpoS@0
!  SI@0;
!Opt WITH Pess@0 FamS@0 FriS@0 SpoS@0
!  SI@0;
!Pess WITH FamS@0 FriS@0 SpoS@0 SI@0;
!FamS WITH FriS@0 SpoS@0 SI@0;
!FriS WITH SpoS@0 SI@0;
!SpoS WITH SI@0;
",
OUTPUT = "STDYX;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2cfit <- mplusModeler(m2c, "MIDUS_PSR_cfa2c.dat", run=TRUE)
m2cfit$results$summaries

screenreg(
  list(
    M0 = extract(m0fit, params = c("loading", "undirected", "variability"), type = "stdyx"),
    M1 = extract(m1fit, params = c("loading", "undirected", "variability"), type = "stdyx"),
    M1B = extract(m1bfit, params = c("loading", "undirected", "variability"), type = "stdyx"),
    M2 = extract(m2fit, params = c("loading", "undirected", "variability"), type = "stdyx"),
    M2B = extract(m2bfit, params = c("loading", "undirected", "variability"), type = "stdyx"),
    M2C = extract(m2cfit, params = c("loading", "undirected", "variability"), type = "stdyx")),
    single.row = TRUE)



plot(data.frame(
  M0 = subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2 = subset(coef(m2fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2B = subset(coef(m2bfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2C = subset(coef(m2cfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est))

colMeans(abs(data.frame(
  M2 = subset(coef(m2fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2B = subset(coef(m2bfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2C = subset(coef(m2cfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est)))

ggplot(melt(data.frame(
  M2 = subset(coef(m2fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2B = subset(coef(m2bfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est,
  M2C = subset(coef(m2cfit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est - subset(coef(m0fit, params = "loading", type = "stdyx"), grepl("<-G", Label ))$est)),
  aes(value)) +
  geom_histogram() +
  facet_wrap(~variable)





################################################################################
#                                                                              #
#                                    PSRs                                      #
#                                                                              #
################################################################################

psr.items <- rbind.data.frame(
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

psr.items[] <- lapply(psr.items, as.character)

table(psr.items$M2items %in% colnames(d))
table(psr.items$MAitems %in% colnames(dm))

d[, psr.items$M2items] <- lapply(d[, psr.items$M2items], function(x) {
    x[which(x %in% c(7, 8, 9))] <- NA;
    return(factor(x, ordered = TRUE))
})

dm[, psr.items$MAitems] <- lapply(dm[, psr.items$MAitems], function(x) {
    x[which(x %in% c(7, 8, 9))] <- NA;
    return(factor(x, ordered = TRUE))
})

for(i in 1:nrow(psr.items)) {
  dm[, psr.items$M2items[i]] <- dm[, psr.items$MAitems[i]]
}
dm$M2FAMNUM <- dm$M2ID

dmerged <- rbind(d[, c("M2ID", "M2FAMNUM", psr.items$M2items)],
                 dm[, c("M2ID", "M2FAMNUM", psr.items$M2items)])

cd(base, pre <- "psr_", num <- "imputed")

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

psr.imputed.vars <- psr.items$M2items
psr.factor.vars <- c("MASTERY", "CONSTRAINT", "SELFESTEEM", "OPTIMISM",
"PESSIMISM", "FAMILYSUPT", "FRIENDSUPT", "SPOUSESUPT", "PR", "SR")

psr.data <- lapply(1:50, function(i) {
  psr.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/psr_imputed/psr_imp", i, ".dat"), na.strings = "*")
  colnames(psr.d) <- c(psr.imputed.vars, "M2ID", psr.factor.vars)
  cbind(Imputation = i, psr.d[, c("M2ID", psr.factor.vars)])
})

mean(sapply(psr.data, function(x) {with(x, cor(PR, SR))}))

## mean(sapply(1:50, function(i) cor(psr.data[[i]]$PR, psr.data[[i]]$SR, use = 'pairwise.complete.obs')))
## cor(rowMeans(sapply(1:50, function(i) psr.data[[i]]$PR), na.rm = TRUE),
## rowMeans(sapply(1:50, function(i) psr.data[[i]]$SR), na.rm = TRUE), use = 'pairwise.complete.obs')

################################################################################
#                                                                              #
#                                  PA NA                                       #
#                                                                              #
################################################################################

mood.items <- rbind.data.frame(
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


mood.items[] <- lapply(mood.items, as.character)

table(mood.items$M2items %in% colnames(d))
table(mood.items$MAitems %in% colnames(dm))

d[, mood.items$M2items] <- lapply(d[, mood.items$M2items], function(x) {
    x[which(x %in% c(7, 8, 9))] <- NA;
    return(factor(x, ordered = TRUE))
})

dm[, mood.items$MAitems] <- lapply(dm[, mood.items$MAitems], function(x) {
    x[which(x %in% c(7, 8, 9))] <- NA;
    return(factor(x, ordered = TRUE))
})

for(i in 1:nrow(mood.items)) {
  dm[, mood.items$M2items[i]] <- dm[, mood.items$MAitems[i]]
}
dm$M2FAMNUM <- dm$M2ID

dmerged2 <- rbind(d[, c("M2ID", "M2FAMNUM", mood.items$M2items)],
                 dm[, c("M2ID", "M2FAMNUM", mood.items$M2items)])


cd(base, pre <- "pana_", num <- "imputed")

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

pana.imputed.vars <- c(mood.items$M2items, "M2ID")
pana.factor.vars <- c("NegAff", "PosAff")

pana.data <- lapply(1:50, function(i) {
  pana.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/pana_imputed/pana_imp", i, ".dat"), na.strings = "*")
  colnames(pana.d) <- c(pana.imputed.vars[-c(12, 13, 24)], pana.factor.vars)
  cbind(Imputation = i, pana.d[, c("M2ID", pana.factor.vars)])
})


##}}}
