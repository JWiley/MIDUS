preloaded <- FALSE
source("projectwide_setup.R")
options(width = 80)

cd(base, pre <- "psr_", num <- "imputed")


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

##}}}

##{{{ Full Form Analyses and Validation

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

t(rbind(
  TwoFactor = m2fit$results$summaries,
  OneFactor = m2bfit$results$summaries))

compareModels(m2fit$results, m2bfit$results, diffTest = TRUE)

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




m2c2 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

IDVARIABLE = M2ID;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1 M2 M3;
 LC BY LC2@-1 LC5*-1 LC7*-1;
 SE BY SE2@1 SE4*1 SE6*1;
 Opt BY Opt1 Opt2 Opt3;
 PLife BY PLife2@1 PLife3 PLife5;
 Con BY Con2@1 Con3 Con5;
 EXT BY Ext1@1 Ext3 Ext4;
 FamS BY FamS1 FamS2 FamS4;
 FriS BY FriS1 FriS2 FriS4;
 SpoS BY SpoS1 SpoS2 SpoS3;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR@1;
 PR BY M* LC SE Opt !Pess
   PLife Con EXT;
 SR@1;
 SR BY FamS* FriS SpoS SI;
 PR WITH SR*;
",
OUTPUT = "STDYX; CINTERVAL;",
PLOT = "TYPE IS PLOT2;",
SAVEDATA = "
  FILE = MIDUS_PSR_cfa2c2_fscores.dat;
  SAVE = FSCORES;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2c2fit <- mplusModeler(m2c2, "MIDUS_PSR_cfa2c2.dat", run=TRUE)
m2c2fit <- list(results = readModels("psr_imputed/MIDUS_PSR_cfa2c2.out"))

m2c2fit$results$summaries
PSRQ_LF <- cbind(m2c2fit$results$parameters$ci.stdyx[, c(1, 2)],
                 apply(m2c2fit$results$parameters$ci.stdyx[, c(6, 4, 8)], 2, function(x) gsub(" ", "", gsub("0\\.", "\\.", format(round(x, 2), digits = 2, nsmall = 2)))))
PSRQ_LF$Res <- with(PSRQ_LF, sprintf("%s [%s,%s]", est, low2.5, up2.5))
write.table(PSRQ_LF, "clipboard", sep = "\t", row.names = FALSE)

## Configural + Metric + Scalar
m2c2.mgage1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

IDVARIABLE = M2ID;
GROUPING = AGE (1 = Young 2 = Middle 3 = Old);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1@1 M2*1 M3*1;
 LC BY LC2@-1 LC5*-1 LC7*-1;
 SE BY SE2@1 SE4*1 SE6*1;
 Opt BY Opt1@1 Opt2*1 Opt3*1;
 PLife BY PLife2@1 PLife3*1 PLife5*1;
 Con BY Con2@1 Con3*1 Con5*1;
 EXT BY Ext1@1 Ext3*1 Ext4*1;
 FamS BY FamS1@1 FamS2*1 FamS4*1;
 FriS BY FriS1@1 FriS2*1 FriS4*1;
 SpoS BY SpoS1@1 SpoS2*1 SpoS3*1;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M@1 LC*1 SE*1 Opt*1
   PLife*1 Con*1 EXT*1;
 SR BY FamS@1 FriS*1 SpoS*1 SI*1;
 PR WITH SR*;
[M LC SE Opt PLife Con EXT FamS FriS SpoS SI] (fm1-fm11);
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = within(tmpdat, {
  AGE <- as.integer(cut(tmpdat$AGE, c(0, 45, 60, 100), include.lowest = TRUE, right = TRUE))
}))

m2c2.mgage1fit <- mplusModeler(m2c2.mgage1, "MIDUS_PSR_cfa2c2_mgage1.dat", run=TRUE)
m2c2.mgage1fit$results$summaries

## Configural + Metric + Scalar
m2c2.mgsex1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

IDVARIABLE = M2ID;
GROUPING = SEX (1 = Men 2 = Women);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1@1 M2*1 M3*1;
 LC BY LC2@-1 LC5*-1 LC7*-1;
 SE BY SE2@1 SE4*1 SE6*1;
 Opt BY Opt1@1 Opt2*1 Opt3*1;
 PLife BY PLife2@1 PLife3*1 PLife5*1;
 Con BY Con2@1 Con3*1 Con5*1;
 EXT BY Ext1@1 Ext3*1 Ext4*1;
 FamS BY FamS1@1 FamS2*1 FamS4*1;
 FriS BY FriS1@1 FriS2*1 FriS4*1;
 SpoS BY SpoS1@1 SpoS2*1 SpoS3*1;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M@1 LC*1 SE*1 Opt*1
   PLife*1 Con*1 EXT*1;
 SR BY FamS@1 FriS*1 SpoS*1 SI*1;
 PR WITH SR*;
[M@0 LC@0 SE@0 Opt@0 PLife@0 Con@0 EXT@0 FamS@0 FriS@0 SpoS@0 SI@0] (fm1-fm11);
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2c2.mgsex1fit <- mplusModeler(m2c2.mgsex1, "MIDUS_PSR_cfa2c2_mgsex1.dat", run=TRUE)
m2c2.mgsex1fit$results$summaries


## Configural + Metric + Scalar
m2c2.mgrace1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

CATEGORICAL ARE
 M1 M2 M3  LC2 LC5 LC7
 SE2 SE4 SE6  Opt1 Opt2 Opt3
 PLife2 PLife3 PLife5
 Con2 Con3 Con5  Ext1 Ext3 Ext4
 FamS1 FamS2 FamS4  FriS1 FriS2 FriS4
 SpoS1 SpoS2 SpoS3  SI1 SI2 SI3;

IDVARIABLE = M2ID;
GROUPING = RACE (1 = White 2 = AA);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 M BY M1@1 M2*1 M3*1;
 LC BY LC2@-1 LC5*-1 LC7*-1;
 SE BY SE2@1 SE4*1 SE6*1;
 Opt BY Opt1@1 Opt2*1 Opt3*1;
 PLife BY PLife2@1 PLife3*1 PLife5*1;
 Con BY Con2@1 Con3*1 Con5*1;
 EXT BY Ext1@1 Ext3*1 Ext4*1;
 FamS BY FamS1@1 FamS2*1 FamS4*1;
 FriS BY FriS1@1 FriS2*1 FriS4*1;
 SpoS BY SpoS1@1 SpoS2*1 SpoS3*1;
 SI BY SI1@-1 SI2*1 SI3*1;

 PR BY M@1 LC*1 SE*1 Opt*1
   PLife*1 Con*1 EXT*1;
 SR BY FamS@1 FriS*1 SpoS*1 SI*1;
 PR WITH SR*;
[M@0 LC@0 SE@0 Opt@0 PLife@0 Con@0 EXT@0 FamS@0 FriS@0 SpoS@0 SI@0] (fm1-fm11);
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = subset(tmpdat, RACE != 3))

m2c2.mgrace1fit <- mplusModeler(m2c2.mgrace1, "MIDUS_PSR_cfa2c2_mgrace1.dat", run=TRUE)
m2c2.mgrace1fit$results$summaries

write.table(t(with(rbind.data.frame(
  Overall = m2c2fit$results$summaries[, -6],
  Age = m2c2.mgage1fit$results$summaries,
  Sex = m2c2.mgsex1fit$results$summaries,
  Race = m2c2.mgrace1fit$results$summaries), {
    data.frame(NParam = Parameters,
               X2 = ChiSqM_Value,
               DF = as.integer(ChiSqM_DF),
               P = ifelse(ChiSqM_PValue < .001, "< .001", ChiSqM_PValue),
               CFI = CFI,
               RMSEA = sprintf("%s\n[%s, %s]", RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB),
               WRMR = WRMR)
  })), file = "clipboard", sep = "\t")


coef(m2c2fit$results, type = "stdyx", param = "loading")

##}}}

##{{{ Short Form Analyses and Validation
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
 PR WITH SR*;
",
OUTPUT = "STDYX; CINTERVAL;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
SAVEDATA = "
  FILE = MIDUS_PSR_cfa2c2sc_fscores.dat;
  SAVE = FSCORES;",
rdata = tmpdat)

m2c2scfit <- mplusModeler(m2c2sc, "MIDUS_PSR_cfa2c2sc.dat", run=TRUE)
m2c2scfit <- list(results = readModels("psr_imputed/MIDUS_PSR_cfa2c2sc.out"))
m2c2scfit$results$summaries

coef(m2c2scfit$results, type = "stdyx", param = "loading")
coef(m2c2scfit$results, type = "stdyx")[1:13, ]


fscores <- data.frame(
  M2ID = m2c2fit$results$savedata$M2ID,
  PSRQPR = m2c2fit$results$savedata$PR,
  PSRQSR = m2c2fit$results$savedata$SR,
  PSRQSFPR = m2c2scfit$results$savedata$PR,
  PSRQSFSR = m2c2scfit$results$savedata$SR)

round(colMeans(fscores), 3)
round(apply(fscores, 2, sd), 3)

round(cor(fscores[,-1]), 3)

summary(lm(I(scale(PSRQPR) + scale(PSRQSR)) ~ I(scale(PSRQSFPR) + scale(PSRQSFSR)), data = fscores))


tmppr <- with(fscores, bland.altman.stats(PSRQPR, PSRQSFPR))
prop.table(table(with(tmppr, diffs < lower.limit | diffs > upper.limit)))

tmpsr <- with(fscores, bland.altman.stats(PSRQSR, PSRQSFSR))
prop.table(table(with(tmpsr, diffs < lower.limit | diffs > upper.limit)))

lfsf <- plot_grid(
  ggplot(within(melt(fscores[, c("PSRQPR", "PSRQSFPR")]), {
  variable <- factor(variable, levels = c("PSRQPR", "PSRQSFPR"), labels = c("PSRQ", "PSRQ-SF"))
}), aes(value, colour = variable, linetype = variable)) +
  geom_density(size = 2) +
  scale_colour_manual("", values = c("PSRQ" = "black", "PSRQ-SF" = "grey50")) +
  scale_linetype_manual("", values = c("PSRQ" = 2, "PSRQ-SF" = 1)) +
  theme_classic() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  xlab("PR Scores") + coord_cartesian(xlim = c(-3, 3)),
ggplot(within(melt(fscores[, c("PSRQSR", "PSRQSFSR")]), {
  variable <- factor(variable, levels = c("PSRQSR", "PSRQSFSR"), labels = c("PSRQ", "PSRQ-SF"))
}), aes(value, colour = variable, linetype = variable)) +
  geom_density(size = 2) +
  scale_colour_manual("", values = c("PSRQ" = "black", "PSRQ-SF" = "grey50")) +
  scale_linetype_manual("", values = c("PSRQ" = 2, "PSRQ-SF" = 1)) +
  theme_classic() +
  theme(legend.key.width = unit(1.5, "cm"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  xlab("SR Scores") + coord_cartesian(xlim = c(-3, 3)),
ggplot(fscores, aes(PSRQPR, PSRQSFPR)) +
geom_point(alpha = .5) +
geom_abline(intercept = 0, slope = 1, colour = "grey50", linetype = 2, size = 2) +
  theme_classic() +
xlab("PSRQ PR Scores") +
ylab("PSRQ-SF PR Scores") +
coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)),
ggplot(fscores, aes(PSRQSR, PSRQSFSR)) +
geom_point(alpha = .5) +
geom_abline(intercept = 0, slope = 1, colour = "grey50", linetype = 2, size = 2) +
  theme_classic() +
xlab("PSRQ SR Scores") +
ylab("PSRQ-SF SR Scores") +
coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)),
with(fscores, bland.altman.plot(PSRQPR, PSRQSFPR, graph.sys = "ggplot2")) +
  theme_classic() +
  xlab("Mean of PR Scores") +
  ylab("Difference in PR Scores") + coord_cartesian(xlim = c(-3, 3)),
with(fscores, bland.altman.plot(PSRQSR, PSRQSFSR, graph.sys = "ggplot2")) +
  theme_classic() +
  xlab("Mean of SR Scores") +
  ylab("Difference in SR Scores") + coord_cartesian(xlim = c(-3, 3)), ncol = 2)

ggsave("lfsf.pdf", plot = lfsf, width = 9, height = 4.5*3, units = "in")


## Configural + Metric + Scalar
m2c2sc.mgage1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;
CATEGORICAL ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

IDVARIABLE = M2ID;
GROUPING = AGE (1 = Young 2 = Middle 3 = Old);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 PR@1;
 PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*;
 SR@1;
 SR BY FamS2*1 FriS2 SpoS3 SI2 SI1;
 PR WITH SR*;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = within(tmpdat, {
  AGE <- as.integer(cut(tmpdat$AGE, c(0, 45, 60, 100), include.lowest = TRUE, right = TRUE))
}))

m2c2sc.mgage1fit <- mplusModeler(m2c2sc.mgage1, "MIDUS_PSR_cfa2c2sc_mgage1.dat", run=TRUE)
m2c2sc.mgage1fit$results$summaries

## Configural + Metric + Scalar
m2c2sc.mgsex1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;
CATEGORICAL ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

IDVARIABLE = M2ID;
GROUPING = SEX (1 = Men 2 = Women);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 PR@1;
 PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*;
 SR@1;
 SR BY FamS2*1 FriS2 SpoS3 SI2 SI1;
 PR WITH SR*;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = tmpdat)

m2c2sc.mgsex1fit <- mplusModeler(m2c2sc.mgsex1, "MIDUS_PSR_cfa2c2sc_mgsex1.dat", run=TRUE)
m2c2sc.mgsex1fit$results$summaries

## Configural + Metric + Scalar
m2c2sc.mgrace1 <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;
CATEGORICAL ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

IDVARIABLE = M2ID;
GROUPING = RACE (1 = White 2 = AA);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 2;",
MODEL = "
 PR@1;
 PR BY M2*1 LC2*-1 SE2* Opt2* PLife2*1 Con3*1 Ext4*;
 SR@1;
 SR BY FamS2*1 FriS2 SpoS3 SI2 SI1;
 PR WITH SR*;
",
OUTPUT = "STDYX;",
PLOT = "TYPE IS PLOT2;",
usevariables = colnames(tmpdat),
rdata = subset(tmpdat, RACE != 3))

m2c2sc.mgrace1fit <- mplusModeler(m2c2sc.mgrace1, "MIDUS_PSR_cfa2c2sc_mgrace1.dat", run=TRUE)
m2c2sc.mgrace1fit$results$summaries


t(rbind(
  Overall = m2c2scfit$results$summaries[, -6],
  Age = m2c2sc.mgage1fit$results$summaries,
  Sex = m2c2sc.mgsex1fit$results$summaries,
  Race = m2c2sc.mgrace1fit$results$summaries))




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



alpha(polychoric(na.omit(tmpdat[, c("FamS2", "FriS2", "SpoS3", "SI2", "SI1")]),
                 global = FALSE)$rho, check.keys = TRUE)


##}}}

##{{{ Archive
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

##}}}
