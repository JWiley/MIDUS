## plot(SEMSummary(~ ., data = tmpdat[,-which(colnames(tmpdat) %in% c("M2ID", "M2FAMNUM", "Died", "AGE"))], use = "pairwise.complete.obs"))
## plot(SEMSummary(~ ., data = tmpdat[,-which(colnames(tmpdat) %in% c("M2ID", "M2FAMNUM", "Died", "AGE"))], use = "pairwise.complete.obs"), order = "asis")
## plot(SEMSummary(~ ., data = tmpdat[,-(1:2)], use = "pairwise.complete.obs"), order = "asis")


aim2.psrsfscale <- mplusObject(
VARIABLE = "
USEVARIABLES ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;
CATEGORICAL ARE
 M2 LC2 SE2 Opt2 PLife2 Con3 Ext4
 FamS2 FriS2 SpoS3 SI2 SI1;

IDVARIABLE = M2ID;
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
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
usevariables = colnames(tmpdat.full),
SAVEDATA = "
  FILE = MIDUS_PSRSF_scale_fscores.dat;
  SAVE = FSCORES;",
rdata = tmpdat.full)

## aim2.psrsfscale.fit <- mplusModeler(aim2.psrsfscale, "MIDUS_PSRSF_scale.dat", run=TRUE)
aim2.psrsfscale.fit <- list(results = readModels("MIDUS_PSRSF_scale.out"))
aim2.psrsfscale.fit$results$summaries
str(aim2.psrsfscale.fit$results$savedata)



aim2.psrscale <- mplusObject(
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
CLUSTER = M2FAMNUM;
",
ANALYSIS = "
  TYPE = COMPLEX;
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
  FILE = MIDUS_PSR_scale_fscores.dat;
  SAVE = FSCORES;",
usevariables = colnames(tmpdat.full),
rdata = tmpdat.full)

## aim2.psrscale.fit <- mplusModeler(aim2.psrscale, "MIDUS_PSR_scale.dat", run=TRUE)
aim2.psrscale.fit <- list(results = readModels("MIDUS_PSR_scale.out"))
aim2.psrscale.fit$results$summaries
str(aim2.psrscale.fit$results$savedata)

psrscale_scores <- merge(
  aim2.psrscale.fit$results$savedata[, c("M2ID", "PR", "SR")],
  aim2.psrsfscale.fit$results$savedata[, c("M2ID", "PR", "SR")],
  by = "M2ID", all=TRUE)
colnames(psrscale_scores) <- c("M2ID", "PR", "SR", "PRSF", "SRSF")
psrscale_scores[, -1] <- psrscale_scores[, -1] * -1
