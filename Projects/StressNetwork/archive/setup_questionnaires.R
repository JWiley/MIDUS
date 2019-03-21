source("~/OneDrive/codebits/mat_rel_ml/ml_rel_mat2.R")
source("setup.R")
options(width = 150)

## Nice Names
key <- "
Var	Label
A1PAGE_M2	Age (M1)
B1PAGE_M2	Age (M2)
AGE	Age (M1)
Sex	Sex (M1)
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
LifeStress	Stressful Life Events (M2)
B4QCT_SA	Sexual Abuse (M2P4)
B4QCT_PN	Physical Neglect (M2P4)
B4QCT_PA	Physical Abuse (M2P4)
B4QCT_EA	Emotional Abuse (M2P4)
B4QCT_EN	Emotional Neglect (M2P4)
A1SPIFAM	Family Inequality (M1)
B1SPIFAM	Family Inequality (M2)
A1SHOMET	Neighborhood Quality (M1)
A1SPIHOM	Home Inequality (M1)
B1SHOMET	Neighborhood Quality (M2)
B1SPIHOM	Home Inequality (M2)
A1SCHRON	Chronic Conditions (M1)
B1SCHRON	Chronic Conditions (M2)
A1PA6	Self-rated Relative Health (M1)
B1PA3	Self-rated Relative Health (M2)
B1PA1	Self-rated Physical Health (M2)
B1PA2	Self-rated Mental Health (M2)
A1PA4	Self-rated Physical Health (M1)
A1PA5	Self-rated Mental Health (M1)
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
"

key2 <- read.table(textConnection(key), header=TRUE, sep = "\t")
key2$Timepoint <- gsub("(.*)\\((.*)\\)", "\\2", key2$Label)
key2$Label2 <- gsub("(.*)\\((.*)\\)", "\\1", key2$Label)

key2$TimepointCol <- ifelse(key2$Timepoint == "M1",
                            "steelblue2",
                            ifelse(key2$Timepoint == "M2",
                                   "lightgoldenrod", "brown1"))



################################################################################
##                                   MIDUS I                                  ##
################################################################################

d1 <- read.table("~\\Onedrive\\Projects\\MIDUS\\ICPSR_02760\\DS0001\\02760-0001-Data.tsv",
  header=TRUE, sep = '\t', stringsAsFactors = FALSE)

if (FALSE) {
## create lifetime and daily discrimination scores based on
## procedure from MIDUS II
d1[, paste0("A1SS13", LETTERS[1:11])] <- lapply(d1[, paste0("A1SS13", LETTERS[1:11])], function(x) {
  ifelse(x > 100, NA, x)
})

d1$A1SLFEDI <- rowSums(d1[, paste0("A1SS13", LETTERS[1:11])] >= 1, na.rm = TRUE)

d1[, paste0("A1SS14", LETTERS[1:9])] <- lapply(d1[, paste0("A1SS14", LETTERS[1:9])], function(x) {
  ifelse(x > 4, NA, x)
})

d1$A1SDAYDI <- rowMeans(5 - d1[, paste0("A1SS14", LETTERS[1:9])], na.rm = TRUE) * 9
}

stress <- list(
  "A1PA4", "A1PA5", "A1PA6", "A1SCHRON",
abuse = c(
## How often LIST A: Insulted you or sworeat you; Sulked or refused to talk to you;
## Stomped out of the room; Did or said something to spite you; Threatened to hit you; Smashed or
## kicked something in anger
"A1SE17A", ## Mother / woman
"A1SE17B", ## Father / man
"A1SE17C", ## Brothers
"A1SE17D", ## Sisters
"A1SE17E", ## Anyone Else
## LIST B: Pushed, grabbed, or shoved you; Slapped you; Threw something at you.
"A1SE17F", ## Mother / woman
"A1SE17G", ## Father / man
"A1SE17H", ## Brothers
"A1SE17I", ## Sisters
"A1SE17J", ## Anyone Else
## LIST C: Kicked, bit, or hit you with a fist; Hit or tried to hit you with something; Beat you up; Choked
## you; Burned or scalded you.
"A1SE17K", ## Mother / woman
"A1SE17L", ## Father / man
"A1SE17M", ## Brothers
"A1SE17N", ## Sisters
"A1SE17O"), ## Anyone Else
piw = "A1SPIWOR", ## perceived inequality in work; alpha = .78
pnq = "A1SHOMET", ## perceived neighborhood quality/health; alpha = .68
pih = "A1SPIHOM", ## perceived inequality in home; alpha = .80
famstrain = "A1SKINNE", ## family strain; alpha = .80
friendstrain = "A1SFDSNE", ## friend strain; alpha = .79
partnerstrain = "A1SSPCRI", ## partner strain
pif = "A1SPIFAM", ## perceived inequality in family; alpha = .69
lifetimediscrimination = paste0("A1SS13", LETTERS[1:11]), ## lifetime discrimination
dailydiscrimination = paste0("A1SS14", LETTERS[1:9]) ## daily discrimination, alpha = .97
)

dtmp.1 <- d1[, c("M2ID", "M2FAMNUM", "SAMPLMAJ", "A1PRSEX", "A1PAGE_M2", unlist(stress))]

################################################################################
##                                  MIDUS II                                  ##
################################################################################
d2 <- read.table("~\\Onedrive\\Projects\\MIDUS\\ICPSR_04652\\DS0001\\04652-0001-Data.tsv",
  header=TRUE, sep = '\t', stringsAsFactors = FALSE)

d2[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")] <- lapply(d2[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")], function(x) ifelse(x %in% c(1, 2), x, NA))

d2$LifeStress <- rowSums(d2[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")] == 1, na.rm = TRUE)
d2$LifeStress[rowSums(is.na(d2[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")])) == 27] <- NA

stress2 <- list(
  "B1PA1", "B1PA2", "B1PA3", "B1SCHRON", "LifeStress",
  "B1SJOBDI", ## chronic job discrimination
  piw = "B1SPIWOR", ## perceived inequality in work
  pnq = "B1SHOMET", ## perceived neighborhood quality/health;
  pih = "B1SPIHOM", ## perceived inequality in home;
  famstrain = "B1SKINNE", ## family strain;
  friendstrain = "B1SFDSNE", ## friend strain;
  partnerstrain = "B1SSPCRI", ## partner strain
  pif = "B1SPIFAM", ## perceived inequality in family;
  lifetimediscrimination = "B1SLFEDI", ## paste0("A1SS13", LETTERS[1:11]), ## lifetime discrimination
  dailydiscrimination = "B1SDAYDI") ## paste0("A1SS14", LETTERS[1:9]) ## daily discrimination;

dtmp.2 <- d2[, c("M2ID", "M2FAMNUM", "B1PAGE_M2", "B1PRSEX", "SAMPLMAJ", unlist(stress2))]


################################################################################
##                            MIDUS II Milwaukee                              ##
################################################################################

## Milwaukee Sample
dm <- read.table("~\\Onedrive\\Projects\\MIDUS\\ICPSR_22840\\DS0001\\22840-0001-Data-REST.tsv",
  header=TRUE, sep = '\t', stringsAsFactors = FALSE)
dm$M2FAMNUM <- dm$M2ID
dm$SAMPLMAJ <- 13 ## milwaukee

dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")] <- lapply(dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")], function(x) ifelse(x %in% c(1, 2), x, NA))
dm$LifeStress <- rowSums(dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")] == 1, na.rm = TRUE)
dm$LifeStress[rowSums(is.na(dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")])) == 27] <- NA


stress2m <- list(
  "BACA1", "BACA2", "BACA3", "BACCHRON", "LifeStress",
  "BASJOBDI", ## chronic job discrimination
  piw = "BASPIWOR", ## perceived inequality in work
  pnq = "BACHOMET", ## perceived neighborhood quality/health;
  pih = "BACPIHOM", ## perceived inequality in home;
  famstrain = "BACKINNE", ## family strain;
  friendstrain = "BACFDSNE", ## friend strain;
  partnerstrain = "BACSPCRI", ## partner strain
  pif = "BACPIFAM", ## perceived inequality in family;
  lifetimediscrimination = "BACLFEDI", ## paste0("A1SS13", LETTERS[1:11]), ## lifetime discrimination
  dailydiscrimination = "BACDAYDI") ## paste0("A1SS14", LETTERS[1:9]) ## daily discrimination;

dtmp.2m <- dm[, c("M2ID", "M2FAMNUM", "BACRAGE", "BACRSEX", "SAMPLMAJ", unlist(stress2m))]
cbind(colnames(dtmp.2), colnames(dtmp.2m))
colnames(dtmp.2m) <- colnames(dtmp.2)
dtmp.2all <- rbind(dtmp.2, dtmp.2m)

################################################################################
##                        MIDUS II Biomarker Project                          ##
################################################################################

## Biomarker Project Sample
dp4 <- read.table("~\\Onedrive\\Projects\\MIDUS\\ICPSR_29282\\DS0001\\29282-0001-Data.tsv",
                  header=TRUE, sep = '\t', stringsAsFactors = FALSE)

stressp4 <- list(
"B4QPS_PS", ## perceived stress scale
"B4QCT_EA", ## emotional abuse
"B4QCT_PA", ## physical abuse
"B4QCT_SA", ## sexual abuse
"B4QCT_EN", ## emotional neglect
"B4QCT_PN", ## physical neglect
"B4QCT_MD" ## minimization/denial
)

dtmp.p4 <- dp4[, c("M2ID", unlist(stressp4))]
dtmp.2all <- merge(dtmp.2all, dtmp.p4, by = "M2ID", all=TRUE)

dtmp.2all <- merge(dtmp.2all, cog[, c("m2id", "b3tem", "exec_fxn")], by.x = "M2ID", by.y = "m2id", all.x=TRUE)
dtmp.2all <- merge(dtmp.2all, fdat[, c("m2id",
                                       "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel", "pulpress", "Rb4p1gs",
                                       "avgb_rm", "avgb_lf", "avgb_hf")], by.x = "M2ID", by.y = "m2id", all.x=TRUE)


d.all <- merge(dtmp.1, dtmp.2all, by = c("SAMPLMAJ", "M2FAMNUM", "M2ID"), all = TRUE)



droplist <- list(single = c(8, 9),
                 double = c(98, 99),
                 triple = c(996, 997, 998, 999),
                 quad = c(9996, 9997, 9998, 9999),
                 five = c(99996, 99997, 99998, 99999))

for (v in colnames(d.all)[-(1:3)]) {
  ddex <- ifelse(max(d.all[[v]], na.rm = TRUE) < 10, 1,
                 ifelse(max(d.all[[v]], na.rm = TRUE) < 100, 2,
                 ifelse(max(d.all[[v]], na.rm = TRUE) < 1000, 3,
                 ifelse(max(d.all[[v]], na.rm = TRUE) < 10000, 4,
                 ifelse(max(d.all[[v]], na.rm = TRUE) < 100000, 5, NA)))))

  d.all[[v]] <- ifelse(d.all[[v]] %in% droplist[[c("single", "double", "triple", "quad", "five")[ddex]]],
                     NA, d.all[[v]])
}

for (v in c(stress$abuse, "A1PA4", "A1PA5", "A1PA6", "B1PA1", "B1PA2", "B1PA3", "A1PRSEX", "B1PRSEX")) {
  d.all[[v]] <- ifelse(d.all[[v]] %in% 6:9, NA, d.all[[v]])
}

d.all$Sex <- ifelse(is.na(d.all$A1PRSEX), d.all$B1PRSEX, d.all$A1PRSEX)
d.all <- d.all[, -c(4, 53)]

d.all$A1SLFEDI <- rowSums(d.all[, stress$lifetimediscrimination] > 0, na.rm = TRUE)

d.all$A1SDAYDI <- rowMeans(5-d.all[, stress$dailydiscrimination], na.rm = TRUE) *
  length(stress$dailydiscrimination)


## d.all[["B1SLFEDI"]] <- ifelse(d.all[["B1SLFEDI"]] %in% 99998, NA, d.all[["B1SLFEDI"]])
## d.all[["B1SJOBDI"]] <- ifelse(d.all[["B1SJOBDI"]] %in% c(98, 99), NA, d.all[["B1SJOBDI"]])
## for (v in c("B1SDAYDI", "A1PAGE_M2", "B1PAGE_M2")) {
##   d.all[[v]] <- ifelse(d.all[[v]] %in% 98, NA, d.all[[v]])
## }
## for (v in c()) {
##   d.all[[v]] <- ifelse(d.all[[v]] %in% 8:9, NA, d.all[[v]])
## }

apply(d.all[, -(1:3)], 2, max, na.rm = TRUE)


## plot(SEMSummary(~., data = d1b[, unlist(stress)]))
## chordDiagram(cor(d1b[, unlist(stress)][, 1:40], use = "pairwise.complete.obs"), symmetric = TRUE)

## make a single age variable
d.all$AGE <- with(d.all, ifelse(is.na(A1PAGE_M2) & !is.na(B1PAGE_M2), B1PAGE_M2 - 9,
                                A1PAGE_M2))


tmp.d.all <- cbind(as.data.frame(winsorizor(d.all[,
                                            c("AGE", "A1PA4", "A1PA5", "A1PA6", "A1SCHRON",
                                              "B1PA1", "B1PA2", "B1PA3", "B1SCHRON",
                                              "A1SPIWOR", "A1SHOMET", "A1SPIHOM", "A1SKINNE", "A1SFDSNE", "A1SSPCRI", "A1SPIFAM",
                                              "A1SLFEDI", "A1SDAYDI",
                                              "LifeStress", "B1SJOBDI", "B1SPIWOR", "B1SHOMET",
                                              "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
                                              "B1SLFEDI", "B1SDAYDI",
                                              "B4QPS_PS", "B4QCT_EA", "B4QCT_PA", "B4QCT_SA", "B4QCT_EN",
                                              "B4QCT_PN", "B4QCT_MD",
                                              "b3tem", "exec_fxn")
                                            ], .005)),
                   d.all[, c("Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel",
                                              "pulpress", "Rb4p1gs", "avgb_rm", "avgb_lf", "avgb_hf", "Sex")])

colnames(tmp.d.all)
apply(tmp.d.all, 2, max, na.rm = TRUE)


if (FALSE) {
cl <- makeCluster(5)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2016-01-15", R.version = "3.2.3")
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

tmp.d.all2 <- scale(tmp.d.all)
scaling_info <- c(
  attr(tmp.d.all, c("scaled:center")),
  attr(tmp.d.all, c("scaled:scale")))
tmp.d.all2 <- as.data.frame(tmp.d.all2)

clusterExport(cl, "tmp.d.all2")
clusterExport(cl, "imputation_seeds")


imputed <- parLapplyLB(cl, 1:5, function(i) {
  set.seed(imputation_seeds[i])
  ## now do the imputation
  dimp.real <- mice(
    tmp.d.all2,
    m = 1,
    method = "rf",
    maxit = 30,
    seed = imputation_seeds[i],
    ntree = 100
  )
  return(dimp.real)
})
saveRDS(imputed, file = "midus_stress_imputed.RDS")

final_imputed <- imputed[[1]]
for (i in 2:length(imputed)) {
  final_imputed <- ibind(final_imputed, imputed[[i]])
}

saveRDS(final_imputed, file = "midus_stress_final_imputed.RDS")
}

final_imputed <- readRDS("midus_stress_final_imputed.RDS")













h2oServer <- h2o.init(max_mem_size = "16g", nthreads = 10)

brel <- ml_rel_mat(tmp.d.all, h2oServer)
saveRDS(brel, file = "~/Desktop/brel.RDS")

brel <- readRDS("brel.RDS")


xvars <- c("AGE", "Sex",
           "A1SPIWOR", "A1SHOMET", "A1SPIHOM", "A1SKINNE",
           "A1SFDSNE", "A1SSPCRI", "A1SPIFAM", "A1SLFEDI", "A1SDAYDI", "LifeStress",
           "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM", "B1SKINNE", "B1SFDSNE",
           "B1SSPCRI", "B1SPIFAM", "B1SLFEDI", "B1SDAYDI", "B4QPS_PS", "B4QCT_EA",
           "B4QCT_PA", "B4QCT_SA", "B4QCT_EN", "B4QCT_PN", "B4QCT_MD")

yvars <- c(
  "A1PA4", "A1PA5", "A1PA6", "B1PA1", "B1PA2", "B1PA3",
  "A1SCHRON", "B1SCHRON",
  "b3tem", "exec_fxn", "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel",
  "pulpress", "Rb4p1gs", "avgb_rm", "avgb_lf", "avgb_hf")
## A1PA6	Self-rated Relative Health (M1)
## B1PA3	Self-rated Relative Health (M2)
## A1PA4	Self-rated Physical Health (M1)
## B1PA1	Self-rated Physical Health (M2)
## A1PA5	Self-rated Mental Health (M1)
## B1PA2	Self-rated Mental Health (M2)
## b4bsesel	sE-Selectin (M2P4)
## b4bsicam	sICAM (M2P4)
## b4bil6	IL-6 (M2P4)
## Rb4bcrp	CRP (M2P4)
## b4bfgn	Fibrinogen (M2P4)
## avgb_lf	LFHRV (M2P4)
## avgb_rm	HRV RMSSD (M2P4)
## avgb_hf	HFHRV (M2P4)
## pulpress	Pulse Pressure (M2P4)
## Rb4p1gs	SBP (M2P4)
## b3tem	Episodic Memory (M2)
## exec_fxn	Executive Function (M2)


yvar.info <- data.frame(
  Vars = c(
    "A1PA6", "B1PA3",
    "A1PA4", "B1PA1",
    "A1PA5", "B1PA2",
    "A1SCHRON", "B1SCHRON",
    "b4bsesel", "b4bsicam", "b4bil6", "Rb4bcrp", "b4bfgn",
    "avgb_lf", "avgb_rm", "avgb_hf",
    "pulpress", "Rb4p1gs",
    "b3tem", "exec_fxn"),
  Labels = c(
    "Self-rated Relative Health (M1)", "Self-rated Relative Health (M2)",
    "Self-rated Physical Health (M1)", "Self-rated Physical Health (M2)",
    "Self-rated Mental Health (M1)", "Self-rated Mental Health (M2)",
    "Chronic Conditions (M1)", "Chronic Conditions (M2)",
    "sE-Selectin (M2P4)", "sICAM (M2P4)", "IL-6 (M2P4)", "CRP (M2P4)", "Fibrinogen (M2P4)",
    "LFHRV (M2P4)", "HRV RMSSD (M2P4)", "HFHRV (M2P4)",
    "Pulse Pressure (M2P4)", "SBP (M2P4)",
    "Episodic Memory (M2)", "Executive Function (M2)"),
  Type = c(rep("ordered", 6), rep("count", 2), rep("continuous", 12)),
  Levels = c(rep(5, 6), rep(NA, 14)))

mygam.fit <- function(dv, form, impdata = final_imputed, impnum = 1, rawdata = tmp.d.all, info = yvar.info, topk = 5, minp = .001, maxp = .1, select = TRUE, ...) {
  final_form <- paste0(dv, form)
  usedata <- complete(impdata, impnum)[!is.na(rawdata[[dv]]), ]

  if (info[which(info$Vars == dv), "Type"] == "ordered") {
    stopifnot(length(unique(usedata[[dv]])) == info[which(info$Vars == dv), "Levels"])

    usedata[[dv]] <- as.integer(factor(usedata[[dv]], levels = sort(unique(usedata[[dv]])), ordered = TRUE))

    obj <- gam(
      formula = as.formula(final_form),
      family = ocat(R = info[which(info$Vars == dv), "Levels"]),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$dev.expl


  } else if (info[which(info$Vars == dv), "Type"] == "count") {
    usedata[[dv]] <- as.integer(factor(usedata[[dv]], levels = sort(unique(usedata[[dv]])), ordered = TRUE)) - 1

    obj <- gam(
      formula = as.formula(final_form),
      family = nb(),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$r.sq

  } else {
    obj <- gam(
      formula = as.formula(final_form),
      data = usedata,
      method = "REML",
      select = select, ...)

    perf <- summary(obj)$r.sq

  }

  tab <- as.data.frame(summary(obj)$s.table)
  tab <- cbind(Vars = gsub("s\\((.*)\\)", "\\1", rownames(tab)),
               tab)
  rownames(tab) <- NULL

  colnames(tab) <- c("Vars", "edf", "Refdf", "statistic", "p")

  if (length(tab$edf) <= topk) {
    minedf <- -.0001
  } else {
    minedf <- tab$edf[which(rank(tab$edf) == (length(tab$edf) - topk))]
  }

  usetab <- subset(tab, (p < minp) | ((p < maxp) & (edf > minedf)))

  return(list(Model = obj, Table = tab, Refined = usetab, Performance = perf, Type = ifelse(info[which(info$Vars == dv), "Type"] == "ordered", "Deviance", "R2")))
}


cl <- makeCluster(5)
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2016-01-15", R.version = "3.2.3")
  library(mgcv)
  library(mice)
})

clusterExport(cl, c("mygam.fit", "final_imputed", "tmp.d.all", "yvar.info"))

mywrapper <- function(dv, k = 5, ...) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", xvars[-(1:2)], ", k = ", k, ", bs = 'ts')"), collapse = " + "))
  clusterExport(cl, "form", envir = env)

  res <- parLapplyLB(cl, 1:5, function(i) mygam.fit(dv, form, impnum = i, ...))
  res.freq <- table(droplevels(unlist(lapply(res, function(x) x$Refined$Vars))))
  res.topvars <- names(res.freq)[res.freq >= 3]

  reducedform <- paste0(" ~ ", paste0(paste0("s(", res.topvars, ", k = ", k, ")"), collapse = " + "))
  clusterExport(cl, "reducedform", envir = env)
  reducedres <- parLapplyLB(cl, 1:5, function(i) mygam.fit(dv, reducedform, impnum = i, select = FALSE, ...))

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




indmultires <- function(dv, iv, k = 5) {
  env <- environment()

  form <- paste0(" ~ ", paste0(paste0("s(", iv, ", k = ", k, ")"), collapse = " + "))
  clusterExport(cl, "form", envir = env)

  ires <- parLapplyLB(cl, 1:5, function(i) mygam.fit(dv, form, impnum = i, select = FALSE))

  tmp <- do.call(rbind, lapply(ires, function(x) x$Refined))


  data.frame(DV = dv, IV = iv,
             Performance = tanh(mean(atanh(sapply(ires, function(x) x$Performance)))),
             P = mean(tmp$p))
}

ires.all <- c(
  lapply(yvars[1:17], function(dv) {
    lapply(xvars[-(1:2)], function(iv) {
      indmultires(dv, iv, k = 5)
    })
  }),
  lapply(yvars[18:20], function(dv) {
    lapply(xvars[-(1:2)], function(iv) {
      indmultires(dv, iv, k = 4)
    })
  }))


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



################################################################################
##                        MIDUS II Biomarker Project                          ##
################################################################################

sol <- paste0("B4WS", 1:7, "OL")
sleepeff <- paste0("B4WS", 1:7, "EFF")
waso <- paste0("B4WS", 1:7, "WSO")
tst <- paste0("B4WS", 1:7, "SLT")

sleep.vars <- c(sol, sleepeff, waso, tst)

dtmp.p4sleep <- dp4[, c("M2ID", unlist(sleep.vars))]

droplist <- list(single = c(8, 9),
                 double = c(98, 99),
                 triple = c(996, 997, 998, 999),
                 quad = c(9996, 9997, 9998, 9999),
                 five = c(99996, 99997, 99998, 99999))

for (v in colnames(dtmp.p4sleep)[-(1)]) {
  ddex <- ifelse(max(dtmp.p4sleep[[v]], na.rm = TRUE) < 10, 1,
                 ifelse(max(dtmp.p4sleep[[v]], na.rm = TRUE) < 100, 2,
                 ifelse(max(dtmp.p4sleep[[v]], na.rm = TRUE) < 1000, 3,
                 ifelse(max(dtmp.p4sleep[[v]], na.rm = TRUE) < 10000, 4,
                 ifelse(max(dtmp.p4sleep[[v]], na.rm = TRUE) < 100000, 5, NA)))))

  dtmp.p4sleep[[v]] <- ifelse(dtmp.p4sleep[[v]] %in% droplist[[c("single", "double", "triple", "quad", "five")[ddex]]],
                     NA, dtmp.p4sleep[[v]])
}



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


##### Merge Actigraphy Sleep w/ PSR #####
dtmp.p4sleep.long <- droplevels(na.omit(reshape(dtmp.p4sleep, varying = list(
                                             sol,
                                             sleepeff,
                                             waso,
                                             tst),
                             v.names = c("SOL", "SleepEff", "WASO", "TST"),
                             timevar = "Day", times = 1:7, idvar = "M2ID",
                             direction = "long")))

dtmp.p4sleep.long <- dtmp.p4sleep.long[with(dtmp.p4sleep.long, order(M2ID, Day)), ]

psrsleep.data <- lapply(1:50, function(i) {
  tmpd <- merge(psr.data[[i]], d.all, by = "M2ID", all = TRUE)
  tmpd <- merge(tmpd, pana.data[[i]][, c("M2ID", "NegAff", "PosAff")], by = "M2ID", all = TRUE)
  tmpout <- merge(tmpd, dtmp.p4sleep.long, by = "M2ID", all.y=TRUE)
  tmpout <- within(tmpout, {
    SOL <- winsorizor(sqrt(SOL), .005)
    TST <- winsorizor(TST, .005)
    WASO <- winsorizor(sqrt(WASO), .005)
    SleepEff <- winsorizor(SleepEff, .005)
    LifeStress <- winsorizor(LifeStress, .005)
    PSR <- scale(scale(PR) + scale(SR))
    dDay <- factor(Day)
    Sex <- factor(Sex)
    A1SDAYDI <- scale(winsorizor(A1SDAYDI, .005))
    B1SDAYDI <- scale(winsorizor(B1SDAYDI, .005))
    A1SLFEDI <- scale(winsorizor(A1SLFEDI, .005))
    B1SLFEDI <- scale(winsorizor(B1SLFEDI, .005))
  })
  return(tmpout)
})




psrsleep.data <- merge(d.all, dtmp.p4sleep.long, by = "M2ID", all.y=TRUE)
psrsleep.data <- within(psrsleep.data, {
    SOL <- winsorizor(sqrt(SOL), .005)
    TST <- winsorizor(TST, .005)
    WASO <- winsorizor(sqrt(WASO), .005)
    SleepEff <- winsorizor(SleepEff, .005)
    LifeStress <- winsorizor(LifeStress, .005)
    #PSR <- scale(scale(PR) + scale(SR))
    dDay <- factor(Day)
    Sex <- factor(Sex)
    A1SDAYDI <- scale(winsorizor(A1SDAYDI, .005))
    B1SDAYDI <- scale(winsorizor(B1SDAYDI, .005))
    A1SLFEDI <- scale(winsorizor(A1SLFEDI, .005))
    B1SLFEDI <- scale(winsorizor(B1SLFEDI, .005))
  })

library(lme4)
library(lmerTest)
library(mice)

stress.vars <- c("A1SSPCRI", "B1SSPCRI", "A1SKINNE", "B1SKINNE", "A1SFDSNE",
"B1SFDSNE", "A1SLFEDI", "B1SLFEDI", "A1SDAYDI", "B1SDAYDI", "B4QPS_PS", "A1SPIWOR", "B1SPIWOR",
"LifeStress", "B4QCT_SA", "B4QCT_PN", "B4QCT_PA", "B4QCT_EA",
"B4QCT_EN", "A1SPIFAM", "B1SPIFAM", "A1SHOMET", "A1SPIHOM", "B1SHOMET",
"B1SPIHOM")

stress.vars <- c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
"A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SHOMET", "A1SPIHOM")


m.sol <- do.call(rbind, lapply(stress.vars, function(v) {
  f <- paste0("scale(SOL) ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))

rownames(m.sol) <- stress.vars
m.sol[order(m.sol[, 5]), ]


m.waso <- do.call(rbind, lapply(stress.vars, function(v) {
  f <- paste0("scale(WASO) ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))
rownames(m.waso) <- stress.vars
m.waso[order(m.waso[, 5]), ]

summary(lmer(scale(SOL) ~ dDay + Sex + B1PAGE_M2 +
               A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI +
               A1SPIWOR + A1SPIFAM + A1SHOMET + A1SPIHOM + (1 | M2ID),
             data = psrsleep.data))
summary(lmer(scale(WASO) ~ dDay + Sex + B1PAGE_M2 +
               A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI +
               A1SPIWOR + A1SPIFAM + A1SHOMET + A1SPIHOM + (1 | M2ID),
             data = psrsleep.data))
summary(lmer(scale(SleepEff) ~ dDay + Sex + B1PAGE_M2 +
               A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI +
               A1SPIWOR + A1SPIFAM + A1SHOMET + A1SPIHOM + (1 | M2ID),
             data = psrsleep.data))
summary(lmer(scale(TST) ~ dDay + Sex + B1PAGE_M2 +
               A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI +
               A1SPIWOR + A1SPIFAM + A1SHOMET + A1SPIHOM + (1 | M2ID),
             data = psrsleep.data))


m.se <- do.call(rbind, lapply(stress.vars, function(v) {
  f <- paste0("scale(SleepEff) ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))
rownames(m.se) <- stress.vars
m.se[order(m.se[, 5]), ]


m.tst <- do.call(rbind, lapply(stress.vars, function(v) {
  f <- paste0("scale(TST) ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))
rownames(m.tst) <- stress.vars
m.tst[order(m.tst[, 5]), ]

#sd(psrsleep.data[!duplicated(psrsleep.data$M2ID), "B1PAGE_M2"])
table(psrsleep.data[!duplicated(psrsleep.data$M2ID), "Sex"])


summary(lmer(TST ~ dDay + Sex + B1PAGE_M2 +
               scale(A1SDAYDI) + scale(B1SLFEDI) +
               (1 | M2ID), data = psrsleep.data))


m.sol <- do.call(rbind, lapply(c("A1SSPCRI", "B1SSPCRI", "A1SKINNE", "B1SKINNE", "A1SFDSNE",
"B1SFDSNE", "A1SLFEDI", "B1SLFEDI", "A1SDAYDI", "B1SDAYDI", "B4QPS_PS", "A1SPIWOR", "B1SPIWOR",
"LifeStress", "B4QCT_SA", "B4QCT_PN", "B4QCT_PA", "B4QCT_EA",
"B4QCT_EN", "A1SPIFAM", "B1SPIFAM", "A1SHOMET", "A1SPIHOM", "B1SHOMET",
"B1SPIHOM"), function(v) {
  f <- paste0("SOL ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))




hist(winsorizor(sqrt(dtmp.p4sleep.long$SOL), .005))
m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(SOL ~ dDay + PSR + (1 | M2ID),
       data = ldat)
})

summary(pool(as.mira(m)))

hist(winsorizor((dtmp.p4sleep.long$TST), .005))
m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(TST ~ dDay + PSR + (1 | M2ID),
       data = ldat)
})

summary(pool(as.mira(m)))

hist(winsorizor(sqrt(dtmp.p4sleep.long$WASO), .005))
m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(WASO ~ dDay + PSR + (1 | M2ID),
       data = ldat)
})

summary(pool(as.mira(m)))


hist(winsorizor((dtmp.p4sleep.long$SleepEff), .005))
m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(SleepEff ~ dDay + PSR + (1 | M2ID),
       data = ldat)
})

summary(pool(as.mira(m)))



m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(SOL ~ dDay + Sex + B1PAGE_M2 +
         (A1SDAYDI * PosAff):Sex  +
         (1 | M2ID),
       data = ldat)
})
summary(pool(as.mira(m)))


m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(WASO ~ dDay + Sex + B1PAGE_M2 +
         (A1SDAYDI * PosAff):Sex  +
         (1 | M2ID),
       data = ldat)
})
summary(pool(as.mira(m)))

m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(SleepEff ~ dDay + Sex + B1PAGE_M2 +
         (A1SDAYDI * PosAff):Sex  +
         (1 | M2ID),
       data = ldat)
})
summary(pool(as.mira(m)))

m <- lapply(psrsleep.data[1:25], function(ldat) {
  lmer(TST ~ dDay + Sex + B1PAGE_M2 +
         B1SDAYDI + B4QCT_SA + #(A1SDAYDI * PosAff):Sex  +
         (1 | M2ID),
       data = ldat)
})
summary(pool(as.mira(m)))











################################################################################
#                                                                              #
#                                      AL                                      #
#                                                                              #
################################################################################

imputed.data.vars <- c(
  "Radj_epi", "Radj_nor", "avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf",
  "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam",
  "b4bsesel", "pulpress", "Rb4p1gs", "Rb4p1d", "b4bha1c",
  "Rb4bgluc", "p4homair", "b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr",
  "b1pgender", "b1page_m2", "M2ID")
bi.vars <- c("bi.sym", "bi.hpa", "bi.card", "bi.gluc", "bi.lipid", "bi.infl", "bi.para", "F")

bi.data <- lapply(1:50, function(i) {
  bi.d <- read.table(paste0("~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/factor_allcov_BiFacplus_nosbpgluc/bialplus_imp", i, ".dat"), na.strings = "*")
  colnames(bi.d) <- c(imputed.data.vars, bi.vars)
  cbind(Imputation = i, bi.d[, c("M2ID", bi.vars)])
})


psr_pana_bi.data <- lapply(1:50, function(i) {
    tmp <- merge(psr.data[[i]][, c("M2ID", "Imputation", "MASTERY", "CONSTRAINT", "SELFESTEEM", "OPTIMISM", "PESSIMISM", "FAMILYSUPT", "FRIENDSUPT", "SPOUSESUPT", "PR", "SR")],
                 bi.data[[i]][, -which(colnames(bi.data[[i]]) %in% "Imputation")], , by = "M2ID", all=TRUE)
    tmp <- merge(tmp, pana.data[[i]][, -which(colnames(pana.data[[i]]) %in% "Imputation")], by = "M2ID", all = TRUE)
    return(tmp)
})


demos <- read.dta("~/Onedrive/Projects/MIDUS/p4subset_biology_3-20-12.dta")[, c("m2id", "m2famnum", "samplmaj", "b1page_m2", "b1pgender")]

ses <- read.dta("~/Onedrive/Projects/MIDUS/data/ses.dta")
#table(subset(ses, m2id %in% p4final[[1]]$M2ID)$m2ed_all, useNA = 'always')

## Table 2: Components used to create Childhood Advantage Score m1chad
## Indicator Variable:	Recode applied to original variable:
## m1welf (on welfare as child)	RECODE OF VARIABLE a1pc14:
## 0 = yes
## 2 = no
## m1cses (financial level growing up)	RECODE OF VARIABLE a1se9:
## 0 = worse off than others
## 1 = same as others
## 2 = better off than others
## m1ped3 (parent highest education)	RECODE OF VARIABLE m1phed:
## 0 = < High School
## 1 = High School/GED
## 2 = Some College+
ses$m1chadv <- rowMeans(ses[, c("m1welf_all", "m1ped3_all", "m1cses_all")], na.rm = TRUE) * 3

## Table 4: Components used to create M2 Adult Advantage Score m2aadv
## Indicator Variable:	Recode applied to original variable:
## m2ed3 (participant education)	RECODE OF VARIABLE m2eddg:
## 0 = high school/GED
## 1 = some college/AA
## 2 = college degree or greater
## m2fpir3 (family-adjusted poverty to income ratio)	RECODE OF VARIABLE m2fpir:
## 0 = <300%
## 1 = 300-599%
## 2 = ≥600%
## m2qf1 (current financial situation)	RECODE OF VARIABLE b1sg1:
## 0 = worst
## 1 = average
## 2 = best
## m2qf6 (enough money to meet needs)	RECODE OF VARIABLE b1sg6:
## 0=not enough
## 1=just enough
## 2=more than enough
## m2qf7 (difficulty paying bills)	RECODE OF VARIABLE b1sg7:
## 0=very or somewhat difficult
## 1=not very difficult
## 2=not difficult at all
ses$m2aadv <- rowMeans(ses[, c("m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all")], na.rm = TRUE) * 5

useses <- ses[, c("m2id", "m1chadv", "m2aadv")]

chronic <- read.dta("~/Onedrive/Projects/MIDUS/data/m2p4chronic_2_7_11.dta")[, c("m2id", "p4majorconditions", "p4minorconditions", "p4sumburden")]


d$LifeStress <- rowSums(d[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")] == 1, na.rm = TRUE)
d$LifeStress[rowSums(is.na(d[, c(paste0("B1SE11", LETTERS[1:26]), "B1SE11AA")])) == 27] <- NA
dm$LifeStress <- rowSums(dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")] == 1, na.rm = TRUE)
dm$LifeStress[rowSums(is.na(dm[, c(paste0("BACES11", LETTERS[1:26]), "BACES11AA")])) == 27] <- NA

## mean(d$LifeStress, na.rm = TRUE) + 3 * sd(d$LifeStress, na.rm = TRUE)
## mean(dm$LifeStress, na.rm = TRUE) + 3 * sd(dm$LifeStress, na.rm = TRUE)
## d$LifeStress2 <- ifelse(d$LifeStress < 10, d$LifeStress, 10)

## # smoking
## smoking.items <- c(
##   "B1PA37", # ever smoke (never = 96, 97 = dont know, 98 = refuse, otherwise age of first cig.)
##   "B1PA38A", # ever smoked cigarettes regularly (a few every day, 1 = yes, 2 = no, 9 = inapp (never smoked cig.)
##   "B1PA38B", # what age started smoking regularly
##   "B1PA39", # do you smoke cigarettes regularly now?
##   # on average about how many cigs did you smoke per day during
##   # the one year in your life when you smoked most heavily
##   # (96=96+, 97 = dont know, 98 = refused, 99 = inapp)
##   "B1PA40" #
## )
## d$Smoke <- recode(as.character(paste0(d$B1PA38A, d$B1PA39)),
##   "'11' = 'current'; '12' = 'past'; '29' = 'never'; else = NA;")

p4dat <- read.table("~\\Onedrive\\Projects\\MIDUS\\ICPSR_29282\\DS0001\\29282-0001-Data.tsv",
                    header=TRUE, sep = '\t', stringsAsFactors = FALSE)
## table(subset(p4dat, M2ID %in% p4final[[1]]$M2ID)$B4Q13O1, useNA = 'always')

p4dat <- within(p4dat, {
  ## Missing Data General
  B4QCESD <- ifelse(B4QCESD == 98, NA, B4QCESD)
  B4QMA_D <- ifelse(B4QMA_D == 98, NA, B4QMA_D)
  B4QMA_A <- ifelse(B4QMA_A == 98, NA, B4QMA_A)
  B4QMA_LI <- ifelse(B4QMA_LI == 98, NA, B4QMA_LI)
  B4QMA_AA <- ifelse(B4QMA_AA == 98, NA, B4QMA_AA)
  B4QMA_PA <- ifelse(B4QMA_PA == 98, NA, B4QMA_PA)

  ## Missing Data PSQI Items
  B4SSQ_GS <- ifelse(B4SSQ_GS == 98, NA, B4SSQ_GS) # global score
  ## also need to get component scores and one specific item to remake global
  ## score without enthusiasm and without subjective sleep quality
  B4SSQ_S1 <- ifelse(B4SSQ_S1 == 8, NA, B4SSQ_S1) # SSQ
  B4SSQ_S2 <- ifelse(B4SSQ_S2 == 8, NA, B4SSQ_S2) # latency
  B4SSQ_S3 <- ifelse(B4SSQ_S3 == 8, NA, B4SSQ_S3) # duration
  B4SSQ_S4 <- ifelse(B4SSQ_S4 == 8, NA, B4SSQ_S4) # efficiency
  ## if efficiency > 100% don't compute
  B4SSQ_S4 <- ifelse(B4SSQ_S4 == 4, 0, B4SSQ_S4) # efficiency
  B4SSQ_S5 <- ifelse(B4SSQ_S5 == 8, NA, B4SSQ_S5) # disturbances
  B4SSQ_S6 <- ifelse(B4SSQ_S6 == 8, NA, B4SSQ_S6) # sleep meds
  B4SSQ_S7 <- ifelse(B4SSQ_S7 == 8, NA, B4SSQ_S7) # daytime dysfunction
  B4S8 <- ifelse(B4S8 == 8, NA, p4dat$B4S8) # trouble staying awake

  ## Missing Data Diet / Nutrition Items
  B4H16[B4H16 == 8] <- NA
  B4H17AF[B4H17AF > 99] <- NA
  B4H17BF[B4H17BF > 99] <- NA
  B4H17CF[B4H17CF > 99] <- NA
  B4H18AF[B4H18AF > 99] <- NA
  B4H18BF[B4H18BF > 99] <- NA
  B4H18CF[B4H18CF > 99] <- NA
  B4H19[B4H19 %in% c(5, 8)] <- NA
  B4H20[B4H20 %in% c(5, 8)] <- NA
  B4H21[B4H21 %in% c(5, 7, 8)] <- NA
  B4H22[B4H22 %in% c(5, 8)] <- NA
  B4H23A[B4H23A == 8] <- NA
  B4H23B[B4H23B == 8] <- NA
  B4H23C[B4H23C == 8] <- NA
  B4H23D[B4H23D == 8] <- NA
  B4H24[B4H24 == 8] <- NA
})

p4dat <- within(p4dat, {
  ## Diet Variables
  ## fast food + high fat meed
  Diet <- (B4H23B + B4H24) / 2
  ## no water or 8+ water
  ## no vegetables
  ## 4+ sugar sweetened beverages per day
  ## no or 5+ whole grains
  ExtremeDiet <- (B4H19 %in% c(1)) +
    (B4H20 >= 3) +
    (B4H21 == 1) +
    (B4H22 %in% c(1, 4))
  ## overall diet previous two, rescaled so each is 0 to 1 range
  ## then summed
  OverallDiet <-
    (B4H23B-1)/4 +
      (B4H24-1)/4 +
      (B4H19 %in% c(1)) +
      (B4H20 >= 3) +
      (B4H21 == 1) +
      (B4H22 %in% c(1, 4))

  ## PSQI with and without various items
  PSQI1 <- rowSums(p4dat[, paste0("B4SSQ_S", c(1, 2, 3, 4, 5, 6, 7))])
  ## PSQI - enthusiasm item
  PSQI2 <- rowSums(p4dat[, c(paste0("B4SSQ_S", c(1, 2, 3, 4, 5, 6)), "B4S8")])
  ## PSQI - enthusiasm item - SSQ
  PSQI3 <- rowSums(p4dat[, c(paste0("B4SSQ_S", c(2, 3, 4, 5, 6)), "B4S8")])

  ## Smoking
  Smoke <- recode(as.character(paste0(B4H26, B4H26A)),
     "'11' = 'current'; c('12', '18') = 'past'; '29' = 'never'; else = NA;",
      as.factor.result = TRUE, levels = c("never", "past", "current"))
  CurrentSmoke <- as.integer(Smoke == "current")

  ## count of not use seatbelt, not use helmet (if ride bike/motorcycle), excessive sun, and not wear sunscreen
  ## categorized into low / high unhealth behavior
  RiskyBehaviors <- as.integer(rowSums(cbind(B4H47 == 2, B4H48A == 2, B4H49 == 1, B4H50 == 2),
                                       na.rm = TRUE) > 2)
  })

p4dat$CurrentAlcohol <- with(p4dat, {
  tmp <- as.character(paste0(as.integer(B4H34 <= 3),
                as.integer(B4H36 >= 4)))
  tmp2 <- recode(tmp, "'11' = 'Frequent4+'; '10' = 'Frequent<4'; '01' = 'Infrequent4+'; '00' = 'Infrequent<4'; else = NA;")
  tmp2[B4H36>=4] <- "Heavy"
  tmp2[B4H33 == 3] <- NA_character_
  tmp2[B4H33 == 2 | B4H34 == 6 | B4H35 == 4] <- "None"
  tmp2
})

p4dat$WorstAlcohol <- with(p4dat, {
  tmp <- as.character(paste0(as.integer(B4H38 <= 3),
                as.integer(B4H40 >= 4)))
  tmp2 <- recode(tmp, "'11' = 'Frequent4+'; '10' = 'Frequent<4'; '01' = 'Infrequent4+'; '00' = 'Infrequent<4'; else = NA;")
  tmp2[B4H40>=4] <- "Heavy"
  tmp2[B4H38 == 9 | B4H38 == 8 | B4H40 > 999] <- NA_character_
  tmp2[B4H38 == 6 | B4H39 == 4] <- "None"
  tmp2
})

physact <- function(x, weights1 = "1 = 3; 2 = 2; 3 = 1;", weights2 = "1 = 5; 2 = 3; 3 = 1;") {
  anyact <- as.integer(p4dat$B4H25 == 1)
  tmp <- lapply(LETTERS[1:7], function(i) {
    freq <- p4dat[, paste0("B4H25", i, "FW")]
    duration <- p4dat[, paste0("B4H25", i, "M")]
    intensity <- p4dat[, paste0("B4H25", i, "I")]
    freq <- ifelse(freq > 7, NA, freq)
    duration <- ifelse(duration > 1000, NA, duration)
    intensity <- ifelse(!intensity %in% 1:3, NA, intensity)
    res1 <- freq * recode(intensity, weights1)
    res2 <- freq * duration * recode(intensity, weights1)
    res3 <- freq * recode(intensity, weights2)
    res4 <- freq * duration * recode(intensity, weights2)
    data.frame(FreqInt = res1, FreqDurInt = res2,
               FreqIntW = res3, FreqDurIntW = res4)
  })

  data.frame(AnyAct = anyact,
    FreqInt = rowSums(sapply(tmp, `[[`, "FreqInt"), na.rm = TRUE),
    FreqDurInt = rowSums(sapply(tmp, `[[`, "FreqDurInt"), na.rm = TRUE),
    FreqIntW = rowSums(sapply(tmp, `[[`, "FreqIntW"), na.rm = TRUE),
    FreqDurIntW = rowSums(sapply(tmp, `[[`, "FreqDurIntW"), na.rm = TRUE))
}

p4dat <- cbind(p4dat, physact(p4dat))

p4dat$PhysAct <- cut(p4dat$FreqDurInt,
                     breaks = c(-Inf, 1, quantile(subset(p4dat, FreqDurInt > 0)$FreqDurInt, probs = c(1/3, 2/3)), Inf),
                     labels = 1:4, ordered_result = TRUE)


included.variables <- c(
  "Diet", "ExtremeDiet", "OverallDiet",
  "PSQI1", "PSQI2", "PSQI3",
  "Smoke", "CurrentSmoke",
  "RiskyBehaviors", "CurrentAlcohol", "WorstAlcohol",
  ## different variants and weights of physical activity composite
  "FreqInt", "FreqDurInt", "FreqIntW", "FreqDurIntW", "PhysAct",
  "B4QCESD", "B4QMA_D", "B4QMA_A", "B4QMA_LI", "B4QMA_AA", "B4QMA_PA")


misc.d <- merge(rbind(d[, c("M2ID", "LifeStress")], dm[, c("M2ID", "LifeStress")]),
                demos, by.x = "M2ID", by.y = "m2id", all = TRUE)
misc.d <- merge(misc.d, useses, by.x = "M2ID", by.y = "m2id", all = TRUE)
misc.d <- merge(misc.d, chronic, by.x = "M2ID", by.y = "m2id", all = TRUE)
misc.d <- merge(misc.d, p4dat[, c("M2ID", included.variables)], by = "M2ID", all = TRUE)

misc.d.final <- misc.d[, c("M2ID", "m2famnum", "b1page_m2", "b1pgender",
  "m1chadv", "m2aadv", "LifeStress",
  "p4majorconditions", "p4minorconditions", "p4sumburden",
  included.variables)]

misc.d.final <- subset(misc.d.final, M2ID %in% na.omit(psr_pana_bi.data[[1]][, c("M2ID", "F")])$M2ID)
misc.d.final <- within(misc.d.final, {
  Diet <- factor(Diet, ordered = TRUE)
  ExtremeDiet <- factor(ExtremeDiet, ordered = TRUE)
  OverallDiet <- factor(OverallDiet, ordered = TRUE)
  CurrentSmoke <- factor(CurrentSmoke)
  RiskyBehaviors <- factor(RiskyBehaviors, ordered = TRUE)
  CurrentAlcohol <- factor(CurrentAlcohol)
  WorstAlcohol <- factor(WorstAlcohol)
})


if (FALSE) {
## imputed before but not necessary as Mplus can use imputed data with FIML
d.2.impute <- misc.d.final[, -(1:2)]

pMat <- matrix(1L, nrow = ncol(d.2.impute), ncol = ncol(d.2.impute))
diag(pMat) <- 0L
colnames(pMat) <- rownames(pMat) <- colnames(d.2.impute)

misc.d.final.imputed <- mice(misc.d.final[, -(1:2)],
  m = 50,
  maxit = 20,
  defaultMethod = c("fastpmm", "logreg", "polyreg", "polr"),
                             seed = 235419324)

saveRDS(misc.d.final.imputed,
        file = "~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/misc_d_final_imputed.rds")


n <- function(x) as.numeric(as.character(x))

misc.d.final.imputed2 <- lapply(1:50, function(i) {
  x <- complete(misc.d.final.imputed, i)
  within(cbind(misc.d.final[, 1:2], x), {
   dSES <- m2aadv - m1chadv
   b1pgender <- as.integer(b1pgender == "female")
   Diet <- n(Diet)
   ExtremeDiet <- n(ExtremeDiet)
   OverallDiet <- n(OverallDiet)
   CurrentSmoke <- as.integer(CurrentSmoke == 1)
   RiskyBehaviors <- as.integer(RiskyBehaviors == 1)
   CurrentAlcohol <- as.integer(factor(CurrentAlcohol,
                                      levels = c("None", "Infrequent<4", "Frequent<4", "Heavy"))) - 1L
   WorstAlcohol <- as.integer(factor(WorstAlcohol,
                                     levels = c("None", "Infrequent<4", "Frequent<4", "Heavy"))) - 1L
   PhysAct <- n(PhysAct)
   Milwaukee <- as.integer(is.na(m2famnum))
   Imputation <- i
   m2famnum <- ifelse(is.na(m2famnum), M2ID, m2famnum)
 })
})

saveRDS(misc.d.final.imputed2,
        file = "~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/misc_d_final_imputed2.rds")
}

p4final <- lapply(1:50, function(i) {
                    merge(misc.d.final,
                          psr_pana_bi.data[[i]], by = "M2ID", all.x=TRUE)
                  })

saveRDS(p4final, "~/Onedrive/Projects/MIDUS/JW-MIDUS-AL/p4final.RDS")

#### A lot of work to come up with how to derive cut points for physical activity
## testit <- merge(testit, misc.d, by = "M2ID", all.x = TRUE)
## testit <- merge(testit, read.dta("~/Documents/Research/MIDUS/NEW_AL_measures_WITH_MEDS_12-19-12.dta")[, c("m2id", "RXNNtotAL")], by.x = "M2ID", by.y = "m2id", all.x = TRUE)

## ggplot(testit) +
##     stat_summary(aes(p4majorconditions - .05, y = scale(FreqInt)),
##                  fun.data = mean_cl_boot, colour = "blue") +
##     stat_summary(aes(p4majorconditions - .1, y = scale(FreqIntW)),
##                  fun.data = mean_cl_boot, colour = "red") +
##     stat_summary(aes(p4majorconditions + .05, y = scale(FreqDurInt)),
##                  fun.data = mean_cl_boot, colour = "green") +
##     stat_summary(aes(p4majorconditions + .1, y = scale(FreqDurIntW)),
##                  fun.data = mean_cl_boot, colour = "purple") +
##     theme_bw()

## psummary <- function(object) {
##     exp(cbind(Est = coef(object), confint(object)))
## }

## psummary(glm(p4majorconditions ~ scale(FreqInt) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(FreqIntW) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(FreqDurInt) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(FreqDurIntW) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(sqrt(FreqDurIntW)) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(sqrt(FreqDurIntW)) + !AnyAct, data = testit, family = poisson))
## psummary(glm(p4majorconditions ~ scale(FreqDurIntW) + scale(FreqDurIntW^2) + !AnyAct, data = testit, family = poisson))
## ggplot(testit, aes(sqrt(FreqDurInt), scale(RXNNtotAL))) +
##     geom_point() + stat_smooth(se = FALSE) +
##     geom_vline(aes(xintercept = sqrt(1))) +
##     geom_vline(aes(xintercept = sqrt(430))) +
##     geom_vline(aes(xintercept = sqrt(1080)))


#### depression ####
# depressed affect
dep.items <- c(
  ###
  "B1PDEPAF", # 0 - 7 sum of items below
  # during two weeks in npast 12 months, when you felt sad, blue, or depressed, did you
  # lose interest in most things?
  # feel more tired out or low on energy than is usual?
  # lose your appetitte or appetite increased?
  # have more trouble falling asleep than usual?
  # have a lot more trouble concentrating than usual?
  # feel down on yourself, no good, or worthless?
  # think a lot about death?
  ###
  # anhedonia, sum of 6 items below
  "B1PANHED",
  # during two weeks when you lost interest in most things did you
  # feel more tired out or low on energy than is usual
  # los your appetite or appettite increased
  # have more trouble falling assleep than usual
  # have a lot more trouble concentrating than usual
  # feel down on yourself, no good, or worthless
  # think a lot about death
  ###
  # binary variable based on depressed affect and anhedonia
  "B1DEPRE",
  # binary depression variable
  "B1PDEPDX")

anxiety.items <- c(
  # sum of 'most days' responses to the items
  "B1PANXIE",
  # how often over the past 12 months you
  # were restless because of your worry
  # were keyed up on edge or had a lot of nervous energy
  # were irritable becausew of your worry
  # had trouble falling asleep
  # had trouble staying asleep because of your worry
  # had trouble keeping your mind on what you were doing
  # had trouble remembering things ecause of your worry
  # were low on energy
  # tired easily because of your worry
  # had sore or aching muscles because of tension
  ### binary diagnosis
  "B1PANXTD"
)

# big five
personality.items <- c(
  "B1SNEURO", # 4 adjectives
  "B1SEXTRA", # 5 adjectives
  "B1SOPEN",  # 7 adjectives
  "B1SCONS2", # 5 adjectives
  "B1SAGREE"  # 5 adjectives
)

d[, personality.items] <- lapply(d[, personality.items], function(x) {x[which(x == 8)] <- NA; return(x)})














require(mi)
zout <- function(x) (scale(x) >= -4) & (scale(x) <= 4)

mycombine <- function(formula, d, sex) {
  fit <- lapply(d, function(xd) {
    m <- summary(lm(formula, data = subset(xd, b1pgender == sex)))
    tmp <- coef(m)[, 1:2]
    r2 <- m$r.squared
    r2.se <- 1/(length(m$residuals) - 3)
    tmp <- rbind(tmp, cbind(atanh(r2), r2.se))
    rownames(tmp)[nrow(tmp)] <- "Rsquared"
    return(tmp)
  })

  out <- do.call(cbind, mi.pooled(
    lapply(fit, function(x) x[, 1]),
    lapply(fit, function(x) x[, 2])))

  out <- cbind(out, LL = out[, 1] - 1.96 * out[, 2])
  out <- cbind(out, UL = out[, 1] + 1.96 * out[, 2])
  out[nrow(out), c(1, 3, 4)] <- tanh(out[nrow(out), c(1, 3, 4)])

  return(out)
}

mres <- list(
f = mycombine(scale(F) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
sym = mycombine(scale(bi.sym) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
hpa = mycombine(scale(bi.hpa) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
card = mycombine(scale(bi.card) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
gluc = mycombine(scale(bi.gluc) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
lipid = mycombine(scale(bi.lipid) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
infl = mycombine(scale(bi.infl) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
para = mycombine(scale(bi.para) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"))

mres2 <- list(
f = mycombine(scale(F) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
sym = mycombine(scale(bi.sym) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
hpa = mycombine(scale(bi.hpa) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
card = mycombine(scale(bi.card) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
gluc = mycombine(scale(bi.gluc) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
lipid = mycombine(scale(bi.lipid) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
infl = mycombine(scale(bi.infl) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
para = mycombine(scale(bi.para) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"))


mycombine2 <- function(f, fwo, d) {
  res <- mycombine(f, f, d = d)
  reswo <- mycombine(fwo, f, d = d)
  list(dR2 = res[nrow(res), 1] - reswo[nrow(reswo), 1],
       res = res, reswo = reswo)
}

mylm <- function(f, fwo, d) {
  d <- na.omit(d[zout(d[, all.vars(f)[1]]), all.vars(f)])
  summary(lm(f, data = d))$r.squared - summary(lm(fwo, data = d))$r.squared
}





mal <- mplusObject(
VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
 B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F
    b1page_m2;

CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

    CLUSTER = m2famnum;
    idvariable = m2id;

",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  ! PSR model
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery*1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt*1  friendsupt*1  spousesupt*1;
  PR@1; SR@1;

  ! AL model
    Radj_epi Radj_nor ON b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1page_m2;
    Rb4p1d ON b1page_m2;
    Radj_crt b4bdheas ON b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON b1page_m2;
    Rb4p1gs pulpress ON b1page_m2;
    Rb4bgluc p4homair b4bha1c ON b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON b1page_m2;

    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;

    F BY Radj_epi* Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;
    F@1;
    F WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    Rb4bgluc@0;

   F ON pr (psrf)
        sr (psrf);
   sym ON pr (psrs)
        sr (psrs);
   para ON pr (psrp)
        sr (psrp);
   hpa ON pr (psrh)
        sr (psrh);
   infl ON pr (psri)
        sr (psri);
   card ON pr (psrc)
        sr (psrc);
   gluc ON pr (psrg)
        sr (psrg);
   lipid ON pr (psrl)
        sr (psrl);

  MODEL TEST:
  PSRF = 0;
  psrs = 0;
  psrp = 0;
  psrh = 0;
  psri = 0;
  psrc = 0;
  psrg = 0;
  psrl = 0;

",
OUTPUT = "STDYX;",
usevariables = c(unlist(psr.items), unlist(vars),
    "m2famnum", "m2id", "b1pgender", "b1page_m2",
    "Agecat", "white", "p4majorconditions", "p4minorconditions",
    "p4sumburden", "b3tem", "exec_fxn", "compression",
    "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
rdata = within(d3m, {b1pgender <- as.numeric(b1pgender) - 1}))

mal.female <- mplusModeler(update(mal, rdata = subset(mal$rdata, b1pgender == 1)), "MIDUS_psrs_al_female.dat", run=TRUE)

mal.male <- mplusModeler(update(mal, rdata = subset(mal$rdata, b1pgender == 0)), "MIDUS_psrs_al_male.dat", run=TRUE)

mal.all <- mplusModeler(mal, "MIDUS_psrs_al_all.dat", run=TRUE)






mgal <- mplusObject(
VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
 B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F
    b1page_m2;

CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D !B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

    GROUPING = b1pgender (0 = male 1 = female);

    CLUSTER = m2famnum;
    idvariable = m2id;

",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  ! PSR model
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;


  ! AL model
    Radj_epi Radj_nor ON b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1page_m2;
    Rb4p1d ON b1page_m2;
    Radj_crt b4bdheas ON b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON  b1page_m2;
    Rb4p1gs pulpress ON  b1page_m2;
    Rb4bgluc p4homair b4bha1c ON  b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON  b1page_m2;

    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;

    F BY Radj_epi* Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;
    F@1;
    F WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    Rb4bgluc@0;

   F sym para hpa infl card gluc lipid ON pr sr;

",
OUTPUT = "STDYX;",
usevariables = c(unlist(psr.items), unlist(vars),
    "m2famnum", "m2id", "b1pgender", "b1page_m2",
    "Agecat", "white", "p4majorconditions", "p4minorconditions",
    "p4sumburden", "b3tem", "exec_fxn", "compression",
    "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
rdata = within(d3m, {b1pgender <- as.numeric(b1pgender) - 1}))

mgal <- mplusModeler(mgal, "MIDUS_psrs_al_mg.dat", run=TRUE)






mg <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

  GROUPING = b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  MODEL OVERALL:
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;
",
OUTPUT = "STDYX; MODINDICES (ALL 200);",
usevariables = c("b1prsex", unlist(psr.items)),
rdata = d3m)

mg <- mplusModeler(mg, "MIDUS_psrs_mg.dat", run=TRUE)



m1 <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PSR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1
    familysupt  friendsupt  spousesupt;
",
OUTPUT = "STDYX;",
usevariables = unlist(psr.items),
rdata = d3)

m1 <- mplusModeler(m1, "MIDUS_psrs1.dat", run=TRUE)


m2 <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2 b1prsex;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", "b1prsex", unlist(psr.items)),
rdata = d3m)

m2 <- mplusModeler(m2, "MIDUS_psrs_AL.dat", run=TRUE)


########

m2.m <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

 !GROUPING is b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", unlist(psr.items)),
rdata = subset(d3m, b1prsex == 0))

m2.m <- mplusModeler(m2.m, "MIDUS_psrs_AL_males.dat", run=TRUE)

m2.f <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

 !GROUPING is b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", unlist(psr.items)),
rdata = subset(d3m, b1prsex == 1))

m2.f <- mplusModeler(m2.f, "MIDUS_psrs_AL_females.dat", run=TRUE)


compareModels(m2$results, m2b$results, diffTest=TRUE)
