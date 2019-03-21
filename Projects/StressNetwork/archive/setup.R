library(checkpoint)
checkpoint("2016-01-15", R.version = "3.2.3")

## for setup.R
library(foreign)
library(MplusAutomation)
library(semutils)
library(ggplot2)
library(texreg)

## for setup_questionnaires.R
library(psych)
library(car)
library(foreign)
library(ggplot2)
library(gridExtra)
library(mice)
library(pscore)
library(parallel)
library(caret)
library(doParallel)
library(ipred)
library(randomForest)

rfoo <- function(object) {
  tmp <- paramExtract(object$results$parameters$unstandardized, type = "undirected")
  print(sd(tmp$est))
  hist(tmp$est, breaks = 20)
  index <- abs(tmp$est) > .2
  print(tmp[index, ])
  print(table(index)/253)
  return(invisible(tmp))
}

fit <- function(x) {
  with(x$results$summaries, {
    data.frame(Value = c(round(c(Param = Parameters,
               Chi2 = ChiSqM_Value, DF = ChiSqM_DF, P = ChiSqM_PValue,
               CFI = CFI, TLI = TLI, RMSEA = RMSEA_Estimate, RMSEA_LL = RMSEA_90CI_LB, RMSEA_UL = RMSEA_90CI_UB, SRMR = SRMR,
                   AIC = AIC, #AICC = AICC,
                   BIC = BIC,
                   #aBIC = aBIC,
                   LL = LL), 3)),
               stringsAsFactors = FALSE)
  })
}

base <- "~/OneDrive/Projects/StressNetwork/"

if (FALSE) {
dat <- read.dta("~/OneDrive/Projects/MIDUS/p4subset_biology_3-20-12.dta")
demodat <- read.dta("~/OneDrive/Projects/MIDUS/demographics_for_JW.dta")
meddat <- read.dta("~/OneDrive/Projects/MIDUS/p4_med_flags_10-28-12.dta")
refdat <- read.dta("~/OneDrive/Projects/MIDUS/NEW_AL_measures_WITH_MEDS_12-19-12.dta")
refdatnomed <- read.dta("~/OneDrive/Projects/MIDUS/NEW_AL_measures_NO_MEDS_12-19-12.dta")
cog <- read.dta("~/OneDrive/Projects/MIDUS/data/btact.dta")
chronic <- read.dta("~/OneDrive/Projects/MIDUS/data/m2p4chronic_2_7_11.dta")
ses <- read.dta("~/OneDrive/Projects/MIDUS/data/ses.dta")
bone <- read.dta("~/OneDrive/Projects/MIDUS/data/ses_zallo_bone.dta")


med.vars <- c("fg_bpup", "fg_bpdwn", "fg_hrdwn", "fg_hrup",
  "fg_hpaup", "fg_sympdwn", "fg_sympup", "fg_paradwn",
  "fg_paraup", "fg_infldwn", "fg_influp", "fg_glucdwn",
  "fg_glucup", "fg_rxchol")

## vars <- list(
##   sympathetic = c("Radj_epi", "Radj_nor"),
##   parasympathetic = c("avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf"),
##   hpa = c("Radj_crt", "b4bdheas"),
##   inflammation = c("Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel"),
##   cardiovascular = c("pulpress", "Rb4p1gs", "Rb4p1d"),
##   glucose = c("b4bha1c", "Rb4bgluc", "p4homair"),
##   lipid = c("b4bldl", "b4bhdl", "Rb4btrig", "Rb4pbmi", "Rb4pwhr")
## )

vars <- list(
  sympathetic = c("Radj_epi", "Radj_nor"),
  parasympathetic = c("avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf", "Rb4p1d"),
  hpa = c("Radj_crt", "b4bdheas"),
  inflammation = c("Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel"),
  cardiovascular = c("pulpress", "Rb4p1gs"),
  glucose = c("b4bha1c", "Rb4bgluc", "p4homair"),
  lipid = c("b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr")
)

vars.hr <- list(
  sympathetic = c("hr25aepi", "hr25anor"),
  parasympathetic = c("hr25avgsd", "hr25avgrm", "hr25avglf", "hr25avghf", "hr25pulse"),
  hpa = c("hr25acrt", "hr25dheas"),
  inflammation = c("hr25crp", "hr25il6", "hr25fgn", "hr25icam", "hr25esel"),
  cardiovascular = c("hr25ppress", "hr25sbp"),
  glucose = c("hr25ha1c", "hr25gluc", "hr25homair"),
  lipid = c("hr25ldl", "hr25hdl", "hr25trig", "hr25bmi", "hr25whr")
)

all(unlist(vars) %in% colnames(dat))
all(unlist(vars.hr) %in% colnames(dat))


fdat <- dat[, unlist(vars)]
fdat[] <- lapply(fdat, function(x) {
  x[x %in% c(999999, 99999, 9999, 999, 998)] <- NA
  return(x)
})

## p.un <- ggplot(reshape2::melt(fdat), aes(value)) +
##   geom_histogram() +
##   facet_wrap(~variable, scales="free")


## bx1 <- as.data.frame(boxcox(b4bha1c ~ 1, seq(-5, 5, .1), data = fdat))
## bx2 <- as.data.frame(boxcox(Rb4bgluc ~ 1, seq(-5, 5, .1), data = fdat))
## bx1[which.max(bx1$y), ]
## bx2[which.max(bx2$y), ]

fdat <- within(fdat, {
  Radj_epi <- log(Radj_epi)
  Radj_nor <- log(Radj_nor)
  avgb_sd <- log(avgb_sd)
  avgb_rm <- log(avgb_rm)
  avgb_lf <- log(avgb_lf)
  avgb_hf <- log(avgb_hf)
  Radj_crt <- log(Radj_crt)
  b4bdheas <- log(b4bdheas)
  Rb4bcrp <- log(Rb4bcrp)
  b4bil6 <- log(b4bil6)
  b4bsesel <- log(b4bsesel)
  ## b4bha1c <- b4bha1c^-2.4 *-1
  ## Rb4bgluc <- Rb4bgluc^-2.3 *-1
  b4bha1c <- log(b4bha1c)
  Rb4bgluc <- log(Rb4bgluc)
  p4homair <- log(p4homair)
  Rb4btrig <- log(Rb4btrig)
})


fdat[] <- lapply(colnames(fdat), function(n) {
  x <- fdat[, n]

  LL <- quantile(x, probs = .005, na.rm=TRUE)
  UL <- quantile(x, probs = .995, na.rm=TRUE)
  x <- pmax(pmin(x, UL), LL)
  return(as.vector(scale(x)))
})

# multivariate normality (not really)
mvqq(fdat[, unlist(vars)])

fdat <- cbind(dat[, c("m2id", "m2famnum", "b1pgender", "b1page_m2")], fdat)

p.trans <- ggplot(reshape2::melt(fdat[, unlist(vars)]), aes(value)) +
  geom_histogram() +
  facet_wrap(~variable, scales="free")

tmplong <- reshape2::melt(fdat[, c("b1page_m2", unlist(vars))], id.vars="b1page_m2")
tmplong$age <- cut(tmplong$b1page_m2, breaks = quantile(fdat$b1page_m2), include.lowest=TRUE)

p.trans <- ggplot(tmplong, aes(value, y = ..density..)) +
  geom_histogram() +
  facet_grid(age ~ variable)

p.scat <- ggplot(tmplong, aes(b1page_m2, value)) +
  geom_point(alpha = .1) + stat_smooth(size=1.5) +
  facet_wrap(~ variable) + theme_bw()

pdf("age_bio.pdf", width = 11, height = 8.5)
print(p.scat)
dev.off()
pdf("bio_distribution.pdf", width = 11, height = 8.5)
print(p.un + theme_bw())
print(p.trans + theme_bw())
dev.off()

fdat <- merge(fdat, within(demodat[, c("m2id", "m1m2race")], {
  white <- as.integer(m1m2race==1)
}), by = "m2id", all.x = TRUE)
fdat <- merge(fdat, meddat[, c("m2id", med.vars)], by = "m2id", all.x = TRUE)
fdat <- merge(fdat, refdat[, c("m2id", "RXNNregAL")], by = "m2id", all.x = TRUE)
fdat <- merge(fdat, refdatnomed[, c("m2id", "GNNregAL")], by = "m2id", all.x = TRUE)
fdat <- merge(fdat, cog, by = "m2id", all.x = TRUE)
fdat <- merge(fdat, chronic[, c("m2id", "samplmaj", "p4majorconditions", "p4minorconditions", "p4sumburden")],
              by = "m2id", all.x = TRUE)
fdat <- merge(fdat, bone, by = "m2id", all.x = TRUE)
fdat <- merge(fdat, ses[, c("m2id", "m2fpir3_all")], by = "m2id", all.x = TRUE)

fdat$Agecat <- as.numeric(cut(fdat$b1page_m2, breaks = c(0, 45, 60, 100)))
fdat <- within(fdat, {
  m2famnum <- ifelse(is.na(m2famnum), m2id, m2famnum)
})



fdat2 <- fdat
fdat2[, vars$sympathetic][] <- lapply(vars$sympathetic, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_sympdwn + fg_sympup + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$parasympathetic][] <- lapply(vars$parasympathetic, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_paradwn + fg_paraup + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$hpa][] <- lapply(vars$hpa, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_hpaup + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$inflammation][] <- lapply(vars$inflammation, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_infldwn + fg_influp + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$cardiovascular][] <- lapply(vars$cardiovascular, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_bpdwn + fg_bpup + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$glucose][] <- lapply(vars$glucose, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_glucdwn + fg_glucup + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})
fdat2[, vars$lipid][] <- lapply(vars$lipid, function(x) {
  resid(lm(as.formula(paste(x, "~", "fg_rxchol + b1pgender + b1page_m2")), data = fdat, na.action = na.exclude))
})

mvqq(fdat2[, unlist(vars)])

rs <- SEMSummary(~ ., data = fdat2[, unlist(vars)], use = "pairwise.complete.obs")
plot(rs, plot = "cor", order = 'asis')
dev.new()
rs <- SEMSummary(~ ., data = fdat[, unlist(vars)], use = "pairwise.complete.obs")
plot(rs, plot = "cor", order = 'asis')

save(fdat, dat, demodat, meddat, refdat, refdatnomed, cog, chronic, bone, ses, med.vars, vars, vars.hr, file = "midus.RData")
}

load("midus.RData")

desc <- sapply(c("b1page_m2", unlist(vars)), function(v) {
   sprintf("%0.2f (%0.2f, %0.2f)",
     Median = median(dat[,v], na.rm = TRUE),
     LQ = as.vector(quantile(dat[, v], probs = .25, na.rm=TRUE)),
     UQ = as.vector(quantile(dat[, v], probs = .75, na.rm=TRUE)))
   ## data.frame(
   ##   Median = median(dat[,v], na.rm = TRUE),
   ##   LQ = as.vector(quantile(dat[, v], probs = .25, na.rm=TRUE)),
   ##   UQ = as.vector(quantile(dat[, v], probs = .75, na.rm=TRUE)),
   ##   Valid = sum(!is.na(dat[, v])))
})
names(desc) <- c("age", unlist(vars))

write.table(data.frame(Vars = names(desc), MIQR = as.vector(desc)), "clipboard", row.names = FALSE, sep = '\t')

table(dat$b1pgender)
prop.table(table(dat$b1pgender)) * 100

table(fdat$m1m2race)
prop.table(table(fdat$m1m2race)) * 100


if (FALSE) {
plot(p4majorconditions ~ RXNNregAL, data = fdat)

require(gridExtra)

grid.arrange(
  ggplot(fdat, aes(GNNregAL, RXNNregAL)) +
    geom_point(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(GNNregAL, reg_izallo)) +
    geom_point(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(RXNNregAL, reg_izallo)) +
    geom_point(alpha = .3) +
    theme_classic(), ncol = 3)

cor(fdat[, c("GNNregAL", "RXNNregAL", "reg_izallo")], use = 'pairwise.complete.obs')^2

grid.arrange(
  ggplot(fdat, aes(factor(p4majorconditions), RXNNregAL)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(factor(p4majorconditions), GNNregAL)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(factor(p4majorconditions), reg_izallo)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(factor(p4minorconditions), RXNNregAL)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(factor(p4minorconditions), GNNregAL)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(),
  ggplot(fdat, aes(factor(p4minorconditions), reg_izallo)) +
    geom_boxplot() +
    geom_jitter(alpha = .3) +
    theme_classic(), ncol = 3)

round(cor(fdat[, c("GNNregAL", "RXNNregAL", "reg_izallo",
  "p4majorconditions", "p4minorconditions", "p4sumburden")], use = 'pairwise.complete.obs'), 3)

summary(lm(GNNregAL ~ factor(p4majorconditions), data = fdat))$r.squared
summary(lm(RXNNregAL ~ factor(p4majorconditions), data = fdat))$r.squared
summary(lm(reg_izallo~ factor(p4majorconditions), data = fdat))$r.squared

summary(lm(GNNregAL ~ factor(p4minorconditions), data = fdat))$r.squared
summary(lm(RXNNregAL ~ factor(p4minorconditions), data = fdat))$r.squared
summary(lm(reg_izallo~ factor(p4minorconditions), data = fdat))$r.squared

summary(lm(GNNregAL ~ sqrt(p4sumburden), data = fdat))$r.squared
summary(lm(RXNNregAL ~ sqrt(p4sumburden), data = fdat))$r.squared
summary(lm(reg_izallo~ sqrt(p4sumburden), data = fdat))$r.squared

summary(lm(GNNregAL ~ b3tem, data = fdat))$r.squared
summary(lm(RXNNregAL ~ b3tem, data = fdat))$r.squared
summary(lm(reg_izallo ~ b3tem, data = fdat))$r.squared

summary(lm(GNNregAL ~ exec_fxn, data = fdat))$r.squared
summary(lm(RXNNregAL ~ exec_fxn, data = fdat))$r.squared
summary(lm(reg_izallo ~ exec_fxn, data = fdat))$r.squared



grid.arrange(
  ggplot(fdat, aes(GNNregAL, b3tem)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
  ggplot(fdat, aes(RXNNregAL, b3tem)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
  ggplot(fdat, aes(reg_izallo, b3tem)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
  ggplot(fdat, aes(GNNregAL, exec_fxn)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
  ggplot(fdat, aes(RXNNregAL, exec_fxn)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
  ggplot(fdat, aes(reg_izallo, exec_fxn)) +
    geom_point(alpha = .3) + stat_smooth() +
    theme_classic(),
    ncol = 3)

}
