source("../MIDUS-Setup/setup_packages_functions.R")
options(stringsAsFactors = FALSE, digits = 3)

mVars <- read.xlsx("../MIDUS-Setup/midus_master_vars.xlsx", sheetIndex = 1)
mVars <- as.data.table(mVars)
setkey(mVars, "Domain")

load("../MIDUS-Setup/midus_merged_data.RData")

mVars[Domain == "Stress", .(Name, Abbr, M1)][!is.na(M1)]

dwl2 <- dwl[, .(
  TST = mean(TST/60, na.rm = TRUE),
  SOL = mean(SOL/60, na.rm = TRUE),
  SE = mean(SE, na.rm = TRUE),
  WASO = mean(WASO/60, na.rm = TRUE),
  RT2 = mean(RT2, na.rm = TRUE),
  BT2 = mean(BT2, na.rm = TRUE)), by = M2ID][, Index := Reduce(`+`, lapply(.SD, is.na)), by = M2ID][Index < 6]

sdat <- merge(dw, dwl2, by = "M2ID", all.y = TRUE)

## "A1PA4", "A1PA5", "A1PA6", "A1SCHRON",
stress.vars <- c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
                 "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SHOMET", "A1SPIHOM",
                 "ParentAbuse", "SiblingAbuse", "ChildLifeStress")
sleep.vars <- c("RT2", "BT2", "TST", "SOL", "SE", "WASO")

stress.labs <- data.table(
  levels = c("A1SDAYDI", "A1SLFEDI", "ChildLifeStress",
             "A1SKINNE", "A1SFDSNE", "A1SSPCRI",
             "A1SPIFAM", "A1SPIWOR", "A1SPIHOM",
             "A1SHOMET", "ParentAbuse", "SiblingAbuse"),
  labels = c("Daily Discrimination", "Lifetime Discrimination", "Childhood SLEs",
             "Family Strain", "Friend Strain", "Partner Strain",
             "Family Inequality", "Work Inequality", "Home Inequality",
             "Neighborhood Quality", "Parental Abuse", "Sibling Abuse"
             ))

sdat[, SOL := log(SOL)]
sdat[, WASO := sqrt(WASO)]

sdat[, c(stress.vars, sleep.vars, "OtherAbuse") := lapply(.SD, function(x) (winsorizor(x, .005))),
     .SDcols = c(stress.vars, sleep.vars, "OtherAbuse")]


## if (FALSE) {
##   cl <- makeCluster(2)
##   registerDoParallel(cl)
##   clusterEvalQ(cl, library(mice))
##   clusterEvalQ(cl, library(randomForest))
##   imputation_seeds <- c(403L, 624L, -1394370482L, -1723143049L, 2071488076L, 1659356893L,
##                         -1081051142L, 885114163L, -614367016L, 561456377L, -29212570L,
##                         951631791L, -1923325404L, 1176136405L, -446079598L, 1823627243L,
##                         1899124272L, -1981844367L, 894106494L, 2001652199L)
##   sdatimporig <- sdatimp <- as.data.frame(sdat[, unique(c(stress.vars, "OtherAbuse", sleep.vars,
##     "Sex", "B1PAGE_M2", "white", "RaceG3", "A1SCHRON", "A1PDEPDX", "A1PANXTD",
##     "A1PA54", "A1PSmoke", "B1PDEPDX", "B1PANXTD",
##     "B1SCHRON", "A1SBMI", "B1SBMI",
##     "Smoke", "CurrentSmoke", "PSQI1", "ExtremeDiet",
##     "CurrentAlcohol", "WorstAlcohol",
##     "m1chadv", "m2aadv", "LifeStress",
##     "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
##     "B4QPS_PS", "CTQTotal", "B4QCT_PA"
##     )), with = FALSE])
##   clusterExport(cl, "sdatimp")
##   clusterExport(cl, "imputation_seeds")
##   micedimp <- parLapply(cl, 1:20, function(i) {
##     set.seed(imputation_seeds[i])
##     mice(sdatimp, m = 1,
##          method = "rf",
##          ntree = 20,
##          maxit = 20,
##          seed = imputation_seeds[i])
##   })
##   save(micedimp, file = "sleepstress_micerfimp.RData")
## }
## load("sleepstress_micerfimp.RData")
## dimputed <- lapply(micedimp, function(d) mice::complete(d, 1))


if (FALSE) {
  cl <- makeCluster(2)
  registerDoParallel(cl)
  clusterEvalQ(cl, library(caret))

  sdatimporig <- sdatimp <- as.data.frame(sdat[, unique(c(stress.vars, "OtherAbuse", sleep.vars,
            "Sex", "B1PAGE_M2", "white", "A1SCHRON", "A1PDEPDX", "A1PANXTD",
            "A1PA54", "A1PSmoke", "B1PDEPDX", "B1PANXTD",
            "B1SCHRON", "A1SBMI", "B1SBMI",
            "Smoke", "CurrentSmoke", "PSQI1", "ExtremeDiet",
            "CurrentAlcohol", "WorstAlcohol",
            "m1chadv", "m2aadv", "LifeStress",
            "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
            "B4QPS_PS", "CTQTotal", "B4QCT_PA"
            )), with = FALSE])
  facs <- which(unlist(lapply(sdatimp, class)) %in% c("ordered", "factor"))
  sdatimp[, facs][] <- lapply(sdatimp[, facs], as.integer)

  imputation_seeds <- c(403L, 624L, -1394370482L, -1723143049L, 2071488076L, 1659356893L,
                        -1081051142L, 885114163L, -614367016L, 561456377L, -29212570L,
                        951631791L, -1923325404L, 1176136405L, -446079598L, 1823627243L,
                        1899124272L, -1981844367L, 894106494L, 2001652199L)


  clusterExport(cl, "sdatimp")
  clusterExport(cl, "imputation_seeds")

  dimputed <- parLapply(cl, 1:20, function(i) {
    set.seed(imputation_seeds[i])
    traind <- preProcess(sdatimp, method = 'bagImpute')
    predict(traind, sdatimp)
  })
  save(dimputed, file = "sleepstress_bagimp.RData")
}
load("sleepstress_bagimp.RData")

dimputed <- lapply(dimputed, function(d) {
  d$SOL <- exp(d$SOL)
  d$WASO <- d$WASO^2
  return(d)
})

dimputedl <- as.data.table(do.call(rbind, lapply(1:20, function(i) cbind(dimputed[[i]], Imp = i, M2ID = sdat$M2ID))))

egltable(vars = c("Sex", "B1PAGE_M2", "white", "m1chadv", "A1SCHRON", "A1PDEPDX", "A1PANXTD",
                  stress.vars, sleep.vars),
         data = within(as.data.frame(dimputedl), {
           A1PDEPDX <- factor(round(A1PDEPDX))
           A1PANXTD <- factor(round(A1PANXTD))
         }))

dimputed <- lapply(dimputed, function(d) {
  d$A1SLFEDI <- pmin(d$A1SLFEDI, 3)
  return(d)
})


round(cor(sdat[, sleep.vars, with = FALSE], use = "pairwise.complete.obs"), 2)
plot(sdat[, sleep.vars, with = FALSE])


sr <- cor(sdat[, stress.labs$levels, with = FALSE], use = "pairwise.complete.obs")
sr2 <- as.dist(sr)
attr(sr2, "Labels") <- stress.labs$labels

rownames(sr) <- colnames(sr) <- stress.labs$labels

sc <- hclust(sr2)
plot(sc)
plot(prcomp(sr, scale = TRUE))

corplot(sr[sc$order, sc$order], plot = "cor", order = "asis") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))


sdatl <- melt(dimputedl[, c("M2ID", "Imp", stress.vars), with = FALSE], id.vars = c("M2ID", "Imp"))

sdatl[, variable := factor(variable,
                           levels = stress.labs$levels,
                           labels = stress.labs$labels)]


p.dist <- ggplot(sdatl[, .(zscore = as.numeric(scale(value))), by = .(Imp, variable)], aes(zscore)) +
  geom_density(adjust = 2) +
  facet_wrap(~variable, scales = "free", nrow = 3, dir="v") +
  xlab("Z Score") + ylab("") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

save_plot(
  "stress_distributions.emf",
  p.dist,
  base_height = 6, base_aspect_ratio = 1.4)


m.linear <- lapply(sleep.vars, function(dv) {
  lapply(stress.vars, function(iv) {
    f1 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(round(A1PDEPDX) == 1 | round(A1PANXTD) == 1) + scale(%s)",
                 dv, iv)
    f2 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(round(A1PDEPDX) == 1 | round(A1PANXTD) == 1)",
                 dv)

    ## usedat <- na.omit(model.frame(as.formula(f), data = sdat))
    m <- lapply(dimputed, function(usedat) {
      m1 <- lm(as.formula(f1), data = usedat)
      m2 <- lm(as.formula(f2), data = usedat)
      list(m1 = m1, m2 = m2)
    })

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    r2 <- data.table(
      DV = dv, IV = iv,
      CovR2 = pool.r.squared(m2imp)[1, "est"],
      AllR2 = pool.r.squared(m1imp)[1, "est"],
      StrR2 = pool.r.squared(m1imp)[1, "est"] - pool.r.squared(m2imp)[1, "est"])

    list(B = cbind.data.frame(DV = dv, IV = iv, as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
})


m.linear.coef <- as.data.table(do.call(rbind, lapply(m.linear, function(d) do.call(rbind, lapply(d, function(x) x$B)))))

setnames(m.linear.coef, old = names(m.linear.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m.linear.r2 <- as.data.table(do.call(rbind, lapply(m.linear, function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

m.linear.coef <- cbind(m.linear.coef, m.linear.r2[, .(StrR2)])

m.linear.coef[order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3))]

m.linear.r2[order(DV, -StrR2), .(DV, IV, UniqueR2 = round(StrR2 * 100, 1))]

(r2order <- m.linear.r2[, .(M = sum(StrR2)), by = IV][order(-M)])

m.linear.coef[, DV := factor(DV,
                             levels = c("BT2", "RT2", "TST", "SOL", "WASO", "SE"),
                             labels = c("Bedtime", "Rise Time", "Total Sleep Time",
                                        "Sleep Onset Latency", "Wake After Sleep Onset",
                                        "Sleep Efficiency"))]


m.linear.coef[, IV := factor(IV,
                             levels = r2order$IV,
                             labels = stress.labs[match(r2order$IV, stress.labs$levels)]$labels)]
m.linear.coef[, IV := factor(IV, levels = rev(levels(IV)))]


p <- ggplot(m.linear.coef, aes(DV, IV, fill = StrR2*100)) +
  geom_tile() +
  scale_fill_gradient(name = expression(R^2), low = "#ffffff", high = "#0079AA", limits = c(0, 4)) +
  geom_text(aes(label = gsub("0\\.", ".", sprintf("%0.2f%s", B,
    symnum(P, legend = FALSE, na = "", cutpoints = c(0, 0.001,
      0.01, 0.05, .10, 1), symbols = c("***", "**", "*", "^", " ")))),
    colour = ifelse(StrR2*100 < 2, "black", "black")),
    size = 4) +
  scale_colour_manual("", values = c("black" = "black", "white" = "white"), guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("") + ylab("")
ggdraw(switch_axis_position(p, axis = 'x'))

save_plot(
  "relations.emf",
  switch_axis_position(p, axis = 'x'),
  base_height = 5, base_aspect_ratio = 1.2)


## m.linear.coef.wide <- dcast(m.linear.coef[order(DV, P), .(DV, IV, B)], IV ~ DV)
## m.linear.r2.wide <- dcast(m.linear.r2[order(DV, -StrR2), .(DV, IV, StrR2)], IV ~ DV)
## round(m.linear.r2.wide[,-1]*100, 1)
## temp <- m.linear.coef.wide[, -1]
## rownames(temp) <- m.linear.coef.wide[,1]
## par(mfrow = c(2,1))
## plot(hclust(dist(temp)))
## plot(hclust(dist(abs(temp))))

m.linear.r2[order(DV, -StrR2)]



m.linear.all <- lapply(sleep.vars, function(dv) {
    f1 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(round(A1PDEPDX) == 1 | round(A1PANXTD) == 1) + %s",
                 dv, paste(paste0("scale(", stress.vars, ")"), collapse = " + "))
    f2 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(round(A1PDEPDX) == 1 | round(A1PANXTD) == 1)",
                 dv)

    ## usedat <- na.omit(model.frame(as.formula(f), data = sdat))
    m <- lapply(dimputed, function(usedat) {
      m1 <- lm(as.formula(f1), data = usedat)
      m2 <- lm(as.formula(f2), data = usedat)
      list(m1 = m1, m2 = m2)
    })

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    r2 <- data.table(
      DV = dv,
      CovR2 = pool.r.squared(m2imp)[1, "est"],
      AllR2 = pool.r.squared(m1imp)[1, "est"],
      StrR2 = pool.r.squared(m1imp)[1, "est"] - pool.r.squared(m2imp)[1, "est"])

    list(B = cbind.data.frame(DV = dv, IV = stress.vars, as.data.frame(sm1[8:nrow(sm1), , drop = FALSE])),
         R2 = r2)
})

m.linear.all[[1]]$B

m.linear.all.coef <- as.data.table(do.call(rbind, lapply(m.linear.all, function(x) x$B))[, c("DV", "IV", "est", "Pr(>|t|)")])

setnames(m.linear.all.coef, names(m.linear.all.coef), c("DV", "IV", "B", "P"))
m.linear.all.coef[order(DV, P)]

m.linear.all.coef[P<.05][order(IV, P)]

m.linear.all.coef[IV=="A1SDAYDI"][order(IV, P)]

m.linear.all.coef.wide <- dcast(m.linear.all.coef[order(DV, P), .(DV, IV, B)], IV ~ DV)

temp <- m.linear.all.coef.wide[, -1]
rownames(temp) <- m.linear.all.coef.wide[,1]
sort(rowMeans(abs(temp)), TRUE)




#########################################
m.linear.age <- lapply(sleep.vars, function(dv) {
  lapply(stress.vars, function(iv) {
    f1 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(B1PAGE_M2 <= 53) * scale(%s)",
                 dv, iv)
    f2 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(B1PAGE_M2 <= 53) + scale(%s)",
                 dv, iv)

    ## usedat <- na.omit(model.frame(as.formula(f), data = sdat))
    m <- lapply(dimputed, function(usedat) {
      m1 <- lm(as.formula(f1), data = usedat)
      m2 <- lm(as.formula(f2), data = usedat)
      list(m1 = m1, m2 = m2)
    })

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    r2 <- data.table(
      DV = dv, IV = iv,
      CovR2 = pool.r.squared(m2imp)[1, "est"],
      AllR2 = pool.r.squared(m1imp)[1, "est"],
      StrR2 = pool.r.squared(m1imp)[1, "est"] - pool.r.squared(m2imp)[1, "est"])

    list(B = cbind.data.frame(DV = dv, IV = iv, as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
})


m.linear.age.coef <- as.data.table(do.call(rbind, lapply(m.linear.age, function(d) do.call(rbind, lapply(d, function(x) x$B)))))

setnames(m.linear.age.coef, old = names(m.linear.age.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m.linear.age.r2 <- as.data.table(do.call(rbind, lapply(m.linear.age, function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

m.linear.age.coef <- cbind(m.linear.age.coef, m.linear.age.r2[, .(StrR2)])

m.linear.age.coef[order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3))]


#########################################
m.linear.ses <- lapply(sleep.vars, function(dv) {
  lapply(stress.vars, function(iv) {
    f1 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) * scale(%s)",
                 dv, iv)
    f2 <- sprintf("scale(%s) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) + scale(%s)",
                 dv, iv)

    ## usedat <- na.omit(model.frame(as.formula(f), data = sdat))
    m <- lapply(dimputed, function(usedat) {
      m1 <- lm(as.formula(f1), data = usedat)
      m2 <- lm(as.formula(f2), data = usedat)
      list(m1 = m1, m2 = m2)
    })

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    r2 <- data.table(
      DV = dv, IV = iv,
      CovR2 = pool.r.squared(m2imp)[1, "est"],
      AllR2 = pool.r.squared(m1imp)[1, "est"],
      StrR2 = pool.r.squared(m1imp)[1, "est"] - pool.r.squared(m2imp)[1, "est"])

    list(B = cbind.data.frame(DV = dv, IV = iv, as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
})


m.linear.ses.coef <- as.data.table(do.call(rbind, lapply(m.linear.ses, function(d) do.call(rbind, lapply(d, function(x) x$B)))))

setnames(m.linear.ses.coef, old = names(m.linear.ses.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m.linear.ses.r2 <- as.data.table(do.call(rbind, lapply(m.linear.ses, function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

m.linear.ses.coef <- cbind(m.linear.ses.coef, m.linear.ses.r2[, .(StrR2)])

m.linear.ses.coef[order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3), StrR2)]

m.linear.ses.coef[P<=.05][order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3), R2 = round(StrR2*100, 1))]


mtest <- lapply(dimputed, function(usedat) {
  lm(scale(BT2) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) + I(m1chadv < 4):scale(A1SDAYDI), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))

mtest <- lapply(dimputed, function(usedat) {
  lm(scale(RT2) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) + I(m1chadv < 4):scale(A1SDAYDI), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))


mtest <- lapply(dimputed, function(usedat) {
  lm(scale(SOL) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) + I(m1chadv < 4):scale(A1SPIHOM), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))

mtest <- lapply(dimputed, function(usedat) {
  lm(scale(WASO) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + I(m1chadv < 4) + I(m1chadv < 4):scale(A1SPIWOR), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))


    m1imp <- as.mira(mtest)
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))


#########################################
m.linear.sex <- lapply(sleep.vars, function(dv) {
  lapply(stress.vars, function(iv) {
    f1 <- sprintf("scale(%s) ~ B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + factor(Sex) * scale(%s)",
                 dv, iv)
    f2 <- sprintf("scale(%s) ~ factor(Sex) + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + scale(%s)",
                 dv, iv)

    ## usedat <- na.omit(model.frame(as.formula(f), data = sdat))
    m <- lapply(dimputed, function(usedat) {
      m1 <- lm(as.formula(f1), data = usedat)
      m2 <- lm(as.formula(f2), data = usedat)
      list(m1 = m1, m2 = m2)
    })

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    r2 <- data.table(
      DV = dv, IV = iv,
      CovR2 = pool.r.squared(m2imp)[1, "est"],
      AllR2 = pool.r.squared(m1imp)[1, "est"],
      StrR2 = pool.r.squared(m1imp)[1, "est"] - pool.r.squared(m2imp)[1, "est"])

    list(B = cbind.data.frame(DV = dv, IV = iv, as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
})

m.linear.sex.coef <- as.data.table(do.call(rbind, lapply(m.linear.sex, function(d) do.call(rbind, lapply(d, function(x) x$B)))))

setnames(m.linear.sex.coef, old = names(m.linear.sex.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m.linear.sex.r2 <- as.data.table(do.call(rbind, lapply(m.linear.sex, function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

m.linear.sex.coef <- cbind(m.linear.sex.coef, m.linear.sex.r2[, .(StrR2)])

m.linear.sex.coef[P<=.05][order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3), R2 = round(StrR2*100, 1))]


mtest <- lapply(dimputed, function(usedat) {
  lm(scale(WASO) ~ factor(Sex) + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + factor(Sex):scale(A1SDAYDI), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))


mtest <- lapply(dimputed, function(usedat) {
  lm(scale(TST) ~ factor(Sex) + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + factor(Sex):scale(A1SHOMET), data = usedat)
})
summary(mice::pool(as.mira(mtest)))
qqnorm(unlist(lapply(mtest, residuals)))




library(quantreg)
sdat[, PDx := round(A1PDEPDX) == 1 | round(A1PANXTD) == 1]

m1 <- rq((TST) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(ChildLifeStress), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)


m1 <- rq((SOL) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)

m1 <- rq((TST) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)

m1 <- rq((BT2) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)

m1 <- rq((RT2) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)

m1 <- rq((SE) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)

m1 <- rq((WASO) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + PDx +
           scale(A1SDAYDI), tau = c(.2, .5, .8), data = sdat)
summary(m1)
plot(m1)


m2 <- rq((SE) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + scale(A1SDAYDI), tau = c(.25, .5, .75), data = sdat)
summary(m2)

m3 <- rq((TST) ~ Sex + B1PAGE_M2 + white + m1chadv + A1SCHRON + I(A1PDEPDX == 1 | A1PANXTD == 1) + scale(A1SDAYDI), tau = c(.25, .5, .75), data = sdat)
summary(m3)
plot(m3)


summary(lm(scale(TST) ~ Sex + B1PAGE_M2 + RaceG2 + scale(winsorizor(A1SDAYDI, .005)), data = sdat))

m.sol <- do.call(rbind, lapply(stress.vars, function(v) {
  f <- paste0("scale(SOL) ~ dDay + Sex + B1PAGE_M2 + scale(",
              v, ") + (1 | M2ID)")

  coef(summary(lmer(as.formula(f), data = psrsleep.data)))[10, ]
}))


library(lme4)
library(lmerTest)
library(mice)



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











preloaded <- FALSE
source("projectwide_setup.R")
options(width = 80)

if (!preloaded) {
d.all <- within(d.all, {
  A1SBMI <- ifelse(A1SBMI > 90, NA, A1SBMI)
  B1SBMI <- ifelse(B1SBMI > 90, NA, B1SBMI)
  A1SWSTHI <- ifelse(A1SWSTHI > 4, NA, A1SWSTHI)
  B1SWSTHI <- ifelse(B1SWSTHI > 4, NA, B1SWSTHI)
  RaceG3 <- ifelse(is.na(A1SS7) & is.na(B1PF7A), "Missing", ifelse(is.na(A1SS7), B1PF7A, A1SS7))
  RaceG3 <- ifelse(RaceG3 == 1, "White",
            ifelse(RaceG3 == 2, "AA",
            ifelse(RaceG3 %in% c(3, 4, 5, 6), "Other", "Missing")))
  RaceG3 <- factor(ifelse(RaceG3 == "Missing", NA, RaceG3),
            levels = c("White", "AA", "Other"))
})

## merge in PSRQ-SF from aim 2
d.all <- merge(d.all, cbind(M2ID = m2a.items$M2ID, m2a.items.psrqsf), by = "M2ID", all.x = TRUE)
save.image("midus_stressnetwork.RData")
}


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
