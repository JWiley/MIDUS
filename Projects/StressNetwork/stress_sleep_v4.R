source("~/OneDrive/Projects/MIDUS/Setup/setup_packages_functions.R")
options(stringsAsFactors = FALSE, digits = 3)


## midus.iiv <- environment()
## load("~/Dropbox/02_Projects/Variability_MIDUS/MIDUS R/MIDUS_sleep_variability.RData", envir = midus.iiv)
## d.iiv <- with(midus.iiv, list(
##   TST = list(d = d.tst, m = m1.results),
##   BT = list(d = d.bt2, m = m5.results),
##   RT = list(d = d.rt2, m = m4.results),
##   SOL = list(d = d.sol, m = m2.results),
##   SE = list(d = d.se, m = m3.results),
##   WASO = list(d = d.waso, m = m6.results)))
## saveRDS(d.iiv, file = "midus_sleep_iiv.RDS", compress = "xz")


r2.gls <- function(object) {
  cor(fitted(object), fitted(object) + residuals(object))^2
}

pool.r2.gls <- function(obj, index) {
  if (length(index) == 1L) {
    tanh(mean(atanh(sapply(obj, function(x) {
      r2.gls(x[[index]])
    }))))
  } else if (length(index) == 2L) {
    tanh(mean(atanh(sapply(obj, function(x) {
      r2.gls(x[[index[[1]]]])
    })) - atanh(sapply(obj, function(x) {
      r2.gls(x[[index[[2]]]])
    }))))
  }
}


dw <- readRDS("~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dwclean.RDS")
dwl <- readRDS("~/OneDrive/Projects/MIDUS/Data/Processed/midus_merged_data_dwl.RDS")

d.iiv <- readRDS("midus_sleep_iiv.RDS")

snimp <- readRDS("~/OneDrive/Projects/MIDUS/Setup/stressnetwork_imputation/midus_stress_final_imputed.RDS")

dimputedl <- as.data.table(do.call(rbind, lapply(1:50, function(i) {
  tmp.iiv <- lapply(names(d.iiv), function(v) {
    useIDs <- unique(d.iiv[[v]]$d$M2ID)
    tmpd <- data.table(
      M2ID = useIDs,
      U_Sleep = d.iiv[[v]]$m$U[i, ],
      V_Sleep = d.iiv[[v]]$m$Sigma_V[i, ])
    setnames(tmpd, c("M2ID", paste0(c("U_", "V_"), v)))
    return(tmpd)
  })

  tmp.iiv2 <- tmp.iiv[[1]]
  for (k in 2:length(tmp.iiv)) {
    tmp.iiv2 <- merge(tmp.iiv2, tmp.iiv[[k]], by = "M2ID", all=TRUE)
  }

  merge(cbind(as.data.table(snimp[[i]])[M2ID %in% d.iiv$TST$d$M2ID], Imp = i),
        tmp.iiv2, by = "M2ID", all = TRUE)
})))

stress.vars <- c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
                 "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SHOMET", "A1SPIHOM",
                 "A1ParentAbuse", "A1SiblingAbuse", "B1ChildLifeStress")
stress.vars2 <- c("B4CTQTotal", "B1LifeStress",
    "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM",
    "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
    "B1SLFEDI", "B1SDAYDI", "B4QPS_PS")

for (v in c(stress.vars, stress.vars2)) {
  dimputedl[, (v) := as.numeric(winsorizor(as.vector(get(v)), percentile = .01))]
}

dimputedl[, A1DepAnx := as.integer(A1PDEPDX == 1 | A1PANXTD == 1)]
dimputedl[, B1DepAnx := as.integer(B1PDEPDX == 1 | B1PANXTD == 1)]

dimputedl[, A1SHOMET := A1SHOMET * -1]
dimputedl[, B1SHOMET := B1SHOMET * -1]

dimputedl[, A1CumStress := Reduce(`+`, lapply(.SD, function(x) as.integer(x > quantile(x, probs = .75)))),
          .SDcols = stress.vars, by = Imp]
dimputedl[, B1CumStress := Reduce(`+`, lapply(.SD, function(x) as.integer(x > quantile(x, probs = .75)))),
          .SDcols = stress.vars2, by = Imp]

dimputed <- lapply(1:50, function(i) {
  dimputedl[Imp == i]
})


stress.vars <- c(stress.vars, "A1CumStress")

stress.labs <- data.table(
  levels = c("A1SDAYDI", "A1SLFEDI", "B1ChildLifeStress",
             "A1SKINNE", "A1SFDSNE", "A1SSPCRI",
             "A1SPIFAM", "A1SPIWOR", "A1SPIHOM",
             "A1SHOMET", "A1ParentAbuse", "A1SiblingAbuse", "A1CumStress"),
  labels = c("Daily Discrimination", "Lifetime Discrimination", "Childhood SLEs",
             "Family Strain", "Friend Strain", "Partner Strain",
             "Family Inequality", "Work Inequality", "Home Inequality",
             "Neighborhood Quality", "Parental Abuse", "Sibling Abuse", "Cumulative Stress"
             ))


stress.vars2 <- c(stress.vars2, "B1CumStress")

stress.labs2 <- data.table(
  levels = c("B4CTQTotal", "B1LifeStress",
             "B1SJOBDI", "B1SDAYDI", "B1SLFEDI",
             "B1SKINNE", "B1SFDSNE", "B1SSPCRI",
             "B1SPIFAM", "B1SPIWOR", "B1SPIHOM",
             "B1SHOMET", "B4QPS_PS", "B1CumStress"),
  labels = c("Childhood Trauma", "Stressful Life Events",
             "Job Discrimination", "Daily Discrimination", "Lifetime Discrimination",
             "Family Strain", "Friend Strain", "Partner Strain",
             "Family Inequality", "Work Inequality", "Home Inequality",
             "Neighborhood Quality", "Perceived Stress", "Cumulative Stress"
             ))


cl <- makeCluster(10)

clusterExport(cl, c("stress.vars", "stress.vars2", "dimputed", "pool.r2.gls", "r2.gls"))
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-10-15", R.version = "3.4.2")

  library(mice)
  library(nlme)
  library(data.table)
})

m.linear <- lapply(c("1",
                     "1 + Sex + AGEM2 + I(RaceG3=='White')",
                     "1 + Sex + AGEM2 + I(RaceG3=='White') + A1SCHRON + A1DepAnx"),
                   function(covs) {
  lapply(c("U_TST", "U_BT", "U_RT", "U_SOL", "U_SE", "U_WASO",
           "V_TST", "V_BT", "V_RT", "V_SOL", "V_SE", "V_WASO"), function(dv) {
  lapply(c(as.list(stress.vars), list(stress.vars[-length(stress.vars)])), function(iv) {
    f1 <- sprintf("scale(%s) ~ %s + %s", dv, covs, paste(sapply(iv, function(tmp) sprintf("scale(%s)", tmp)), collapse = " + "))
    f2 <- sprintf("scale(%s) ~ %s", dv, covs)
    m <- parLapplyLB(cl, dimputed, function(usedat) {
      m1 <- tryCatch(gls(as.formula(f1), data = usedat,
                         correlation = corCompSymm(form = ~ 1 | M2FAMNUM), na.action = na.omit),
                     error = function(e) e)
      if (inherits(m1, "error")) {
        m1 <- NULL
      }
      m2 <- tryCatch(gls(as.formula(f2), data = usedat,
                         correlation = corCompSymm(form = ~ 1 | M2FAMNUM), na.action = na.omit),
                     error = function(e) e)
      if (inherits(m2, "error")) {
        m2 <- NULL
      }
      list(m1 = m1, m2 = m2)
    })

    ok <- sapply(m, function(x) !is.null(x[[1]]) && !is.null(x[[2]]))
    m <- m[ok]

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    if (length(iv) > 1) {
      iv <- "All"
    }

    if (nchar(covs) < 3) {
      r2 <- data.table(DV = dv, IV = iv,
                       CovR2 = pool.r2.gls(m, 2), AllR2 = pool.r2.gls(m, 1),
                       StrR2 = pool.r2.gls(m, c(1)))
    } else {
      r2 <- data.table(DV = dv, IV = iv,
                       CovR2 = pool.r2.gls(m, 2), AllR2 = pool.r2.gls(m, 1),
                       StrR2 = pool.r2.gls(m, c(1, 2)))
    }

    list(B = cbind.data.frame(
           DV = dv, IV = iv,
           as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
  })
})

saveRDS(m.linear, file = "stress_sleep_linear_results_imp_all.RDS", compress = "xz")


m.linear.coef <- as.data.table(do.call(rbind, lapply(m.linear[[3]], function(d) do.call(rbind, lapply(d, function(x) x$B)))))
m.linear.r2 <- as.data.table(do.call(rbind, lapply(m.linear[[3]], function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

setnames(m.linear.coef, old = names(m.linear.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m.linear.coef <- cbind(m.linear.coef, m.linear.r2[, .(StrR2)])

m.linear.coef[order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3))][P<.05]

m.linear.r2[order(DV, -StrR2), .(DV, IV, UniqueR2 = round(StrR2 * 100, 1))]

m.linear.r2.iiv <- m.linear.r2[grepl("^V_.*$", DV)]
m.linear.r2.mean <- m.linear.r2[grepl("^U_.*$", DV)]

(r2order <- m.linear.r2[, .(M = sum(StrR2)), by = IV][order(-M)])
(r2order.iiv <- m.linear.r2.iiv[, .(M = sum(StrR2)), by = IV][order(-M)])
(r2order.mean <- m.linear.r2.mean[, .(M = sum(StrR2)), by = IV][order(-M)])


## m.linear.coef[, DV := factor(DV,
##                              levels = c("U_BT", "U_RT", "U_TST", "U_SOL", "U_WASO", "U_SE",
##                                         "V_BT", "V_RT", "V_TST", "V_SOL", "V_WASO", "V_SE"),
##                              labels = c("Bedtime", "Rise Time", "Total Sleep Time",
##                                         "Sleep Onset Latency", "Wake After Sleep Onset", "Sleep Efficiency",
##                                         "Bedtime IIV", "Rise Time IIV", "Total Sleep Time IIV",
##                                         "Sleep Onset Latency IIV", "Wake After Sleep Onset IIV", "Sleep Efficiency IIV"
##                                         ))]

m.linear.coef[, DV := factor(DV,
                             levels = c("U_BT", "U_RT", "U_TST", "U_SOL", "U_WASO", "U_SE",
                                        "V_BT", "V_RT", "V_TST", "V_SOL", "V_WASO", "V_SE"),
                             labels = c("BT", "RT", "TST",
                                        "SOL", "WASO", "SE",
                                        "BT IIV", "RT IIV", "TST IIV",
                                        "SOL IIV", "WASO IIV", "SE IIV"
                                        ))]

m.linear.coef.iiv <- droplevels(m.linear.coef[grepl("IIV", DV)])
m.linear.coef.mean <- droplevels(m.linear.coef[!grepl("IIV", DV)])

m.linear.coef.iiv[, IV := factor(IV,
                                 levels = r2order.iiv$IV,
                                 labels = stress.labs[match(r2order.iiv$IV, stress.labs$levels)]$labels)]
m.linear.coef.iiv[, IV := factor(IV, levels = rev(levels(IV)))]

m.linear.coef.mean[, IV := factor(IV,
                                 levels = r2order.mean$IV,
                                 labels = stress.labs[match(r2order.mean$IV, stress.labs$levels)]$labels)]
m.linear.coef.mean[, IV := factor(IV, levels = rev(levels(IV)))]

0079AA
p.mean <- ggplot(m.linear.coef.mean, aes(DV, IV, fill = StrR2*100)) +
  geom_tile() +
  scale_fill_gradient(name = expression(R^2), low = "#ffffff", high = "#000000", limits = c(0, 5)) +
  geom_text(aes(label = gsub("0\\.", ".", sprintf("%0.2f%s", B,
    symnum(P, legend = FALSE, na = "", cutpoints = c(0, 0.001,
      0.01, 0.05, .10, 1), symbols = c("***", "**", "*", "^", " ")))),
    colour = ifelse(StrR2*100 < 2, "black", "black")),
    size = 4) +
    scale_colour_manual("", values = c("black" = "black", "white" = "white"), guide = FALSE) +
    scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0)) +
    xlab("") + ylab("")

p.iiv <- ggplot(m.linear.coef.iiv, aes(DV, IV, fill = StrR2*100)) +
  geom_tile() +
  scale_fill_gradient(name = expression(R^2), low = "#ffffff", high = "#0079AA", limits = c(0, 4)) +
  geom_text(aes(label = gsub("0\\.", ".", sprintf("%0.2f%s", B,
    symnum(P, legend = FALSE, na = "", cutpoints = c(0, 0.001,
      0.01, 0.05, .10, 1), symbols = c("***", "**", "*", "^", " ")))),
    colour = ifelse(StrR2*100 < 2, "black", "black")),
    size = 4) +
    scale_colour_manual("", values = c("black" = "black", "white" = "white"), guide = FALSE) +
    scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0)) +
    xlab("") + ylab("")

save_plot(
  "relations_mean.emf", p.mean,
  base_height = 5, base_aspect_ratio = 1.2)
save_plot(
  "relations_iiv.emf", p.iiv,
  base_height = 5, base_aspect_ratio = 1.2)






















m2.linear <- lapply(c("1",
                     "1 + Sex + AGEM2 + I(RaceG3=='White')",
                     "1 + Sex + AGEM2 + I(RaceG3=='White') + A1SCHRON + A1DepAnx"),
                   function(covs) {
  lapply(c("U_TST", "U_BT", "U_RT", "U_SOL", "U_SE", "U_WASO",
           "V_TST", "V_BT", "V_RT", "V_SOL", "V_SE", "V_WASO"), function(dv) {
  lapply(stress.vars2, function(iv) {
    f1 <- sprintf("scale(%s) ~ %s + %s", dv, covs, paste(sapply(iv, function(tmp) sprintf("scale(%s)", tmp)), collapse = " + "))
    f2 <- sprintf("scale(%s) ~ %s", dv, covs)
    m <- parLapplyLB(cl, dimputed, function(usedat) {
      m1 <- tryCatch(gls(as.formula(f1), data = usedat,
                         correlation = corCompSymm(form = ~ 1 | M2FAMNUM), na.action = na.omit),
                     error = function(e) e)
      if (inherits(m1, "error")) {
        m1 <- NULL
      }
      m2 <- tryCatch(gls(as.formula(f2), data = usedat,
                         correlation = corCompSymm(form = ~ 1 | M2FAMNUM), na.action = na.omit),
                     error = function(e) e)
      if (inherits(m2, "error")) {
        m2 <- NULL
      }
      list(m1 = m1, m2 = m2)
    })

    ok <- sapply(m, function(x) !is.null(x[[1]]) && !is.null(x[[2]]))
    m <- m[ok]

    m1imp <- as.mira(lapply(m, function(x) x$m1))
    m2imp <- as.mira(lapply(m, function(x) x$m2))

    sm1 <- summary(mice::pool(m1imp))
    sm2 <- summary(mice::pool(m2imp))

    if (nchar(covs) < 3) {
      r2 <- data.table(DV = dv, IV = iv,
                       CovR2 = pool.r2.gls(m, 2), AllR2 = pool.r2.gls(m, 1),
                       StrR2 = pool.r2.gls(m, c(1)))
    } else {
      r2 <- data.table(DV = dv, IV = iv,
                       CovR2 = pool.r2.gls(m, 2), AllR2 = pool.r2.gls(m, 1),
                       StrR2 = pool.r2.gls(m, c(1, 2)))
    }

    list(B = cbind.data.frame(
           DV = dv, IV = iv,
           as.data.frame(sm1[nrow(sm1), , drop = FALSE])),
         R2 = r2)
  })
  })
})

saveRDS(m2.linear, file = "stress2_sleep_linear_results_imp_all.RDS", compress = "xz")


m2.linear.coef <- as.data.table(do.call(rbind, lapply(m2.linear[[2]], function(d) do.call(rbind, lapply(d, function(x) x$B)))))
m2.linear.r2 <- as.data.table(do.call(rbind, lapply(m2.linear[[2]], function(d) do.call(rbind, lapply(d, function(x) x$R2)))))

setnames(m2.linear.coef, old = names(m2.linear.coef), c("DV", "IV", "B", "SE", "T", "DF", "P", "LL", "UL", "NMIS", "FMI", "Lambda"))

m2.linear.coef <- cbind(m2.linear.coef, m2.linear.r2[, .(StrR2)])

m2.linear.coef[order(DV, -abs(B)), .(DV, IV, B, P = round(P, 3))][P<.05]

m2.linear.r2[order(DV, -StrR2), .(DV, IV, UniqueR2 = round(StrR2 * 100, 1))]

m2.linear.r2.iiv <- m2.linear.r2[grepl("^V_.*$", DV)]
m2.linear.r2.mean <- m2.linear.r2[grepl("^U_.*$", DV)]

(r2order2 <- m2.linear.r2[, .(M = sum(StrR2)), by = IV][order(-M)])
(r2order2.iiv <- m2.linear.r2.iiv[, .(M = sum(StrR2)), by = IV][order(-M)])
(r2order2.mean <- m2.linear.r2.mean[, .(M = sum(StrR2)), by = IV][order(-M)])


## m.linear.coef[, DV := factor(DV,
##                              levels = c("U_BT", "U_RT", "U_TST", "U_SOL", "U_WASO", "U_SE",
##                                         "V_BT", "V_RT", "V_TST", "V_SOL", "V_WASO", "V_SE"),
##                              labels = c("Bedtime", "Rise Time", "Total Sleep Time",
##                                         "Sleep Onset Latency", "Wake After Sleep Onset", "Sleep Efficiency",
##                                         "Bedtime IIV", "Rise Time IIV", "Total Sleep Time IIV",
##                                         "Sleep Onset Latency IIV", "Wake After Sleep Onset IIV", "Sleep Efficiency IIV"
##                                         ))]

m2.linear.coef[, DV := factor(DV,
                             levels = c("U_BT", "U_RT", "U_TST", "U_SOL", "U_WASO", "U_SE",
                                        "V_BT", "V_RT", "V_TST", "V_SOL", "V_WASO", "V_SE"),
                             labels = c("BT", "RT", "TST",
                                        "SOL", "WASO", "SE",
                                        "BT IIV", "RT IIV", "TST IIV",
                                        "SOL IIV", "WASO IIV", "SE IIV"
                                        ))]

m2.linear.coef.iiv <- droplevels(m2.linear.coef[grepl("IIV", DV)])
m2.linear.coef.mean <- droplevels(m2.linear.coef[!grepl("IIV", DV)])

m2.linear.coef.iiv[, IV := factor(IV,
                                 levels = r2order2.iiv$IV,
                                 labels = stress.labs2[match(r2order2.iiv$IV, stress.labs2$levels)]$labels)]
m2.linear.coef.iiv[, IV := factor(IV, levels = rev(levels(IV)))]

m2.linear.coef.mean[, IV := factor(IV,
                                 levels = r2order2.mean$IV,
                                 labels = stress.labs2[match(r2order2.mean$IV, stress.labs2$levels)]$labels)]
m2.linear.coef.mean[, IV := factor(IV, levels = rev(levels(IV)))]


p2.mean <- ggplot(m2.linear.coef.mean, aes(DV, IV, fill = StrR2*100)) +
  geom_tile() +
  scale_fill_gradient(name = expression(R^2), low = "#ffffff", high = "#000000", limits = c(0, 4)) +
  geom_text(aes(label = gsub("0\\.", ".", sprintf("%0.2f%s", B,
    symnum(P, legend = FALSE, na = "", cutpoints = c(0, 0.001,
      0.01, 0.05, .10, 1), symbols = c("***", "**", "*", "^", " ")))),
    colour = ifelse(StrR2*100 < 2, "black", "white")),
    size = 4) +
    scale_colour_manual("", values = c("black" = "black", "white" = "white"), guide = FALSE) +
    scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0)) +
    xlab("") + ylab("")

p2.iiv <- ggplot(m2.linear.coef.iiv, aes(DV, IV, fill = StrR2*100)) +
  geom_tile() +
  scale_fill_gradient(name = expression(R^2), low = "#ffffff", high = "#000000", limits = c(0, 4)) +
  geom_text(aes(label = gsub("0\\.", ".", sprintf("%0.2f%s", B,
    symnum(P, legend = FALSE, na = "", cutpoints = c(0, 0.001,
      0.01, 0.05, .10, 1), symbols = c("***", "**", "*", "^", " ")))),
    colour = ifelse(StrR2*100 < 2, "black", "white")),
    size = 4) +
    scale_colour_manual("", values = c("black" = "black", "white" = "white"), guide = FALSE) +
    scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0)) +
    xlab("") + ylab("")

save_plot(
  "relations_mean.emf", p.mean,
  base_height = 5, base_aspect_ratio = 1.2)
save_plot(
  "relations_iiv.emf", p.iiv,
  base_height = 5, base_aspect_ratio = 1.2)










mVars <- read.xlsx("~/OneDrive/Projects/MIDUS/Setup/midus_master_vars.xlsx", sheetIndex = 1)
mVars <- as.data.table(mVars)
setkey(mVars, "Domain")

## load("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data.RData")
## mVars[Domain == "Stress", .(Name, Abbr, M1)][!is.na(M1)]


## dwl2 <- dwl[rowSums(is.na(dwl)) < 6]
## dwl2 <- dwl2[, .(
##   TST1 = mean(TST[time<2], na.rm = TRUE),
##   TST2 = mean(TST[time<3], na.rm = TRUE),
##   TST3 = mean(TST[time<4], na.rm = TRUE),
##   TST4 = mean(TST[time<5], na.rm = TRUE),
##   TST5 = mean(TST[time<6], na.rm = TRUE),
##   TST6 = mean(TST[time<7], na.rm = TRUE),
##   TST7 = mean(TST[time<8], na.rm = TRUE)),
##   by = M2ID]
## plot(SEMSummary(~ ., data = dwl2[, -1, with=FALSE], use = "fiml"), order = "asis")


dwcleanimp <- readRDS("~/OneDrive/Projects/MIDUS/Setup/rcm_imputation/midus_rcm_final_imputed.RDS")




## M2P3Cortl <- readRDS("midus_merged_data_M2P3Cortl.RDS")
## M2P3Cortl[, Smoke := as.integer(Smoke == 1)]
## bi.data <- readRDS("midus_merged_data_psrpanabi_imp.RDS")

## load("midus_merged_data.RData")
med.vars <- c("fg_bpup", "fg_bpdwn", "fg_hrdwn", "fg_hrup",
  "fg_hpaup", "fg_sympdwn", "fg_sympup", "fg_paradwn",
  "fg_paraup", "fg_infldwn", "fg_influp", "fg_glucdwn",
  "fg_glucup", "fg_rxchol")
dw[, ALMedGrp := as.integer(rowSums(dw[, med.vars, with = FALSE], na.rm=TRUE) == 0)]
dw[, CurrentlyWorking := as.integer(B1PBWORK %in% c(1, 2))]
dw[, White := as.integer(RaceG3 == "White")]
## dw[, White := factor(ifelse(m1m2race == 4, 3, m1m2race))]
## dw[, White := factor(m1m2race == 2)]
dw[, Female := as.integer(Sex == 2)]
dw[, BedPartner := as.integer(B4S9 == 4)]
dw[, m2ed_all := factor(m2ed_all)]
dw[, Disorder := as.integer(B1PANXTD == 1 | B1PDEPDX == 1)]

v.cov <- c(
  "CortMeds", "Smoke", "Rb4pwhr", ## cort only
  "ALMedGrp", ## AL only
  "Female", "b1page_m2",
  "CurrentlyWorking",
  "m2ed_all", "White",
  "B1CurrentAlcohol", "B1PhysAct",
  "B1Smoke",
  "B4QPS_PS", "Disorder",
  "BedPartner",
  "p4majorconditions")
## "B1Sleep", "B1SSQ"

dwl2 <- dwl[, .(
  TST = mean(TST/60, na.rm = TRUE),
  SOL = mean(SOL/60, na.rm = TRUE),
  SE = mean(SE, na.rm = TRUE),
  WASO = mean(WASO/60, na.rm = TRUE),
  RT2 = mean(RT2, na.rm = TRUE),
  BT2 = mean(BT2, na.rm = TRUE)), by = M2ID][, Index := Reduce(`+`, lapply(.SD, is.na)), by = M2ID][Index < 6]

sdat <- merge(dw, dwl2, by = "M2ID", all.y = TRUE)




sdat[, SOL := log(SOL)]
sdat[, WASO := sqrt(WASO)]

sdat[, c(stress.vars, sleep.vars, "A1OtherAbuse") := lapply(.SD, function(x) (as.vector(winsorizor(as.numeric(x), .005)))),
     .SDcols = c(stress.vars, sleep.vars, "A1OtherAbuse")]

sdat[, A1DepAnx := as.integer(A1PDEPDX == 1 | A1PANXTD == 1)]
sdat[, B1DepAnx := as.integer(B1PDEPDX == 1 | B1PANXTD == 1)]

sdat[, A1CurrentSmoke := as.integer(A1Smoke == "current")]
sdat[, B1CurrentSmoke := as.integer(B1Smoke == "current")]
sdat[, B1CurrentAlcHigh := as.integer(B1CurrentAlcohol == "High")]
sdat[, B1CurrentAlcMod := as.integer(B1CurrentAlcohol == "Moderate")]
sdat[, B4CurrentAlcHigh := as.integer(B4CurrentAlcohol == "High")]
sdat[, B4CurrentAlcMod := as.integer(B4CurrentAlcohol == "Moderate")]


if (FALSE) {
  cl <- makeCluster(10)
  ## registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(checkpoint)
    checkpoint("2017-10-15", R.version = "3.4.2")
    library(caret)
  })

  sdatimporig <- sdatimp <- as.data.frame(sdat[, unique(c(stress.vars, "A1OtherAbuse", sleep.vars,
            "Sex", "AGEM1", "AGEM2", "AGEM3", "white", "A1SCHRON", "A1DepAnx", "B1DepAnx",
            "A1PA54", "A1CurrentSmoke", # "RaceG3",
            "B1SCHRON", "C1SCHRON", "A1SBMI", "B1SBMI",
            "B1CurrentSmoke", "B4PSQI1", "B4ExtremeDiet",
            "B1CurrentAlcHigh", "B1CurrentAlcMod", "B4CurrentAlcHigh", "B4CurrentAlcMod",
            "m1chadv", "m2aadv", "B1LifeStress",
            "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
            "B4QPS_PS", "B4CTQTotal", "B4QCT_PA",
            "B1Sleep", "C1Sleep", "B1SleepOnsetLatency", "C1SleepOnsetLatency",
            ## M2
            "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM",
            "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
            "B1SLFEDI", "B1SDAYDI", "B4QPS_PS",
            ## change variables
            "dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi",
            "dFriSt", "dFamSt", "dParSt"
            )), with = FALSE])

  facs <- which(unlist(lapply(sdatimp, class)) %in% c("ordered", "factor"))
  sdatimp[, facs][] <- lapply(sdatimp[, facs], as.integer)

  imputation_seeds <- c(403L, 3L, 913898947L, -1414610478L, 1378650212L, -329963609L,
                        -1232000771L, 597884148L, 836787162L, -1911853275L, -2069580225L,
                        -1674396218L, -1140270512L, -511781261L, -64398703L, 1524703936L,
                        -146946914L, -875321239L, -1388084069L, -364036694L, 1844440524L,
                        1483571343L, 738984133L, -2009223044L, -69890862L, -613768115L,
                        931345671L, 1671387374L, -1123419704L, -204300597L, -882176215L,
                        148313976L, 389491878L, -439992639L, -447023277L, -1228409854L,
                        660492596L, 881244855L, -1868126547L, 135407556L, 183570410L,
                        -754724779L, 1731669487L, 493696502L, 24793888L, -981889821L,
                        -1496954495L, 763052016L, -156918130L, -1438653959L)


  clusterExport(cl, "sdatimp")
  clusterExport(cl, "imputation_seeds")

  micedimp <- parLapply(cl, 1:50, function(i) {
    set.seed(imputation_seeds[i])
    complete(mice(sdatimp, m = 1,
         method = "rf",
         ntree = 10,
         maxit = 10,
         seed = imputation_seeds[i]), 1)
  })
  save(micedimp, file = "sleepstress_micerfimp.RData", compress = "xz")


  ## dimputed <- parLapply(cl, 1:50, function(i) {
  ##   set.seed(imputation_seeds[i])
  ##   traind <- preProcess(sdatimp, method = 'bagImpute')
  ##   predict(traind, sdatimp)
  ## })
  ## save(dimputed, file = "sleepstress_bagimp.RData", compress = "xz")
}
load("sleepstress_bagimp.RData")
load("sleepstress_micerfimp.RData")


egltable(vars = c("Sex", "B1PAGE_M2", "white", "m1chadv", "A1SCHRON", "A1PDEPDX", "A1PANXTD",
                  stress.vars, sleep.vars),
         data = within(as.data.frame(dimputedl), {
           A1PDEPDX <- factor(round(A1PDEPDX))
           A1PANXTD <- factor(round(A1PANXTD))
         }))



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


## systems <- c("B4bi.sym", "B4bi.hpa", "B4bi.card", "B4bi.gluc", "B4bi.lipid", "B4bi.infl", "B4bi.para", "B4F")
## clusterExport(cl, c("bi.data", "v.cov"))
## sleep.al.all <- lapply(1:6, function(sleepv) {
##   lapply(systems, function(dv) {
##     dv <- dv
##     sleepv <- sleepv
##     e <- environment()
##     clusterExport(cl, varlist = c("dv", "sleepv"), envir = e)
##     parLapply(cl, 1:50, function(i) {
##       useIDs <- unique(d.cort.all[[sleepv]]$d$M2ID)
##       tmpd <- data.table(
##         M2ID = useIDs,
##         U_Sleep = d.cort.all[[sleepv]]$m$U[i, ],
##         V_Sleep = d.cort.all[[sleepv]]$m$Sigma_V[i, ])
##       setkey(tmpd, M2ID)
##       tmpd <- merge(tmpd, dw[, c(v.cov[!v.cov %in% c("Smoke", "CortMeds", names(tmpd))], "M2ID", "M2FAMNUM"), with=FALSE], all.x = TRUE)
##       tmpd <- merge(tmpd, bi.data[[i]], by = "M2ID")
##       ## use to get only one person per family
##       ## tmpd <- merge(tmpd, dw[, .(M2ID, M2FAMNUM)], by = "M2ID")[!duplicated(M2FAMNUM)]
##       mv <- unique(tmpd)[, .(Mu = mean(U_Sleep, na.rm = TRUE),
##                              SDu = sd(U_Sleep, na.rm = TRUE),
##                              Mv = mean(V_Sleep, na.rm = TRUE),
##                              SDv = sd(V_Sleep, na.rm = TRUE))]
##       tmpd[, U_Sleep := (U_Sleep - mv$Mu) / mv$SDu]
##       tmpd[, V_Sleep := (V_Sleep - mv$Mv) / mv$SDv]
##       tmpd <- na.omit(tmpd[, c(dv, "ALMedGrp", "White", "B1Smoke", "B4QPS_PS", "p4majorconditions",
##                                "M2ID", "M2FAMNUM", "U_Sleep", "V_Sleep"), with = FALSE])
##       f0 <- sprintf("scale(%s) ~ U_Sleep", dv)
##       f1 <- sprintf("scale(%s) ~ U_Sleep + V_Sleep", dv)
##       f2 <- sprintf("scale(%s) ~ ALMedGrp + White + B1Smoke + B4QPS_PS + p4majorconditions + U_Sleep", dv)
##       f3 <- sprintf("scale(%s) ~ ALMedGrp + White + B1Smoke + B4QPS_PS + p4majorconditions + U_Sleep + V_Sleep", dv)
##       lapply(list(f3, f2, f1, f0), function(f) {
##         m <- tryCatch(do.call(gls, list(model = as.formula(f), data = tmpd, correlation = corCompSymm(form = ~ 1 | M2FAMNUM), na.action = na.omit)),
##                       error = function(e) e)
##         m$call$data <- "tmpd"
##         return(m)
##       })
##     })
##   })
## })
## sleep.al.all[[3]][[3]][[39]] <- NULL


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

