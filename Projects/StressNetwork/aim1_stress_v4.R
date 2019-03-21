if (Sys.getenv("USERNAME") == "jwile") {
  dir.setup <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Setup/"
  dir.data <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Data/Processed/"
  dir.imp <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Setup/stressnetwork_imputation/"
  dir.proj <- "c:/Users/jwile/OneDrive/Projects/MIDUS/Projects/StressNetwork/"
} else {
  dir.setup <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
  dir.data <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
  dir.imp <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
  dir.proj <- "/home/b8szwf/EncDB/b8szwf@gmail.com/MIDUS/"
}

## hack to make rJava work on ubuntu RStudio server
if (identical(Sys.getenv("RSTUDIO"), "1")) {
  dyn.load("/usr/lib/jvm/java-9-oracle/lib/server/libjvm.so")
}

source(file.path(dir.setup, "setup_packages_functions.R"))
options(stringsAsFactors = FALSE, digits = 3)

## Power and Interval Precision Estimation
## via simulation

library(MASS)

simple.surv.sim.Modified <- function(n, foltime, dist.ev, anc.ev, beta0.ev, dist.cens = "weibull",
                                     anc.cens, beta0.cens, z = NULL, beta = NA, mu, Sigma,
                                     empirical = FALSE) {
sim.data <- vector("list", length = n)

eff <- mvrnorm(
  n = n,
  mu = mu,
  Sigma = Sigma,
  empirical = empirical)

for (i in 1:n) {
  sim.data[[i]] <- simple.ev.sim(foltime, anc.ev, beta0.ev,
                                 anc.cens, beta0.cens, z, beta,
                                 as.vector(eff[i, ]), dist.ev, dist.cens,
                                 i)
}
sim.data <- do.call(rbind, sim.data)
class(sim.data) <- c("simple.surv.sim", "data.frame")
attr(sim.data, "n") <- n
attr(sim.data, "foltime") <- foltime
return(sim.data)
}

set.seed(12345L)
  x <- simple.surv.sim.Modified(
    n = 500000, ## variable sample sizes
    foltime = 97 - 30, ## follow up to 97, assume no dead before 30
    dist.ev = "weibull",
    anc.ev = 4.0,
    beta0.ev = 4.1,
    dist.cens = "weibull",
    anc.cens = 1.2,
    beta0.cens = 3.35,
    z = list(c("unif", .8, 1.2)), ## random noise in intercept
    beta = c(0, -.028), ## log HR of about .10
    mu = c(0, 0),
    Sigma = matrix(c(1, .95, .95, 1), ncol = 2),
    empirical = TRUE)

m <- coxph(Surv(stop+30, status) ~ x + x.1, data = x)
m2 <- coxph(Surv(stop+30, status) ~ x, data = x)

cov(x[, c("x", "x.1")])
cor(x[, c("x", "x.1")])


b <- .14
rs <- seq(.1, 1, length.out = 1000)

plot((rs), (b / rs), type = "l")

.15 / seq(0, 1, by = .01)

coef(m2) / .95

sum(coef(m) * c(1, .95))
sum(coef(m) * c(0, .95))


sum(coef(m) * c(1, .95))

summary(m)
summary(m2)


sum(coef(m) * c(1, .33))

sum(coef(m) * c(1, .95))


print(0.1194 * .8 -0.0155, digits = 5)

if (FALSE) {
set.seed(12345)
simres <-mclapply(1:1000, function(i) {
  x <- simple.surv.sim(
    n = runif(1, 5000, 10000), ## variable sample sizes
    foltime = 97 - 30, ## follow up to 97, assume no dead before 30
    dist.ev = "weibull",
    anc.ev = 4.0,
    beta0.ev = 4.1,
    dist.cens = "weibull",
    anc.cens = 1.2,
    beta0.cens = 3.35,
    z = list(c("unif", .8, 1.2)), ## random noise in intercept
    beta = -.028, ## log HR of about .10
    x = list(c("normal", 0, 1)))

  m <- coxph(Surv(stop+30, status) ~ x, data = x)
  ci <- confint(m)

  data.table(
    N = nrow(x),
    Dead = mean(x$status == 1),
    Censored = mean(x$status == 0),
    LastAge = mean(x$stop + 30),
    B = coef(m)[[1]],
    LL = ci[1,1],
    UL = ci[1,2],
    P = coef(summary(m))[, 5])
}, mc.cores = 8L, mc.allow.recursive = FALSE)
saveRDS(simres, file = "cox_sim.RDS", compress = "xz")
} else {
  simres <- readRDS(file.path(dir.data, "cox_sim.RDS"))
}
simres <- do.call(rbind, simres)

## plot using GAMs for average and quantile regression for 80th percentile
plot_grid(
ggplot(simres, aes(N, UL - LL)) +
  geom_smooth(se=FALSE, colour = "blue") +
  stat_quantile(formula=y~ x + I(x^2), quantiles=c(.8), colour = "black") +
  geom_hline(yintercept = .15) +
  geom_vline(xintercept = 5500),
ggplot(simres, aes(N*Dead, UL - LL)) +
  geom_smooth(se=FALSE, colour = "blue") +
  stat_quantile(formula=y~ x + I(x^2), quantiles=c(.8), colour = "black") +
  geom_hline(yintercept = .15) +
  geom_vline(xintercept = 750),
ggplot(simres, aes(N, P)) +
  geom_smooth(se=FALSE, colour = "blue") +
  stat_quantile(formula=y~ x + I(x^2), quantiles=c(.8), colour = "black") +
  geom_hline(yintercept = .05) +
  geom_vline(xintercept = 5500),
ggplot(simres, aes(N*Dead, P)) +
  geom_smooth(se=FALSE, colour = "blue") +
  stat_quantile(formula=y~ x + I(x^2), quantiles=c(.8), colour = "black") +
  geom_hline(yintercept = .05) +
  geom_vline(xintercept = 750), ncol = 2)

## additional power analyses

powerEpiCont.default(n = 7000, theta = 1.10, sigma2 = 1, psi = .16, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 7000, theta = 1.10, sigma2 = 1, psi = .14, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 7000, theta = 1.10, sigma2 = 1, psi = .12, rho2 = 0, alpha = .05)

powerEpiCont.default(n = 6000, theta = 1.10, sigma2 = 1, psi = .16, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 6000, theta = 1.10, sigma2 = 1, psi = .14, rho2 = 0, alpha = .05)
powerEpiCont.default(n = 6000, theta = 1.10, sigma2 = 1, psi = .12, rho2 = 0, alpha = .05)

## smallest sample size given 14% death rate
powerEpiCont.default(n = 6200, theta = 1.10, sigma2 = 1, psi = .14, rho2 = 0, alpha = .05)


## code based off mice::pool.compare
pool.compare.null <- function (fit1) {
    m1 <- length(fit1$analyses)
    m <- m1
    est1 <- mice::pool(fit1)
    dimQ1 <- length(est1$qbar)
    dimQ2 <- dimQ1 - 0
    formula1 <- formula(fit1$analyses[[1]])
    Q <- diag(rep(1, dimQ1), ncol = dimQ1)
    Q <- matrix(Q[((dimQ1 - dimQ2 + 1):dimQ1), ], nrow = dimQ2,
                ncol = dimQ1)
    qbar <- Q %*% est1$qbar
    Ubar <- Q %*% est1$ubar %*% (t(Q))
    Bm <- Q %*% est1$b %*% (t(Q))
    rm <- (1 + 1/m) * sum(diag(Bm %*% (solve(Ubar))))/dimQ2
    Dm <- (t(qbar)) %*% (solve(Ubar)) %*% qbar/(dimQ2 * (1 +
                                                         rm))
    v <- dimQ2 * (m - 1)
    if (v > 4)
        w <- 4 + (v - 4) * ((1 + (1 - 2/v) * (1/rm))^2)
    else w <- v * (1 + 1/dimQ2) * ((1 + 1/rm)^2)/2
    statistic <- list(Dm = Dm, rm = rm, df1 = dimQ2, df2 = w,
        pvalue = 1 - pf(Dm, dimQ2, w))
    return(statistic)
}

format.mortality <- function(object) {
  res <- summary(mice::pool(as.mira(object)))
  data.table(
    IV = rownames(res),
    Est = sprintf("%0.2f%s\n[%0.2f, %0.2f]",
                  exp(res[, "est"]),
                  star(res[, "Pr(>|t|)"]),
                  exp(res[, "lo 95"]),
                  exp(res[, "hi 95"])))
}

pool.cindex <- function(obj) {
  tanh(mean(atanh(sapply(1:50, function(i) summary(obj[[i]])$concordance[[1]] ))))
}


NNT <- function(est) {
  p <- 1 - est[, 1]
  p1 <- 1 - est[, 2]
  1 / (p - p1)
}

dwclean <- readRDS(file.path(dir.data, "midus_merged_data_dwclean.RDS"))
psr_pana_bi.data <- readRDS(file.path(dir.data, "midus_merged_data_psrpanabi_imp.RDS"))
dwcleanimp <- readRDS(file.path(dir.imp, "midus_stress_final_imputed.RDS"))

v <- list(
  stress = c(
    ## M1
    "A1SLFEDI", "A1SDAYDI",
    "A1SPIWOR", "A1SHOMET", "A1SPIHOM", "A1SPIFAM",
    "A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1ParentAbuse", "A1SiblingAbuse",
    ## M2 (Retrospective)
    "B1ChildLifeStress", "B4CTQTotal",
    "B1LifeStress", "B1LifeStressImpactS", "B1LifeStressImpactL",
    ## M2
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM", "B1SHOMET",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS",
    ## change variables
    "dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi",
    "dFriSt", "dFamSt", "dParSt"
  ),
  health.cat = c(
  "A1PA4", "A1PA5", "A1PA6",
  "B1PA1", "B1PA2", "B1PA3",
  "A1PDEPDX", "A1PANXTD",
  "B1PDEPDX", "B1PANXTD"
  ),
  health = c(
  "A1SCHRON", "B1SCHRON", "C1SCHRON", #"p4majorconditions",
  "b3tem", "exec_fxn",
  "Radj_epi", "Radj_nor", "avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf",
  "Rb4p1d", "Radj_crt", "b4bdheas", "Rb4bcrp", "b4bil6", "b4bfgn",
  "b4bsicam", "b4bsesel", "pulpress", "Rb4p1gs", "b4bha1c", "Rb4bgluc",
  "p4homair", "b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr",
  ## mood
  "B1NegAff", "B1PosAff", "B1PR", "B1SR",
  ## AL and systems
  "B4F", "B4bi.sym", "B4bi.hpa", "B4bi.card",
  "B4bi.gluc", "B4bi.lipid", "B4bi.infl", "B4bi.para"
  ),
  cov = c(
    "A1SBMI", "B1SBMI", "C1SBMI",
    "A1SWSTHI", "B1SWSTHI", "C1SWSTHI",
    "BirthYear", "AGEM1", "AGEM2",
    ## health behaviors
    "A1PhysAct", "B1PhysAct", "C1PhysAct",
    "B1Sleep", "B1SSQ", "C1Sleep", "C1SSQ",
    ## SES
    "m1chadv", "m2aadv",
    ## psychosocial resources / personality / coping
    "B1SREINT", "B1SACTIV", "B1SPLAN", "B1SVENT", "B1SDENIA",
    "B1SDISEN", "B1SFDCOP", "B1SAGENC", "B1SAGREE", "B1SCONS2", "B1SEXTRA",
    "B1SNEURO", "B1SOPEN", "B1SPWBA2", "B1SPWBG2", "B1SPWBE2", "B1SPWBU2",
    "B1SPWBS2", "B1SPWBR2", "B1SMASTE", "B1SCONST", "B1SSWBSI", "B1SKINPO",
    "B1SFDSPO", "B1SSPEMP", "B1SESTEE", "B1SMPQCN", "B1SOPTIM", "B1SPESSI"
  ),
  cov.cat = c(
    "Sex", "RaceG3",
    ## health insurance
    "A1SC1", "B1SC1", "C1SC1",
    ## health behaviors
    "A1Smoke", "B1Smoke", "C1Smoke",
    "B1CurrentAlcohol", "A1WorstAlcohol", "B1WorstAlcohol", "C1CurrentAlcohol", "C1WorstAlcohol",
    ## bed partner
    #"B4S9",
    ## education & work status
    "A1PB1", "B1PB1", "C1PB1",
    "A1PBWORK", "B1PBWORK", "C1PB3WK",
    ## SES
    "m1welf_all", "m1ped3_all", "m1cses_all",
    "m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all"
  ),
  psr = c("B1PSRQSFPR", "B1PSRQSFSR"),
  noimp = c("M2ID", "M2FAMNUM", "LastAge", "DECEASED")
)

## only includes MIDUS participants with some stress or health measure
nrow(dwcleanimp[[1]]) ## total

useIDs <- dwcleanimp[[1]][["M2ID"]]

dwclean.use <- dwclean[match(useIDs, dwclean$M2ID)]
## all stress measures
x <- get_all_vars(as.formula(paste0("~",
                                    "A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI + A1SPIWOR + A1SPIFAM + A1SPIHOM",
                                    "+B1SLFEDI + B1SDAYDI + B1SJOBDI + B1SPIWOR + B1SPIHOM + B1SPIFAM + B1SSPCRI + B1SKINNE + B1SFDSNE + B4QPS_PS + B1LifeStress")),
                  data = dwclean.use)
table(rowMeans(is.na(x)) == 1)
useIDs <- dwclean.use[rowMeans(is.na(x)) < 1, M2ID]

dwclean.use <- dwclean[match(useIDs, dwclean$M2ID)]
dwcleanimp <- lapply(dwcleanimp, function(dat) {
  subset(dat, M2ID %in% useIDs)
})

table(dwclean.use$M2ID %in% useIDs)

dwclean.use$PAfterM2 <- rowMeans(sapply(dwcleanimp, function(d) with(d, LastAge > AGEM2 | DECEASED == 0)))

table(dwclean.use$PAfterM2)


dwclean.use$CumStressM1 <- rowSums(apply(dwclean.use[, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
                                                         "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) as.vector(scale(x))))
dwclean.use$CumStressM2 <- rowSums(apply(dwclean.use[, c(
  "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
  "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
  "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
  "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

dwclean.use$DiscM1 <- rowSums(apply(dwclean.use[, c("A1SLFEDI", "A1SDAYDI")],
                                    2, function(x) as.vector(scale(x))))
dwclean.use$SocM1 <- rowSums(apply(dwclean.use[, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE")],
                                   2, function(x) as.vector(scale(x))))
dwclean.use$IneqM1 <- rowSums(apply(dwclean.use[, c("A1SPIWOR", "A1SPIFAM", "A1SPIHOM")],
                                    2, function(x) as.vector(scale(x))))

dwclean.use$DiscM2 <- rowSums(apply(dwclean.use[, c(
  "B1SLFEDI", "B1SDAYDI", "B1SJOBDI")], 2, function(x) as.vector(scale(x))))
dwclean.use$SocM2 <- rowSums(apply(dwclean.use[, c(
  "B1SSPCRI", "B1SKINNE", "B1SFDSNE")], 2, function(x) as.vector(scale(x))))
dwclean.use$IneqM2 <- rowSums(apply(dwclean.use[, c(
  "B1SPIWOR", "B1SPIHOM", "B1SPIFAM")], 2, function(x) as.vector(scale(x))))

dwclean.use$CumStressBM1 <- rowSums(apply(dwclean.use[, c("DiscM1", "IneqM1", "SocM1")],
                                          2, function(x) as.vector(scale(x))))
dwclean.use$CumStressBM2 <- rowSums(apply(dwclean.use[, c(
  "DiscM2", "IneqM2", "SocM2",
  "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

dwclean.use$CumStressBaltM2 <- rowSums(apply(dwclean.use[, c(
  "DiscM2", "IneqM2", "SocM2",
  "B1LifeStress")], 2, function(x) as.vector(scale(x))))


dwclean.use$CumStressBM1 <- (dwclean.use$CumStressBM1 -
                               quantile(dwclean.use$CumStressBM1, probs = .25,
                                        na.rm = TRUE)) / IQR(dwclean.use$CumStressBM1, na.rm = TRUE)

dwclean.use$DiscM1 <- (dwclean.use$DiscM1 -
  quantile(dwclean.use$DiscM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$DiscM1, na.rm = TRUE)
dwclean.use$IneqM1 <- (dwclean.use$IneqM1 -
  quantile(dwclean.use$IneqM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$IneqM1, na.rm = TRUE)
dwclean.use$SocM1 <- (dwclean.use$SocM1 -
  quantile(dwclean.use$SocM1, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$SocM1, na.rm = TRUE)


dwclean.use$CumStressBM2 <- (dwclean.use$CumStressBM2 -
  quantile(dwclean.use$CumStressBM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$CumStressBM2, na.rm = TRUE)
dwclean.use$CumStressBaltM2 <- (dwclean.use$CumStressBaltM2 -
  quantile(dwclean.use$CumStressBaltM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$CumStressBaltM2, na.rm = TRUE)
dwclean.use$DiscM2 <- (dwclean.use$DiscM2 -
  quantile(dwclean.use$DiscM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$DiscM2, na.rm = TRUE)
dwclean.use$IneqM2 <- (dwclean.use$IneqM2 -
  quantile(dwclean.use$IneqM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$IneqM2, na.rm = TRUE)
dwclean.use$SocM2 <- (dwclean.use$SocM2 -
  quantile(dwclean.use$SocM2, probs = .25,
    na.rm = TRUE)) / IQR(dwclean.use$SocM2, na.rm = TRUE)

dwclean.use$B4QPS_PS <- (dwclean.use$B4QPS_PS -
  quantile(dwclean.use$B4QPS_PS, probs = .25, na.rm = TRUE)) /
  IQR(dwclean.use$B4QPS_PS, na.rm = TRUE)
dwclean.use$B1LifeStress <- (dwclean.use$B1LifeStress -
  quantile(dwclean.use$B1LifeStress, probs = .25, na.rm = TRUE)) /
  IQR(dwclean.use$B1LifeStress, na.rm = TRUE)

for (i in 1:50) {
  dwcleanimp[[i]]$PAfterM2 <- dwclean.use$PAfterM2
}


for (i in 1:50) {
  dwcleanimp[[i]]$CumStressM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$CumStressM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))

  dwcleanimp[[i]]$CumStressQ50M1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) x > median(x, na.rm = TRUE)))
  dwcleanimp[[i]]$CumStressQ50M2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) x > median(x, na.rm = TRUE)))

  dwcleanimp[[i]]$CumStressQ75M1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM")], 2, function(x) x > quantile(x, probs = .75, na.rm = TRUE)))
  dwcleanimp[[i]]$CumStressQ75M2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) x > quantile(x, probs = .75, na.rm = TRUE)))

  dwcleanimp[[i]]$DiscM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SLFEDI", "A1SDAYDI")],
                                          2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$SocM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SSPCRI", "A1SKINNE", "A1SFDSNE")],
                                          2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$IneqM1 <- rowSums(apply(dwcleanimp[[i]][, c("A1SPIWOR", "A1SPIFAM", "A1SPIHOM")],
                                         2, function(x) as.vector(scale(x))))

  dwcleanimp[[i]]$DiscM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SLFEDI", "B1SDAYDI", "B1SJOBDI")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$SocM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SSPCRI", "B1SKINNE", "B1SFDSNE")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$IneqM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "B1SPIWOR", "B1SPIHOM", "B1SPIFAM")], 2, function(x) as.vector(scale(x))))


  dwcleanimp[[i]]$CumStressBM1 <- rowSums(apply(dwcleanimp[[i]][,
    c("DiscM1", "IneqM1", "SocM1")], 2, function(x) as.vector(scale(x))))
  dwcleanimp[[i]]$CumStressBM2 <- rowSums(apply(dwcleanimp[[i]][, c(
    "DiscM2", "IneqM2", "SocM2",
    "B4QPS_PS", "B1LifeStress")], 2, function(x) as.vector(scale(x))))
}

## covariates
use.cov.m1stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all", "A1PB1", "A1PBWORK", "A1SC1"),
 c("A1Smoke", "A1WorstAlcohol", "A1PhysAct", "A1SWSTHI"),
 c("A1PDEPDX", "A1PANXTD", "A1SCHRON"))

use.cov.m2stress <- list("",
 c("Sex", "RaceG3", "m1ped3_all", "m1welf_all", "B1PB1", "B1PBWORK", "B1SC1"),
 c("B1Smoke", "B1WorstAlcohol", "B1PhysAct", "B1SWSTHI"),
 c("B1PDEPDX", "B1PANXTD", "B1SCHRON"))


m1.stress.vars <- list("",
  c("DiscM1"),
  c("IneqM1"),
  c("SocM1"),
  c("CumStressBM1"))

m2.stress.vars.full <- list("",
  c("DiscM2"),
  c("IneqM2"),
  c("SocM2"),
  c("B4QPS_PS"),
  "B1LifeStress",
  "CumStressBM2")

dwcleanimp <- lapply(dwcleanimp, function(d) {
  d <- within(d, {
    DECEASED <- as.integer(DECEASED == "1")
    A1PB1 <- as.integer(as.character(A1PB1))
    B1PB1 <- as.integer(as.character(B1PB1))
    A1SC1 <- as.integer(as.character(A1SC1)) - 1L
    B1SC1 <- as.integer(as.character(B1SC1)) - 1L
    A1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
    B1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
  })
  d <- within(d, {
    A1PB1 <- recode(A1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor = TRUE)
    B1PB1 <- recode(B1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor = TRUE)
  })
  d <- d[, unique(unlist(c(use.cov.m1stress[-1], use.cov.m2stress[-1], m1.stress.vars[-1], m2.stress.vars.full[-1],
                           "LastAge", "DECEASED", "M2FAMNUM", "M2ID", "PAfterM2", "AGEM2", "AGEM1",
                           "CumStressM1", "CumStressM2",
                           "CumStressBM1", "CumStressBM2",
                           "A1SSPCRI", "A1SKINNE", "A1SFDSNE", "A1SLFEDI",
                           "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SPIHOM",
                           "B1SLFEDI", "B1SDAYDI", "B1SJOBDI", "B1SPIWOR",
                           "B1SPIHOM", "B1SPIFAM", "B1SSPCRI", "B1SKINNE",
                           "B1SFDSNE", "B4QPS_PS", "B1LifeStress")))]
  d$CumStressBM1 <- (d$CumStressBM1 - quantile(d$CumStressBM1, probs = .25)) / IQR(d$CumStressBM1)
  d$DiscM1 <- (d$DiscM1 - quantile(d$DiscM1, probs = .25)) / IQR(d$DiscM1)
  d$IneqM1 <- (d$IneqM1 - quantile(d$IneqM1, probs = .25)) / IQR(d$IneqM1)
  d$SocM1 <- (d$SocM1 - quantile(d$SocM1, probs = .25)) / IQR(d$SocM1)

  d$CumStressBM2 <- (d$CumStressBM2 - quantile(d$CumStressBM2, probs = .25)) / IQR(d$CumStressBM2)
  d$DiscM2 <- (d$DiscM2 - quantile(d$DiscM2, probs = .25)) / IQR(d$DiscM2)
  d$IneqM2 <- (d$IneqM2 - quantile(d$IneqM2, probs = .25)) / IQR(d$IneqM2)
  d$SocM2 <- (d$SocM2 - quantile(d$SocM2, probs = .25)) / IQR(d$SocM2)
  d$B4QPS_PS <- (d$B4QPS_PS - quantile(d$B4QPS_PS, probs = .25)) / IQR(d$B4QPS_PS)
  d$B1LifeStress <- (d$B1LifeStress - quantile(d$B1LifeStress, probs = .25)) / IQR(d$B1LifeStress)

  return(d)
})


################################################################################
##                                                                            ##
##                            Descriptive Statistics                          ##
##                                                                            ##
################################################################################

## follow-up years MIDUS 1
print(mean(dwclean.use$LastAge - dwclean.use$AGEM1), digits = 4)
## follow-up years MIDUS 2
print(mean(dwclean.use$LastAge - dwclean.use$AGEM2), digits = 3)

egltable(c(
  "A1SLFEDI", "B1SLFEDI",
  "A1SDAYDI", "B1SDAYDI", "B1SJOBDI",
  "A1SPIWOR", "B1SPIWOR",
  "A1SPIHOM", "B1SPIHOM",
  "A1SPIFAM", "B1SPIFAM",
  "B4QPS_PS",
  "A1SKINNE", "B1SKINNE",
  "A1SFDSNE", "B1SFDSNE",
  "A1SSPCRI", "B1SSPCRI",
  "B1LifeStress"
  ),
  data = as.data.frame(dwclean.use), parametric=FALSE)

egltable(c(
  "Sex", "m1ped3_all",
  "A1PB1", "B1PB1",
  "A1SC1", "B1SC1",
  "A1PBWORK", "B1PBWORK",
  "A1Smoke", "B1Smoke",
  "A1WorstAlcohol", "B1WorstAlcohol",
  "A1SWSTHI", "B1SWSTHI",
  "AGEM1", "AGEM2", "A1PDEPDX", "B1PDEPDX",
  "A1SCHRON", "B1SCHRON"), "Sex",
  data = as.data.frame(dwclean.use), parametric=TRUE, strict=FALSE)


cortab.m1 <- tanh(Reduce(`+`, lapply(dwcleanimp, function(d) {
  atanh(cor(d[, c("A1SLFEDI", "A1SDAYDI", "AGEM1",
                  "A1SPIWOR", "A1SPIHOM", "A1SPIFAM",
                  "A1SSPCRI", "A1SKINNE", "A1SFDSNE",
                  "AGEM1", "AGEM1")],
      method = "spearman"))
}))/50)

cortab.m2 <- tanh(Reduce(`+`, lapply(dwcleanimp, function(d) {
  atanh(cor(d[, c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI",
                  "B1SPIWOR", "B1SPIHOM", "B1SPIFAM",
                  "B1SSPCRI", "B1SKINNE", "B1SFDSNE",
                  "B4QPS_PS", "B1LifeStress")],
      method = "spearman"))
}))/50)

cortab.m1m2 <- cortab.m2
cortab.m1m2[upper.tri(cortab.m1m2)] <- cortab.m1[upper.tri(cortab.m1)]
cortab.m1m2[] <- round(cortab.m1m2[], 2)
diag(cortab.m1m2) <- ""
cortab.m1m2
## write.table(cortab.m1m2, file = "clipboard", sep = "\t")

################################################################################
##                                                                            ##
##                                Exploratory                                 ##
##                                                                            ##
################################################################################


tmplongdat <- melt(as.data.frame(scale(dwclean.use[, c("M2ID", grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE)), with = FALSE])), id.vars = "M2ID")

tmplongdat$variable <- factor(tmplongdat$variable,
                              levels = levels(tmplongdat$variable),
                              labels = stresskey2$Label[match(levels(tmplongdat$variable), stresskey2$Var)])

ggplot(tmplongdat,
       aes(value)) +
  geom_histogram(aes(y = ..density..), bins = 20) +
  geom_density() +
  theme_cowplot() +
  facet_wrap(~variable, scales = "free") + ggtitle("Stress Meausures for MIDUS 1 (A) and MIDUS 2 (B)") +
  xlab("Z Scores")

napprox <- colSums(!is.na(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE]))

r.pearson <- cor(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE],
                 use = "pairwise.complete.obs", method = "pearson")
r.spearman <- cor(dwclean.use[, grep("^(?!d).*$", v$stress, value = TRUE, perl=TRUE), with = FALSE],
                 use = "pairwise.complete.obs", method = "spearman")

colnames(r.pearson) <- rownames(r.pearson) <- stresskey2$Label[match(colnames(r.pearson), stresskey2$Var)]

colnames(r.spearman) <- rownames(r.spearman) <- stresskey2$Label[match(colnames(r.spearman), stresskey2$Var)]

hist((r.pearson - r.spearman)[lower.tri(r.pearson)], breaks = 30,
     xlab = "Difference between Pearson and Spearman",
     main = "Histogram of Correlation Differences")

corplot(r.pearson[1:11, 1:11], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Pearson")

corplot(r.pearson[-(1:11), -(1:11)], plot = "cor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Pearson")



hist(sapply(1:50, function(i) {
alpha(scale(as.data.table(dwcleanimp[[i]])[, .(A1SLFEDI, A1SDAYDI,
                                A1SPIWOR, A1SPIHOM, A1SPIFAM)]))$total$std.alpha
}))


################################################################################
##                                                                            ##
##                       Stress Grouping / Clustering                         ##
##                                                                            ##
################################################################################


alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI,
                                               A1SPIWOR, A1SPIHOM, A1SPIFAM)]))

alpha(scale(dwclean.use[, .(A1SLFEDI, A1SDAYDI)]))

alpha(scale(dwclean.use[, .(A1SPIWOR, A1SPIHOM, A1SPIFAM)]))


alpha(scale(dwclean.use[, .(A1SKINNE, A1SFDSNE, A1SSPCRI)]))

alpha(scale(dwclean.use[, .(B1SLFEDI, B1SDAYDI, B1SJOBDI,
                      B1SPIWOR, B1SPIHOM, B1SPIFAM)]))
alpha(scale(dwclean.use[, .(B4QPS_PS, B1SKINNE, B1SFDSNE, B1SSPCRI)]))

m1.cfa1 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m1_stresscfa1.dat", run = 1L)

summary(m1.cfa1)
coef(m1.cfa1, type = "stdyx", param = "loading")
screenreg(m1.cfa1, type = "stdyx", param = "loading", single.row = TRUE)

m1.cfa2 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use), dataout = "m1_stresscfa2.dat", run = 1L)
summary(m1.cfa2)
coef(m1.cfa2, type = "stdyx", param = "loading")
screenreg(m1.cfa2, type = "stdyx", param = "loading", single.row = TRUE)

dwclean.use.m1.cat <- copy(dwclean.use[, m1.cfa1$usevariables, with=FALSE])
dwclean.use.m1.cat[, (colnames(dwclean.use.m1.cat)) :=
                       lapply(.SD, function(x) {
                         cut(x, breaks = unique(quantile(x,
                           probs = seq(0, 1, by = .2),
                           na.rm = TRUE)),
                           include.lowest = TRUE, ordered_result = TRUE)
                       })
                   ]


m1.cfa3 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa3.dat", run = 1L)
coef(m1.cfa3, type = "stdyx", param = "loading")

m1.cfa3b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI
     A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa3b.dat", run = 1L)
m1.cfa3b$results$summaries
coef(m1.cfa3b, type = "stdyx", param = "loading")

m1.cfa4 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa4.dat", run = 1L)
coef(m1.cfa4, type = "stdyx", param = "loading")

m1.cfa4b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      A1SLFEDI A1SDAYDI
      A1SPIWOR A1SPIHOM A1SPIFAM
      A1SKINNE A1SFDSNE A1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY A1SLFEDI A1SDAYDI;
   Ineq BY A1SPIWOR A1SPIHOM A1SPIFAM;
   Perc BY A1SKINNE A1SFDSNE A1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m1.cat), dataout = "m1_stresscfa4b.dat", run = 1L)
m1.cfa4b$results$summaries


screenreg(list(
  M1 = extract(m1.cfa1, type = "stdyx", param = "loading"),
  M1CatML = extract(m1.cfa3, type = "stdyx", param = "loading"),
  M1CatWLSMV = extract(m1.cfa3b, type = "stdyx", param = "loading")
  ))

summary(m1.cfa1)
m1.cfa3b$results$summaries

screenreg(list(
  M1Alt = extract(m1.cfa2, type = "stdyx", param = "loading"),
  M1AltCatML = extract(m1.cfa4, type = "stdyx", param = "loading"),
  M1AltCatWLSMV = extract(m1.cfa4b, type = "stdyx", param = "loading")
  ))

summary(m1.cfa2)
m1.cfa4b$results$summaries



m2.cfa1 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = subset(dwclean.use, PAfterM2==1)), dataout = "m2_stresscfa1.dat", run = 1L)
summary(m2.cfa1)
coef(m2.cfa1, type = "stdyx", param = "loading")
screenreg(m2.cfa1, type = "stdyx", param = "loading", single.row = TRUE)

m2.cfa2 <- mplusModeler(mplusObject(
  VARIABLE = "CLUSTER = M2FAMNUM;",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = subset(dwclean.use, PAfterM2==1)), dataout = "m2_stresscfa2.dat", run = 1L)
summary(m2.cfa2)
coef(m2.cfa2, type = "stdyx", param = "loading")
screenreg(m2.cfa2, type = "stdyx", param = "loading", single.row = TRUE)

dwclean.use.m2.cat <- copy(dwclean.use[, m2.cfa1$usevariables, with=FALSE])
dwclean.use.m2.cat[, (colnames(dwclean.use.m2.cat)) :=
                       lapply(.SD, function(x) {
                         cut(x, breaks = unique(quantile(x,
                           probs = seq(0, 1, by = .2),
                           na.rm = TRUE)),
                           include.lowest = TRUE, ordered_result = TRUE)
                       })
                   ]


m2.cfa3 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa3.dat", run = 1L)
coef(m2.cfa3, type = "stdyx", param = "loading")

m2.cfa3b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI
     B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa3b.dat", run = 1L)
m2.cfa3b$results$summaries
coef(m2.cfa3b, type = "stdyx", param = "loading")

m2.cfa4 <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = MLR;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa4.dat", run = 1L)
coef(m2.cfa4, type = "stdyx", param = "loading")

m2.cfa4b <- mplusModeler(mplusObject(
  VARIABLE = "
    CATEGORICAL ARE
      B1SLFEDI B1SDAYDI B1SJOBDI
      B1SPIWOR B1SPIHOM B1SPIFAM
      B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
    CLUSTER = M2FAMNUM;
  ",
  ANALYSIS = "
   TYPE = COMPLEX;
   ESTIMATOR = WLSMV;
   PROCESSORS = 2;",
  MODEL = "
   Disc BY B1SLFEDI B1SDAYDI B1SJOBDI;
   Ineq BY B1SPIWOR B1SPIHOM B1SPIFAM;
   Perc BY B4QPS_PS B1SKINNE B1SFDSNE B1SSPCRI;
  ",
  OUTPUT = "STDYX;",
  rdata = dwclean.use.m2.cat), dataout = "m2_stresscfa4b.dat", run = 1L)
m2.cfa4b$results$summaries

screenreg(list(
  M2 = extract(m2.cfa1, type = "stdyx", param = "loading"),
  M2CatML = extract(m2.cfa3, type = "stdyx", param = "loading"),
  M2CatWLSMV = extract(m2.cfa3b, type = "stdyx", param = "loading")
  ))

screenreg(list(
  M2Alt = extract(m2.cfa2, type = "stdyx", param = "loading"),
  M2AltCatML = extract(m2.cfa4, type = "stdyx", param = "loading"),
  M2AltCatWLSMV = extract(m2.cfa4b, type = "stdyx", param = "loading")
  ))


################################################################################
##                                                                            ##
##                     Mortality Model M1 - M2 Automated                      ##
##                                                                            ##
################################################################################

cov.list.m1 <- list(
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK + A1SC1",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK + A1SC1 +
   A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + A1PB1 + A1PBWORK + A1SC1 +
   A1Smoke + A1WorstAlcohol + A1PhysAct + A1SWSTHI +
   A1PDEPDX + A1PANXTD + A1SCHRON")
cov.list.m2 <- list(
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK + B1SC1",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK + B1SC1 +
   B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI",
  "Sex + RaceG3 + m1ped3_all + m1welf_all + B1PB1 + B1PBWORK + B1SC1 +
   B1Smoke + B1WorstAlcohol + B1PhysAct + B1SWSTHI +
   B1PDEPDX + B1PANXTD + B1SCHRON")

## no one should be missing all stress measures
x <- get_all_vars(as.formula(paste0("~",
                                    "A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI + A1SPIWOR + A1SPIFAM + A1SPIHOM",
                                    "+B1SLFEDI + B1SDAYDI + B1SJOBDI + B1SPIWOR + B1SPIHOM + B1SPIFAM + B1SSPCRI + B1SKINNE + B1SFDSNE + B4QPS_PS + B1LifeStress")),
                  data = dwclean.use)
summary(rowMeans(is.na(x)))

x <- get_all_vars(as.formula(paste0("~",
  cov.list.m1[[3]],
  "+ LastAge + DECEASED + A1SSPCRI + A1SKINNE + A1SFDSNE + A1SLFEDI + A1SDAYDI + A1SPIWOR + A1SPIFAM + A1SPIHOM")),
  data = dwclean.use)
colMeans(is.na(x))
mean(is.na(x))

x <- get_all_vars(as.formula(paste0("~",
  cov.list.m2[[3]],
  "+ LastAge + DECEASED + B1SLFEDI + B1SDAYDI + B1SJOBDI + B1SPIWOR + B1SPIHOM + B1SPIFAM + B1SSPCRI + B1SKINNE + B1SFDSNE + B4QPS_PS + B1LifeStress")),
  data = dwclean.use[PAfterM2 == 1])
colMeans(is.na(x))
mean(is.na(x))

detach(package:caret)
if (FALSE) {
## setMKLthreads(1)
m1 <- mclapply(c("CumStressBM1", "DiscM1", "IneqM1", "SocM1"),
               function(v) {
                 runStress(v, cov = cov.list.m1, wave = c(0, 1), binary = FALSE)
               },
               mc.cores = 1L, mc.allow.recursive = FALSE)

## m1.binary <- mclapply(c(.5, .6, .7, .75, .8), function(p) {
##                  runStress("CumStressBM1", cov = cov.list.m1, wave = c(0, 1),
##                            binary = TRUE, pcut = p)
##                },
##                mc.cores = 2L, mc.allow.recursive = FALSE)

m2 <- mclapply(c("CumStressBM2", "DiscM2", "IneqM2", "SocM2", "B4QPS_PS", "B1LifeStress"),
               function(v) {
                 runStress(v, cov = cov.list.m2, wave = c(1), binary = FALSE)
               },
               mc.cores = 1L, mc.allow.recursive = FALSE)

## m2.binary <- mclapply(c(.5, .6, .7, .75, .8), function(p) {
##   runStress("CumStressBM2", cov = cov.list.m2, wave = c(1),
##             binary = TRUE, pcut = p)
##   },
## mc.cores = 2L, mc.allow.recursive = FALSE)

## saveRDS(list(M1 = m1, M1binary = m1.binary, M2 = m2, M2binary = m2.binary),
##         file = file.path(dir.data, "m1_m2_coxph.RDS"), compress = "xz")

saveRDS(list(M1 = m1, M2 = m2),
        file = file.path(dir.data, "m1_m2_coxph.RDS"), compress = "xz")
} else {
  allm <- readRDS(file.path(dir.data, "m1_m2_coxph.RDS"))
m1 <- allm$M1
## m1.binary <- allm$M1binary
m2 <- allm$M2
## m2.binary <- allm$M2binary
}

jit <- position_dodge(width = .9)
vpal <- viridis(6)

for (i in 1:4) {
write.csv(cbind(
  m1[[i]]$HRs[, .(
    Model = Model,
    HR = format(round(HR, digits = 2), nsmall = 2),
    LL = format(round(LL, digits = 2), nsmall = 2),
    UL = format(round(UL, digits = 2), nsmall = 2),
    P = formatPval(P, sd = 4, d = 4))],
  m2[[i]]$HRs[, .(
    HR = format(round(HR, digits = 2), nsmall = 2),
    LL = format(round(LL, digits = 2), nsmall = 2),
    UL = format(round(UL, digits = 2), nsmall = 2),
    P = formatPval(P, sd = 4, d = 4))]),
  file = file.path(dir.proj, sprintf("supplementary_ts%d.csv", i)), row.names = FALSE)
}

write.csv(cbind(
  m2[[5]]$HRs[, .(
    Model = Model,
    HR = format(round(HR, digits = 2), nsmall = 2),
    LL = format(round(LL, digits = 2), nsmall = 2),
    UL = format(round(UL, digits = 2), nsmall = 2),
    P = formatPval(P, sd = 4, d = 4))],
  m2[[6]]$HRs[, .(
    HR = format(round(HR, digits = 2), nsmall = 2),
    LL = format(round(LL, digits = 2), nsmall = 2),
    UL = format(round(UL, digits = 2), nsmall = 2),
    P = formatPval(P, sd = 4, d = 4))]),
  file = file.path(dir.proj, "supplementary_ts5-6.csv"), row.names = FALSE)

m1.all.hr <- rbind(
  cbind(m1[[1]]$HRs, Type = "Multi-dimensional"),
  cbind(m1[[2]]$HRs, Type = "Discrimination"),
  cbind(m1[[3]]$HRs, Type = "Inequality"),
  cbind(m1[[4]]$HRs, Type = "Social Strain"))
m1.all.hr[, Type := factor(Type,
  levels = c("Multi-dimensional", "Discrimination",
             "Inequality", "Social Strain"))]



m1.m4out <- mclapply(c("CumStressBM1"),
               function(v) {
                 runStress(v, cov = cov.list.m1, wave = c(0, 1), binary = FALSE, saveout=TRUE)
               },
               mc.cores = 1L, mc.allow.recursive = FALSE)

tmp <- summary(survfit(m1.m4out[[1]]$Model[[1]],
                newdata = data.frame(CumStressBM1 = c(0, 1))),
        times = c(30, 50, 70, 96))


tmp <- do.call(rbind, lapply(seq_along(m1.m4out[[1]]$Model), function(iter) {
tmp <- summary(survfit(m1.m4out[[1]]$Model[[iter]],
                newdata = data.frame(CumStressBM1 = c(0, 1))),
               times = 30:96)
rbind(
  data.table(
  Imp = iter,
  Age = tmp$time,
  Survival = tmp$surv[, 1],
  Stress = "Low"),
  data.table(
  Imp = iter,
  Age = tmp$time,
  Survival = tmp$surv[, 2],
  Stress = "High"))
}))

## ggplot(tmp[Stress == "High"], aes(Age, Survival, group = Imp)) +
##   geom_line(alpha = .2)

tmp2 <- tmp[, .(
  Survival = mean(Survival, na.rm=TRUE)),
  by = .(Age, Stress)]
tmp2[, Stress := factor(
         Stress, levels = c("Low", "High"),
         labels = c("Lower Quartile", "Upper Quartile"))]


m1.m4out2 <- mclapply(c("CumStressBM2"),
               function(v) {
                 runStress(v, cov = cov.list.m2, wave = c(1), binary = FALSE, saveout=TRUE)
               },
               mc.cores = 1L, mc.allow.recursive = FALSE)

summary(survfit(m1.m4out2[[1]]$Model[[1]],
                newdata = data.frame(CumStressBM2 = c(0, 1))),
        times = c(30, 50, 70, 96))


tmpb <- do.call(rbind, lapply(seq_along(m1.m4out2[[1]]$Model), function(iter) {
tmp <- summary(survfit(m1.m4out2[[1]]$Model[[iter]],
                newdata = data.frame(CumStressBM2 = c(0, 1))),
               times = 30:96)
rbind(
  data.table(
  Imp = iter,
  Age = tmp$time,
  Survival = tmp$surv[, 1],
  Stress = "Low"),
  data.table(
  Imp = iter,
  Age = tmp$time,
  Survival = tmp$surv[, 2],
  Stress = "High"))
}))

## ggplot(tmp[Stress == "High"], aes(Age, Survival, group = Imp)) +
##   geom_line(alpha = .2)

tmp2b <- tmpb[, .(
  Survival = mean(Survival, na.rm=TRUE)),
  by = .(Age, Stress)]
tmp2b[, Stress := factor(
         Stress, levels = c("Low", "High"),
         labels = c("Lower Quartile", "Upper Quartile"))]


pdf(file = file.path(dir.proj, "predicted_survival_plot.pdf"),
    width = 6, height = 9)

plot_grid(
ggplot(tmp2[Age %gele% c(50, 85)], aes(Age, Survival,
                            linetype = Stress)) +
  geom_line(size = 1.5) +
  scale_linetype("Multi-dimensional Stress") +
  scale_y_continuous(breaks = seq(.5, 1, .1),
                     labels = sprintf("%d%%", seq(50, 100, 10))) +
  theme(legend.position = "none",
        legend.key.width = unit(2, "cm")) +
  coord_cartesian(
    xlim = c(50, 85),
    ylim = c(.5, 1),
    expand = FALSE) + ggtitle("MIDUS 1"),
ggplot(tmp2b[Age %gele% c(50, 85)], aes(Age, Survival,
                            linetype = Stress)) +
  geom_line(size = 1.5) +
  scale_linetype("Multi-dimensional Stress") +
  scale_y_continuous(breaks = seq(.5, 1, .1),
                     labels = sprintf("%d%%", seq(50, 100, 10))) +
  theme(legend.position = c(.03, .15),
        legend.key.width = unit(2, "cm")) +
  coord_cartesian(
    xlim = c(50, 85),
    ylim = c(.5, 1),
    expand = FALSE) + ggtitle("MIDUS 2"), ncol = 1)

dev.off()








m2.all.hr <- rbind(
  cbind(m2[[1]]$HRs, Type = "Multi-dimensional"),
  cbind(m2[[2]]$HRs, Type = "Discrimination"),
  cbind(m2[[3]]$HRs, Type = "Inequality"),
  cbind(m2[[4]]$HRs, Type = "Social Strain"),
  cbind(m2[[5]]$HRs, Type = "Perceived"),
  cbind(m2[[6]]$HRs, Type = "Life Events"))
m2.all.hr[, Type := factor(Type,
  levels = c("Multi-dimensional", "Discrimination",
              "Inequality", "Social Strain",
              "Perceived", "Life Events"))]
all.hr <- rbind(
  cbind(m1.all.hr, Wave = "MIDUS 1"),
  cbind(m2.all.hr, Wave = "MIDUS 2"))

## obtained precisions
all.hr[!grepl("Complete", Model)][,
  .(D = median(log(UL) - log(LL))), by = .(Type, Wave)]

p.all.hr <- ggplot(all.hr,
       aes(rev(Order), log(HR), ymin = log(LL), ymax = log(UL), colour = Type, shape = Type)) +
  geom_pointrange(position = jit) +
  scale_colour_manual("Stress Measure", values = vpal) +
  scale_shape_discrete("Stress Measure") +
  scale_x_continuous(breaks = 1:9, labels = rev(m2[[1]]$HRs$Model)) +
  scale_y_continuous(
    "Hazard Ratio with 95% Confidence Interval",
    breaks = log(c(1, 1.1, 1.2, 1.3, 1.4, 1.6, 1.8, 2)),
    labels = c("1.0", "1.1", "1.2", "1.3", "1.4", "1.6", "1.8", "2.0")) +
  coord_flip(xlim = c(.2, 9.8), ylim = log(c(.94, 2.0)), expand = FALSE) +
  facet_wrap(~Wave, ncol = 1)+
  theme(
    panel.grid.major.x = element_line(colour = "grey95"),
    axis.line.y = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    axis.title.y = element_blank(),
    plot.margin=unit(c(.2,.4,.2,.4),"cm"))

pdf(file = file.path(dir.proj, "all_hr_plot.pdf"), width = 7, height = 10)
print(p.all.hr)
dev.off()


all.hr2 <- copy(all.hr)
all.hr2 <- all.hr2[!grepl("Covariate", Model)]
all.hr2[, Model := gsub(", MSM", "", Model)]
all.hr2[Order == 5, Order := 4]
all.hr2[Order == 7, Order := 5]

p.all.hr2 <- ggplot(all.hr2,
  aes(rev(Order) - (as.integer(Type) - 3.5) / 6,
      log(HR), ymin = log(LL), ymax = log(UL), colour = Type, shape = Type,
      size = as.numeric(Type=="Multi-dimensional"))) +
  geom_hline(yintercept = log(1)) +
  ## geom_segment(aes(xend = rev(Order) - (as.integer(Type) - 3.5) / 6, y = log(LL),
  ##                  size = as.numeric(Type=="Multi-dimensional"))) +
  ## geom_point(size = 1.6) +
  geom_pointrange(position = "identity") +
  scale_size_continuous(range = c(.6, 1), guide=FALSE) +
  scale_colour_manual("Stress Measure", values = vpal) +
  scale_shape_discrete("Stress Measure") +
  scale_x_continuous(breaks = 1:5,
                     labels = rev(all.hr2[Wave=="MIDUS 1" & Type == "Multi-dimensional", Model])) +
  scale_y_continuous(
    "Hazard Ratio with 95% Confidence Interval",
    breaks = log(c(1, 1.1, 1.2, 1.3, 1.4, 1.6, 1.8, 2)),
    labels = c("1.0", "1.1", "1.2", "1.3", "1.4", "1.6", "1.8", "2.0")) +
  coord_flip(xlim = c(.2, 5.8), ylim = log(c(.94, 2.0)), expand = FALSE) +
  facet_wrap(~Wave, scales = "free_x", ncol = 1)+
  theme(
    panel.grid.major.x = element_line(colour = "grey95"),
    axis.line.y = element_blank(),
    legend.position = c(1, .9),
    legend.justification = c(1, 1),
    axis.title.y = element_blank(),
    plot.margin=unit(c(.2,.4,.2,.4),"cm"))

pdf(file = file.path(dir.proj, "all_hr2_plot.pdf"), width = 6.5, height = 8)
print(p.all.hr2)
dev.off()


m1.all.nnt <- data.table(
  Age = rep(m1[[1]]$NNT$Age, 4),
  Type = factor(rep(c("Multi-dimensional", "Discrimination", "Inequality", "Social Strain"),
                    each = length(m1[[1]]$NNT$Age)),
                levels = c("Multi-dimensional", "Discrimination", "Inequality", "Social Strain",
                           "Perceived", "Life Events")),
  NNT = c(
    m1[[1]]$NNT$NNT,
    m1[[2]]$NNT$NNT,
    m1[[3]]$NNT$NNT,
    m1[[4]]$NNT$NNT))[Age %gele% c(50, 87)]

m2.all.nnt <- data.table(
  Age = rep(m2[[1]]$NNT$Age, 6),
  Type = factor(rep(c("Multi-dimensional", "Discrimination", "Inequality", "Social Strain",
               "Perceived", "Life Events"), each = length(m2[[1]]$NNT$Age)),
               levels = c("Multi-dimensional", "Discrimination", "Inequality", "Social Strain",
                          "Perceived", "Life Events")),
  NNT = c(
    m2[[1]]$NNT$NNT,
    m2[[2]]$NNT$NNT,
    m2[[3]]$NNT$NNT,
    m2[[4]]$NNT$NNT,
    m2[[5]]$NNT$NNT,
    m2[[6]]$NNT$NNT))[Age %gele% c(50, 87)]

all.nnt <- rbind(
  cbind(m1.all.nnt, Wave = "MIDUS 1"),
  cbind(m2.all.nnt, Wave = "MIDUS 2"))

p.all.nnt <- ggplot(all.nnt,
  aes(Age, NNT, linetype = Type, colour = Type)) +
  geom_line() +
  scale_y_continuous("Number Needed to Treat",
                     breaks = c(25, 50, 100, 200, 400, 600, 1000, 2000, 3000)) +
  scale_linetype_discrete("Stress Measure") +
  scale_colour_manual("Stress Measure", values = vpal) +
  theme(
    panel.grid.major = element_line(colour = "grey95", size = .2),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.key.width = unit(1, "cm")) +
  coord_trans(y = "log10",
              limx = c(50, 80),
              limy = c(25, 3000)) +
  facet_wrap(~Wave, ncol = 1) +
  NULL


pdf(file = file.path(dir.proj, "all_nnt_plot.pdf"), width = 6, height = 9)
print(p.all.nnt)
dev.off()



runStress <- function(stressvar, cov, wave = c(0, 1), binary = FALSE, pcut = .75, saveout = FALSE) {

  binary <- isTRUE(binary)

  if (binary) {
    dwclean.use[[stressvar]] <- as.integer(dwclean.use[[stressvar]] >= quantile(
      dwclean.use[[stressvar]], probs = pcut, na.rm = TRUE))
  }

m <- coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)),
    data = subset(dwclean.use, PAfterM2 %in% wave))

hr <- data.table(
  Model = "Model 1, Complete Case",
  Order = 1,
  HR = exp(coef(m)),
  LL = exp(confint(m))[,1],
  UL = exp(confint(m))[,2],
  P = coef(summary(m))[, "Pr(>|z|)"])

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }

  dat <- subset(dat, PAfterM2 %in% wave)

  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)),
    data = dat)
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 1, Imputed",
  Order = 2,
  HR = exp(m[, "estimate"]),
  LL = exp(m[, "2.5 %"]),
  UL = exp(m[, "97.5 %"]),
  P = m[, "p.value"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = FAM,
    link = LIN,
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[1]])),
      FAM = ifelse(binary, "binomial", "gaussian"),
      LIN = ifelse(binary, "logit", "identity"))))

  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)),
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 2, MSM",
  Order = 3,
  HR = exp(m[, "estimate"]),
  LL = exp(m[, "2.5 %"]),
  UL = exp(m[, "97.5 %"]),
  P = m[, "p.value"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[1]], stressvar)), data = dat)
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 2, Covariates",
  Order = 4,
  HR = exp(m[nrow(m), "estimate"]),
  LL = exp(m[nrow(m), "2.5 %"]),
  UL = exp(m[nrow(m), "97.5 %"]),
  P = m[nrow(m), "p.value"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = FAM,
    link = LIN,
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[2]])),
      FAM = ifelse(binary, "binomial", "gaussian"),
      LIN = ifelse(binary, "logit", "identity"))))

  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)),
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 3, MSM",
  Order = 5,
  HR = exp(m[, "estimate"]),
  LL = exp(m[, "2.5 %"]),
  UL = exp(m[, "97.5 %"]),
  P = m[, "p.value"]))


m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[2]], stressvar)), data = dat)
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 3, Covariates",
  Order = 6,
  HR = exp(m[nrow(m), "estimate"]),
  LL = exp(m[nrow(m), "2.5 %"]),
  UL = exp(m[nrow(m), "97.5 %"]),
  P = m[nrow(m), "p.value"]))


m <- summary(pool(as.mira(m4save <- lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = FAM,
    link = LIN,
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])),
      FAM = ifelse(binary, "binomial", "gaussian"),
      LIN = ifelse(binary, "logit", "identity"))))

  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s", stressvar)),
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))), conf.int = TRUE)

if (!saveout) {
  rm(m4save)
}

hr <- rbind(hr, data.table(
  Model = "Model 4, MSM",
  Order = 7,
  HR = exp(m[, "estimate"]),
  LL = exp(m[, "2.5 %"]),
  UL = exp(m[, "97.5 %"]),
  P = m[, "p.value"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)
  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[3]], stressvar)), data = dat)
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 4, Covariates",
  Order = 8,
  HR = exp(m[nrow(m), "estimate"]),
  LL = exp(m[nrow(m), "2.5 %"]),
  UL = exp(m[nrow(m), "97.5 %"]),
  P = m[nrow(m), "p.value"]))

m <- summary(pool(as.mira(lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }
  dat <- subset(dat, PAfterM2 %in% wave)

  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = FAM,
    link = LIN,
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])),
      FAM = ifelse(binary, "binomial", "gaussian"),
      LIN = ifelse(binary, "logit", "identity"))))

  coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ cluster(M2FAMNUM) + %s + %s", cov[[3]], stressvar)),
        data = dat, weights = winsorizor(w$ipw.weights, .01))
}))), conf.int = TRUE)

hr <- rbind(hr, data.table(
  Model = "Model 4, MSM + Covariates",
  Order = 9,
  HR = exp(m[nrow(m), "estimate"]),
  LL = exp(m[nrow(m), "2.5 %"]),
  UL = exp(m[nrow(m), "97.5 %"]),
  P = m[nrow(m), "p.value"]))


p <- ggplot(hr, aes(Order)) +
  geom_pointrange(aes(y = HR, ymin = LL, ymax = UL)) +
  scale_x_continuous(breaks = 1:9, labels = hr$Model) +
  geom_hline(yintercept = 1, colour = "grey50") +
  coord_flip() +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank())

## Effect Size
temp <- lapply(dwcleanimp, function(dat) {
  if (binary) {
    dat[[stressvar]] <- as.integer(dat[[stressvar]] >= quantile(
      dat[[stressvar]], probs = pcut, na.rm = TRUE))
  }

  dat <- subset(dat, PAfterM2 %in% wave)
  ## weights
  w <- eval(substitute(ipwpoint(
    exposure = EXP,
    family = FAM,
    link = LIN,
    numerator = ~ 1,
    denominator = COVS,
    data = dat), list(
      EXP = as.symbol(stressvar),
      COVS = as.formula(sprintf("~ %s", cov[[3]])),
      FAM = ifelse(binary, "binomial", "gaussian"),
      LIN = ifelse(binary, "logit", "identity"))))

  m <- coxph(as.formula(sprintf(
    "Surv(LastAge, DECEASED) ~ %s", stressvar)),
    data = dat, weights = winsorizor(w$ipw.weights, .01),
    ties = "breslow")

  ps <- eval(substitute(
    stdCoxph(m, data = as.data.frame(dat),
                 X = stressvar,
                 x = c(NA, ifelse(binary, 0, quantile(dat[[stressvar]], probs = .75))),
                 t = 49:88, clusterid = "M2FAMNUM",
                 subsetnew = EXP > ifelse(BIN, 0, quantile(EXP, probs = .75))),
    list(EXP = as.symbol(stressvar),
         BIN = binary)))

  NNT(ps$est)
})

nnt.m4 <- data.table(
  Age = 49:88,
  NNT = Reduce(`+`, temp)/50)

  if (saveout) {
    return(list(
      HRs = hr,
      Plot = p,
      NNT = nnt.m4,
      Model = m4save))
  } else {
    return(list(
      HRs = hr,
      Plot = p,
      NNT = nnt.m4))
  }
}
