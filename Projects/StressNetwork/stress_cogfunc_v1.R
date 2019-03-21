source("../MIDUS-Setup/setup_packages_functions.R")
options(stringsAsFactors = FALSE, digits = 3)
setMKLthreads(1)

r2.gls <- function(object) {
  cor(fitted(object), fitted(object) + residuals(object))^2
}

pool.r2.gls <- function(obj, obj2) {
  if (missing(obj2)) {
    tanh(mean(atanh(sapply(obj, r2.gls))))
  } else {
    tanh(mean(
      atanh(sapply(obj, r2.gls)) -
      atanh(sapply(obj2, r2.gls))))
  }
}

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

pool.r2pval.gls <- function(obj1, obj2, label = "") {
  res <- summary(mice::pool(as.mira(obj1)))

  if (missing(obj2)) {
    r2 <- pool.r2.gls(obj1)
    tmp <- pool.compare.null(as.mira(obj1))
    use <- (1:nrow(res))
  } else {
    r2 <- pool.r2.gls(obj1, obj2)
    tmp <- pool.compare(as.mira(obj1), as.mira(obj2))
    use <- (nrow(res) - tmp$df1) : nrow(res)
  }

  v <- rownames(res)[use]
  b <- as.vector(res[, "est"])[use]
  se <- as.vector(res[, "se"])[use]
  bpval <- as.vector(res[, "Pr(>|t|)"])[use]
  LL <- as.vector(res[, "lo 95"])[use]
  UL <- as.vector(res[, "hi 95"])[use]

  wald <- sprintf("Chi2 (df = %0.0f) = %0.2f", tmp$df1, tmp$Dm[1,1])
  pval <- formatPval(tmp$pvalue[1,1], d = 3, includeP = TRUE)

  data.table(
    Model = label,
    Variable = c(v, "Overall"),
    Est = c(b, r2),
    SE = c(se, NA_real_),
    LL = c(LL, NA_real_),
    UL = c(UL, NA_real_),
    P = c(bpval, tmp$pvalue[1,1]),
    DF = c(rep(1, length(use)), tmp$df1)
    )
}


dwcleanimp <- readRDS("~/OneDrive/Projects/MIDUS-Setup/cogfunction_imputation/midus_cogfunc_final_imputed.RDS")

dwcleanimp <- lapply(dwcleanimp, function(d) {
  d <- within(d, {
    A1PB1 <- as.integer(as.character(A1PB1))
    B1PB1 <- as.integer(as.character(B1PB1))
    B1SA7D <- as.integer(as.character(B1SA7D))
    C1SA7D <- as.integer(as.character(C1SA7D))
    A1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
    B1PBWORK <- as.integer(A1PBWORK %in% c(1, 2))
    B1SleepDuration <- factor(cut(B1Sleep, breaks = c(-Inf, 6, 9, Inf), include.lowest = TRUE, labels = c("Short", "Normal", "Long")),
                              levels = c("Normal", "Short", "Long"))
    C1SleepDuration <- factor(cut(C1Sleep, breaks = c(-Inf, 6, 9, Inf), include.lowest = TRUE, labels = c("Short", "Normal", "Long")),
                              levels = c("Normal", "Short", "Long"))
  })
  d <- within(d, {
    A1PB1 <- recode(A1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
    B1PB1 <- recode(B1PB1, "c(1, 2, 3, 4) = '< HS'; 5 = 'HS'; c(6, 7, 8) = 'Some College'; c(9, 10, 11, 12) = 'College Degree+';", as.factor.result = TRUE)
  })
  return(d)
})

use.cov.m1m2 <- list(
 c("AGEM1", "Sex", "RaceG3", "m1ped3_all", "m1welf_all", "B1SA7D"),
 c("A1PB1", "A1PBWORK"),
 c("B1Smoke", "B1CurrentAlcohol", "B1PhysAct", "B1SBMI", "B1SleepDuration", "B1SSQ"))
use.cov.m1m2 <- lapply(1:length(use.cov.m1m2), function(i) {
  x <- unique(unlist(use.cov.m1m2[1:i]))
  x[nzchar(x)]
})

use.cov.m1m3.temz <- list(
 c("AGEM1", "Sex", "RaceG3", "m1ped3_all", "m1welf_all", "C1SA7D", "B3TEMZ3"),
 c("A1PB1", "A1PBWORK"),
 c("C1Smoke", "C1CurrentAlcohol", "C1PhysAct", "C1SBMI", "C1SleepDuration", "C1SSQ"))
use.cov.m1m3.temz <- lapply(1:length(use.cov.m1m3.temz), function(i) {
  x <- unique(unlist(use.cov.m1m3.temz[1:i]))
  x[nzchar(x)]
})

use.cov.m1m3.tefz <- list(
 c("AGEM1", "Sex", "RaceG3", "m1ped3_all", "m1welf_all", "C1SA7D", "B3TEFZ3"),
 c("A1PB1", "A1PBWORK"),
 c("C1Smoke", "C1CurrentAlcohol", "C1PhysAct", "C1SBMI", "C1SleepDuration", "C1SSQ"))
use.cov.m1m3.tefz <- lapply(1:length(use.cov.m1m3.tefz), function(i) {
  x <- unique(unlist(use.cov.m1m3.tefz[1:i]))
  x[nzchar(x)]
})

use.cov.m2m3.temz <- list(
 c("AGEM1", "Sex", "RaceG3", "m1ped3_all", "m1welf_all", "C1SA7D", "B3TEMZ3"),
 c("B1PB1", "B1PBWORK"),
 c("C1Smoke", "C1CurrentAlcohol", "C1PhysAct", "C1SBMI", "C1SleepDuration", "C1SSQ"))
use.cov.m2m3.temz <- lapply(1:length(use.cov.m2m3.temz), function(i) {
  x <- unique(unlist(use.cov.m2m3.temz[1:i]))
  x[nzchar(x)]
})

use.cov.m2m3.tefz <- list(
 c("AGEM1", "Sex", "RaceG3", "m1ped3_all", "m1welf_all", "C1SA7D", "B3TEFZ3"),
 c("B1PB1", "B1PBWORK"),
 c("C1Smoke", "C1CurrentAlcohol", "C1PhysAct", "C1SBMI", "C1SleepDuration", "C1SSQ"))
use.cov.m2m3.tefz <- lapply(1:length(use.cov.m2m3.tefz), function(i) {
  x <- unique(unlist(use.cov.m2m3.tefz[1:i]))
  x[nzchar(x)]
})



m1.stress.vars <- list("",
  c("A1SLFEDI", "A1SDAYDI"),
  c("A1SPIWOR", "A1SPIHOM", "A1SPIFAM"),
  c("A1SKINNE", "A1SFDSNE", "A1SSPCRI"))
m1.stress.vars <- c(m1.stress.vars, list(unique(unlist(m1.stress.vars))[nzchar(unique(unlist(m1.stress.vars)))]))

m2.stress.vars <- list("",
  c("B1SLFEDI", "B1SDAYDI", "B1SJOBDI"),
  c("B1SPIWOR", "B1SPIHOM", "B1SPIFAM"),
  c("B4QPS_PS", "B1SKINNE", "B1SFDSNE", "B1SSPCRI"),
  "B1LifeStress")
m2.stress.vars <- c(m2.stress.vars, list(unique(unlist(m2.stress.vars))[nzchar(unique(unlist(m2.stress.vars)))]))

dwc <- readRDS("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data_dwclean.RDS")[M2ID %in% dwcleanimp[[1]]$M2ID]
m1m2scvars <- unique(unlist(c(m1.stress.vars, "B3TEMZ3", "B3TEFZ3")))
m1m2scvars <- m1m2scvars[nzchar(m1m2scvars)]
m1m3scvars <- unique(unlist(c(m1.stress.vars, #"B3TEMZ3", "B3TEFZ3",
                              "C3TEMZ", "C3TEFCZ")))
m1m3scvars <- m1m3scvars[nzchar(m1m3scvars)]
m2m3scvars <- unique(unlist(c(m2.stress.vars, #"B3TEMZ3", "B3TEFZ3",
                              "C3TEMZ", "C3TEFCZ")))
m2m3scvars <- m2m3scvars[nzchar(m2m3scvars)]

m1m2usedex <- rowSums(is.na(dwc[, m1m2scvars, with=FALSE])) < length(m1m2scvars)
m1m3usedex <- rowSums(is.na(dwc[, m1m3scvars, with=FALSE])) < length(m1m3scvars)
m2m3usedex <- rowSums(is.na(dwc[, m2m3scvars, with=FALSE])) < length(m2m3scvars)

table(m1m2usedex)
table(m1m3usedex)
table(m2m3usedex)


cl <- makeCluster(10)
clusterExport(cl, c("m1.stress.vars", "m2.stress.vars", "use.cov.m1m2",
                    "use.cov.m1m3.temz", "use.cov.m1m3.tefz",
                    "use.cov.m2m3.temz", "use.cov.m2m3.tefz",
                    "m1m2usedex", "m1m3usedex", "m2m3usedex",
                    "dwcleanimp", "pool.r2.gls", "r2.gls"))
clusterEvalQ(cl, {
  library(checkpoint)
  checkpoint("2017-05-26", R.version = "3.4.0")
  library(mice)
  library(nlme)
  setMKLthreads(1)
  ## library(data.table)
})

## cognitive function .variables
res.m1m2 <- lapply(c("B3TEMZ3", "B3TEFZ3"), function(dv) {
  lapply(m1.stress.vars, function(iv) {
    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }
    f <- lapply(use.cov.m1m2, function(cov) {
      usecovs <- paste(cov, collapse = " + ")
      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1
      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })
    env <- environment()
    clusterExport(cl, "f", envir = env)
    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[m1m2usedex, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })
    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
})
## saveRDS(res.m1m2, file = "~/Desktop/res_m1m2.RDS")
res.m1m2.temz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(M = i, rbind(
    pool.r2pval.gls(res.m1m2[[1]][[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m1m2[[1]][[2]][[1]][[i]], res.m1m2[[1]][[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m1m2[[1]][[3]][[1]][[i]], res.m1m2[[1]][[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m1m2[[1]][[4]][[1]][[i]], res.m1m2[[1]][[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m1m2[[1]][[5]][[1]][[i]], res.m1m2[[1]][[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m1m2.temz.final, file = "res_m1m2_temz_final.RDS", compress = "xz")
res.m1m2.tefz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(M = i, rbind(
    pool.r2pval.gls(res.m1m2[[2]][[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m1m2[[2]][[2]][[1]][[i]], res.m1m2[[2]][[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m1m2[[2]][[3]][[1]][[i]], res.m1m2[[2]][[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m1m2[[2]][[4]][[1]][[i]], res.m1m2[[2]][[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m1m2[[2]][[5]][[1]][[i]], res.m1m2[[2]][[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m1m2.tefz.final, file = "res_m1m2_tefz_final.RDS", compress = "xz")
rm(res.m1m2)
## res.m1m2 <- readRDS("~/Desktop/res_m1m2.RDS")


res.m1m3.temz <- lapply(m1.stress.vars, function(iv) {
  dv <- "C3TEMZ"
    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }
    f <- lapply(use.cov.m1m3.temz, function(cov) {
      usecovs <- paste(cov, collapse = " + ")
      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1
      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })
    env <- environment()
    clusterExport(cl, "f", envir = env)
    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[m1m3usedex, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })
    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
## saveRDS(res.m1m3.temz, file = "~/Desktop/res_m1m3_temz.RDS")
res.m1m3.temz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(Model = i, rbind(
    pool.r2pval.gls(res.m1m3.temz[[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m1m3.temz[[2]][[1]][[i]], res.m1m3.temz[[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m1m3.temz[[3]][[1]][[i]], res.m1m3.temz[[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m1m3.temz[[4]][[1]][[i]], res.m1m3.temz[[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m1m3.temz[[5]][[1]][[i]], res.m1m3.temz[[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m1m3.temz.final, file = "res_m1m3_temz_final.RDS", compress = "xz")
rm(res.m1m3.temz)
## res.m1m3.temz <- readRDS("~/Desktop/res_m1m3_temz.RDS")



res.m1m3.tefz <- lapply(m1.stress.vars, function(iv) {
  dv <- "C3TEFCZ"
    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }
    f <- lapply(use.cov.m1m3.tefz, function(cov) {
      usecovs <- paste(cov, collapse = " + ")
      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1
      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })
    env <- environment()
    clusterExport(cl, "f", envir = env)
    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[m1m3usedex, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })
    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
})
## saveRDS(res.m1m3.tefz, file = "~/Desktop/res_m1m3_tefz.RDS")
res.m1m3.tefz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(Model = i, rbind(
    pool.r2pval.gls(res.m1m3.tefz[[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m1m3.tefz[[2]][[1]][[i]], res.m1m3.tefz[[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m1m3.tefz[[3]][[1]][[i]], res.m1m3.tefz[[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m1m3.tefz[[4]][[1]][[i]], res.m1m3.tefz[[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m1m3.tefz[[5]][[1]][[i]], res.m1m3.tefz[[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m1m3.tefz.final, file = "res_m1m3_tefz_final.RDS", compress = "xz")
rm(res.m1m3.tefz)
## res.m1m3.tefz <- readRDS("~/Desktop/res_m1m3_tefz.RDS")


res.m2m3.temz <- lapply(m2.stress.vars, function(iv) {
  dv <- "C3TEMZ"
    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }
    f <- lapply(use.cov.m2m3.temz, function(cov) {
      usecovs <- paste(cov, collapse = " + ")
      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1
      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })
    env <- environment()
    clusterExport(cl, "f", envir = env)
    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[m2m3usedex, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })
    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
## saveRDS(res.m2m3.temz, file = "~/Desktop/res_m2m3_temz.RDS")
res.m2m3.temz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(Model = i, rbind(
    pool.r2pval.gls(res.m2m3.temz[[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m2m3.temz[[2]][[1]][[i]], res.m2m3.temz[[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m2m3.temz[[3]][[1]][[i]], res.m2m3.temz[[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m2m3.temz[[4]][[1]][[i]], res.m2m3.temz[[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m2m3.temz[[5]][[1]][[i]], res.m2m3.temz[[1]][[1]][[i]], label = "Covariates + Life Stress"),
    pool.r2pval.gls(res.m2m3.temz[[6]][[1]][[i]], res.m2m3.temz[[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m2m3.temz.final, file = "res_m2m3_temz_final.RDS", compress = "xz")
rm(res.m2m3.temz)
## res.m2m3.temz <- readRDS("~/Desktop/res_m2m3_temz.RDS")



res.m2m3.tefz <- lapply(m2.stress.vars, function(iv) {
  dv <- "C3TEFCZ"
    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }
    f <- lapply(use.cov.m2m3.tefz, function(cov) {
      usecovs <- paste(cov, collapse = " + ")
      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1
      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })
    env <- environment()
    clusterExport(cl, "f", envir = env)
    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[m2m3usedex, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })
    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
})
## saveRDS(res.m2m3.tefz, file = "~/Desktop/res_m2m3_tefz.RDS")
res.m2m3.tefz.final <- do.call(rbind, lapply(1:3, function(i) {
  cbind(M = i, rbind(
    pool.r2pval.gls(res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates Only"),
    pool.r2pval.gls(res.m2m3.tefz[[2]][[1]][[i]], res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates + Discrimination"),
    pool.r2pval.gls(res.m2m3.tefz[[3]][[1]][[i]], res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates + Inequality"),
    pool.r2pval.gls(res.m2m3.tefz[[4]][[1]][[i]], res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates + Perceived Stress"),
    pool.r2pval.gls(res.m2m3.tefz[[5]][[1]][[i]], res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates + Life Stress"),
    pool.r2pval.gls(res.m2m3.tefz[[6]][[1]][[i]], res.m2m3.tefz[[1]][[1]][[i]], label = "Covariates + All Stress")))
}))
saveRDS(res.m2m3.tefz.final, file = "res_m2m3_tefz_final.RDS", compress = "xz")
rm(res.m2m3.tefz)
## res.m2m3.tefz <- readRDS("~/Desktop/res_m2m3_tefz.RDS")






## cognitive function .variables
m.linear2 <- lapply(c("B3TEMZ3", "B3TEFZ3"), function(dv) {
  lapply(m2.stress.vars, function(iv) {

    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }

    f <- lapply(use.cov.m2stress, function(cov) {
      usecovs <- paste(cov, collapse = " + ")

      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1

      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })

    env <- environment()
    clusterExport(cl, "f", envir = env)

    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })

    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
})

## cognitive function .variables
m.linear3a <- lapply(c("C3TEMZ", "C3TEFCZ"), function(dv) {
  lapply(m1.stress.vars, function(iv) {

    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }

    f <- lapply(use.cov.m1stress, function(cov) {
      if (dv == "C3TEMZ") {
        cov <- c(cov, "B3TEMZ3")
      } else {
        cov <- c(cov, "B3TEFZ3")
      }
      cov <- cov[nzchar(cov)]
      usecovs <- paste(cov, collapse = " + ")

      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1

      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })

    env <- environment()
    clusterExport(cl, "f", envir = env)

    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })

    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
})

m.linear3b <- lapply(c("C3TEMZ", "C3TEFCZ"), function(dv) {
  lapply(m2.stress.vars, function(iv) {

    useivs <- paste(paste0("scale(", iv, ")"), collapse = " + ")
    if (length(iv) <= 1 && nchar(iv) <= 1) {
      useivs <- ""
    }

    f <- lapply(use.cov.m2stress, function(cov) {
      if (dv == "C3TEMZ") {
        cov <- c(cov, "B3TEMZ3")
      } else {
        cov <- c(cov, "B3TEFZ3")
      }
      cov <- cov[nzchar(cov)]
      usecovs <- paste(cov, collapse = " + ")

      hascov <- nchar(usecovs) > 1
      hasiv <- nchar(useivs) > 1

      if (hascov & hasiv) {
        sprintf("scale(%s) ~ %s + %s", dv, usecovs, useivs)
      } else if (hascov & !hasiv) {
        sprintf("scale(%s) ~ %s", dv, usecovs)
      } else if (!hascov & hasiv) {
        sprintf("scale(%s) ~ %s", dv, useivs)
      } else {
        sprintf("scale(%s) ~ 1", dv)
      }
    })

    env <- environment()
    clusterExport(cl, "f", envir = env)

    m <- parLapplyLB(cl, dwcleanimp, function(usedat) {
      ## m <- lapply(dwcleanimp, function(usedat) {
        lapply(f, function(form) {
        usedat <- na.omit(usedat[, unique(c(all.vars(as.formula(form)), "M2FAMNUM"))])
        mint <- tryCatch(eval(substitute(gls(model = zyx, data = usedat,
                             correlation = corCompSymm(form = ~ 1 | M2FAMNUM),
                             na.action = na.omit), list(zyx = as.formula(form)))),
                      error = function(e) e)
        if (inherits(mint, "error")) {
          mint <- NULL
        }
        return(mint)
      })
    })

    mok <- lapply(1:length(f), function(i) {
      tmp <- lapply(m, function(x) x[[i]])
      tmp[!sapply(tmp, is.null)]
    })
    return(list(imp = mok))
  })
})

## DV, IV, type (raw/imp), cov
summary(mice::pool(as.mira(m.linear[[1]][[1]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear[[1]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[1]][[1]]$imp[[3]])))

summary(mice::pool(m.linear[[1]][[1]]$imp[[2]]))

pool.r2.gls(m.linear[[1]][[1]]$imp[[2]]) ## dv 1, no stress, all covariates
pool.r2.gls(m.linear[[1]][[1]]$imp[[3]], m.linear[[1]][[1]]$imp[[2]]) ## dv 1, no stress, all covariates

summaryFormat <- function(object, ref) {
  n <- unique(c(unlist(m1.stress.vars), unlist(m2.stress.vars)))
  n <- n[nzchar(n)]
  tab <- summary(mice::pool(as.mira(object)))
  rn <- rownames(tab)
  index <- do.call(pmax, lapply(n, grepl, x = rn))==1
  tab <- tab[index, , drop=FALSE]
  out <- data.table(
    Stress = gsub("\\)", "", gsub("scale\\(", "", rn[index])),
    Est = sprintf("%0.2f [%0.2f, %0.2f], %s", tab[, "est"], tab[, "lo 95"], tab[, "hi 95"],
                  formatPval(tab[, "Pr(>|t|)"], 3, includeP=TRUE)))
  if (!missing(ref)) {
    wald <- pool.compare(as.mira(object), as.mira(ref))
    deltar2 <- pool.r2.gls(object, ref)
    out <- rbind(out,
                 data.table(Stress = "Overall",
                 Est = sprintf("Test = %0.2f (%0.0f, %0.1f), %s, R2 = %0.1f%%", wald$Dm[1,1], wald$df1[1], wald$df2[1],
                               formatPval(wald$pvalue[1,1], includeP = TRUE), deltar2 * 100)))
  }
  return(out)
}


res1atab <- rbind(
  summaryFormat(m.linear[[1]][[2]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[1]][[3]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[1]][[4]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[1]][[5]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[1]][[5]]$imp[[3]], m.linear[[1]][[1]]$imp[[3]]))

res1btab <- rbind(
  summaryFormat(m.linear[[2]][[2]]$imp[[2]], m.linear[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[2]][[3]]$imp[[2]], m.linear[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[2]][[4]]$imp[[2]], m.linear[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[2]][[5]]$imp[[2]], m.linear[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear[[2]][[5]]$imp[[3]], m.linear[[2]][[1]]$imp[[3]]))

res2atab <- rbind(
  summaryFormat(m.linear2[[1]][[2]]$imp[[2]], m.linear2[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[1]][[3]]$imp[[2]], m.linear2[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[1]][[4]]$imp[[2]], m.linear2[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[1]][[5]]$imp[[2]], m.linear2[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[1]][[6]]$imp[[2]], m.linear2[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[1]][[6]]$imp[[3]], m.linear2[[1]][[1]]$imp[[3]]))

res2btab <- rbind(
  summaryFormat(m.linear2[[2]][[2]]$imp[[2]], m.linear2[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[2]][[3]]$imp[[2]], m.linear2[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[2]][[4]]$imp[[2]], m.linear2[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[2]][[5]]$imp[[2]], m.linear2[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[2]][[6]]$imp[[2]], m.linear2[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear2[[2]][[6]]$imp[[3]], m.linear2[[2]][[1]]$imp[[3]]))

res3atab <- rbind(
  summaryFormat(m.linear3a[[1]][[2]]$imp[[2]], m.linear3a[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[1]][[3]]$imp[[2]], m.linear3a[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[1]][[4]]$imp[[2]], m.linear3a[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[1]][[5]]$imp[[2]], m.linear3a[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[1]][[5]]$imp[[3]], m.linear3a[[1]][[1]]$imp[[3]]))

res3btab <- rbind(
  summaryFormat(m.linear3a[[2]][[2]]$imp[[2]], m.linear3a[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[2]][[3]]$imp[[2]], m.linear3a[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[2]][[4]]$imp[[2]], m.linear3a[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[2]][[5]]$imp[[2]], m.linear3a[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3a[[2]][[5]]$imp[[3]], m.linear3a[[2]][[1]]$imp[[3]]))

res4atab <- rbind(
  summaryFormat(m.linear3b[[1]][[2]]$imp[[2]], m.linear3b[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[1]][[3]]$imp[[2]], m.linear3b[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[1]][[4]]$imp[[2]], m.linear3b[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[1]][[5]]$imp[[2]], m.linear3b[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[1]][[6]]$imp[[2]], m.linear3b[[1]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[1]][[6]]$imp[[3]], m.linear3b[[1]][[1]]$imp[[3]]))

res4btab <- rbind(
  summaryFormat(m.linear3b[[2]][[2]]$imp[[2]], m.linear3b[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[2]][[3]]$imp[[2]], m.linear3b[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[2]][[4]]$imp[[2]], m.linear3b[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[2]][[5]]$imp[[2]], m.linear3b[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[2]][[6]]$imp[[2]], m.linear3b[[2]][[1]]$imp[[2]]),
  summaryFormat(m.linear3b[[2]][[6]]$imp[[3]], m.linear3b[[2]][[1]]$imp[[3]]))

  summaryFormat(m.linear3b[[2]][[5]]$imp[[3]], m.linear3b[[2]][[1]]$imp[[3]])



summaryFormat(m.linear[[2]][[5]]$imp[[3]], m.linear[[2]][[1]]$imp[[3]])




pool.r2.gls(m.linear[[1]][[2]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]) * 100

summaryFormat(m.linear[[1]][[3]]$imp[[2]])
pool.r2.gls(m.linear[[1]][[3]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]) * 100

summaryFormat(m.linear[[1]][[4]]$imp[[2]])
pool.compare(as.mira(m.linear[[1]][[4]]$imp[[2]]), as.mira(m.linear[[1]][[1]]$imp[[2]]))
pool.r2.gls(m.linear[[1]][[4]]$imp[[2]], m.linear[[1]][[1]]$imp[[2]]) * 100



summary(mice::pool(as.mira(m.linear[[1]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[1]][[2]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[1]][[3]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[1]][[4]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[1]][[5]]$imp[[2]])))

summary(mice::pool(as.mira(m.linear[[2]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[2]][[2]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[2]][[3]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[2]][[4]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear[[2]][[5]]$imp[[2]])))

summary(mice::pool(as.mira(m.linear2[[1]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[1]][[2]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[1]][[3]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[1]][[4]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[1]][[5]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[1]][[6]]$imp[[2]])))

summary(mice::pool(as.mira(m.linear2[[2]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[2]][[2]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[2]][[3]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[2]][[4]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear2[[2]][[5]]$imp[[2]])))


summary(mice::pool(as.mira(m.linear3a[[1]][[1]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[2]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[3]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[4]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[5]]$imp[[1]])))

summary(mice::pool(as.mira(m.linear3a[[1]][[1]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[2]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[3]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[4]]$imp[[2]])))
summary(mice::pool(as.mira(m.linear3a[[1]][[5]]$imp[[2]])))

summary(mice::pool(as.mira(m.linear3b[[1]][[1]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3b[[1]][[2]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3b[[1]][[3]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3b[[1]][[4]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3b[[1]][[5]]$imp[[1]])))
summary(mice::pool(as.mira(m.linear3b[[1]][[6]]$imp[[1]])))


pool.compare(as.mira(m.linear3b[[2]][[1]]$imp[[2]]),
             as.mira(m.linear3b[[2]][[1]]$imp[[1]]))
[c("Dm", "rm", "df1", "df2", "pvalue")]


pool.compare(as.mira(m1f[[6]]),  as.mira(m1f[[2]]))[c("Dm", "rm", "df1", "df2", "pvalue")],

pool.r2.gls(m.linear[[2]][[1]]$raw, 2) ## all covariates


saveRDS(m.linear, file = "~/Desktop/cogtest.RDS", compress = "xz")

