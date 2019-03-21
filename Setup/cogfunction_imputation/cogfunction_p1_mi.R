aws <- FALSE

library(checkpoint)
checkpoint("2017-05-01", R.version = "3.3.3")


library(data.table)
library(JWileymisc)
library(mice)
library(parallel)
library(randomForest)

options(stringsAsFactors = FALSE)


################################################################################
##                             Create Imputed Data                            ##
################################################################################
dwclean <- readRDS("../midus_merged_data_dwclean.RDS")
psr_pana_bi.data <- readRDS("../midus_merged_data_psrpanabi_imp.RDS")

dwcleanimp <- lapply(1:50, function(i) {
  merge(dwclean, psr_pana_bi.data[[i]], by = "M2ID", all = TRUE)
})

v <- list(
  stress = c(
    ## M1
    "A1SSPCRI", "A1SKINNE", "A1SFDSNE",
    "A1SLFEDI", "A1SDAYDI", "A1SPIWOR", "A1SPIFAM", "A1SHOMET", "A1SPIHOM",
    "A1ParentAbuse", "A1SiblingAbuse",
    ## M2 (Retrospective)
    "B1ChildLifeStress", "B4CTQTotal",
    "B1LifeStress", "B1LifeStressImpactS", "B1LifeStressImpactL",
    ## M2
    "B1SJOBDI", "B1SPIWOR", "B1SHOMET", "B1SPIHOM",
    "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
    "B1SLFEDI", "B1SDAYDI", "B4QPS_PS"
  ),
  health.cat = c(
  "A1PA4", "A1PA5", "A1PA6",
  "B1PA1", "B1PA2", "B1PA3",
  "B1SA7B", "B1SA7D", "C1SA7B", "C1SA7D",
  "A1PDEPDX", "A1PANXTD",
  "B1PDEPDX", "B1PANXTD"
  ),
  health = c(
    "A1SCHRON", "B1SCHRON", "C1SCHRON", #"p4majorconditions",
    "B3TCOMPZ3", "B3TEMZ3", "B3TEFZ3", "C3TCOMPZ", "C3TEMZ", "C3TEFCZ"
  ),
  cov = c(
    "A1SBMI", "B1SBMI", "C1SBMI",
    "BirthYear", "AGEM1", "AGEM2",
    ## health behaviors
    "A1PhysAct", "B1PhysAct", "C1PhysAct",
    "B1Sleep", "B1SSQ", "C1Sleep", "C1SSQ",
    ## SES
    "m1chadv", "m2aadv"
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
  noimp = c("M2ID", "M2FAMNUM")
)

## exclude any MIDUS participants who are missing ALL stress variables
nrow(dwcleanimp[[1]]) ## total
## useIDs <- dwclean[rowSums(is.na(dwcleanimp[[1]][, c(v$stress), with = FALSE])) < length(c(v$stress)), M2ID]
useIDs <- dwcleanimp[[1]][rowSums(is.na(dwcleanimp[[1]][, c(v$stress, v$health), with = FALSE])) < length(c(v$stress, v$health)), M2ID]
length(useIDs) ## after excluding those missing all stress measures

xtabs(~I(M2ID %in% useIDs) + SAMPLMAJ, data = dwclean)

for (i in seq_along(dwcleanimp)) {
  dwcleanimp[[i]] <- dwcleanimp[[i]][M2ID %in% useIDs][, unlist(v), with = FALSE]
#  dwcleanimp[[i]][, B4S9 := factor(as.integer(B4S9 == 4))]
  dwcleanimp[[i]][, c(v$cov.cat, v$health.cat) := lapply(.SD, as.factor), .SDcols = c(v$cov.cat, v$health.cat)]
}



if (TRUE) {

  if (!aws) {

    ## need to limit due to memory constraints
    cl <- makeCluster(10)
    ## registerDoParallel(cl)
    clusterEvalQ(cl, {
      library(checkpoint)
      checkpoint("2017-05-01", R.version = "3.3.3")
      library(mice)
      library(randomForest)
      ## library(caret)
      ## library(ipred)
    })

  }

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

  ## dwcleanimp <- list(readRDS(file.path(tmpdir, paste0("cogfunc_", 1, ".RDS"))))
  noimpdex <- which(colnames(dwcleanimp[[1]]) %in% c("M2ID", "M2FAMNUM"))

  pmat <- (1 - diag(1, ncol(dwcleanimp[[1]])))
  colnames(pmat) <- rownames(pmat) <- colnames(dwcleanimp[[1]])
  ## do not impute ID, family number, or last age
  pmat[noimpdex, ] <- 0
  pmat[, noimpdex] <- 0

  if (aws) {
    imputed <- mclapply(X = 1:50, FUN = function(i) {
      set.seed(imputation_seeds[i])
      ## now do the imputation
      out <- mice(
        as.data.frame(dwcleanimp[[i]]),
        m = 1,
        method = "rf",
        predictorMatrix = pmat,
        maxit = 30,
        seed = imputation_seeds[i],
        ntree = 10,
        nodesize = 10
      )
      saveRDS(out, file = sprintf("midus_cogfunc_imputed_%s.RDS", as.character(i)), compress = "xz")
      return(out)
    }, mc.cores = 50)

  } else {

    ## tmpdir <- tempdir()
    tmpdir <- path.expand("~/Desktop/rtmp")
    ## lapply(1:50, function(i) {
    ##   saveRDS(as.data.frame(dwcleanimp[[i]]), file = file.path(tmpdir, paste0("cogfunc_", i, ".RDS")))
    ## })

    clusterExport(cl, c("tmpdir", "imputation_seeds", "pmat"))

    imputed <- parLapplyLB(cl, 1:50, function(i) {
      set.seed(imputation_seeds[i])
      ## now do the imputation
      out <- mice(
        readRDS(file.path(tmpdir, paste0("cogfunc_", i, ".RDS"))),
        m = 1,
        method = "rf",
        predictorMatrix = pmat,
        maxit = 30,
        seed = imputation_seeds[i],
        ntree = 10,
        nodesize = 10
      )
      saveRDS(out, file = sprintf("midus_cogfunc_imputed_%s.RDS", as.character(i)), compress = "xz")
      return(out)
    })
  }

  saveRDS(imputed, file = "midus_cogfunc_imputed.RDS", compress = "xz")

  final_imputed <- lapply(imputed, mice::complete, 1)
  saveRDS(final_imputed, file = "midus_cogfunc_final_imputed.RDS", compress = "xz")
}
