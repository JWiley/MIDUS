aws <- TRUE

library(checkpoint)
checkpoint("2017-01-07", R.version = "3.3.2")

library(data.table)
library(JWileymisc)
library(mice)
library(parallel)
library(randomForest)
library(car)
## library(xlsx)

options(stringsAsFactors = FALSE)


################################################################################
##                             Create Imputed Data                            ##
################################################################################
dwclean <- readRDS("../midus_merged_data_dwclean.RDS")
psr_pana_bi.data <- readRDS("../midus_merged_data_psrpanabi_nosbpgluc_imp.RDS")

dwclean[, B4S9 := as.integer(B4S9 == 4)]
dwclean[, A1PBWORK := recode(A1PBWORK, "1 = 1; 2 = 2; c(3, 4) = '3,4'; 5 = 5; 6 = 6; c(7, 8, 11) = '7,8,11'; c(9,10) = '9,10';")]
dwclean[, B1PBWORK := recode(B1PBWORK, "1 = 1; 2 = 2; c(3, 4) = '3,4'; 5 = 5; 6 = 6; c(7, 8, 11) = '7,8,11'; c(9,10) = '9,10';")]
dwclean[, C1PB3WK := recode(C1PB3WK, "1 = 1; 2 = 2; c(3, 4) = '3,4'; 5 = 5; 6 = 6; c(7, 8, 11) = '7,8,11'; c(9,10) = '9,10';")]


dwcleanimp <- lapply(1:50, function(i) {
  merge(dwclean, psr_pana_bi.data[[i]], by = "M2ID", all = TRUE)
})

## mVars <- as.data.table(read.xlsx("../MIDUS-Setup/midus_master_vars.xlsx", sheetIndex = 1))

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
  "A1PDEPDX", "A1PANXTD",
  "B1PDEPDX", "B1PANXTD"
  ),
  health = c(
  "A1SCHRON", "B1SCHRON", "C1SCHRON", "p4majorconditions",
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
    "B4S9",
    ## education
    "A1PB1", "B1PB1", "C1PB1",
    ## work status
    "A1PBWORK", "B1PBWORK", "C1PB3WK",
    ## SES
    "m1welf_all", "m1ped3_all", "m1cses_all",
    "m2ed_all", "m2fpir3_all", "m2qf1_all", "m2qf6_all", "m2qf7_all"
  ),
  psr = c("B1PSRQSFPR", "B1PSRQSFSR"),
  noimp = c("M2ID", "M2FAMNUM")
)

nrow(dwcleanimp[[1]]) ## total
## only take those from M2 or Milwaukee, using the B1Smoke variable
useIDs <- dwcleanimp[[1]][!is.na(B1Smoke), M2ID]
length(useIDs) ## after excluding those missing all stress measures

dwclean.use <- copy(dwclean[M2ID %in% useIDs, setdiff(unlist(v), c("B4F",
              "B1NegAff", "B1PosAff", "B1PR", "B1SR",
              "B4bi.sym", "B4bi.hpa", "B4bi.card", "B4bi.gluc",
              "B4bi.lipid", "B4bi.infl", "B4bi.para")), with = FALSE])

for (i in seq_along(dwcleanimp)) {
  dwcleanimp[[i]] <- dwcleanimp[[i]][M2ID %in% useIDs][, unlist(v), with = FALSE]

  dwcleanimp[[i]][, c(v$cov.cat, v$health.cat) := lapply(.SD, as.factor), .SDcols = c(v$cov.cat, v$health.cat)]
}



if (TRUE) {

  if (!aws) {

    ## need to limit due to memory constraints
    cl <- makeCluster(6)
    ## registerDoParallel(cl)
    clusterEvalQ(cl, {
      library(checkpoint)
      checkpoint("2017-01-07", R.version = "3.3.2")
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

  noimpdex <- which(colnames(dwcleanimp[[1]]) %in% c("M2ID", "M2FAMNUM"))

  pmat <- (1 - diag(1, ncol(dwcleanimp[[1]])))
  colnames(pmat) <- rownames(pmat) <- colnames(dwcleanimp[[1]])
  ## do not impute ID, family number
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
      saveRDS(out, file = sprintf("midus_rcm_imputed_%s.RDS", as.character(i)))
      return(out)
    }, mc.cores = 50)

  } else {
    clusterExport(cl, c("dwcleanimp", "imputation_seeds", "pmat"))

    imputed <- parLapplyLB(cl, 1:50, function(i) {
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
      saveRDS(out, file = sprintf("midus_rcm_imputed_%s.RDS", as.character(i)))
      return(out)
    })
  }

  saveRDS(imputed, file = "midus_rcm_imputed.RDS")

  final_imputed <- lapply(imputed, mice::complete, 1)

saveRDS(final_imputed, file = "midus_rcm_final_imputed.RDS")
}
