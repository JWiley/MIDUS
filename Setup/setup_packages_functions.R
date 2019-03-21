
## downlaod randomForestSRC with OpenMP for multiple cores
## download.file("http://www.ccs.miami.edu/~hishwaran/rfsrc/randomForestSRC_2.0.7.zip",
##               destfile = "randomForestSRC_2.0.7.zip")

## source("~/OneDrive/codebits/mat_rel_ml/ml_rel_mat2.R")
## options(width = 150)

library(checkpoint)
checkpoint("2019-01-25", R.version = "3.5.1", use.knitr = TRUE)
## checkpoint("2017-05-01", R.version = "3.3.3")
## checkpoint("2017-01-07", R.version = "3.3.2")
## install.packages("randomForestSRC", repos = NULL)
## install.packages("randomForestSRC_2.0.7.zip", repos = NULL)

## ## rhdf5 needed for MplusAutomation to read in all .gh5 files from Mplus
## source("http://bioconductor.org/biocLite.R")
## biocLite("rhdf5")

library(lintr)
## for setup.R
library(devtools)
## install_github("JWiley/JWileymisc") ## first time only
library(DEoptimR)
library(foreign)
library(data.table)
library(bit64)
library(MplusAutomation)
library(reshape2)
library(gridExtra)
library(JWileymisc)
library(pscore)
library(ggplot2)
library(scales)
library(texreg)
library(survival)
library(ipw)
library(stdReg)
library(viridis)
library(xlsx)
library(varian)

library(survsim)
library(powerSurvEpi)

library(psych)
library(car)
library(mice)
library(parallel)
library(caret)
library(doParallel)
library(ipred)
library(mgcv)
## library(randomForest)
## library(randomForestSRC)
## library(ggRandomForests)
## library(pec)
library(prodlim)
library(Hmisc)
## library(BlandAltmanLeh)
library(cowplot)
## library(mi)

zout <- function(x) (scale(x) >= -4) & (scale(x) <= 4)
n <- function(x) as.numeric(as.character(x))


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

cleanVar <- function(x, LL, UL, inlist) {
  if (!class(x)[1] %in% c("numeric", "integer", "logical", "factor", "ordered", "character")) {
    stop("invalid class of x")
  }

  if (!missing(inlist)) {
    if (is.factor(x)) {
      out <- factor(ifelse(x %in% inlist, x, NA), levels = levels(x), ordered = is.ordered(x))
    } else {
      out <- ifelse(x %in% inlist, x, switch(class(x)[1],
                                             numeric = NA_real_,
                                             integer = NA_integer_,
                                             logical = NA,
                                             character = NA_character_))
    }
  } else {
    if (missing(LL)) LL <- -Inf
    if (missing(UL)) UL <- Inf
    if (is.factor(x)) {
      tmp <- as.numeric(as.character(x))
      out <- factor(ifelse(tmp >= LL & tmp <= UL, x, NA),
                    levels = levels(x), ordered = is.ordered(x))
    } else {
      if (is.character(x)) stop("cannot use limits with character")

      out <- ifelse(x >= LL & x <= UL, x, switch(class(x)[1],
                                             numeric = NA_real_,
                                             integer = NA_integer_,
                                             logical = NA))
    }
  }
  return(out)
}

## ## testing cleanVar()
## cleanVar(c(1, 2, 3, 99), UL = 4)
## cleanVar(c(1, 2, 3, 99, -1), LL = 1, UL = 4)
## cleanVar(c(1L, 2L, 99L, -1L), LL = 1, UL = 4)
## cleanVar(factor(c(1:3, 99), ordered = TRUE), LL = 1, UL = 4)
## cleanVar(factor(c(1:3, 99), ordered = TRUE), inlist = 1:4)

chooseVars <- function(x, drop) {
  x <- na.omit(x)
  x <- x[nzchar(x)]

  if (!missing(drop)) {
    x <- x[!x %in% drop]
  }

  return(x)
}

varsIn <- function(x, d) {
  x <- chooseVars(x)
  test <- x %in% names(d)
  cat(sprintf("%d/%d (%0.1f%%) variables in data",
              sum(test), length(test), mean(test) * 100), fill = TRUE)
  return(x[!test])
}

ftimediff <- function(x) {
    x1 <- difftime(as.POSIXct(paste("2015-02-24", x), format = "%Y-%M-%d %H:%M:%S"),
                   as.POSIXct("2015-02-24 24:00:00", format = "%Y-%M-%d %H:%M:%S"), units = "hours")
    x2 <- difftime(as.POSIXct(paste("2015-02-24", x), format = "%Y-%M-%d %H:%M:%S"),
                   as.POSIXct("2015-02-23 24:00:00", format = "%Y-%M-%d %H:%M:%S"), units = "hours")
    ifelse(abs(x1) < abs(x2), x1, x2)
}

ftimediff2 <- function(x) {
    x1 <- difftime(as.POSIXct(paste("2015-02-24", x), format = "%Y-%M-%d %H:%M:%S"),
                   as.POSIXct("2015-02-24 06:00:00", format = "%Y-%M-%d %H:%M:%S"), units = "hours")
    x2 <- difftime(as.POSIXct(paste("2015-02-24", x), format = "%Y-%M-%d %H:%M:%S"),
                   as.POSIXct("2015-02-23 06:00:00", format = "%Y-%M-%d %H:%M:%S"), units = "hours")
    ifelse(abs(x1) < abs(x2), x1, x2)
}

## function to help fill in missing values in order of priority
recursiveFillNA <- function(...) {
  x <- list(...)
  if (length(x) == 1) {
    x[[1]]
  } else {
    ifelse(is.na(x[[1]]), do.call(recursiveFillNA, x[-1]), x[[1]])
  }
}

## ## testing recursiveFillNA()
## all.equal(recursiveFillNA(1:5), 1:5)
## all.equal(recursiveFillNA(1:5, 6:10), 1:5)
## all.equal(recursiveFillNA(1:5, 6:10, 11:15), 1:5)
## all.equal(recursiveFillNA(c(1:4, NA)), c(1:4, NA))
## all.equal(recursiveFillNA(c(1:4, NA), 6:10), c(1:4, 10))
## all.equal(recursiveFillNA(c(NA, 2:4, NA), c(NA, 7:10), 11:15), c(11, 2:4, 10))
