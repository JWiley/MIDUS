source("https://bioconductor.org/biocLite.R")
biocLite("graph", suppressUpdates = TRUE)
biocLite("RBGL", suppressUpdates = TRUE)
library("devtools")
install_github("sachaepskamp/lvnet")
## install_github("JWiley/JWileymisc")

library(checkpoint)
checkpoint("2016-09-04", R.version = "3.3.1", use.knitr = TRUE, scan.rnw.with.knitr=TRUE)

library(JWileymisc)
library(pscore)
library(mice)
library(MplusAutomation)
library(grid)
library(qgraph)
library(pcalg)
library(data.table)
library(lvnet)


# Load dataset:
library("lavaan")
data(HolzingerSwineford1939)
Data <- HolzingerSwineford1939[,7:15]

# Measurement model:
Lambda <- matrix(0, 9, 3)
Lambda[1:3,1] <- NA
Lambda[4:6,2] <- NA
Lambda[7:9,3] <- NA

# Fit CFA model:
CFA <- lvnet(Data, lambda = Lambda)

summary(CFA)
plot(CFA)

# Latent network:
Omega_psi <- matrix(c(
  0,NA,NA,
  NA,0,0,
  NA,0,0
),3,3,byrow=TRUE)

# Fit model:
LNM <- lvnet(Data, lambda = Lambda, omega_psi=Omega_psi)
summary(LNM)
plot(LNM)

# Compare fit:
lvnetCompare(cfa=CFA,lnm=LNM)
plot(LNM, "factorStructure")


n <- function(x) as.numeric(as.character(x))
z <- function(x) as.vector(scale(x))


dwclean <- readRDS("~/OneDrive/Projects/MIDUS-Setup/midus_merged_data_dwclean.RDS")

dwclean2 <- copy(dwclean[, .(
  M2ID, B1PAGE_M2, Sex,
  Radj_epi, Radj_nor, Radj_crt, b4bdheas,
  Rb4p1gs, pulpress, Rb4p1d,
  p4homair, Rb4bgluc, b4bha1c,
  b4bldl, b4bhdl, Rb4btrig, Rb4pwhr,
  Rb4bcrp, b4bil6, b4bfgn, b4bsicam, b4bsesel,
  Rb4p1d, avgb_rm, avgb_hf, avgb_sd, avgb_lf)])

dwclean2[, MissAll := Reduce(`+`, lapply(.SD, is.na)), by = M2ID]
dwclean2 <- dwclean2[MissAll < 24]
table(dwclean2$MissAll)
str(dwclean2)

dwclean3 <- copy(dwclean2[, .(
  Radj_epi, Radj_nor, Radj_crt, b4bdheas,
  Rb4p1gs, pulpress, Rb4p1d,
  p4homair, Rb4bgluc, b4bha1c,
  b4bldl, b4bhdl, Rb4btrig, Rb4pwhr,
  Rb4bcrp, b4bil6, b4bfgn, b4bsicam, b4bsesel,
  avgb_rm, avgb_hf, avgb_sd, avgb_lf)])
colnames(dwclean3)

## > prcomp(~avgb_rm + avgb_hf + avgb_sd + avgb_lf, data = dwclean3)
## Standard deviations:
## [1] 1.8527992 0.6522419 0.3237215 0.1921463

## Rotation:
##                PC1        PC2         PC3        PC4
## avgb_rm -0.5083739  0.4673757 -0.09038762  0.7175974
## avgb_hf -0.5053625  0.4709487  0.37645208 -0.6173329
## avgb_sd -0.5120769 -0.2923137 -0.76178855 -0.2683433
## avgb_lf -0.4732251 -0.6887089  0.51941516  0.1787343
## > cor(dwclean3[, .(avgb_rm, avgb_hf, avgb_sd, avgb_lf)], use = "pairwise.complete.obs")
##           avgb_rm   avgb_hf   avgb_sd   avgb_lf
## avgb_rm 1.0000000 0.9556656 0.8356512 0.6887412
## avgb_hf 0.9556656 1.0000000 0.8058700 0.6994045
## avgb_sd 0.8356512 0.8058700 1.0000000 0.8742863
## avgb_lf 0.6887412 0.6994045 0.8742863 1.0000000


## residualize age and sex
round(apply(dwclean3, 2, sd, na.rm = TRUE), 2)
dwclean3[, (colnames(dwclean3)) := lapply(.SD, function(dv) resid(lm(dv ~ dwclean2$B1PAGE_M2 + factor(dwclean2$Sex), na.action = "na.exclude")))]
round(apply(dwclean3, 2, sd, na.rm = TRUE), 2)


setnames(dwclean3, names(dwclean3),
         c("E", "NE",
           "Crt", "DHE",
           "HR",
           "PP", "SBP",
           "IR", "GLU", "A1C",
           "LDL", "HDL", "TRI", "WHR",
           "CRP", "IL6", "FIB", "SIC", "ESE",
           "RMS", "HF", "SDR", "LF"))

## dwclean4 <- as.data.table(scale(na.omit(dwclean3)))
## dwclean4[, AL1 := (E + SDR + RMS + Crt + IL6 + FIB + SIC + SBP + GLU + IR + LDL + TRI)/12]
## dwclean4[, AL2 := (NE + DHE + HR + PP + A1C + HDL + WHR + CRP + ESE + HF + LF)/11]
## cor(dwclean4[, .(AL1, AL2)])

# Measurement model:
Lambda <- matrix(0, 23, 7)
Lambda[1:2, 1] <- NA
Lambda[3:4, 2] <- NA
Lambda[c(5, 20:23) ,3] <- NA
Lambda[6:7, 4] <- NA
Lambda[8:10, 5] <- NA
Lambda[11:14, 6] <- NA
Lambda[15:19, 7] <- NA

# Fit CFA model:
AL.CFA <- lvnet(as.data.frame(dwclean3), lambda = Lambda)
summary(AL.CFA)
plot(AL.CFA)

otheta <- matrix(NA, 23, 23)
diag(otheta) <- 0L

AL.RNM <- lvnet(as.data.frame(dwclean3), lambda = Lambda, omega_theta = otheta,
                lassoMatrix = "omega_theta", lasso = .2, lassoTol = 1e-3)
summary(AL.RNM)
plot(AL.RNM)




rmat <- SEMSummary(~ ., data = dwclean3, use = "fiml")
approxN <- round(nrow(dwclean3) * mean(!is.na(dwclean3)))

labs <- data.table(
  nodeNames = c("Epinephrine", "Norepinephrine", "Cortisol",
                "Dehydroepiandrosterone Sulfate", "Heart Rate",
                "Pulse Pressure", "Systolic Blood Pressure",
                "Insulin Resistance",
                "Glucose", "Glycosylated Hemoglobin",
                "Low Density Lipoprotein", "High Density Lipoprotein",
                "Triglycerides",
                "Waist-to-Hip Ratio", "C-reactive Protein", "Interleukin 6",
                "Fibrinogen", "Soluble Intracellular Adhesion Molecule 1",
                "Soluble E-selectin",
                "Root Mean Square of Successive Differences",
                "High Frequency Spectral Power", "Standard Deviation of R-R Intervals",
                "Low Frequency Spectral Power"),
  groups = c("SNS", "SNS", "HPA", "HPA", "PSNS", "Blood Pressure", "Blood Pressure",
                 "Metabolic - Glucose", "Metabolic - Glucose", "Metabolic - Glucose",
                 "Metabolic - Lipids", "Metabolic - Lipids", "Metabolic - Lipids", "Metabolic - Lipids",
                 "Inflammation", "Inflammation", "Inflammation", "Inflammation", "Inflammation",
             "PSNS", "PSNS", "PSNS", "PSNS"))
labs[, groups := factor(groups, levels = unique(groups))]

## determine directed graph
dirg <- pc(
  list(C = rmat$sSigma,
       n = approxN),
  gaussCItest, alpha = .01,
  labels = colnames(dwclean3))


par(mfrow = c(1, 2))
glassoGraph <- qgraph(rmat$sSigma,
                      layout = "spring",
##                      layout = corGraph$layout,
      graph = "glasso", sampleSize = approxN,
      nodeNames = labs[, nodeNames],
      groups = labs[, groups],
      legend.cex = 0.45,
      cut = 0.1, maximum = 1, minimum = 0, esize = 10,
      vsize = 5, repulsion = 1.2,
      gray = TRUE)

##plot directed graph
qgraph(dirg,
       esize = 1, edge.color = "black",
       ## layout = corGraph$layout,
       layout = glassoGraph$layout,
       directed = TRUE,
       labels = colnames(dwclean3),
      nodeNames = labs[, nodeNames],
      groups = labs[, groups],
      legend.cex = 0.45, gray = TRUE, vsize = 5, legend = FALSE
       )

pdf(file = "al_dag.pdf", width = 12, height = 7)
qgraph(dirg,
       esize = 1, edge.color = "black",
       layout = "spring",
       directed = TRUE,
       labels = colnames(dwclean3),
       nodeNames = labs[, nodeNames],
       groups = labs[, groups],
       legend.cex = 0.45, gray = TRUE, vsize = 5, repulsion = .8
       )
dev.off()

qgraph(
  cor(dwclean3, use = "pairwise.complete.obs"),
  graph = "cor",
  layout = "spring")

corGraph <- qgraph(cor(dwclean3, use = "pairwise.complete.obs"),
       layout = "spring", graph = "cor",
       ##nodeNames = Names, groups = Groups,
       legend.cex = 0.3,
       cut = 0.3, maximum = 1, minimum = 0, esize = 10,
       vsize = 5, repulsion = 1.9, gray = TRUE)

pcorGraph <- qgraph(cor(dwclean3, use = "pairwise.complete.obs"),
       layout = corGraph$layout, graph = "pcor",
       ##nodeNames = Names, groups = Groups,
       legend.cex = 0.3,
       cut = 0.1, maximum = 1, minimum = 0, esize = 20,
       vsize = 5)

pcorGraph2 <- qgraph(cor(dwclean3, use = "pairwise.complete.obs"),
                     layout = corGraph$layout, graph = "pcor",
                     threshold = "holm", sampleSize = mean(colSums(!is.na(dwclean3))),
                     ##nodeNames = Names, groups = Groups,
                     legend.cex = 0.3,
                     cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                     vsize = 5)

## optGraph <- findGraph(cor(dwclean3, use = "pairwise.complete.obs"),
##                       mean(colSums(!is.na(dwclean3))),
##                       type = "cor")
## optimalGraph <- qgraph(optGraph, layout = corGraph$layout,
##                        ##nodeNames = Names, groups = Groups,
##                        legend.cex = 0.3,
##       cut = 0.1, maximum = 1, minimum = 0, esize = 20,
##       vsize = 5)


glassoGraph <- qgraph(cor(dwclean3, use = "pairwise.complete.obs"),
                      layout = corGraph$layout,
      graph = "glasso", sampleSize = mean(colSums(!is.na(dwclean3))),
      nodeNames = c("Epinephrine", "Norepinephrine", "Cortisol", "DHEA-S", "Heart Rate", "Pulse Pressure", "DBP",
           "HOMA-IR", "Glucose", "HbA1c", "LDL", "HDL", "Triglycerides",
           "WHR", "CRP", "IL6", "Fibrinogen", "s-ICAM", "sE-selectin",
           "HR RMSSD", "High Frequency HRV", "HR SDRR", "Low Frequency HRV"),
      groups = c("SNS", "SNS", "HPA", "HPA", "PSNS", "Blood Pressure", "Blood Pressure",
                 "Metabolic - Glucose", "Metabolic - Glucose", "Metabolic - Glucose",
                 "Metabolic - Lipids", "Metabolic - Lipids", "Metabolic - Lipids", "Metabolic - Lipids",
                 "Inflammation", "Inflammation", "Inflammation", "Inflammation", "Inflammation",
                 "PSNS", "PSNS", "PSNS", "PSNS"),
      legend.cex = 0.45,
      cut = 0.05, maximum = 1, minimum = 0, esize = 10,
      vsize = 5)
