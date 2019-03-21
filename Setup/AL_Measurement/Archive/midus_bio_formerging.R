library(foreign)
dat <- read.dta("~/Documents/Research/MIDUS/p4subset_biology_3-20-12.dta")
demodat <- read.dta("~/Documents/Research/MIDUS/demographics_for_JW.dta")
chronic <- read.dta("~/Documents/Research/MIDUS/data/m2p4chronic_2_7_11.dta")


vars <- list(
  misc = c(
    "m2id", "m2famnum",
    "b1page_m2", "b1pgender", "b4zsite", "b4p1a", "b4p1b", "b4pbmi",
     "m1m2race", "p4majorconditions", "p4minorconditions", "p4sumburden"),
  sympathetic = c("Radj_epi", "Radj_nor"),
  parasympathetic = c("avgb_sd", "avgb_rm", "avgb_lf", "avgb_hf", "Rb4p1d"),
  hpa = c("Radj_crt", "b4bdheas"),
  inflammation = c("Rb4bcrp", "b4bil6", "b4bfgn", "b4bsicam", "b4bsesel"),
  cardiovascular = c("pulpress", "Rb4p1gs"),
  glucose = c("b4bha1c", "Rb4bgluc", "p4homair"),
  lipid = c("b4bldl", "b4bhdl", "Rb4btrig", "Rb4pwhr")
)


all <- merge(dat, demodat, by = "m2id", all = TRUE)
all <- merge(all, chronic, by = "m2id", all = TRUE)
fdat <- all[, unlist(vars)]

fdat[] <- lapply(fdat, function(x) {
  x[x %in% c(999999, 99999, 9999, 999, 998)] <- NA
  return(x)
})
saveRDS(fdat, file = "midus_bio_formerging.RDS")

