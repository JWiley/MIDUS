library(MplusAutomation)

m1 <- readModels("factor_allcov_clustering_bifac_alt_1.out")
m2 <- readModels("factor_allcov_clustering_bifac_alt_2.out")

d1 <- lapply(1:50, function(i) {
  read.table(sprintf("al_alt_1_imp%d.dat", i), header = FALSE, na.strings = "*")[, 18]
})

d2 <- lapply(1:50, function(i) {
  read.table(sprintf("al_alt_2_imp%d.dat", i), header = FALSE, na.strings = "*")[, 20]
})


hist(sapply(1:50, function(i) {
  cor(d1[[i]], d2[[i]])
}))
