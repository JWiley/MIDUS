library(parallel)
cl <- makeCluster(20)

index <- list(c(1L, 1L), c(2L, 2L), c(3L, 3L), c(4L, 4L), c(5L, 5L), c(6L,
6L), c(7L, 7L), c(8L, 8L), c(9L, 9L), c(10L, 10L), c(11L, 11L
), c(12L, 12L), c(13L, 13L), c(14L, 14L), c(15L, 15L), c(16L,
16L), c(17L, 17L), c(18L, 18L), c(19L, 19L), c(20L, 20L), c(21L,
21L), c(22L, 22L), c(23L, 23L), c(24L, 24L), c(25L, 25L), c(26L,
26L), c(27L, 27L), c(28L, 28L), c(29L, 29L), c(30L, 30L), c(31L,
31L), c(32L, 32L), c(33L, 33L), c(34L, 34L), c(35L, 35L), c(36L,
36L), c(37L, 37L), c(38L, 38L), c(39L, 39L), c(40L, 40L), c(41L,
41L), c(42L, 42L), c(43L, 43L), c(44L, 44L), c(45L, 45L), c(46L,
46L), c(47L, 47L), c(48L, 48L), c(49L, 49L), c(50L, 50L))


parLapply(cl, index, function(x) {
  x <- as.integer(x)
  system2(
    command = "Rscript",
    args = sprintf("midus_rcm_stan_fit_r1.R %d %d", x[1], x[2])
  )
})
