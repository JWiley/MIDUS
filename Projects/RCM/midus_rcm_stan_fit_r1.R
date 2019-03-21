args <- commandArgs(trailingOnly=TRUE)

index <- seq(
  from = as.integer(args[1]),
  to = as.integer(args[2]),
  by = 1)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 2)

load("midus_rcm_stan_data.RData")

rcm.fit <- lapply(index, function(i) {
  stan.data <- list(
    N = N,
    J = J,
    K = K,
    X1 = X1[[i]], y1 = as.integer(y1[[i]]),
    X2 = X2[[i]], y2 = as.integer(y2[[i]]),
    X3 = X3[[i]], y3 = y3[[i]],
    X4 = X4[[i]], y4 = y4[[i]],
    X5 = X5[[i]], y5 = y5[[i]],
    X6 = X6[[i]], y6 = y6[[i]],
    X7 = X7[[i]], y7 = as.integer(y7[[i]]),
    X8 = X8[[i]], y8 = as.integer(y8[[i]]),
    X9 = X9[[i]], y9 = y9[[i]],
    X10 = X10[[i]], y10 = y10[[i]],
    ## re-use for all systems (x7)
    X11 = X11[[i]],
    y11 = y11[[i]], y12 = y12[[i]], y13 = y13[[i]], y14 = y14[[i]],
    y15 = y15[[i]], y16 = y16[[i]], y17 = y17[[i]],
    X18 = X18[[i]], y18 = as.integer(y18[[i]]),
    X19 = X19[[i]], y19 = as.integer(y19[[i]])
  )

  out <- sampling(
    m.comp,
    data = stan.data,
    pars = stan.parameters,
    chains = 2, warmup = 1000, init = "random",
    iter = 4000, thin = 2, seed = seeds[i],
    cores = getOption("mc.cores", 1L))
  saveRDS(out, file = sprintf("midus_rcm_fit_%d.RDS", i))
  return(TRUE)
})
