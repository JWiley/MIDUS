summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.tst$M2ID)
      tmpd <- data.frame(
        M2ID = useIDs,
        Deceased = as.integer(useIDs %in% midus3.mort$M2ID),
        DDate = ifelse(useIDs %in% midus3.mort$M2ID,
                       midus3.mort$DDate[match(useIDs, midus3.mort$M2ID)],
                       as.Date("2015-05-15")) - as.numeric(as.Date("1995-03-01"))
        )
      tmpd$U <- m1.results$U[i, ]
      tmpd$V <- m1.results$Sigma_V[i, ]
      coxph(Surv(DDate, Deceased) ~ scale(U) + scale(V), data = tmpd)
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(
        M2ID = useIDs,
        Deceased = as.integer(useIDs %in% midus3.mort$M2ID),
        DDate = ifelse(useIDs %in% midus3.mort$M2ID,
                       midus3.mort$DDate[match(useIDs, midus3.mort$M2ID)],
                       as.Date("2015-05-15")) - as.numeric(as.Date("1995-03-01"))
        )
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      coxph(Surv(DDate, Deceased) ~ scale(U) + scale(V), data = tmpd)
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(
        M2ID = useIDs,
        Deceased = as.integer(useIDs %in% midus3.mort$M2ID),
        DDate = ifelse(useIDs %in% midus3.mort$M2ID,
                       midus3.mort$DDate[match(useIDs, midus3.mort$M2ID)],
                       as.Date("2015-05-15")) - as.numeric(as.Date("1995-03-01"))
        )
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      coxph(Surv(DDate, Deceased) ~ scale(U), data = tmpd)
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(
        M2ID = useIDs,
        Deceased = as.integer(useIDs %in% midus3.mort$M2ID),
        DDate = ifelse(useIDs %in% midus3.mort$M2ID,
                       midus3.mort$DDate[match(useIDs, midus3.mort$M2ID)],
                       as.Date("2015-05-15")) - as.numeric(as.Date("1995-03-01"))
        )
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      coxph(Surv(DDate, Deceased) ~ scale(V), data = tmpd)
    }))))


summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.se$M2ID)
      tmpd <- data.frame(
        M2ID = useIDs,
        Deceased = as.integer(useIDs %in% midus3.mort$M2ID),
        DDate = ifelse(useIDs %in% midus3.mort$M2ID,
                       midus3.mort$DDate[match(useIDs, midus3.mort$M2ID)],
                       as.Date("2015-05-15")) - as.numeric(as.Date("1995-03-01"))
        )
      tmpd$U <- m3.results$U[i, ]
      tmpd$V <- m3.results$Sigma_V[i, ]
      coxph(Surv(DDate, Deceased) ~ scale(U) + scale(V), data = tmpd)
    }))))







summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.tst$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m1.results$U[i, ]
      tmpd$V <- m1.results$Sigma_V[i, ]
      glm(Deceased ~ scale(V), data = tmpd, family = binomial())
    }))))


summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      glm(Deceased ~ scale(V), data = tmpd, family = binomial())
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      glm(Deceased ~ scale(U), data = tmpd, family = binomial())
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.sol$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m2.results$U[i, ]
      tmpd$V <- m2.results$Sigma_V[i, ]
      glm(Deceased ~ scale(U) + scale(V), data = tmpd, family = binomial())
    }))))


summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.se$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m3.results$U[i, ]
      tmpd$V <- m3.results$Sigma_V[i, ]
      glm(Deceased ~ scale(U), data = tmpd, family = binomial())
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.rt2$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m4.results$U[i, ]
      tmpd$V <- m4.results$Sigma_V[i, ]
      glm(Deceased ~ scale(U) + scale(V), data = tmpd, family = binomial())
    }))))

summary(pool(as.mira(
    lapply(1:50, function(i) {
      useIDs <- unique(d.bt2$M2ID)
      tmpd <- data.frame(M2ID = useIDs, Deceased = as.integer(useIDs %in% midus3.mort$M2ID))
      tmpd$U <- m5.results$U[i, ]
      tmpd$V <- m5.results$Sigma_V[i, ]
      glm(Deceased ~ scale(V), data = tmpd, family = binomial())
    }))))
