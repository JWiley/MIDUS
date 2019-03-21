library(ggplot2)
library(gridExtra)
library(scales)
library(mgcv)

p4final <- readRDS("~/OneDrive/Projects/MIDUS/JW-MIDUS-AL/p4final.RDS")

n <- function(x) as.numeric(as.character(x))

p4final <- lapply(p4final, function(d) {
d <- within(d, {
   Diet <- n(Diet)
   ExtremeDiet <- n(ExtremeDiet)
   OverallDiet <- n(OverallDiet)
   CurrentSmoke <- as.integer(CurrentSmoke == 1)
   RiskyBehaviors <- as.integer(RiskyBehaviors == 1)
   PhysAct <- n(PhysAct)
   Milwaukee <- as.integer(is.na(m2famnum))
   m2famnum <- ifelse(is.na(m2famnum), M2ID, m2famnum)

   b1pgender <- as.integer(b1pgender == "female")
   b1page_m2 <- (b1page_m2 - 55) / 10;

   NegAff <- NegAff * -1
   PosAff <- PosAff * -1

   m1chadv <- as.vector(scale(m1chadv))
   m2aadv <- as.vector(scale(m2aadv))
   dSES <- m2aadv - m1chadv

   ##CurAlc2 <- CurrentAlcohol^2
   CANone <- as.integer(CurrentAlcohol == "None")
   CAInfrequent <- as.integer(CurrentAlcohol == "Infrequent<4")
   CAFrequent <- as.integer(CurrentAlcohol == "Frequent<4")
   CAHeavy <- as.integer(CurrentAlcohol == "Heavy")

   PSR <- as.vector(scale(scale(PR) + scale(SR)))

 })

return(d)
})




summary(m1 <- lm(disp ~ hp, data = mtcars))
summary(m2 <- lm(mpg ~ disp + hp, data = mtcars))

d1 <- data.frame(Disp = mtcars$disp,
                    Disphat = fitted(m1))

grid.arrange(
  ggplot(d1) +
    geom_density(aes(Disp), colour = "black") +
    geom_density(aes(Disphat), colour = "blue") +
    theme_bw(),
  ggplot(d1, aes(Disp, Disphat)) +
    geom_point() + theme_bw(), ncol = 2)

set.seed(1234)
d2 <- do.call(rbind, lapply(1:nrow(mtcars), function(i) {
                     location <- predict(m1, newdata = data.frame(hp = mtcars$hp[i]))
                     scale <- summary(m1)$sigma
                     data.frame(hp = mtcars$hp[i],
                                mpg = mtcars$mpg[i],
                                disp = mtcars$disp[i],
                                Disphat = location,
                                DispDist = rnorm(10, location, scale))
                   }))

  ggplot(d2) +
    geom_density(aes(disp), colour = "black") +
    geom_density(aes(Disphat), colour = "blue") +
    geom_density(aes(DispDist), colour = "red") +
    theme_bw()

set.seed(1234)
d3 <- do.call(rbind, lapply(1:nrow(d2), function(i) {
                                    location <- predict(m2, newdata = data.frame(
                                                              hp = d2$hp[i],
                                                              disp = d2$DispDist[i]
                                      ))
                                    location2 <- predict(m2, newdata = data.frame(
                                                              hp = d2$hp[i],
                                                              disp = d2$disp[i]
                                      ))

                     scale <- summary(m2)$sigma
                     data.frame(hp = d2$hp[i],
                                disp = d2$disp[i],
                                Disphat = d2$Disphat[i],
                                DispDist = d2$DispDist[i],
                                mpg = d2$mpg[i],
                                mpghat = location,
                                mpgDist = rnorm(10, location, scale),
                                mpghat2 = location2,
                                mpgDist2 = rnorm(10, location2, scale)
                                )
                   }))

grid.arrange(
  ggplot(d2) +
    geom_density(aes(disp), colour = "black") +
    geom_density(aes(Disphat), colour = "blue") +
    geom_density(aes(DispDist), colour = "red") +
    theme_bw(),
  ggplot(d3) +
    geom_density(aes(mpg), colour = "black") +
    geom_density(aes(mpghat), colour = "blue") +
#    geom_density(aes(mpghat2), colour = "orange") +
    geom_density(aes(mpgDist), colour = "red") +
    geom_density(aes(mpgDist2), colour = "green") +
    theme_bw(), ncol = 2)




summary(m2 <- gam(price ~ s(carat, k = 15), data = diamonds))

plot(resid(m2), fitted(m2))


sim.poisson <- function(d, b, nperobs = 1) {
  location <- as.vector(d %*% b)
  rpois(length(location) * nperobs, lambda = exp(location))
}

sim.normal <- function(d, b, scale, nperobs = 1) {
  location <- as.vector(d %*% b)
  rnorm(length(location) * nperobs, location, scale)
}

sim.binomial <- function(d, b, nperobs = 1) {
  location <- as.vector(d %*% b)
  rbinom(length(location) * nperobs, size = 1, prob = plogis(location))
}

sim.multinomial <- function(d, b, nperobs = 1) {
    location <- plogis(d %*% b)
    location <- cbind(
        location[, -ncol(location), drop = FALSE] - location[, -1, drop = FALSE],
        location[, ncol(location), drop = FALSE])
    location <- cbind(1 - rowSums(location), location)
    colnames(location) <- NULL

    unlist(lapply(1:nrow(d), function(i) {
                          res <- rmultinom(nperobs, size = 1, prob = location[i, ])
                          apply(res == 1, 2, which)
                      }))
}

setup <- list(
    use = function(...) {
        x <- list(...)
        default <- list(
            sex = "raw",
            age = "raw",
            SES = "raw",
            dSES = "raw",
            LifeStress = "estimated",
            PSR = "estimated",
            PSRxStress = "computed",
            PSRxSES = "computed",
            PSRxdSES = "computed",
            PSRxSex = "computed",
            NegAff = "estimated",
            F = "estimated",
            PSQI3 = "estimated",
            CurrentSmoke = "estimated",
            RiskyBehaviors = "estimated",
            CANone = "estimated",
            CAFrequent = "estimated",
            CAHeavy = "estimated",
            PhysAct = "estimated",
            OverallDiet = "estimated",
            Conditions = "estimated")

        stopifnot(all(names(x) %in% names(default)))
        for (n in names(x)) {
            default[[n]] <- x[[n]]
        }
        return(default)
    },
    k = function(...) {
        x <- list(...)
        default <- list(
            sex = NA,
            age = NA,
            SES = NA,
            dSES = NA,
            LifeStress = NA,
            PSR = NA,
            PSRxStress = NA,
            PSRxSES = NA,
            PSRxdSES = NA,
            PSRxSex = NA,
            NegAff = NA,
            F = NA,
            PSQI3 = NA,
            CurrentSmoke = NA,
            RiskyBehaviors = NA,
            CANone = NA,
            CAFrequent = NA,
            CAHeavy = NA,
            PhysAct = NA,
            OverallDiet = NA,
            Conditions = NA)

        stopifnot(all(names(x) %in% names(default)))
        for (n in names(x)) {
            default[[n]] <- x[[n]]
        }
        return(default)
    },
    b = function(...) {
        x <- list(...)
        default <- list(
            sex = NA,
            age = NA,
            SES = NA,
            dSES = NA,
            LifeStress = NA,
            PSR = NA,
            PSRxStress = NA,
            PSRxSES = NA,
            PSRxdSES = NA,
            PSRxSex = NA,
            NegAff = NA,
            F = NA,
            PSQI3 = NA,
            CurrentSmoke = NA,
            RiskyBehaviors = NA,
            CANone = NA,
            CAFrequent = NA,
            CAHeavy = NA,
            PhysAct = NA,
            OverallDiet = NA,
            Conditions = NA)

        stopifnot(all(names(x) %in% names(default)))
        for (n in names(x)) {
            default[[n]] <- x[[n]]
        }
        return(default)
    })






rcm.simulator <- function(b = setup$b(), v = setup$b(), use = setup$use(), k = setup$k(), d, nperobs = 1, seed = 1234) {
    set.seed(seed)

    simulated.values <- list()

    if (use$sex == "raw") {
        simulated.values$sex <- d$b1pgender
    } else {
        stop("can only use raw sex")
    }
    if (!is.na(k$sex)) {
        simulated.values$sex <- k$sex(simulated.values$sex)
    }


    if (use$age == "raw") {
        simulated.values$age <- d$b1page_m2
    } else {
        stop("can only use raw age")
    }
    if (!is.na(k$age)) {
        simulated.values$age <- k$age(simulated.values$age)
    }


    if (use$SES == "raw") {
        simulated.values$SES <- d$m1chadv
    } else {
        stop("can only use raw SES")
    }
    if (!is.na(k$SES)) {
        simulated.values$SES <- k$SES(simulated.values$SES)
    }


    if (use$dSES == "raw") {
        simulated.values$dSES <- d$dSES
    } else {
        stop("can only use raw dSES")
    }
    if (!is.na(k$dSES)) {
        simulated.values$dSES <- k$dSES(simulated.values$dSES)
    }

    if (use$LifeStress == "raw") {
        simulated.values$LifeStress <- d$LifeStress
    } else if (use$LifeStress == "estimated") {
        simulated.values$LifeStress <- sim.poisson(
            with(simulated.values, cbind(1, sex, age, SES, dSES)),
            b = b$LifeStress, nperobs = nperobs)
    } else {
        stop("Unknown LifeStress option")
    }
    if (!is.na(k$LifeStress)) {
        simulated.values$LifeStress <- k$LifeStress(simulated.values$LifeStress)
    }


    if (use$PSR == "raw") {
        simulated.values$PSR <- d$PSR
    } else if (use$PSR == "estimated") {
        simulated.values$PSR <- sim.normal(
            with(simulated.values, cbind(1, sex, age, SES, dSES)),
            b = b$PSR, scale = sqrt(v$PSR), nperobs = nperobs)
    } else {
        stop("Unknown PSR option")
    }
    if (!is.na(k$PSR)) {
        simulated.values$PSR <- k$PSR(simulated.values$PSR)
    }

    if (use$PSRxStress == "computed") {
        simulated.values$PSRxStress <- with(simulated.values, PSR * LifeStress)
    } else {
        stop("can only use computed PSRxStress")
    }
    if (use$PSRxSES == "computed") {
        simulated.values$PSRxSES <- with(simulated.values, PSR * SES)
    } else {
        stop("can only use computed PSRxSES")
    }
    if (!is.na(k$PSRxSES)) {
        simulated.values$PSRxSES <- k$PSRxSES(simulated.values$PSRxSES)
    }
    if (use$PSRxdSES == "computed") {
        simulated.values$PSRxdSES <- with(simulated.values, PSR * dSES)
    } else {
        stop("can only use computed PSRxdSES")
    }
    if (!is.na(k$PSRxdSES)) {
        simulated.values$PSRxdSES <- k$PSRxdSES(simulated.values$PSRxdSES)
    }
    if (use$PSRxSex == "computed") {
        simulated.values$PSRxSex <- with(simulated.values, PSR * sex)
    } else {
        stop("can only use computed PSRxSex")
    }
    if (!is.na(k$PSRxSex)) {
        simulated.values$PSRxSex <- k$PSRxSex(simulated.values$PSRxSex)
    }


    if (use$NegAff == "raw") {
        simulated.values$NegAff <- d$NegAff
    } else if (use$NegAff == "estimated") {
        simulated.values$NegAff <- sim.normal(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress,
                                       PSRxSES, PSRxdSES)),
            b = b$NegAff, scale = sqrt(v$NegAff), nperobs = nperobs)
    } else {
        stop("Unknown NegAff option")
    }
    if (!is.na(k$NegAff)) {
        simulated.values$NegAff <- k$NegAff(simulated.values$NegAff)
    }


    if (use$PSQI3 == "raw") {
        simulated.values$PSQI3 <- d$PSQI3
    } else if (use$PSQI3 == "estimated") {
        simulated.values$PSQI3 <- sim.normal(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff,
                                       PSRxSES, PSRxdSES)),
            b = b$PSQI3, scale = sqrt(v$PSQI3), nperobs = nperobs)
    } else {
        stop("Unknown PSQI3 option")
    }
    if (!is.na(k$PSQI3)) {
        simulated.values$PSQI3 <- k$PSQI3(simulated.values$PSQI3)
    }


    if (use$CurrentSmoke == "raw") {
        simulated.values$CurrentSmoke <- d$CurrentSmoke
    } else if (use$CurrentSmoke == "estimated") {
        simulated.values$CurrentSmoke <- sim.binomial(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$CurrentSmoke, nperobs = nperobs)
    } else {
        stop("Unknown CurrentSmoke option")
    }
    if (!is.na(k$CurrentSmoke)) {
        simulated.values$CurrentSmoke <- k$CurrentSmoke(simulated.values$CurrentSmoke)
    }


    if (use$RiskyBehaviors == "raw") {
        simulated.values$RiskyBehaviors <- d$RiskyBehaviors
    } else if (use$RiskyBehaviors == "estimated") {
        simulated.values$RiskyBehaviors <- sim.binomial(
            with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$RiskyBehaviors, nperobs = nperobs)
    } else {
        stop("Unknown RiskyBehaviors option")
    }
    if (!is.na(k$RiskyBehaviors)) {
        simulated.values$RiskyBehaviors <- k$RiskyBehaviors(simulated.values$RiskyBehaviors)
    }


    if (use$CANone == "raw") {
        simulated.values$CANone <- d$CANone
    } else if (use$CANone == "estimated") {
        simulated.values$CANone <- sim.binomial(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$CANone, nperobs = nperobs)
    } else {
        stop("Unknown CANone option")
    }
    if (!is.na(k$CANone)) {
        simulated.values$CANone <- k$CANone(simulated.values$CANone)
    }

    if (use$CAFrequent == "raw") {
        simulated.values$CAFrequent <- d$CAFrequent
    } else if (use$CAFrequent == "estimated") {
        simulated.values$CAFrequent <- sim.binomial(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$CAFrequent, nperobs = nperobs)
    } else {
        stop("Unknown CAFrequent option")
    }
    if (!is.na(k$CAFrequent)) {
        simulated.values$CAFrequent <- k$CAFrequent(simulated.values$CAFrequent)
    }

    if (use$CAHeavy == "raw") {
        simulated.values$CAHeavy <- d$CAHeavy
    } else if (use$CAHeavy == "estimated") {
        simulated.values$CAHeavy <- sim.binomial(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$CAHeavy, nperobs = nperobs)
    } else {
        stop("Unknown CAHeavy option")
    }
    if (!is.na(k$CAHeavy)) {
        simulated.values$CAHeavy <- k$CAHeavy(simulated.values$CAHeavy)
    }


    if (use$PhysAct == "raw") {
        simulated.values$PhysAct <- d$PhysAct
    } else if (use$PhysAct == "estimated") {
        simulated.values$PhysAct <- sim.multinomial(
            with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$PhysAct, nperobs = nperobs)
    } else {
        stop("Unknown PhysAct option")
    }
    if (!is.na(k$PhysAct)) {
        simulated.values$PhysAct <- k$PhysAct(simulated.values$PhysAct)
    }


    if (use$OverallDiet == "raw") {
        simulated.values$OverallDiet <- d$OverallDiet
    } else if (use$OverallDiet == "estimated") {
        simulated.values$OverallDiet <- sim.normal(
          with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff)),
            b = b$OverallDiet, scale = sqrt(v$OverallDiet), nperobs = nperobs)
    } else {
        stop("Unknown OverallDiet option")
    }
    if (!is.na(k$OverallDiet)) {
        simulated.values$OverallDiet <- k$OverallDiet(simulated.values$OverallDiet)
    }


    if (use$F == "raw") {
        simulated.values$F <- d$F
    } else if (use$F == "estimated") {
        simulated.values$F <- sim.normal(
            with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff,
                                         PSQI3, CurrentSmoke, RiskyBehaviors,
                                         CANone, CAFrequent, CAHeavy,
                                         PhysAct, OverallDiet)),
            b = b$F, scale = sqrt(v$F), nperobs = nperobs)
    } else {
        stop("Unknown F option")
    }
    if (!is.na(k$F)) {
        simulated.values$F <- k$F(simulated.values$F)
    }


    if (use$Conditions == "raw") {
        simulated.values$Conditions <- d$p4majorconditions
    } else if (use$Conditions == "estimated") {
        simulated.values$Conditions <- sim.poisson(
            with(simulated.values, cbind(1, sex, age, SES, dSES, PSR, LifeStress, NegAff,
                                         PSQI3, CurrentSmoke, RiskyBehaviors,
                                         CANone, CAFrequent, CAHeavy,
                                         PhysAct, OverallDiet, F, PSRxStress
                                         )),

            b = b$Conditions, nperobs = nperobs)
    } else {
        stop("Unknown Conditions option")
    }
    if (!is.na(k$Conditions)) {
        simulated.values$Conditions <- k$Conditions(simulated.values$Conditions)
    }

    return(simulated.values)

}


test <- rcm.simulator(b = setup$b(
    LifeStress = matrix(c(1.319, -0.154, 0.012, -0.351, -0.235)),
    PSR = matrix(c(-0.148, 0.149, 0.126, 0.375, 0.306)),
    NegAff = matrix(c(-.257, .140, -.147, -.033, -.076, -.458, .044, .085, .080)),
    PSQI3 = matrix(c(4.472, .650, .059, -.363, -.190, -.194, .192, .703, .275, .163)),
    CurrentSmoke = matrix(c(-2.291, -.450, -.330, -.554, -.555, .046, .119, .375)),
    RiskyBehaviors = matrix(c(-2.003, -1.416, .001, -.457, -.364, -.048, -.021, .027)),
    CANone = matrix(c(-.674, .428, .164, -.313, -.293, -.076, -.053, -.130)),
    CAFrequent = matrix(c(-1.097, -.683, .171, .458, .367, .145, -.011, .283)),
    CAHeavy = matrix(c(-3.490, -1.733, -.679, -.551, -.556, .198, .192, .097)),
    PhysAct = cbind(
        c(1.375, -.364, -.148, .164, .092, .185, .008, -.030),
        c(.204, -.364, -.148, .164, .092, .185, .008, -.030),
        c(-.964, -.364, -.148, .164, .092, .185, .008, -.030)),
    OverallDiet = matrix(c(1.232, -.246, -.076, -.126, -.081, -.062, .001, .034)),
    F = matrix(c(-.303, -.072, .023, -.134, -.091, .074, .032, .042, .042, .058, .000, .100, -.117, .237, -.157, .182)),
    Conditions = matrix(c(-.284, .056, .335, -.016, -.072, -.045, .020, .069, .039, -.036, .240, .118, .023, -.136, -.084, .000, .249, .021))
  ),
  v = setup$b(
    PSR = 0.890,
    NegAff = 0.665,
    PSQI3 = 8.065,
    OverallDiet = .440,
    F = .834
  ),
  d = subset(p4final[[1]], !is.na(m1chadv) & !is.na(dSES)),
  nperobs = 1, seed = 1234)

test2 <- rcm.simulator(b = setup$b(
    LifeStress = matrix(c(1.319, -0.154, 0.012, -0.351, -0.235)),
    PSR = matrix(c(-0.148, 0.149, 0.126, 0.375, 0.306)),
    NegAff = matrix(c(-.257, .140, -.147, -.033, -.076, -.458, .044, .085, .080)),
    PSQI3 = matrix(c(4.472, .650, .059, -.363, -.190, -.194, .192, .703, .275, .163)),
    CurrentSmoke = matrix(c(-2.291, -.450, -.330, -.554, -.555, .046, .119, .375)),
    RiskyBehaviors = matrix(c(-2.003, -1.416, .001, -.457, -.364, -.048, -.021, .027)),
    CANone = matrix(c(-.674, .428, .164, -.313, -.293, -.076, -.053, -.130)),
    CAFrequent = matrix(c(-1.097, -.683, .171, .458, .367, .145, -.011, .283)),
    CAHeavy = matrix(c(-3.490, -1.733, -.679, -.551, -.556, .198, .192, .097)),
    PhysAct = cbind(
        c(1.375, -.364, -.148, .164, .092, .185, .008, -.030),
        c(.204, -.364, -.148, .164, .092, .185, .008, -.030),
        c(-.964, -.364, -.148, .164, .092, .185, .008, -.030)),
    OverallDiet = matrix(c(1.232, -.246, -.076, -.126, -.081, -.062, .001, .034)),
    F = matrix(c(-.303, -.072, .023, -.134, -.091, .074, .032, .042, .042, .058, .000, .100, -.117, .237, -.157, .182)),
    Conditions = matrix(c(-.284, .056, .335, -.016, -.072, -.045, .020, .069, .039, -.036, .240, .118, .023, -.136, -.084, .000, .249, .021))
  ),
  v = setup$b(
    PSR = 0.890,
    NegAff = 0.665,
    PSQI3 = 8.065,
    OverallDiet = .440,
    F = .834
    ),
  k = setup$k(SES = function(x) x - 1),
  d = subset(p4final[[1]], !is.na(m1chadv) & !is.na(dSES)),
  nperobs = 1, seed = 1234)

test3 <- rcm.simulator(b = setup$b(
    LifeStress = matrix(c(1.319, -0.154, 0.012, -0.351, -0.235)),
    PSR = matrix(c(-0.148, 0.149, 0.126, 0.375, 0.306)),
    NegAff = matrix(c(-.257, .140, -.147, -.033, -.076, -.458, .044, .085, .080)),
    PSQI3 = matrix(c(4.472, .650, .059, -.363, -.190, -.194, .192, .703, .275, .163)),
    CurrentSmoke = matrix(c(-2.291, -.450, -.330, -.554, -.555, .046, .119, .375)),
    RiskyBehaviors = matrix(c(-2.003, -1.416, .001, -.457, -.364, -.048, -.021, .027)),
    CANone = matrix(c(-.674, .428, .164, -.313, -.293, -.076, -.053, -.130)),
    CAFrequent = matrix(c(-1.097, -.683, .171, .458, .367, .145, -.011, .283)),
    CAHeavy = matrix(c(-3.490, -1.733, -.679, -.551, -.556, .198, .192, .097)),
    PhysAct = cbind(
        c(1.375, -.364, -.148, .164, .092, .185, .008, -.030),
        c(.204, -.364, -.148, .164, .092, .185, .008, -.030),
        c(-.964, -.364, -.148, .164, .092, .185, .008, -.030)),
    OverallDiet = matrix(c(1.232, -.246, -.076, -.126, -.081, -.062, .001, .034)),
    F = matrix(c(-.303, -.072, .023, -.134, -.091, .074, .032, .042, .042, .058, .000, .100, -.117, .237, -.157, .182)),
    Conditions = matrix(c(-.284, .056, .335, -.016, -.072, -.045, .020, .069, .039, -.036, .240, .118, .023, -.136, -.084, .000, .249, .021))
  ),
  v = setup$b(
    PSR = 0.890,
    NegAff = 0.665,
    PSQI3 = 8.065,
    OverallDiet = .440,
    F = .834
    ),
  k = setup$k(SES = function(x) x + 1),
  d = subset(p4final[[1]], !is.na(m1chadv) & !is.na(dSES)),
  nperobs = 1, seed = 1234)

tmp <- panelGraph(test2, test3, "-1 SD SES", "+1 SD SES", print=FALSE)

png(filename = "tmp_graphs/graph%03d.png", width = 3.5, height = 3.5, units = "in", res = 600)
for (i in 1:length(tmp)) {
  print(tmp[[i]] + theme(legend.position = "none"))
}
dev.off()

alcConvert <- function(none, frequent, heavy) {
  factor(ifelse(heavy == 1, "Heavy",
         ifelse(frequent == 1,
                "Frequent",
                ifelse(none == 1, "None", "Infrequent"))),
         levels = c("None", "Infrequent", "Frequent", "Heavy"))
}

panelGraph <- function(sim1, sim2, lab1, lab2, print = TRUE) {

  sim1$Alcohol <- with(sim1, alcConvert(CANone, CAFrequent, CAHeavy))
  sim2$Alcohol <- with(sim2, alcConvert(CANone, CAFrequent, CAHeavy))

  sim1$PhysAct <- factor(sim1$PhysAct, levels = 1:4,
                         labels = c("None", "Lowest Tertile",
                           "Middle Tertile", "Upper Tertile"))
  sim2$PhysAct <- factor(sim2$PhysAct, levels = 1:4,
                         labels = c("None", "Lowest Tertile",
                           "Middle Tertile", "Upper Tertile"))

  dat1a <- rbind(
    cbind.data.frame(Value = sim1$SES, Type = lab1),
    cbind.data.frame(Value = sim2$SES, Type = lab2))

  dat1b <- rbind(
    cbind.data.frame(Value = sim1$dSES, Type = lab1),
    cbind.data.frame(Value = sim2$dSES, Type = lab2))

  dat1c <- rbind(
    cbind.data.frame(Value = sim1$age, Type = lab1),
    cbind.data.frame(Value = sim2$age, Type = lab2))

  dat1d <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$sex))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$sex))), Type = lab2))

  dat2a <- rbind(
    cbind.data.frame(Value = sim1$PSR, Type = lab1),
    cbind.data.frame(Value = sim2$PSR, Type = lab2))

  dat2b <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$LifeStress))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$LifeStress))), Type = lab2))

  dat3 <- rbind(
    cbind.data.frame(Value = sim1$NegAff, Type = lab1),
    cbind.data.frame(Value = sim2$NegAff, Type = lab2))

  dat4a <- rbind(
    cbind.data.frame(Value = sim1$PSQI3, Type = lab1),
    cbind.data.frame(Value = sim2$PSQI3, Type = lab2))

  dat4b <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$PhysAct))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$PhysAct))), Type = lab2))

  dat4c <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$CurrentSmoke))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$CurrentSmoke))), Type = lab2))

  dat4d <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$RiskyBehaviors))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$RiskyBehaviors))), Type = lab2))

  dat4e <- rbind(
    cbind.data.frame(Value = sim1$OverallDiet, Type = lab1),
    cbind.data.frame(Value = sim2$OverallDiet, Type = lab2))

  dat4f <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$Alcohol))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$Alcohol))), Type = lab2))

  dat5 <- rbind(
    cbind.data.frame(Value = sim1$F, Type = lab1),
    cbind.data.frame(Value = sim2$F, Type = lab2))

  dat6 <- rbind(
    cbind(as.data.frame(prop.table(table(sim1$Conditions))), Type = lab1),
    cbind(as.data.frame(prop.table(table(sim2$Conditions))), Type = lab2))

  p1a <- ggplot(dat1a, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Socioeconomic Status") +
    theme_classic()

  p1b <- ggplot(dat1b, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab(expression(Delta~"SES")) +
    theme_classic()

  p1c <- ggplot(dat1c, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Age") +
    theme_classic()

  p1d <- ggplot(dat1d, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Sex") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic()

  p2a <- ggplot(dat2a, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Psychosocial Resources") +
    theme_classic()

  p2a <- ggplot(dat2a, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Psychosocial Resources") +
    theme_classic()

  p2b <- ggplot(dat2b, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Stressful Life Events") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic()

  p3 <- ggplot(dat3, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Negative Affect") +
    theme_classic()

  p4a <- ggplot(dat4a, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Sleep (PSQI)") +
    theme_classic()

  p4b <- ggplot(dat4b, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Physical Activity") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  p4c <- ggplot(dat4c, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Currently Smoke") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic()

  p4d <- ggplot(dat4d, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Risky Behaviors") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic()

  p4e <- ggplot(dat4e, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Overall Diet") +
    theme_classic()

  p4f <- ggplot(dat4f, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Alcohol Use") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  p5 <- ggplot(dat5, aes(Value, colour = Type)) +
    geom_density(size = 1.5, adjust = 1.5) +
    xlab("Allostatic Load") +
    theme_classic()

  p6 <- ggplot(dat6, aes(Var1, Freq, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Number of Major Conditions") +
    scale_y_continuous("Percent", labels = percent) +
    theme_classic()

  if (print) {
  grid.arrange(p1a, p1b, p1c, p1d,
               p2a, p2b,
               p3,
               p4a, p4b, p4c, p4d, p4e, p4f,
               p5,
               p6,
               ncol = 5)
  }

  return(invisible(list(
    p1a, p1b, p1c, p1d,
    p2a, p2b,
    p3,
    p4a, p4b, p4c, p4d, p4e, p4f,
    p5,
    p6)))
}

ggplot(rbind(
    cbind(as.data.frame(prop.table(table(test$Conditions))), Type = "True PSR"),
    cbind(as.data.frame(prop.table(table(subset(p4final[[1]], !is.na(m1chadv) & !is.na(dSES))$p4majorconditions))), Type = "Raw PSR")),
            aes(Var1, Freq, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Number of Major Conditions") +
  scale_y_continuous("Percent", labels = percent) +
  theme_bw()


ggplot(rbind(
    cbind(as.data.frame(prop.table(table(test$Conditions))), Type = "True PSR"),
    cbind(as.data.frame(prop.table(table(test2$Conditions))), Type = "Boosted PSR")),
            aes(Var1, Freq, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Number of Major Conditions") +
  scale_y_continuous("Percent", labels = percent) +
  theme_bw()

ggplot(rbind(
    cbind(as.data.frame(prop.table(table(test$PhysAct))), Type = "True PSR"),
    cbind(as.data.frame(prop.table(table(test2$PhysAct))), Type = "Boosted PSR")),
            aes(Var1, Freq, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Physical Activity") +
  scale_y_continuous("Percent", labels = percent) +
  theme_bw()



ggplot(rbind(cbind.data.frame(#SES = ifelse(test$SES > -1, "high SES", "low SES"),
                              Value = test$NegAff, Type = "True PSR"),
             cbind.data.frame(#SES = ifelse(test2$SES > -1, "high SES", "low SES"),
                              Value = test2$NegAff, Type = "Boosted PSR")),
            aes(Value, colour = Type)) +
  geom_density(size = 1.5, adjust = 1.5) +
  xlab("Negative Affect") +
  theme_bw()

ggplot(rbind(cbind.data.frame(#SES = ifelse(test$SES > -1, "high SES", "low SES"),
                              Value = test$PSQI, Type = "True PSR"),
             cbind.data.frame(#SES = ifelse(test2$SES > -1, "high SES", "low SES"),
                              Value = test2$PSQI, Type = "Boosted PSR")),
            aes(Value, colour = Type)) +
  geom_density(size = 1.5, adjust = 1.5) +
  xlab("PSQI") +
  theme_bw()


ggplot(rbind(cbind.data.frame(#SES = ifelse(test$SES > -1, "high SES", "low SES"),
                              Value = test$F, Type = "True PSR"),
             cbind.data.frame(#SES = ifelse(test2$SES > -1, "high SES", "low SES"),
                              Value = test2$F, Type = "Boosted PSR")),
            aes(Value, colour = Type)) +
  geom_density(size = 1.5, adjust = 1.5) +
  xlab("Allostatic Load") +
  theme_bw()


ggplot(rbind(cbind.data.frame(#SES = ifelse(test$SES > -1, "high SES", "low SES"),
                              Value = test$PSQI3, Type = "True PSR"),
             cbind.data.frame(#SES = ifelse(test2$SES > -1, "high SES", "low SES"),
                              Value = test2$PSQI3, Type = "Boosted PSR")),
            aes(Type, Value)) +
  stat_summary(fun.data = mean_cl_normal) +
  xlab("Allostatic Load") +
  theme_bw()



ggplot(rbind(cbind.data.frame(#SES = ifelse(test$SES > -1, "high SES", "low SES"),
                              Value = test$F, Type = "True PSR"),
             cbind.data.frame(#SES = ifelse(test2$SES > -1, "high SES", "low SES"),
                              Value = test2$F, Type = "Boosted PSR")),
            aes(Type, Value)) +
  stat_summary(fun.data = mean_cl_normal) +
  xlab("Allostatic Load") +
  theme_bw()




par(mfrow = c(2, 1))
barplot(table(test$Conditions))
barplot(table(test2$Conditions))



plotdat <- rbind(
cbind(as.data.frame(prop.table(table(simulated.values$Conditions))), Type = "Boosted PSR"),
cbind(subset(plotdat.old, Type == "Full Estimate")[, -3], Type = "True PSR"),
cbind(as.data.frame(prop.table(table(p4final[[1]]$p4majorconditions))), Type = "True"))



ggplot(subset(plotdat, Type != "True"), aes(factor(Var1), Freq * 1254, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Number of Major Conditions") +
  scale_y_continuous("Frequency", breaks = seq(from = 0, to = 500, by = 50)) +
  theme_bw()

plotdat.old <- plotdat
p.old <- p



grid.arrange(p + ggtitle("boosted SES"), p.old + ggtitle("True SES"))



x <- as.matrix(cbind(1, p4final[[1]][, c("b1pgender", "b1page_m2", "m1chadv", "dSES")],
                     PSR = simulated.values$PSR,
                     LifeStress = simulated.values$LifeStress,
                     PSRxSES = simulated.values$PSR *
                     ))
simulated.values$NegAff <- sim.normal(x, b = matrix(c(-0.164, 0.172, 0.141, 0.391, 0.320)), 0.909, 1)



xalt <- as.matrix(cbind(1, p4final[[1]][, c("b1pgender", "b1page_m2", "m1chadv", "dSES")]))
xalt[, "m1chadv"] <- xalt[, "m1chadv"] + .2
yhatalt <- sim.poisson(xalt, b = matrix(c(1.323, -0.154, 0.013, -0.350, -0.234)), 1)

plotting <- data.frame(
  Stress = c(p4final[[1]]$LifeStress, yhat, yhatalt),
  Type = rep(c("Actual", "Estimated", "EstimatedAlt"), each = 1254))

ggplot(droplevels(subset(plotting, Type != "Actual")), aes(Stress, colour = Type)) +
  #geom_bar(position = "dodge", width = 1/2) +
  geom_density(adjust = 2, size = 1) +
  theme_bw()

ggplot(data.frame(ActualStress = p4final[[1]]$LifeStress,
                  EstimatedStress = yhat,
                  EstiamtedStressAlt = yhatalt)) +
  geom_bar(aes(ActualStress), colour = "red") +
  geom_

par(mfrow = c(3, 1))
barplot(table(yhat))
barplot(table(p4final[[1]]$LifeStress))

x <- as.matrix(cbind(1, p4final[[1]][, c("b1pgender", "b1page_m2", "m1chadv", "dSES")]))
yhat2 <- sim.normal(x, b = matrix(c(-0.164, 0.172, 0.141, 0.391, 0.320)), 0.909, 1)

ggplot() +
  geom_density(aes(PSR), data = p4final[[1]], colour = "black") +
  geom_density(aes(PSR), data = data.frame(PSR = yhat2), colour = "blue") +
  theme_bw()


par(mfrow = c(2, 1))



barplot(table(yhat))
barplot(table(p4final[[1]]$LifeStress))


