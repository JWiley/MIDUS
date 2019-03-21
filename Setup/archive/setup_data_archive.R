

## summary(d2l$BT2)
## summary(d2l$RT2)
## ggplot(d2l[TST<9000], aes(TST/60, (RT2 - BT2 + 6))) +
##   geom_point(alpha = .3) +
##   coord_cartesian(xlim = c(0, 30), ylim = c(0, 30))



if (FALSE) {
## m2a.items.psrqsf <- as.data.table(as.data.frame(scale(m2a.items[,
##   c("M2", "LC2", "SE2", "Opt2", "PLife2", "Con3", "Ext4",
##     "FamS2", "FriS2", "SpoS3", "SI2", "SI1")])))
## tmp.items <- subset(midus.psr.items, Abbr %in% c("M", "LC", "SE", "Opt", "Pess", "FamS", "FriS", "SpoS", "SI", "Con", "Ext", "PLife", "Ctrl"))
## tmpdat <- m2a.items[, c("M2ID", "M2FAMNUM", tmp.items$ItemAbbr), with = FALSE]
## ##tmpdat$Died <- ifelse(tmpdat$M2ID %in% midus3.mort$M2ID, 1, 0)
## tmpdat$AGE <- c(d2$B1PAGE_M2, dm$BACRAGE)
## tmpdat$SEX <- c(d2$B1PRSEX, dm$BACRSEX) ## 1 = male, 2 = female
## ## 1 = white, 2 = AA, 3 = Other
## tmpdat$RACE <- c(ifelse(is.na(d2$B1PF7A), NA, ifelse(d2$B1PF7A == 1, 1L, ifelse(d2$B1PF7A == 2, 2L, 3L))),
##                  ifelse(is.na(dm$BACF7A), NA, ifelse(dm$BACF7A == 1, 1L, ifelse(dm$BACF7A == 2, 2L, 3L))))

## drop people missing all
dropindex <- rowMeans(is.na(dw[, midus.psr.items$MIDUS2, with = FALSE]))
table(dropindex == 1)

setkey(dw, M2FAMNUM, M2ID)
dw[, FamIndex := 1:.N, by = M2FAMNUM]

## validation
tmpdat.valid <- copy(dw)[dropindex != 1 & FamIndex == 2]
## development
tmpdat <- copy(dw)[dropindex != 1 & FamIndex == 1]


################################################################################
##                        Merge PSRS With Stress Data                         ##
################################################################################
d.all <- merge(d.all, psrscale_scores, by = "M2ID", all = TRUE)

tmp.d.all <- cbind(as.data.frame(winsorizor(d.all[,
  c("A1SBMI", "B1SBMI", "A1SWSTHI", "B1SWSTHI",
    "MFAbuse", "A1SPIWOR",
    "A1SHOMET", "A1SPIHOM", "A1SKINNE", "A1SFDSNE", "A1SSPCRI", "A1SPIFAM",
    "A1SLFEDI", "A1SDAYDI",
    "LifeStress",
    "LifeStressImpactS", "LifeStressImpactL", "B1SJOBDI", "B1SPIWOR",
    "B1SHOMET", "B1SPIHOM", "B1SKINNE", "B1SFDSNE", "B1SSPCRI", "B1SPIFAM",
    "B1SLFEDI", "B1SDAYDI",
    "B4QPS_PS", "CTQTotal",
    "dHomIn", "dNeiQu", "dFamIn", "dWorIn", "dDayDi", "dLfeDi",
    "dFriSt", "dFamSt", "dParSt",
    "PR", "SR", "PRSF", "SRSF",
    "A1SCHRON", "B1SCHRON", "b3tem", "exec_fxn",
    "Rb4bcrp", "b4bil6"
    )
   ], .005)),
  d.all[, c("A1SS7", "A1PA4", "A1PA5", "A1PA6", "B1PF7A", "B1PA1", "B1PA2", "B1PA3",
    "AGE", "Sex", "BirthYear", "DECEASED", "LastAge", "RaceG3")]
)

tmp.d.all <- within(tmp.d.all, {
  Sex <- factor(Sex, levels = 2:1, labels = c("Female", "Male"))
  A1SS7 <- factor(A1SS7, ordered = TRUE)
  A1PA4 <- factor(A1PA4, ordered = TRUE)
  A1PA5 <- factor(A1PA5, ordered = TRUE)
  A1PA6 <- factor(A1PA6, ordered = TRUE)
  B1PF7A <- factor(B1PF7A, ordered = TRUE)
  B1PA1 <- factor(B1PA1, ordered = TRUE)
  B1PA2 <- factor(B1PA2, ordered = TRUE)
  B1PA3 <- factor(B1PA3, ordered = TRUE)
})



















































mycombine <- function(formula, d, sex) {
  fit <- lapply(d, function(xd) {
    m <- summary(lm(formula, data = subset(xd, b1pgender == sex)))
    tmp <- coef(m)[, 1:2]
    r2 <- m$r.squared
    r2.se <- 1/(length(m$residuals) - 3)
    tmp <- rbind(tmp, cbind(atanh(r2), r2.se))
    rownames(tmp)[nrow(tmp)] <- "Rsquared"
    return(tmp)
  })

  out <- do.call(cbind, mi.pooled(
    lapply(fit, function(x) x[, 1]),
    lapply(fit, function(x) x[, 2])))

  out <- cbind(out, LL = out[, 1] - 1.96 * out[, 2])
  out <- cbind(out, UL = out[, 1] + 1.96 * out[, 2])
  out[nrow(out), c(1, 3, 4)] <- tanh(out[nrow(out), c(1, 3, 4)])

  return(out)
}

mres <- list(
  f = mycombine(scale(F) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  sym = mycombine(scale(bi.sym) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  hpa = mycombine(scale(bi.hpa) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  card = mycombine(scale(bi.card) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  gluc = mycombine(scale(bi.gluc) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  lipid = mycombine(scale(bi.lipid) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  infl = mycombine(scale(bi.infl) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"),
  para = mycombine(scale(bi.para) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "female"))

mres2 <- list(
  f = mycombine(scale(F) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  sym = mycombine(scale(bi.sym) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  hpa = mycombine(scale(bi.hpa) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  card = mycombine(scale(bi.card) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  gluc = mycombine(scale(bi.gluc) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  lipid = mycombine(scale(bi.lipid) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  infl = mycombine(scale(bi.infl) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"),
  para = mycombine(scale(bi.para) ~ scale(b1page_m2) + scale(PR + SR), psrbi.data, "male"))


mycombine2 <- function(f, fwo, d) {
  res <- mycombine(f, f, d = d)
  reswo <- mycombine(fwo, f, d = d)
  list(dR2 = res[nrow(res), 1] - reswo[nrow(reswo), 1],
       res = res, reswo = reswo)
}

mylm <- function(f, fwo, d) {
  d <- na.omit(d[zout(d[, all.vars(f)[1]]), all.vars(f)])
  summary(lm(f, data = d))$r.squared - summary(lm(fwo, data = d))$r.squared
}





mal <- mplusObject(
VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
 B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F
    b1page_m2;

CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

    CLUSTER = m2famnum;
    idvariable = m2id;

",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  ! PSR model
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery*1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt*1  friendsupt*1  spousesupt*1;
  PR@1; SR@1;

  ! AL model
    Radj_epi Radj_nor ON b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1page_m2;
    Rb4p1d ON b1page_m2;
    Radj_crt b4bdheas ON b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON b1page_m2;
    Rb4p1gs pulpress ON b1page_m2;
    Rb4bgluc p4homair b4bha1c ON b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON b1page_m2;

    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;

    F BY Radj_epi* Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;
    F@1;
    F WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    Rb4bgluc@0;

   F ON pr (psrf)
        sr (psrf);
   sym ON pr (psrs)
        sr (psrs);
   para ON pr (psrp)
        sr (psrp);
   hpa ON pr (psrh)
        sr (psrh);
   infl ON pr (psri)
        sr (psri);
   card ON pr (psrc)
        sr (psrc);
   gluc ON pr (psrg)
        sr (psrg);
   lipid ON pr (psrl)
        sr (psrl);

  MODEL TEST:
  PSRF = 0;
  psrs = 0;
  psrp = 0;
  psrh = 0;
  psri = 0;
  psrc = 0;
  psrg = 0;
  psrl = 0;

",
OUTPUT = "STDYX;",
usevariables = c(unlist(psr.items), unlist(vars),
    "m2famnum", "m2id", "b1pgender", "b1page_m2",
    "Agecat", "white", "p4majorconditions", "p4minorconditions",
    "p4sumburden", "b3tem", "exec_fxn", "compression",
    "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
rdata = within(d3m, {b1pgender <- as.numeric(b1pgender) - 1}))

mal.female <- mplusModeler(update(mal, rdata = subset(mal$rdata, b1pgender == 1)), "MIDUS_psrs_al_female.dat", run=TRUE)

mal.male <- mplusModeler(update(mal, rdata = subset(mal$rdata, b1pgender == 0)), "MIDUS_psrs_al_male.dat", run=TRUE)

mal.all <- mplusModeler(mal, "MIDUS_psrs_al_all.dat", run=TRUE)






mgal <- mplusObject(
VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr
 B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F
    b1page_m2;

CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D !B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

    GROUPING = b1pgender (0 = male 1 = female);

    CLUSTER = m2famnum;
    idvariable = m2id;

",
ANALYSIS = "
  TYPE = COMPLEX;
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  ! PSR model
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;


  ! AL model
    Radj_epi Radj_nor ON b1page_m2;
    avgb_sd avgb_rm avgb_hf avgb_lf ON b1page_m2;
    Rb4p1d ON b1page_m2;
    Radj_crt b4bdheas ON b1page_m2;
    Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel ON  b1page_m2;
    Rb4p1gs pulpress ON  b1page_m2;
    Rb4bgluc p4homair b4bha1c ON  b1page_m2;
    Rb4pwhr Rb4btrig b4bhdl b4bldl ON  b1page_m2;

    sym BY Radj_nor@1 Radj_epi@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    card BY Rb4p1gs@1 pulpress@1;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1 p4homair*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;

    F BY Radj_epi* Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;
    F@1;
    F WITH sym@0 para@0 hpa@0 infl@0
    card@0 gluc@0 lipid@0;
    Rb4bgluc@0;

   F sym para hpa infl card gluc lipid ON pr sr;

",
OUTPUT = "STDYX;",
usevariables = c(unlist(psr.items), unlist(vars),
    "m2famnum", "m2id", "b1pgender", "b1page_m2",
    "Agecat", "white", "p4majorconditions", "p4minorconditions",
    "p4sumburden", "b3tem", "exec_fxn", "compression",
    "bending", "impact", "reg_izallo", "GNNregAL", "RXNNregAL"),
rdata = within(d3m, {b1pgender <- as.numeric(b1pgender) - 1}))

mgal <- mplusModeler(mgal, "MIDUS_psrs_al_mg.dat", run=TRUE)






mg <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

  GROUPING = b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  MODEL OVERALL:
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;
",
OUTPUT = "STDYX; MODINDICES (ALL 200);",
usevariables = c("b1prsex", unlist(psr.items)),
rdata = d3m)

mg <- mplusModeler(mg, "MIDUS_psrs_mg.dat", run=TRUE)



m1 <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PSR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1
    familysupt  friendsupt  spousesupt;
",
OUTPUT = "STDYX;",
usevariables = unlist(psr.items),
rdata = d3)

m1 <- mplusModeler(m1, "MIDUS_psrs1.dat", run=TRUE)


m2 <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2 b1prsex;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", "b1prsex", unlist(psr.items)),
rdata = d3m)

m2 <- mplusModeler(m2, "MIDUS_psrs_AL.dat", run=TRUE)


########

m2.m <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

 !GROUPING is b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", unlist(psr.items)),
rdata = subset(d3m, b1prsex == 0))

m2.m <- mplusModeler(m2.m, "MIDUS_psrs_AL_males.dat", run=TRUE)

m2.f <- mplusObject(
VARIABLE = "
CATEGORICAL ARE B1SE4A B1SE4B B1SE4C B1SE4D
 B1SE4E B1SE4F B1SE4G B1SE4H B1SE4I B1SE4J
 B1SE4K B1SE4L B1SE4M B1SE4N B1SE4O B1SE4P
 B1SE4Q B1SE4R B1SE4S B1SE10A B1SE10B B1SE10C
 B1SE10D B1SE10E B1SE10F B1SJ2A B1SJ2B B1SJ2C
 B1SJ2D B1SJ4A B1SJ4B B1SJ4C B1SJ4D B1SL11A
 B1SL11B B1SL11C B1SL11D B1SL11E B1SL11F;

 !GROUPING is b1prsex (0 = male 1 = female);
",
ANALYSIS = "
  ESTIMATOR = WLSMV;
  PROCESSORS = 4;",
MODEL = "
  mastery BY B1SE4C@-1  B1SE4F*-1  B1SE4H*-1  B1SE4L;
  constraint BY B1SE4A@-1  B1SE4B  B1SE4D
    B1SE4E  B1SE4G  B1SE4J  B1SE4K  B1SE4I;
  selfesteem BY B1SE4M*-1  B1SE4N@-1
    B1SE4O  B1SE4P*-1  B1SE4Q  B1SE4R*-1  B1SE4S;
  optimism BY B1SE10A@-1  B1SE10C*-1  B1SE10F*-1;
  pessimism BY B1SE10B@-1  B1SE10D*-1  B1SE10E*-1;
  familysupt BY B1SJ2A@-1  B1SJ2B  B1SJ2C  B1SJ2D;
  friendsupt BY B1SJ4A@-1  B1SJ4B  B1SJ4C  B1SJ4D;
  spousesupt BY B1SL11A@-1  B1SL11B  B1SL11C
    B1SL11D  B1SL11E  B1SL11F;
  PR BY mastery@1  constraint*-1  optimism*1  pessimism*-1 selfesteem*1;
  SR BY familysupt  friendsupt  spousesupt;

  RXNNregAL ON b1page_m2;
  RXNNregAL ON PR (a)
               SR (b);

  MODEL TEST:
  a = 0;
  b = 0;
",
OUTPUT = "STDYX;",
usevariables = c("RXNNregAL", "b1page_m2", unlist(psr.items)),
rdata = subset(d3m, b1prsex == 1))

m2.f <- mplusModeler(m2.f, "MIDUS_psrs_AL_females.dat", run=TRUE)


compareModels(m2$results, m2b$results, diffTest=TRUE)

}
