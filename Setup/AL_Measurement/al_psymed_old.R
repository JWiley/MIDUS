
############################################################
#                         Archive                          #
############################################################


m.body$MGAL <- mplusObject(
  TITLE = "MG CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;
  GROUPING = Agecat (1 = young 2 = middle 3 = old);",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    sym BY Radj_nor@1 Radj_epi*.5;
    Radj_nor@0;
    hpa BY Radj_crt@1 b4bdheas*.6;
    Radj_crt@0;
    card BY Rb4p1gs@1 pulpress*1;
    Rb4p1gs@0;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*-1 hpa*1 card*1;
    [AL@0];
",
  OUTPUT = "STDYX; TECH1; MODINDICES (ALL);",
  usevariables = c(unlist(vars), "Agecat"), rdata = fdat)

cd(base, pre, num <- "_MGAL")
m$MGAL <- mplusModeler(m.body$MGAL, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)



m.body$AL <- mplusObject(
  TITLE = "CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    sym BY Radj_nor@1 Radj_epi*.5;
    Radj_nor@0;
    hpa BY Radj_crt@1 b4bdheas*.6;
    Radj_crt@0;
    card BY Rb4p1gs@1 pulpress*1;
    Rb4p1gs@0;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*-1 hpa*1 card*1;
",
  OUTPUT = "STDYX; TECH1; MODINDICES (ALL);",
  usevariables = unlist(vars), rdata = fdat)

cd(base, pre, num <- "_AL")
m$AL <- mplusModeler(m.body$AL, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)

compareModels(m$Fac$results, m$AL$results, show = "summaries", diffTest=TRUE)


m.body$AL2 <- mplusObject(
  TITLE = "CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
!    sym BY Radj_nor@1 Radj_epi*1;
!    Radj_nor@0;
!    hpa BY Radj_crt@1 b4bdheas*.6;
!    Radj_crt@0;
    card BY Rb4p1gs@1 pulpress*1;
    Rb4p1gs@0;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    Rb4bgluc WITH p4homair*;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;
    AL BY lipid@1 infl*1 gluc*1 para*-1 card*1 Radj_nor* Radj_crt*;
",
  OUTPUT = "STDYX; TECH1; MODINDICES (ALL);",
  usevariables = unlist(vars), rdata = fdat2)

cd(base, pre, num <- "_AL2")
m$AL2 <- mplusModeler(m.body$AL2, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)



m.body$Fac <- mplusObject(
  TITLE = "CFA for bio subsystems of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    sym BY Radj_nor@1 Radj_epi*1;
    Radj_nor@0;
    hpa BY Radj_crt@1 b4bdheas*-.6;
    Radj_crt@0;
    card BY Rb4p1gs@1 pulpress*1;
    Rb4p1gs@0;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_sd@0;
    avgb_rm WITH avgb_hf*;
",
  OUTPUT = "STDYX; TECH1;",
  usevariables = unlist(vars), rdata = fdat)

cd(base, pre, num <- "_Fac")
m$Fac <- mplusModeler(m.body$Fac, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)




m.body$One <- mplusObject(
  TITLE = "CFA for one factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
  AL BY Radj_epi*-1 Radj_nor*-1
  Radj_crt b4bdheas
  Rb4p1gs pulpress Rb4p1d
  p4homair Rb4bgluc b4bha1c
  b4bldl b4bhdl Rb4btrig Rb4pwhr
  Rb4bcrp@1 b4bil6 b4bfgn b4bsicam b4bsesel Rb4p1d
  avgb_rm avgb_hf avgb_sd avgb_lf Rb4p1d;

  avgb_rm WITH avgb_hf*;
  ",
  OUTPUT = "STDYX;",
  usevariables = unlist(vars), rdata = fdat)

cd(base, pre, num <- "_One")
m$One <- mplusModeler(m.body$One, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)


m.body$AL <- mplusObject(
  TITLE = "CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Radj_crt b4bdheas Rb4bcrp
    b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = BAYES;
    PROCESSORS = 4;
    BSEED = 6841;
    STVALUES = PERTURBED;
    CHAINS = 4;
    BITERATIONS = 50000 (5000);
    !THIN = 10;",
  MODEL = "
    sym BY Radj_epi@1 Radj_nor*.5;
    hpa BY Radj_crt@1 b4bdheas*.6;
    card BY Rb4p1gs@1 pulpress*1;
    gluc BY Rb4bgluc@1 p4homair*1  b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9 b4bsicam*.25 b4bsesel*.5;
    para BY avgb_rm@1 avgb_hf*1 avgb_sd*1 avgb_lf*1 Rb4p1d*-1;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*-1 hpa*1 card*1 (sl1-sl7);
    Radj_epi-Rb4pwhr WITH Radj_epi-Rb4pwhr (cov1-cov253);


  MODEL PRIORS:
!  sl2-sl7 ~ N(0, 10);
  !theta1-theta7 ~ N(0, 100);
  cov1-cov253 ~ IW(0, 50);
",
  OUTPUT = "TECH8; TECH1;",
  ## SAVEDATA = "
  ## FILE IS fs.dat;
  ## SAVE = FSCORES;",
  usevariables = unlist(vars), rdata = fdat2)


m.body$AL <- mplusObject(
  TITLE = "CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Rb4bcrp
    b4bil6 b4bfgn pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = BAYES;
    PROCESSORS = 4;
    BSEED = 6841;
    STVALUES = PERTURBED;
    CHAINS = 4;
    BITERATIONS = 50000 (5000);
    !THIN = 10;",
  MODEL = "
    sym BY Radj_epi@1 Radj_nor*.5;
    card BY Rb4p1gs@1 pulpress*1;
    gluc BY Rb4bgluc@1 p4homair*1  b4bha1c*1;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9;
    para BY avgb_rm@1 avgb_hf*1 avgb_sd*1 avgb_lf*1 Rb4p1d*-1;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*-1 card*1 (sl1-sl6);
    Radj_epi-Rb4pwhr WITH Radj_epi-Rb4pwhr (cov1-cov171);


  MODEL PRIORS:
  sl2-sl6 ~ N(0, 10);
  !theta1-theta7 ~ N(0, 100);
  cov1-cov171 ~ IW(0, 500);
",
  OUTPUT = "TECH8; TECH1;",
  ## SAVEDATA = "
  ## FILE IS fs.dat;
  ## SAVE = FSCORES;",
  usevariables = unlist(vars), rdata = fdat)


m.body$AL <- mplusObject(
  TITLE = "CFA for second order AL factor of biomarkers;",
  VARIABLE = "
  usevariables = Radj_epi Radj_nor avgb_sd avgb_rm avgb_lf
    avgb_hf Rb4bcrp
    b4bil6 b4bfgn pulpress Rb4p1gs Rb4p1d
    b4bha1c Rb4bgluc
    p4homair b4bldl b4bhdl Rb4btrig Rb4pwhr;",
  ANALYSIS = "
    ESTIMATOR = MLR;
    PROCESSORS = 4;",
  MODEL = "
    sym BY Radj_epi@1 Radj_nor*.5;
    Radj_epi@0;
    card BY Rb4p1gs@1 pulpress*1;
    RB4P1GS@0;
    gluc BY Rb4bgluc@1 p4homair*1 b4bha1c*1;
    gluc WITH b4bha1c*;
    lipid BY Rb4pwhr@1 Rb4btrig*.65 b4bhdl*-1 b4bldl*1;
    infl BY Rb4bcrp@1 b4bil6*1 b4bfgn*.9;
    para BY avgb_sd@1 avgb_rm*1 avgb_hf*1 avgb_lf*1 Rb4p1d*-1;
    avgb_rm WITH avgb_hf*;
    avgb_sd@0;
    AL BY lipid@1 infl*1 gluc*1 sym*1 para*-1 card*1 (sl1-sl6);
",
  OUTPUT = "STDYX; TECH1; MODINDICES;",
  ## SAVEDATA = "
  ## FILE IS fs.dat;
  ## SAVE = FSCORES;",
  usevariables = unlist(vars), rdata = fdat)


cd(base, pre, num <- "_AL")
m$AL <- mplusModeler(m.body$AL, paste0(pre, num, ".dat"),
  paste0(pre, num, ".inp"), run=TRUE)

rfoo(m$AL)


x <- as.matrix(fdat[, c(2, 27:41)])
mod <- lm(as.matrix(fdat[, 3:26]) ~ x)
r2 <- summary(mod)
r2 <- sapply(r2, function(m) m$adj.r.squared)

## vlist <- which(lower.tri(matrix(1, 24, 24)), arr.ind=TRUE)

## pdf(file="tmp.pdf")
## for(i in 1:nrow(vlist)) {
## print(ggplot(fdat, aes_string(x = unlist(vars)[vlist[i, 1]],
##   y = unlist(vars)[vlist[i, 2]])) +
##   geom_point() +
##   stat_smooth(method="loess", size=2, se=FALSE) +
##   stat_smooth(method="lm", colour="red", size = 2, se=FALSE))
## }
## dev.off()

m <- list()

m$cardio <- list(
  body = mplusObject(
  TITLE = "CFA for cardiovascular biomarkers;",
  VARIABLE = "
    USEVARIABLES = Rb4p1gs pulpress Rb4p1d;",
  ANALYSIS = "
  ESTIMATOR = MLR;
  PROCESSORS = 2;",
  MODEL = "
    cardiovascular BY Rb4p1gs*1 pulpress*.8 Rb4p1d*.05;
    cardiovascular@1
    Rb4p1gs*.05 (e1)
    pulpress*.3 (e2)
    Rb4p1d*1;
  MODEL CONSTRAINT:
    e1 > 0;
    e2 > 0;",
  OUTPUT = "
    STDYX;
    MODINDICES (ALL 10);",
  usevariables = colnames(fdat)[2:42],
  rdata = fdat),
  pre = "factor", num = "cardio")

m$glucose <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for metabolic glucose biomarkers;",
  VARIABLE = ~ "
    USEVARIABLES = Rb4bgluc b4bha1c p4homair;",
  MODEL = ~ "
    glucose BY Rb4bgluc@1 b4bha1c* p4homair*;
    Rb4bgluc@0;"),
  pre = "factor", num = "glucose")

m$lipids <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for metabolic lipids biomarkers;",
  VARIABLE = ~ "
    USEVARIABLES = b4bldl b4bhdl Rb4btrig Rb4pbmi Rb4pwhr;",
  MODEL = ~ "
    lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pbmi* Rb4pwhr*;"),
  pre = "factor", num = "lipids")

m$inflammation <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for inflammatory biomarkers;",
  VARIABLE = ~ "
    USEVARIABLES = Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel;",
  MODEL = ~ "
    inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;"),
  pre = "factor", num = "inflammation")

m$parasympathetic <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for parasympathetic biomarkers;",
  VARIABLE = ~ "
    USEVARIABLES = avgb_rm avgb_hf avgb_sd avgb_lf;",
  MODEL = ~ "
    parasympathetic BY avgb_rm@1 avgb_hf*1 avgb_sd*.8 avgb_lf*.8;
    avgb_sd WITH avgb_lf;"),
  pre = "factor", num = "parasympathetic")

m$EFA <- list(
  body = mplusObject(
  TITLE = "EFA for all 24 biomarkers;",
  ANALYSIS = "
    TYPE = EFA 1 8;
    PROCESSORS = 2;
    iterations = 100000;",
  PLOT = "
    type = plot2;",
  usevariables = colnames(fdat),
  rdata = fdat.train),
  pre = "factor", num = "EFA")

m$systems <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for first order biological system factors;",
  VARIABLE = ~ "",
  MODEL = ~ "
    sympathetic BY Radj_epi@-1 Radj_nor*-1;
    Radj_epi@0;
    hpa BY Radj_crt@-1 b4bdheas*-1;
    Radj_crt@0;

    cardiovascular BY Rb4p1gs@1 pulpress*.8 Rb4p1d*.05;
    Rb4p1gs@0;

    glucose BY p4homair@1 Rb4bgluc* b4bha1c*;

    lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pwhr* Rb4pbmi*;

    inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;

    parasympathetic BY avgb_rm@-1 avgb_hf*-1 avgb_sd*-.8 avgb_lf*-.8;
    avgb_sd WITH avgb_lf;",
  SAVEDATA = ~ "
    FILE IS fs.dat;
    SAVE = FSCORES;"),
  pre = "factor", num = "systems")

m$sysUnit<- list(
  body = update(m$cardio$body,
  TITLE = ~ "Unit weights for first order biological system factors;",
  VARIABLE = ~ "",
  MODEL = ~ "
    sympathetic BY Radj_epi@1 Radj_nor@1;
    hpa BY Radj_crt@1 b4bdheas@1;
    cardiovascular BY Rb4p1gs@1 pulpress@1 Rb4p1d@1;
    glucose BY p4homair@1 Rb4bgluc@1 b4bha1c@1;
    lipid BY b4bldl@1 b4bhdl@1 Rb4btrig@1 Rb4pwhr@1 Rb4pbmi@1;
    inflammation BY Rb4bcrp@1 b4bil6@1 b4bfgn@1 b4bsicam@1 b4bsesel@1;
    parasympathetic BY avgb_rm@1 avgb_hf@1 avgb_sd@1 avgb_lf@1;",
  SAVEDATA = ~ "
    FILE IS fs.dat;
    SAVE = FSCORES;"),
  pre = "factor", num = "sys_unitweight")


m$onefactor <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for all biomarkers load directly on AL;",
  VARIABLE = ~ "",
  MODEL = ~ "
  AL BY Radj_epi@1 Radj_nor*1.58 avgb_sd*-9 avgb_rm*-14 avgb_lf*-4
    avgb_hf*-5.6 Radj_crt*-.5 b4bdheas*-1.5 Rb4bcrp*.7 b4bil6*2.3
    b4bfgn*2 b4bsicam*2 b4bsesel*.7 pulpress*.6 Rb4p1gs*1.2
    Rb4p1d*3.5 b4bha1c*1.8 Rb4bgluc*2.3 p4homair*3
    b4bldl*3 b4bhdl*-2 Rb4btrig*2.2 Rb4pbmi*.4 Rb4pwhr*1.7;

  avgb_sd WITH avgb_lf;",
  SAVEDATA = ~ "
    FILE IS fs.dat;
    SAVE = FSCORES;"),
  pre = "factor", num = "onefactor")

m$AL <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for second order AL factor of biomarkers;",
  VARIABLE = ~ "",
  MODEL = ~ "
  Radj_epi-Rb4pwhr ON b1page_m2 fg_bpup-fg_rxchol;

  sympathetic BY Radj_epi@-1 Radj_nor*-1;
    Radj_epi@0;
  hpa BY Radj_crt@-1 b4bdheas*-1;
    Radj_crt@0;

  cardiovascular BY Rb4p1gs@1 pulpress*.8 Rb4p1d*.05;
    Rb4p1gs@0;

  glucose BY p4homair@1 Rb4bgluc* b4bha1c*;

  lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pwhr* Rb4pbmi*;

  inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;

  parasympathetic BY avgb_rm@-1 avgb_hf*-1 avgb_sd*-.8 avgb_lf*-.8;
  avgb_sd WITH avgb_lf;

  AL BY lipid* inflammation* glucose* sympathetic*
    parasympathetic* hpa* cardiovascular*;
  AL@1;",
  OUTPUT = ~ "
  STDYX;",
  SAVEDATA = ~"
  FILE IS fs.dat;
  SAVE = FSCORES;"),
  pre = "factor", num = "al")


m$bifactorAL <- list(
  body = update(m$cardio$body,
  TITLE = ~ "Bifactor CFA for biomarkers on systems and AL;",
  VARIABLE = ~ "",
  MODEL = ~ "
  sympathetic BY Radj_epi@-1 Radj_nor*-1;
    Radj_epi@0;
  hpa BY Radj_crt@-1 b4bdheas*-1;
    Radj_crt@0;

  cardiovascular BY Rb4p1gs@1 pulpress*.8 Rb4p1d*.05;
    Rb4p1gs@0;

  glucose BY p4homair@1 Rb4bgluc* b4bha1c*;

  lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pwhr* Rb4pbmi*;

  inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;

  parasympathetic BY avgb_rm@-1 avgb_hf*-1 avgb_sd*-.8 avgb_lf*-.8;
  avgb_sd WITH avgb_lf;

  AL WITH sympathetic@0 parasympathetic@0 hpa@0 inflammation@0
    cardiovascular@0 glucose@0 lipid@0;
  AL BY Radj_epi* Radj_nor avgb_sd avgb_rm avgb_lf avgb_hf Radj_crt
     b4bdheas Rb4bcrp b4bil6 b4bfgn b4bsicam b4bsesel pulpress Rb4p1gs Rb4p1d
     b4bha1c Rb4bgluc p4homair b4bldl b4bhdl Rb4btrig Rb4pbmi Rb4pwhr;
  AL@1;",
  OUTPUT = ~ "
  STDYX;",
  SAVEDATA = ~ "
  FILE IS fs.dat;
  SAVE = FSCORES;"),
  pre = "factor", num = "bifactoral")

m$ALfinal <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for second order AL factor of biomarkers;",
  VARIABLE = ~ "",
  MODEL = ~ "
  sympathetic BY Radj_epi@1 Radj_nor*1;
    Radj_epi@0;
  hpa BY Radj_crt@1 b4bdheas*1;
    Radj_crt@0;

  cardiovascular BY Rb4p1gs@1 pulpress*.8 Rb4p1d*.05;
    Rb4p1gs@0;

  glucose BY p4homair@1 Rb4bgluc* b4bha1c*;

  lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pwhr* Rb4pbmi*;

  inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;

  parasympathetic BY avgb_rm@1 avgb_hf*1 avgb_sd*.8 avgb_lf*.8;
  avgb_sd WITH avgb_lf;

  AL BY lipid* inflammation* glucose* sympathetic*
    parasympathetic* hpa* cardiovascular*;
  AL@1;

  ! modifications
   avgb_rm@0;
   AL BY Rb4pbmi*;
   inflammation BY Radj_nor*1 Rb4p1d*;
   parasympathetic BY Rb4p1d*;
   glucose BY Rb4pwhr*;

!  AL WITH Rb4p1d*;
   RADJ_NOR WITH B4BDHEAS;
   P4HOMAIR WITH GLUCOSE*;
   AVGB_HF  WITH AVGB_LF*;
   inflammation WITH lipid;
   PULPRESS WITH B4BDHEAS;",
  OUTPUT = ~ "
    STDYX;
    MODINDICES (ALL 50);",
  SAVEDATA = ~ "
  FILE IS fs.dat;
  SAVE = FSCORES;"),
  pre = "factor", num = "alfinal")

m$ALfinal2 <- list(
  body = update(m$cardio$body,
  TITLE = ~ "CFA for second order AL factor of biomarkers;",
  VARIABLE = ~ "",
  MODEL = ~ "
  sympathetic BY Radj_epi@1 Radj_nor*1;
    Radj_epi@0;
  hpa BY Radj_crt@1 b4bdheas*1;
    Radj_crt@0;

  cardiovascular BY Rb4p1gs@1 pulpress*.8 Rb4p1d*.05;
    Rb4p1gs@0;

  glucose BY p4homair@1 Rb4bgluc* b4bha1c*;

  lipid BY b4bldl@1 b4bhdl* Rb4btrig* Rb4pwhr* Rb4pbmi*;

  inflammation BY Rb4bcrp@1 b4bil6* b4bfgn* b4bsicam* b4bsesel*;

  parasympathetic BY avgb_rm@1 avgb_hf*1 avgb_sd*.8 avgb_lf*.8;
  avgb_sd WITH avgb_lf;

  AL BY lipid* inflammation* glucose* sympathetic*
    parasympathetic* hpa* cardiovascular*;
  AL@1;

  ! modifications
   avgb_rm@0;
   AL BY Rb4pbmi*;
   inflammation BY Radj_nor*1 Rb4p1d*;
   parasympathetic BY Rb4p1d* b4bdheas*;
   glucose BY Rb4pwhr*;

!  AL WITH Rb4p1d*;
   RADJ_NOR WITH B4BDHEAS;
   P4HOMAIR WITH GLUCOSE*;
   AVGB_HF  WITH AVGB_LF*;
   inflammation WITH lipid;
   PULPRESS WITH B4BDHEAS;",
  OUTPUT = ~ "
    STDYX;
    MODINDICES (ALL 50);",
  SAVEDATA = ~ "
  FILE IS fs.dat;
  SAVE = FSCORES;"),
  pre = "factor", num = "alfinal2")

m.bk <- m
m <- lapply(m, function(obj) {
  with(obj, {
    cd(base, pre, num)
    mplusModeler(body, paste0(pre, num, ".dat"),
      paste0(pre, num, ".inp"), run=TRUE, varwarnings=FALSE)
  })
})

index <- c(2, 6, 7, 8, 9, 10, 19, 20, 24:28)

sres <- do.call(cbind, lapply(m[1:5], function(x) t(x$results$summaries)))
sres <- rbind(sres[index, ], sres[-index, ])
write.table(sres, file="clipboard", sep="\t", col.names=FALSE)

sres2 <- do.call(cbind, lapply(m[c(7, 8, 9, 11)], function(x) t(x$results$summaries)))
sres2 <- rbind(sres2[index, ], sres2[-index, ])
write.table(sres2, file="clipboard", sep="\t", col.names=FALSE)


set.seed(40)
key <- sample(1:nrow(fdat))
fdat.train <- fdat[key[1:625], ]
fdat.test <- fdat[key[625:1255], ]

##   ! modifications
##    avgb_rm@0;
##    AL BY Rb4pbmi*;
##    inflammation BY Radj_nor*1 Rb4p1d*;
##    parasympathetic BY Rb4p1d*;
##    glucose BY Rb4pwhr*;

## !  AL WITH Rb4p1d*;
##    RADJ_NOR WITH B4BDHEAS;
##    P4HOMAIR WITH GLUCOSE*;
##    AVGB_HF  WITH AVGB_LF*;
##    inflammation WITH lipid;
##    PULPRESS WITH B4BDHEAS;

m$ALfinal <- within(m$ALfinal, {
  cd(base, pre, num)
  results <- mplusModeler(data = fdat,
    dataout = paste0(pre, num, ".dat"),
    title = title, body = body,
    modelout = paste0(pre, num, ".inp"),
    run=TRUE, varwarnings = FALSE)
})

paramExtract(m$systems$results$parameters$unstandardized, type = "v")
paramExtract(m$onefactor$results$parameters$unstandardized, type = "v")
paramExtract(m$AL$results$parameters$unstandardized, type = "v")
paramExtract(m$bifactorAL$results$parameters$unstandardized, type = "v")

compareModels(m$systems$results, m$AL$results, show="summaries", showNS=FALSE, diffTest=TRUE)
compareModels(m$systems$results, m$ALfinal$results, show="summaries", showNS=FALSE, diffTest=TRUE)

splot <- SEMSummary(~., data = m$systems$results$savedata[,
  c(25, 27, 29, 31, 33, 35, 37)])
plot(splot, points=FALSE)

index <- rowSums(is.na(fdat)) != 24

plot(dat$FsumALavg[index], m$ALfinal$results$savedata$AL)
cor(dat$FsumALavg[index], m$ALfinal$results$savedata$AL, use = "pairwise.complete.obs")

plot(dat$FsumALavg[index], m$AL$results$savedata$AL)
cor(dat$FsumALavg[index], m$AL$results$savedata$AL, use = "pairwise.complete.obs")

plot(refdat$G4NtotAL[index], m$AL$results$savedata$AL)
cor(refdat$G4NtotAL[index], m$AL$results$savedata$AL, use = "pairwise.complete.obs")


require(GGally)
ggpairs(na.omit(fdat), lower=list(continuous="smooth"))

F1 <- mplusModeler(
  data = fdat,
  dataout = paste0("factor_", num, ".dat"),
  title = "TITLE: factor analysis 1;",
  body = body,
  modelout = paste0("factor_", num, ".inp"),
  run=TRUE, varwarnings = FALSE)
