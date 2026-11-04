# ============================================================================
# 67d. Dinamiskais faktoru modelis paaudžu līmenī (docs/46 Slānis 11; docs/61 3e paplašinājums)
# ============================================================================
#
# JAUTĀJUMS: vai lasīšanas vides un iesaistes/labbūtības latentais stāvoklis Latvijā
# (latviešu valodas plūsmā) sāka kristies agrāk nekā prasmes stāvoklis, un vai tas dod
# ex ante brīdinājumu PISA 2025 paaudzei (dzimuši 2009) pirms 2025. gada?
#
# DATI (viss pa dzimšanas gadiem 1984–2013, relatīvi pret salīdzinājuma valstīm):
#   PRASME:   paaudžu panelis 67a (PISA lasīšana, PIRLS, TIMSS 4. kl., ICCS, ICILS; vidējais),
#             divvirzienu fiksētie efekti kā 67b, dalīts ar skolēnu SD (d vienības).
#   VIDE:     PISA anketas (grāmatas mājās, lasīšana priekam, daiļliteratūra, attieksmes,
#             mājas resursi) pa cikliem; IEA grāmatas mājās > 100 (4. un 8. kl.), PIRLS
#             lasīšana priekam. z pēc salīdzinājuma valstu izkliedes.
#   IESAISTE: PISA anketas (piepūle testā, piederība, kavēšana, dzīves apmierinātība,
#             skolotāju atbalsts, ekrāna laiks); HBSC Data Browser (patika pret skolu, skolas
#             spiediens, dzīves apmierinātība, atbalsts, miegs, sūdzības, ņirgāšanās, ekrāns)
#             11/13/15 gados → dz. gads = aptaujas gads − vecums. HBSC ir visa valsts, bez
#             valodas plūsmām; latviešu valodas plūsmai to lieto kā tuvinājumu.
# MODELIS (KFAS, katrai vienībai atsevišķi, hiperparametri kopīgi visām vienībām, ML):
#   y[j, b] = λ_j · α[bloks(j), b] + ε,  Var ε = SE_j² + σ²[avots]
#   α[b] = T α[b−1] + η,  Var η = Q (3×3); sākuma stāvoklis N(0, 10·I) (ne difūzs — sk. buvet)
#   M0 tikai prasme (67b analogs d vienībās); M1 diagonāla Q; M2 brīva Q (stāvokļu izmaiņu
#   korelācija); M3–M5 vadība: prasme[b] = prasme[b−1] + φ_V·vide[b−k] + φ_I·iesaiste[b−k]
#   (k = 1, 2, 3; T matricas ārpusdiagonāles elements ar nobīdi); M6 brīvas slodzes.
# EX ANTE: kā 67b/67c: katram PISA ciklam 2006–2025 tikai pirms testa publicētie dati;
#   prognoze = prasmes stāvoklis paaudzei b_T mīnus iepriekšējā PISA cikla atlikums.
# ABSOLŪTAIS VARIANTS (bez ciklu efekta): 7 valstu vidējā, EST un LVA A trajektorijas.
#
# Izvade: output/pisa2025/dfm_stavokli.csv, dfm_slodzes.csv, dfm_salidzinajums.csv,
#         dfm_panelis.csv, plots/67d_dfm_*.png, logs/67d_dinamiskais_faktoru_modelis_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(KFAS); library(ggplot2); library(patchwork); library(parallel) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/67d_dinamiskais_faktoru_modelis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260913); t00 <- Sys.time()
# Kešs: katrs ML novērtējums (parametri, logLik) glabājas .rds failā, lai pārtraukts skripts to nerēķinātu no jauna;
# novērtējumi neatkarīgi, tāpēc tos rēķina paralēli (dakšošana; katrs KFAS logLik ir viena pavediena)
KESS <- "output/pisa2025/dfm_cache"; dir.create(KESS, showWarnings = FALSE, recursive = TRUE)
MC <- max(1, min(8, detectCores() - 2))
cat("Paralēli pavedieni:", MC, "; kešs:", KESS, "\n")

VAL8 <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU"); SALID <- setdiff(VAL8, "LVA")
KOPAS <- list(`A: valstis` = VAL8, `B: LVA A` = c(SALID, "LVA A"))
YRS <- 1984:2013; NB <- length(YRS)
PISA_GADI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025); MERKI <- c(2006, 2009, 2012, 2015, 2018, 2022, 2025)
bdz <- function(T) floor(T - 15.8 + 0.5)
SDS <- c("PISA" = 88, "PIRLS lasīšana" = 68, "TIMSS4 matemātika" = 72, "TIMSS4 dabaszinātnes" = 72, "ICCS pilsoniskās zināšanas" = 80, "ICILS datorpratība" = 80)
BLOKI <- c(P = "prasme", V = "vide", I = "iesaiste")
P1V <- 10   # sākuma stāvokļa dispersija (sk. buvet)

# ---------------------------------------------------------------------------
# 1. Rādītāju bloki (neapstrādātas vērtības: vienība × pētījums × cikls)
# ---------------------------------------------------------------------------
cat(strrep("=", 100), "\n1. Rādītāju bloki\n", strrep("=", 100), "\n", sep = "")
wmean_se <- function(x, w, cl) {
  i <- !is.na(x) & !is.na(w) & w > 0; x <- x[i]; w <- w[i]; cl <- cl[i]
  if (length(x) < 100) return(c(NA_real_, NA_real_, length(x)))
  m <- sum(w * x) / sum(w); us <- tapply(w * (x - m), cl, sum); G <- length(us)
  c(m, sqrt(G / (G - 1) * sum(us^2)) / sum(w), length(x))
}

# 1a. Prasme: paaudžu panelis (67a)
PP <- fread("data/pisa2025/processed/paaudzu_panelis.csv")[radītājs == "vid" & is.finite(est) & is.finite(se)]
PP[, serija := fifelse(petijums == "PISA lasīšana", "PISA", petijums)]
PR <- PP[, .(bloks = "prasme", avots = fifelse(serija == "PISA", "PISA prasme", "IEA prasme"), serija, grupa, gads, est, se, dz_gads, publ, zime = 1)]

# 1b. Vide un iesaiste: PISA anketas (mikrodati, visi cikli)
PV <- list(
  L_GRAMATAS_MAJAS = c("vide", 1), L_LAIKS_PRIEKAM = c("vide", 1), L_IDX_LASITPRIEKS = c("vide", 1), L_MAT_DAILLITERATURA = c("vide", 1),
  L_ATT_TIKAI_JALASA = c("vide", -1), L_ATT_HOBIJS = c("vide", 1), AUTO_HEDRES = c("vide", 1), AUTO_CULTPOSS = c("vide", 1), L_MAJAS_DZEJA = c("vide", -1),
  E_PIEPULE_TESTAM = c("iesaiste", 1), AUTO_BELONG = c("iesaiste", 1), K_PIED_PIEDERU = c("iesaiste", 1), K_PIED_ATSTUMTS = c("iesaiste", -1),
  W_DZIVES_APMIERINATIBA = c("iesaiste", 1), S_KAVE_STUNDAS = c("iesaiste", -1), S_NOKAVE = c("iesaiste", -1), AUTO_TEACHSUP = c("iesaiste", 1),
  AUTO_DISCLIMA = c("iesaiste", 1), E_SKOLA_LAIKA_IZSKIESANA = c("iesaiste", -1), D_INTERNETS_DARBDIENA = c("iesaiste", -1), D_IERICES_BRIVLAIKS_DARBDIENA = c("iesaiste", -1))
# L_MAJAS_DZEJA: 1 = ir, 2 = nav → zīme −1
NOS <- c(L_GRAMATAS_MAJAS = "grāmatas mājās (kategorija)", L_LAIKS_PRIEKAM = "laiks lasīšanai priekam", L_IDX_LASITPRIEKS = "lasīšanas prieks (indekss)",
         L_MAT_DAILLITERATURA = "lasa daiļliteratūru", L_ATT_TIKAI_JALASA = "lasa tikai, ja jālasa (−)", L_ATT_HOBIJS = "lasīšana ir hobijs", AUTO_HEDRES = "mājas izglītības resursi",
         AUTO_CULTPOSS = "kultūras īpašumi mājās", L_MAJAS_DZEJA = "dzeja mājās", E_PIEPULE_TESTAM = "piepūle PISA testā", AUTO_BELONG = "piederība skolai (indekss)",
         K_PIED_PIEDERU = "jūtos piederīgs skolai", K_PIED_ATSTUMTS = "jūtos atstumts (−)", W_DZIVES_APMIERINATIBA = "dzīves apmierinātība", S_KAVE_STUNDAS = "kavē stundas (−)",
         S_NOKAVE = "nokavē (−)", AUTO_TEACHSUP = "skolotāju atbalsts", AUTO_DISCLIMA = "disciplīna stundās", E_SKOLA_LAIKA_IZSKIESANA = "skola ir laika izšķiešana (−)",
         D_INTERNETS_DARBDIENA = "internets darbdienā (−)", D_IERICES_BRIVLAIKS_DARBDIENA = "ierīces brīvajā laikā darbdienā (−)")
S <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")[, c("gads", "cnt", "plusma", "skola", "w", names(PV)), with = FALSE]
S <- S[!is.na(w) & w > 0 & cnt %in% VAL8][, gads := as.integer(gads)]
for (v in names(PV)) set(S, j = v, value = as.numeric(S[[v]]))
AN <- list()
for (gg in sort(unique(S$gads))) for (gr in c(VAL8, "LVA A")) {
  d <- if (gr == "LVA A") S[gads == gg & cnt == "LVA" & plusma %in% "A"] else S[gads == gg & cnt == gr]
  if (nrow(d) < 200) next
  AN[[paste(gg, gr)]] <- rbindlist(lapply(names(PV), function(v) {
    e <- wmean_se(d[[v]], d$w, d$skola); if (is.na(e[1]) || e[3] < 0.5 * nrow(d)) return(NULL)
    data.table(bloks = PV[[v]][1], avots = paste("PISA", PV[[v]][1]), serija = NOS[[v]], grupa = gr, gads = gg, est = e[1], se = e[2], dz_gads = gg - 15.8,
               publ = sprintf("%d-12", gg + 1), zime = as.numeric(PV[[v]][2]))
  }))
}
AN <- rbindlist(AN); rm(S); invisible(gc(verbose = FALSE))
cat("PISA anketu rādītāji: ", nrow(AN), " punkti, ", uniqueN(AN$serija), " sērijas\n", sep = "")

# 1c. IEA: grāmatas mājās > 100 (4. un 8. kl.), PIRLS lasīšana priekam
I <- readRDS("data/iea/processed/iea_skoleni.rds")[, .(st, g, cnt, skola, w, test_val, gram100, vm_ja, prieks_ja)]
I <- I[!(cnt == "DNW" & paste(st, g) %in% I[cnt == "DEU", unique(paste(st, g))])][, grupa := cnt][cnt == "DNW", grupa := "DEU"]
I[cnt == "LVA", lat := is.na(test_val) | test_val == "" | grepl("latv|<test language>", test_val, ignore.case = TRUE)]
PUBL <- c("PIRLS 2001" = "2003-04", "PIRLS 2006" = "2007-11", "PIRLS 2016" = "2017-12", "PIRLS 2021" = "2023-05", "TIMSS4 2003" = "2004-12", "TIMSS4 2007" = "2008-12",
          "TIMSS4 2019" = "2020-12", "TIMSS4 2023" = "2024-12", "ICCS 2009" = "2010-06", "ICCS 2016" = "2017-11", "ICCS 2022" = "2023-11", "ICILS 2023" = "2024-11")
IE <- list()
for (sg in unique(I[, paste(st, g)])) for (gr in c(VAL8, "LVA A")) {
  st0 <- sub(" .*", "", sg); g0 <- as.integer(sub(".* ", "", sg))
  d <- if (gr == "LVA A") I[st == st0 & g == g0 & cnt == "LVA" & lat & vm_ja %in% 1] else I[st == st0 & g == g0 & grupa == gr]
  if (nrow(d) < 200) next
  vec <- if (st0 %in% c("PIRLS", "TIMSS4")) 10.5 else 14.3
  for (v in c("gram100", "prieks_ja")) {
    e <- wmean_se(100 * d[[v]], d$w, d$skola); if (is.na(e[1]) || e[3] < 0.5 * nrow(d)) next
    IE[[paste(sg, gr, v)]] <- data.table(bloks = "vide", avots = "IEA vide", serija = paste0(st0, if (v == "gram100") ": grāmatas > 100 (%)" else ": lasa priekam (%)"),
                                         grupa = gr, gads = g0, est = e[1], se = e[2], dz_gads = g0 - vec, publ = PUBL[[sg]], zime = 1)
  }
}
IE <- rbindlist(IE); rm(I); invisible(gc(verbose = FALSE))
cat("IEA vides rādītāji: ", nrow(IE), " punkti, ", uniqueN(IE$serija), " sērijas\n", sep = "")

# 1d. HBSC Data Browser (visa valsts; 11/13/15 gadi; 2014/2018/2022)
HB <- fread("data/hbsc/hbsc_simptomi_8valstis.csv", encoding = "UTF-8")[cnt %in% VAL8 & !is.na(kopa)]
HV <- c(sch_satis = 1, sch_pressure = -1, lifesat_mean = 1, teachersup_cutoff = 1, studsup_cutoff = 1, d_sleep = -1, multihealth = -1, beenbullied_2_3 = -1,
        famsup_hi = 1, peersup_hi = 1, EMC_intensive = -1, SMPdum = -1)
HNOS <- c(sch_satis = "HBSC: patīk skola", sch_pressure = "HBSC: skolas spiediens (−)", lifesat_mean = "HBSC: dzīves apmierinātība", teachersup_cutoff = "HBSC: skolotāju atbalsts",
          studsup_cutoff = "HBSC: klasesbiedru atbalsts", d_sleep = "HBSC: miega grūtības (−)", multihealth = "HBSC: vairākas sūdzības (−)", beenbullied_2_3 = "HBSC: ņirgāšanās upuris (−)",
          famsup_hi = "HBSC: ģimenes atbalsts", peersup_hi = "HBSC: vienaudžu atbalsts", EMC_intensive = "HBSC: intensīva saziņa tiešsaistē (−)", SMPdum = "HBSC: problemātiski sociālie mediji (−)")
HB <- HB[HBSC_id %in% names(HV)][, `:=`(gads = as.integer(gads), n0 = 1500)]
# SE tuvinājums: procentiem binomiāls ar n ≈ 1500 vecuma grupā; vidējiem (0–10 skala) SD ≈ 1,8
HB[, se := fifelse(Measure_type == "MEAN_10", 1.8 / sqrt(n0), 100 * sqrt(pmin(pmax(kopa, 1), 99) / 100 * (1 - pmin(pmax(kopa, 1), 99) / 100) / n0))]
HPUBL <- c(`2014` = "2016-03", `2018` = "2020-05", `2022` = "2024-03")
HBD <- HB[, .(bloks = "iesaiste", avots = "HBSC iesaiste", serija = paste0(HNOS[HBSC_id], " ", vecums, " g."), grupa = cnt, gads, est = kopa, se, dz_gads = gads - vecums - 0.5,
              publ = HPUBL[as.character(gads)], zime = HV[HBSC_id])]
HBD <- rbind(HBD, HBD[grupa == "LVA"][, grupa := "LVA A"])   # tuvinājums: valsts kopā kā LVA A mērījums
cat("HBSC iesaistes rādītāji: ", nrow(HBD[grupa != "LVA A"]), " punkti, ", uniqueN(HBD$serija), " sērijas (vecums iekļauts sērijā)\n", sep = "")

RAW <- rbind(PR, AN, IE, HBD)[, b := floor(dz_gads + 0.5)][b %in% YRS]
RAW[, est := est * zime]   # visas sērijas: lielāks = labāk
cat("\nKopā neapstrādāto punktu: ", nrow(RAW), "\n", sep = "")
print(RAW[, .(punkti = .N, serijas = uniqueN(serija), vienibas = uniqueN(grupa), dz_gadi = paste(range(b), collapse = "–")), by = .(bloks, avots)], row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. Relatīvās vērtības, standartizācija, panelis pa kopām
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n2. Panelis: divvirzienu fiksētie efekti un standartizācija\n", strrep("=", 100), "\n", sep = "")
# Standartizācijas SD: cikla-demeanotu vērtību izkliede pa 8 valstīm × cikliem (A kopa), katrai sērijai
SDZ <- RAW[grupa %in% VAL8, { X <- copy(.SD)[, sc := paste(serija, gads)]; X[, rel := est - mean(est), by = sc]; .(sd_rel = sd(X$rel), n = nrow(X)) }, by = .(bloks, serija)]
SDZ[bloks == "prasme", sd_rel := SDS[serija]]
buvet_paneli <- function(vien) {
  X <- RAW[grupa %in% vien][, `:=`(sc = paste(serija, gads), ks = paste(grupa, serija))]
  X <- X[, if (uniqueN(grupa) >= 5) .SD else NULL, by = sc]   # cikla efektu var noteikt tikai, ja ir vairākas valstis
  fe_r <- function(est, se, sc, ks) {   # divvirzienu fiksētie efekti; sērijai ar vienu ciklu paliek tikai vienības efekts (atlikums 0)
    w <- 1 / (se^2 + 1)
    f <- if (uniqueN(sc) >= 2 && uniqueN(ks) >= 2) lm(est ~ 0 + factor(sc) + factor(ks), weights = w) else if (uniqueN(ks) >= 2) lm(est ~ 0 + factor(ks), weights = w) else lm(est ~ 1, weights = w)
    est - fitted(f)
  }
  X[, r := fe_r(est, se, sc, ks), by = serija]
  X <- merge(X, SDZ[, .(serija, sd_rel)], by = "serija")
  X[, `:=`(y = r / sd_rel, se_z = se / sd_rel)]
  X[, .(grupa, bloks, avots, serija, gads, b, y, se = se_z, publ)]
}
PAN <- rbindlist(lapply(names(KOPAS), function(kn) buvet_paneli(KOPAS[[kn]])[, kopa := kn]))
fwrite(PAN, "output/pisa2025/dfm_panelis.csv")
cat("Panelis: ", nrow(PAN), " rindas (abas kopas)\n", sep = "")
cat("\nLatviešu valodas plūsma (B kopa): relatīvā z vērtība pa blokiem un dzimšanas gadiem (vidējais pa sērijām)\n")
print(dcast(PAN[kopa == "B: LVA A" & grupa == "LVA A", .(z = round(mean(y), 2)), by = .(bloks, b)], b ~ bloks, value.var = "z"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. Modelis
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n3. Dinamiskais faktoru modelis (KFAS)\n", strrep("=", 100), "\n", sep = "")
# Sērijas, to bloki un trokšņa grupas (prasmei katrai sērijai sava σ² kā 67b; citiem pa avotiem)
ser_info <- function(X) {
  si <- unique(X[, .(serija, bloks, avots)])[order(bloks, avots, serija)]
  si[, s2g := fifelse(bloks == "prasme", serija, avots)]
  si[, anchor := serija == c(prasme = "PISA", vide = "grāmatas mājās (kategorija)", iesaiste = "piepūle PISA testā")[bloks]]
  si[, lamg := sub(" \\d+ g\\.$", "", serija)]   # brīvo slodžu grupa: HBSC rādītājam viena slodze visiem vecumiem
  si
}
# Parametru izkārtojums
spec_par <- function(si, q_tips, lead_k, brivas, brP = FALSE) {
  nq <- if (q_tips == "P") 1 else if (q_tips == "diag") 3 else 6
  s2g <- unique(si$s2g); lam <- if (brivas) unique(si[anchor == FALSE & (brP | bloks != "prasme"), lamg]) else character(0)
  list(nq = nq, q_tips = q_tips, s2g = s2g, lead_k = lead_k, lam = lam,
       n = nq + length(s2g) + (if (lead_k > 0) 2 else 0) + length(lam))
}
unpack <- function(par, sp) {
  i <- 0
  if (sp$q_tips == "P") { Q <- matrix(exp(par[1]), 1, 1); i <- 1 }
  else if (sp$q_tips == "diag") { Q <- diag(exp(par[1:3])); i <- 3 }
  else { L <- matrix(0, 3, 3); diag(L) <- exp(par[1:3]); L[2, 1] <- par[4]; L[3, 1] <- par[5]; L[3, 2] <- par[6]; Q <- L %*% t(L); i <- 6 }
  s2 <- setNames(exp(par[i + seq_along(sp$s2g)]), sp$s2g); i <- i + length(sp$s2g)
  phi <- c(0, 0); if (sp$lead_k > 0) { phi <- par[i + 1:2]; i <- i + 2 }
  lam <- setNames(rep(1, length(sp$lam)), sp$lam); if (length(sp$lam)) lam[] <- par[i + seq_along(sp$lam)]
  list(Q = Q, s2 = s2, phi = phi, lam = lam)
}
start_par <- function(sp) {
  p <- if (sp$q_tips == "P") log(0.003) else if (sp$q_tips == "diag") log(c(0.003, 0.03, 0.03)) else c(log(sqrt(c(0.003, 0.03, 0.03))), 0, 0, 0)
  p <- c(p, log(ifelse(grepl("PISA$|PIRLS|TIMSS|ICCS|ICILS", sp$s2g) & !grepl("vide|iesaiste", sp$s2g), 0.003, 0.15)))
  if (sp$lead_k > 0) p <- c(p, 0, 0)
  c(p, rep(1, length(sp$lam)))
}
buvet <- function(Xk, si, th, sp) {
  p <- nrow(si); pr <- sp$q_tips == "P"; m0 <- if (pr) 1 else 3; k <- sp$lead_k
  m <- if (k >= 2) m0 + 2 * (k - 1) else m0
  Y <- matrix(NA_real_, NB, p); H <- array(0, c(p, p, NB)); for (j in 1:p) H[j, j, ] <- 1
  if (nrow(Xk)) {
    a <- Xk[, .(y = mean(y), se2 = mean(se^2)), by = .(b, serija)]
    for (i in seq_len(nrow(a))) { t <- match(a$b[i], YRS); j <- match(a$serija[i], si$serija)
      if (!is.na(t) && !is.na(j)) { Y[t, j] <- a$y[i]; H[j, j, t] <- a$se2[i] + th$s2[[si$s2g[j]]] } }
  }
  Z <- matrix(0, p, m); bi <- match(si$bloks, BLOKI)
  for (j in 1:p) Z[j, if (pr) 1 else bi[j]] <- if (si$lamg[j] %in% names(th$lam)) th$lam[[si$lamg[j]]] else 1
  Tm <- diag(m); R <- matrix(0, m, m0); R[1:m0, 1:m0] <- diag(m0)
  if (k >= 1 && !pr) {
    if (k == 1) { Tm[1, 2] <- th$phi[1]; Tm[1, 3] <- th$phi[2] }
    else { # nobīdes stāvokļi: V[b−1], I[b−1], V[b−2], I[b−2], ...
      for (l in 1:(k - 1)) { iv <- 3 + 2 * (l - 1) + 1; ii <- iv + 1
        if (l == 1) { Tm[iv, 2] <- 1; Tm[ii, 3] <- 1 } else { Tm[iv, iv - 2] <- 1; Tm[ii, ii - 2] <- 1 } }
      ivk <- 3 + 2 * (k - 2) + 1; Tm[1, ivk] <- th$phi[1]; Tm[1, ivk + 1] <- th$phi[2] }
  }
  # Sākuma stāvoklis: N(0, P1V) pamatstāvokļiem (P1V = 10 ≫ stāvokļu mērogs, praktiski plakans), nobīdes stāvokļi sāk no 0.
  # Nevis difūzs (P1inf): ar brīvām slodzēm difūzā ticamība ir pārrāvumaina pie λ → 0 (F_inf kļūst singulāra, un ticamības
  # zars pārslēdzas), un BFGS šos lēcienus "izmanto" — pirmajos laidienos M6/M7 deva logLik +80…+100 pie tiem pašiem parametriem.
  P1 <- diag(c(rep(P1V, m0), rep(0, m - m0)), m)
  SSModel(Y ~ -1 + SSMcustom(Z = Z, T = Tm, R = R, Q = th$Q, a1 = rep(0, m), P1 = P1), H = H)
}
ll_vien <- function(X, vien, si, th, sp) {
  sum(sapply(vien, function(k) { ll <- tryCatch(logLik(buvet(X[grupa == k], si, th, sp)), error = function(e) -1e10); if (is.finite(ll)) ll else -1e10 }))
}
novertet <- function(X, vien, q_tips = "full", lead_k = 0, brivas = FALSE, hess = FALSE, brP = FALSE, start = NULL) {
  if (q_tips == "P") X <- X[bloks == "prasme"]
  si <- ser_info(X); sp <- spec_par(si, q_tips, lead_k, brivas, brP)
  nll <- function(par) { th <- unpack(par, sp); if (any(!is.finite(unlist(th)))) return(1e10); -ll_vien(X, vien, si, th, sp) }
  o <- optim(if (is.null(start)) start_par(sp) else start, nll, method = "BFGS", control = list(maxit = if (brivas) 150 else 300, reltol = 1e-8), hessian = hess)
  th <- unpack(o$par, sp); se_par <- rep(NA_real_, sp$n)
  if (hess) { se_par <- tryCatch(sqrt(diag(solve(o$hessian))), error = function(e) rep(NA_real_, sp$n)) }
  list(si = si, sp = sp, th = th, par = o$par, se_par = se_par, ll = -o$value, npar = sp$n, n_obs = nrow(X), conv = o$convergence)
}
# Novērtējums ar kešu: ja .rds jau ir, to nolasa (tas pats datu apjoms un modelis), citādi rēķina un saglabā
kesa_fails <- function(kess) file.path(KESS, paste0(gsub("[^A-Za-z0-9]+", "_", kess), ".rds"))
novertet_k <- function(kess, ...) {
  f <- kesa_fails(kess)
  if (file.exists(f)) { h <- readRDS(f); h$no_kesa <- TRUE; return(h) }
  t0 <- Sys.time(); h <- novertet(...); h$min <- as.numeric(difftime(Sys.time(), t0, units = "mins")); h$no_kesa <- FALSE
  saveRDS(h, f); h
}
izlidzinat <- function(X, vien, h) {
  rbindlist(lapply(vien, function(k) {
    Xk <- X[grupa == k]; m0 <- if (h$sp$q_tips == "P") 1 else 3
    ks <- tryCatch(KFS(buvet(Xk, h$si, h$th, h$sp), smoothing = "state"), error = function(e) NULL)
    rb <- Xk[, .(lo = min(b), hi = max(b)), by = bloks]
    rbindlist(lapply(1:m0, function(j) { bl <- if (m0 == 1) "prasme" else BLOKI[[j]]; r <- rb[bloks == bl]
      v <- if (is.null(ks)) rep(NA_real_, NB) else as.numeric(ks$V[j, j, ])
      data.table(grupa = k, bloks = bl, b = YRS, theta = if (is.null(ks)) NA_real_ else as.numeric(ks$alphahat[, j]),
                 se = fifelse(is.finite(v), sqrt(pmax(v, 0)), NA_real_), dati = if (nrow(r)) YRS >= r$lo & YRS <= r$hi else FALSE) }))
  }))
}
apraksts <- function(h) {
  th <- h$th; s <- sprintf("logLik %.1f, parametri %d, AIC %.1f, novērojumi %d, konverģence %d", h$ll, h$npar, -2 * h$ll + 2 * h$npar, h$n_obs, h$conv)
  if (h$sp$q_tips != "P") { Qc <- cov2cor(th$Q); s <- paste0(s, sprintf("\n  Q diag (stāvokļa izmaiņas dispersija gadā): prasme %.4f, vide %.4f, iesaiste %.4f", th$Q[1, 1], th$Q[2, 2], th$Q[3, 3]))
    if (h$sp$q_tips == "full") s <- paste0(s, sprintf("\n  stāvokļu izmaiņu korelācija: prasme–vide %.2f, prasme–iesaiste %.2f, vide–iesaiste %.2f", Qc[1, 2], Qc[1, 3], Qc[2, 3])) }
  else s <- paste0(s, sprintf("\n  q (prasme) %.4f", th$Q[1, 1]))
  if (h$sp$lead_k > 0) { i <- h$sp$nq + length(h$sp$s2g) + 1:2
    s <- paste0(s, sprintf("\n  vadība k = %d: φ_vide %.3f (SE %.3f), φ_iesaiste %.3f (SE %.3f)", h$sp$lead_k, th$phi[1], h$se_par[i[1]], th$phi[2], h$se_par[i[2]])) }
  s
}

MODELI <- list(`M0 tikai prasme (67b)` = list(q = "P", k = 0, br = FALSE), `M1 DFM diagonāla Q` = list(q = "diag", k = 0, br = FALSE),
               `M2 DFM brīva Q` = list(q = "full", k = 0, br = FALSE), `M3 DFM vadība k=1` = list(q = "full", k = 1, br = FALSE),
               `M4 DFM vadība k=2` = list(q = "full", k = 2, br = FALSE), `M5 DFM vadība k=3` = list(q = "full", k = 3, br = FALSE),
               `M6 DFM brīvas slodzes` = list(q = "full", k = 0, br = TRUE), `M7 DFM brīvas slodzes arī prasmei` = list(q = "full", k = 0, br = TRUE, brP = TRUE))

# ---------------------------------------------------------------------------
# 4. Pilnie dati: stāvokļi, slodzes, salīdzinājums
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n4. Pilnie dati (līdz PISA 2025): novērtējumi pa kopām un modeļiem\n", strrep("=", 100), "\n", sep = "")
FIT <- list(); ST <- list(); SAL <- list(); SLO <- list()
UZD <- list(); for (kn in names(KOPAS)) for (mn in names(MODELI)) {
  if (kn == "A: valstis" && !mn %in% c("M0 tikai prasme (67b)", "M2 DFM brīva Q", "M3 DFM vadība k=1")) next
  UZD[[paste(kn, mn)]] <- list(kn = kn, mn = mn) }
t0 <- Sys.time()
FIT <- mclapply(UZD, function(u) { m <- MODELI[[u$mn]]
  novertet_k(paste("pilnie", u$kn, u$mn), PAN[kopa == u$kn], KOPAS[[u$kn]], m$q, m$k, m$br, hess = m$k > 0 || m$br, brP = isTRUE(m$brP)) },
  mc.cores = MC, mc.preschedule = FALSE)
cat(sprintf("Novērtēti %d modeļi: %.1f min (no keša: %d)\n", length(FIT), as.numeric(difftime(Sys.time(), t0, units = "mins")), sum(sapply(FIT, function(h) isTRUE(h$no_kesa)))))
# Ligzdotu modeļu pārbaude: M3–M5 ietver M2 (φ = 0), M7 ietver M6 (prasmes slodzes = 1); ja ietverošā modeļa logLik ir zemāks,
# BFGS ir apstājies lokālā optimumā, un modeli pārrēķina ar ietvertā modeļa parametriem kā sākuma vērtībām (φ = 0, jaunās slodzes = 1)
LIGZDAS <- list(c("M3 DFM vadība k=1", "M2 DFM brīva Q"), c("M4 DFM vadība k=2", "M2 DFM brīva Q"), c("M5 DFM vadība k=3", "M2 DFM brīva Q"),
                c("M7 DFM brīvas slodzes arī prasmei", "M6 DFM brīvas slodzes"))
for (kn in names(KOPAS)) for (lg in LIGZDAS) {
  hb <- FIT[[paste(kn, lg[1])]]; ha <- FIT[[paste(kn, lg[2])]]; if (is.null(hb) || is.null(ha) || hb$ll >= ha$ll - 1e-6) next
  cat(sprintf("\n[%s] %s ietver %s, bet logLik %.1f < %.1f: lokāls optimums; pārrēķina ar %s sākuma vērtībām\n", kn, lg[1], lg[2], hb$ll, ha$ll, lg[2]))
  m <- MODELI[[lg[1]]]; X <- PAN[kopa == kn]; si <- ser_info(X); sp <- spec_par(si, m$q, m$k, m$br, isTRUE(m$brP)); spa <- ha$sp
  st <- ha$par[seq_len(spa$nq + length(spa$s2g))]; if (m$k > 0) st <- c(st, 0, 0)
  if (m$br) { lam0 <- setNames(rep(1, length(sp$lam)), sp$lam); lam0[names(ha$th$lam)] <- ha$th$lam; st <- c(st, unname(lam0)) }
  stopifnot(length(st) == sp$n); file.remove(kesa_fails(paste("pilnie", kn, lg[1])))
  hj <- novertet_k(paste("pilnie", kn, lg[1]), X, KOPAS[[kn]], m$q, m$k, m$br, hess = m$k > 0 || m$br, brP = isTRUE(m$brP), start = st)
  cat(sprintf("  pēc pārrēķina: logLik %.1f (%.1f min)\n", hj$ll, hj$min))
  if (hj$ll < hb$ll) { hj <- hb; saveRDS(hb, kesa_fails(paste("pilnie", kn, lg[1]))); cat("  sliktāks par iepriekšējo — paliek iepriekšējais\n") }
  FIT[[paste(kn, lg[1])]] <- hj
}
# Pretējā pārbaude: ja ietverošais modelis pēc pārrēķina ir manāmi labāks (> 1 logLik ar 2–5 papildu parametriem) par ietverto,
# arī ietvertais varēja būt lokālā optimumā; to pārrēķina ar ietverošā modeļa parametriem (φ un liekās slodzes atmet)
for (kn in names(KOPAS)) for (lg in LIGZDAS) {
  hb <- FIT[[paste(kn, lg[1])]]; ha <- FIT[[paste(kn, lg[2])]]; if (is.null(hb) || is.null(ha) || hb$ll <= ha$ll + 1) next
  ma <- MODELI[[lg[2]]]; X <- PAN[kopa == kn]; si <- ser_info(X); spa <- spec_par(si, ma$q, ma$k, ma$br, isTRUE(ma$brP))
  st <- hb$par[seq_len(spa$nq + length(spa$s2g))]; if (ma$k > 0) st <- c(st, hb$th$phi)
  if (ma$br) st <- c(st, unname(hb$th$lam[spa$lam]))
  stopifnot(length(st) == spa$n, all(is.finite(st)))
  cat(sprintf("\n[%s] %s pēc pārrēķina (logLik %.1f) ir labāks par ietverto %s (%.1f): arī tas bija lokālā optimumā; pārrēķina ar %s sākuma vērtībām\n", kn, lg[1], hb$ll, lg[2], ha$ll, lg[1]))
  file.remove(kesa_fails(paste("pilnie", kn, lg[2])))
  hj <- novertet_k(paste("pilnie", kn, lg[2]), X, KOPAS[[kn]], ma$q, ma$k, ma$br, hess = ma$k > 0 || ma$br, brP = isTRUE(ma$brP), start = st)
  cat(sprintf("  pēc pārrēķina: logLik %.1f (%.1f min)\n", hj$ll, hj$min))
  # ja ietverošā modeļa parametri ietvertajam ir slikts sākums (piem., M7 prasmes slodzes tālu no 1), paliek iepriekšējais
  if (hj$ll < ha$ll) { hj <- ha; saveRDS(ha, kesa_fails(paste("pilnie", kn, lg[2]))); cat("  sliktāks par iepriekšējo — paliek iepriekšējais; abi modeļi ir vietējos optimumos\n") }
  FIT[[paste(kn, lg[2])]] <- hj
}
for (u in UZD) { kn <- u$kn; mn <- u$mn; m <- MODELI[[mn]]; vien <- KOPAS[[kn]]; X <- PAN[kopa == kn]; h <- FIT[[paste(kn, mn)]]
  if (inherits(h, "try-error") || is.null(h$th)) stop("Novērtējums neizdevās: ", kn, " | ", mn, "\n", as.character(h))
  cat(sprintf("\n[%s | %s] %.1f min%s\n  %s\n", kn, mn, if (is.null(h$min)) NA else h$min, if (isTRUE(h$no_kesa)) " (no keša)" else "", apraksts(h)))
  ST[[paste(kn, mn)]] <- izlidzinat(X, vien, h)[, `:=`(kopa = kn, modelis = mn)]
  Qc <- if (m$q == "full") cov2cor(h$th$Q) else diag(3)
  i_phi <- if (m$k > 0) h$sp$nq + length(h$sp$s2g) + 1:2 else c(NA, NA)
  SAL[[paste(kn, mn)]] <- data.table(kopa = kn, modelis = mn, logLik = h$ll, npar = h$npar, AIC = -2 * h$ll + 2 * h$npar, n_obs = h$n_obs, konv = h$conv,
                                     q_prasme = h$th$Q[1, 1], q_vide = if (m$q == "P") NA else h$th$Q[2, 2], q_iesaiste = if (m$q == "P") NA else h$th$Q[3, 3],
                                     kor_PV = Qc[1, 2], kor_PI = Qc[1, 3], kor_VI = Qc[2, 3], k = m$k, phi_vide = h$th$phi[1], phi_vide_se = if (m$k > 0) h$se_par[i_phi[1]] else NA,
                                     phi_iesaiste = h$th$phi[2], phi_iesaiste_se = if (m$k > 0) h$se_par[i_phi[2]] else NA)
  lam_se <- if (m$br) h$se_par[h$sp$nq + length(h$sp$s2g) + seq_along(h$sp$lam)] else NA
  NP <- X[, .N, by = serija]
  SLO[[paste(kn, mn)]] <- h$si[, .(kopa = kn, modelis = mn, bloks, avots, serija, slodze = if (m$br) fifelse(anchor, 1, h$th$lam[lamg]) else 1,
                                   slodze_se = if (m$br) fifelse(anchor, 0, lam_se[match(lamg, h$sp$lam)]) else NA_real_,
                                   sigma2 = h$th$s2[s2g], n_punkti = NP$N[match(serija, NP$serija)])]
}
ST <- rbindlist(ST); SAL <- rbindlist(SAL); SLO <- rbindlist(SLO)
fwrite(SLO, "output/pisa2025/dfm_slodzes.csv")

cat("\nSlodzes un σ² (B kopa, M6 un M7 brīvas slodzes; enkurs = 1: PISA, grāmatas mājās, piepūle testā; HBSC rādītājam viena slodze visiem vecumiem):\n")
print(merge(SLO[kopa == "B: LVA A" & modelis == "M6 DFM brīvas slodzes", .(bloks, serija = sub(" \\d+ g\\.$", "", serija), M6 = round(slodze, 2), M6_se = round(slodze_se, 2), sigma2 = round(sigma2, 3), n_punkti)][, .(M6 = M6[1], M6_se = M6_se[1], sigma2 = sigma2[1], n_punkti = sum(n_punkti)), by = .(bloks, serija)],
            SLO[kopa == "B: LVA A" & modelis == "M7 DFM brīvas slodzes arī prasmei", .(bloks, serija = sub(" \\d+ g\\.$", "", serija), M7 = round(slodze, 2), M7_se = round(slodze_se, 2))][, .(M7 = M7[1], M7_se = M7_se[1]), by = .(bloks, serija)], by = c("bloks", "serija"))[order(bloks, serija)], row.names = FALSE, nrows = 100)
cat("\nσ² pa avotiem (B kopa, M2):\n"); print(unique(SLO[kopa == "B: LVA A" & modelis == "M2 DFM brīva Q", .(bloks, avots, sigma2 = round(sigma2, 4))]), row.names = FALSE)

# Lūzuma gadi: (1) viena pārrāvuma taisnes (svērtais MK) lūzuma gads; (2) maksimuma gads; (3) pirmais gads, kad stāvoklis ir zem maksimuma par vairāk nekā 1,96 SE
luzums <- function(d) {
  d <- d[dati & is.finite(theta) & is.finite(se)]; if (nrow(d) < 6) return(data.table(luzums = NA_integer_, sl_pirms = NA_real_, sl_pec = NA_real_, maks = NA_integer_, zem_maks = NA_integer_))
  d[, w := 1 / pmax(se, 1e-3)^2]   # SE = 0 (vai ~0) nedrīkst dot bezgalīgu svaru
  kand <- d$b[3:(nrow(d) - 2)]
  sse <- sapply(kand, function(c) { f <- lm(theta ~ b + pmax(b - c, 0), data = d, weights = w); sum(weighted.residuals(f)^2) })
  c0 <- kand[which.min(sse)]; f <- lm(theta ~ b + pmax(b - c0, 0), data = d, weights = w)
  im <- which.max(d$theta); zm <- d$b[d$b > d$b[im] & d$theta < d$theta[im] - 1.96 * d$se]
  data.table(luzums = c0, sl_pirms = coef(f)[2], sl_pec = coef(f)[2] + coef(f)[3], maks = d$b[im], zem_maks = if (length(zm)) zm[1] else NA_integer_)
}
LUZ <- ST[modelis %in% c("M2 DFM brīva Q", "M0 tikai prasme (67b)") & grupa %in% c("LVA A", "LVA", "EST", "LTU", "POL", "FIN"), luzums(.SD), by = .(kopa, modelis, grupa, bloks)]
cat("\nLūzuma gadi (viena pārrāvuma taisne; sl = slīpums z vai d gadā pirms/pēc; maks = augstākais stāvoklis; zem_maks = pirmais gads zem maksimuma par > 1,96 SE):\n")
print(LUZ[order(kopa, modelis, grupa, bloks), .(kopa, modelis, grupa, bloks, luzums, sl_pirms = round(sl_pirms, 3), sl_pec = round(sl_pec, 3), maks, zem_maks)], row.names = FALSE)

cat("\nLatviešu valodas plūsma (B kopa, M2): izlīdzinātie stāvokļi pa dzimšanas gadiem (95 % josla)\n")
print(dcast(ST[kopa == "B: LVA A" & modelis == "M2 DFM brīva Q" & grupa == "LVA A" & dati, .(bloks, b, v = sprintf("%.2f ± %.2f", theta, 1.96 * se))], b ~ bloks, value.var = "v"), row.names = FALSE)

# Stāvokļu izmaiņu krusteniskās korelācijas (M2, izlīdzinātie stāvokļi, visas vienības kopā): Δvide[b−l] ↔ Δprasme[b]
cat("\nKrusteniskā korelācija starp izlīdzināto stāvokļu gada izmaiņām (B kopa, M2, 8 vienības kopā; l > 0: vide/iesaiste apsteidz prasmi):\n")
XC <- ST[kopa == "B: LVA A" & modelis == "M2 DFM brīva Q" & dati][order(grupa, bloks, b)][, d := c(NA, diff(theta)), by = .(grupa, bloks)]
XW <- dcast(XC[!is.na(d)], grupa + b ~ bloks, value.var = "d")
xk <- rbindlist(lapply(-3:3, function(l) { z <- copy(XW)[, `:=`(vl = shift(vide, l), il = shift(iesaiste, l)), by = grupa]
  data.table(nobide = l, r_vide_prasme = cor(z$vl, z$prasme, use = "complete.obs"), r_iesaiste_prasme = cor(z$il, z$prasme, use = "complete.obs"), n = sum(complete.cases(z[, .(vl, prasme)]))) }))
print(xk[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], row.names = FALSE)

# ---------------------------------------------------------------------------
# 5. Ex ante pārbaude (B kopa): kā 67b, M0 pret DFM
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n5. Ex ante pārbaude (B kopa: 7 valstis + LVA A), mērķi 2006–2025\n", strrep("=", 100), "\n", sep = "")
faktiska <- function(T, vien) {
  Tp <- max(PISA_GADI[PISA_GADI < T])
  x <- merge(PP[serija == "PISA" & gads == T & grupa %in% vien, .(grupa, y2 = est)], PP[serija == "PISA" & gads == Tp & grupa %in% vien, .(grupa, y1 = est)], by = "grupa")
  x[, .(grupa, fakt = (y2 - mean(y2)) - (y1 - mean(y1)))]
}
EXM <- c("M0 tikai prasme (67b)", "M1 DFM diagonāla Q", "M2 DFM brīva Q", "M3 DFM vadība k=1")
EX <- list(); t0 <- Sys.time()
vien <- KOPAS[["B: LVA A"]]
UZE <- list(); for (T in MERKI) for (mn in EXM) UZE[[paste(T, mn)]] <- list(T = T, mn = mn)
FITE <- mclapply(UZE, function(u) { m <- MODELI[[u$mn]]
  novertet_k(paste("ex_ante", u$T, u$mn), PAN[kopa == "B: LVA A" & publ < sprintf("%d-03", u$T)], vien, m$q, m$k, m$br) }, mc.cores = MC, mc.preschedule = FALSE)
cat(sprintf("Ex ante novērtēti %d modeļi: %.1f min (no keša: %d)\n", length(FITE), as.numeric(difftime(Sys.time(), t0, units = "mins")), sum(sapply(FITE, function(h) isTRUE(h$no_kesa)))))
for (u in UZE) { T <- u$T; mn <- u$mn
  m <- MODELI[[mn]]; X <- PAN[kopa == "B: LVA A" & publ < sprintf("%d-03", T)]
  h <- FITE[[paste(T, mn)]]; if (inherits(h, "try-error") || is.null(h$th)) stop("Ex ante novērtējums neizdevās: ", T, " | ", mn, "\n", as.character(h))
  st <- izlidzinat(X, vien, h); Tp <- max(PISA_GADI[PISA_GADI < T]); iT <- bdz(T)
  pr <- rbindlist(lapply(vien, function(k) { rp <- X[grupa == k & serija == "PISA" & gads == Tp, y]
    s <- st[grupa == k & b == iT]; data.table(grupa = k, th = s[bloks == "prasme", theta], th_se = s[bloks == "prasme", se], r_iepr = if (length(rp)) rp[1] else NA_real_,
                                           vide = if (m$q == "P") NA_real_ else s[bloks == "vide", theta], vide_se = if (m$q == "P") NA_real_ else s[bloks == "vide", se],
                                           iesaiste = if (m$q == "P") NA_real_ else s[bloks == "iesaiste", theta], iesaiste_se = if (m$q == "P") NA_real_ else s[bloks == "iesaiste", se]) }))
  pr[, prog := (th - r_iepr) * SDS[["PISA"]]]
  x <- merge(pr, faktiska(T, vien), by = "grupa")[!is.na(prog)][, prog_rel := prog - mean(prog)]
  EX[[paste(T, mn)]] <- x[, `:=`(merkis = T, modelis = mn, ll = h$ll)]
  cat(sprintf("  %d %s: logLik %.1f, konverģence %d, novērojumi %d%s\n", T, mn, h$ll, h$conv, h$n_obs, if (is.null(h$min)) "" else sprintf(", %.1f min", h$min)))
}
cat(sprintf("Ex ante kopā: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
EX <- rbindlist(EX)
fwrite(EX, "output/pisa2025/dfm_ex_ante.csv")
cat("\nRelatīvās PISA pārmaiņas prognoze (punkti): MAE pa modeļiem, visas 8 vienības × 7 mērķi\n")
MAE <- EX[, .(pari = .N, mae = mean(abs(fakt - prog_rel)), mae_naivais = mean(abs(fakt)), kor = cor(fakt, prog_rel), virziens = mean(sign(fakt) == sign(prog_rel))), by = modelis]
print(MAE[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], row.names = FALSE)
cat("\nLatviešu valodas plūsma: faktiskā un prognozētā relatīvā pārmaiņa pa mērķiem (punkti)\n")
LX <- EX[grupa == "LVA A"]
print(dcast(LX, merkis + round(fakt, 1) ~ modelis, value.var = "prog_rel", fun.aggregate = function(v) round(v[1], 1)), row.names = FALSE)
print(LX[, .(pari = .N, virziens = sum(sign(fakt) == sign(prog_rel)), mae = round(mean(abs(fakt - prog_rel)), 1), naivais = round(mean(abs(fakt)), 1)), by = modelis], row.names = FALSE)
cat("\nEx ante stāvokļi paaudzei b_T (LVA A; z vai d; kas bija redzams pirms katra PISA testa):\n")
print(LX[modelis != "M0 tikai prasme (67b)", .(merkis, modelis, b = bdz(merkis), prasme = round(th, 2), prasme_se = round(th_se, 2), vide = round(vide, 2), vide_se = round(vide_se, 2), iesaiste = round(iesaiste, 2), iesaiste_se = round(iesaiste_se, 2))], row.names = FALSE)

# 67b un 67c salīdzinājums (tie paši mērķi, B kopa, vidējais)
S67 <- list()
f67b <- "output/pisa2025/paaudzu_modelis_parbaude.csv"; f67c <- "output/pisa2025/paaudzu_modelis_petijumi.csv"
if (file.exists(f67b)) { R67 <- fread(f67b)[radītājs == "vid" & kopa == "B: LVA A"]
  S67[["b"]] <- R67[, .(avots = "67b", pari = .N, mae = mean(abs(fakt - prog_rel)), lva_a_mae = mean(abs(fakt - prog_rel)[grupa == "LVA A"]), lva_a_2025 = prog_rel[grupa == "LVA A" & merkis == 2025][1]), by = modelis] }
if (file.exists(f67c)) { R67c <- fread(f67c)[radītājs == "vid" & kopa == "B: LVA A" & variants == "PISA + PIRLS"]
  S67[["c"]] <- R67c[, .(avots = "67c", modelis = "PISA + PIRLS", pari = .N, mae = mean(abs(fakt - prog_rel)), lva_a_mae = mean(abs(fakt - prog_rel)[grupa == "LVA A"]), lva_a_2025 = prog_rel[grupa == "LVA A" & merkis == 2025][1])] }
S67[["d"]] <- EX[, .(avots = "67d", pari = .N, mae = mean(abs(fakt - prog_rel)), lva_a_mae = mean(abs(fakt - prog_rel)[grupa == "LVA A"]), lva_a_2025 = prog_rel[grupa == "LVA A" & merkis == 2025][1]), by = modelis]
S67 <- lapply(S67, function(d) setcolorder(d, c("avots", "modelis", "pari", "mae", "lva_a_mae", "lva_a_2025")))
S67 <- rbindlist(S67, use.names = TRUE, fill = TRUE)
cat("\nSalīdzinājums ar 67b/67c (B kopa, vidējais, relatīvā pārmaiņa punktos; LVA A 2025 faktiskā: ", round(LX[merkis == 2025, fakt][1], 1), ")\n", sep = "")
print(S67[, .(avots, modelis, pari, mae = round(mae, 2), lva_a_mae = round(lva_a_mae, 2), lva_a_2025 = round(lva_a_2025, 1))], row.names = FALSE)
SALK <- merge(SAL, S67[avots == "67d", .(modelis, ex_ante_mae = mae, ex_ante_lva_a_mae = lva_a_mae, ex_ante_lva_a_2025 = lva_a_2025)], by = "modelis", all.x = TRUE)
fwrite(SALK[order(kopa, modelis)], "output/pisa2025/dfm_salidzinajums.csv")

# ---------------------------------------------------------------------------
# 6. Absolūtais variants: bez ciklu efekta (7 valstu vidējais, EST, LVA A)
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n6. Absolūtais variants (tikai vienības × sērijas efekts; 7 valstu vidējais, EST, LVA A)\n", strrep("=", 100), "\n", sep = "")
XA <- RAW[grupa %in% KOPAS[["B: LVA A"]]][, ks := paste(grupa, serija)]
XA[, r := est - weighted.mean(est, 1 / (se^2 + 1)), by = ks]
XA <- merge(XA, SDZ[, .(serija, sd_rel)], by = "serija")[, `:=`(y = r / sd_rel, se_z = se / sd_rel)]
XA <- XA[, .(grupa, bloks, avots, serija, gads, b, y, se = se_z, publ)]
hA <- novertet_k("absoluts M2", XA, KOPAS[["B: LVA A"]], "full", 0, FALSE)
cat("  ", apraksts(hA), "\n")
STA <- izlidzinat(XA, KOPAS[["B: LVA A"]], hA)
ST7 <- STA[grupa %in% SALID & dati, .(theta = mean(theta), se = sqrt(mean(se^2) / .N), dati = TRUE, grupa = "7 valstu vidējais"), by = .(bloks, b)][, .(grupa, bloks, b, theta, se, dati)]
STA <- rbind(STA, ST7)[, `:=`(kopa = "absolūti", modelis = "M2 DFM brīva Q")]
LUZA <- STA[grupa %in% c("LVA A", "EST", "7 valstu vidējais"), luzums(.SD), by = .(grupa, bloks)]
cat("\nLūzuma gadi absolūtajā variantā:\n")
print(LUZA[order(grupa, bloks), .(grupa, bloks, luzums, sl_pirms = round(sl_pirms, 3), sl_pec = round(sl_pec, 3), maks, zem_maks)], row.names = FALSE)
cat("\nAbsolūtie stāvokļi (z; 7 valstu vidējais, EST, LVA A) izvēlētās paaudzēs:\n")
print(dcast(STA[grupa %in% c("LVA A", "EST", "7 valstu vidējais") & dati & b %in% c(1987, 1990, 1993, 1996, 1999, 2002, 2006, 2009, 2012), .(grupa, bloks, b, v = round(theta, 2))], b ~ grupa + bloks, value.var = "v"), row.names = FALSE)

ST_ALL <- rbind(ST, STA, fill = TRUE); fwrite(ST_ALL, "output/pisa2025/dfm_stavokli.csv")
fwrite(rbind(LUZ[, .(kopa, modelis, grupa, bloks, luzums, sl_pirms, sl_pec, maks, zem_maks)], LUZA[, .(kopa = "absolūti", modelis = "M2 DFM brīva Q", grupa, bloks, luzums, sl_pirms, sl_pec, maks, zem_maks)]), "output/pisa2025/dfm_luzumi.csv")

# ---------------------------------------------------------------------------
# 7. Attēli (200 dpi; divas laika joslas: dzimšanas gads apakšā, PISA gads augšā)
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n7. Attēli\n", strrep("=", 100), "\n", sep = "")
TEMA <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text(face = "bold"),
                                              plot.caption = element_text(colour = "grey40", hjust = 0), legend.position = "bottom")
zim <- function(p, nos, w = 12, h = 7) { ggsave(file.path("output/pisa2025/plots", paste0(nos, ".png")), p, width = w, height = h, dpi = 200, bg = "white"); cat("  ", nos, "\n") }
ass_x <- function() scale_x_continuous("Dzimšanas gads (paaudze)", breaks = seq(1984, 2013, 3), limits = c(1984, 2013.5),
                                      sec.axis = sec_axis(~ . + 16, name = "PISA mērījuma gads (dzimšanas gads + 16)", breaks = PISA_GADI))
BL_LAB <- c(prasme = "Prasme (d, skolēnu SD)", vide = "Lasīšanas vide (z)", iesaiste = "Iesaiste un labbūtība (z)")
KR <- c("LVA A" = "#b3261e", "LVA" = "#e08a00", "EST" = "#2a78d6", "LTU" = "#7a57c7", "POL" = "#0f8a6e", "FIN" = "#5c5c5c", "CZE" = "#9aa3ad", "HUN" = "#c9a227", "DEU" = "#7fa7d8", "7 valstu vidējais" = "#2a78d6")
ZINL <- data.table(x = c(2007.9 - 16, 2017.9 - 16, 2023.4 - 16, 2025.3 - 16), etikete = c("PIRLS 2006\nziņojums", "\"ievērojami\nuzlabojušies\"", "PIRLS 2021\n\"drauds\"", "PISA 2025\ntests"))

# 7a. Trīs stāvokļi: LVA A (B kopa) un LVA (A kopa) ar EST fonā, M2
d1 <- rbind(ST[kopa == "B: LVA A" & modelis == "M2 DFM brīva Q" & grupa %in% c("LVA A", "EST") & dati],
            ST[kopa == "A: valstis" & modelis == "M2 DFM brīva Q" & grupa == "LVA" & dati])[, bloks := factor(bloks, names(BL_LAB), BL_LAB)]
p1 <- ggplot(d1, aes(b, theta, colour = grupa, fill = grupa)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = theta - 1.96 * se, ymax = theta + 1.96 * se), alpha = 0.12, colour = NA) + geom_line(linewidth = 0.9) +
  facet_wrap(~ bloks, ncol = 1, scales = "free_y") + ass_x() + scale_colour_manual(values = KR, name = NULL, labels = c(`LVA A` = "latviešu valodas plūsma", LVA = "Latvija kopā", EST = "Igaunija")) +
  scale_fill_manual(values = KR, name = NULL, labels = c(`LVA A` = "latviešu valodas plūsma", LVA = "Latvija kopā", EST = "Igaunija")) +
  labs(y = "Latentais stāvoklis relatīvi pret salīdzinājuma valstīm", title = "Dinamiskais faktoru modelis: prasme, lasīšanas vide un iesaiste pa dzimšanas gadiem",
       subtitle = "KFAS, trīs gadījuma klejošanas stāvokļi ar brīvu izmaiņu kovariāciju (M2); 95 % josla; līkne tikai gados ar vienības mērījumiem; relatīvi = pēc cikla un vienības × sērijas fiksētajiem efektiem",
       caption = "Avoti: PISA 2000–2025 (rezultāti un anketas), PIRLS 2001–2021, TIMSS 4. kl. 2003–2023, ICCS 2009–2022, ICILS 2023, HBSC Data Browser 2014/2018/2022 (visa valsts, 11/13/15 g.). Skripts 67d, docs/73.") + TEMA
zim(p1, "67d_dfm_stavokli", 12, 10)

# 7b. LVA A: stāvoklis un mērījumu punkti pa blokiem (kas velk stāvokli)
d2p <- PAN[kopa == "B: LVA A" & grupa == "LVA A"][, bloks := factor(bloks, names(BL_LAB), BL_LAB)]
d2s <- ST[kopa == "B: LVA A" & modelis == "M2 DFM brīva Q" & grupa == "LVA A" & dati][, bloks := factor(bloks, names(BL_LAB), BL_LAB)]
p2 <- ggplot() + geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(data = d2s, aes(b, ymin = theta - 1.96 * se, ymax = theta + 1.96 * se), fill = "#b3261e", alpha = 0.15) + geom_line(data = d2s, aes(b, theta), colour = "#b3261e", linewidth = 1) +
  geom_linerange(data = d2p, aes(b, ymin = y - 1.96 * se, ymax = y + 1.96 * se, colour = avots), alpha = 0.5) + geom_point(data = d2p, aes(b, y, colour = avots, shape = avots), size = 2.2, alpha = 0.85) +
  geom_vline(data = ZINL, aes(xintercept = x), colour = "grey50", linetype = "dotted") + geom_text(data = ZINL, aes(x, y = Inf, label = etikete), vjust = 1.1, hjust = 0, size = 2.5, colour = "grey30", nudge_x = 0.2) +
  facet_wrap(~ bloks, ncol = 1, scales = "free_y") + ass_x() + scale_colour_brewer(palette = "Dark2", name = NULL) + scale_shape_manual(values = c(16, 17, 15, 18, 8, 4, 3), name = NULL) +
  labs(y = "Relatīvā vērtība (z; prasmei d) un latentais stāvoklis", title = "Latviešu valodas plūsma: mērījumi un latentais stāvoklis katrā blokā",
       subtitle = "Punkti = standartizēti rādītāji ar 95 % svītrām (pēc fiksētajiem efektiem; lielāks = labāk); līkne = M2 izlīdzinātais stāvoklis; punktētās līnijas = ziņojumu datumi (kalendāra gads − 16)",
       caption = "HBSC rādītāji ir visa Latvija (bez valodas plūsmām); dzimšanas gads = aptaujas gads − vecums. IEA vide: grāmatas mājās > 100, PIRLS lasa priekam (4. kl. ~10,5 g., 8. kl. ~14,3 g.).") + TEMA
zim(p2, "67d_dfm_merijumi_lva_a", 13, 11)

# 7c. Ex ante: prognozes pret faktisko (LVA A) pa modeļiem; ex ante vides un iesaistes stāvokļi paaudzei b_T
d3 <- LX[, .(merkis, modelis, prog_rel, fakt)][, modelis := factor(modelis, EXM)]
p3a <- ggplot(d3, aes(merkis, prog_rel, colour = modelis)) + geom_hline(yintercept = 0, colour = "grey60") + geom_line(aes(y = fakt), colour = "black", linewidth = 1) + geom_point(aes(y = fakt), colour = "black", size = 2.5) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) + scale_x_continuous("PISA mērķa gads", breaks = MERKI) + scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(y = "Relatīvā PISA pārmaiņa pret iepriekšējo ciklu (punkti)", title = "Ex ante: latviešu valodas plūsmas relatīvā pārmaiņa, faktiskā (melna) un prognozētā pa modeļiem",
       subtitle = "Katram mērķim modelis novērtēts tikai ar pirms testa publicētiem datiem (B kopa: 7 valstis + LVA A)") + TEMA + guides(colour = guide_legend(nrow = 2))
d3b <- melt(LX[modelis == "M2 DFM brīva Q", .(merkis, b = bdz(merkis), prasme = th, vide, iesaiste, prasme_se = th_se, vide_se, iesaiste_se)],
            id.vars = c("merkis", "b"), measure.vars = list(theta = c("prasme", "vide", "iesaiste"), se = c("prasme_se", "vide_se", "iesaiste_se")))[, bloks := factor(c("prasme", "vide", "iesaiste")[variable], names(BL_LAB), BL_LAB)]
p3b <- ggplot(d3b, aes(merkis, theta, colour = bloks)) + geom_hline(yintercept = 0, colour = "grey60") + geom_pointrange(aes(ymin = theta - 1.96 * se, ymax = theta + 1.96 * se), position = position_dodge(width = 1)) + geom_line(position = position_dodge(width = 1)) +
  scale_x_continuous("PISA mērķa gads (paaudze = gads − 16)", breaks = MERKI) + scale_colour_manual(values = c("#b3261e", "#2a78d6", "#0f8a6e"), name = NULL) +
  labs(y = "Ex ante stāvoklis mērķa paaudzei", title = "Kas bija redzams pirms katra testa: mērķa paaudzes trīs stāvokļi (M2, LVA A)", subtitle = "Stāvoklis dzimšanas gadam T − 16 pēc datiem, kas publicēti pirms PISA T testa (95 % josla)") + TEMA
zim(p3a / p3b, "67d_dfm_ex_ante", 12, 11)

# 7d. Absolūtais variants: 7 valstu vidējais, EST, LVA A
d4 <- STA[grupa %in% c("LVA A", "EST", "7 valstu vidējais") & dati][, bloks := factor(bloks, names(BL_LAB), BL_LAB)]
p4 <- ggplot(d4, aes(b, theta, colour = grupa, fill = grupa)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = theta - 1.96 * se, ymax = theta + 1.96 * se), alpha = 0.12, colour = NA) + geom_line(linewidth = 0.9) + facet_wrap(~ bloks, ncol = 1, scales = "free_y") + ass_x() +
  scale_colour_manual(values = KR, name = NULL, labels = c(`LVA A` = "latviešu valodas plūsma", EST = "Igaunija", `7 valstu vidējais` = "7 salīdzinājuma valstu vidējais")) +
  scale_fill_manual(values = KR, name = NULL, labels = c(`LVA A` = "latviešu valodas plūsma", EST = "Igaunija", `7 valstu vidējais` = "7 salīdzinājuma valstu vidējais")) +
  labs(y = "Latentais stāvoklis (absolūti, novirze no vienības vidējā laikā)", title = "Absolūtais variants: kopīgās pārmaiņas visās valstīs un Latvijas atšķirība",
       subtitle = "Bez cikla efekta: katras sērijas vērtība mīnus vienības vidējais laikā, standartizēta ar salīdzinājuma valstu izkliedi; M2 hiperparametri novērtēti no jauna",
       caption = "7 valstu vidējais = 7 izlīdzināto stāvokļu vidējais (SE bez kovariācijas). Absolūtajā variantā ciklu atšķirības ietver arī anketu formulējumu maiņu, tāpēc līmeņi ir aptuveni.") + TEMA
zim(p4, "67d_dfm_absoluts", 12, 10)

# 7e. Vadības koeficienti pa nobīdēm
d5 <- SAL[kopa == "B: LVA A" & k > 0, .(k, phi_vide, phi_vide_se, phi_iesaiste, phi_iesaiste_se)]
d5 <- melt(d5, id.vars = "k", measure.vars = list(phi = c("phi_vide", "phi_iesaiste"), se = c("phi_vide_se", "phi_iesaiste_se")))[, stavoklis := c("vide → prasme", "iesaiste → prasme")[variable]]
p5 <- ggplot(d5, aes(factor(k), phi, colour = stavoklis)) + geom_hline(yintercept = 0, colour = "grey60") + geom_pointrange(aes(ymin = phi - 1.96 * se, ymax = phi + 1.96 * se), position = position_dodge(width = 0.4), size = 0.7) +
  scale_colour_manual(values = c("#2a78d6", "#0f8a6e"), name = NULL) + labs(x = "Nobīde k (gadi starp paaudzēm)", y = "φ: prasmes izmaiņa (d) uz 1 z stāvokļa pirms k gadiem",
       title = "Vadība: vai vides vai iesaistes stāvoklis paredz prasmes stāvokļa izmaiņu", subtitle = "prasme[b] = prasme[b−1] + φ_V·vide[b−k] + φ_I·iesaiste[b−k] + η; ML novērtējums ar 95 % intervālu (B kopa, 8 vienības)") + TEMA
zim(p5, "67d_dfm_vadiba", 9, 6)

cat(sprintf("\nKopējais laiks: %.1f min\n", as.numeric(difftime(Sys.time(), t00, units = "mins"))))
cat("Pabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
