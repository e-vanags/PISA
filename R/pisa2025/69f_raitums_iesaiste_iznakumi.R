# ============================================================================
# 69f. Slānis 9: raitums, iesaiste un uzdevumu veidi kā simptomu modeļa iznākumi
# ============================================================================
#
# Turpina 69a (docs/64, 4. sadaļa) ar tiem pašiem rīkiem (64_simptomi_riki.R,
# C variants: kopīgās pazīmes, −999 = neatbildēts, slīdošs sākumpunkts), bet
# iznākums nāk no kognitīvā testa uzvedības un uzdevumu veidiem, ne no PV:
#
#   RAIT        raituma (CR590 teikumu) pareizo daļa, %, skolēniem ar ≥ 10 vērtētiem
#               raituma teikumiem (neatbildēts = nepareizs, kā PISA vērtējumā);
#   RAIT_ATL    tas pats mīnus prognoze no PVREAD (svērta kubiskā regresija katrā
#               ciklā, 8 valstis kopā) — raitums relatīvi pret kopējo lasīšanu;
#   NESAS       nesasniegto lasīšanas uzdevumu daļa, % no uzdotajiem;
#   ATRAS       ātro atbilžu daļa, % no vērtētajām ar laiku; slieksnis katram
#               uzdevumam = 10 % no 2022. gada 8 valstu vidējā atbildes laika, ne vairāk
#               par 10 s (docs/54 noteikums, bet 8 valstu, ne Latvijas vidējais);
#   PIEP        pašnovērtētā piepūle testā (EFFORT1, 1–10);
#   PIEP_D      piepūle, ja rezultāts skaitītos, mīnus faktiskā (EFFORT2 − EFFORT1);
#               abiem piepūles iznākumiem piepūles pazīmes (E_PIEPULE_*, AUTO_DEFFORT un
#               neapstrādātā ST331 baterija) no modeļa izslēgtas;
#   ATRAST_ATL, SAPRAST_ATL, IZVERT_ATL
#               pareizo daļa attiecīgā kognitīvā procesa uzdevumos (klasifikācija:
#               PISA 2022 TR A pielikums; 2015. gada uzdevumiem 2015 TR aspekti) mīnus
#               prognoze no PVREAD (kā RAIT_ATL), skolēniem ar ≥ 4 šā veida uzdevumiem.
#
# Pieejamība (1. solis): raitums 2018–2025; uzdevumu laiki tikai 2022 (COG) un 2025
# (COG_PROCESS); 2018 TIM failā ir tikai anketas bloku laiki, 2015 COG failā tikai
# "pēdējā apmeklējuma" laiks — tāpēc ATRAS mērķis ir tikai 2025; piepūle 2018–2025;
# nesasniegtie un uzdevumu veidi 2015–2025. Visi iznākumi ir visām 8 valstīm, tāpēc
# valsts iekšējais variants (64k) nav vajadzīgs.
#
# Mērķi: 2025 visiem; 2022, ja iznākums ir 2018. gadā; 2018, ja ir 2015. gadā.
# Modelis: XGBoost kā 69a (eta 0,1, ≤ 1500 koki, 4 pavedieni). Novērtējums grupu
# līmenī (7 valstis, LVA A): faktiskais pret paredzēto, pārmaiņa, relatīvā pārmaiņa;
# SHAP pa pazīmēm un jomām; salīdzinājums ar lasīšanas vidējo no 69a.
#
# Izvade: output/pisa2025/citi_iznakumi_2_parbaude.csv, citi_iznakumi_2_shap.csv,
#         citi_iznakumi_2_pieejamiba.csv,
#         data/pisa2025/processed/iznakumi_2_uzdevumi.rds (kognitīvo failu izvilkums),
#         data/pisa2025/processed/simptomi_iznakumi_2.rds (skolēnu līmeņa iznākumi),
#         output/pisa2025/logs/69f_raitums_iesaiste_iznakumi_output.txt
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
suppressPackageStartupMessages(library(haven))
sakt_zurnalu("69f_raitums_iesaiste_iznakumi")
PAR_C$nthread <- 4L                       # paralēli rēķinās citi darbi
TESTS <- nzchar(Sys.getenv("PISA_69F_TESTS"))   # ātra pārbaude ar dažiem kokiem
V8 <- c("LVA", SALID)
SAV25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
F_COG <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_COG.sav",
           "2018" = "data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav",
           "2022" = "data/pisa2022/raw/CY08MSP_STU_COG.SAV",
           "2025" = file.path(SAV25, "CY09_MS_COG_20260806.sav"))
F_TT <- c("2022" = "data/pisa2022/raw/CY08MSP_STU_COG.SAV",
          "2025" = file.path(SAV25, "CY09_MS_COG_PROCESS_20260806.sav"))

# ---------------------------------------------------------------------------
# 1. Kognitīvo failu izvilkums 8 valstīm: vērtējumi (K) un laiki (T)
# ---------------------------------------------------------------------------
# Kodējums kā 62a: 0 bez kredīta, 1 daļējs, 2 pilns (vienpunkta uzdevumos pilns = 1),
# −1 neatbildēts, −2 nesasniegts, −3 nederīgs, NA nav uzdots. Laiks ms.
cat(strrep("=", 100), "\n1. Kognitīvo failu izvilkums (8 valstis, 2015–2025)\n", strrep("=", 100), "\n\n", sep = "")
vertetie <- function(n) {
  k1 <- sub("^CR(.*)S$", "\\1", grep("^CR[0-9]{3}Q[0-9]{2}[A-Z]?S$", n, value = TRUE))
  k2 <- sub("^DR(.*)C$", "\\1", grep("^DR[0-9]{3}Q[0-9]{2}C$", n, value = TRUE))
  c(stats::setNames(paste0("CR", k1, "S"), k1), stats::setNames(paste0("DR", setdiff(k2, k1), "C"), setdiff(k2, k1)))
}
kods <- function(x) {
  lab <- as.character(as_factor(x)); o <- rep(NA_integer_, length(lab))
  o[grepl("No credit", lab, ignore.case = TRUE)] <- 0L
  o[grepl("Partial credit", lab, ignore.case = TRUE)] <- 1L
  o[grepl("Full credit", lab, ignore.case = TRUE)] <- 2L
  o[grepl("No Response", lab, ignore.case = TRUE)] <- -1L
  o[grepl("Not Reached", lab, ignore.case = TRUE)] <- -2L
  o[grepl("Invalid|Technical", lab, ignore.case = TRUE)] <- -3L
  o
}
laiks_ms <- function(x) { v <- suppressWarnings(as.numeric(x)); v[!is.finite(v) | v <= 0 | v >= 1e7] <- NA; v }
lasit_blokus <- function(f, kols) {
  cnt <- as.character(read_sav(f, col_select = "CNT")$CNT)
  do.call(rbind, lapply(V8, function(c0) {
    i <- which(cnt == c0); if (!length(i)) return(NULL)
    if (any(diff(i) != 1)) stop(basename(f), " ", c0, ": rindas nav vienā blokā")
    read_sav(f, user_na = TRUE, col_select = tidyselect::all_of(kols), skip = i[1] - 1, n_max = length(i))
  }))
}
KESS <- "data/pisa2025/processed/iznakumi_2_uzdevumi.rds"
if (file.exists(KESS)) { UZ <- readRDS(KESS); cat("Nolasīts kešs:", KESS, "\n") } else {
  UZ <- list()
  for (g in names(F_COG)) {
    t0 <- Sys.time()
    n <- names(read_sav(F_COG[[g]], n_max = 0)); v <- vertetie(n)
    d <- lasit_blokus(F_COG[[g]], c("CNT", "CNTSTUID", unname(v)))
    K <- vapply(d[unname(v)], kods, integer(nrow(d))); colnames(K) <- paste0("CR", names(v))
    poly <- apply(K, 2, function(x) any(x == 1L, na.rm = TRUE))
    for (j in which(!poly)) K[K[, j] %in% 2L, j] <- 1L
    mx <- ifelse(poly, 2L, 1L)
    sk <- data.table(CNT = as.character(d$CNT), CNTSTUID = as.numeric(d$CNTSTUID))
    Tm <- NULL
    if (g %in% names(F_TT)) {
      nt <- names(read_sav(F_TT[[g]], n_max = 0)); ttk <- intersect(paste0(colnames(K), "TT"), nt)
      dt <- lasit_blokus(F_TT[[g]], c("CNTSTUID", ttk))
      Tm <- vapply(dt[ttk], laiks_ms, numeric(nrow(dt))); colnames(Tm) <- sub("TT$", "", ttk)
      Tm <- Tm[match(sk$CNTSTUID, as.numeric(dt$CNTSTUID)), , drop = FALSE]
      rm(dt)
    }
    UZ[[g]] <- list(sk = sk, K = K, T = Tm, mx = mx)
    cat(sprintf("%s: %d skolēni (%s) | uzdevumi %d, ar laiku %d, raituma %d | %.1f min\n", g, nrow(K),
                paste(names(table(sk$CNT)), table(sk$CNT), collapse = ", "), ncol(K), if (is.null(Tm)) 0 else ncol(Tm),
                sum(grepl("^CR590", colnames(K))), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    rm(d, K, Tm); gc(verbose = FALSE)
  }
  saveRDS(UZ, KESS); cat("Saglabāts kešs:", KESS, "\n")
}

# ---------------------------------------------------------------------------
# 2. Iznākumi skolēna līmenī
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n2. Iznākumi skolēna līmenī\n", strrep("=", 100), "\n\n", sep = "")
KL22 <- fread("output/pisa2025/uzdevumu_klasifikacija_2022.csv", colClasses = "character")
KL15 <- fread("output/pisa2025/uzdevumu_klasifikacija_2015.csv", colClasses = "character")
veids <- function(uzd) {
  v <- c("Locate information" = "atrast", "Understand" = "saprast", "Evaluate and reflect" = "izvertet")[KL22$process_skala[match(uzd, KL22$uzd)]]
  v15 <- c("Access and retrieve" = "atrast", "Integrate and interpret" = "saprast", "Reflect and evaluate" = "izvertet")[KL15$aspekts[match(uzd, KL15$uzd)]]
  v[is.na(v)] <- v15[is.na(v)]
  v[grepl("^CR590", uzd)] <- "raitums"
  unname(v)
}
# Ātro atbilžu slieksnis: 2022. gada 8 valstu vidējais laiks katram uzdevumam vērtētajās
# atbildēs; uzdevumiem, kuru 2022. gadā nav, 2025. gada vidējais.
vid_laiks <- function(g) { K <- UZ[[g]]$K; Tm <- UZ[[g]]$T; Vv <- !is.na(K) & K >= -1
  x <- Tm; x[!Vv[, colnames(Tm)]] <- NA; colMeans(x, na.rm = TRUE) }
vl22 <- vid_laiks("2022"); vl25 <- vid_laiks("2025")
vl <- c(vl22, vl25[setdiff(names(vl25), names(vl22))])
SL <- pmin(0.10 * vl, 10000); SL[!is.finite(SL)] <- NA    # uzdevumi bez derīga laika neieiet ātro daļā
cat(sprintf("Ātro atbilžu slieksnis: %d uzdevumi (no 2022: %d; bez laika %d); mediāna %.1f s, min %.1f, maks %.1f\n",
            sum(!is.na(SL)), length(vl22), sum(is.na(SL)), median(SL, na.rm = TRUE) / 1000, min(SL, na.rm = TRUE) / 1000, max(SL, na.rm = TRUE) / 1000))

IZ2 <- list(); PIEEJ2 <- list()
for (g in names(UZ)) {
  K <- UZ[[g]]$K; mx <- UZ[[g]]$mx; u <- colnames(K); Tm <- UZ[[g]]$T
  Vv <- !is.na(K) & K >= -1                      # vērtētās atbildes (neatbildēts = 0 punkti)
  ADM <- !is.na(K) & K != -3                      # uzdotās
  NS <- !is.na(K) & K == -2
  P <- ifelse(Vv, pmax(K, 0) / rep(mx, each = nrow(K)), NA)
  tp <- veids(u)
  dala <- function(kopa, minn) { nv <- rowSums(Vv[, kopa, drop = FALSE]); s <- 100 * rowSums(P[, kopa, drop = FALSE], na.rm = TRUE) / nv
    s[nv < minn] <- NA; s }
  o <- data.table(skolens = paste(g, UZ[[g]]$sk$CNTSTUID), cnt = UZ[[g]]$sk$CNT,
                  RAIT = dala(which(tp %in% "raitums"), 10),
                  NESAS = { na <- rowSums(ADM); x <- 100 * rowSums(NS) / na; x[na < 1] <- NA; x },
                  ATRAST_D = dala(which(tp %in% "atrast"), 4), SAPRAST_D = dala(which(tp %in% "saprast"), 4),
                  IZVERT_D = dala(which(tp %in% "izvertet"), 4))
  if (!is.null(Tm)) {
    sl <- SL[colnames(Tm)]; A0 <- sweep(Tm, 2, sl, "<"); A0[is.na(A0)] <- FALSE
    Vt <- Vv[, colnames(Tm)] & !is.na(Tm) & rep(!is.na(sl), each = nrow(Tm)); nt <- rowSums(Vt)
    o[, ATRAS := { x <- 100 * rowSums(A0 & Vt) / nt; x[nt < 10] <- NA; x }]
  } else o[, ATRAS := NA_real_]
  IZ2[[g]] <- o
  cat(sprintf("%s: uzdevumi pa veidiem: %s\n", g, paste(names(table(tp, useNA = "ifany")), table(tp, useNA = "ifany"), collapse = ", ")))
}
IZ2 <- rbindlist(IZ2)

D <- sagatavot_datus(0.9); A <- D$A; PIEEJ <- pieejamiba(D$ND_V); rm(D); gc(verbose = FALSE)
A <- merge(A, IZ2[, !"cnt"], by = "skolens", all.x = TRUE)
na999 <- function(x) { x <- as.numeric(x); x[x == -999] <- NA; x }
A[, PIEP := na999(E_PIEPULE_TESTAM)][, PIEP_D := na999(E_PIEPULE_JA_SKAITITOS) - PIEP]
# Piepūles iznākumiem izslēdz gan saskaņotos konstruktus, gan to pašu jautājumu bateriju
# neapstrādātā formā (ST331Q01–Q03 2022/2025: piepūle testā, ja skaitītos, anketā) — citādi
# modelis "paredz" piepūli no pašas piepūles (pirmajā palaidienā R² 0,34 tieši tāpēc).
EFF <- c("E_PIEPULE_TESTAM", "E_PIEPULE_JA_SKAITITOS", "AUTO_DEFFORT", grep("ST331", names(A), value = TRUE))

# Atlikumi pret PVREAD: svērta kubiskā regresija katrā ciklā, 8 valstis kopā (wn: katra
# valsts × cikls = 1000). Grupas atlikums tātad ir relatīvs pret 8 valstu kopu tajā ciklā.
atlikums <- function(y, pv, w, g) {
  r <- rep(NA_real_, length(y))
  for (gg in unique(g[!is.na(y)])) {
    i <- which(g == gg & !is.na(y) & !is.na(pv))
    m <- lm(y[i] ~ poly(pv[i], 3), weights = w[i]); r[i] <- residuals(m)
  }
  r
}
A[, RAIT_ATL := atlikums(RAIT, PVREAD, wn, gads)]
A[, ATRAST_ATL := atlikums(ATRAST_D, PVREAD, wn, gads)]
A[, SAPRAST_ATL := atlikums(SAPRAST_D, PVREAD, wn, gads)]
A[, IZVERT_ATL := atlikums(IZVERT_D, PVREAD, wn, gads)]

IZK <- c("RAIT", "RAIT_ATL", "NESAS", "ATRAS", "PIEP", "PIEP_D", "ATRAST_ATL", "SAPRAST_ATL", "IZVERT_ATL")
cat("\nPieejamība: skolēnu daļa ar iznākumu, % (gads × valsts)\n")
PV2 <- A[gads %in% names(UZ), c(list(n = .N), lapply(.SD, function(x) round(100 * mean(!is.na(x)), 1))), by = .(gads, cnt), .SDcols = IZK][order(gads, cnt)]
print(PV2, row.names = FALSE)
fwrite(PV2, "output/pisa2025/citi_iznakumi_2_pieejamiba.csv")
cat("\nIznākumu svērtie vidējie pa grupām un gadiem (līmeņi):\n")
LIM <- A[gads %in% names(UZ), lapply(.SD, function(x) round(wm(x[!is.na(x)], wn[!is.na(x)]), 2)), by = .(gads, grupa), .SDcols = IZK][order(grupa, gads)]
print(LIM, row.names = FALSE)
cat("\nKorelācija ar PVREAD skolēnu līmenī (2025, visas 8 valstis):\n")
print(A[gads == "2025", lapply(.SD, function(x) round(kor(x, PVREAD), 2)), .SDcols = IZK], row.names = FALSE)
saveRDS(A[gads %in% names(UZ), c("skolens", "gads", "cnt", "grupa", "wn", "PVREAD", IZK), with = FALSE],
        "data/pisa2025/processed/simptomi_iznakumi_2.rds")

# ---------------------------------------------------------------------------
# 3. Simptomu modelis katram iznākumam (kā 69a)
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n3. Simptomu modelis pa iznākumiem un mērķiem\n", strrep("=", 100), "\n\n", sep = "")
par01 <- PAR_C; par01$eta <- 0.1
NMAX <- if (TESTS) 20 else 1500
IZNAK <- list(
  list(nos = "RAIT",        merki = c("2022", "2025")),
  list(nos = "RAIT_ATL",    merki = c("2022", "2025")),
  list(nos = "NESAS",       merki = c("2018", "2022", "2025")),
  list(nos = "ATRAS",       merki = "2025"),
  list(nos = "PIEP",        merki = c("2022", "2025"), izslegt = EFF),
  list(nos = "PIEP_D",      merki = c("2022", "2025"), izslegt = EFF),
  list(nos = "ATRAST_ATL",  merki = c("2018", "2022", "2025")),
  list(nos = "SAPRAST_ATL", merki = c("2018", "2022", "2025")),
  list(nos = "IZVERT_ATL",  merki = c("2018", "2022", "2025")))
if (TESTS) IZNAK <- lapply(IZNAK, function(z) { z$merki <- "2025"; z })

REZ <- list(); SHL <- list()
for (iz in IZNAK) for (Tg in iz$merki) {
  t0 <- Sys.time(); iepr <- iepr_gads(Tg)
  pz <- setdiff(kopigas(PIEEJ, Tg, iepr), if (is.null(iz$izslegt)) character() else iz$izslegt)
  B <- A[gads <= Tg & !is.na(get(iz$nos)), unique(c("gads", "grupa", "wn", "PVREAD", iz$nos, pz)), with = FALSE]
  B[, y := get(iz$nos)]
  if (TESTS) B <- B[sample(.N, min(.N, 15000))]
  m <- apmacit(B[gads < Tg], pz, par01, merkis = "y", nmax = NMAX)
  te <- B[gads == Tg]; pr <- B[gads == iepr]; Xte <- mat(te, pz); Xpr <- mat(pr, pz)
  te[, pred := predict(m, xgb.DMatrix(Xte, missing = NA))]; pr[, pred := predict(m, xgb.DMatrix(Xpr, missing = NA))]
  r2 <- 1 - sum(te$wn * (te$y - te$pred)^2) / sum(te$wn * (te$y - wm(te$y, te$wn))^2)
  kop <- merge(te[, .(fakt = wm(y, wn), pred = wm(pred, wn), n = .N), by = grupa],
               pr[, .(fakt_iepr = wm(y, wn), pred_iepr = wm(pred, wn), n_iepr = .N), by = grupa], by = "grupa", all.x = TRUE)
  kop[, `:=`(iznakums = iz$nos, merkis = Tg, iepr = iepr, pazimes = length(pz), koki = xgb.get.num.boosted.rounds(m),
             apmac_n = sum(B$gads < Tg), r2_skoleni = r2, parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr)]
  REZ[[paste(Tg, iz$nos)]] <- kop
  g2 <- grupu_shap(m, Xte, te); g1 <- grupu_shap(m, Xpr, pr); gg <- intersect(rownames(g1), rownames(g2))
  Dd <- g2[gg, , drop = FALSE] - g1[gg, , drop = FALSE]
  SHL[[paste(Tg, iz$nos)]] <- data.table(iznakums = iz$nos, merkis = Tg, grupa = rep(gg, ncol(Dd)), pazime = rep(colnames(Dd), each = length(gg)), starpiba = as.vector(Dd))
  v <- kop[grupa %in% SALID]
  cat(sprintf("%s %-11s (%d pazīmes, %d koki, apmācībā %d, %.1f min): skolēnu R² %.3f; 7 valstu |kļūda| %.2f, naivais %.2f; pārmaiņas kor %.2f; LVA A fakt %+.2f pred %+.2f; rel fakt %+.2f pred %+.2f\n",
              Tg, iz$nos, length(pz), xgb.get.num.boosted.rounds(m), sum(B$gads < Tg), as.numeric(difftime(Sys.time(), t0, units = "mins")), r2,
              mean(abs(v$fakt - v$pred)), mean(abs(v$parm_fakt), na.rm = TRUE), kor(v$parm_fakt, v$parm_pred),
              kop[grupa == "LVA A", parm_fakt], kop[grupa == "LVA A", parm_pred],
              kop[grupa == "LVA A", parm_fakt] - mean(v$parm_fakt, na.rm = TRUE), kop[grupa == "LVA A", parm_pred] - mean(v$parm_pred, na.rm = TRUE)))
  print(kop[order(grupa), .(grupa, n, fakt = round(fakt, 2), pred = round(pred, 2), fakt_iepr = round(fakt_iepr, 2), pred_iepr = round(pred_iepr, 2),
                            parm_fakt = round(parm_fakt, 2), parm_pred = round(parm_pred, 2))], row.names = FALSE)
  rm(m, te, pr, Xte, Xpr, B); gc(verbose = FALSE)
}
R <- rbindlist(REZ); SH <- rbindlist(SHL)
fwrite(R, "output/pisa2025/citi_iznakumi_2_parbaude.csv"); fwrite(SH, "output/pisa2025/citi_iznakumi_2_shap.csv")

# ---------------------------------------------------------------------------
# 4. Kopsavilkumi
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n4.1. Kopsavilkums pa iznākumiem: 7 valstis (grupu līmenis); READ = lasīšanas vidējais no 69a tajos pašos mērķos\n", strrep("=", 100), "\n", sep = "")
R0 <- fread("output/pisa2025/citi_iznakumi_parbaude.csv")[iznakums == "READ"]
kops <- function(V) V[, .(pari = .N, r2 = if ("r2_skoleni" %in% names(V)) round(mean(r2_skoleni), 2) else NA_real_,
                          limena_mae = round(mean(abs(fakt - pred)), 2), parm_mae = round(mean(abs(parm_fakt - parm_pred)), 2),
                          naivais = round(mean(abs(parm_fakt)), 2), parm_kor = round(kor(parm_fakt, parm_pred), 2),
                          rel_kor = round(kor(rel_f, rel_p), 2), rel_mae = round(mean(abs(rel_f - rel_p)), 2), rel_naiv = round(mean(abs(rel_f)), 2))]
rel <- function(V) { V <- V[grupa %in% SALID & !is.na(parm_fakt)]
  V[, `:=`(rel_f = parm_fakt - mean(parm_fakt), rel_p = parm_pred - mean(parm_pred)), by = .(iznakums, merkis)]; V }
V <- rel(R); V0 <- rel(R0)
K1 <- rbindlist(lapply(IZNAK, function(iz) kops(V[iznakums == iz$nos])[, iznakums := iz$nos]))
KR <- rbindlist(lapply(unique(lapply(IZNAK, `[[`, "merki")), function(mk) kops(V0[merkis %in% mk])[, iznakums := paste0("READ (", paste(mk, collapse = ","), ")")]))
K1 <- rbind(K1, KR)
print(K1[, c("iznakums", setdiff(names(K1), "iznakums")), with = FALSE], row.names = FALSE)
cat("\nPa mērķiem: pārmaiņas korelācija 7 valstīs (iznākums × mērķis); READ no 69a\n")
KM <- rbind(V[, .(kor = round(kor(parm_fakt, parm_pred), 2)), by = .(iznakums, merkis)],
            V0[, .(kor = round(kor(parm_fakt, parm_pred), 2)), by = .(iznakums, merkis)])
print(dcast(KM, iznakums ~ merkis, value.var = "kor"), row.names = FALSE)
cat("\n7 valstu vidējā faktiskā un paredzētā pārmaiņa (kopīgā daļa)\n")
print(dcast(V[, .(v = sprintf("%+.2f / %+.2f", mean(parm_fakt), mean(parm_pred))), by = .(iznakums, merkis)], iznakums ~ merkis, value.var = "v"), row.names = FALSE)

cat("\n", strrep("=", 100), "\n4.2. Latviešu valodas plūsma: pārmaiņa un relatīvā pārmaiņa (fakt / pred) pa iznākumiem un mērķiem\n", strrep("=", 100), "\n", sep = "")
S7 <- V[, .(sf = mean(parm_fakt), sp = mean(parm_pred)), by = .(iznakums, merkis)]
LA <- merge(R[grupa == "LVA A"], S7, by = c("iznakums", "merkis"))[, `:=`(rel_fakt = parm_fakt - sf, rel_pred = parm_pred - sp)]
LA0 <- merge(R0[grupa == "LVA A"], V0[, .(sf = mean(parm_fakt), sp = mean(parm_pred)), by = .(iznakums, merkis)], by = c("iznakums", "merkis"))[, `:=`(rel_fakt = parm_fakt - sf, rel_pred = parm_pred - sp)]
LA2 <- rbind(LA[, .(iznakums, merkis, fakt, pred, parm_fakt, parm_pred, rel_fakt, rel_pred)], LA0[, .(iznakums, merkis, fakt, pred, parm_fakt, parm_pred, rel_fakt, rel_pred)])
print(dcast(LA2, iznakums ~ merkis, value.var = c("parm_fakt", "parm_pred", "rel_fakt", "rel_pred"), fun.aggregate = function(x) round(x[1], 2)), row.names = FALSE)
cat("\nLVA A līmeņi (fakt / pred) pa mērķiem:\n")
print(dcast(LA2, iznakums ~ merkis, value.var = c("fakt", "pred"), fun.aggregate = function(x) round(x[1], 2)), row.names = FALSE)
cat("\nLVA B (krievu valodas plūsma) faktiskā pārmaiņa, uzziņai (netiek modelēta atsevišķi):\n")
print(dcast(R[grupa == "LVA B", .(iznakums, merkis, v = sprintf("%+.2f / %+.2f", parm_fakt, parm_pred))], iznakums ~ merkis, value.var = "v"), row.names = FALSE)

cat("\n", strrep("=", 100), "\n4.3. SHAP pa jomām, LVA A relatīvi pret 7 valstīm, pa iznākumiem (2025 un 2022)\n", strrep("=", 100), "\n", sep = "")
SH0 <- fread("output/pisa2025/citi_iznakumi_shap.csv")[iznakums == "READ"]
SHA <- rbind(SH, SH0)
for (Tg in c("2025", "2022")) {
  dom <- SHA[merkis == Tg, .(s = sum(starpiba)), by = .(iznakums, grupa, domens = jomas(pazime))]
  DD <- merge(dom[grupa == "LVA A", .(iznakums, domens, lva = s)], dom[grupa %in% SALID, .(salid = mean(s)), by = .(iznakums, domens)], by = c("iznakums", "domens"), all = TRUE)
  for (x in c("lva", "salid")) set(DD, which(is.na(DD[[x]])), x, 0); DD[, rel := lva - salid]
  cat("\nMērķis", Tg, "\n"); print(dcast(DD, domens ~ iznakums, value.var = "rel", fun.aggregate = function(x) round(sum(x), 2)), row.names = FALSE)
}

cat("\n", strrep("=", 100), "\n4.4. Pazīmju līmenis 2025: LVA A relatīvais SHAP ieguldījums; korelācija ar lasīšanas vidējā karti\n", strrep("=", 100), "\n", sep = "")
f <- function(iz, Tg = "2025") merge(SHA[merkis == Tg & iznakums == iz & grupa == "LVA A", .(pazime, lva = starpiba)],
                                     SHA[merkis == Tg & iznakums == iz & grupa %in% SALID, .(salid = mean(starpiba)), by = pazime], by = "pazime")[, .(pazime, rel = lva - salid)]
XR <- f("READ")
for (iz in IZNAK) {
  X <- merge(f(iz$nos), XR, by = "pazime", suffixes = c("", "_read"))
  X[, `:=`(domens = jomas(pazime), apraksts = apraksts(pazime))]
  la <- LA[iznakums == iz$nos & merkis == "2025"]
  cat(sprintf("\n%s (2025): relatīvā paredzētā pārmaiņa LVA A %+.2f, faktiskā %+.2f; pazīmju relatīvo ieguldījumu korelācija ar READ karti: %.2f (%d pazīmes)\n",
              iz$nos, la$rel_pred, la$rel_fakt, X[, kor(rel, rel_read)], nrow(X)))
  o <- X[order(-abs(rel))][1:10]
  print(o[, .(pazime, domens, rel = round(rel, 2), rel_read = round(rel_read, 2), apraksts)], row.names = FALSE)
}
cat("\nKorelācijas matrica: pazīmju relatīvie ieguldījumi 2025 starp iznākumiem (LVA A pret 7 valstīm)\n")
FL <- Reduce(function(a, b) merge(a, b, by = "pazime"), lapply(c(sapply(IZNAK, `[[`, "nos"), "READ"), function(z) setnames(f(z), "rel", z)))
print(round(cor(as.matrix(FL[, -1])), 2))

beigt_zurnalu()
