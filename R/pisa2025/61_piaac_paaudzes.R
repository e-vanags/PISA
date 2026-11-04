# ============================================================================
# 61. PIAAC 2023: PISA paaudzes pieaugušo vecumā, latviešu valodas plūsma
#     (docs/57, citi mērījumi)
# ============================================================================
#
# JAUTĀJUMS. PIAAC 2023 (Pieaugušo prasmju pētījums, 2. cikls) izmēra tās pašas
# paaudzes, kuras PISA izmērīja 15 gados, ar citu testu, citu izlasi
# (mājsaimniecības, ne skolas) un bez skolas konteksta. Rezultāti publicēti 2024.
# gada decembrī, pirms PISA 2025 testa. Vai jaunākās paaudzes latviešu valodas
# plūsmā ir vājākas par vecākajām vairāk nekā citās valstīs, un vai paaudžu
# relatīvais stāvoklis saskan ar PISA?
#
# DATI. OECD publiskie faili (PUF) data/piaac2023/raw/PRG<valsts>P2.sav: Latvija un
# salīdzinājumam EST, LTU, POL, FIN, CZE, HUN, DEU (tās pašas, kas uzdevumu ķēdē).
# Dizains visās valstīs: Fay BRR, k = 0,3, 80 replikāti; 10 PV lasītprasmē (LIT),
# matemātiskajā pratībā (NUM), adaptīvajā problēmu risināšanā (APS).
# Dispersija: V = V_izl + (1 + 1/M) B, V_izl = Σ (θ_r − θ_0)² / (R (1 − k)²), vidēji pa PV.
#
# LATVIEŠU VALODAS PLŪSMA (A): testa valoda latviešu (LNG_CI = LAV); tiem, kas testu
# nepildīja (330, gandrīz visi 35+; PV imputēti no aptaujas), aptaujas valoda
# latviešu. Salīdzināmajām valstīm visi iedzīvotāji, kā PISA valstu vidējos.
# Jutīgums: tikai testa valoda; mājās runā latviski.
#
# PAAUDZES. 5 gadu vecuma grupas (AGEG5LFS; precīza vecuma nav EST, HUN, DEU failos).
# Dati vākti 2022. g. septembrī – 2023. g. augustā. Grupas [a1, a2] vidējais PISA gads
# (dzimšanas gads + 16, kā PISA 2018 = dz. 2002): 2039 − (a1 + a2 + 1) / 2.
#   16–19 → ~2021 (PISA 2018–2022)   20–24 → ~2016,5 (2015–2018)
#   25–29 → ~2011,5 (2009–2012)       30–34 → ~2006,5 (2006–2009)   35–39 → ~2001,5 (2000–2003)
#
# ANALĪZES
#   1. Izlase
#   2. Vecuma profils: vidējais pa 5 gadu grupām
#   3. Jaunāko paaudžu starpība (16–24) − (25–34) katrā valstī. Latvijai specifiskā =
#      LVA A − salīdzināmo valstu vidējais: vecuma efekts, kas visās valstīs līdzīgs, atkrīt
#   4. Paaudžu relatīvais stāvoklis: LVA A − valstu kopas vidējais katrā grupā, blakus
#      PISA A relatīvajam stāvoklim tai pašai paaudzei 15 gados
#   5. Sadalījums: P10, P50, P90, 1. līmenis un zemāk (< 226), 4.–5. līmenis (≥ 326)
#   6. Apakšgrupas: dzimums; vecāku izglītība (jaunie pārsvērti uz 25–34 sadalījumu)
#   7. Fons un paradumi: grāmatas mājās 14 gados (tās pašas kategorijas kā PISA),
#      grāmatu lasīšana ikdienā, lasīšanas indekss mājās
#   8. Matemātiskā pratība un problēmu risināšana
#   9. Jutīgums: A definīcija, tikai valstī dzimušie, Igaunija tikai igauņu valodā,
#      5 valstu kopa, atskaites grupa 30–44, pa vienai izslēgta salīdzināmā valsts
#
# Izvade:  output/pisa2025/piaac_statistikas.csv    visas valstis, grupas, rādītāji
#          output/pisa2025/piaac_latvija.csv        LVA A pret valstu kopu
#          output/pisa2025/piaac_paaudzes_pisa.csv  paaudžu salīdzinājums ar PISA
#          output/pisa2025/logs/61_piaac_paaudzes_output.txt
# Avots: Programme for the International Assessment of Adult Competencies (PIAAC),
#        Organisation for Economic Co-operation and Development (OECD), Paris.
# ============================================================================

suppressPackageStartupMessages(library(haven))

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/61_piaac_paaudzes_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

VALSTIS <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
SAL7 <- VALSTIS[-1]; SAL5 <- c("POL", "FIN", "CZE", "HUN", "DEU")
M <- 10; R <- 80; FAY <- 1 / (R * (1 - 0.3)^2)
L1 <- 226; L4 <- 326                       # 1. līmenis un zemāk < 226; 4.–5. līmenis ≥ 326
GR5 <- c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-65")
PISA_GADS <- c(2021, 2016.5, 2011.5, 2006.5, 2001.5)   # grupām 16–19 … 35–39
NOBIDE_2018 <- 9.6                          # docs/55: Latvijai specifiskā nobīde 2018, vienreizēja

# ---------------------------------------------------------------------------
# Dati
# ---------------------------------------------------------------------------
K <- "data/piaac2023/processed/piaac_8_v2.rds"
if (!file.exists(K)) {
  V <- c("AGEG5LFS", "AGE_R", "GENDER_R", "LNG_CI", "LNG_BQ", "LNG_HOME", "A2_Q03a", "PAREDC2", "EDE_ISCED1_C",
         "J2_Q06", "G2_Q01d", "READHOMEC2_T1", paste0("PV", rep(c("LIT", "NUM", "APS"), each = 10), 1:10),
         paste0("SPFWT", 0:80))
  dir.create(dirname(K), showWarnings = FALSE, recursive = TRUE)
  out <- list()
  for (c0 in VALSTIS) {
    d <- haven::read_sav(sprintf("data/piaac2023/raw/PRG%sP2.sav", c0), col_select = tidyselect::all_of(V))
    lab <- function(x) as.character(haven::as_factor(x))
    kat <- function(x, der) { x <- as.numeric(x); x[!(x %in% der)] <- NA; x }
    sk <- data.frame(band = kat(d$AGEG5LFS, 1:10), age = kat(d$AGE_R, 16:65), female = as.numeric(d$GENDER_R) == 2,
      native = as.numeric(d$A2_Q03a) == 1, pared = kat(d$PAREDC2, 1:3), enrolled = !is.na(as.numeric(d$EDE_ISCED1_C)),
      books = kat(d$J2_Q06, 1:6), readbooks = kat(d$G2_Q01d, 1:5), readhome = as.numeric(d$READHOMEC2_T1),
      ci = lab(d$LNG_CI), bq = lab(d$LNG_BQ), home = lab(d$LNG_HOME), stringsAsFactors = FALSE)
    mat <- function(p) sapply(paste0(p, 1:10), function(k) as.numeric(d[[k]]))
    out[[c0]] <- list(sk = sk, LIT = mat("PVLIT"), NUM = mat("PVNUM"), APS = mat("PVAPS"),
                      W = sapply(paste0("SPFWT", 0:80), function(k) as.numeric(d[[k]])))
  }
  saveRDS(out, K)
}
P <- readRDS(K)

grupas <- function(c0, sk) {
  g <- list(visi = rep(TRUE, nrow(sk)))
  if (c0 == "LVA") {
    g$A <- sk$ci == "LAV" | (!(sk$ci %in% c("LAV", "RUS")) & sk$bq == "LAV")
    g$A_tests <- sk$ci == "LAV"
    g$A_maja <- sk$home == "Latvian"
  }
  if (c0 == "EST") g$igauņu <- sk$ci == "EST" | (!(sk$ci %in% c("EST", "RUS")) & sk$bq == "EST")
  lapply(g, function(x) x %in% TRUE)
}

# ---------------------------------------------------------------------------
# Novērtēšana: fun(y, w) atgriež nosauktu vektoru; y = viena PV kolonna (NULL fonam)
# ---------------------------------------------------------------------------
rubin <- function(X, dom, fun) {
  MM <- if (is.null(dom)) 1 else M
  E <- NULL; vs <- 0
  for (m in seq_len(MM)) {
    y <- if (is.null(dom)) NULL else X[[dom]][, m]
    t0 <- fun(y, X$W[, 1])
    tr <- matrix(vapply(2:(R + 1), function(r) fun(y, X$W[, r]), t0), nrow = length(t0))
    vs <- vs + FAY * rowSums((tr - t0)^2)
    E <- cbind(E, t0)
  }
  B <- if (MM > 1) apply(E, 1, stats::var) else 0
  data.frame(radītājs = names(t0), est = rowMeans(E), se = sqrt(vs / MM + (1 + 1 / MM) * B), row.names = NULL)
}
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }

f_main <- function(sk, a) {
  b <- sk$band; fem <- sk$female %in% TRUE; mal <- sk$female %in% FALSE; pe <- sk$pared
  ib <- lapply(1:10, function(k) a & b %in% k)
  jn <- a & b %in% 1:2; rf <- a & b %in% 3:4; rf2 <- a & b %in% 4:6; vc <- a & b %in% 5:6
  function(y, w) {
    wm <- function(j) sum(w[j] * y[j]) / sum(w[j])
    sh <- function(j, cond) 100 * sum(w[j] * cond[j]) / sum(w[j])
    m5 <- vapply(ib, wm, 1); mj <- wm(jn); mr <- wm(rf); mr2 <- wm(rf2); mv <- wm(vc)
    qj <- wq(y[jn], w[jn], c(.1, .5, .9)); qr <- wq(y[rf], w[rf], c(.1, .5, .9))
    lo <- y < L1; hi <- y >= L4
    gj <- wm(jn & fem) - wm(jn & mal); gr <- wm(rf & fem) - wm(rf & mal)
    sr <- vapply(1:3, function(p) sum(w[rf & pe %in% p]), 1); sr <- sr / sum(sr)
    mjp <- sum(sr * vapply(1:3, function(p) wm(jn & pe %in% p), 1))
    mrp <- sum(sr * vapply(1:3, function(p) wm(rf & pe %in% p), 1))
    c(stats::setNames(m5, paste0("vid_", GR5)),
      vid_16_24 = mj, vid_25_34 = mr, vid_35_44 = mv, vid_30_44 = mr2,
      starp_16_24 = mj - mr, starp_16_19 = m5[1] - mr, starp_20_24 = m5[2] - mr,
      starp_16_24_pret_30_44 = mj - mr2, starp_25_34_pret_35_44 = mr - mv,
      P10_16_24 = qj[1], P50_16_24 = qj[2], P90_16_24 = qj[3],
      P10_25_34 = qr[1], P50_25_34 = qr[2], P90_25_34 = qr[3],
      starp_P10 = qj[1] - qr[1], starp_P50 = qj[2] - qr[2], starp_P90 = qj[3] - qr[3],
      zem2_16_24 = sh(jn, lo), zem2_25_34 = sh(rf, lo), starp_zem2 = sh(jn, lo) - sh(rf, lo),
      l45_16_24 = sh(jn, hi), l45_25_34 = sh(rf, hi), starp_l45 = sh(jn, hi) - sh(rf, hi),
      dzim_16_24 = gj, dzim_25_34 = gr, starp_dzim = gj - gr,
      starp_siev = wm(jn & fem) - wm(rf & fem), starp_vir = wm(jn & mal) - wm(rf & mal),
      starp_16_24_vecaki = mjp - mrp)
  }
}

NMB <- c(GR5, "16-24", "25-34", "35-44", "45-65")
f_bg <- function(sk, a, aflag = NULL) {
  b <- sk$band
  sets <- c(lapply(1:10, function(k) a & b %in% k), list(a & b %in% 1:2, a & b %in% 3:4, a & b %in% 5:6, a & b %in% 7:10))
  jn <- a & b %in% 1:2; vc <- a & b %in% 5:6
  function(y, w) {
    sh <- function(j, v, cond) { k <- j & !is.na(v); 100 * sum(w[k] * cond(v[k])) / sum(w[k]) }
    mn <- function(j, v) { k <- j & !is.na(v); sum(w[k] * v[k]) / sum(w[k]) }
    g200 <- function(v) v >= 5; g10 <- function(v) v <= 1; nel <- function(v) v == 1; ned <- function(v) v >= 4
    out <- c(
      stats::setNames(vapply(sets, sh, 1, v = sk$books, cond = g200), paste0("gram_200plus_", NMB)),
      stats::setNames(vapply(sets, sh, 1, v = sk$books, cond = g10), paste0("gram_lidz10_", NMB)),
      stats::setNames(vapply(sets, sh, 1, v = sk$readbooks, cond = nel), paste0("gramatas_nelasa_", NMB)),
      stats::setNames(vapply(sets, sh, 1, v = sk$readbooks, cond = ned), paste0("gramatas_katru_ned_", NMB)),
      stats::setNames(vapply(sets, mn, 1, v = sk$readhome), paste0("lasis_indekss_", NMB)),
      stats::setNames(vapply(sets, sh, 1, v = sk$pared, cond = function(v) v == 3), paste0("vecaki_augst_", NMB)),
      stats::setNames(vapply(sets, sh, 1, v = as.numeric(sk$enrolled), cond = function(v) v == 1), paste0("macas_", NMB)),
      starp_gram_200plus = sh(jn, sk$books, g200) - sh(vc, sk$books, g200),
      starp_gram_lidz10 = sh(jn, sk$books, g10) - sh(vc, sk$books, g10),
      starp_gramatas_nelasa = sh(jn, sk$readbooks, nel) - sh(vc, sk$readbooks, nel),
      starp_lasis_indekss = mn(jn, sk$readhome) - mn(vc, sk$readhome))
    if (!is.null(aflag)) out <- c(out, stats::setNames(vapply(1:10, function(k) {
      j <- b %in% k; 100 * sum(w[j & aflag]) / sum(w[j]) }, 1), paste0("A_daļa_", GR5)))
    out
  }
}

# Precīzs vecums (tikai valstīs, kur tas ir PUF): divu gadu logi, kas atbilst
# konkrētiem PISA cikliem (PISA gads = dzimšanas gads + 16; aptauja 2022/23)
AR_VECUMU <- c("LVA", "LTU", "POL", "FIN", "CZE"); SAL4 <- AR_VECUMU[-1]
LOGI <- list("16-17" = 16:17, "20-21" = 20:21, "23-24" = 23:24, "26-27" = 26:27, "29-30" = 29:30, "32-33" = 32:33)
PISA_LOGI <- c(2022, 2018, 2015, 2012, 2009, 2006)
f_vec <- function(sk, a) {
  ib <- lapply(LOGI, function(v) a & sk$age %in% v); k <- length(LOGI)
  function(y, w) {
    m <- vapply(ib, function(j) sum(w[j] * y[j]) / sum(w[j]), 1)
    c(stats::setNames(m, paste0("vec_", names(LOGI))),
      stats::setNames(m[-k] - m[-1], paste0("pāris_", names(LOGI)[-k], "_", names(LOGI)[-1])))
  }
}

# ---------------------------------------------------------------------------
# Visas aplēses
# ---------------------------------------------------------------------------
t0 <- Sys.time()
S <- list()
for (c0 in VALSTIS) {
  X <- P[[c0]]; sk <- X$sk; G <- grupas(c0, sk)
  for (g in names(G)) {
    doms <- if (g %in% c("visi", "A", "igauņu")) c("LIT", "NUM", "APS") else "LIT"
    for (dm in doms) S[[length(S) + 1]] <- cbind(valsts = c0, grupa = g, joma = dm, rubin(X, dm, f_main(sk, G[[g]])))
    af <- if (c0 == "LVA" && g == "visi") G$A else NULL
    S[[length(S) + 1]] <- cbind(valsts = c0, grupa = g, joma = "fons", rubin(X, NULL, f_bg(sk, G[[g]], af)))
    if (c0 %in% AR_VECUMU && g %in% c("visi", "A")) for (dm in c("LIT", "NUM"))
      S[[length(S) + 1]] <- cbind(valsts = c0, grupa = g, joma = paste0(dm, "_vec"), rubin(X, dm, f_vec(sk, G[[g]])))
    if (g %in% c("visi", "A", "igauņu")) {
      a2 <- G[[g]] & sk$native %in% TRUE
      S[[length(S) + 1]] <- cbind(valsts = c0, grupa = paste0(g, "_dzimuši"), joma = "LIT", rubin(X, "LIT", f_main(sk, a2)))
    }
  }
  cat(sprintf("  %s aprēķināts (%.0f s)\n", c0, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
S <- do.call(rbind, S)
utils::write.csv(S, "output/pisa2025/piaac_statistikas.csv", row.names = FALSE)

ge <- function(c0, gr, dm, st) {
  x <- S[S$valsts == c0 & S$grupa == gr & S$joma == dm & S$radītājs == st, ]
  if (!nrow(x)) c(est = NA_real_, se = NA_real_) else c(est = x$est[1], se = x$se[1])
}
# LVA grupa pret salīdzināmo valstu vidējo (neatkarīgas izlases)
pret_kopu <- function(dm, st, lv = "A", kopa = SAL7, kg = "visi", eg = kg) {
  a <- ge("LVA", lv, dm, st)
  k <- t(vapply(kopa, function(c0) ge(c0, if (c0 == "EST") eg else kg, dm, st), c(est = 0, se = 0)))
  d <- a[["est"]] - mean(k[, "est"]); s <- sqrt(a[["se"]]^2 + sum(k[, "se"]^2) / length(kopa)^2)
  loo <- vapply(seq_along(kopa), function(i) a[["est"]] - mean(k[-i, "est"]), 1)
  data.frame(joma = dm, radītājs = st, lv_grupa = lv, kopa = paste(kopa, collapse = " "), kopa_grupa = kg,
    lv = a[["est"]], lv_se = a[["se"]], kopa_vid = mean(k[, "est"]), kopa_min = min(k[, "est"]), kopa_max = max(k[, "est"]),
    starp = d, se = s, z = d / s, loo_min = min(loo), loo_max = max(loo), rangs = sum(k[, "est"] > a[["est"]]) + 1,
    no = length(kopa) + 1)
}
fm <- function(e, s, dg = 1, zv = TRUE) if (is.na(e)) "      –      " else
  sprintf(paste0("%+.", dg, "f (%.", dg, "f)%s"), e, s, if (zv && !is.na(s) && s > 0 && abs(e / s) > 1.96) "*" else " ")
LV <- list()
lvrinda <- function(...) { r <- pret_kopu(...); LV[[length(LV) + 1]] <<- r; r }

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n1. Izlase\n", strrep("=", 100), "\n\n")
cat(sprintf("  %-10s %s\n", "", paste(sprintf("%6s", GR5), collapse = "")))
for (c0 in VALSTIS) {
  sk <- P[[c0]]$sk
  cat(sprintf("  %-10s %s\n", c0, paste(sprintf("%6d", tabulate(sk$band, 10)), collapse = "")))
  if (c0 == "LVA") { A <- grupas("LVA", sk)$A
    cat(sprintf("  %-10s %s\n", "LVA A", paste(sprintf("%6d", tabulate(sk$band[A], 10)), collapse = "")))
    cat(sprintf("  %-10s %s\n", "A daļa, %", paste(sprintf("%6.1f", vapply(GR5, function(g) ge("LVA", "visi", "fons", paste0("A_daļa_", g))[["est"]], 1)), collapse = ""))) }
}
cat("\n  Mācās pašlaik (%, 16-19 / 20-24):",
    paste(vapply(VALSTIS, function(c0) sprintf("%s %.0f/%.0f", c0, ge(c0, "visi", "fons", "macas_16-19")[["est"]],
                                                ge(c0, "visi", "fons", "macas_20-24")[["est"]]), ""), collapse = "; "), "\n")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n2. Vecuma profils, lasītprasme (vidējais)\n", strrep("=", 100), "\n\n")
prof <- function(dm) {
  cat(sprintf("  %-12s %s\n", "", paste(sprintf("%7s", GR5), collapse = "")))
  rows <- list(c("LVA", "A", "LVA A"), c("LVA", "visi", "LVA visi"), c("EST", "igauņu", "EST igauņu"))
  rows <- c(rows, lapply(SAL7, function(c0) c(c0, "visi", c0)))
  for (r in rows) cat(sprintf("  %-12s %s\n", r[3], paste(vapply(GR5, function(g)
    sprintf("%7.1f", ge(r[1], r[2], dm, paste0("vid_", g))[["est"]]), ""), collapse = "")))
  km <- vapply(GR5, function(g) mean(vapply(SAL7, function(c0) ge(c0, "visi", dm, paste0("vid_", g))[["est"]], 1)), 1)
  cat(sprintf("  %-12s %s\n", "7 valstu vid.", paste(sprintf("%7.1f", km), collapse = "")))
  cat(sprintf("  %-12s %s\n", "LVA A − vid.", paste(sprintf("%+7.1f", vapply(GR5, function(g)
    ge("LVA", "A", dm, paste0("vid_", g))[["est"]], 1) - km), collapse = "")))
}
prof("LIT")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n3. Jaunāko paaudžu starpība pret 25-34 gadus veciem, lasītprasme\n", strrep("=", 100), "\n\n")
ST3 <- c("starp_16_24", "starp_16_19", "starp_20_24", "starp_16_24_pret_30_44", "starp_25_34_pret_35_44")
cat(sprintf("  %-12s %16s %16s %16s %16s %16s\n", "", "16-24 − 25-34", "16-19 − 25-34", "20-24 − 25-34", "16-24 − 30-44", "25-34 − 35-44"))
for (r in c(list(c("LVA", "A", "LVA A"), c("LVA", "visi", "LVA visi"), c("EST", "igauņu", "EST igauņu")),
            lapply(SAL7, function(c0) c(c0, "visi", c0)))) {
  cat(sprintf("  %-12s %s\n", r[3], paste(vapply(ST3, function(st) { x <- ge(r[1], r[2], "LIT", st); sprintf("%16s", fm(x[["est"]], x[["se"]])) }, ""), collapse = " ")))
}
cat("\n  Latvijai specifiskā starpība (LVA A − 7 valstu vidējais), LOO = pa vienai izslēgta valsts; rangs no 8 (1 = mazākais kritums):\n")
for (st in ST3) { r <- lvrinda("LIT", st)
  cat(sprintf("    %-24s LVA A %s | 7 valstis %+6.1f (%+.1f … %+.1f) | Latvijai specifiskā %s | LOO %+.1f … %+.1f | rangs %d\n",
      st, fm(r$lv, r$lv_se, zv = grepl("^starp|^dzim|^pāris", st)), r$kopa_vid, r$kopa_min, r$kopa_max, fm(r$starp, r$se), r$loo_min, r$loo_max, r$rangs)) }

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n4. Paaudžu relatīvais stāvoklis: PIAAC 2023 pret PISA tai pašai paaudzei\n", strrep("=", 100), "\n\n")
PV <- utils::read.csv("output/pisa2025/pisa_valstu_videjie.csv", stringsAsFactors = FALSE)
VER <- utils::read.csv("output/pisa2025/verifikacija.csv", stringsAsFactors = FALSE)
CJ <- utils::read.csv("output/pisa2025/citas_jomas_limeni.csv", stringsAsFactors = FALSE)
rk <- grep("^rad", names(CJ), value = TRUE)[1]
A_LIT <- with(unique(VER[VER$grupa == "A", c("gads", "aplese")]), stats::setNames(aplese, gads))
A_LIT_K <- A_LIT + ifelse(as.numeric(names(A_LIT)) >= 2018, NOBIDE_2018, 0)
A_NUM <- with(CJ[CJ$joma == "MATH" & CJ[[rk]] == "vid", ], stats::setNames(vertiba, gads))
LV_LIT <- with(PV[PV$cnt == "LVA", ], stats::setNames(lasisana, gads))
LV_NUM <- with(PV[PV$cnt == "LVA", ], stats::setNames(matematika, gads))
pisa_rel <- function(ser, joma, kopa) {
  g <- names(ser)
  km <- vapply(g, function(y) { x <- PV[PV$gads == y, ]; v <- x[[joma]][match(kopa, x$cnt)]
    if (any(is.na(v))) NA_real_ else mean(v) }, 1)
  d <- ser - km; ok <- !is.na(d); yy <- as.numeric(g[ok])
  data.frame(pisa_gads = PISA_GADS, starp = stats::approx(yy, d[ok], xout = PISA_GADS, rule = 2)$y,
             ekstrapol = PISA_GADS < min(yy) | PISA_GADS > max(yy))
}
sd_piaac <- function(dm, kopa) mean(vapply(kopa, function(c0) { X <- P[[c0]]; w <- X$W[, 1]
  mean(apply(X[[dm]], 2, function(y) { m <- sum(w * y) / sum(w); sqrt(sum(w * (y - m)^2) / sum(w)) })) }, 1))
SD_PISA <- 100
koh <- list()
paaudzes <- function(dm, kopa, knos, ser_list) {
  sdp <- sd_piaac(dm, kopa)
  cat(sprintf("  %s, kopa %s (%s); PIAAC vidējā SD valstī %.1f, PISA SD pieņemta %d\n", dm, knos, paste(kopa, collapse = " "), sdp, SD_PISA))
  cat(sprintf("  %-6s %-9s %18s %8s | %s\n", "grupa", "PISA gads", "PIAAC LVA A − kopa", "SD", paste(sprintf("%-26s", names(ser_list)), collapse = "")))
  pr <- lapply(ser_list, function(s) pisa_rel(s, if (dm == "LIT") "lasisana" else "matematika", kopa))
  for (b in 1:5) {
    lvg <- if (grepl("visa Latvija", names(ser_list)[1])) "visi" else "A"
    r <- pret_kopu(dm, paste0("vid_", GR5[b]), lv = lvg, kopa = kopa)
    cel <- vapply(seq_along(pr), function(i) sprintf("%+6.1f (%+.2f SD)%s       ", pr[[i]]$starp[b], pr[[i]]$starp[b] / SD_PISA,
                  if (pr[[i]]$ekstrapol[b]) "¹" else " "), "")
    cat(sprintf("  %-6s %-9.1f %18s %+7.2f  | %s\n", GR5[b], PISA_GADS[b], fm(r$starp, r$se), r$starp / sdp, paste(cel, collapse = "")))
    koh[[length(koh) + 1]] <<- data.frame(joma = dm, kopa = knos, lv_grupa = lvg, grupa = GR5[b], pisa_gads = PISA_GADS[b],
      piaac_starp = r$starp, piaac_se = r$se, piaac_sd = r$starp / sdp,
      pisa_1 = pr[[1]]$starp[b], pisa_2 = if (length(pr) > 1) pr[[2]]$starp[b] else NA_real_,
      pisa_nosaukumi = paste(names(ser_list), collapse = " | "), ekstrapol = pr[[1]]$ekstrapol[b])
  }
  cat("\n")
}
paaudzes("LIT", SAL7, "7 valstis", list("PISA A oficiālā" = A_LIT, "PISA A, 2018+ koriģēta" = A_LIT_K))
paaudzes("LIT", SAL5, "5 valstis", list("PISA A oficiālā" = A_LIT, "PISA A, 2018+ koriģēta" = A_LIT_K))
paaudzes("LIT", SAL7, "7 valstis", list("PISA visa Latvija" = LV_LIT))
cat("  ¹ PISA gads ārpus pieejamās A rindas (2003–2025); ņemta tuvākā vērtība.\n",
    " PISA 2018+ koriģētā: + 9,6 punkti no 2018. gada (docs/55, vienreizēja Latvijai specifiska nobīde).\n")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n4b. Konkrēti PISA cikli pēc precīza vecuma (LVA, LTU, POL, FIN, CZE)\n", strrep("=", 100), "\n\n")
cat("  Divu gadu vecuma logs ↔ PISA cikls (dzimšanas gads): 16-17 ↔ 2022 (2005-06), 20-21 ↔ 2018 (2001-02),\n",
    " 23-24 ↔ 2015 (1998-99), 26-27 ↔ 2012 (1995-96), 29-30 ↔ 2009 (1992-93), 32-33 ↔ 2006 (1989-90).\n",
    " Precīza vecuma nav EST, HUN, DEU publiskajos failos, tāpēc kopa ir 4 valstis.\n\n")
pisa_pmain <- function(ser, joma, y1, y0, kopa) {
  x1 <- PV[PV$gads == y1, ]; x0 <- PV[PV$gads == y0, ]
  (ser[[as.character(y1)]] - ser[[as.character(y0)]]) -
    mean(x1[[joma]][match(kopa, x1$cnt)] - x0[[joma]][match(kopa, x0$cnt)])
}
for (dm in c("LIT", "NUM")) {
  dmv <- paste0(dm, "_vec"); A <- grupas("LVA", P$LVA$sk)$A
  cat(sprintf("  %s: līmenis pret 4 valstu vidējo\n", dm))
  for (i in seq_along(LOGI)) { st <- paste0("vec_", names(LOGI)[i]); r <- lvrinda(dmv, st, kopa = SAL4)
    cat(sprintf("    %-6s (PISA %d)  n LVA A %4d | LVA A %s | 4 valstis %6.1f | LVA A − valstis %s\n", names(LOGI)[i], PISA_LOGI[i],
        sum(A & P$LVA$sk$age %in% LOGI[[i]]), fm(r$lv, r$lv_se, zv = FALSE), r$kopa_vid, fm(r$starp, r$se))) }
  cat(sprintf("\n  %s: jaunākā paaudze − iepriekšējā (Latvijai specifiskā = LVA A − 4 valstu vidējais), blakus PISA\n", dm))
  ser <- if (dm == "LIT") list(of = A_LIT, kor = A_LIT_K) else list(of = A_NUM)
  jom <- if (dm == "LIT") "lasisana" else "matematika"
  for (i in 1:(length(LOGI) - 1)) {
    st <- paste0("pāris_", names(LOGI)[i], "_", names(LOGI)[i + 1]); r <- lvrinda(dmv, st, kopa = SAL4)
    pp <- vapply(ser, function(s) pisa_pmain(s, jom, PISA_LOGI[i], PISA_LOGI[i + 1], SAL4), 1)
    cat(sprintf("    %-14s PISA %d−%d | PIAAC LVA A %s, 4 valstis %+6.1f, Latvijai specifiskā %s (%+.2f SD) | PISA: %s\n",
        paste(names(LOGI)[i], "−", names(LOGI)[i + 1]), PISA_LOGI[i], PISA_LOGI[i + 1], fm(r$lv, r$lv_se), r$kopa_vid,
        fm(r$starp, r$se), r$starp / sd_piaac(dm, SAL4),
        paste(sprintf("%s %+.1f (%+.2f SD)", c(of = "oficiālā", kor = "koriģētā")[names(pp)], pp, pp / SD_PISA), collapse = ", ")))
  }
  cat("\n")
}
cat("  PISA: Latvijas A pārmaiņa starp cikliem − 4 valstu vidējā pārmaiņa (valstu vidējie, bez SE).\n")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n5. Sadalījums: 16-24 pret 25-34, lasītprasme\n", strrep("=", 100), "\n\n")
ST5 <- c("P10_16_24", "P10_25_34", "starp_P10", "P50_16_24", "P50_25_34", "starp_P50", "P90_16_24", "P90_25_34", "starp_P90",
         "zem2_16_24", "zem2_25_34", "starp_zem2", "l45_16_24", "l45_25_34", "starp_l45")
for (st in ST5) { r <- lvrinda("LIT", st)
  cat(sprintf("  %-12s LVA A %s | 7 valstis %6.1f (%.1f … %.1f) | LVA A − valstis %s | rangs %d\n",
      st, fm(r$lv, r$lv_se, zv = grepl("^starp|^dzim|^pāris", st)), r$kopa_vid, r$kopa_min, r$kopa_max, fm(r$starp, r$se), r$rangs)) }
cat("  zem2 = 1. līmenis un zemāk (< 226 punkti), %; l45 = 4.–5. līmenis (≥ 326), %\n")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n6. Apakšgrupas: dzimums un vecāku izglītība, lasītprasme\n", strrep("=", 100), "\n\n")
for (st in c("dzim_16_24", "dzim_25_34", "starp_dzim", "starp_siev", "starp_vir", "starp_16_24", "starp_16_24_vecaki")) {
  r <- lvrinda("LIT", st)
  cat(sprintf("  %-20s LVA A %s | 7 valstis %+6.1f (%+.1f … %+.1f) | LVA A − valstis %s | rangs %d\n",
      st, fm(r$lv, r$lv_se, zv = grepl("^starp|^dzim|^pāris", st)), r$kopa_vid, r$kopa_min, r$kopa_max, fm(r$starp, r$se), r$rangs)) }
cat("  dzim = sievietes − vīrieši; starp_siev / starp_vir = 16-24 − 25-34 katram dzimumam;\n",
    " starp_16_24_vecaki = 16-24 − 25-34, abas grupas pārsvērtas uz 25-34 vecāku izglītības sadalījumu\n")

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n7. Fons un paradumi pa paaudzēm\n", strrep("=", 100), "\n\n")
PAR <- utils::read.csv("output/pisa2025/paradumi.csv", stringsAsFactors = FALSE)
rp <- grep("^rad", names(PAR), value = TRUE)[1]
pisa_gram <- function(pat) { x <- PAR[grepl(pat, PAR[[rp]]) & PAR$grupa == "A", ]
  stats::approx(as.numeric(x$gads), x$pct, xout = PISA_GADS, rule = 2)$y }
PG200 <- pisa_gram("vairāk nekā 200"); PG10 <- pisa_gram("ne vairāk kā 10")
for (ind in c("gram_200plus", "gram_lidz10", "gramatas_nelasa", "gramatas_katru_ned", "lasis_indekss", "vecaki_augst")) {
  cat(sprintf("  %s\n  %-12s %s\n", ind, "", paste(sprintf("%7s", NMB[1:10]), collapse = "")))
  for (r in c(list(c("LVA", "A", "LVA A"), c("EST", "igauņu", "EST igauņu")), lapply(SAL7, function(c0) c(c0, "visi", c0))))
    cat(sprintf("  %-12s %s\n", r[3], paste(vapply(NMB[1:10], function(g) sprintf("%7.1f", ge(r[1], r[2], "fons", paste0(ind, "_", g))[["est"]]), ""), collapse = "")))
  if (ind %in% c("gram_200plus", "gram_lidz10")) {
    pg <- if (ind == "gram_200plus") PG200 else PG10
    cat(sprintf("  %-12s %s   (PISA A 15 gados, tā pati paaudze; 16-19 = 2018. g. vērtība)\n", "PISA A", paste(sprintf("%7.1f", pg), collapse = "")))
  }
}
cat("\n  Starpība 16-24 − 35-44 (procentpunkti; indeksam punkti):\n")
for (st in c("starp_gram_200plus", "starp_gram_lidz10", "starp_gramatas_nelasa", "starp_lasis_indekss")) {
  r <- lvrinda("fons", st)
  cat(sprintf("    %-22s LVA A %s | 7 valstis %+6.1f (%+.1f … %+.1f) | LVA A − valstis %s | rangs %d\n",
      st, fm(r$lv, r$lv_se, zv = grepl("^starp|^dzim|^pāris", st)), r$kopa_vid, r$kopa_min, r$kopa_max, fm(r$starp, r$se), r$rangs)) }

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n8. Matemātiskā pratība (NUM) un problēmu risināšana (APS)\n", strrep("=", 100), "\n\n")
for (dm in c("NUM", "APS")) {
  cat(" ", dm, "vecuma profils\n"); prof(dm); cat("\n")
  for (st in c("starp_16_24", "starp_16_19", "starp_20_24", "starp_25_34_pret_35_44")) { r <- lvrinda(dm, st)
    cat(sprintf("    %-24s LVA A %s | 7 valstis %+6.1f (%+.1f … %+.1f) | Latvijai specifiskā %s | rangs %d\n",
        st, fm(r$lv, r$lv_se, zv = grepl("^starp|^dzim|^pāris", st)), r$kopa_vid, r$kopa_min, r$kopa_max, fm(r$starp, r$se), r$rangs)) }
  cat("\n")
}
paaudzes("NUM", SAL7, "7 valstis", list("PISA A matemātika" = A_NUM))
paaudzes("NUM", SAL5, "5 valstis", list("PISA A matemātika" = A_NUM))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n9. Jutīgums: lasītprasme\n", strrep("=", 100), "\n\n")
VAR <- list(
  "galvenais (A, 7 valstis)"          = list(lv = "A", kopa = SAL7, kg = "visi", eg = "visi"),
  "A tikai pēc testa valodas"         = list(lv = "A_tests", kopa = SAL7, kg = "visi", eg = "visi"),
  "A pēc mājas valodas"               = list(lv = "A_maja", kopa = SAL7, kg = "visi", eg = "visi"),
  "tikai valstī dzimušie"             = list(lv = "A_dzimuši", kopa = SAL7, kg = "visi_dzimuši", eg = "visi_dzimuši"),
  "Igaunija tikai igauņu valodā"      = list(lv = "A", kopa = SAL7, kg = "visi", eg = "igauņu"),
  "5 valstu kopa"                     = list(lv = "A", kopa = SAL5, kg = "visi", eg = "visi"),
  "visa Latvija"                      = list(lv = "visi", kopa = SAL7, kg = "visi", eg = "visi"))
cat(sprintf("  %-32s %18s %18s %18s %18s %18s\n", "", "16-24 − 25-34", "16-24 − 30-44", "16-19: pret kopu", "20-24: pret kopu", "25-29: pret kopu"))
jut <- list()
for (nm in names(VAR)) { v <- VAR[[nm]]
  rr <- lapply(c("starp_16_24", "starp_16_24_pret_30_44", "vid_16-19", "vid_20-24", "vid_25-29"), function(st)
    pret_kopu("LIT", st, lv = v$lv, kopa = v$kopa, kg = v$kg, eg = v$eg))
  jut[[nm]] <- cbind(variants = nm, do.call(rbind, rr))
  cat(sprintf("  %-32s %s\n", nm, paste(vapply(rr, function(r) sprintf("%18s", fm(r$starp, r$se)), ""), collapse = " ")))
}
cat("\n  Tabulā: Latvijas grupa − salīdzināmo valstu vidējais. Pirmās divas kolonnas: starpību starpība\n",
    " (vecuma efekts atkrīt); pēdējās trīs: līmenis pret valstu kopu tajā pašā vecuma grupā.\n")

LV <- do.call(rbind, LV); LV$variants <- "galvenais"
jut <- do.call(rbind, jut)
utils::write.csv(rbind(LV, jut[, names(LV)]), "output/pisa2025/piaac_latvija.csv", row.names = FALSE)
utils::write.csv(do.call(rbind, koh), "output/pisa2025/piaac_paaudzes_pisa.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/piaac_statistikas.csv\n  output/pisa2025/piaac_latvija.csv\n",
    " output/pisa2025/piaac_paaudzes_pisa.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
