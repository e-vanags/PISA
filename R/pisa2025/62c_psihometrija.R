# ============================================================================
# 62c. PISA lasīšanas mērījuma psihometriskā kvalitāte Latvijā 2015–2025 (docs/59)
# ============================================================================
#
# JAUTĀJUMS. Vai PISA lasīšanas tests Latvijā mērīja ticami, vienādi abās valodu
# versijās un to pašu konstruktu visos ciklos? Latvijas ziņojumos to neapspriež;
# PISA tehniskie ziņojumi dod tikai starptautiskus kopsavilkumus.
#
# DATI. 62a izvilkums (visi lasīšanas uzdevumi, Latvija un Igaunija salīdzinājumam),
# 62b IRT modeļi (mirt), 00_ab_dati.R (PV, svari, grupas), ab_citas_jomas.rds.
#
# SADAĻAS
#   A. Ticamība: (1) PV ticamība pēc PISA TR formulas 1 − E[posterior dispersija] /
#      kopējā dispersija, 2003–2025, visa Latvija un abas valodas plūsmas;
#      (2) mūsu IRT modeļu empīriskā un marginālā ticamība; (3) Kronbaha α katrai
#      uzdevumu kombinācijai (testa formai / adaptīvā testa ceļam), jo skolēni
#      nesaņem vienu testu; (4) mērījuma precizitāte pa spēju līmeņiem.
#   B. Klasiskie uzdevumu rādītāji: izpilde, izlaisti, nesasniegti, korelācija ar
#      pārējo testu (formas ietvaros) un ar EAP.
#   C. IRT atbilstība: infit / outfit, lokālā atkarība (Q3), Latvijas uzdevumu
#      grūtība pret starptautiskajiem 2022. gada parametriem.
#   D. Dimensionalitāte: viena dimensija pret trim procesa dimensijām; fluence.
#   E. DIF (loģistiskā regresija, izlīdzināts pēc kopīgā EAP): latviešu pret krievu
#      versiju (Latvijā un, kā etalons, Igaunijā), meitenes pret zēniem, Latvija
#      pret Igauniju; ETS A/B/C klases, neto ietekme uz testu.
#   F. Atbilžu variantu (distraktoru) analīze; kļūdu koncentrācija 2022 → 2025.
#   G. Validitāte: saistība ar matemātiku, dabaszinātnēm, ESCS, dzimumu, klasi.
#
# Uzdevumu statistika bez svariem (mērinstrumenta īpašības); populācijas rādītāji
# (A1, A4, G) ar svariem, BRR (Fay 0,5) un Rubina kombinēšanu.
#
# Izvade:  output/pisa2025/psihometrija_*.csv
#          output/pisa2025/logs/62c_psihometrija_output.txt
# ============================================================================

# haven netiek pievienots ar library(): tad as.numeric uz marķētiem teksta ID
# ab_dati() iekšienē beidzas ar kļūdu (sk. 53b).
source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/62c_psihometrija_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

GADI <- c("2015", "2018", "2022", "2025")
DIR <- "data/pisa2025/processed/irt"
P <- stats::setNames(lapply(GADI, function(g) readRDS(sprintf("data/pisa2025/processed/psihometrija_%s.rds", g))), GADI)
M1 <- function(g, gr) { f <- file.path(DIR, sprintf("%s_M1_%s.rds", g, gr)); if (file.exists(f)) readRDS(f) else NULL }
DIM <- function(g, gr) { f <- file.path(DIR, sprintf("%s_DIM_%s.rds", g, gr)); if (file.exists(f)) readRDS(f) else NULL }
D <- ab_dati()
KJ <- readRDS("data/pisa2025/processed/ab_citas_jomas.rds")
K22 <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2022.csv", stringsAsFactors = FALSE)
hdr <- function(t) cat("\n", strrep("=", 100), "\n", t, "\n", strrep("=", 100), "\n\n", sep = "")
f2 <- function(x, d = 2) formatC(x, format = "f", digits = d)

versija <- function(sk) {
  l <- tolower(sk$LANGTEST_COG_LAB)
  ifelse(grepl("latvian|^lav", l), "LV", ifelse(grepl("russian|^rus", l), "RU", ifelse(grepl("estonian|^est", l), "EE", NA)))
}
for (g in GADI) {
  P[[g]]$sk$ver <- versija(P[[g]]$sk)
  s <- D[[g]]$sk; m <- match(P[[g]]$sk$CNTSTUID, s$id)
  P[[g]]$sk$grupa <- ifelse(P[[g]]$sk$CNT == "LVA", s$grupa[m], NA)
  P[[g]]$sk$dz <- ifelse(P[[g]]$sk$CNT == "LVA", s$dzimums[m], NA)   # 1 meitene, 2 zēns
  Y <- P[[g]]$K; Y[Y == -1L] <- 0L; Y[Y %in% c(-2L, -3L)] <- NA
  P[[g]]$Yp <- sweep(Y, 2, P[[g]]$mx, "/")
}
GRUPAS <- list("LVA" = function(sk) sk$CNT == "LVA", "LVA, latviešu versija" = function(sk) sk$CNT == "LVA" & sk$ver %in% "LV",
               "LVA, krievu versija" = function(sk) sk$CNT == "LVA" & sk$ver %in% "RU", "EST" = function(sk) sk$CNT == "EST",
               "EST, igauņu versija" = function(sk) sk$CNT == "EST" & sk$ver %in% "EE", "EST, krievu versija" = function(sk) sk$CNT == "EST" & sk$ver %in% "RU")
eap_vec <- function(g, gr) { m <- M1(g, gr); th <- rep(NA_real_, nrow(P[[g]]$K)); if (!is.null(m)) th[m$rindas] <- m$eap; th }

# ===========================================================================
hdr("A1. PV ticamība (PISA TR formula), lasīšana, Latvija")
# ===========================================================================
pv_rel <- function(pv, w) {
  PM <- do.call(cbind, pv); ok <- stats::complete.cases(PM) & !is.na(w); PM <- PM[ok, ]; wv <- w[ok] / sum(w[ok])
  err <- sum(wv * apply(PM, 1, stats::var))
  tot <- mean(apply(PM, 2, function(x) { m <- sum(wv * x); sum(wv * (x - m)^2) }))
  c(ticamiba = 1 - err / tot, sd_kopa = sqrt(tot), sd_kluda = sqrt(err))
}
A1 <- do.call(rbind, lapply(names(D), function(g) do.call(rbind, lapply(c("visi", "A", "B"), function(gr) {
  d <- if (gr == "visi") D[[g]] else ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% gr)
  data.frame(gads = g, grupa = gr, n = nrow(d$sk), t(pv_rel(d$pv, d$sk$w)))
}))))
print(format(A1, digits = 3), row.names = FALSE)
cat("\n  PISA 2022 TR, 14.A.11 tabula (lasīšana): LVA 0,85; EST 0,84; LTU 0,85; FIN 0,86; POL 0,87; CZE 0,87; HUN 0,88; DEU 0,88.\n",
    " Plūsmas iekšienē ticamība ir zemāka, jo mazāka ir kopējā izkliede (tā pati kļūda, mazāka signāla dispersija).\n")
utils::write.csv(A1, "output/pisa2025/psihometrija_pv_ticamiba.csv", row.names = FALSE)

# ===========================================================================
hdr("A2. IRT ticamība (mūsu 62b modeļi, EAP)")
# ===========================================================================
A2 <- do.call(rbind, lapply(GADI, function(g) do.call(rbind, lapply(c("LVA", "EST"), function(gr) {
  m <- M1(g, gr); if (is.null(m)) return(NULL)
  data.frame(gads = g, valsts = gr, uzdevumi = nrow(m$par), skoleni = length(m$eap),
             empiriska = unname(m$rxx_emp), marginala = unname(m$rxx_marg), vid_eap_se = mean(m$eap_se))
}))))
print(format(A2, digits = 3), row.names = FALSE)

# ===========================================================================
hdr("A3. Kronbaha α katrai uzdevumu kombinācijai (testa forma / adaptīvā testa ceļš)")
# ===========================================================================
alfa <- function(S) { k <- ncol(S); v <- apply(S, 2, stats::var); if (any(v == 0)) { S <- S[, v > 0, drop = FALSE]; k <- ncol(S); v <- v[v > 0] }
  if (k < 3) return(NA_real_); k / (k - 1) * (1 - sum(v) / stats::var(rowSums(S))) }
# Adaptīvajā testā (2018+) gandrīz katram skolēnam ir savs uzdevumu komplekts, tāpēc
# α rēķina testletēm: uzdevumu grupām, kas gandrīz vienmēr uzdotas kopā (Žakāra
# līdzība ≥ 0,95; 2015. gadā tās ir klasteri). Visa skolēna ceļa ticamība =
# stratificētā α: 1 − Σ var_t (1 − α_t) / var(Σ X_t), ar testlešu kovariācijām no
# skolēniem, kas saņēma abas testletes.
testletes <- function(K, rows, min_n = 50) {
  adm <- !is.na(K[rows, , drop = FALSE]); J <- which(colSums(adm) >= min_n)
  A <- adm[, J, drop = FALSE] * 1; C <- crossprod(A); n <- diag(C)
  jac <- C / (outer(n, n, "+") - C)
  cl <- stats::cutree(stats::hclust(stats::as.dist(1 - jac), method = "complete"), h = 0.05)
  split(J, cl)
}
alfa_testletes <- function(K, rows) {
  tl <- testletes(K, rows); tl <- tl[lengths(tl) >= 3]
  ir_sum <- ir_n <- stats::setNames(numeric(ncol(K)), colnames(K))
  X <- matrix(NA_real_, length(rows), length(tl)); info <- list()
  for (t in seq_along(tl)) {
    j <- tl[[t]]; S <- K[rows, j, drop = FALSE]
    tak <- rowSums(!is.na(S)) == length(j) & rowSums(S <= -2L, na.rm = TRUE) == 0
    if (sum(tak) < 30) next
    Sc <- S[tak, , drop = FALSE]; Sc[Sc == -1L] <- 0L; tot <- rowSums(Sc)
    X[tak, t] <- tot
    info[[length(info) + 1]] <- data.frame(testlete = t, k = length(j), n = sum(tak), alfa = alfa(Sc), var = stats::var(tot))
    for (q in seq_along(j)) { x <- Sc[, q]; if (stats::var(x) == 0) next
      r <- suppressWarnings(stats::cor(x, tot - x)); if (is.na(r)) next
      ir_sum[j[q]] <- ir_sum[j[q]] + sum(tak) * r; ir_n[j[q]] <- ir_n[j[q]] + sum(tak) }
  }
  info <- do.call(rbind, info); if (is.null(info)) return(NULL)
  Xs <- X[, info$testlete, drop = FALSE]; ok <- !is.na(Xs)
  V <- suppressWarnings(stats::cov(Xs, use = "pairwise.complete.obs")); NN <- crossprod(ok * 1)
  strat <- apply(ok, 1, function(h) { t <- which(h); if (length(t) < 2 || any(NN[t, t] < 30) || any(is.na(V[t, t]))) return(NA_real_)
    1 - sum(info$var[t] * (1 - info$alfa[t])) / sum(V[t, t]) })
  list(info = info, strat = strat, k_cels = as.vector(ok %*% info$k), itemrest = ifelse(ir_n > 0, ir_sum / ir_n, NA))
}
A3 <- list(); IR <- list()
for (g in GADI) for (gr in names(GRUPAS)) {
  rows <- which(GRUPAS[[gr]](P[[g]]$sk)); if (length(rows) < 200) next
  at <- alfa_testletes(P[[g]]$K, rows); if (is.null(at)) next
  i <- at$info; s <- !is.na(at$strat)
  a30 <- at$strat[s] * 30 / at$k_cels[s] / (1 + (30 / at$k_cels[s] - 1) * at$strat[s])
  A3[[length(A3) + 1]] <- data.frame(gads = g, grupa = gr, testletes = nrow(i), testl_uzd_med = stats::median(i$k),
    testl_alfa_med = stats::weighted.mean(i$alfa, i$n, na.rm = TRUE), testl_alfa_min = min(i$alfa, na.rm = TRUE),
    testl_alfa_max = max(i$alfa, na.rm = TRUE), skoleni_cels = sum(s), cels_uzd_vid = mean(at$k_cels[s]),
    alfa_cels_vid = mean(at$strat[s]), alfa_cels_p10 = stats::quantile(at$strat[s], .1), alfa_30 = mean(a30))
  IR[[paste(g, gr)]] <- at$itemrest
}
A3 <- do.call(rbind, A3)
print(format(A3, digits = 3), row.names = FALSE)
cat("\n  testl_alfa_med: testlešu α (vidēji, svērts ar skolēnu skaitu); alfa_cels: stratificētā α visam skolēna\n",
    " uzdevumu komplektam; alfa_30: Spīrmena-Brauna pārrēķins uz 30 uzdevumiem (cikli ar dažādu testa garumu).\n")
utils::write.csv(A3, "output/pisa2025/psihometrija_alfa.csv", row.names = FALSE)

# ===========================================================================
hdr("A4. Mērījuma precizitāte pa spēju līmeņiem (PV izkliede skolēna iekšienē), latviešu valodas plūsma")
# ===========================================================================
ROB <- c(-Inf, 335, 407, 480, 553, 626, Inf)
LIM <- c("zem 1a", "1a", "2", "3", "4", "5+")
A4 <- do.call(rbind, lapply(c("2015", "2018", "2022", "2025"), function(g) {
  d <- ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A"); PM <- do.call(cbind, d$pv); w <- d$sk$w
  sdw <- apply(PM, 1, stats::sd); lim <- cut(rowMeans(PM), ROB, labels = LIM)
  data.frame(gads = g, limenis = LIM, dala = as.numeric(tapply(w, lim, sum)) / sum(w) * 100,
             pv_sd = as.numeric(tapply(sdw * w, lim, sum) / tapply(w, lim, sum)))
}))
print(stats::reshape(A4, idvar = "limenis", timevar = "gads", direction = "wide"), row.names = FALSE, digits = 3)
cat("\n  pv_sd: vidējā PV standartnovirze skolēna iekšienē (mērījuma kļūda PISA punktos); dala: % skolēnu līmenī.\n")
utils::write.csv(A4, "output/pisa2025/psihometrija_precizitate.csv", row.names = FALSE)

# ===========================================================================
hdr("B. Klasiskie uzdevumu rādītāji")
# ===========================================================================
B <- list()
for (g in GADI) for (gr in names(GRUPAS)) {
  sk <- P[[g]]$sk; rows <- which(GRUPAS[[gr]](sk)); if (length(rows) < 200) next
  K <- P[[g]]$K[rows, , drop = FALSE]; Yp <- P[[g]]$Yp[rows, , drop = FALSE]
  cc <- if (grepl("^LVA", gr)) "LVA" else "EST"; th <- eap_vec(g, cc)[rows]
  adm <- colSums(!is.na(K)); keep <- adm >= 100
  ir <- IR[[paste(g, gr)]]; if (is.null(ir)) ir <- stats::setNames(rep(NA_real_, ncol(K)), colnames(K))
  r_eap <- vapply(which(keep), function(j) { ok <- !is.na(Yp[, j]) & !is.na(th); if (sum(ok) < 50 || stats::var(Yp[ok, j]) == 0) NA_real_ else stats::cor(Yp[ok, j], th[ok]) }, 1)
  B[[length(B) + 1]] <- data.frame(gads = g, grupa = gr, uzd = colnames(K)[keep], n = adm[keep],
    izpilde = colMeans(Yp[, keep, drop = FALSE], na.rm = TRUE),
    izlaisti = colSums(K[, keep, drop = FALSE] == -1L, na.rm = TRUE) / adm[keep],
    nesasniegti = colSums(K[, keep, drop = FALSE] == -2L, na.rm = TRUE) / adm[keep],
    r_parejais = ir[keep], r_eap = r_eap, fluence = grepl("^590", colnames(K)[keep]), stringsAsFactors = FALSE)
}
B <- do.call(rbind, B)
Bs <- do.call(rbind, lapply(split(B, list(B$gads, B$grupa), drop = TRUE), function(x) data.frame(gads = x$gads[1], grupa = x$grupa[1],
  uzdevumi = nrow(x), izpilde_vid = mean(x$izpilde), r_parejais_med = stats::median(x$r_parejais, na.rm = TRUE),
  r_zem_020 = sum(x$r_parejais < 0.20, na.rm = TRUE), r_negativs = sum(x$r_parejais < 0, na.rm = TRUE),
  r_eap_med = stats::median(x$r_eap, na.rm = TRUE), izlaisti_vid = 100 * mean(x$izlaisti), nesasniegti_vid = 100 * mean(x$nesasniegti))))
Bs <- Bs[order(Bs$gads, Bs$grupa), ]
print(format(Bs, digits = 3), row.names = FALSE)
cat("\n  Uzdevumi ar vāju sakarību ar pārējo testu Latvijā (r < 0,15), bet ne Igaunijā (r ≥ 0,25):\n")
for (g in GADI) {
  l <- B[B$gads == g & B$grupa == "LVA", ]; e <- B[B$gads == g & B$grupa == "EST", ]
  m <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee"))
  fl <- m[!is.na(m$r_parejais_lv) & m$r_parejais_lv < 0.15 & m$r_parejais_ee >= 0.25, ]
  cat(sprintf("    %s: %d no %d kopīgajiem%s\n", g, nrow(fl), nrow(m),
              if (nrow(fl)) paste0(": ", paste(sprintf("%s (%.2f / %.2f)", fl$uzd, fl$r_parejais_lv, fl$r_parejais_ee), collapse = ", ")) else ""))
  kor <- function(a, b) { ok <- !is.na(a) & !is.na(b); if (sum(ok) < 5) NA_real_ else stats::cor(a[ok], b[ok]) }
  cat(sprintf("       izpildes korelācija LVA–EST pa uzdevumiem r = %.3f; r_parejais r = %.3f; r_eap r = %.3f\n",
              kor(m$izpilde_lv, m$izpilde_ee), kor(m$r_parejais_lv, m$r_parejais_ee), kor(m$r_eap_lv, m$r_eap_ee)))
}
utils::write.csv(B, "output/pisa2025/psihometrija_uzdevumi.csv", row.names = FALSE)

# ===========================================================================
hdr("C. IRT atbilstība, lokālā atkarība un grūtība pret starptautiskajiem parametriem")
# ===========================================================================
C <- list(); Cq <- list(); Ci <- list()
for (g in GADI) for (gr in c("LVA", "EST")) {
  m <- M1(g, gr); if (is.null(m)) next
  p <- m$par; p$gads <- g; p$valsts <- gr; C[[length(C) + 1]] <- p
  q <- m$q3; q3v <- mean(q$q3, na.rm = TRUE)
  Cq[[length(Cq) + 1]] <- data.frame(gads = g, valsts = gr, pari = nrow(q), q3_vid = q3v,
    q3_viena_vieniba = mean(q$q3[q$viena_vieniba]) - q3v, q3_citas = mean(q$q3[!q$viena_vieniba]) - q3v,
    viena_virs_020 = mean(q$q3[q$viena_vieniba] - q3v > 0.20), citas_virs_020 = mean(q$q3[!q$viena_vieniba] - q3v > 0.20))
  it <- K22[match(paste0("CR", p$uzd), K22$uzd), c("slipums", "grutiba")]
  ok <- !is.na(it$grutiba) & !is.na(p$b) & abs(p$b) < 8
  if (sum(ok) >= 20) {
    fit <- stats::lm(p$b[ok] ~ it$grutiba[ok]); z <- stats::rstandard(fit)
    Ci[[length(Ci) + 1]] <- data.frame(gads = g, valsts = gr, uzdevumi = sum(ok), r_grutiba = stats::cor(p$b[ok], it$grutiba[ok]),
      r_slipums = stats::cor(p$a[ok], it$slipums[ok]), novirze_25 = sum(abs(z) > 2.5),
      novirzes = paste(p$uzd[ok][abs(z) > 2.5], collapse = " "))
  }
}
C <- do.call(rbind, C); Cq <- do.call(rbind, Cq); Ci <- do.call(rbind, Ci)
Cs <- do.call(rbind, lapply(split(C, list(C$gads, C$valsts), drop = TRUE), function(x) data.frame(gads = x$gads[1], valsts = x$valsts[1],
  uzdevumi = nrow(x), a_med = stats::median(x$a), infit_med = stats::median(x$infit), outfit_med = stats::median(x$outfit),
  infit_ara_08_12 = sum(x$infit < 0.8 | x$infit > 1.2), outfit_ara_07_13 = sum(x$outfit < 0.7 | x$outfit > 1.3))))
cat("  Atbilstība (infit / outfit vidējie kvadrāti; ideāli ~1):\n"); print(format(Cs, digits = 3), row.names = FALSE)
cat("\n  Lokālā atkarība: Q3 atlikumu korelācija, centrēta pret vidējo Q3 (Q3*); > 0,20 nozīmē atkarību:\n"); print(format(Cq, digits = 3), row.names = FALSE)
cat("\n  Grūtība un slīpums pret PISA 2022 starptautiskajiem parametriem (A pielikums), novirze = |standartizēts atlikums| > 2,5:\n")
print(format(Ci[, c("gads", "valsts", "uzdevumi", "r_grutiba", "r_slipums", "novirze_25")], digits = 3), row.names = FALSE)
for (i in seq_len(nrow(Ci))) if (Ci$novirze_25[i] > 0) cat(sprintf("    %s %s: %s\n", Ci$gads[i], Ci$valsts[i], Ci$novirzes[i]))
cat("\n  Sliktākā atbilstība Latvijā 2025 (outfit), blakus Igaunijai:\n")
l <- C[C$gads == "2025" & C$valsts == "LVA", ]; e <- C[C$gads == "2025" & C$valsts == "EST", ]
mm <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee")); mm <- mm[order(-mm$outfit_lv), ]
print(format(head(mm[, c("uzd", "a_lv", "a_ee", "infit_lv", "infit_ee", "outfit_lv", "outfit_ee")], 10), digits = 3), row.names = FALSE)
utils::write.csv(C, "output/pisa2025/psihometrija_irt_parametri.csv", row.names = FALSE)
utils::write.csv(rbind(transform(Cq, tips = "Q3"))[, c("tips", setdiff(names(Cq), "tips"))], "output/pisa2025/psihometrija_q3.csv", row.names = FALSE)
utils::write.csv(Ci, "output/pisa2025/psihometrija_starptautiskie.csv", row.names = FALSE)

# ===========================================================================
hdr("D. Dimensionalitāte")
# ===========================================================================
Dm <- list()
for (g in GADI) for (gr in c("LVA", "EST")) {
  x <- DIM(g, gr); if (is.null(x)) next
  if (!is.null(x$proc)) { R <- x$proc_kor; r <- R[upper.tri(R)]
    Dm[[length(Dm) + 1]] <- data.frame(gads = g, valsts = gr, salidzinajums = "1D pret 3 procesiem",
      dBIC = x$proc$BIC[2] - x$proc$BIC[1], dAIC = x$proc$AIC[2] - x$proc$AIC[1], kor_min = min(r), kor_vid = mean(r),
      korelacijas = paste(sprintf("%s-%s %.2f", rownames(R)[row(R)[upper.tri(R)]], colnames(R)[col(R)[upper.tri(R)]], r), collapse = "; ")) }
  if (!is.null(x$flu)) { R <- x$flu_kor; r <- R[upper.tri(R)]
    Dm[[length(Dm) + 1]] <- data.frame(gads = g, valsts = gr, salidzinajums = "1D pret fluence/pārējie",
      dBIC = x$flu$BIC[2] - x$flu$BIC[1], dAIC = x$flu$AIC[2] - x$flu$AIC[1], kor_min = min(r), kor_vid = mean(r),
      korelacijas = sprintf("fluence-pārējie %.2f", r)) }
}
Dm <- do.call(rbind, Dm)
if (!is.null(Dm)) { print(format(Dm[, c("gads", "valsts", "salidzinajums", "kor_min", "kor_vid", "korelacijas")], digits = 3), row.names = FALSE)
  utils::write.csv(Dm, "output/pisa2025/psihometrija_dimensijas.csv", row.names = FALSE) }
cat("\n  Latentās korelācijas ≥ 0,90: praktiski viena dimensija. Daudzdimensiju modeļi novērtēti ar MHRM, kura\n",
    " logLik ir Montekarlo tuvinājums (sistemātiski zemāks), tāpēc BIC salīdzinājums ar precīzo 1D EM nav uzticams\n",
    " un netiek interpretēts (dBIC saglabāts CSV tikai pilnīguma dēļ).\n")

# ===========================================================================
hdr("E. DIF: loģistiskā regresija, izlīdzināts pēc kopīgā (LVA + EST) EAP")
# ===========================================================================
dif_lr <- function(Yp, th, grp, ref, foc, min_n = 100) {
  out <- lapply(seq_len(ncol(Yp)), function(j) {
    y <- Yp[, j]; ok <- !is.na(y) & !is.na(th) & grp %in% c(ref, foc)
    nr <- sum(ok & grp == ref); nf <- sum(ok & grp == foc)
    if (nr < min_n || nf < min_n || length(unique(y[ok])) < 2) return(NULL)
    gg <- as.numeric(grp[ok] == foc); t <- th[ok]; yy <- y[ok]
    m1 <- suppressWarnings(stats::glm(yy ~ t + gg, family = stats::quasibinomial()))
    m2 <- suppressWarnings(stats::glm(yy ~ t * gg, family = stats::quasibinomial()))
    cf <- summary(m1)$coefficients; co <- stats::coef(m1); b <- co[["gg"]]
    tf <- t[gg == 1]; p0 <- stats::plogis(co[1] + co[2] * tf)
    data.frame(uzd = colnames(Yp)[j], n_ref = nr, n_foc = nf, beta = b, delta = 2.35 * b, p = cf["gg", 4],
               p_nevien = summary(m2)$coefficients["t:gg", 4],
               dp = mean(stats::plogis(co[1] + co[2] * tf + b) - p0), slipums = mean(co[2] * p0 * (1 - p0)))
  })
  r <- do.call(rbind, out); if (is.null(r)) return(NULL)
  r$klase <- ifelse(abs(r$delta) < 1, "A", ifelse(abs(r$delta) < 1.5, "B", "C"))
  r$klase[r$p > 0.05 & r$klase != "A"] <- "A"
  r
}
SAL <- list(
  list(nos = "valoda LVA: krievu pret latviešu versiju", f = function(sk) ifelse(sk$CNT == "LVA", sk$ver, NA), ref = "LV", foc = "RU"),
  list(nos = "valoda EST: krievu pret igauņu versiju", f = function(sk) ifelse(sk$CNT == "EST", sk$ver, NA), ref = "EE", foc = "RU"),
  list(nos = "valsts: LVA latviešu pret EST igauņu versiju", f = function(sk) ifelse(sk$CNT == "LVA" & sk$ver %in% "LV", "LVA",
                                                               ifelse(sk$CNT == "EST" & sk$ver %in% "EE", "EST", NA)), ref = "EST", foc = "LVA"),
  list(nos = "valsts: LVA krievu pret EST krievu versiju", f = function(sk) ifelse(sk$ver %in% "RU", sk$CNT, NA), ref = "EST", foc = "LVA"),
  list(nos = "dzimums LVA latviešu plūsma: meitenes pret zēniem", f = function(sk) ifelse(sk$CNT == "LVA" & sk$grupa %in% "A",
                                                               ifelse(sk$dz %in% 1, "M", ifelse(sk$dz %in% 2, "Z", NA)), NA), ref = "Z", foc = "M"))
E <- list(); Es <- list()
for (g in GADI) {
  th <- eap_vec(g, "KOP")
  for (s in SAL) {
    grp <- s$f(P[[g]]$sk)
    r <- dif_lr(P[[g]]$Yp, th, grp, s$ref, s$foc); if (is.null(r) || nrow(r) < 10) next
    r$gads <- g; r$salidzinajums <- s$nos; E[[length(E) + 1]] <- r
    tt <- stats::t.test(r$delta)
    net <- stats::weighted.mean(r$dp, r$n_foc); sl <- stats::weighted.mean(r$slipums, r$n_foc)
    Es[[length(Es) + 1]] <- data.frame(gads = g, salidzinajums = s$nos, uzdevumi = nrow(r),
      B_pct = 100 * mean(r$klase == "B"), C_pct = 100 * mean(r$klase == "C"), vid_delta = mean(r$delta),
      vid_delta_p = tt$p.value, pret_fokusu = sum(r$klase != "A" & r$delta < 0), par_labu_fokusam = sum(r$klase != "A" & r$delta > 0),
      nevienmerigs_p001 = sum(r$p_nevien < 0.001), neto_pp = 100 * net, neto_theta = net / sl, neto_punkti_apt = 100 * net / sl)
  }
}
E <- do.call(rbind, E); Es <- do.call(rbind, Es)
print(format(Es, digits = 3), row.names = FALSE)
cat("\n  delta > 0: uzdevums vieglāks fokusa grupai (krievu versija; Latvija; meitenes) pie vienādas spējas.\n",
    " ETS: A |delta| < 1; B 1–1,5; C ≥ 1,5 (B un C tikai, ja p < 0,05). neto_pp: vidējā izpildes starpība fokusa grupai\n",
    " visos uzdevumos DIF dēļ; neto_punkti_apt: aptuveni PISA punktos (θ kopīgajā skalā × 100). Izlīdzināšanas mainīgais\n",
    " satur arī pašus uzdevumus; vienmērīgu visas versijas atšķirību šī metode neredz (docs/26).\n")
cat("\n  Valodas DIF stabilitāte starp cikliem (tie paši uzdevumi; delta korelācija):\n")
for (nos in c("valoda LVA: krievu pret latviešu versiju", "valoda EST: krievu pret igauņu versiju")) {
  x <- E[E$salidzinajums == nos, ]
  for (pr in list(c("2015", "2018"), c("2018", "2022"), c("2015", "2022"))) {
    a <- x[x$gads == pr[1], ]; b <- x[x$gads == pr[2], ]; m <- merge(a, b, by = "uzd")
    if (nrow(m) >= 10) cat(sprintf("    %-44s %s–%s: %3d uzdevumi, r = %.2f; C abos ciklos: %s\n", nos, pr[1], pr[2], nrow(m),
        stats::cor(m$delta.x, m$delta.y), paste(m$uzd[m$klase.x == "C" & m$klase.y == "C"], collapse = " ")))
  }
}
utils::write.csv(E, "output/pisa2025/psihometrija_dif.csv", row.names = FALSE)
utils::write.csv(Es, "output/pisa2025/psihometrija_dif_kopsavilkums.csv", row.names = FALSE)

# ===========================================================================
hdr("F. Atbilžu variantu (distraktoru) analīze")
# ===========================================================================
Fd <- list()
for (g in GADI) {
  Pg <- P[[g]]; R <- Pg$R; if (is.null(R)) next
  th <- eap_vec(g, "KOP")
  grp <- ifelse(Pg$sk$CNT == "LVA" & Pg$sk$ver %in% "LV", "LVA-LV", ifelse(Pg$sk$CNT == "LVA" & Pg$sk$ver %in% "RU", "LVA-RU",
         ifelse(Pg$sk$CNT == "EST" & Pg$sk$ver %in% "EE", "EST-EE", ifelse(Pg$sk$CNT == "EST" & Pg$sk$ver %in% "RU", "EST-RU", NA))))
  for (j in intersect(colnames(R), colnames(Pg$K))) {
    r <- R[, j]; kk <- Pg$K[, j]; mxj <- Pg$mx[[j]]
    opt <- sort(unique(r[r %in% 1:6])); if (length(opt) < 3 || mxj != 1) next
    key <- opt[which.max(vapply(opt, function(o) mean(kk[r %in% o] == 1L, na.rm = TRUE), 1))]
    if (mean(kk[r %in% key] == 1L, na.rm = TRUE) < 0.95) next           # nav vienas atbildes atslēgas
    for (gg in c("LVA-LV", "LVA-RU", "EST-EE", "EST-RU")) {
      i <- which(grp %in% gg & r %in% c(opt, 9L)); if (length(i) < 100) next
      ri <- r[i]; ti <- th[i]; resp <- ri %in% opt
      for (o in opt) {
        ind <- as.numeric(ri[resp] == o)
        Fd[[length(Fd) + 1]] <- data.frame(gads = g, uzd = j, grupa = gg, variants = LETTERS[o], atslega = o == key,
          n = sum(resp), dala = mean(ind), vid_theta = mean(ti[resp][ind == 1], na.rm = TRUE),
          pb = { okt <- !is.na(ti[resp]); if (sum(okt) >= 30 && stats::var(ind[okt]) > 0) stats::cor(ind[okt], ti[resp][okt]) else NA_real_ },
          izlaisti = mean(ri == 9L))
      }
    }
  }
}
Fd <- do.call(rbind, Fd)
Fs <- do.call(rbind, lapply(split(Fd, list(Fd$gads, Fd$grupa, Fd$uzd), drop = TRUE), function(x) {
  k <- x[x$atslega, ]; ds <- x[!x$atslega, ]
  data.frame(gads = x$gads[1], grupa = x$grupa[1], uzd = x$uzd[1], n = k$n, pareizi = k$dala, atslegas_pb = k$pb,
    distr_pb_max = max(ds$pb, na.rm = TRUE), distr_theta_virs_atslegas = any(ds$vid_theta > k$vid_theta & ds$dala >= 0.05, na.rm = TRUE),
    kludu_koncentracija = if (sum(ds$dala) > 0) max(ds$dala) / sum(ds$dala) else NA_real_, distraktori = nrow(ds),
    izlaisti = k$izlaisti)
}))
cat("  Karodziņi pa grupām: atslēgas pb < 0,10; kāds distraktors ar pozitīvu pb; distraktora izvēlētāji spēcīgāki par pareizo:\n")
print(do.call(rbind, lapply(split(Fs, list(Fs$gads, Fs$grupa), drop = TRUE), function(x) data.frame(gads = x$gads[1], grupa = x$grupa[1],
  uzdevumi = nrow(x), atslegas_pb_med = stats::median(x$atslegas_pb, na.rm = TRUE), atslega_vaja = sum(x$atslegas_pb < 0.10, na.rm = TRUE),
  distr_pozitivs = sum(x$distr_pb_max > 0, na.rm = TRUE), distr_speciigaki = sum(x$distr_theta_virs_atslegas),
  kludu_konc = stats::weighted.mean(x$kludu_koncentracija, x$n, na.rm = TRUE)))), row.names = FALSE, digits = 3)
cat("\n  Karodziņoti uzdevumi Latvijā (latviešu versija), kas nav karodziņoti Igaunijā (igauņu versija):\n")
for (g in GADI) {
  l <- Fs[Fs$gads == g & Fs$grupa == "LVA-LV", ]; e <- Fs[Fs$gads == g & Fs$grupa == "EST-EE", ]
  m <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee"))
  fl <- m[which((m$distr_pb_max_lv > 0 | m$distr_theta_virs_atslegas_lv | m$atslegas_pb_lv < 0.10) &
          !(m$distr_pb_max_ee > 0 | m$distr_theta_virs_atslegas_ee | m$atslegas_pb_ee < 0.10)), ]
  cat(sprintf("    %s: %d no %d%s\n", g, nrow(fl), nrow(m), if (nrow(fl)) paste0(": ", paste(sprintf("%s (atslēgas pb %.2f/%.2f, max distr. pb %.2f/%.2f)",
      fl$uzd, fl$atslegas_pb_lv, fl$atslegas_pb_ee, fl$distr_pb_max_lv, fl$distr_pb_max_ee), collapse = "; ")) else ""))
}
cat("\n  Kļūdu raksturs 2022 → 2025 kopīgajos atbilžu izvēles uzdevumos: nepareizo daļa un to koncentrācija uz\n",
    " populārāko distraktoru (1/(k−1) = vienmērīga minēšana; augstāka = sistemātisks maldīgs variants):\n")
kop <- intersect(Fs$uzd[Fs$gads == "2022"], Fs$uzd[Fs$gads == "2025"])
for (gg in c("LVA-LV", "EST-EE")) for (g in c("2022", "2025")) {
  x <- Fs[Fs$gads == g & Fs$grupa == gg & Fs$uzd %in% kop, ]
  cat(sprintf("    %s %s: %d uzdevumi | nepareizi %.1f %% | koncentrācija %.3f (vienmērīgi %.3f) | izlaisti %.1f %%\n", gg, g, nrow(x),
      100 * stats::weighted.mean(1 - x$pareizi, x$n), stats::weighted.mean(x$kludu_koncentracija, x$n, na.rm = TRUE),
      stats::weighted.mean(1 / x$distraktori, x$n), 100 * stats::weighted.mean(x$izlaisti, x$n)))
}
utils::write.csv(Fd, "output/pisa2025/psihometrija_distraktori.csv", row.names = FALSE)
utils::write.csv(Fs, "output/pisa2025/psihometrija_distraktori_uzd.csv", row.names = FALSE)

# ===========================================================================
hdr("G. Validitāte: saistība ar citām jomām un fona rādītājiem, Latvija")
# ===========================================================================
cor_w <- function(x, y, w) { ok <- !is.na(x) & !is.na(y) & !is.na(w); x <- x[ok]; y <- y[ok]; w <- w[ok] / sum(w[ok])
  mx <- sum(w * x); my <- sum(w * y); sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2)) }
rubin2 <- function(pvx, pvy, w, rw, stat) {
  M <- length(pvx); e <- vapply(seq_len(M), function(m) stat(pvx[[m]], pvy[[m]], w), 1)
  U <- mean(vapply(seq_len(M), function(m) sum((vapply(rw, function(r) stat(pvx[[m]], pvy[[m]], r), 1) - e[m])^2) / 20, 1))
  c(est = mean(e), se = sqrt(U + (1 + 1 / M) * stats::var(e)))
}
citas <- function(g, joma) {
  if (g %in% VECIE) { d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
    return(lapply(paste0("PV", 1:5, joma), function(k) if (k %in% names(d)) as.numeric(d[[k]]) else rep(NA_real_, nrow(d)))) }
  x <- KJ[[g]]; r <- match(as.numeric(D[[g]]$sk$id), x$CNTSTUID); lapply(paste0("PV", 1:10, joma), function(k) x[[k]][r])
}
G <- list()
for (g in names(D)) for (gr in c("visi", "A", "B")) {
  i <- if (gr == "visi") rep(TRUE, nrow(D[[g]]$sk)) else D[[g]]$sk$grupa %in% gr
  d <- ab_apakskopa(D[[g]], i); sk <- d$sk
  mat <- lapply(citas(g, "MATH"), `[`, i); sci <- lapply(citas(g, "SCIE"), `[`, i)
  one <- function(v) rep(list(v), length(d$pv))
  st <- list(
    r_mat = if (!all(is.na(mat[[1]]))) rubin2(d$pv, mat, sk$w, d$rw, cor_w) else c(est = NA, se = NA),
    r_dab = if (!all(is.na(sci[[1]]))) rubin2(d$pv, sci, sk$w, d$rw, cor_w) else c(est = NA, se = NA),
    r_escs = rubin2(d$pv, one(sk$escs), sk$w, d$rw, cor_w),
    meit_zeni = rubin2(d$pv, one(sk$dzimums), sk$w, d$rw, function(x, dz, w) VID(x[dz %in% 1], w[dz %in% 1]) - VID(x[dz %in% 2], w[dz %in% 2])),
    kl9_kl8 = rubin2(d$pv, one(sk$klase), sk$w, d$rw, function(x, k, w) VID(x[k %in% 9], w[k %in% 9]) - VID(x[k %in% 8], w[k %in% 8])))
  G[[length(G) + 1]] <- data.frame(gads = g, grupa = gr, radītājs = names(st), est = vapply(st, `[[`, 1, "est"), se = vapply(st, `[[`, 1, "se"))
}
G <- do.call(rbind, G)
Gw <- stats::reshape(G[, c("gads", "grupa", "radītājs", "est")], idvar = c("grupa", "radītājs"), timevar = "gads", direction = "wide")
print(format(Gw[order(Gw$radītājs, Gw$grupa), ], digits = 3), row.names = FALSE)
cat("\n  PISA 2022 TR 14.A.13 (visa valsts): lasīšana–matemātika LVA 0,79, EST 0,77, LTU 0,81, FIN 0,79, POL 0,81, CZE 0,81,\n",
    " HUN 0,84, DEU 0,85; lasīšana–dabaszinātnes LVA 0,78, EST 0,74, LTU 0,81, FIN 0,77, POL 0,80, CZE 0,80, HUN 0,84, DEU 0,86.\n",
    " kl9_kl8: 9. klases skolēnu priekšrocība pret 8. klases skolēniem (viens mācību gads; nav tīrs efekts).\n")
utils::write.csv(G, "output/pisa2025/psihometrija_validitate.csv", row.names = FALSE)

# G2. Vai 2025. gadā mainījās lasīšanas saistība ar SES, vai pats SES mērījums?
# Ja kritums ir gan lasīšanai, gan matemātikai un koncentrējas vienā komponentē,
# tas ir fona mērījuma, ne lasīšanas testa jautājums.
cat("\n  G2. Lasīšanas un matemātikas korelācija ar ESCS un tā komponentēm (PV vidējie, svērts):\n")
PAP <- readRDS("data/pisa2025/processed/ab_papildu_2015_2022.rds")
p25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds"); names(p25) <- toupper(names(p25))
G2 <- list()
for (g in JAUNIE) {
  s <- D[[g]]$sk
  src <- if (g == "2025") data.frame(CNTSTUID = as.numeric(p25$CNTSTUID), HISEI = as.numeric(p25$HISEI),
                                      PAREDINT = as.numeric(p25$PAREDINT), HOMEPOS = as.numeric(p25$HOMEPOS)) else PAP[PAP$gads == g, ]
  m <- match(s$id, src$CNTSTUID); rd <- rowMeans(do.call(cbind, D[[g]]$pv))
  x <- KJ[[g]][match(s$id, KJ[[g]]$CNTSTUID), ]; mt <- rowMeans(x[, paste0("PV", 1:10, "MATH")])
  kom <- list(ESCS = s$escs, HISEI = as.numeric(src$HISEI[m]), PAREDINT = as.numeric(src$PAREDINT[m]), HOMEPOS = as.numeric(src$HOMEPOS[m]))
  for (gr in c("A", "B")) { i <- s$grupa %in% gr
    for (k in names(kom)) G2[[length(G2) + 1]] <- data.frame(gads = g, grupa = gr, komponente = k,
      r_lasisana = cor_w(rd[i], kom[[k]][i], s$w[i]), r_matematika = cor_w(mt[i], kom[[k]][i], s$w[i]),
      trukst_pct = 100 * mean(is.na(kom[[k]][i]))) }
}
G2 <- do.call(rbind, G2); G2 <- G2[!is.na(G2$r_lasisana), ]
print(format(G2, digits = 3), row.names = FALSE)
utils::write.csv(G2, "output/pisa2025/psihometrija_ses_komponentes.csv", row.names = FALSE)

cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
