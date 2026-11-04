# ============================================================================
# 54. Kuros lasīšanas uzdevumu veidos A plūsma krita, 2022 -> 2025
# ============================================================================
#
# JAUTĀJUMS. A plūsmas pārmaiņa oficiālajā skalā ir -19,4. Vai kritums ir
# vienmērīgs, vai koncentrējas konkrētos uzdevumu veidos: kognitīvais process
# (atrast informāciju / saprast / izvērtēt un reflektēt), teksta avots (viens /
# vairāki), formāts, raitums, grūtība, MSAT posms?
#
# KLASIFIKĀCIJA: PISA 2022 tehniskā ziņojuma A pielikums (54a skripts). Uzdevumu
# saturs ir slepens.
#
# RĀDĪTĀJI katram veidam
#   R1 pārmaiņa   iegūto punktu īpatsvars 2022 -> 2025, pp. Adaptīvā testa dēļ
#                 posmu uzdevumus saņem dažāda līmeņa skolēni, tāpēc tiešs
#                 salīdzinājums starp veidiem ir tīrs tikai kodola uzdevumiem.
#   Atlikums      novērotā pārmaiņa mīnus tā, ko paredz oficiālā 2025. gada skala
#                 (51. skripta modelis: 2022. gada parametri katram uzdevumam,
#                 2025. gada faktisko saņēmēju PV). Negatīvs: šis veids kritās
#                 VAIRĀK nekā kopējā spēja; pozitīvs: mazāk.
#   Ekvivalentā   oficiālā pārmaiņa + atlikums / slīpums, skalas punktos: cik
#   pārmaiņa      būtu kritums, ja visu mērītu tikai ar šā veida uzdevumiem.
#   Ātrās         ātro atbilžu daļa (slieksnis kā 23. un 52. skriptā).
#
# Tikai A plūsma (docs/51, tvēruma lēmums 2026-09-11).
#
# Izvade:  output/pisa2025/uzdevumu_veidi_2022_2025.csv
#          output/pisa2025/logs/54_uzdevumu_veidi_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")
source("R/pisa2025/00_kopigas_funkcijas.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/54_uzdevumu_veidi_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
LE <- LE_TAB$LE[LE_TAB$joma == "READ" & LE_TAB$no == 2022 & LE_TAB$uz == 2025]   # 1,094 (skriptā pašlaik nelieto); OECD 2025 Vol. I, atvasināts, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md
MIN_SANEMEJI <- 30
U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds")
uzd <- U$uzd; J <- length(uzd)
KL <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2022.csv", stringsAsFactors = FALSE)
kl <- KL[match(uzd, KL$uzd), ]
cat(sprintf("Klasifikācija atrasta %d no %d uzdevumiem\n", sum(!is.na(kl$uzd)), J))
D <- ab_dati()
zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "
plusmaA <- function(g) ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A")

dati <- function(g, d) {
  s <- plusmaA(g)
  d <- d[as.character(d$CNT) == "LVA", ]
  d <- d[match(as.numeric(s$sk$id), as.numeric(d$CNTSTUID)), ]
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  Tm <- vapply(d[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(d)))
  dimnames(S) <- dimnames(Tm) <- list(NULL, uzd)
  c(s, list(S = S, P = punkti_matrica(S), T = Tm, V = !is.na(S) & S %in% VERTETAS))
}
X22 <- dati("2022", U$d22); X25 <- dati("2025", U$d25)
M <- length(X22$pv)

lva22 <- U$d22[as.character(U$d22$CNT) == "LVA", ]
T22 <- vapply(lva22[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(lva22)))
sl <- pmin(0.10 * apply(T22, 2, function(x) mean(x, na.rm = TRUE)), 10000)
atrs <- function(Xg) { A <- sweep(Xg$T, 2, sl, "<"); A[is.na(A)] <- FALSE; A & Xg$V }
X22$A <- atrs(X22); X25$A <- atrs(X25)

agg <- function(Y, Xg, sel, w = Xg$sk$w) {
  W <- matrix(w, nrow(sel), ncol(sel)); 100 * sum(W[sel] * Y[sel]) / sum(W[sel]) }
maska <- function(Xg, kopa) Xg$V & matrix(kopa, nrow(Xg$V), J, byrow = TRUE)
rep_se <- function(f, Xg) { e <- f(Xg$sk$w); sqrt(sum((vapply(Xg$rw, f, 1) - e)^2) / 20) }
parametri <- function(m) {
  th <- (X22$pv[[m]] - 500) / 100
  t(vapply(seq_len(J), function(j) {
    v <- X22$V[, j]
    if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- X22$sk$w[v]
    stats::coef(suppressWarnings(stats::glm(X22$P[v, j] ~ th[v], family = stats::quasibinomial(),
                                            weights = w / mean(w))))
  }, numeric(2)))
}
pmat <- function(Xg, B, m, nobide = 0) {
  th <- (Xg$pv[[m]] - nobide - 500) / 100
  stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
}
B <- lapply(seq_len(M), parametri)
ok <- !is.na(B[[1]][, 1])
P22 <- lapply(seq_len(M), function(m) pmat(X22, B[[m]], m))
P25 <- lapply(seq_len(M), function(m) pmat(X25, B[[m]], m))
P25s <- lapply(seq_len(M), function(m) pmat(X25, B[[m]], m, -1))

o <- lapply(list(X22, X25), function(Xg) rubin_brr(Xg$pv, Xg$sk$w, Xg$rw, VID))
off <- o[[2]][["est"]] - o[[1]][["est"]]
cat(sprintf("Oficiālā A plūsmas pārmaiņa 2022 -> 2025: %+.1f\n\n", off))

rezultats <- function(kp) {
  s22 <- maska(X22, kp); s25 <- maska(X25, kp)
  o22 <- agg(X22$P, X22, s22); o25 <- agg(X25$P, X25, s25)
  p22 <- mean(vapply(seq_len(M), function(m) agg(P22[[m]], X22, s22), 1))
  p25 <- mean(vapply(seq_len(M), function(m) agg(P25[[m]], X25, s25), 1))
  slp <- mean(vapply(seq_len(M), function(m) agg(P25s[[m]], X25, s25) - agg(P25[[m]], X25, s25), 1))
  se_o <- sqrt(rep_se(function(w) agg(X22$P, X22, s22, w), X22)^2 +
               rep_se(function(w) agg(X25$P, X25, s25, w), X25)^2)
  se_a <- sqrt(rep_se(function(w) agg(X22$P, X22, s22, w) - agg(P22[[1]], X22, s22, w), X22)^2 +
               rep_se(function(w) agg(X25$P, X25, s25, w) - agg(P25[[1]], X25, s25, w), X25)^2)
  W22 <- matrix(X22$sk$w, nrow(s22), J); W25 <- matrix(X25$sk$w, nrow(s25), J)
  atl <- (o25 - o22) - (p25 - p22)
  c(uzd = sum(kp), r1_2022 = o22, r1_2025 = o25, izm = o25 - o22, se_izm = se_o,
    atl = atl, se_atl = se_a, slipums = slp, ekv = off + atl / slp, se_ekv = se_a / slp,
    atras_2022 = 100 * sum(W22[X22$A & s22]) / sum(W22[s22]),
    atras_2025 = 100 * sum(W25[X25$A & s25]) / sum(W25[s25]))
}

rait <- grepl("^CR590", uzd)
gr_terc <- rep(NA_character_, J)
q <- stats::quantile(kl$grutiba[!rait], c(1 / 3, 2 / 3), na.rm = TRUE)
gr_terc[!rait] <- as.character(cut(kl$grutiba[!rait], c(-Inf, q, Inf), labels = c("vieglākā trešdaļa", "vidējā trešdaļa", "grūtākā trešdaļa")))
gr_terc[rait] <- "raitums"
VAR <- list(
  "Kopā" = rep("visi", J),
  "Raitums pret pārējiem" = ifelse(rait, "raitums", "pārējie uzdevumi"),
  "Kognitīvais process" = ifelse(rait, "raitums", kl$process_skala),
  "Detalizēts process" = ifelse(rait, "raitums", kl$process),
  "Teksta avots" = ifelse(rait, NA, kl$avots),
  "Uzdevuma formāts" = ifelse(rait, "raitums (jā/nē)", kl$formats),
  "Vienības tips" = ifelse(rait, "raitums", kl$vienibas_tips),
  "MSAT posms" = kl$posms,
  "Teksta formāts" = ifelse(rait, NA, kl$teksta_formats),
  "Grūtība (2022, starptautiski)" = gr_terc)

rez <- list()
for (v in names(VAR)) {
  z <- VAR[[v]]
  cat(strrep("-", 78), "\n", v, "\n", sep = "")
  cat(sprintf("  %-40s %4s %15s %17s %16s %13s\n", "", "uzd.", "R1 pārmaiņa pp", "atlikums pp", "ekvivalentā", "ātrās %"))
  for (l in sort(unique(stats::na.omit(z)))) {
    kp <- ok & z %in% l
    if (sum(kp) < 4) next
    r <- rezultats(kp)
    rez[[length(rez) + 1]] <- data.frame(mainigais = v, vertiba = l, t(r))
    cat(sprintf("  %-40s %4d %+7.2f (%.2f)%s %+7.2f (%.2f)%s %+7.1f (%.1f) %5.1f -> %4.1f\n",
                substr(l, 1, 40), r[["uzd"]], r[["izm"]], r[["se_izm"]], zv(r[["izm"]], r[["se_izm"]]),
                r[["atl"]], r[["se_atl"]], zv(r[["atl"]], r[["se_atl"]]), r[["ekv"]], r[["se_ekv"]],
                r[["atras_2022"]], r[["atras_2025"]]))
  }
}
rez <- do.call(rbind, rez)
utils::write.csv(rez, "output/pisa2025/uzdevumu_veidi_2022_2025.csv", row.names = FALSE)
cat("\nIzveidots: output/pisa2025/uzdevumu_veidi_2022_2025.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
