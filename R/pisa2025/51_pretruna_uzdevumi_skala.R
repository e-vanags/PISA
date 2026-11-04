# ============================================================================
# 51. Pretruna: uzdevumu līmenis pret oficiālo skalu, A plūsma 2022-2025
#     (docs/51, 5. solis; docs/40 6. sadaļa)
# ============================================================================
#
# PRETRUNA. docs/40 salīdzināja divus skaitļus:
#   -19,4  A plūsmas pārmaiņa oficiālajā PV skalā; skalas punkti; bez korekcijas
#   -0,74  A plūsmas pārmaiņa iegūto punktu īpatsvarā 196 kopīgajos uzdevumos;
#          procentpunkti; PĒC iesaistes korekcijas (R5, docs/24 un docs/28)
# Tie atšķiras gan mērvienībā, gan korekcijā. Oficiālās skalas pāris uzdevumu
# līmenī ir R1 (nesasniegtie nav vērtēti, ātrās atbildes vērtētas kā PISA to
# dara): -3,47 pp.
#
# ŠIS SKRIPTS PĀRBAUDA TRĪS LIETAS
#   1. Kas saņēma kopīgos uzdevumus: daļa, uzdevumu skaits, šo skolēnu PV
#      līmenis un PV pārmaiņa. Tests ir adaptīvs, tāpēc saņēmēji nav nejauša
#      apakšizlase, un atlase var mainīties starp cikliem.
#   2. Saskaņotība: vai oficiālā PV pārmaiņa paredz novēroto R1 pārmaiņu?
#      Katram uzdevumam 2022. gada A plūsmā novērtē frakcionālo logit modeli
#      punkti ~ PV (katrai PV atsevišķi) un ar 2022. gada parametriem paredz
#      2025. gada R1 no 2025. gada faktisko saņēmēju PV. Maršrutēšana ir ņemta
#      vērā, jo katram uzdevumam lieto tā faktiskos saņēmējus.
#   3. Pārrēķins: cik skalas punktu atbilst -3,47 pp un -0,74 pp šajā uzdevumu
#      kopā šai skolēnu grupai (PV nobīde, kas 2022. gada datos dod tādu pašu
#      R1 pārmaiņu).
#
# UZMANĪBU. PV ir iegūtas no tām pašām atbildēm ar oficiālo vērtēšanu, tāpēc
# 2. punkts ir saskaņotības pārbaude, nevis neatkarīgs apstiprinājums. Ja
# prognoze sakrīt ar novēroto, pretrunas R1 līmenī nav, un visa atšķirība starp
# -19,4 un -0,74 ir iesaistes korekcija (6. solis). R5 pārrēķins punktos ir
# aptuvens, jo R5 maina arī to, kuras atbildes tiek vērtētas.
#
# Tikai A plūsma (docs/51, tvēruma lēmums 2026-09-11).
#
# Izvade:  output/pisa2025/pretruna_parbaude.csv
#          output/pisa2025/pretruna_uzdevumi.csv   katra uzdevuma saņēmēji
#          output/pisa2025/logs/51_pretruna_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")
source("R/pisa2025/00_kopigas_funkcijas.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/51_pretruna_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
LE <- LE_TAB$LE[LE_TAB$joma == "READ" & LE_TAB$no == 2022 & LE_TAB$uz == 2025]   # 1,094: OECD 2025 Vol. I, atvasināts, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md (līdz 2026-09-13 pagaidu 3)
stopifnot(length(LE) == 1)
MIN_SANEMEJI <- 30
U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds")
uzd <- U$uzd
D <- ab_dati()
zv <- function(d, s) if (abs(d / s) > 1.96) "*" else " "

# A plūsmas skolēni no ielādētāja, ar viņu atbildēm uz kopīgajiem uzdevumiem.
dati <- function(g, d) {
  s <- ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A")
  d <- d[as.character(d$CNT) == "LVA", ]
  r <- match(as.numeric(s$sk$id), as.numeric(d$CNTSTUID))
  cat(sprintf("%s: A plūsmā %d skolēni, no tiem uzdevumu failā %d\n", g, nrow(s$sk), sum(!is.na(r))))
  d <- d[r, ]
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  dimnames(S) <- list(NULL, uzd)
  c(s, list(S = S, P = punkti_matrica(S), V = !is.na(S) & S %in% VERTETAS))
}
X22 <- dati("2022", U$d22)
X25 <- dati("2025", U$d25)
M <- length(X22$pv)

# R1: iegūto punktu īpatsvars, svērts pēc atbildēm, kā 23. skriptā.
R1 <- function(X, w = X$sk$w, ok = rep(TRUE, length(uzd))) {
  v <- X$V & matrix(ok, nrow(X$V), ncol(X$V), byrow = TRUE)
  W <- matrix(w, nrow(v), ncol(v))
  100 * sum(W[v] * X$P[v]) / sum(W[v])
}
R1_se <- function(X, ok = rep(TRUE, length(uzd))) {
  e <- R1(X, ok = ok)
  sqrt(sum((vapply(X$rw, function(r) R1(X, r, ok), numeric(1)) - e)^2) / 20)
}

cat(sprintf("\nPārbaude pret 23. skriptu: R1 2022 = %.2f (23: 74,60), 2025 = %.2f (23: 71,13)\n",
            R1(X22), R1(X25)))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n1. Kas saņēma kopīgos uzdevumus\n", strrep("=", 78), "\n\n")
sanem <- list()
for (X in list(X22, X25)) {
  g <- X$gads; n <- rowSums(X$V); w <- X$sk$w; r <- n > 0
  sub <- function(i) list(pv = lapply(X$pv, `[`, i), w = w[i], rw = lapply(X$rw, `[`, i))
  s1 <- sub(r); s0 <- sub(!r)
  a  <- rubin_brr(X$pv, w, X$rw, VID)
  sa <- rubin_brr(s1$pv, s1$w, s1$rw, VID)
  ne <- rubin_brr(s0$pv, s0$w, s0$rw, VID)
  at <- rubin_brr(X$pv, w * n, lapply(X$rw, function(q) q * n), VID)
  sanem[[g]] <- data.frame(gads = g, dala_sanemeji = 100 * sum(w[r]) / sum(w),
    uzd_vid = sum(w[r] * n[r]) / sum(w[r]), pv_visi = a[["est"]], se_visi = a[["se"]],
    pv_sanemeji = sa[["est"]], se_sanemeji = sa[["se"]],
    pv_nesanemeji = ne[["est"]], se_nesanemeji = ne[["se"]],
    pv_atbildes = at[["est"]], se_atbildes = at[["se"]])
  x <- sanem[[g]]
  cat(sprintf("  %s  saņēma %.1f %% skolēnu, vidēji %.1f kopīgi uzdevumi saņēmējam\n",
              g, x$dala_sanemeji, x$uzd_vid))
  cat(sprintf("        PV: visi %.1f (%.1f) | saņēmēji %.1f (%.1f) | nesaņēmēji %.1f (%.1f) | svērts pēc atbildēm %.1f (%.1f)\n\n",
              x$pv_visi, x$se_visi, x$pv_sanemeji, x$se_sanemeji,
              x$pv_nesanemeji, x$se_nesanemeji, x$pv_atbildes, x$se_atbildes))
}
sanem <- do.call(rbind, sanem)
izm <- function(k) { d <- sanem[2, paste0("pv_", k)] - sanem[1, paste0("pv_", k)]
  s <- sqrt(sanem[2, paste0("se_", k)]^2 + sanem[1, paste0("se_", k)]^2 + LE^2); c(d, s) }
for (k in c("visi", "sanemeji", "nesanemeji", "atbildes")) {
  x <- izm(k)
  cat(sprintf("  PV pārmaiņa 2022 -> 2025, %-11s %+6.1f (%.1f)%s\n", k, x[1], x[2], zv(x[1], x[2])))
}

# Katra uzdevuma saņēmēju PV pārmaiņa: vai maršrutēšana mainījās?
pvv <- function(X) Reduce(`+`, X$pv) / M
p22 <- pvv(X22); p25 <- pvv(X25)
uz <- data.frame(uzd = uzd,
  n22 = colSums(X22$V), n25 = colSums(X25$V),
  pv22 = vapply(seq_along(uzd), function(j) { v <- X22$V[, j]; if (sum(v)) VID(p22[v], X22$sk$w[v]) else NA }, 1),
  pv25 = vapply(seq_along(uzd), function(j) { v <- X25$V[, j]; if (sum(v)) VID(p25[v], X25$sk$w[v]) else NA }, 1),
  p22 = vapply(seq_along(uzd), function(j) { v <- X22$V[, j]; if (sum(v)) VID(X22$P[v, j], X22$sk$w[v]) else NA }, 1),
  p25 = vapply(seq_along(uzd), function(j) { v <- X25$V[, j]; if (sum(v)) VID(X25$P[v, j], X25$sk$w[v]) else NA }, 1))
uz$pv_izm <- uz$pv25 - uz$pv22
q <- stats::quantile(uz$pv_izm, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
cat(sprintf("\n  Uzdevuma saņēmēju PV pārmaiņa pa uzdevumiem: P10 %+.1f, P25 %+.1f, mediāna %+.1f, P75 %+.1f, P90 %+.1f\n",
            q[1], q[2], q[3], q[4], q[5]))
cat(sprintf("  Uzdevumi ar < %d saņēmējiem A plūsmā: 2022 %d, 2025 %d\n",
            MIN_SANEMEJI, sum(uz$n22 < MIN_SANEMEJI), sum(uz$n25 < MIN_SANEMEJI)))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n2. Vai oficiālā PV pārmaiņa paredz novēroto R1 pārmaiņu?\n",
    strrep("=", 78), "\n\n")
parametri <- function(m) {
  th <- (X22$pv[[m]] - 500) / 100
  t(vapply(seq_along(uzd), function(j) {
    v <- X22$V[, j]
    if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- X22$sk$w[v]
    stats::coef(suppressWarnings(stats::glm(X22$P[v, j] ~ th[v], family = stats::quasibinomial(),
                                            weights = w / mean(w))))
  }, numeric(2)))
}
prognoze <- function(X, B, m, w = X$sk$w, nobide = 0) {
  ok <- !is.na(B[, 1])
  th <- (X$pv[[m]] - nobide - 500) / 100
  Pp <- stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
  v <- X$V & matrix(ok, nrow(X$V), ncol(X$V), byrow = TRUE)
  W <- matrix(w, nrow(v), ncol(v))
  100 * sum(W[v] * Pp[v]) / sum(W[v])
}

B <- lapply(seq_len(M), parametri)
ok <- !is.na(B[[1]][, 1])
cat(sprintf("  Modelī iekļauti %d no %d uzdevumiem (>= %d saņēmēji 2022. gadā)\n",
            sum(ok), length(uzd), MIN_SANEMEJI))
cat(sprintf("  Slīpums (logit uz 100 punktiem): mediāna %.2f, diapazons %.2f līdz %.2f\n\n",
            stats::median(B[[1]][ok, 2]), min(B[[1]][ok, 2]), max(B[[1]][ok, 2])))

o22 <- R1(X22, ok = ok); o25 <- R1(X25, ok = ok)
f22 <- vapply(seq_len(M), function(m) prognoze(X22, B[[m]], m), 1)
f25 <- vapply(seq_len(M), function(m) prognoze(X25, B[[m]], m), 1)
d_obs <- o25 - o22
se_obs <- sqrt(R1_se(X22, ok)^2 + R1_se(X25, ok)^2)
d_pro <- mean(f25) - o22
# SE prognozei: izlase caur replikātsvariem ar fiksētiem parametriem (1. PV)
# plus dispersija starp PV. Parametru nenoteiktība nav iekļauta.
rep25 <- vapply(X25$rw, function(r) prognoze(X25, B[[1]], 1, r), 1)
rep22 <- vapply(X22$rw, function(r) R1(X22, r, ok), 1)
se_pro <- sqrt(sum((rep25 - f25[1])^2) / 20 + sum((rep22 - o22)^2) / 20 +
               (1 + 1 / M) * stats::var(f25))
atl <- d_obs - d_pro
cat(sprintf("  Modeļa pārbaude 2022: novērots %.2f, modelis %.2f (jāsakrīt)\n", o22, mean(f22)))
cat(sprintf("  R1 2022 -> 2025 novērots:                  %+6.2f pp (SE %.2f)\n", d_obs, se_obs))
cat(sprintf("  R1 2022 -> 2025 paredzēts no 2025 PV:      %+6.2f pp (SE %.2f)\n", d_pro, se_pro))
cat(sprintf("  Atlikums (novērots - paredzēts):           %+6.2f pp\n", atl))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n3. Procentpunkti skalas punktos\n", strrep("=", 78), "\n\n")
cat("  Nobīde Δ: par cik jāpazemina visu 2022. gada A skolēnu PV, lai modelis dotu\n",
    " doto R1 pārmaiņu tajās pašās atbildēs. Lokāls pārrēķins šai uzdevumu kopai.\n\n")
delta <- function(merkis) stats::uniroot(function(D)
  mean(vapply(seq_len(M), function(m) prognoze(X22, B[[m]], m, nobide = D), 1)) - o22 - merkis,
  c(-150, 150))$root
kalib <- list()
for (k in list(c("R1 novērotā (šī uzdevumu kopa)", d_obs, se_obs),
               c("R1 no 23. skripta", -3.47, 0.55),
               c("R5 no docs/24, docs/28 (aptuveni)", -0.74, 0.49))) {
  mk <- as.numeric(k[2]); s <- as.numeric(k[3])
  dl <- delta(mk); lo <- delta(mk - 1.96 * s); hi <- delta(mk + 1.96 * s)
  kalib[[length(kalib) + 1]] <- data.frame(ko = k[1], pp = mk, pp_se = s,
                                           punkti = -dl, apaksa = -lo, augsa = -hi)
  cat(sprintf("  %-36s %+6.2f pp  ≈ %+6.1f skalas punkti  (95 %%: %+.1f līdz %+.1f)\n",
              k[1], mk, -dl, -lo, -hi))
}
kalib <- do.call(rbind, kalib)
cat(sprintf("\n  Salīdzinājumam: oficiālā A plūsmas pārmaiņa %+.1f (visi), %+.1f (svērts pēc atbildēm)\n",
            izm("visi")[1], izm("atbildes")[1]))

rez <- data.frame(
  radītājs = c("R1 novērots", "R1 paredzēts no PV", "R1 atlikums", "PV visi", "PV saņēmēji",
               "PV svērts pēc atbildēm"),
  vertiba = c(d_obs, d_pro, atl, izm("visi")[1], izm("sanemeji")[1], izm("atbildes")[1]),
  se = c(se_obs, se_pro, NA, izm("visi")[2], izm("sanemeji")[2], izm("atbildes")[2]),
  merv = c("pp", "pp", "pp", "punkti", "punkti", "punkti"))
utils::write.csv(rez, "output/pisa2025/pretruna_parbaude.csv", row.names = FALSE)
utils::write.csv(kalib, "output/pisa2025/pretruna_parrekins.csv", row.names = FALSE)
utils::write.csv(cbind(uz, modeli = ok), "output/pisa2025/pretruna_uzdevumi.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/pretruna_parbaude.csv\n  output/pisa2025/pretruna_parrekins.csv\n",
    " output/pisa2025/pretruna_uzdevumi.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
