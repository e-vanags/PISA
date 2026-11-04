# ============================================================================
# 52. Iesaiste un ātrā minēšana, A plūsma (docs/51, 6. solis)
# ============================================================================
#
# JAUTĀJUMS. `docs/53` parādīja, ka visa atšķirība starp oficiālo A plūsmas
# pārmaiņu 2022 -> 2025 (-19,4) un uzdevumu līmeņa R5 (-0,74 pp, apm. -4...-5
# punkti) ir iesaistes korekcija. Vai tā ir pamatota? Proti, vai ātrā atbilde
# un nesasniegtais uzdevums ir NEIESAISTE (skolēns negrib), nevis NESPĒJA
# (skolēns nevar), un vai 2025. gadā mainījās uzvedība vai testa noformējums?
#
# BLOKI
#   1. Pašnovērtētā piepūle (EFFORT1, EFFORT2), 2018, 2022, 2025. No atbilžu
#      laika neatkarīgs iesaistes mērs.
#   2. Ātrā minēšana un nesasniegtie: atbilžu un skolēnu līmenī, precizitāte,
#      atsevišķi fluences uzdevumiem.
#   3. Negrib vai nevar:
#      3a. vai ātri minētāji paši ziņo par zemāku piepūli;
#      3b. vai tas pats skolēns ātri atbild tieši uz grūtākajiem uzdevumiem
#          (tad izmešana novirza aplēsi uz augšu);
#      3c. vai tie ir 2025. gada skolēni bez anketas datiem (docs/52).
#   4. Vai 2022. gada nesasniedzēji un 2025. gada ātrie minētāji ir līdzīgi
#      skolēni; atbilžu temps; lasīšanas testa dizains (2025. gadā ir jauns
#      lineārais dizains L un UH testleti).
#   5. Korekcijas josla skalas punktos: R5 ar pieciem sliekšņiem, divi
#      pārrēķini (docs/53), daļējas pamatotības scenāriji φ, motivācijas
#      filtrēšana PV līmenī.
#
# Slieksnis kā 23. skriptā: 10 % no 2022. gada Latvijas vidējā atbildes laika
# katram uzdevumam, ne vairāk par 10 s, piemērots abiem gadiem. Laiks ir ms.
#
# Tikai A plūsma (docs/51, tvēruma lēmums 2026-09-11).
#
# Izvade:  output/pisa2025/iesaiste_piepule.csv
#          output/pisa2025/iesaiste_josla.csv
#          output/pisa2025/korekcijas/06_iesaiste.csv
#          output/pisa2025/logs/52_iesaiste_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")
source("R/pisa2025/00_kopigas_funkcijas.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/korekcijas", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/52_iesaiste_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
LE <- LE_TAB$LE[LE_TAB$joma == "READ" & LE_TAB$no == 2022 & LE_TAB$uz == 2025]   # 1,094: OECD 2025 Vol. I, atvasināts, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md (līdz 2026-09-13 pagaidu 3)
stopifnot(length(LE) == 1)
U  <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds")
uzd <- U$uzd
flu <- grepl("^CR590", uzd)
DZ <- readRDS("data/pisa2025/processed/lasisanas_dizains.rds")
KAL <- utils::read.csv("output/pisa2025/pretruna_parrekins.csv")
D  <- ab_dati()

zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "
plusmaA <- function(g) ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A")
brr1 <- function(x, w, rw, stat = VID) {
  e <- stat(x, w)
  c(est = e, se = sqrt(sum((vapply(rw, function(r) stat(x, r), 1) - e)^2) / 20))
}
wcor <- function(x, y, w) {
  mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w)
  sum(w * (x - mx) * (y - my)) / sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}
vidpv <- function(s) Reduce(`+`, s$pv) / length(s$pv)

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n1. Pašnovērtētā piepūle, A plūsma\n", strrep("=", 78), "\n\n")
cat("  EFFORT1: cik lielu piepūli ieliki šajā testā (1-10). EFFORT2: cik lielu\n",
    " ieliktu, ja tests ietekmētu atzīmes. 'Zema' ir EFFORT1 <= 5.\n\n")
piep <- list()
for (g in c("2018", "2022", "2025")) {
  s <- plusmaA(g); w <- s$sk$w
  for (k in c("piepule1", "piepule2", "starpiba")) {
    x <- if (k == "starpiba") s$sk$piepule2 - s$sk$piepule1 else s$sk[[k]]
    i <- !is.na(x)
    if (!any(i)) next
    rwi <- lapply(s$rw, `[`, i)
    a <- brr1(x[i], w[i], rwi)
    z <- if (k == "piepule1") 100 * brr1(as.numeric(x[i] <= 5), w[i], rwi) else c(est = NA, se = NA)
    piep[[length(piep) + 1]] <- data.frame(gads = g, merijums = k, vid = a[["est"]], se = a[["se"]],
      zema_pct = z[["est"]], zema_se = z[["se"]], trukst_pct = 100 * sum(w[!i]) / sum(w))
  }
}
piep <- do.call(rbind, piep)
print(format(piep, digits = 3), row.names = FALSE)
for (k in c("piepule1", "starpiba")) for (p in list(c("2018", "2022"), c("2022", "2025"))) {
  a <- piep[piep$merijums == k & piep$gads == p[1], ]; b <- piep[piep$merijums == k & piep$gads == p[2], ]
  if (!nrow(a) || !nrow(b)) next
  d <- b$vid - a$vid; s <- sqrt(a$se^2 + b$se^2)
  cat(sprintf("  %-9s %s->%s  vidējais %+.2f (%.2f)%s", k, p[1], p[2], d, s, zv(d, s)))
  if (k == "piepule1") { dz <- b$zema_pct - a$zema_pct; sz <- sqrt(a$zema_se^2 + b$zema_se^2)
    cat(sprintf("   zema piepūle %+.1f pp (%.1f)%s", dz, sz, zv(dz, sz))) }
  cat("\n")
}
cat("\n")
for (g in c("2018", "2022", "2025")) {
  s <- plusmaA(g); w <- s$sk$w; pv <- vidpv(s); m <- is.na(s$sk$piepule1)
  cat(sprintf("  %s bez piepūles atbildes %4.1f %%: PV %.1f, ar atbildi %.1f\n",
              g, 100 * sum(w[m]) / sum(w), VID(pv[m], w[m]), VID(pv[!m], w[!m])))
}
utils::write.csv(piep, "output/pisa2025/iesaiste_piepule.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Uzdevumu līmeņa dati A plūsmai, kā 51. skriptā, plus atbilžu laiki.
# ---------------------------------------------------------------------------
lva22 <- U$d22[as.character(U$d22$CNT) == "LVA", ]
T22 <- vapply(lva22[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(lva22)))
videjie <- apply(T22, 2, function(x) mean(x, na.rm = TRUE))
SLIEKSNI <- list("normatīvs 5 %"  = pmin(0.05 * videjie, 10000),
                 "normatīvs 10 %" = pmin(0.10 * videjie, 10000),
                 "normatīvs 15 %" = pmin(0.15 * videjie, 10000),
                 "normatīvs 20 %" = pmin(0.20 * videjie, 10000),
                 "fiksēts 2 s"    = rep(2000, length(uzd)))
GALV <- "normatīvs 10 %"

dati <- function(g, d) {
  s <- plusmaA(g)
  d <- d[as.character(d$CNT) == "LVA", ]
  d <- d[match(as.numeric(s$sk$id), as.numeric(d$CNTSTUID)), ]
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  Tm <- vapply(d[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(d)))
  dimnames(S) <- dimnames(Tm) <- list(NULL, uzd)
  c(s, list(S = S, P = punkti_matrica(S), T = Tm,
            V = !is.na(S) & S %in% VERTETAS, NS = !is.na(S) & S == "nesasniegts",
            ADM = !is.na(S) & S != "nederigs"))
}
atrs <- function(Xg, sl) { M <- sweep(Xg$T, 2, sl, "<"); M[is.na(M)] <- FALSE; M & Xg$V }
X <- list("2022" = dati("2022", U$d22), "2025" = dati("2025", U$d25))
for (g in names(X)) {
  Xg <- X[[g]]
  X[[g]]$A <- atrs(Xg, SLIEKSNI[[GALV]])
  nV <- rowSums(Xg$V)
  X[[g]]$adm <- rowSums(Xg$ADM) > 0
  X[[g]]$r <- ifelse(nV > 0, rowSums(X[[g]]$A) / pmax(nV, 1), NA)
  eng <- Xg$V & !X[[g]]$A
  X[[g]]$prec <- ifelse(rowSums(eng) > 0, rowSums(ifelse(eng, Xg$P, 0)) / pmax(rowSums(eng), 1), NA)
  X[[g]]$ns_any <- rowSums(Xg$NS) > 0
}

# R3M: ātrās atbildes netiek izmestas, bet aizstātas ar paredzamo punktu, ja
# skolēns būtu iesaistījies, ņemot vērā, ka ātri atbildētie uzdevumi ir
# grūtāki (3b). Spēja pēc Raša tuvinājuma no skolēna iesaistītajām atbildēm:
# θ = logit(precizitāte) + iesaistīto uzdevumu vidējā grūtība. Grūtība `grut`
# ir no 2022. gada (definēta 3b). Skolēniem bez iesaistītām atbildēm ātrās
# atbildes paliek izmestas kā R3.
ipats_r3m <- function(Xg, w, Ar) {
  eng <- Xg$V & !Ar
  G <- matrix(grut, nrow(Ar), ncol(Ar), byrow = TRUE)
  ne <- rowSums(eng)
  a <- rowSums(ifelse(eng, Xg$P, 0)) / pmax(ne, 1)
  gbar <- rowSums(ifelse(eng, G, 0)) / pmax(ne, 1)
  th <- stats::qlogis(pmin(pmax(a, 0.02), 0.98)) + gbar
  Y <- ifelse(Ar, stats::plogis(th - G), Xg$P)
  sauc <- Xg$V & !(Ar & matrix(ne == 0, nrow(Ar), ncol(Ar)))
  W <- matrix(w, nrow(Ar), ncol(Ar))
  100 * sum(W[sauc] * Y[sauc]) / sum(W[sauc])
}
ipats <- function(Xg, w, noteik, Ar) {
  if (noteik == "R3M") return(ipats_r3m(Xg, w, Ar))
  b <- !is.na(Xg$S)
  sauc <- switch(noteik,
    R1 = Xg$V,
    R2 = b & Xg$S %in% VERTETAS_AR_NESASNIEGTIEM,
    R3 = Xg$V & !Ar,
    R5 = b & Xg$S %in% VERTETAS_AR_NESASNIEGTIEM & !Ar)
  W <- matrix(w, nrow(Xg$S), ncol(Xg$S))
  100 * sum(W[sauc] * Xg$P[sauc]) / sum(W[sauc])
}
izm_pp <- function(noteik, sl = SLIEKSNI[[GALV]]) {
  r <- lapply(X, function(Xg) {
    Ar <- atrs(Xg, sl)
    e <- ipats(Xg, Xg$sk$w, noteik, Ar)
    c(e, sum((vapply(Xg$rw, function(q) ipats(Xg, q, noteik, Ar), 1) - e)^2) / 20)
  })
  c(d = r[["2025"]][1] - r[["2022"]][1], se = sqrt(r[["2025"]][2] + r[["2022"]][2]))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n2. Ātrā minēšana un nesasniegtie uzdevumi, A plūsma\n",
    strrep("=", 78), "\n\n")
tab2 <- list()
for (g in names(X)) {
  Xg <- X[[g]]; w <- Xg$sk$w; W <- matrix(w, nrow(Xg$S), ncol(Xg$S))
  pr <- function(m) sum(W[m] * Xg$P[m]) / sum(W[m])
  FL <- matrix(flu, nrow(Xg$S), ncol(Xg$S), byrow = TRUE)
  a <- Xg$adm; san <- !is.na(Xg$r)
  tab2[[g]] <- data.frame(gads = g,
    nesasniegti_pct = 100 * sum(W[Xg$NS]) / sum(W[Xg$ADM]),
    atras_pct = 100 * sum(W[Xg$A]) / sum(W[Xg$V]),
    atras_fluence_pct = 100 * sum(W[Xg$A & FL]) / sum(W[Xg$V & FL]),
    atras_parejie_pct = 100 * sum(W[Xg$A & !FL]) / sum(W[Xg$V & !FL]),
    prec_atras = pr(Xg$A), prec_iesaist = pr(Xg$V & !Xg$A),
    prec_atras_fluence = pr(Xg$A & FL), prec_atras_parejie = pr(Xg$A & !FL),
    sk_ar_atru_pct = 100 * sum(w[san & Xg$r > 0]) / sum(w[san]),
    sk_atras_10_pct = 100 * sum(w[san & Xg$r >= 0.10]) / sum(w[san]),
    sk_atras_25_pct = 100 * sum(w[san & Xg$r >= 0.25]) / sum(w[san]),
    sk_nesasniedza_pct = 100 * sum(w[a & Xg$ns_any]) / sum(w[a]))
}
tab2 <- do.call(rbind, tab2)
print(t(format(tab2, digits = 3)))
med_iesaist <- function(Xg) apply(ifelse(Xg$V & !Xg$A, Xg$T, NA), 2, stats::median, na.rm = TRUE)
temps <- med_iesaist(X[["2025"]]) / med_iesaist(X[["2022"]])
cat(sprintf("\n  Iesaistīto atbilžu mediānais laiks 2025 / 2022 (mediāna pa uzdevumiem): %.2f; fluence %.2f, pārējie %.2f\n",
            stats::median(temps, na.rm = TRUE), stats::median(temps[flu], na.rm = TRUE),
            stats::median(temps[!flu], na.rm = TRUE)))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n3. Negrib vai nevar\n", strrep("=", 78), "\n\n")
cat("  3a. Ātro atbilžu daļa pēc pašnovērtētās piepūles (kopīgo uzdevumu saņēmēji)\n\n")
for (g in names(X)) {
  Xg <- X[[g]]; e <- Xg$sk$piepule1; i <- !is.na(Xg$r) & !is.na(e); w <- Xg$sk$w
  j <- cut(e[i], c(0, 4, 6, 8, 10), labels = c("1-4", "5-6", "7-8", "9-10"))
  vid <- tapply(seq_along(j), j, function(k) 100 * VID(Xg$r[i][k], w[i][k]))
  dal <- tapply(w[i], j, sum) / sum(w[i]) * 100
  cat(sprintf("  %s  %s   korelācija ar piepūli %.2f\n", g,
              paste(sprintf("%s: %4.1f %% (%.0f %% sk.)", names(vid), vid, dal), collapse = " | "),
              wcor(Xg$r[i], e[i], w[i])))
}

cat("\n  3b. Vai tas pats skolēns ātri atbild uz grūtākiem uzdevumiem?\n")
cat("      Uzdevuma grūtība: -logit no 2022. gada A plūsmas iesaistīto atbilžu\n",
    "     precizitātes. Starpība: ātri atbildēto mīnus iesaistīti atbildēto uzdevumu\n",
    "     vidējā grūtība tā paša skolēna ietvaros, logitos. > 0 nozīmē, ka ātrās\n",
    "     atbildes koncentrējas grūtākajos uzdevumos.\n\n")
X22 <- X[["2022"]]; eng22 <- X22$V & !X22$A
W22 <- matrix(X22$sk$w, nrow(X22$S), ncol(X22$S))
pj <- colSums(ifelse(eng22, W22 * X22$P, 0)) / colSums(ifelse(eng22, W22, 0))
grut <- -stats::qlogis(pmin(pmax(pj, 0.01), 0.99))
b3 <- list()
for (g in names(X)) for (kopa in c("visi", "bez fluences")) {
  Xg <- X[[g]]; eng <- Xg$V & !Xg$A; A <- Xg$A
  if (kopa == "bez fluences") { A[, flu] <- FALSE; eng[, flu] <- FALSE }
  G <- matrix(grut, nrow(A), ncol(A), byrow = TRUE)
  ok <- rowSums(A) > 0 & rowSums(eng) > 0 & !is.na(G[1, 1])
  d <- rowSums(ifelse(A, G, 0)) / pmax(rowSums(A), 1) - rowSums(ifelse(eng, G, 0)) / pmax(rowSums(eng), 1)
  x <- brr1(d[ok], Xg$sk$w[ok], lapply(Xg$rw, `[`, ok))
  b3[[length(b3) + 1]] <- data.frame(gads = g, kopa = kopa, n = sum(ok), starpiba = x[["est"]], se = x[["se"]])
  cat(sprintf("  %s %-13s skolēni ar ātrām un iesaistītām atbildēm %4d   starpība %+.3f (%.3f)%s\n",
              g, kopa, sum(ok), x[["est"]], x[["se"]], zv(x[["est"]], x[["se"]])))
}

cat("\n  3c. 2025. gada A plūsmas skolēni bez anketas datiem (ESCS trūkst)\n\n")
X25 <- X[["2025"]]; s25 <- plusmaA("2025"); m <- is.na(X25$sk$escs); w <- X25$sk$w
pv25 <- vidpv(s25)
for (k in c(TRUE, FALSE)) {
  i <- m == k; ir <- i & !is.na(X25$r); ie <- i & !is.na(X25$sk$piepule1)
  cat(sprintf("  %-15s n %4d (%4.1f %%)  PV %5.1f  ātrās %4.1f %%  ≥10 %% ātru %4.1f %%  piepūle %s (trūkst %4.1f %%)\n",
              if (k) "bez anketas" else "ar anketu", sum(i), 100 * sum(w[i]) / sum(w),
              VID(pv25[i], w[i]), 100 * VID(X25$r[ir], w[ir]),
              100 * sum(w[ir & X25$r >= 0.10]) / sum(w[ir]),
              if (any(ie)) sprintf("%.2f", VID(X25$sk$piepule1[ie], w[ie])) else "nav",
              100 * sum(w[i & is.na(X25$sk$piepule1)]) / sum(w[i])))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n4. Nesasniegtie 2022 un ātrie minētāji 2025; testa dizains\n",
    strrep("=", 78), "\n\n")
prof <- function(Xg, i, nos) {
  w <- Xg$sk$w; ip <- i & !is.na(Xg$prec); ie <- i & !is.na(Xg$sk$piepule1)
  cat(sprintf("  %-34s %5.1f %% saņēmēju   precizitāte iesaistītajās %.3f   piepūle %.2f\n",
              nos, 100 * sum(w[i]) / sum(w[Xg$adm]), VID(Xg$prec[ip], w[ip]), VID(Xg$sk$piepule1[ie], w[ie])))
}
for (g in names(X)) {
  Xg <- X[[g]]; a <- Xg$adm; r10 <- !is.na(Xg$r) & Xg$r >= 0.10
  prof(Xg, a & Xg$ns_any, paste(g, "nesasniedza vismaz vienu"))
  prof(Xg, a & rowSums(Xg$NS) / pmax(rowSums(Xg$ADM), 1) >= 0.10, paste(g, "≥ 10 % nesasniegtu"))
  prof(Xg, a & r10, paste(g, "≥ 10 % ātru atbilžu"))
  prof(Xg, a & (Xg$ns_any | r10), paste(g, "kāds no abiem"))
  prof(Xg, a & !(Xg$ns_any | r10), paste(g, "neviens"))
  cat("\n")
}
cat("  Lasīšanas testa dizains, A plūsma (svērti %)\n\n")
diz <- list()
for (g in names(X)) {
  s <- plusmaA(g); w <- s$sk$w; dz <- DZ[[g]][match(as.numeric(s$sk$id), DZ[[g]]$CNTSTUID), ]
  pv <- vidpv(s)
  for (k in c("RDESIGN_LAB", "RCORE_PERF_LAB")) {
    if (!k %in% names(dz)) next
    z <- ifelse(is.na(dz[[k]]), "nav", dz[[k]])
    for (v in sort(unique(z))) {
      i <- z == v
      diz[[length(diz) + 1]] <- data.frame(gads = g, mainigais = k, vertiba = v, n = sum(i),
                                           pct = 100 * sum(w[i]) / sum(w), pv = VID(pv[i], w[i]))
    }
  }
}
diz <- do.call(rbind, diz)
print(format(diz, digits = 4), row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n5. Korekcijas josla skalas punktos\n", strrep("=", 78), "\n\n")
s22 <- plusmaA("2022"); s25 <- plusmaA("2025")
o22 <- rubin_brr(s22$pv, s22$sk$w, s22$rw, VID); o25 <- rubin_brr(s25$pv, s25$sk$w, s25$rw, VID)
off <- c(d = o25[["est"]] - o22[["est"]], se = sqrt(o22[["se"]]^2 + o25[["se"]]^2 + LE^2))
r1 <- izm_pp("R1")
FAK <- c("vienmērīgā nobīde" = KAL$punkti[1] / KAL$pp[1], "faktiskā attiecība" = off[["d"]] / r1[["d"]])
cat(sprintf("  Oficiālā pārmaiņa %+.1f (%.1f); R1 %+.2f pp (%.2f); pārrēķins: 1 pp = %.2f vai %.2f punkti\n\n",
            off[["d"]], off[["se"]], r1[["d"]], r1[["se"]], FAK[1], FAK[2]))
cat("  korekcija = (noteikuma pp - R1 pp) x pārrēķins; koriģētā pārmaiņa = oficiālā + korekcija.\n",
    " R2: nesasniegtie kā nepareizi; R3: ātrās izmestas; R3M: ātrās aizstātas ar paredzamo\n",
    " punktu pēc grūtības; R5: R2 un R3 kopā (docs/24).\n\n")
josla <- list()
for (sl in names(SLIEKSNI)) for (nt in c("R2", "R3", "R3M", "R5")) {
  if (nt == "R2" && sl != GALV) next
  x <- izm_pp(nt, SLIEKSNI[[sl]])
  for (f in names(FAK)) {
    k <- (x[["d"]] - r1[["d"]]) * FAK[[f]]
    josla[[length(josla) + 1]] <- data.frame(
      slieksnis = if (nt == "R2") "(nav)" else sl, noteikums = nt, pp = x[["d"]], pp_se = x[["se"]],
      parrekins = f, korekcija = k, koriget_izm = off[["d"]] + k)
  }
}
josla <- do.call(rbind, josla)
print(format(josla, digits = 3), row.names = FALSE)
utils::write.csv(josla, "output/pisa2025/iesaiste_josla.csv", row.names = FALSE)

cat("\n  Daļējas pamatotības scenāriji (slieksnis 10 %). φ ir daļa no korekcijas,\n",
    " kas atspoguļo neiesaisti, nevis nespēju.\n\n")
for (nt in c("R3M", "R5")) {
  g5 <- josla[josla$noteikums == nt & josla$slieksnis == GALV, ]
  cat(sprintf("  %s\n", nt))
  for (phi in c(0, 0.25, 0.5, 0.75, 1))
    cat(sprintf("    φ = %.2f   A pārmaiņa 2022 -> 2025: %+.1f (vienmērīgā) / %+.1f (attiecība)\n", phi,
                off[["d"]] + phi * g5$korekcija[1], off[["d"]] + phi * g5$korekcija[2]))
}

cat("\n  Motivācijas filtrēšana PV līmenī. Filtrs izslēdz vājākus skolēnus, tāpēc\n",
    " līmenis ceļas; nozīme ir tikai pārmaiņai, salīdzinot ar to pašu kopu bez filtra.\n\n")
filtrs <- function(nos, f22, f25) {
  pa <- function(s, i) { i[is.na(i)] <- FALSE
    rubin_brr(lapply(s$pv, `[`, i), s$sk$w[i], lapply(s$rw, `[`, i), VID) }
  a <- pa(s22, f22); b <- pa(s25, f25)
  d <- b[["est"]] - a[["est"]]; se <- sqrt(a[["se"]]^2 + b[["se"]]^2 + LE^2)
  cat(sprintf("  %-42s %5.1f -> %5.1f   %+6.1f (%.1f)%s\n", nos, a[["est"]], b[["est"]], d, se, zv(d, se)))
  data.frame(filtrs = nos, y2022 = a[["est"]], y2025 = b[["est"]], izm = d, se = se)
}
flt <- rbind(
  filtrs("kopīgo uzdevumu saņēmēji, visi", !is.na(X[["2022"]]$r), !is.na(X[["2025"]]$r)),
  filtrs("saņēmēji ar < 10 % ātru atbilžu", X[["2022"]]$r < 0.10, X[["2025"]]$r < 0.10),
  filtrs("ar piepūles atbildi, visi", !is.na(s22$sk$piepule1), !is.na(s25$sk$piepule1)),
  filtrs("piepūle >= 6", s22$sk$piepule1 >= 6, s25$sk$piepule1 >= 6))
utils::write.csv(flt, "output/pisa2025/iesaiste_filtri.csv", row.names = FALSE)

# Standarta formāts: korekcija kā 2025. gada līmeņa nobīde pret 2022. gadu.
g3 <- josla[josla$noteikums %in% c("R3", "R3M"), ]
gm <- josla[josla$noteikums == "R3M" & josla$slieksnis == GALV & josla$parrekins == "faktiskā attiecība", ]
g5 <- josla[josla$noteikums == "R5" & josla$slieksnis == GALV & josla$parrekins == "faktiskā attiecība", ]
kor <- rbind(
  korekcijas_rinda("2025", "A", "iesaiste: ātrās atbildes ar grūtības korekciju (R3M), slieksnis 10 %",
                   gm$korekcija, 0, max(g3$korekcija),
                   paste("nesasniegtie vērtēti kā oficiāli; ātrās atbildes aizstātas ar paredzamo punktu",
                         "pēc skolēna iesaistītajām atbildēm un uzdevuma grūtības; apakšējā 0 = oficiālā",
                         "vērtēšana; augšējā = lielākā R3/R3M korekcija pa 5 sliekšņiem un 2 pārrēķiniem")),
  korekcijas_rinda("2025", "A", "R5 (docs/24): ietver nesasniegtos kā nepareizus", g5$korekcija,
                   pienemumi = "sk. 4. bloku par to, kas 2022. gadā nesasniedza testa beigas"))
utils::write.csv(kor, "output/pisa2025/korekcijas/06_iesaiste.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/iesaiste_piepule.csv\n  output/pisa2025/iesaiste_josla.csv\n",
    " output/pisa2025/iesaiste_filtri.csv\n  output/pisa2025/korekcijas/06_iesaiste.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
