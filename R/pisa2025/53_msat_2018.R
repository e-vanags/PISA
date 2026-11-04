# ============================================================================
# 53. 2018. gada adaptīvā testa artefakts, A plūsma 2015 -> 2018
#     (docs/51, 7. solis; docs/43 5. sadaļa)
# ============================================================================
#
# HIPOTĒZE (docs/43 5. sad.). 2018. gadā lasīšanā pirmo reizi bija daudzpakāpju
# adaptīvais tests (MSAT). Tas maina, kurus uzdevumus saņem vājš skolēns, tāpēc
# 2015 -> 2018 apakšgala kritums varētu būt formāta, nevis prasmes pārmaiņa.
# A plūsmai: vidējais 489,4 -> 486,6; zem 2. līmeņa 17,0 -> 19,4 %; P10 -4,4.
#
# DIZAINS
#   Kodola uzdevumu salīdzinājums nav iespējams: visi kopīgie uzdevumi 2018.
#   gadā ir maršrutētajos posmos (2. sadaļa to parāda). Tāpēc:
#   1. Saskaņotība ar skalu. Katram kopīgajam uzdevumam 2015. gadā frakcionālais
#      logit punkti ~ PV (katrai PV atsevišķi, katrai grupai savi parametri); ar
#      tiem paredz 2018. gada rezultātu no 2018. gada faktisko saņēmēju PV, tātad
#      maršrutēšana ir ņemta vērā caur saņēmēju spēju. Atlikums = novērotā
#      pārmaiņa - paredzētā.
#   2. Salīdzinājums ar Igauniju un Lietuvu. Ja formāta maiņa (MSAT, uzdevumu
#      pozīcija, laiks) uzdevumus padarīja vieglākus visām valstīm, atlikums ir
#      visur, un to PISA starptautiskā skalēšana ņem vērā ar uzdevumu parametriem.
#      Latvijas skalas artefakts ir tikai Latvijai specifiskā daļa:
#      LVA A atlikums - EST un LTU vidējais atlikums.
#   3. Tas pats pa spēju kvintilēm: vai atlikums koncentrējas apakšgalā.
#   4. Iesaiste 2015 un 2018.
#
# Tikai A plūsma (docs/51, tvēruma lēmums 2026-09-11); EST un LTU ir visa valsts.
#
# Izvade:  output/pisa2025/msat_2018.csv, msat_2018_kvintiles.csv
#          output/pisa2025/korekcijas/07_msat.csv
#          output/pisa2025/logs/53_msat_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")
source("R/pisa2025/00_kopigas_funkcijas.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/korekcijas", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/53_msat_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
LE <- LE_TAB$LE[LE_TAB$joma == "READ" & LE_TAB$no == 2015 & LE_TAB$uz == 2018]   # 3,93 oficiālā (PISA 2018 Vol. I tab. I.A7.1); līdz 2026-09-13 pagaidu 3
stopifnot(length(LE) == 1)
MIN_SANEMEJI <- 30
GRUPAS <- c("LVA A", "LVA", "EST", "LTU")   # LVA: visa Latvija, robustuma pārbaudei
SALID <- c("EST", "LTU")
U <- readRDS("data/pisa2025/processed/uzdevumi_2015_2018.rds")
uzd <- U$uzd; J <- length(uzd)
D <- ab_dati()
KESA <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "
wq <- function(x, w, p) { o <- order(x); cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(pp) x[o][which(cw >= pp)[1]], 1) }

skoleni <- function(g, gr) {
  if (gr == "LVA A") return(ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A"))
  if (gr == "LVA") return(D[[g]])
  d <- KESA[[g]]; d <- d[as.character(d$CNT) == gr, ]
  list(gads = g, sk = data.frame(id = as.numeric(d$CNTSTUID), w = as.numeric(d$W_FSTUWT)),
       pv = lapply(paste0("PV", 1:10, "READ"), function(k) as.numeric(d[[k]])),
       rw = lapply(paste0("W_FSTURWT", 1:80), function(k) as.numeric(d[[k]])))
}
dati <- function(g, gr) {
  s <- skoleni(g, gr)
  d <- U[[paste0("d", substr(g, 3, 4))]]
  d <- d[as.character(d$CNT) == substr(gr, 1, 3), ]
  d <- d[match(as.numeric(s$sk$id), as.numeric(d$CNTSTUID)), ]
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  Tm <- matrix(NA_real_, nrow(d), J)
  ir <- paste0(uzd, "T") %in% names(d)
  Tm[, ir] <- vapply(d[paste0(uzd[ir], "T")], tirs_laiks, numeric(nrow(d)))
  dimnames(S) <- dimnames(Tm) <- list(NULL, uzd)
  x <- c(s, list(S = S, P = punkti_matrica(S), T = Tm,
                 V = !is.na(S) & S %in% VERTETAS, NS = !is.na(S) & S == "nesasniegts",
                 ADM = !is.na(S) & S != "nederigs"))
  if (g == "2018") x$diz <- data.frame(RDESIGN = as.numeric(d$RDESIGN),
                                       RCORE_TEST = as.numeric(d$RCORE_TEST),
                                       RCORE_PERF = as.numeric(d$RCORE_PERF))
  x
}
X <- lapply(GRUPAS, function(gr) list("2015" = dati("2015", gr), "2018" = dati("2018", gr)))
names(X) <- GRUPAS
M <- length(X[[1]][["2015"]]$pv)

agg <- function(Y, Xg, sel, w = Xg$sk$w) {
  W <- matrix(w, nrow(sel), ncol(sel)); 100 * sum(W[sel] * Y[sel]) / sum(W[sel]) }
maska <- function(Xg, kopa, rindas = rep(TRUE, nrow(Xg$V))) {
  s <- Xg$V & matrix(kopa, nrow(Xg$V), J, byrow = TRUE); s[!rindas, ] <- FALSE; s }
rep_se <- function(f, Xg) { e <- f(Xg$sk$w)
  sqrt(sum((vapply(Xg$rw, f, 1) - e)^2) / 20) }

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n1. Oficiālā pārmaiņa un kopīgo uzdevumu saņēmēji\n", strrep("=", 78), "\n\n")
cat(sprintf("  Kopīgi vērtēti uzdevumi 2015 un 2018: %d\n\n", J))
off <- list()
for (gr in GRUPAS) {
  o <- lapply(X[[gr]], function(Xg) rubin_brr(Xg$pv, Xg$sk$w, Xg$rw, VID))
  off[[gr]] <- c(d = o[["2018"]][["est"]] - o[["2015"]][["est"]],
                 se = sqrt(o[["2015"]][["se"]]^2 + o[["2018"]][["se"]]^2 + LE^2))
  sn <- vapply(X[[gr]], function(Xg) { n <- rowSums(Xg$V); w <- Xg$sk$w
    c(100 * sum(w[n > 0]) / sum(w), sum(w[n > 0] * n[n > 0]) / sum(w[n > 0])) }, numeric(2))
  cat(sprintf("  %-6s oficiālā pārmaiņa %+6.1f (%.1f) | saņēmēji 2015 %.0f %% (%.1f uzd.), 2018 %.0f %% (%.1f uzd.)\n",
              gr, off[[gr]][["d"]], off[[gr]][["se"]], sn[1, 1], sn[2, 1], sn[1, 2], sn[2, 2]))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n2. Vai kopīgie uzdevumi 2018. gadā ir kodolā vai posmos (LVA A)\n",
    strrep("=", 78), "\n\n")
X18 <- X[["LVA A"]][["2018"]]; dz <- X18$diz; w18 <- X18$sk$w
msat <- dz$RDESIGN %in% c(1, 2)
kl <- t(vapply(seq_len(J), function(j) {
  r <- X18$ADM[, j] & msat
  if (sum(r) < 10) return(c(NA, NA, NA))
  tb <- table(dz$RCORE_TEST[r])
  c(max(tb) / sum(tb), mean(dz$RCORE_PERF[r] == 1), mean(dz$RCORE_PERF[r] == 3))
}, numeric(3)))
kodols <- !is.na(kl[, 1]) & kl[, 1] >= 0.95
cat(sprintf("  MSAT skolēnu daļa: %.1f %%. Kodola uzdevumi (>= 95 %% saņēmēju vienā testletē): %d no %d.\n",
            100 * sum(w18[msat]) / sum(w18), sum(kodols), J))
cat(sprintf("  Saņēmēju daļa ar zemu kodola sniegumu pa uzdevumiem: %.0f-%.0f %% (visi: %.0f %%); ar augstu: %.0f-%.0f %% (visi: %.0f %%)\n",
            100 * min(kl[, 2], na.rm = TRUE), 100 * max(kl[, 2], na.rm = TRUE), 100 * mean(dz$RCORE_PERF[msat] == 1),
            100 * min(kl[, 3], na.rm = TRUE), 100 * max(kl[, 3], na.rm = TRUE), 100 * mean(dz$RCORE_PERF[msat] == 3)))

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n3. Saskaņotība ar skalu: novērotā pret paredzēto pārmaiņu\n",
    strrep("=", 78), "\n\n")
parametri <- function(X15, m) {
  th <- (X15$pv[[m]] - 500) / 100
  t(vapply(seq_len(J), function(j) {
    v <- X15$V[, j]
    if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- X15$sk$w[v]
    stats::coef(suppressWarnings(stats::glm(X15$P[v, j] ~ th[v], family = stats::quasibinomial(),
                                            weights = w / mean(w))))
  }, numeric(2)))
}
pmat <- function(Xg, B, m, nobide = 0) {
  th <- (Xg$pv[[m]] - nobide - 500) / 100
  stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
}
B <- lapply(GRUPAS, function(gr) lapply(seq_len(M), function(m) parametri(X[[gr]][["2015"]], m)))
names(B) <- GRUPAS
ok <- Reduce(`&`, lapply(B, function(b) !is.na(b[[1]][, 1])))
cat(sprintf("  Uzdevumi ar >= %d saņēmējiem 2015. gadā visās trijās grupās: %d no %d\n\n", MIN_SANEMEJI, sum(ok), J))

rez <- list(); slip <- list()
for (gr in GRUPAS) {
  X15 <- X[[gr]][["2015"]]; X18 <- X[[gr]][["2018"]]; Bg <- B[[gr]]
  s15 <- maska(X15, ok); s18 <- maska(X18, ok)
  o15 <- agg(X15$P, X15, s15); o18 <- agg(X18$P, X18, s18)
  p15 <- mean(vapply(seq_len(M), function(m) agg(pmat(X15, Bg[[m]], m), X15, s15), 1))
  p18 <- mean(vapply(seq_len(M), function(m) agg(pmat(X18, Bg[[m]], m), X18, s18), 1))
  sl <- mean(vapply(seq_len(M), function(m) agg(pmat(X18, Bg[[m]], m, -1), X18, s18) -
                                             agg(pmat(X18, Bg[[m]], m), X18, s18), 1))
  P1 <- pmat(X18, Bg[[1]], 1)
  se_atl <- sqrt(rep_se(function(w) agg(X15$P, X15, s15, w) - agg(pmat(X15, Bg[[1]], 1), X15, s15, w), X15)^2 +
                 rep_se(function(w) agg(X18$P, X18, s18, w) - agg(P1, X18, s18, w), X18)^2)
  atl <- (o18 - o15) - (p18 - p15)
  slip[[gr]] <- sl
  rez[[gr]] <- data.frame(grupa = gr, uzd = sum(ok), r1_2015 = o15, r1_2018 = o18,
    izm_pp = o18 - o15, paredz_pp = p18 - p15, atlikums_pp = atl, atlikums_se = se_atl,
    slipums_pp_uz_punktu = sl, atlikums_punkti = atl / sl, oficiala = off[[gr]][["d"]])
  cat(sprintf("  %-6s R1 %.2f -> %.2f: %+5.2f pp | paredzēts %+5.2f pp | atlikums %+5.2f pp (%.2f)%s ≈ %+5.1f punkti\n",
              gr, o15, o18, o18 - o15, p18 - p15, atl, se_atl, zv(atl, se_atl), atl / sl))
}
rez <- do.call(rbind, rez)
sal <- mean(rez$atlikums_pp[rez$grupa %in% SALID])
lsp <- rez$atlikums_pp[rez$grupa == "LVA A"] - sal
lsp_se <- sqrt(rez$atlikums_se[rez$grupa == "LVA A"]^2 + mean(rez$atlikums_se[rez$grupa %in% SALID]^2) / 2)
cat(sprintf("\n  Latvijai specifiskais atlikums (LVA A - EST/LTU vidējais): %+.2f pp (SE %.2f)%s ≈ %+.1f punkti\n",
            lsp, lsp_se, zv(lsp, lsp_se), lsp / slip[["LVA A"]]))

# Vērtēšanas veids: ja Latvijas vērtētāji 2018. gadā atvērtos uzdevumus vērtētu
# maigāk nekā 2015. gadā, atlikums būtu tikai vērtētāju kodētajos (DR...C).
atl_kopa <- function(gr, kp) {
  X15 <- X[[gr]][["2015"]]; X18 <- X[[gr]][["2018"]]; Bg <- B[[gr]]
  s15 <- maska(X15, kp); s18 <- maska(X18, kp)
  o <- agg(X18$P, X18, s18) - agg(X15$P, X15, s15)
  p <- mean(vapply(seq_len(M), function(m) agg(pmat(X18, Bg[[m]], m), X18, s18) -
                                            agg(pmat(X15, Bg[[m]], m), X15, s15), 1))
  P15 <- pmat(X15, Bg[[1]], 1); P18 <- pmat(X18, Bg[[1]], 1)
  se <- sqrt(rep_se(function(w) agg(X15$P, X15, s15, w) - agg(P15, X15, s15, w), X15)^2 +
             rep_se(function(w) agg(X18$P, X18, s18, w) - agg(P18, X18, s18, w), X18)^2)
  c(atl = o - p, se = se)
}
kodets <- grepl("C$", U$avots18[sub("^CR", "", uzd)])
cat("\n  Atlikums pēc vērtēšanas veida:\n")
veids <- list()
for (tips in c("automātiski vērtēti", "vērtētāju kodēti")) {
  kp <- ok & (if (tips == "vērtētāju kodēti") kodets else !kodets)
  v <- vapply(GRUPAS, function(gr) atl_kopa(gr, kp), numeric(2))
  veids[[tips]] <- data.frame(veids = tips, uzd = sum(kp), grupa = GRUPAS, atlikums_pp = v[1, ], se = v[2, ])
  cat(sprintf("  %-19s (%2d uzd.): %s\n", tips, sum(kp),
              paste(sprintf("%s %+.2f (%.2f)", GRUPAS, v[1, ], v[2, ]), collapse = " | ")))
}
utils::write.csv(do.call(rbind, veids), "output/pisa2025/msat_2018_veids.csv", row.names = FALSE)

# Uzdevumu klasifikācija no PISA 2015 tehniskā ziņojuma A.3 tabulas
# (py/tr2015_a3_klasifikacija.py): vai Latvijai specifiskais atlikums ir kādā
# konkrētā uzdevumu veidā, vai visur līdzīgs (tad tā ir skalas līmeņa nobīde).
KL <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2015.csv", stringsAsFactors = FALSE)
cat(sprintf("\n  Klasifikācija atrasta %d no %d uzdevumiem.\n", sum(uzd %in% KL$uzd), J))
for (k in c("aspekts", "teksta_formats")) {
  z <- KL[[k]][match(uzd, KL$uzd)]
  cat(sprintf("\n  Atlikums pēc: %s (LVA A | EST | LTU | Latvijai specifiskais)\n", k))
  for (v in sort(unique(stats::na.omit(z)))) {
    kp <- ok & z %in% v
    if (sum(kp) < 4) next
    a <- vapply(c("LVA A", SALID), function(gr) atl_kopa(gr, kp), numeric(2))
    d <- a[1, 1] - mean(a[1, -1]); s <- sqrt(a[2, 1]^2 + mean(a[2, -1]^2) / 2)
    cat(sprintf("  %-24s (%2d uzd.) %+5.2f | %+5.2f | %+5.2f | %+5.2f (%.2f)%s\n",
                v, sum(kp), a[1, 1], a[1, 2], a[1, 3], d, s, zv(d, s)))
  }
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n4. Atlikums pa spēju kvintilēm\n", strrep("=", 78), "\n\n")
kv <- list()
for (gr in GRUPAS) {
  Bg <- B[[gr]]
  acc <- array(0, c(2, M, 5, 3))
  for (gi in 1:2) {
    Xg <- X[[gr]][[c("2015", "2018")[gi]]]
    for (m in seq_len(M)) {
      th <- Xg$pv[[m]]; b <- findInterval(th, wq(th, Xg$sk$w, c(0.2, 0.4, 0.6, 0.8)), left.open = TRUE) + 1
      P0 <- pmat(Xg, Bg[[m]], m); P1 <- pmat(Xg, Bg[[m]], m, -1)
      for (q in 1:5) { s <- maska(Xg, ok, b == q)
        acc[gi, m, q, ] <- c(agg(Xg$P, Xg, s), agg(P0, Xg, s), agg(P1, Xg, s) - agg(P0, Xg, s)) }
    }
  }
  a <- apply(acc, c(1, 3, 4), mean)
  for (q in 1:5) {
    d <- (a[2, q, 1] - a[2, q, 2]) - (a[1, q, 1] - a[1, q, 2])
    kv[[length(kv) + 1]] <- data.frame(grupa = gr, kvintile = q, atl_2015 = a[1, q, 1] - a[1, q, 2],
                                       atl_2018 = a[2, q, 1] - a[2, q, 2], starpiba_pp = d,
                                       punkti = d / a[2, q, 3])
  }
}
kv <- do.call(rbind, kv)
kt <- stats::reshape(kv[, c("grupa", "kvintile", "starpiba_pp")], idvar = "kvintile",
                     timevar = "grupa", direction = "wide")
names(kt) <- sub("starpiba_pp.", "", names(kt))
kt$LVA_specif <- kt[["LVA A"]] - (kt$EST + kt$LTU) / 2
pk <- kv$punkti[kv$grupa == "LVA A"] / kv$starpiba_pp[kv$grupa == "LVA A"]
kt$LVA_specif_punkti <- kt$LVA_specif * pk
cat("  Atlikums 2018 mīnus atlikums 2015 (pp), pa kvintilēm katras grupas iekšienē:\n\n")
print(format(kt, digits = 3), row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n5. Iesaiste 2015 un 2018\n", strrep("=", 78), "\n\n")
for (gr in GRUPAS) {
  X15 <- X[[gr]][["2015"]]; X18 <- X[[gr]][["2018"]]
  irT <- colSums(!is.na(X15$T)) > 0 & colSums(!is.na(X18$T)) > 0
  sl <- rep(NA_real_, J); sl[irT] <- pmin(0.10 * apply(X15$T[, irT, drop = FALSE], 2, function(x) mean(x, na.rm = TRUE)), 10000)
  r13 <- list()
  for (g in c("2015", "2018")) {
    Xg <- X[[gr]][[g]]; w <- Xg$sk$w; W <- matrix(w, nrow(Xg$S), J)
    A <- sweep(Xg$T, 2, sl, "<"); A[is.na(A)] <- FALSE; A <- A & Xg$V
    VT <- maska(Xg, irT & ok)
    nV <- rowSums(VT); r <- ifelse(nV > 0, rowSums(A & VT) / pmax(nV, 1), NA); san <- !is.na(r)
    r13[[g]] <- c(agg(Xg$P, Xg, VT), agg(Xg$P, Xg, VT & !A))
    cat(sprintf("  %-6s %s  nesasniegti %.2f %% | ātrās %.2f %% | skolēni ≥ 10 %% ātru %.1f %%\n",
                gr, g, 100 * sum(W[Xg$NS]) / sum(W[Xg$ADM]), 100 * sum(W[A & VT]) / sum(W[VT]),
                100 * sum(w[san & r >= 0.10]) / sum(w[san])))
  }
  cat(sprintf("  %-6s R1 pārmaiņa %+.2f pp, bez ātrajām %+.2f pp, starpība %+.2f pp\n\n", gr,
              r13[["2018"]][1] - r13[["2015"]][1], r13[["2018"]][2] - r13[["2015"]][2],
              (r13[["2018"]][2] - r13[["2015"]][2]) - (r13[["2018"]][1] - r13[["2015"]][1])))
}

utils::write.csv(rez, "output/pisa2025/msat_2018.csv", row.names = FALSE)
utils::write.csv(kv, "output/pisa2025/msat_2018_kvintiles.csv", row.names = FALSE)
kor <- korekcijas_rinda("2018", "A", "MSAT 2018: Latvijai specifiskais uzdevumu atlikums",
  lsp / slip[["LVA A"]], (lsp - 1.96 * lsp_se) / slip[["LVA A"]], (lsp + 1.96 * lsp_se) / slip[["LVA A"]],
  paste("2015. gada uzdevumu parametri paredz 2018. gada rezultātu no 2018. gada PV; Latvijas A",
        "plūsmas atlikums mīnus EST/LTU vidējais, pārrēķināts punktos ar lokālo slīpumu;",
        "pozitīvs nozīmē, ka 2018. gada skala ir zemāka par trenda uzdevumu ainu; nobīde",
        "saglabājas 2022 un 2025 (53b), tāpēc attiecas uz visiem 2018-2025 līmeņiem, salīdzinot",
        "ar 2015 un agrāk; 2018-2025 pārmaiņas neietekmē"))
utils::write.csv(kor, "output/pisa2025/korekcijas/07_msat.csv", row.names = FALSE)
cat("Izveidotie faili:\n  output/pisa2025/msat_2018.csv\n  output/pisa2025/msat_2018_kvintiles.csv\n",
    " output/pisa2025/korekcijas/07_msat.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
