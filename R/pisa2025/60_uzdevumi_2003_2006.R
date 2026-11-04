# ============================================================================
# 60. Uzdevumu un skalas saskaņotība 2003 -> 2006, latviešu valodas plūsma
#     (docs/57, E grupa; papildina 59. skriptu)
# ============================================================================
#
# JAUTĀJUMS. A plūsmas oficiālā pārmaiņa 2003 -> 2006 ir −16,1 (ar oficiālo saites
# kļūdu tikko nozīmīga). Vai tas ir Latvijai specifisks instrumenta efekts, kā
# 2018. gadā (docs/55), vai tā pati sakarība starp uzdevumiem un skalu?
#
# PROBLĒMA. 2003. gada uzdevumu failā ir neapstrādātas atbildes, nevis punkti:
# izvēlētais variants vai vērtētāja kods; n = nav uzdots, r = nav sasniegts,
# 8 = nederīgs, 9 = neatbildēts. Vērtēšanas atslēga tiek atjaunota no datiem:
#   - atbildētāju kodi 0-6; katram kodam svērtais vidējais PV (visas valstis kopā,
#     katra valsts sver vienādi);
#   - kods ar augstāko vidējo PV = pilns punkts;
#   - ja uzdevums 2006. gadā vērtēts ar 0/1/2, otrs augstākais kods = daļējs punkts;
#   - pārējie kodi, 8 un 9 = 0; r = nav sasniegts (neskaita); n = nav uzdots.
# PĀRBAUDE: atjaunotās 2003. gada izpildes daļas pret 2006. gada daļām tiem pašiem
# uzdevumiem katrā valstī (korelācija pa uzdevumiem), un salīdzināmo valstu
# atlikumiem jābūt tuvu nullei.
#
# SALĪDZINĀJUMAM: Polija, Somija, Čehija, Ungārija, Vācija (Igaunija un Lietuva
# 2003. gadā nepiedalījās). A plūsma: LANGN 428001 (2003), 325 (2006).
#
# Izvade:  output/pisa2025/uzdevumi_2003_2006.csv, uzdevumi_2003_atslega.csv
#          output/pisa2025/logs/60_uzdevumi_2003_2006_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/60_uzdevumi_2003_2006_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

SAL <- c("POL", "FIN", "CZE", "HUN", "DEU")
VALSTIS <- c("LVA", SAL)
M <- 5; MIN_SANEMEJI <- 30
atsl <- function(x) sub("T$", "", sub("^P?R", "", x))
zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "

nolasa <- function(g) {
  k <- utils::read.csv(sprintf("data/pisa_veci/kog_%s.csv", g), colClasses = "character", check.names = FALSE)
  s <- utils::read.csv(sprintf("data/pisa_veci/sal_%s.csv", g), colClasses = "character", check.names = FALSE)
  k <- k[k$CNT %in% VALSTIS, ]
  s <- s[match(paste(k$CNT, k$SCHOOLID, k$STIDSTD), paste(s$CNT, s$SCHOOLID, s$STIDSTD)), ]
  it <- grep("^P?R[0-9]{3}Q", names(k), value = TRUE)
  raw <- as.matrix(k[, it]); colnames(raw) <- atsl(it)
  pv <- lapply(paste0("PV", 1:5, "READ"), function(p) as.numeric(s[[p]]))
  list(cnt = k$CNT, lang = trimws(s$LANGN), raw = raw, w = as.numeric(s$W_FSTUWT), pv = pv,
       pvm = Reduce(`+`, pv) / 5, rw = lapply(paste0("W_FSTR", 1:80), function(p) as.numeric(s[[p]])))
}
R03 <- nolasa("2003"); R06 <- nolasa("2006")
uzd <- intersect(colnames(R03$raw), colnames(R06$raw))
cat("Kopīgi uzdevumi 2003 un 2006:", length(uzd), "\n")
cat("2003 skolēni pa valstīm:", paste(names(table(R03$cnt)), table(R03$cnt), sep = ":", collapse = " "), "\n\n")

# ---------------------------------------------------------------------------
# Vērtēšanas atslēgas atjaunošana 2003. gadam
# ---------------------------------------------------------------------------
cat(strrep("=", 90), "\n1. Atjaunotā vērtēšanas atslēga 2003\n", strrep("=", 90), "\n\n")
wv <- R03$w / ave(R03$w, R03$cnt, FUN = sum)          # katra valsts sver vienādi
max06 <- vapply(uzd, function(j) max(suppressWarnings(as.numeric(R06$raw[, j])) %in% 2), 1)
atslega <- list(); SC03 <- matrix(NA_real_, nrow(R03$raw), length(uzd), dimnames = list(NULL, uzd))
for (j in uzd) {
  x <- R03$raw[, j]
  kodi <- sort(unique(x[x %in% as.character(0:6)]))
  mv <- vapply(kodi, function(k) { i <- x == k; sum(wv[i] * R03$pvm[i]) / sum(wv[i]) }, 1)
  nk <- vapply(kodi, function(k) sum(x == k), 1)
  mv[nk < MIN_SANEMEJI] <- -Inf
  o <- order(mv, decreasing = TRUE)
  pilns <- kodi[o[1]]; dalejs <- if (max06[[j]] == 1 && length(o) > 1) kodi[o[2]] else NA
  s <- rep(NA_real_, length(x))
  s[x %in% c(as.character(0:6), "8", "9")] <- 0
  s[x == pilns] <- 1
  if (!is.na(dalejs)) s[x == dalejs] <- 0.5
  s[x == "r"] <- -8
  SC03[, j] <- s
  atstarpe <- if (length(o) > 1) mv[o[1]] - mv[o[2]] else NA
  atslega[[j]] <- data.frame(uzd = j, kodi = paste(kodi, collapse = ","), pilns = pilns,
    dalejs = ifelse(is.na(dalejs), "", dalejs), max2006 = ifelse(max06[[j]] == 1, 2, 1),
    vid_pv_pilns = mv[o[1]], atstarpe_pret_nakamo = atstarpe)
}
atslega <- do.call(rbind, atslega)
print(format(atslega, digits = 4), row.names = FALSE)
utils::write.csv(atslega, "output/pisa2025/uzdevumi_2003_atslega.csv", row.names = FALSE)

# 2006: vērtēts (0/1/2 punkti; 7 nav uzdots; 8 nav sasniegts; 9 neatbildēts)
K06 <- suppressWarnings(matrix(as.numeric(R06$raw[, uzd]), nrow(R06$raw), dimnames = list(NULL, uzd)))
SC06 <- K06; SC06[K06 %in% 9] <- 0
SC06 <- sweep(SC06, 2, ifelse(max06 == 1, 2, 1), "/")
SC06[K06 %in% 8] <- -8; SC06[!(K06 %in% c(0, 1, 2, 8, 9))] <- NA

# Pārbaude: izpildes daļas pa uzdevumiem 2003 pret 2006
cat("\n  Pārbaude: izpildes daļa pa uzdevumiem 2003 pret 2006 (korelācija pa uzdevumiem)\n")
pd <- function(SC, R, c0) { i <- R$cnt == c0
  vapply(uzd, function(j) { v <- i & !is.na(SC[, j]) & SC[, j] >= 0
    sum(R$w[v] * SC[v, j]) / sum(R$w[v]) }, 1) }
for (c0 in VALSTIS) {
  a <- pd(SC03, R03, c0); b <- pd(SC06, R06, c0); v <- is.finite(a) & is.finite(b)
  cat(sprintf("    %s: r = %.3f, vidējā izpilde 2003 %.3f, 2006 %.3f, lielākā starpība uzdevumā %.2f (%s)%s\n",
              c0, stats::cor(a[v], b[v]), mean(a[v]), mean(b[v]), max(abs(a - b)[v]), uzd[v][which.max(abs(a - b)[v])],
              if (any(!v)) paste0("; nav uzdots: ", paste(uzd[!v], collapse = ", ")) else ""))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 90), "\n2. Saskaņotība ar skalu 2003 -> 2006\n", strrep("=", 90), "\n\n")
grupa <- function(R, SC, gr, acode) {
  i <- if (gr == "LVA A") R$cnt == "LVA" & R$lang == acode else if (gr == "LVA") R$cnt == "LVA" else R$cnt == gr
  i[is.na(i)] <- FALSE
  S <- SC[i, , drop = FALSE]; V <- !is.na(S) & S >= 0
  P <- S; P[!V] <- NA
  list(P = P, V = V, w = R$w[i], pv = lapply(R$pv, `[`, i), rw = lapply(R$rw, `[`, i))
}
agg <- function(Y, X, sel, w = X$w) { W <- matrix(w, nrow(sel), ncol(sel)); 100 * sum(W[sel] * Y[sel]) / sum(W[sel]) }
rep_se <- function(f, X) { e <- f(X$w); sqrt(sum((vapply(X$rw, f, 1) - e)^2) / 20) }
parametri <- function(X, m) {
  th <- (X$pv[[m]] - 500) / 100
  t(vapply(seq_along(uzd), function(j) {
    v <- X$V[, j]; if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- X$w[v]
    stats::coef(suppressWarnings(stats::glm(X$P[v, j] ~ th[v], family = stats::quasibinomial(), weights = w / mean(w))))
  }, numeric(2)))
}
pmat <- function(X, B, m, nobide = 0) {
  th <- (X$pv[[m]] - nobide - 500) / 100
  stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
}
GRUPAS <- c("LVA A", "LVA", SAL)
XA <- lapply(GRUPAS, function(gr) grupa(R03, SC03, gr, "428001")); names(XA) <- GRUPAS
XB <- lapply(GRUPAS, function(gr) grupa(R06, SC06, gr, "325")); names(XB) <- GRUPAS
B <- lapply(GRUPAS, function(gr) lapply(seq_len(M), function(m) parametri(XA[[gr]], m))); names(B) <- GRUPAS
ok <- Reduce(`&`, lapply(B, function(bb) !is.na(bb[[1]][, 1]))) &
      Reduce(`&`, lapply(XB, function(X) colSums(X$V) >= MIN_SANEMEJI))
cat("  modelī uzdevumi:", sum(ok), "no", length(uzd), "; izslēgti:", paste(uzd[!ok], collapse = ", "), "\n\n")
saskanotiba <- function(ok, druka = TRUE) {
rez <- list()
for (gr in GRUPAS) {
  xa <- XA[[gr]]; xb <- XB[[gr]]; Bg <- B[[gr]]
  sa <- xa$V & matrix(ok, nrow(xa$V), length(uzd), byrow = TRUE)
  sb <- xb$V & matrix(ok, nrow(xb$V), length(uzd), byrow = TRUE)
  oa <- agg(xa$P, xa, sa); ob <- agg(xb$P, xb, sb)
  pa <- mean(vapply(seq_len(M), function(m) agg(pmat(xa, Bg[[m]], m), xa, sa), 1))
  pb <- mean(vapply(seq_len(M), function(m) agg(pmat(xb, Bg[[m]], m), xb, sb), 1))
  sl <- mean(vapply(seq_len(M), function(m) agg(pmat(xb, Bg[[m]], m, -1), xb, sb) - agg(pmat(xb, Bg[[m]], m), xb, sb), 1))
  Pa1 <- pmat(xa, Bg[[1]], 1); Pb1 <- pmat(xb, Bg[[1]], 1)
  se <- sqrt(rep_se(function(w) agg(xa$P, xa, sa, w) - agg(Pa1, xa, sa, w), xa)^2 +
             rep_se(function(w) agg(xb$P, xb, sb, w) - agg(Pb1, xb, sb, w), xb)^2)
  off <- mean(vapply(xb$pv, function(v) sum(v * xb$w) / sum(xb$w), 1)) -
         mean(vapply(xa$pv, function(v) sum(v * xa$w) / sum(xa$w), 1))
  atl <- (ob - oa) - (pb - pa)
  rez[[gr]] <- data.frame(grupa = gr, uzd = sum(ok), oficiala = off, izm_pp = ob - oa, paredz_pp = pb - pa,
                          atlikums_pp = atl, se = se, slipums = sl, punkti = atl / sl)
  if (druka) cat(sprintf("  %-6s oficiālā %+6.1f | R1 %+5.2f pp, paredzēts %+5.2f pp | atlikums %+5.2f pp (%.2f)%s ≈ %+5.1f punkti\n",
              gr, off, ob - oa, pb - pa, atl, se, zv(atl, se), atl / sl))
}
rez <- do.call(rbind, rez)
lv <- rez[rez$grupa == "LVA A", ]; sal <- rez[rez$grupa %in% SAL, ]
d <- lv$atlikums_pp - mean(sal$atlikums_pp); s <- sqrt(lv$se^2 + mean(sal$se^2) / nrow(sal))
cat(sprintf("%s  Latvijai specifiskais atlikums (A plūsma − 5 valstu vidējais): %+.2f pp (SE %.2f)%s ≈ %+.1f punkti\n",
            if (druka) "\n" else "", d, s, zv(d, s), d / lv$slipums))
cat(sprintf("  Salīdzināmo valstu atlikumu diapazons: %+.2f … %+.2f pp\n", min(sal$atlikums_pp), max(sal$atlikums_pp)))
rez$latv_specif_pp <- d; rez$latv_specif_se <- s; rez$latv_specif_punkti <- d / lv$slipums
rez
}
rez <- saskanotiba(ok)
rez$variants <- "visi"

# Jutīgums: tikai uzdevumi, kuros atslēga ir skaidra (pilnā koda vidējais PV par
# vismaz 30 punktiem augstāks nekā nākamajam kodam)
skaidri <- atslega$uzd[atslega$atstarpe_pret_nakamo >= 30 | is.na(atslega$atstarpe_pret_nakamo)]
ok2 <- ok & uzd %in% skaidri
cat(sprintf("\n  Jutīgums: tikai uzdevumi ar skaidru atslēgu (atstarpe >= 30 punkti), %d uzdevumi; izslēgti: %s\n",
            sum(ok2), paste(uzd[!ok2], collapse = ", ")))
rez2 <- saskanotiba(ok2, druka = FALSE)
cat(sprintf("  LVA A atlikums %+.2f pp (%.2f)\n", rez2$atlikums_pp[1], rez2$se[1]))
rez2$variants <- "skaidra_atslega"
rez <- rbind(rez, rez2)
utils::write.csv(rez, "output/pisa2025/uzdevumi_2003_2006.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/uzdevumi_2003_2006.csv\n  output/pisa2025/uzdevumi_2003_atslega.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
