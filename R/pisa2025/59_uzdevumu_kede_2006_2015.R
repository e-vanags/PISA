# ============================================================================
# 59. Uzdevumu un skalas saskaņotības ķēde 2006 -> 2009 -> 2012 -> 2015,
#     latviešu valodas plūsma pret septiņām valstīm (docs/57, E grupa)
# ============================================================================
#
# JAUTĀJUMS. Vai pirms 2018. gada ir Latvijai specifiskas instrumenta nobīdes,
# tāpat kā 2018. gadā (docs/55)? Īpaši 2015. gadā, kad PISA pārgāja no papīra uz
# datoru. Ja nobīdes nav, oficiālā 2006-2015 A plūsmas rinda ir salīdzināma, un
# stabilitāte ir reāla.
#
# METODE kā 53. skriptā: katram kopīgajam uzdevumam sākuma ciklā frakcionālais
# logit punkti ~ PV (katrai grupai un PV atsevišķi, 5 PV); ar tiem paredz beigu
# cikla rezultātu no beigu cikla faktisko saņēmēju PV. Atlikums = novērotā
# pārmaiņa − paredzētā. Latvijai specifiskais = A plūsmas atlikums − septiņu
# valstu vidējais atlikums. Nesasniegtie uzdevumi netiek vērtēti (R1).
#
# KODI vecajos failos: 0/1/2 punkti (normalizēti ar uzdevuma maksimumu), 7 nav
# uzdots, 8 nav sasniegts, 9 neatbildēts (0). 2015: etiķetes kā 00_kopigas_funkcijas.R.
# A plūsma: LANGN 325. Salīdzinājumam visa valsts.
#
# Izvade:  output/pisa2025/uzdevumu_kede_2006_2015.csv
#          output/pisa2025/logs/59_uzdevumu_kede_output.txt
# ============================================================================

source("R/pisa2025/00_kopigas_funkcijas.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/59_uzdevumu_kede_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
GRUPAS <- c("LVA A", SAL)
M <- 5; MIN_SANEMEJI <- 30
atsl <- function(x) sub("T$", "", sub("^(P?R|CR|DR)", "", sub("(S|C)$", "", x)))
zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "

# Vecie cikli: uzdevumi un skolēni no py/kog_veci_izvilkums.py
vecais <- function(g) {
  k <- utils::read.csv(sprintf("data/pisa_veci/kog_%s.csv", g), colClasses = "character", check.names = FALSE)
  s <- utils::read.csv(sprintf("data/pisa_veci/sal_%s.csv", g), colClasses = "character", check.names = FALSE)
  s <- s[match(paste(k$CNT, k$SCHOOLID, k$STIDSTD), paste(s$CNT, s$SCHOOLID, s$STIDSTD)), ]
  it <- grep("^P?R[0-9]{3}Q", names(k), value = TRUE)
  raw <- as.matrix(k[, it]); colnames(raw) <- atsl(it)
  kods <- suppressWarnings(matrix(as.numeric(raw), nrow(raw), dimnames = dimnames(raw)))
  list(cnt = k$CNT, lang = trimws(s$LANGN), kods = kods, w = as.numeric(s$W_FSTUWT),
       pv = lapply(paste0("PV", 1:5, "READ"), function(p) as.numeric(s[[p]])),
       rw = lapply(paste0("W_FSTR", 1:80), function(p) as.numeric(s[[p]])))
}
jauns15 <- function() {
  U <- readRDS("data/pisa2025/processed/uzdevumi_2015_salid.rds")
  st <- U$stu[match(as.numeric(U$kog$CNTSTUID), U$stu$CNTSTUID), ]
  it <- unname(U$karte[!is.na(U$karte)])
  S <- vapply(U$kog[it], klasificet, character(nrow(U$kog)))
  P <- punkti_matrica(S)
  # Indeksu piešķiršana saglabā matricas izmērus (ifelse ar %in% tos pazaudētu).
  kods <- P
  kods[S %in% "nesasniegts"] <- -8
  kods[!(S %in% c("pilns", "dalejs", "nulle", "neatbildets", "nesasniegts"))] <- NA
  colnames(kods) <- names(U$karte)[!is.na(U$karte)]
  list(cnt = as.character(U$kog$CNT), lang = as.character(st$LANGN), kods = kods, norm = TRUE,
       w = st$W_FSTUWT, pv = lapply(paste0("PV", 1:M, "READ"), function(p) st[[p]]),
       rw = lapply(paste0("W_FSTURWT", 1:80), function(p) st[[p]]))
}
C <- list("2006" = vecais("2006"), "2009" = vecais("2009"), "2012" = vecais("2012"), "2015" = jauns15())

# Grupas dati: punkti 0-1 (P), vērtētās atbildes (V), PV un svari
grupa <- function(Cg, gr, uzd, maxs) {
  i <- if (gr == "LVA A") Cg$cnt == "LVA" & Cg$lang == "325" else Cg$cnt == gr
  i[is.na(i)] <- FALSE
  K <- Cg$kods[i, uzd, drop = FALSE]
  if (isTRUE(Cg$norm)) { V <- !is.na(K) & K >= 0; P <- ifelse(V, K, NA) }
  else { V <- !is.na(K) & matrix(K %in% c(0, 1, 2, 9), nrow(K))
         P <- K; P[K %in% 9] <- 0
         P <- sweep(P, 2, maxs, "/"); P[!V] <- NA }
  list(P = P, V = V, w = Cg$w[i], pv = lapply(Cg$pv, `[`, i), rw = lapply(Cg$rw, `[`, i))
}
agg <- function(Y, X, sel, w = X$w) { W <- matrix(w, nrow(sel), ncol(sel)); 100 * sum(W[sel] * Y[sel]) / sum(W[sel]) }
rep_se <- function(f, X) { e <- f(X$w); sqrt(sum((vapply(X$rw, f, 1) - e)^2) / 20) }
parametri <- function(X, m, J) {
  th <- (X$pv[[m]] - 500) / 100
  t(vapply(seq_len(J), function(j) {
    v <- X$V[, j]; if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- X$w[v]
    stats::coef(suppressWarnings(stats::glm(X$P[v, j] ~ th[v], family = stats::quasibinomial(), weights = w / mean(w))))
  }, numeric(2)))
}
pmat <- function(X, B, m, nobide = 0) {
  th <- (X$pv[[m]] - nobide - 500) / 100
  stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
}

PARI <- list(c("2006", "2009"), c("2009", "2012"), c("2012", "2015"))
rez <- list()
for (p in PARI) {
  a <- C[[p[1]]]; b <- C[[p[2]]]
  uzd <- intersect(colnames(a$kods), colnames(b$kods))
  maxs <- pmax(apply(a$kods[, uzd, drop = FALSE], 2, function(x) max(x[x %in% 0:2], na.rm = TRUE)),
               apply(if (isTRUE(b$norm)) matrix(1, 1, length(uzd)) else b$kods[, uzd, drop = FALSE], 2,
                     function(x) max(x[x %in% 0:2], na.rm = TRUE)))
  maxs[!is.finite(maxs) | maxs == 0] <- 1
  cat(strrep("=", 90), "\n", p[1], "->", p[2], ": kopīgi uzdevumi", length(uzd), "\n", strrep("=", 90), "\n")
  XA <- lapply(GRUPAS, function(gr) grupa(a, gr, uzd, maxs)); names(XA) <- GRUPAS
  XB <- lapply(GRUPAS, function(gr) grupa(b, gr, uzd, maxs)); names(XB) <- GRUPAS
  B <- lapply(GRUPAS, function(gr) lapply(seq_len(M), function(m) parametri(XA[[gr]], m, length(uzd))))
  names(B) <- GRUPAS
  ok <- Reduce(`&`, lapply(B, function(bb) !is.na(bb[[1]][, 1])))
  ok <- ok & Reduce(`&`, lapply(XB, function(X) colSums(X$V) >= MIN_SANEMEJI))
  cat("  modelī uzdevumi:", sum(ok), "\n")
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
    off <- mean(vapply(xb$pv, function(v) sum(v * xb$w) / sum(xb$w), 1)) - mean(vapply(xa$pv, function(v) sum(v * xa$w) / sum(xa$w), 1))
    atl <- (ob - oa) - (pb - pa)
    rez[[length(rez) + 1]] <- data.frame(no = p[1], lidz = p[2], grupa = gr, uzd = sum(ok), oficiala = off,
      izm_pp = ob - oa, paredz_pp = pb - pa, atlikums_pp = atl, se = se, slipums = sl, punkti = atl / sl)
    cat(sprintf("  %-6s oficiālā %+6.1f | R1 %+5.2f pp, paredzēts %+5.2f pp | atlikums %+5.2f pp (%.2f)%s ≈ %+5.1f punkti\n",
                gr, off, ob - oa, pb - pa, atl, se, zv(atl, se), atl / sl))
  }
  r <- do.call(rbind, rez); r <- r[r$no == p[1], ]
  lv <- r[r$grupa == "LVA A", ]; sal <- r[r$grupa %in% SAL, ]
  d <- lv$atlikums_pp - mean(sal$atlikums_pp); s <- sqrt(lv$se^2 + mean(sal$se^2) / nrow(sal))
  cat(sprintf("\n  Latvijai specifiskais atlikums (A plūsma − 7 valstu vidējais): %+.2f pp (SE %.2f)%s ≈ %+.1f punkti\n",
              d, s, zv(d, s), d / lv$slipums))
  cat(sprintf("  Salīdzināmo valstu atlikumu diapazons: %+.2f … %+.2f pp\n\n", min(sal$atlikums_pp), max(sal$atlikums_pp)))
}
utils::write.csv(do.call(rbind, rez), "output/pisa2025/uzdevumu_kede_2006_2015.csv", row.names = FALSE)
cat("Izveidots: output/pisa2025/uzdevumu_kede_2006_2015.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
