# ============================================================================
# 53b. Uzdevumu un skalas saskaņotības ķēde 2018 -> 2022 -> 2025,
#      Latvijas A plūsma pret Igauniju (docs/51, 7. solis)
# ============================================================================
#
# JAUTĀJUMS. 53. skripts atrada, ka 2015 -> 2018 Latvijas A plūsmas rezultāts
# 77 trenda uzdevumos ir par apm. 2 pp (≈ 10 punkti) labāks, nekā paredz 2018.
# gada skala, bet Igaunijā un Lietuvā nesakritības nav. Vai šī Latvijas novirze
# pēc 2018. gada paliek (tad tā ietekmē tikai 2015 -> 2018 soli) vai atgriežas?
#
# METODE kā 51. un 53. skriptā: katram kopīgajam uzdevumam sākuma gadā
# frakcionālais logit punkti ~ PV (katrai PV atsevišķi, katrai grupai savi
# parametri); ar tiem paredz beigu gada rezultātu no beigu gada faktisko
# saņēmēju PV. Atlikums = novērotā R1 pārmaiņa - paredzētā. Uzdevumi: 196, kas
# ir 2022. un 2025. gadā (visi ir arī 2018. gadā). 2022. un 2025. gada uzdevumu
# kešā ir tikai LVA un EST, tāpēc salīdzinājums ir ar Igauniju.
#
# Izvade:  output/pisa2025/saskanotibas_kede.csv
#          output/pisa2025/logs/53b_saskanotibas_kede_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")
source("R/pisa2025/00_kopigas_funkcijas.R")
# haven netiek pieslēgts ar library(): tad as.numeric() ielādētājā uz marķētiem
# teksta ID (CNTSCHID) neizdodas, tāpēc tikai haven::read_sav.

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/53b_saskanotibas_kede_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
LE <- LE_TAB$LE[LE_TAB$joma == "READ" & LE_TAB$no == 2022 & LE_TAB$uz == 2025]   # 1,094 (skriptā pašlaik nelieto); OECD 2025 Vol. I, atvasināts, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md
MIN_SANEMEJI <- 30
GRUPAS <- c("LVA A", "EST")
U22 <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds")
uzd <- U22$uzd; J <- length(uzd)

K18 <- "data/pisa2025/processed/uzdevumi_2018_196.rds"
if (!file.exists(K18)) {
  d <- haven::read_sav("data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav", user_na = TRUE,
                col_select = tidyselect::all_of(c("CNT", "CNTSTUID", paste0(uzd, "S"))))
  saveRDS(d[as.character(d$CNT) %in% c("LVA", "EST"), ], K18)
}
DATI <- list("2018" = readRDS(K18), "2022" = U22$d22, "2025" = U22$d25)
D <- ab_dati()
KESA <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
zv <- function(d, s) if (!is.na(s) && s > 0 && abs(d / s) > 1.96) "*" else " "

skoleni <- function(g, gr) {
  if (gr == "LVA A") return(ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A"))
  d <- KESA[[g]]; d <- d[as.character(d$CNT) == gr, ]
  list(gads = g, sk = data.frame(id = as.numeric(d$CNTSTUID), w = as.numeric(d$W_FSTUWT)),
       pv = lapply(paste0("PV", 1:10, "READ"), function(k) as.numeric(d[[k]])),
       rw = lapply(paste0("W_FSTURWT", 1:80), function(k) as.numeric(d[[k]])))
}
dati <- function(g, gr) {
  s <- skoleni(g, gr)
  d <- DATI[[g]]; d <- d[as.character(d$CNT) == substr(gr, 1, 3), ]
  d <- d[match(as.numeric(s$sk$id), as.numeric(d$CNTSTUID)), ]
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  dimnames(S) <- list(NULL, uzd)
  c(s, list(S = S, P = punkti_matrica(S), V = !is.na(S) & S %in% VERTETAS))
}
GADI <- c("2018", "2022", "2025")
X <- lapply(GRUPAS, function(gr) stats::setNames(lapply(GADI, dati, gr = gr), GADI))
names(X) <- GRUPAS
M <- length(X[[1]][["2018"]]$pv)

agg <- function(Y, Xg, sel, w = Xg$sk$w) {
  W <- matrix(w, nrow(sel), ncol(sel)); 100 * sum(W[sel] * Y[sel]) / sum(W[sel]) }
maska <- function(Xg, kopa) Xg$V & matrix(kopa, nrow(Xg$V), J, byrow = TRUE)
rep_se <- function(f, Xg) { e <- f(Xg$sk$w); sqrt(sum((vapply(Xg$rw, f, 1) - e)^2) / 20) }
parametri <- function(Xa, m) {
  th <- (Xa$pv[[m]] - 500) / 100
  t(vapply(seq_len(J), function(j) {
    v <- Xa$V[, j]
    if (sum(v) < MIN_SANEMEJI) return(c(NA_real_, NA_real_))
    w <- Xa$sk$w[v]
    stats::coef(suppressWarnings(stats::glm(Xa$P[v, j] ~ th[v], family = stats::quasibinomial(),
                                            weights = w / mean(w))))
  }, numeric(2)))
}
pmat <- function(Xg, B, m, nobide = 0) {
  th <- (Xg$pv[[m]] - nobide - 500) / 100
  stats::plogis(matrix(B[, 1], length(th), nrow(B), byrow = TRUE) + outer(th, B[, 2]))
}

rez <- list()
for (p in list(c("2018", "2022"), c("2022", "2025"))) {
  cat(strrep("=", 78), "\n", p[1], "->", p[2], "\n", strrep("=", 78), "\n\n")
  Bs <- lapply(GRUPAS, function(gr) lapply(seq_len(M), function(m) parametri(X[[gr]][[p[1]]], m)))
  names(Bs) <- GRUPAS
  ok <- Reduce(`&`, lapply(Bs, function(b) !is.na(b[[1]][, 1])))
  for (gr in GRUPAS) {
    Xa <- X[[gr]][[p[1]]]; Xb <- X[[gr]][[p[2]]]; Bg <- Bs[[gr]]
    sa <- maska(Xa, ok); sb <- maska(Xb, ok)
    oa <- agg(Xa$P, Xa, sa); ob <- agg(Xb$P, Xb, sb)
    pa <- mean(vapply(seq_len(M), function(m) agg(pmat(Xa, Bg[[m]], m), Xa, sa), 1))
    pb <- mean(vapply(seq_len(M), function(m) agg(pmat(Xb, Bg[[m]], m), Xb, sb), 1))
    sl <- mean(vapply(seq_len(M), function(m) agg(pmat(Xb, Bg[[m]], m, -1), Xb, sb) -
                                               agg(pmat(Xb, Bg[[m]], m), Xb, sb), 1))
    Pa1 <- pmat(Xa, Bg[[1]], 1); Pb1 <- pmat(Xb, Bg[[1]], 1)
    se <- sqrt(rep_se(function(w) agg(Xa$P, Xa, sa, w) - agg(Pa1, Xa, sa, w), Xa)^2 +
               rep_se(function(w) agg(Xb$P, Xb, sb, w) - agg(Pb1, Xb, sb, w), Xb)^2)
    oA <- rubin_brr(Xa$pv, Xa$sk$w, Xa$rw, VID); oB <- rubin_brr(Xb$pv, Xb$sk$w, Xb$rw, VID)
    atl <- (ob - oa) - (pb - pa)
    rez[[length(rez) + 1]] <- data.frame(no = p[1], lidz = p[2], grupa = gr, uzd = sum(ok),
      oficiala = oB[["est"]] - oA[["est"]], izm_pp = ob - oa, paredz_pp = pb - pa,
      atlikums_pp = atl, se = se, slipums = sl, atlikums_punkti = atl / sl)
    cat(sprintf("  %-6s %3d uzd. | oficiālā %+6.1f | R1 %+5.2f pp, paredzēts %+5.2f pp | atlikums %+5.2f pp (%.2f)%s ≈ %+5.1f punkti\n",
                gr, sum(ok), oB[["est"]] - oA[["est"]], ob - oa, pb - pa, atl, se, zv(atl, se), atl / sl))
  }
  a <- rez[[length(rez) - 1]]; b <- rez[[length(rez)]]
  d <- a$atlikums_pp - b$atlikums_pp; s <- sqrt(a$se^2 + b$se^2)
  cat(sprintf("\n  Latvijai specifiskais atlikums (LVA A - EST): %+.2f pp (SE %.2f)%s ≈ %+.1f punkti\n\n",
              d, s, zv(d, s), d / a$slipums))
}
rez <- do.call(rbind, rez)
utils::write.csv(rez, "output/pisa2025/saskanotibas_kede.csv", row.names = FALSE)
cat("Izveidots: output/pisa2025/saskanotibas_kede.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
