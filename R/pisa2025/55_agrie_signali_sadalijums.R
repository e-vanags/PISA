# ============================================================================
# 55. Agrīnie signāli sadalījumā un apakšgrupās, A plūsma 2003-2025
# ============================================================================
#
# JAUTĀJUMS. Vidējais A plūsmas rezultāts 2006-2015 ir stabils. Vai kritums
# pirms 2018. gada ir redzams citur: sadalījuma galos (P5-P95, līmeņu daļas)
# vai apakšgrupās (zēni, meitenes, 9. klase, 7.-8. klase, zemākā un augstākā
# ESCS ceturtdaļa, dzimumu starpība)?
#
# METODE. Pilnā Rubina dispersija ar BRR (00_ab_dati.R). Pārmaiņu SE ietver
# saites kļūdu no vienotās tabulas output/pisa2025/saites_kludas.csv (78. skripts:
# oficiālās un OECD 2025 Vol. I atvasinātās, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md);
# ja pārim tās nav, LE = 3 un rinda atzīmēta (līdz 2026-09-13 tā bija arī 2022 -> 2025;
# tagad 1,094). Daļām (piem., zem 2. līmeņa) saites kļūda
# pārvērsta procentpunktos: cik mainītos daļa, ja visu sadalījumu pabīdītu par LE.
# ESCS ceturtdaļas ir katra cikla iekšienē, jo ESCS skala cikliem atšķiras.
#
# UZMANĪBU. 2015 -> 2018 oficiālajā skalā ir Latvijai specifiska instrumenta
# nobīde ap -10 punktiem (docs/55); 2015. gadā notika pāreja no papīra uz datoru,
# kas vēl nav pārbaudīta.
#
# Izvade:  output/pisa2025/agrie_signali_limeni.csv
#          output/pisa2025/agrie_signali_izmainas.csv
#          output/pisa2025/logs/55_agrie_signali_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/55_agrie_signali_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
le_of <- function(a, b) {
  i <- which(LE_TAB$joma == "READ" & LE_TAB$no == as.numeric(a) & LE_TAB$uz == as.numeric(b))
  if (length(i)) c(LE_TAB$LE[i[1]], 1) else c(3, 0)
}
D <- ab_dati(); GADI <- names(D)
LIM <- c(L1a = 334.75, L2 = 407.47, L4 = 552.89, L5 = 625.61)

wq1 <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }
zem <- function(c) function(x, w) 100 * sum(w * (x < c)) / sum(w)
virs <- function(c) function(x, w) 100 * sum(w * (x >= c)) / sum(w)
DALA <- c("zem_L1a", "zem_L2", "L4_un_virs", "L5_un_virs")
STAT_VISI <- list(
  vid = VID, P05 = function(x, w) wq1(x, w, .05), P10 = function(x, w) wq1(x, w, .10),
  P25 = function(x, w) wq1(x, w, .25), P50 = function(x, w) wq1(x, w, .50),
  P75 = function(x, w) wq1(x, w, .75), P90 = function(x, w) wq1(x, w, .90),
  P95 = function(x, w) wq1(x, w, .95),
  zem_L1a = zem(LIM[["L1a"]]), zem_L2 = zem(LIM[["L2"]]),
  L4_un_virs = virs(LIM[["L4"]]), L5_un_virs = virs(LIM[["L5"]]))
STAT_GRUPA <- STAT_VISI[c("vid", "P10", "zem_L2")]
ceturt <- function(sk, a, p) { q <- wq1(sk$escs[a], sk$w[a], p); q }
GRUPAS <- list(
  "A visi"               = function(sk) sk$grupa %in% "A",
  "A zēni"               = function(sk) sk$grupa %in% "A" & sk$dzimums %in% 2,
  "A meitenes"           = function(sk) sk$grupa %in% "A" & sk$dzimums %in% 1,
  "A 9. klase"           = function(sk) sk$grupa %in% "A" & sk$klase %in% 9,
  "A 7.-8. klase"        = function(sk) sk$grupa %in% "A" & sk$klase %in% 7:8,
  "A ESCS zemākā 1/4"    = function(sk) { a <- sk$grupa %in% "A" & !is.na(sk$escs)
                                          a & sk$escs <= ceturt(sk, a, .25) },
  "A ESCS augstākā 1/4"  = function(sk) { a <- sk$grupa %in% "A" & !is.na(sk$escs)
                                          a & sk$escs >= ceturt(sk, a, .75) })

# ---------------------------------------------------------------------------
lim <- list()
for (g in GADI) for (gn in names(GRUPAS)) {
  s <- ab_apakskopa(D[[g]], GRUPAS[[gn]](D[[g]]$sk))
  st <- if (gn == "A visi") STAT_VISI else STAT_GRUPA
  for (k in names(st)) {
    r <- rubin_brr(s$pv, s$sk$w, s$rw, st[[k]])
    # Saites kļūdas pārvēršana daļām: daļas pārmaiņa, ja sadalījumu pabīda par 1 punktu.
    jut <- if (k %in% DALA) {
      c0 <- switch(k, zem_L1a = LIM[["L1a"]], zem_L2 = LIM[["L2"]], L4_un_virs = LIM[["L4"]], L5_un_virs = LIM[["L5"]])
      abs(st[[k]](s$pv[[1]] + 1, s$sk$w) - st[[k]](s$pv[[1]], s$sk$w))
    } else 1
    lim[[length(lim) + 1]] <- data.frame(gads = g, grupa = gn, radītājs = k, n = nrow(s$sk),
                                         vertiba = r[["est"]], se = r[["se"]], le_uz_punktu = jut)
  }
  # Dzimumu starpība (meitenes - zēni) A plūsmā
  if (gn == "A visi") {
    dz <- s$sk$dzimums
    st_gap <- function(x, w) VID(x[dz %in% 1], w[dz %in% 1]) - VID(x[dz %in% 2], w[dz %in% 2])
    r <- rubin_brr(s$pv, s$sk$w, s$rw, st_gap)
    lim[[length(lim) + 1]] <- data.frame(gads = g, grupa = gn, radītājs = "dzimumu_starpiba",
                                         n = nrow(s$sk), vertiba = r[["est"]], se = r[["se"]], le_uz_punktu = 0)
  }
}
lim <- do.call(rbind, lim)
utils::write.csv(lim, "output/pisa2025/agrie_signali_limeni.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
PARI <- list(c("2003", "2006"), c("2006", "2009"), c("2009", "2012"), c("2012", "2015"),
             c("2015", "2018"), c("2018", "2022"), c("2022", "2025"),
             c("2003", "2012"), c("2006", "2015"), c("2009", "2015"), c("2003", "2015"),
             c("2009", "2018"), c("2012", "2018"))
izm <- list()
for (gn in unique(lim$grupa)) for (k in unique(lim$radītājs[lim$grupa == gn])) for (p in PARI) {
  a <- lim[lim$grupa == gn & lim$radītājs == k & lim$gads == p[1], ]
  b <- lim[lim$grupa == gn & lim$radītājs == k & lim$gads == p[2], ]
  if (!nrow(a) || !nrow(b)) next
  le <- le_of(p[1], p[2])
  le_k <- le[1] * mean(c(a$le_uz_punktu, b$le_uz_punktu))   # dzimumu starpībai 0: grupu starpība viena cikla ietvaros
  d <- b$vertiba - a$vertiba; s <- sqrt(a$se^2 + b$se^2 + le_k^2)
  izm[[length(izm) + 1]] <- data.frame(grupa = gn, radītājs = k, no = p[1], lidz = p[2], izm = d, se = s,
                                       z = d / s, le = le[1], le_oficiala = le[2])
}
izm <- do.call(rbind, izm)
utils::write.csv(izm, "output/pisa2025/agrie_signali_izmainas.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
cat(strrep("=", 100), "\nPārmaiņas starp blakus cikliem (zvaigznīte: |z| > 1,96; ¹ LE nav tabulā, pieņemts 3)\n",
    strrep("=", 100), "\n\n")
blakus <- sapply(PARI[1:7], paste, collapse = "→")
cat(sprintf("%-22s %-17s %s\n", "grupa", "rādītājs", paste(sprintf("%12s", sub("20", "", gsub("→20", "→", blakus))), collapse = "")))
for (gn in unique(izm$grupa)) for (k in unique(izm$radītājs[izm$grupa == gn])) {
  x <- izm[izm$grupa == gn & izm$radītājs == k, ]
  cel <- vapply(PARI[1:7], function(p) {
    r <- x[x$no == p[1] & x$lidz == p[2], ]
    if (!nrow(r)) return(sprintf("%12s", ""))
    sprintf("%12s", paste0(sprintf("%+.1f", r$izm), if (abs(r$z) > 1.96) "*" else " ", if (r$le_oficiala == 0) "¹" else " "))
  }, "")
  cat(sprintf("%-22s %-17s %s\n", gn, k, paste(cel, collapse = "")))
}
cat("\n", strrep("=", 100), "\nGarāki periodi pirms 2018. gada\n", strrep("=", 100), "\n\n")
gari <- sapply(PARI[8:13], paste, collapse = "→")
cat(sprintf("%-22s %-17s %s\n", "grupa", "rādītājs", paste(sprintf("%13s", gsub("20", "", gari)), collapse = "")))
for (gn in unique(izm$grupa)) for (k in unique(izm$radītājs[izm$grupa == gn])) {
  x <- izm[izm$grupa == gn & izm$radītājs == k, ]
  cel <- vapply(PARI[8:13], function(p) {
    r <- x[x$no == p[1] & x$lidz == p[2], ]
    if (!nrow(r)) return(sprintf("%13s", ""))
    sprintf("%13s", paste0(sprintf("%+.1f", r$izm), if (abs(r$z) > 1.96) "*" else " ", if (r$le_oficiala == 0) "¹" else " "))
  }, "")
  cat(sprintf("%-22s %-17s %s\n", gn, k, paste(cel, collapse = "")))
}

cat("\n", strrep("=", 100), "\nLīmeņi (A visi)\n", strrep("=", 100), "\n\n")
lv <- lim[lim$grupa == "A visi", ]
cat(sprintf("%-17s %s\n", "rādītājs", paste(sprintf("%9s", GADI), collapse = "")))
for (k in unique(lv$radītājs)) {
  x <- lv[lv$radītājs == k, ]
  cat(sprintf("%-17s %s\n", k, paste(sprintf("%9.1f", x$vertiba[match(GADI, x$gads)]), collapse = "")))
}
cat("\nIzveidotie faili:\n  output/pisa2025/agrie_signali_limeni.csv\n  output/pisa2025/agrie_signali_izmainas.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
