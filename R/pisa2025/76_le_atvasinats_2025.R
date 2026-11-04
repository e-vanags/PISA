# ============================================================================
# 76. PISA 2025 pāru saites kļūdas (link error), atvasinātas no Vol I B1 tabulām
# PISA datu analīze 2025
# ============================================================================
#
# Kāpēc: PISA 2025 Technical Report 2026. gada septembrī ir tikai melnraksts bez
#   skalēšanas un saites kļūdu nodaļām (pilns TR gaidāms 2027. gadā). Vol I Annex A3
#   (Table I.A3.1) publicē tikai lineārā trenda saites kļūdas, ne pāru (g → 2025).
#   Pāru vērtības vajadzīgas 2022 → 2025 un 2018 → 2025 izmaiņu standartkļūdām
#   (docs/44, 2. sadaļa; data/pisa_tehniskie/PISA2025_TR_AVOTS.md).
# Kā: Vol I Table I.B1.2a.36–38 (vidējais dabaszinātnēs, lasīšanā, matemātikā pa
#   cikliem) dod katrai valstij SE(vidējais_g), SE(vidējais_2025) un SE(dif 2025 − g).
#   OECD metodē SE(dif)² = SE_2025² + SE_g² + LE², tāpēc LE² = SE(dif)² − SE_g² − SE_2025².
#   LE ir viena visām valstīm; ja atvasinātās vērtības visām valstīm sakrīt, aprēķins ir
#   pareizs. OECD vidējā rinda rēķināta atsevišķi (tikai kontrolei).
# Ievade: data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx (stat.link/mrq53f).
# Izvade: data/pisa_tehniskie/PISA2025_link_errors_atvasinats.csv,
#   output/pisa2025/logs/76_le_atvasinats_2025_output.txt.
# ============================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))))
}
IEVADE <- "data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx"
if (!file.exists(IEVADE)) stop("Skripts jāpalaiž no projekta saknes; trūkst ", IEVADE)

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/76_le_atvasinats_2025_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākums:", format(Sys.time()), "\n\n")

required_packages <- c("readxl")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Trūkst pakotnes: ", pkg, " (netiek instalēta).")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat(strrep("=", 90), "\n1. Tabulu izkārtojums\n", strrep("=", 90), "\n\n", sep = "")
# Kolonnas: 1 valsts; tad katram ciklam pāris (vidējais, SE); tad katram g < 2025 pāris
# (dif 2025 − g, SE); tad lineārā trenda koeficients, SE, p. Galvenes 7.–9. rindā,
# dati no 10. rindas (pirmā rinda "OECD average").
izk <- list(
  science     = list(lapa = "Table I.B1.2a.36", cikli = c(2006, 2009, 2012, 2015, 2018, 2022, 2025)),
  reading     = list(lapa = "Table I.B1.2a.37", cikli = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)),
  mathematics = list(lapa = "Table I.B1.2a.38", cikli = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)))
for (j in names(izk)) cat(sprintf("%-12s %s  cikli: %s\n", j, izk[[j]]$lapa, paste(izk[[j]]$cikli, collapse = " ")))

cat("\n", strrep("=", 90), "\n2. Atvasināšana\n", strrep("=", 90), "\n\n", sep = "")
rez <- list()
for (joma in names(izk)) {
  d <- as.data.frame(suppressMessages(read_excel(IEVADE, sheet = izk[[joma]]$lapa, col_names = FALSE)))
  # pārbaude, ka galvene ir tur, kur gaidīts
  stopifnot(grepl("Mean score", d[9, 2]), grepl("S.E.", d[9, 3]), identical(d[10, 1], "OECD average"))
  cikli <- izk[[joma]]$cikli; nc <- length(cikli)
  num <- function(k) suppressWarnings(as.numeric(d[10:nrow(d), k]))
  valstis <- as.character(d[10:nrow(d), 1])
  se25 <- num(1 + 2 * nc)
  for (i in seq_len(nc - 1)) {
    g   <- cikli[i]
    seg <- num(1 + 2 * i)              # SE(vidējais_g)
    dif <- num(2 * nc + 2 * i)         # dif 2025 − g
    sed <- num(1 + 2 * nc + 2 * i)     # SE(dif)
    le2 <- sed^2 - seg^2 - se25^2
    ir  <- !is.na(le2) & !is.na(valstis)
    v   <- le2[ir & valstis != "OECD average"]
    vo  <- le2[ir & valstis == "OECD average"]
    lva <- which(valstis == "Latvia")
    rez[[length(rez) + 1]] <- data.frame(
      joma = joma, salidzinajums = sprintf("%d-2025", g), n_valstis = length(v),
      LE = round(sqrt(median(v)), 3),
      LE_min = round(sqrt(max(min(v), 0)), 3), LE_max = round(sqrt(max(v)), 3),
      LE_OECD_avg = if (length(vo) && vo[1] > 0) round(sqrt(vo[1]), 3) else NA_real_,
      LVA_dif = round(dif[lva], 2), LVA_dif_SE = round(sed[lva], 3))
  }
}
t <- do.call(rbind, rez)
print(t, row.names = FALSE)
neatbilst <- t[t$LE_max - t$LE_min > 0.005, ]
cat("\nValstu nesakritība (LE_max − LE_min > 0,005):", nrow(neatbilst), "rindas\n")
if (nrow(neatbilst)) print(neatbilst, row.names = FALSE)

cat("\n", strrep("=", 90), "\n3. Salīdzinājums ar publicēto\n", strrep("=", 90), "\n\n", sep = "")
cat("Vol I Reader's Guide: dabaszinātnes 2022→2025 link error 2,9.\n")
cat("Atvasinātā pāru vērtība:", t$LE[t$joma == "science" & t$salidzinajums == "2022-2025"],
    "; Table I.A3.1 lineārais trends 2015/2022/2025: 2,905 — Reader's Guide skaitlis atbilst trendam, ne pārim.\n")
cat("Lasīšana 2022→2025:", t$LE[t$joma == "reading" & t$salidzinajums == "2022-2025"],
    "; 2018→2025:", t$LE[t$joma == "reading" & t$salidzinajums == "2018-2025"], "\n")

IZVADE <- "data/pisa_tehniskie/PISA2025_link_errors_atvasinats.csv"
write.csv(t, IZVADE, row.names = FALSE)
cat("\nSaglabāts:", IZVADE, "\n")
cat("Beigas:", format(Sys.time()), "\n")
sink(); close(log_con)
