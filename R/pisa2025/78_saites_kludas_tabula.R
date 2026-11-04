# ============================================================================
# 78. Saites kļūdu (link error) vienotā tabula: viens avots visiem skriptiem
# PISA datu analīze 2025
# ============================================================================
#
# KĀPĒC. Līdz 2026-09-13 lasīšanas saites kļūda 2022 -> 2025 visos skriptos bija
# pagaidu vērtība 3 (un 2018 -> 2025 = sqrt(1,49² + 3²) ≈ 3,35), jo PISA 2025
# tehniskā ziņojuma nav. Skripts 76 tās atvasināja no Vol I B1 tabulām (lasīšana
# 2022 -> 2025 = 1,094; 2018 -> 2025 = 1,832). Lai vērtības nebūtu jālabo katrā
# skriptā atsevišķi, šis skripts saliek vienu tabulu, no kuras lasa 48–56, 68,
# 72–74.
#
# AVOTI (trīs slāņi; ja pārim ir vairāki, prioritāte tādā secībā)
#   1. Oficiālās pāru LE no tehniskajiem ziņojumiem (roku tabulas
#      output/pisa2025/saites_kludas_oficialas.csv, saites_kludas_mat_dab.csv):
#      PISA 2012 TR tab. 12.31, PISA 2015 TR tab. 12.8, PISA 2018 Vol I tab.
#      I.A7.1, PISA 2022 Vol I A7 pielikums.
#   2. Atvasinātās pret 2025 no PISA 2025 Vol I tab. I.B1.2a.36–38
#      (data/pisa_tehniskie/PISA2025_link_errors_atvasinats.csv, skripts 76;
#      apraksts data/pisa_tehniskie/PISA2025_TR_AVOTS.md). Visām valstīm sakrīt
#      līdz trešajai zīmei, tātad tā ir OECD lietotā vērtība.
#   3. Atvasinātās pret 2022 lasīšanai no PISA 2022 Vol I tab. I.B1.5.5
#      (docs/49 4.1: LE = sqrt(SE_OECD² − SE_mūsu²)); 2018 -> 2022 paliek
#      oficiālā 1,47 (atvasinātā 1,49 atšķiras par 0,02).
#
# KO TABULĀ NAV. Matemātikai un dabaszinātnēm 2018 -> 2022 un pāri pret 2022
# pirms 2018. gada nav ne oficiālu, ne atvasinātu vērtību; skripts 56 tur
# joprojām lieto pagaidu 3 un rindu atzīmē.
#
# Izvade:  output/pisa2025/saites_kludas.csv  (joma, no, uz, LE, veids, avots)
#          output/pisa2025/logs/78_saites_kludas_tabula_output.txt
# ============================================================================

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))))
}
if (!file.exists("output/pisa2025/saites_kludas_oficialas.csv")) stop("Skripts jāpalaiž no projekta saknes.")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/78_saites_kludas_tabula_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākums:", format(Sys.time()), "\n\n")

# === 1. OFICIĀLĀS ============================================================
cat(strrep("=", 78), "\n1. Oficiālās pāru saites kļūdas\n", strrep("=", 78), "\n\n", sep = "")
of_r <- utils::read.csv("output/pisa2025/saites_kludas_oficialas.csv", stringsAsFactors = FALSE)
of_m <- utils::read.csv("output/pisa2025/saites_kludas_mat_dab.csv", stringsAsFactors = FALSE)
of <- rbind(data.frame(joma = "READ", no = of_r$no, uz = of_r$lidz, LE = of_r$le, veids = "oficiālā", avots = of_r$avots),
            data.frame(joma = of_m$joma, no = of_m$no, uz = of_m$lidz, LE = of_m$le, veids = "oficiālā", avots = of_m$avots))
cat("Rindas:", nrow(of), " (lasīšana", sum(of$joma == "READ"), ", matemātika", sum(of$joma == "MATH"),
    ", dabaszinātnes", sum(of$joma == "SCIE"), ")\n\n")

# === 2. ATVASINĀTĀS PRET 2025 ===============================================
cat(strrep("=", 78), "\n2. Atvasinātās pret 2025 (PISA 2025 Vol I B1, skripts 76)\n", strrep("=", 78), "\n\n", sep = "")
a25 <- utils::read.csv("data/pisa_tehniskie/PISA2025_link_errors_atvasinats.csv", stringsAsFactors = FALSE)
if (any(a25$LE_max - a25$LE_min > 0.005)) warning("Skripta 76 tabulā valstu vērtības nesakrīt; pārbaudi.")
JK <- c(reading = "READ", mathematics = "MATH", science = "SCIE")
at25 <- data.frame(joma = unname(JK[a25$joma]), no = as.integer(sub("-2025", "", a25$salidzinajums)), uz = 2025L,
                   LE = a25$LE, veids = "atvasinātā",
                   avots = "OECD 2025 Vol. I tab. I.B1.2a.36-38, atvasināts (skripts 76), sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md")
print(at25[, c("joma", "no", "uz", "LE")], row.names = FALSE)

# === 3. ATVASINĀTĀS PRET 2022 (LASĪŠANA) =====================================
cat("\n", strrep("=", 78), "\n3. Atvasinātās pret 2022, lasīšana (docs/49 4.1)\n", strrep("=", 78), "\n\n", sep = "")
at22 <- data.frame(joma = "READ", no = c(2003L, 2006L, 2009L, 2012L, 2015L), uz = 2022L,
                   LE = c(5.30, 8.61, 4.60, 5.99, 3.58), veids = "atvasinātā",
                   avots = "PISA 2022 Vol. I tab. I.B1.5.5, atvasināts (docs/49 4.1)")
print(at22[, c("joma", "no", "uz", "LE")], row.names = FALSE)
cat("2018 -> 2022 paliek oficiālā 1,47 (atvasinātā 1,49).\n")

# === 4. APVIENOŠANA ==========================================================
cat("\n", strrep("=", 78), "\n4. Apvienotā tabula\n", strrep("=", 78), "\n\n", sep = "")
T <- rbind(of, at25, at22)
T <- T[!duplicated(T[, c("joma", "no", "uz")]), ]     # prioritāte: oficiālā, tad pret 2025, tad pret 2022
T <- T[order(T$joma, T$no, T$uz), ]
rownames(T) <- NULL
stopifnot(all(T$no < T$uz), !anyNA(T$LE), all(T$LE > 0))
print(T[, c("joma", "no", "uz", "LE", "veids")], row.names = FALSE)

cat("\nLasīšanas pāri bez vērtības starp cikliem 2000–2025 (jāņem pagaidu vērtība):\n")
G <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)
p <- t(combn(G, 2)); ir <- paste(T$no[T$joma == "READ"], T$uz[T$joma == "READ"])
tr <- p[!paste(p[, 1], p[, 2]) %in% ir, , drop = FALSE]
cat(if (nrow(tr)) paste(apply(tr, 1, paste, collapse = " -> "), collapse = "; ") else "nav", "\n")
cat("Matemātika un dabaszinātnes bez vērtības: 2018 -> 2022 un pāri pret 2022 pirms 2018 (skripts 56 lieto pagaidu 3).\n")

utils::write.csv(T, "output/pisa2025/saites_kludas.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nSaglabāts: output/pisa2025/saites_kludas.csv (", nrow(T), " rindas)\n", sep = "")
cat("Beigas:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
