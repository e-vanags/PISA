# ============================================================================
# 81. Slānis 2, 2. kārta: 9. un 12. klases eksāmenu rezultātu laikrinda 2008./09.–2024./25.
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS (docs/46 Slānis 2; docs/81). Valsts eksāmenu rezultāti 9. un 12. klasē ir
# neatkarīgs, ikgadējs, pilnas populācijas mērījums. Vai PISA 2018, 2022 un 2025
# krītošās paaudzes (dz. 2002, 2006, 2009) ir relatīvi vājākas arī nacionālajos
# eksāmenos? Vai kritums ir tieši lasīšanā? Kāda ir skolu tipu plaisa un starpskolu
# dispersija (ICC) blakus PISA aizstājējiem (docs/46 Slānis 4, 69d)?
#
# DATI.
#   (1) VIAA centralizēto eksāmenu KĀRTOTĀJU LĪMEŅA datu faili 2011./12.–2024./25.
#       (data/valsts_parbaudes_darbi/mikrodati/<m. g.>/, AVOTS.md katrā mapē):
#       katram kārtotājam skola, pašvaldība, skolas tips, urbanizācija, daļu izpilde %,
#       kopvērtējums %, līdz 2021./22. arī skolas programmas valoda (1 latviešu,
#       2 mazākumtautību, 3 abas) un dzimums. Eksāmeni: VLL (12. kl. latviešu valoda,
#       2011./12.–2021./22.), MAT (12. kl. matemātika), LV9 (9. kl. latviešu valoda
#       mazākumtautību programmās, CE), no 2022./23. LAT09, MAT09 (9. kl. CE visiem),
#       LATOL, LATAL, MATOL, MATAL, MATVL (12. kl. līmeņu CE).
#   (2) 2008./09. VIAA aprakstošās statistikas XLS (N, vidējais, SD pa daļām).
#   (3) 9. klases latviešu mācībvalodas programmu NECENTRALIZĒTAIS eksāmens
#       latviešu valodā un matemātikā 2008./09.–2021./22. mikrodatos NAV (skolas vērtēja
#       pašas, VIAA publicēja tikai grafikus): daļu vidējie nolasīti vizuāli no
#       daļu salīdzinājuma grafikiem (nolasijumi/ce9_dalas_nolasitas.csv), N / vidējais
#       / SD ar OCR no histogrammām (ce9_histogrammas_stats_ocr.csv), skolas tipu
#       vidējie no grafikiem (ce9_skolas_tips_nolasits.csv).
#   (4) PISA A plūsmas rinda output/pisa2025/ab_rinda_no_2003.csv (skripts 43).
#
# METODE.
#   Gadi NAV psihometriski saskaņoti (katru gadu cits darbs, izpilde %, bez IRT
#   saites). Lūzumi: 2012./13. CE vērtējums tikai procentos (agrāk arī līmeņi A–F);
#   2019./20. 9. kl. eksāmeni atcelti (Covid), LV9 brīvprātīgs; 2020./21. 9. kl.
#   eksāmenu vietā diagnosticējošie darbi, 12. kl. tikai obligātie CE; 2021./22.
#   pirmie līmeņu CE 12. kl.; 2022./23. 9. kl. CE visiem, viens darbs abām plūsmām,
#   centralizēta vērtēšana; 12. kl. tikai līmeņu CE. Tāpēc rinda lasīta relatīvi:
#   z pēc katras sērijas visu gadu vidējā, rangs, A − B tajā pašā eksāmenā tajā pašā
#   gadā (12. kl. VLL pēc skolas programmas valodas; 9. kl. A plūsma un LV9 ir dažādi
#   darbi — tikai relatīva atskaite).
#   Paaudzes: 9. kl. m. g. Y/Y+1 → dz. gads ≈ Y − 15; 12. kl. → Y − 18; PISA gads =
#   dz. gads + 16 (PISA 2018 ← dz. 2002: 9. kl. 2017./18., 12. kl. 2020./21.).
#   ICC: lme4 tukšais modelis kopvērtējums ~ 1 + (1 | skola) katram eksāmenam un gadam.
#
# IZVADE.
#   output/pisa2025/ce_9_12_laikrinda.csv, ce_9_12_sadalijums.csv, ce_9_12_skolu_tipi.csv,
#   ce_9_12_icc.csv, ce_9_12_paaudzes.csv, ce_9_12_ab_starpiba.csv,
#   plots/81_ce_laikrinda.png, 81_ce_paaudzes.png, 81_ce_skolu_tipi.png, 81_ce_icc.png,
#   81_ce_dalas.png, 81_ce_sadalijums.png, 81_ce_ab_starpiba.png,
#   data/valsts_parbaudes_darbi/mikrodati/processed/ce_mikrodati_2012_2025.csv.gz (kešs),
#   logs/81_ce_9_12_klase_output.txt
# ============================================================================

# --- Darba direktorija -------------------------------------------------------
if (!dir.exists("R/pisa2025")) {
  p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
  if (nzchar(p)) setwd(file.path(dirname(p), "..", ".."))
}
stopifnot(dir.exists("R/pisa2025"))

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/81_ce_9_12_klase_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "readxl", "ggplot2", "scales", "patchwork", "lme4")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")

TEMA <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"), legend.position = "bottom",
        plot.caption = element_text(colour = "grey35", hjust = 0))
KR <- c(A = "#0b5394", B = "#c0392b", visi = "#5d6d7e", jaukta = "#7a57c7")
MD_DIR <- "data/valsts_parbaudes_darbi/mikrodati"
ND <- "data/valsts_parbaudes_darbi/nolasijumi"
OUT <- "output/pisa2025"
mg <- function(y) sprintf("%d/%02d", y, (y + 1) %% 100)
z_std <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
# Bāzes standartizācija (labots 2026-09-18 pēc lietotāja aizrādījuma): z pret STABILO gadu (līdz
# 2018./19. m. g. / PISA 2018) vidējo un SD, lai 2025. gada sabrukums nenostiepj atskaiti; ja bāzē
# < 3 punktu, krīt atpakaļ uz visiem gadiem.
z_baze <- function(x, g, robeza = 2018) { b <- x[g <= robeza & !is.na(x)]; if (length(b) < 3) return(z_std(x)); (x - mean(b)) / sd(b) }

TIPS_LBL <- c(`1` = "sākumskola", `2` = "pamatskola", `3` = "vidusskola", `4` = "vakara / tālmācības",
              `5` = "ģimnāzija", `6` = "valsts ģimnāzija", `7` = "profesionālā", `8` = "speciālā",
              `9` = "personas ar vidējo izglītību / augstskola")
URB_LBL <- c(`1` = "Rīga", `2` = "valstspilsētas", `3` = "pilsētas", `4` = "lauki")
VAL_LBL <- c(`1` = "A: latviešu programma", `2` = "B: mazākumtautību programma", `3` = "jaukta: abas programmas")
EKS_LBL <- c(LV9 = "9. kl. latviešu valoda mazākumtautību programmās (CE, B plūsma)",
             LAT09 = "9. kl. latviešu valoda (CE visiem, no 2022./23.)",
             MAT09 = "9. kl. matemātika (CE visiem, no 2022./23.)",
             VLL = "12. kl. latviešu valoda (CE)", MAT = "12. kl. matemātika (CE)",
             LATOL = "12. kl. latviešu valoda, optimālais līmenis", LATAL = "12. kl. latviešu valoda un literatūra, augstākais līmenis",
             MATOL = "12. kl. matemātika, optimālais līmenis", MATAL = "12. kl. matemātika, augstākais līmenis",
             MATVL = "12. kl. matemātika, vispārīgais līmenis",
             VLL_prof = "12. kl. latviešu valoda (prof. izgl. programmas, vecais formāts)",
             MAT_prof = "12. kl. matemātika (prof. izgl. programmas, vecais formāts)")
# daļu nosaukumi: eks × d1..d5
DALAS <- rbindlist(list(
  data.table(eks = "LV9", dala = paste0("d", 1:5), dala_nos = c("Lasīšana", "Klausīšanās", "Valodas lietojums", "Runāšana", "Rakstīšana")),
  data.table(eks = "VLL", dala = paste0("d", 1:3), dala_nos = c("1. daļa: zināšanas un pamatprasmes", "2. daļa: teksta izpratne", "3. daļa: tekstveide")),
  data.table(eks = "VLL_prof", dala = paste0("d", 1:3), dala_nos = c("1. daļa: zināšanas un pamatprasmes", "2. daļa: teksta izpratne", "3. daļa: tekstveide")),
  data.table(eks = "MAT", dala = paste0("d", 1:3), dala_nos = c("1. daļa", "2. daļa", "3. daļa")),
  data.table(eks = "MAT_prof", dala = paste0("d", 1:3), dala_nos = c("1. daļa", "2. daļa", "3. daļa")),
  data.table(eks = "LAT09", dala = paste0("d", 1:3), dala_nos = c("1. daļa: teksta satura un valodas līdzekļu analīze (lasīšana)", "2. daļa: tekstveide", "3. daļa: mutvārdu runa")),
  data.table(eks = "MAT09", dala = paste0("d", 1:2), dala_nos = c("1. daļa: zināšanas, izpratne un prasmes", "2. daļa: kompleksu problēmu risināšana")),
  data.table(eks = "LATOL", dala = paste0("d", 1:3), dala_nos = c("1. daļa: teksta satura un valodas līdzekļu analīze", "2. daļa: tekstveide", "3. daļa: runa un saziņa")),
  data.table(eks = "LATAL", dala = paste0("d", 1:2), dala_nos = c("1. daļa: zināšanas un teksta izpratne", "2. daļa: tekstu analīze")),
  data.table(eks = "MATOL", dala = paste0("d", 1:2), dala_nos = c("1. daļa: zināšanas, izpratne un prasmes", "2. daļa: kompleksu problēmu risināšana")),
  data.table(eks = "MATVL", dala = paste0("d", 1:2), dala_nos = c("1. daļa: zināšanas, izpratne un prasmes", "2. daļa: kompleksu problēmu risināšana")),
  data.table(eks = "MATAL", dala = paste0("d", 1:4), dala_nos = c("1. daļa: zināšanas (optimālā līmeņa saturs)", "2. daļa: problēmas (optimālā līmeņa saturs)", "3. daļa: zināšanas (augstākā līmeņa saturs)", "4. daļa: problēmas (augstākā līmeņa saturs)"))))

# === 1. MIKRODATU IELASE ====================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("1. Kārtotāju līmeņa datu ielase 2011./12.–2024./25.\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

eks_kods <- function(nos, klase = NA) {
  n <- tolower(nos)
  fcase(
    (grepl("mazākumtaut", n) | grepl("m\\.t\\.i\\.p", n)) & grepl("latvie", n) & grepl("9", n), "LV9",
    grepl("latvie", n) & grepl("9", n), "LAT09",
    grepl("matem", n) & grepl("9", n), "MAT09",
    grepl("latvie", n) & grepl("literat", n) & grepl("augst", n), "LATAL",
    grepl("latvie", n) & grepl("optim", n), "LATOL",
    grepl("latvie", n) & grepl("profesion", n), "VLL_prof",
    grepl("latvie", n), "VLL",
    grepl("matem", n) & grepl("augst", n), "MATAL",
    grepl("matem", n) & grepl("optim", n), "MATOL",
    grepl("matem", n) & grepl("vispār", n), "MATVL",
    grepl("matem", n) & grepl("profesion", n), "MAT_prof",
    grepl("matem", n), "MAT",
    default = NA_character_)
}
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
tips_kods <- function(x) {
  x <- tolower(trimws(as.character(x)))
  fcase(x %in% c("1","2","3","4","5","6","7","8","9","0"), suppressWarnings(as.integer(x)),
        grepl("sākumskola", x), 1L, grepl("pamatskola", x), 2L, grepl("tālmāc|vakar", x), 4L,
        grepl("valsts ģimn", x), 6L, grepl("ģimn", x), 5L, grepl("prof", x), 7L, grepl("spec", x), 8L,
        grepl("augstsk|personām", x), 9L, grepl("vidusskola", x), 3L, default = NA_integer_)
}
urb_kods <- function(x) {
  x <- tolower(trimws(as.character(x)))
  fcase(x %in% c("1","2","3","4"), suppressWarnings(as.integer(x)),
        grepl("rīga", x), 1L, grepl("valstspils|republikas", x), 2L, grepl("tālmāc", x), NA_integer_,
        grepl("pilsēt", x), 3L, grepl("lauk", x), 4L, default = NA_integer_)
}
std_cols <- c("gads_sak", "klase", "eks", "eks_nos", "skola", "pasv", "tips", "urban", "valoda", "dzimums",
              "d1", "d2", "d3", "d4", "d5", "kop", "punkti", "avots")
finish <- function(d, gads_sak, avots) {
  d[, gads_sak := gads_sak][, avots := avots]
  for (v in c("d1", "d2", "d3", "d4", "d5", "kop", "punkti", "pasv", "valoda", "dzimums", "tips", "urban")) if (!v %in% names(d)) d[, (v) := NA]
  d[, skola := trimws(as.character(skola))]
  d <- d[!is.na(eks)]
  d[, ..std_cols]
}
# vecais formāts 2011./12.–2016./17.: prieksmets, nosaukums, pK pL pR pV pX, koppro, klase, urban, tips, valoda, dzimums
read_old <- function(d, gads_sak, avots) {
  d <- as.data.table(d); setnames(d, tolower(names(d)))
  setnames(d, c("urbanizacija"), c("urban"), skip_absent = TRUE)
  d <- d[prieksmets %in% c("LV9", "VLL", "MAT")]
  d[, eks := prieksmets][, eks_nos := EKS_LBL[eks]]
  d[, klase := fifelse(eks == "LV9", 9L, 12L)]
  d[, skola := nosaukums]
  d[, `:=`(pk = num(pk), pl = num(pl), pr = num(pr), pv = num(pv), px = num(px), kop = num(koppro), punkti = num(kop))]
  # LV9: L lasīšana, K klausīšanās, V valodas lietojums, R runāšana, X rakstīšana; VLL/MAT: K = 1. daļa, R = 2., X = 3. (L un V kolonnas tukšas)
  d[, `:=`(d1 = fifelse(eks == "LV9", pl, pk), d2 = fifelse(eks == "LV9", pk, pr), d3 = fifelse(eks == "LV9", pv, px),
           d4 = fifelse(eks == "LV9", pr, NA_real_), d5 = fifelse(eks == "LV9", px, NA_real_))]
  d[, `:=`(tips = tips_kods(tips), urban = urb_kods(urban), valoda = suppressWarnings(as.integer(valoda)), dzimums = suppressWarnings(as.integer(dzimums)))]
  finish(d, gads_sak, avots)
}
# 2017./18., 2020./21., 2021./22.: Pārbaudījums, PirmāDaļa … PiektāDaļa (%), Punkti, Procenti/Kopprocents
read_dalas <- function(d, gads_sak, avots, four_parts = FALSE) {
  d <- as.data.table(d)
  nm <- names(d)
  setnames(d, nm[grepl("^Pārbaud", nm)][1], "eks_nos")
  setnames(d, nm[grepl("^skola$|Iestāde", nm)][1], "skola")
  setnames(d, nm[grepl("^Kopprocents$|^Procenti$", nm)][1], "kop")
  if (any(grepl("^Klase$", nm))) setnames(d, "Klase", "klase")
  d[, eks := eks_kods(eks_nos)]
  d[, klase := fifelse(eks %in% c("LV9", "LAT09", "MAT09"), 9L, 12L)]
  p <- lapply(c("PirmāDaļa", "OtrāDaļa", "TrešāDaļa", "CeturtāDaļa", "PiektāDaļa"), function(v) num(d[[v]]))
  if (four_parts) { # LV9 2019./20.–2021./22.: 1 lasīšana, 2 klausīšanās, 3 runāšana, 4 rakstīšana (valodas lietojuma daļas nav)
    d[, `:=`(d1 = p[[1]], d2 = p[[2]], d3 = fifelse(eks == "LV9", NA_real_, p[[3]]),
             d4 = fifelse(eks == "LV9", p[[3]], p[[4]]), d5 = fifelse(eks == "LV9", p[[4]], p[[5]]))]
  } else d[, `:=`(d1 = p[[1]], d2 = p[[2]], d3 = p[[3]], d4 = p[[4]], d5 = p[[5]])]
  d[, kop := num(kop)][, punkti := num(Punkti)]
  d[, `:=`(tips = tips_kods(tips), urban = urb_kods(urban), valoda = suppressWarnings(as.integer(valoda)))]
  if ("dzimums" %in% names(d)) d[, dzimums := suppressWarnings(as.integer(dzimums))]
  finish(d, gads_sak, avots)
}
# 2023./24., 2024./25.: lapas pa eksāmeniem, kolonnas "Procenti X" vai "X procentos"
read_lapas <- function(f, gads_sak, avots, lapas = c("LAT09", "MAT09", "LATOL", "LATAL", "MATOL", "MATAL", "MATVL")) {
  out <- list()
  for (s in intersect(lapas, excel_sheets(f))) {
    d <- as.data.table(suppressMessages(read_excel(f, sheet = s, col_types = "text")))
    nm <- names(d)
    pc <- nm[(grepl("^Procenti ", nm) | grepl(" procentos$", nm)) & !grepl("kopā", nm)]
    kopc <- nm[grepl("^Procenti kopā|^Kopvērtējums", nm)][1]
    sk <- nm[grepl("^Iestāde$|iestādes nosaukums", nm)][1]
    e <- data.table(eks = s, eks_nos = d[[nm[grepl("^Norise$|^Eksāmens$", nm)][1]]], skola = d[[sk]],
                    pasv = d[[nm[grepl("Pašvaldība", nm)][1]]], tips = tips_kods(d$Tips), urban = urb_kods(d[[nm[grepl("^Urban", nm)][1]]]),
                    kop = num(d[[kopc]]), punkti = num(d[[nm[grepl("^Punkti kopā", nm)][1]]]))
    for (i in seq_along(pc)) e[, (paste0("d", i)) := num(d[[pc[i]]])]
    e[, klase := fifelse(eks %in% c("LAT09", "MAT09"), 9L, 12L)]
    out[[s]] <- finish(e, gads_sak, avots)
  }
  rbindlist(out)
}

cache <- file.path(MD_DIR, "processed", "ce_mikrodati_2012_2025.csv.gz")
if (file.exists(cache)) {
  MD <- fread(cache, encoding = "UTF-8")
  cat("Kešs ielasīts:", cache, "(", nrow(MD), "rindas )\n")
} else {
  L <- list()
  # 2011./12. CSV (CP1257)
  f <- file.path(MD_DIR, "2011_2012/visi0_dati1_1_34429.csv")
  txt <- iconv(rawToChar(readBin(f, "raw", file.size(f))), "CP1257", "UTF-8")
  L[["2011"]] <- read_old(fread(text = txt), 2011L, basename(f)); cat("2011./12.:", nrow(L[["2011"]]), "\n")
  for (yy in list(c(2012, "2012_2013/visi0_dati1_3_33459.xlsx", "!visi0_dati"), c(2013, "2013_2014/visi0_dati1_2_32508.xlsx", "Sheet1"),
                  c(2014, "2014_2015/visi0_dati1_0_31573.xls", "spss"), c(2015, "2015_2016/visi0_dati1_1_30663.xlsx", "Sheet1"),
                  c(2016, "2016_2017/visi0_dati1_0_30084.xlsx", "visi"))) {
    f <- file.path(MD_DIR, yy[2]); y <- as.integer(yy[1])
    L[[yy[1]]] <- read_old(suppressMessages(read_excel(f, sheet = yy[3], col_types = "text")), y, basename(f)); cat(mg(y), ":", nrow(L[[yy[1]]]), "\n")
  }
  # 2017./18.
  f <- file.path(MD_DIR, "2017_2018/visi0_dati1_29535.xlsx")
  L[["2017"]] <- read_dalas(suppressMessages(read_excel(f, sheet = "dati", col_types = "text")), 2017L, basename(f)); cat("2017./18.:", nrow(L[["2017"]]), "\n")
  # 2018./19.: prieksmets, IzglītībasIestāde, pl1 pk2 pv3 pr4 px5, koppro, dzim, urban, tips, valoda
  f <- file.path(MD_DIR, "2018_2019/visi0_dati1_28977.xls")
  d <- as.data.table(suppressMessages(read_excel(f, sheet = "dati_2019", col_types = "text")))
  d <- d[prieksmets %in% c("LV9", "VLL", "MAT")]
  d[, `:=`(eks = prieksmets, eks_nos = EKS_LBL[prieksmets], klase = fifelse(prieksmets == "LV9", 9L, 12L), skola = IzglītībasIestāde,
           d1 = num(pl1), d2 = num(pk2), d3 = num(pv3), d4 = num(pr4), d5 = num(px5), kop = num(koppro), punkti = num(kop),
           tips = tips_kods(tips), urban = urb_kods(urban), valoda = suppressWarnings(as.integer(valoda)), dzimums = suppressWarnings(as.integer(dzim)))]
  L[["2018"]] <- finish(d, 2018L, basename(f)); cat("2018./19.:", nrow(L[["2018"]]), "\n")
  # 2019./20.: pl1 pk2 pr3 px4 (LV9 bez valodas lietojuma daļas; VLL/MAT 1.–3. daļa)
  f <- file.path(MD_DIR, "2019_2020/visi0_dati_28605.xlsx")
  d <- as.data.table(suppressMessages(read_excel(f, sheet = "2020", col_types = "text")))
  d <- d[prieksmets %in% c("LV9", "VLL", "MAT")]
  d[, `:=`(eks = prieksmets, eks_nos = Pārbaudījums, klase = fifelse(prieksmets == "LV9", 9L, 12L), skola = IzglītībasIestāde, pasv = novads,
           d1 = num(pl1), d2 = num(pk2), d3 = fifelse(prieksmets == "LV9", NA_real_, num(pr3)),
           d4 = fifelse(prieksmets == "LV9", num(pr3), NA_real_), d5 = fifelse(prieksmets == "LV9", num(px4), NA_real_),
           kop = num(`koppro(procenti)`), punkti = num(`kop(punkti)`),
           tips = tips_kods(tips), urban = urb_kods(urban), valoda = suppressWarnings(as.integer(valoda)), dzimums = suppressWarnings(as.integer(dzimums)))]
  L[["2019"]] <- finish(d, 2019L, basename(f)); cat("2019./20.:", nrow(L[["2019"]]), "\n")
  # 2020./21., 2021./22.
  f <- file.path(MD_DIR, "2020_2021/2021_visi_kopa_1_27543.xlsx")
  L[["2020"]] <- read_dalas(suppressMessages(read_excel(f, sheet = "2021", col_types = "text")), 2020L, basename(f), four_parts = TRUE); cat("2020./21.:", nrow(L[["2020"]]), "\n")
  f <- file.path(MD_DIR, "2021_2022/2022_visi_kopa_27399.xlsx")
  L[["2021"]] <- read_dalas(suppressMessages(read_excel(f, sheet = "2022", col_types = "text")), 2021L, basename(f), four_parts = TRUE); cat("2021./22.:", nrow(L[["2021"]]), "\n")
  # 2022./23.: 9. kl. un vidusskola atsevišķi
  f <- file.path(MD_DIR, "2022_2023/ce_datukopa_pmsk_2022_2023_26595.xlsx")
  d <- as.data.table(suppressMessages(read_excel(f, sheet = "CE rezultāti", col_types = "text"))); nm <- names(d)
  e <- data.table(eks_nos = d$Eksāmens, skola = d[[2]], pasv = d$Pašvaldība, tips = tips_kods(d[[nm[grepl("tips", nm)]]]), urban = urb_kods(d[[nm[grepl("^Urban", nm)]]]),
                  d1 = num(d[[nm[grepl("^1", nm)]]]), d2 = num(d[[nm[grepl("^2", nm)]]]), d3 = num(d[[nm[grepl("^3", nm)]]]), d4 = num(d[[nm[grepl("^4", nm)]]]),
                  kop = num(d[[nm[grepl("^Kopv", nm)]]]), punkti = num(d[[nm[grepl("^Punkti", nm)]]]))
  e[, eks := eks_kods(eks_nos)][, klase := 9L]
  L[["2022a"]] <- finish(e, 2022L, basename(f)); cat("2022./23. 9. kl.:", nrow(L[["2022a"]]), "\n")
  f <- file.path(MD_DIR, "2022_2023/ce_datukopa_vsk_2022_2023_26598.xlsx")
  d <- as.data.table(suppressMessages(read_excel(f, sheet = "Dati", col_types = "text"))); nm <- names(d)
  e <- data.table(eks_nos = d$Pārbaudījums, skola = d[[3]], pasv = d$Pašvaldība, tips = tips_kods(d[[nm[grepl("tips", nm)]]]), urban = urb_kods(d[[nm[grepl("^urban", nm)]]]),
                  d1 = num(d[[nm[grepl("^1", nm)]]]), d2 = num(d[[nm[grepl("^2", nm)]]]), d3 = num(d[[nm[grepl("^3", nm)]]]), d4 = num(d[[nm[grepl("^4", nm)]]]),
                  kop = num(d[[nm[grepl("^Kopv", nm)]]]), punkti = num(d[[nm[grepl("^Punkti", nm)]]]))
  e[, eks := eks_kods(eks_nos)][, klase := 12L]
  L[["2022b"]] <- finish(e, 2022L, basename(f)); cat("2022./23. vidusskola:", nrow(L[["2022b"]]), "\n")
  # 2023./24., 2024./25.
  f <- file.path(MD_DIR, "2023_2024/visi_dati_2024_0810_22392.xlsx")
  L[["2023"]] <- read_lapas(f, 2023L, basename(f)); cat("2023./24.:", nrow(L[["2023"]]), "\n")
  L[["2024a"]] <- read_lapas(file.path(MD_DIR, "2024_2025/visi_dati_pamat_2025_47144.xlsx"), 2024L, "visi_dati_pamat_2025_47144.xlsx")
  L[["2024b"]] <- read_lapas(file.path(MD_DIR, "2024_2025/visi_dati_vidus_2025_47147.xlsx"), 2024L, "visi_dati_vidus_2025_47147.xlsx")
  cat("2024./25.:", nrow(L[["2024a"]]) + nrow(L[["2024b"]]), "\n")
  MD <- rbindlist(L, use.names = TRUE)
  MD[, valoda := fifelse(valoda %in% 1:3, valoda, NA_integer_)]
  dir.create(dirname(cache), showWarnings = FALSE)
  fwrite(MD, cache)
  cat("Kešs saglabāts:", cache, "\n")
}
MD[, gads := mg(gads_sak)]
MD[, programma := fcase(valoda == 1L, "A", valoda == 2L, "B", valoda == 3L, "jaukta", default = "nav")]
MD <- MD[!is.na(kop)]
cat("\nKārtotāju skaits pa gadiem un eksāmeniem (latviešu valoda un matemātika):\n")
print(dcast(MD[, .N, by = .(gads, eks)], gads ~ eks, value.var = "N"))
cat("\nSkolas programmas valoda (1 latviešu, 2 mazākumtautību, 3 abas) 12. kl. latviešu valodas CE:\n")
print(dcast(MD[eks == "VLL", .N, by = .(gads, programma)], gads ~ programma, value.var = "N", fill = 0))
cat("Piezīme: no 2020./21. kods 2 gandrīz izzūd (skolas pārkodētas kā 'abas programmas'), tāpēc A/B dalījums pēc\n",
    "skolas programmas ir tīrs tikai līdz 2019./20.; no 2022./23. valodas/programmas lauka failos vairs nav.\n\n")

# === 2. LAIKRINDA ============================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("2. Laikrinda: gads × klase × eksāmens × programma × daļa (ce_9_12_laikrinda.csv)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 2a. no mikrodatiem
long <- melt(MD[, .(gads, gads_sak, klase, eks, programma, skola, tips, urban, d1, d2, d3, d4, d5, kop)],
             measure.vars = c("d1", "d2", "d3", "d4", "d5", "kop"), variable.name = "dala", value.name = "v")[!is.na(v)]
long <- merge(long, DALAS, by = c("eks", "dala"), all.x = TRUE)
long[dala == "kop", dala_nos := "Kopā"]
long <- long[!is.na(dala_nos)]
agg <- function(x) x[, .(videja = mean(v), sd = sd(v), n = .N, se = sd(v) / sqrt(.N)), by = .(gads, gads_sak, klase, eks, programma, dala, dala_nos)]
L_md <- rbind(agg(copy(long)[, programma := "visi"]), agg(long[programma %in% c("A", "B", "jaukta")]))
L_md[, `:=`(avots = "VIAA CE kārtotāju līmeņa datu fails", metode = "mikrodati", ticamiba = "augsta")]

# 2b. 2008./09. aprakstošā statistika (XLS, valsts rindas)
read_aprakst <- function(f, eks, klase) {
  d <- as.data.table(suppressMessages(read_excel(f, col_names = FALSE)))
  d <- d[d[[1]] == "Valsts" & !is.na(d[[3]])]
  data.table(dala_src = as.character(d[[2]]), n = num(d[[3]]), videja = num(d[[6]]), sd = num(d[[9]]), eks = eks, klase = klase)
}
A09 <- rbind(read_aprakst(file.path(MD_DIR, "2008_2009/vll2_aprakst1_36926.xls"), "VLL", 12L),
             read_aprakst(file.path(MD_DIR, "2008_2009/lv92_aprakst1_36815.xls"), "LV9", 9L),
             read_aprakst(file.path(MD_DIR, "2008_2009/mat2_aprakst1_36842.xls"), "MAT", 12L))
A09 <- A09[dala_src %in% c("Kopvērtējums", "1.daļa", "2.daļa", "3.daļa", "Klausīšanās prasme", "Lasīšanas prasme", "Valodas lietojuma prasme", "Runāšanas prasme", "Rakstīšanas prasme")]
A09[, dala := fcase(dala_src == "Kopvērtējums", "kop", dala_src == "1.daļa", "d1", dala_src == "2.daļa", "d2", dala_src == "3.daļa", "d3",
                    dala_src == "Lasīšanas prasme", "d1", dala_src == "Klausīšanās prasme", "d2", dala_src == "Valodas lietojuma prasme", "d3",
                    dala_src == "Runāšanas prasme", "d4", dala_src == "Rakstīšanas prasme", "d5")]
A09 <- merge(A09, DALAS, by = c("eks", "dala"), all.x = TRUE)[dala == "kop", dala_nos := "Kopā"]
A09[, `:=`(gads = "2008/09", gads_sak = 2008L, programma = "visi", se = sd / sqrt(n), avots = "VIAA 2008./09. aprakstošā statistika (XLS, media 36926/36815/36842)",
           metode = "tabula", ticamiba = "augsta")]
L_09 <- A09[, .(gads, gads_sak, klase, eks, programma, dala, dala_nos, videja, sd, n, se, avots, metode, ticamiba)]

# 2c. 9. kl. necentralizētie eksāmeni (A plūsma) no grafikiem
G <- fread(file.path(ND, "ce9_dalas_nolasitas.csv"), encoding = "UTF-8")
G[, gads_sak := as.integer(substr(gads, 1, 4))]
G[, eks := fifelse(grepl("matem", eksamens), "MAT9_nece", "LAT9_nece")]
G[, programma := fifelse(programma == "lat_lat", "A", "visi")]
G[, dala := fcase(dala == "Kopā", "kop", dala %in% c("Lasīšana", "Lasīšana un valodas sistēmas izpratne", "1. daļa"), "d1",
                  dala == "Valodas lietojums", "d3", dala %in% c("Rakstīšana", "2. daļa"), "d2", dala == "Runāšana", "d4")]
G[, dala_nos := fcase(dala == "kop", "Kopā", eks == "LAT9_nece" & dala == "d1" & gads_sak <= 2010, "Lasīšana",
                      eks == "LAT9_nece" & dala == "d1", "Lasīšana un valodas sistēmas izpratne", eks == "LAT9_nece" & dala == "d2", "Rakstīšana",
                      eks == "LAT9_nece" & dala == "d3", "Valodas lietojums", eks == "LAT9_nece" & dala == "d4", "Runāšana",
                      eks == "MAT9_nece" & dala == "d1", "1. daļa", eks == "MAT9_nece" & dala == "d2", "2. daļa")]
H <- fread(file.path(ND, "ce9_histogrammas_stats_ocr.csv"))
H[, gads_sak := as.integer(substr(fails, 1, 4))]
H[, eks := fifelse(grepl("matem", fails, ignore.case = TRUE), "MAT9_nece", fifelse(grepl("mazākumtaut", fails), "LAT9_maz_nece", "LAT9_nece"))]
H <- H[eks != "LAT9_maz_nece", .(gads_sak, eks, n_hist = n_ocr, mean_pkt = mean_ocr, sd_pkt = sd_ocr, zem50_hist = zem50)]
G <- merge(G, H, by = c("gads_sak", "eks"), all.x = TRUE)
# max punkti = vidējais punktos / kopā %; SD % = SD punkti / max punkti * 100
G <- merge(G, G[dala == "kop", .(gads_sak, eks, kop_pct = videja)], by = c("gads_sak", "eks"), all.x = TRUE)
G[, max_pkt := mean_pkt / (kop_pct / 100)]
G[, sd := fifelse(dala == "kop", sd_pkt / max_pkt * 100, NA_real_)]
G[, n := n_hist][, se := sd / sqrt(n)]
G[, klase := 9L]
G[, avots := sprintf("VIAA gada lapa, media/%s (%s); N/SD: histogrammas OCR", id, basename(fails))]
G[, ticamiba := fifelse(gads_sak == 2008, "augsta (Excel datu tabula grafikā)", "augsta (SPSS etiķete ar 2 zīmēm); N un SD no OCR — vidēja")]
G[, metode := "grafiks (vizuāli) + OCR"]
L_g <- G[, .(gads, gads_sak, klase, eks, programma, dala, dala_nos, videja, sd, n, se, avots, metode, ticamiba)]

LR <- rbind(L_md, L_09, L_g, fill = TRUE)
LR[, prieksmets := fifelse(grepl("LAT|VLL|LV9", eks), "latviešu valoda", "matemātika")]
LR[, eksamens_nos := fcase(eks == "LAT9_nece", "9. kl. eksāmens latviešu valodā, latviešu mācībvalodas programmas (necentralizēts; 2020./21. diagnosticējošais darbs)",
                           eks == "MAT9_nece", "9. kl. eksāmens matemātikā (necentralizēts; 2020./21. diagnosticējošais darbs)",
                           default = unname(EKS_LBL[eks]))]
LR[, programma_nos := fcase(programma == "A", "A: latviešu (mācībvalodas) programma", programma == "B", "B: mazākumtautību programma",
                            programma == "jaukta", "skola ar abām programmām", default = "visi kārtotāji")]
LR[, salidzinamiba := fcase(gads_sak == 2019, "Covid: 9. kl. eksāmeni atcelti, LV9 brīvprātīgs; 12. kl. CE notika",
                            gads_sak == 2020, "Covid: 9. kl. diagnosticējošie darbi eksāmenu vietā; 12. kl. tikai obligātie CE",
                            gads_sak == 2021, "12. kl. pirmie līmeņu CE (pilots); 9. kl. pēdējais necentralizētais eksāmens",
                            gads_sak >= 2022 & klase == 9, "9. kl. CE visiem, viens darbs abām plūsmām, centralizēta vērtēšana",
                            gads_sak >= 2022 & klase == 12, "12. kl. tikai līmeņu CE (OL/AL/VL), cits darbs nekā VLL/MAT",
                            gads_sak == 2012, "no 2012./13. CE vērtējums tikai procentos",
                            default = "cits darbs katru gadu, bez IRT saites")]
setorder(LR, klase, eks, programma, gads_sak, dala)
fwrite(LR[, .(gads, gads_sak, klase, prieksmets, eks, eksamens_nos, programma, programma_nos, dala, dala_nos, videja = round(videja, 2), sd = round(sd, 2), n, se = round(se, 3),
              avots, metode, ticamiba, salidzinamiba)], file.path(OUT, "ce_9_12_laikrinda.csv"))
cat("Saglabāts:", file.path(OUT, "ce_9_12_laikrinda.csv"), "(", nrow(LR), "rindas )\n\n")

cat("9. kl. latviešu valoda, A plūsma (necentralizēts eksāmens; no 2022./23. CE visiem): lasīšanas (un valodas sistēmas izpratnes) daļa, rakstīšana, runāšana, kopā, N\n")
t9 <- rbind(LR[eks == "LAT9_nece" & programma == "A", .(gads, dala, videja, n)],
            LR[eks == "LAT09" & programma == "visi", .(gads, dala, videja, n)])
print(dcast(t9, gads ~ dala, value.var = "videja", fun.aggregate = function(x) round(mean(x), 1)))
cat("(dublētas rindas, ja ir:", nrow(t9) - nrow(unique(t9[, .(gads, dala)])), ")\n")
cat("\n12. kl. latviešu valoda (VLL / no 2022./23. LATOL): kopā un daļas, visi kārtotāji\n")
print(dcast(LR[eks %in% c("VLL", "LATOL") & programma == "visi", .(gads, eks, dala, videja)], gads + eks ~ dala, value.var = "videja"))
cat("\n12. kl. latviešu valoda (VLL) kopā pēc skolas programmas valodas\n")
print(dcast(LR[eks == "VLL" & dala == "kop", .(gads, programma, videja = round(videja, 1))], gads ~ programma, value.var = "videja"))
cat("\n9. kl. LV9 (latviešu valoda mazākumtautību programmās, B plūsma, CE): kopā, lasīšana, N\n")
print(LR[eks == "LV9" & programma == "visi" & dala %in% c("kop", "d1"), .(gads, dala_nos, videja = round(videja, 1), n)])

# === 3. SADALĪJUMS ===========================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("3. Sadalījums: percentiles un daļa zem 30/40/50 % (ce_9_12_sadalijums.csv)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
sad <- function(x) x[, .(n = .N, videja = mean(kop), sd = sd(kop), p10 = quantile(kop, .1), p25 = quantile(kop, .25), p50 = median(kop), p75 = quantile(kop, .75), p90 = quantile(kop, .9),
                         zem20 = mean(kop < 20) * 100, zem30 = mean(kop < 30) * 100, zem40 = mean(kop < 40) * 100, zem50 = mean(kop < 50) * 100, virs80 = mean(kop >= 80) * 100, virs90 = mean(kop >= 90) * 100),
                     by = .(gads, gads_sak, klase, eks, programma)]
SD <- rbind(sad(copy(MD)[, programma := "visi"]), sad(MD[programma %in% c("A", "B")]))
SD[, metode := "mikrodati"]
SD9 <- G[dala == "kop", .(gads, gads_sak, klase, eks, programma, n = n_hist, videja, sd, zem50 = zem50_hist, metode = "histogrammas pikseļu profils (± 2 pp), punktos")]
SD <- rbind(SD, SD9, fill = TRUE)
setorder(SD, klase, eks, programma, gads_sak)
fwrite(SD[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], file.path(OUT, "ce_9_12_sadalijums.csv"))
cat("Saglabāts:", file.path(OUT, "ce_9_12_sadalijums.csv"), "\n\n")
cat("12. kl. latviešu valoda (VLL/LATOL), visi: N, vidējais, SD, P10, P50, P90, zem 40 %, 80 %+\n")
print(SD[eks %in% c("VLL", "LATOL") & programma == "visi", .(gads, eks, n, videja = round(videja, 1), sd = round(sd, 1), p10, p50, p90, zem40 = round(zem40, 1), virs80 = round(virs80, 1))])
cat("\n9. kl. latviešu valoda CE (LAT09) un LV9: sadalījums\n")
print(SD[eks %in% c("LAT09", "LV9") & programma == "visi", .(gads, eks, n, videja = round(videja, 1), sd = round(sd, 1), p10, p50, p90, zem40 = round(zem40, 1), virs80 = round(virs80, 1))])

# === 4. SKOLU TIPI, URBANIZĀCIJA, ICC ========================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("4. Skolu tipi, urbanizācija un starpskolu ICC (ce_9_12_skolu_tipi.csv, ce_9_12_icc.csv)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
MD[, tips_nos := TIPS_LBL[as.character(tips)]][, urban_nos := URB_LBL[as.character(urban)]]
st_agg <- function(x, veids, grupa_col) {
  x <- x[!is.na(get(grupa_col))]
  x[, .(n = .N, videja = mean(kop), sd = sd(kop), d1 = mean(d1, na.rm = TRUE)), by = .(gads, gads_sak, klase, eks, programma, grupa = get(grupa_col))][, grupa_veids := veids][]
}
ST <- rbind(st_agg(copy(MD)[, programma := "visi"], "skolas tips", "tips_nos"),
            st_agg(copy(MD)[, programma := "visi"], "urbanizācija", "urban_nos"),
            st_agg(MD[programma == "A"], "skolas tips", "tips_nos"), use.names = TRUE)
ST[, metode := "mikrodati"]
# 9. kl. A plūsmas necentralizētais eksāmens: skolas tipi no grafikiem
STg <- fread(file.path(ND, "ce9_skolas_tips_nolasits.csv"), encoding = "UTF-8")
STg <- STg[, .(gads, gads_sak = as.integer(substr(gads, 1, 4)), klase = 9L, eks = "LAT9_nece", programma = "A", grupa_veids = "skolas tips", grupa = skolas_tips, videja, metode = "grafiks (vizuāli)")]
ST <- rbind(ST, STg, fill = TRUE)
setorder(ST, klase, eks, programma, grupa_veids, gads_sak, grupa)
fwrite(ST[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], file.path(OUT, "ce_9_12_skolu_tipi.csv"))
cat("Saglabāts:", file.path(OUT, "ce_9_12_skolu_tipi.csv"), "\n\n")

plaisa <- function(x, a = "valsts ģimnāzija", b = "pamatskola") {
  w <- dcast(x[grupa_veids == "skolas tips" & grupa %in% c(a, b), .(gads, gads_sak, klase, eks, programma, grupa, videja)], gads + gads_sak + klase + eks + programma ~ grupa, value.var = "videja")
  w[, plaisa := get(a) - get(b)]
  w[]
}
PL <- rbind(plaisa(ST[eks %in% c("LAT9_nece", "LAT09", "MAT09")]), plaisa(ST[eks %in% c("VLL", "MAT", "LATOL", "MATOL", "LV9")], a = "valsts ģimnāzija", b = "vidusskola"), fill = TRUE)
PL <- merge(PL, SD[, .(gads, eks, programma, sd_kop = sd)], by = c("gads", "eks", "programma"), all.x = TRUE)
PL[, plaisa_sd := plaisa / sd_kop]
cat("Valsts ģimnāzija − pamatskola (9. kl.) / − vidusskola (12. kl.), pp un skolēnu SD vienībās:\n")
print(PL[order(klase, eks, programma, gads_sak), .(gads, klase, eks, programma, valsts_gimnazija = round(`valsts ģimnāzija`, 1), pamatskola_vai_vidusskola = round(fifelse(is.na(pamatskola), vidusskola, pamatskola), 1), plaisa = round(plaisa, 1), plaisa_sd = round(plaisa_sd, 2))])
cat("\nUrbanizācija: Rīga − lauki, kopvērtējums, pp\n")
U <- dcast(ST[grupa_veids == "urbanizācija" & grupa %in% c("Rīga", "lauki") & programma == "visi", .(gads, eks, grupa, videja)], gads + eks ~ grupa, value.var = "videja")[, riga_lauki := Rīga - lauki]
print(dcast(U[eks %in% c("LV9", "VLL", "MAT", "LAT09", "MAT09", "LATOL", "MATOL")], gads ~ eks, value.var = "riga_lauki", fun.aggregate = function(x) round(x[1], 1)))

# ICC
cat("\nStarpskolu ICC (lme4, kopvērtējums ~ 1 + (1 | skola)):\n")
icc_fun <- function(x) {
  if (nrow(x) < 200 || uniqueN(x$skola) < 10) return(NULL)
  m <- tryCatch(lmer(kop ~ 1 + (1 | skola), data = x, REML = TRUE), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  vc <- as.data.frame(VarCorr(m)); vb <- vc$vcov[vc$grp == "skola"]; ve <- vc$vcov[vc$grp == "Residual"]
  m1 <- if (all(!is.na(x$d1)) && sd(x$d1) > 0) tryCatch(lmer(d1 ~ 1 + (1 | skola), data = x, REML = TRUE), error = function(e) NULL) else NULL
  icc1 <- if (!is.null(m1)) { v1 <- as.data.frame(VarCorr(m1)); v1$vcov[v1$grp == "skola"] / sum(v1$vcov) } else NA_real_
  data.table(icc = vb / (vb + ve), sd_starp = sqrt(vb), sd_ieks = sqrt(ve), icc_d1 = icc1, skolas = uniqueN(x$skola), n = nrow(x))
}
ICC <- rbind(MD[eks %in% c("LV9", "VLL", "MAT", "LAT09", "MAT09", "LATOL", "MATOL", "MATAL", "LATAL"), icc_fun(.SD), by = .(gads, gads_sak, klase, eks)][, programma := "visi"],
             MD[eks %in% c("VLL", "MAT") & programma %in% c("A", "B"), icc_fun(.SD), by = .(gads, gads_sak, klase, eks, programma)],
             MD[eks %in% c("VLL", "MAT", "LAT09", "MAT09", "LATOL", "MATOL") & tips %in% c(2, 3, 5, 6), icc_fun(.SD), by = .(gads, gads_sak, klase, eks)][, programma := "visi, tikai vispārizglītojošās skolas (tipi 2, 3, 5, 6)"], use.names = TRUE)
setorder(ICC, klase, eks, programma, gads_sak)
fwrite(ICC[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)], file.path(OUT, "ce_9_12_icc.csv"))
cat("Saglabāts:", file.path(OUT, "ce_9_12_icc.csv"), "\n")
print(dcast(ICC[programma == "visi"], gads ~ eks, value.var = "icc", fun.aggregate = function(x) round(x[1], 3)))
cat("\nICC tikai vispārizglītojošās skolās (bez prof., spec., tālmācības):\n")
print(dcast(ICC[grepl("tikai", programma)], gads ~ eks, value.var = "icc", fun.aggregate = function(x) round(x[1], 3)))
cat("\nICC 12. kl. VLL pēc skolas programmas (A / B):\n")
print(dcast(ICC[eks == "VLL" & programma %in% c("A", "B")], gads ~ programma, value.var = "icc", fun.aggregate = function(x) round(x[1], 3)))
cat("\nICC lasīšanas daļai (d1) 9. kl. LAT09 un LV9, 12. kl. VLL 1. daļai:\n")
print(ICC[eks %in% c("LAT09", "LV9", "VLL") & programma == "visi", .(gads, eks, icc = round(icc, 3), icc_d1 = round(icc_d1, 3), skolas, n)])

# === 5. PAAUDZES =============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("5. Paaudžu saskaņošana ar PISA cikliem (ce_9_12_paaudzes.csv)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
ser <- rbind(
  LR[eks == "LAT9_nece" & programma == "A" & dala == "d1", .(gads, gads_sak, klase, videja, n, serija = "9. kl. latviešu valoda A: lasīšana (+ valodas sistēmas izpratne no 2011./12.); 2022./23.+ CE 1. daļa")],
  LR[eks == "LAT09" & programma == "visi" & dala == "d1", .(gads, gads_sak, klase, videja, n, serija = "9. kl. latviešu valoda A: lasīšana (+ valodas sistēmas izpratne no 2011./12.); 2022./23.+ CE 1. daļa")],
  LR[eks == "LAT9_nece" & programma == "A" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "9. kl. latviešu valoda A: kopā; 2022./23.+ CE visiem")],
  LR[eks == "LAT09" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "9. kl. latviešu valoda A: kopā; 2022./23.+ CE visiem")],
  LR[eks == "MAT9_nece" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "9. kl. matemātika: kopā; 2022./23.+ CE visiem")],
  LR[eks == "MAT09" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "9. kl. matemātika: kopā; 2022./23.+ CE visiem")],
  LR[eks == "LV9" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "9. kl. LV9 (B plūsma, latviešu kā otrā valoda): kopā")],
  LR[eks == "VLL" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. latviešu valoda CE: kopā, visi")],
  LR[eks == "VLL" & programma == "A" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. latviešu valoda CE: kopā, A (latviešu programmas skolas)")],
  LR[eks == "VLL" & programma == "B" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. latviešu valoda CE: kopā, B (mazākumtautību programmas skolas)")],
  LR[eks == "VLL" & programma == "visi" & dala == "d2", .(gads, gads_sak, klase, videja, n, serija = "12. kl. latviešu valoda CE: 2. daļa teksta izpratne, visi")],
  LR[eks == "MAT" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. matemātika CE: kopā, visi")],
  LR[eks == "LATOL" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. latviešu valoda OL CE (2021./22.+): kopā")],
  LR[eks == "MATOL" & programma == "visi" & dala == "kop", .(gads, gads_sak, klase, videja, n, serija = "12. kl. matemātika OL CE (2021./22.+): kopā")])
ser[, dz_gads := fifelse(klase == 9L, gads_sak - 15L, gads_sak - 18L)]
ser[, pisa_gads := dz_gads + 16L]
ser[, z := z_baze(videja, gads_sak), by = serija]
ser[, rangs := frank(-videja), by = serija]
ser[, gadu_sk := .N, by = serija]
ser[, pisa_paaudze := fcase(dz_gads == 1999, "PISA 2015", dz_gads == 2002, "PISA 2018", dz_gads == 2006, "PISA 2022", dz_gads == 2009, "PISA 2025", dz_gads == 1996, "PISA 2012", dz_gads == 1993, "PISA 2009", default = "")]
# PISA A plūsma
AB <- fread(file.path(OUT, "ab_rinda_no_2003.csv"))
AB[, dz_gads := gads - 16L]
AB[, z := z_baze(vid, gads), by = grupa]
pisaA <- AB[grupa == "A", .(dz_gads, pisa_gads = gads, pisa_A_vid = round(vid, 1), pisa_A_z = round(z, 2))]
ser <- merge(ser, pisaA, by = c("dz_gads", "pisa_gads"), all.x = TRUE)
setorder(ser, serija, gads_sak)
fwrite(ser[, .(serija, gads, gads_sak, klase, dz_gads, pisa_gads, pisa_paaudze, videja = round(videja, 2), n, z = round(z, 2), rangs, gadu_sk, pisa_A_vid, pisa_A_z)], file.path(OUT, "ce_9_12_paaudzes.csv"))
cat("Saglabāts:", file.path(OUT, "ce_9_12_paaudzes.csv"), "\n\n")
cat("PISA paaudzes nacionālajos eksāmenos: z (pēc sērijas visu gadu vidējā) un rangs (1 = labākais gads)\n")
pp <- ser[pisa_paaudze != "", .(serija, pisa_paaudze, dz_gads, gads, videja = round(videja, 1), z = round(z, 2), rangs = paste0(rangs, "/", gadu_sk), pisa_A_z)]
print(pp[order(serija, dz_gads)], nrows = 200)

# A − B 12. kl. VLL tajā pašā eksāmenā
ABd <- dcast(LR[eks == "VLL" & dala == "kop" & programma %in% c("A", "B", "jaukta"), .(gads, gads_sak, programma, videja, n)], gads + gads_sak ~ programma, value.var = c("videja", "n"))
ABd[, A_B := videja_A - videja_B]
fwrite(ABd[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], file.path(OUT, "ce_9_12_ab_starpiba.csv"))
cat("\n12. kl. latviešu valodas CE: A (latviešu programmas skolas) − B (mazākumtautību programmas skolas), tas pats darbs, pp:\n")
print(ABd[, .(gads, A = round(videja_A, 1), B = round(videja_B, 1), jaukta = round(videja_jaukta, 1), A_B = round(A_B, 1), n_A, n_B, n_jaukta)])
cat("Piezīme: 12. kl. latviešu valodas CE mazākumtautību programmu skolēniem ir tas pats darbs kā latviešu programmu skolēniem\n",
    "(kopš 2012./13. vienots eksāmens); B plūsmai tā ir otrā valoda, tāpēc A − B mēra arī valodas prasmi, ne tikai lasītprasmi.\n")

# === 6. DAĻAS: vai kritums ir tieši lasīšanā ================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("6. Daļas: lasīšana pret rakstīšanu / tekstveidi un runāšanu\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
D9 <- rbind(LR[eks == "LAT9_nece" & programma == "A" & dala %in% c("d1", "d2", "d4"), .(gads, gads_sak, dala, videja)],
            LR[eks == "LAT09" & programma == "visi" & dala %in% c("d1", "d2", "d3"), .(gads, gads_sak, dala = fifelse(dala == "d3", "d4", dala), videja)])
D9[, dala_nos := fcase(dala == "d1", "lasīšana / 1. daļa", dala == "d2", "rakstīšana / tekstveide", dala == "d4", "runāšana / mutvārdu daļa")]
D9[, z := z_std(videja), by = dala_nos]
D9w <- dcast(D9, gads + gads_sak ~ dala_nos, value.var = "videja")
D9w[, las_minus_rakst := `lasīšana / 1. daļa` - `rakstīšana / tekstveide`]
cat("9. kl. latviešu valoda A: lasīšana, rakstīšana, runāšana, starpība lasīšana − rakstīšana\n"); print(D9w[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
cat("\nKorelācijas starp daļu gadu vērtībām (9. kl. A):\n"); print(round(cor(D9w[, .(`lasīšana / 1. daļa`, `rakstīšana / tekstveide`, `runāšana / mutvārdu daļa`)], use = "pairwise"), 2))
D12 <- LR[eks == "VLL" & programma == "visi" & dala %in% c("d1", "d2", "d3"), .(gads, gads_sak, dala_nos, videja)]
D12[, z := z_std(videja), by = dala_nos]
D12w <- dcast(D12, gads ~ dala_nos, value.var = "videja")
cat("\n12. kl. latviešu valoda CE daļas (visi):\n"); print(D12w[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
cat("\nPISA paaudzes daļu z: 9. kl. dz. 2002 (2017./18.), 2006 (2021./22.), 2009 (2024./25.) un 12. kl. dz. 2002 (2020./21.)\n")
print(D9[gads_sak %in% c(2017, 2021, 2024), .(gads, dala_nos, videja = round(videja, 1), z = round(z, 2))])
print(D12[gads_sak %in% c(2017, 2020), .(gads, dala_nos, videja = round(videja, 1), z = round(z, 2))])

# === 7. GRAFIKI ==============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("7. Grafiki\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
CAPT <- "Avots: VIAA valsts pārbaudes darbu statistika (kārtotāju līmeņa datu faili 2011./12.–2024./25.; 2008./09. tabulas; 9. kl. necentralizētie eksāmeni no grafikiem). Gadi nav IRT saskaņoti."
luzumi <- data.table(x = c(2012, 2019.5, 2022), lbl = c("CE tikai %", "Covid", "9. kl. CE visiem;\n12. kl. līmeņu CE"))

# P1 laikrinda
p1a <- ggplot() +
  geom_vline(data = luzumi, aes(xintercept = x), linetype = "dotted", colour = "grey50") +
  geom_line(data = ser[grepl("^9. kl. latviešu valoda A: lasīšana", serija)], aes(gads_sak, videja, colour = "A: lasīšana / 1. daļa"), linewidth = 1) +
  geom_point(data = ser[grepl("^9. kl. latviešu valoda A: lasīšana", serija)], aes(gads_sak, videja, colour = "A: lasīšana / 1. daļa", shape = gads_sak >= 2022), size = 2.5) +
  geom_line(data = ser[grepl("^9. kl. latviešu valoda A: kopā", serija)], aes(gads_sak, videja, colour = "A: kopā"), linewidth = 0.7) +
  geom_line(data = ser[grepl("LV9", serija)], aes(gads_sak, videja, colour = "B: LV9 kopā (otrā valoda)"), linewidth = 0.7, linetype = "longdash") +
  scale_colour_manual(NULL, values = c("A: lasīšana / 1. daļa" = unname(KR["A"]), "A: kopā" = "#6fa8dc", "B: LV9 kopā (otrā valoda)" = unname(KR["B"]))) +
  scale_shape_manual(NULL, values = c(`FALSE` = 16, `TRUE` = 17), labels = c("necentralizēts (skola vērtē)", "CE (centralizēta vērtēšana)")) +
  scale_x_continuous(breaks = 2008:2024, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "9. klase: latviešu valoda", y = "vidējā izpilde %", x = NULL) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p1b <- ggplot() +
  geom_vline(data = luzumi, aes(xintercept = x), linetype = "dotted", colour = "grey50") +
  geom_line(data = ser[grepl("^9. kl. matemātika", serija)], aes(gads_sak, videja, colour = "9. kl. matemātika kopā"), linewidth = 1) +
  geom_point(data = ser[grepl("^9. kl. matemātika", serija)], aes(gads_sak, videja, colour = "9. kl. matemātika kopā", shape = gads_sak >= 2022), size = 2.5) +
  geom_line(data = ser[grepl("^12. kl. matemātika CE", serija)], aes(gads_sak, videja, colour = "12. kl. matemātika CE kopā"), linewidth = 0.8) +
  geom_line(data = ser[grepl("^12. kl. matemātika OL", serija)], aes(gads_sak, videja, colour = "12. kl. matemātika OL CE"), linewidth = 0.8, linetype = "dashed") +
  scale_colour_manual(NULL, values = c("9. kl. matemātika kopā" = "#1b7f5a", "12. kl. matemātika CE kopā" = "#8e44ad", "12. kl. matemātika OL CE" = "#c39bd3")) +
  scale_shape_manual(NULL, values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  scale_x_continuous(breaks = 2008:2024, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "Matemātika, 9. un 12. klase", y = "vidējā izpilde %", x = NULL) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p1c <- ggplot() +
  geom_vline(data = luzumi, aes(xintercept = x), linetype = "dotted", colour = "grey50") +
  geom_line(data = ser[grepl("^12. kl. latviešu valoda CE: kopā, A", serija)], aes(gads_sak, videja, colour = "A (latviešu programmu skolas)"), linewidth = 1) +
  geom_line(data = ser[grepl("^12. kl. latviešu valoda CE: kopā, B", serija)], aes(gads_sak, videja, colour = "B (mazākumtautību programmu skolas)"), linewidth = 1) +
  geom_line(data = ser[grepl("^12. kl. latviešu valoda CE: kopā, visi", serija)], aes(gads_sak, videja, colour = "visi"), linewidth = 0.6) +
  geom_line(data = ser[grepl("^12. kl. latviešu valoda CE: 2. daļa", serija)], aes(gads_sak, videja, colour = "2. daļa: teksta izpratne (visi)"), linewidth = 0.8, linetype = "dashed") +
  geom_line(data = ser[grepl("^12. kl. latviešu valoda OL", serija)], aes(gads_sak, videja, colour = "OL CE (2021./22.+)"), linewidth = 0.8, linetype = "dotdash") +
  scale_colour_manual(NULL, values = c("A (latviešu programmu skolas)" = unname(KR["A"]), "B (mazākumtautību programmu skolas)" = unname(KR["B"]), "visi" = unname(KR["visi"]), "2. daļa: teksta izpratne (visi)" = "#0b5394", "OL CE (2021./22.+)" = "#7a57c7")) +
  scale_x_continuous(breaks = 2008:2024, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "12. klase: latviešu valodas CE", y = "vidējā izpilde %", x = "mācību gads", caption = CAPT) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path(OUT, "plots/81_ce_laikrinda.png"), (p1a / p1b / p1c), width = 11, height = 13, dpi = 200, bg = "white")

# P2 paaudzes: z pēc dzimšanas gada, divas laika joslas
ser_p <- ser[serija %in% c("9. kl. latviešu valoda A: lasīšana (+ valodas sistēmas izpratne no 2011./12.); 2022./23.+ CE 1. daļa",
                           "9. kl. latviešu valoda A: kopā; 2022./23.+ CE visiem", "9. kl. matemātika: kopā; 2022./23.+ CE visiem",
                           "12. kl. latviešu valoda CE: kopā, A (latviešu programmas skolas)", "12. kl. matemātika CE: kopā, visi")]
ser_p[, serija_isa := fcase(grepl("lasīšana", serija), "9. kl. latviešu valoda A: lasīšana / 1. daļa", grepl("^9. kl. latviešu valoda A: kopā", serija), "9. kl. latviešu valoda A: kopā",
                            grepl("^9. kl. matem", serija), "9. kl. matemātika: kopā", grepl("^12. kl. latviešu", serija), "12. kl. latviešu valoda CE: kopā, A", default = "12. kl. matemātika CE: kopā")]
pisa_pts <- pisaA[dz_gads >= 1990]
# Gludas līknes caur punktiem (lietotāja lūgums 2026-09-18): monotons Hermita splains (monoH.FC) —
# iet precīzi caur katru punktu, starp punktiem neliec pāri to vērtībām (nav pārsviedumu), bet
# pārtrūkst tur, kur sērijā trūkst gada (2019./20. atceltie eksāmeni), lai tukšums paliktu redzams.
glud <- function(d, xk = "dz_gads", yk = "z", n = 40, max_gap = 1) {
  d <- d[!is.na(get(yk))][order(get(xk))]; if (nrow(d) < 3) return(d[, .(x = get(xk), y = get(yk), seg = 1L)])
  x <- d[[xk]]; y <- d[[yk]]; f <- splinefun(x, y, method = "monoH.FC")
  rbindlist(lapply(seq_len(length(x) - 1), function(i) { if (x[i + 1] - x[i] > max_gap) return(NULL)
    xs <- seq(x[i], x[i + 1], length.out = n); data.table(x = xs, y = f(xs), seg = i) }))
}
ser_gl <- ser_p[, glud(.SD), by = .(serija_isa, klase)]
pisa_gl <- glud(copy(pisa_pts), xk = "dz_gads", yk = "pisa_A_z", max_gap = 5)
p2 <- ggplot(ser_p, aes(dz_gads, z)) +
  # Visas PISA paaudzes iekrāsotas (labots 2026-09-18 pēc lietotāja aizrādījuma): stabilās pelēkas,
  # krītošās (2018, 2022, 2025) sarkanas
  annotate("rect", xmin = c(1989.5, 1992.5, 1995.5, 1998.5), xmax = c(1990.5, 1993.5, 1996.5, 1999.5), ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey40") +
  annotate("text", x = c(1990, 1993, 1996, 1999), y = 2.6, label = c("PISA 2006\ndz. 1990", "PISA 2009\ndz. 1993", "PISA 2012\ndz. 1996", "PISA 2015\ndz. 1999"), size = 3, colour = "grey35") +
  annotate("rect", xmin = c(2001.5, 2005.5, 2008.5), xmax = c(2002.5, 2006.5, 2009.5), ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "#c0392b") +
  annotate("text", x = c(2002, 2006, 2009), y = 2.6, label = c("PISA 2018\ndz. 2002", "PISA 2022\ndz. 2006", "PISA 2025\ndz. 2009"), size = 3, colour = "#c0392b") +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_path(data = ser_gl, aes(x, y, colour = serija_isa, group = interaction(serija_isa, seg)), linewidth = 0.8) +
  geom_point(aes(colour = serija_isa, shape = klase == 9), size = 2.2) +
  geom_path(data = pisa_gl, aes(x, y, group = seg), colour = "black", linetype = "dotted") +
  geom_point(data = pisa_pts, aes(dz_gads, pisa_A_z), colour = "black", shape = 4, size = 3.5, stroke = 1.2) +
  scale_colour_manual(NULL, values = c("9. kl. latviešu valoda A: lasīšana / 1. daļa" = "#0b5394", "9. kl. latviešu valoda A: kopā" = "#6fa8dc", "9. kl. matemātika: kopā" = "#1b7f5a",
                                       "12. kl. latviešu valoda CE: kopā, A" = "#c0392b", "12. kl. matemātika CE: kopā" = "#8e44ad")) +
  scale_shape_manual(NULL, values = c(`TRUE` = 16, `FALSE` = 15), labels = c("12. klase (dz. gads = m. g. − 18)", "9. klase (dz. gads = m. g. − 15)")) +
  scale_x_continuous("Dzimšanas gads (paaudze)", breaks = seq(1990, 2010, 1), sec.axis = sec_axis(~ . + 16, name = "PISA mērījuma gads (dzimšanas gads + 16)", breaks = c(2009, 2012, 2015, 2018, 2022, 2025))) +
  scale_y_continuous("z (novirze no sērijas stabilo gadu vidējā, stabilo gadu SD vienībās)", limits = c(-6, 2.9), breaks = seq(-6, 3, 1)) +
  labs(title = "Vai PISA paaudzes ir tikpat stipras vai vājas arī valsts eksāmenos?",
       subtitle = "Katra sērija standartizēta pēc savu gadu vidējā un SD līdz 2018./19. m. g. (PISA — līdz 2018. gada cikla), lai 2025. gads nenostieptu atskaiti;\n× = PISA latviešu valodas plūsmas lasīšanas vidējais tajā pašā z (skripts 43); 1 z ≈ stabilo gadu SD (PISA ≈ 2,6 punkti, tāpēc 2025. gada −20 punkti ir −5,6 z).\nGadi nav IRT saskaņoti — tikai relatīvs lasījums.",
       caption = CAPT) + TEMA + guides(colour = guide_legend(nrow = 2))
ggsave(file.path(OUT, "plots/81_ce_paaudzes.png"), p2, width = 12, height = 7.5, dpi = 200, bg = "white")

# P3 skolu tipi
st9 <- rbind(ST[eks == "LAT9_nece" & programma == "A" & grupa %in% c("pamatskola", "vidusskola", "valsts ģimnāzija"), .(gads_sak, eks = "9. kl. latviešu valoda (A; no 2022./23. CE visiem)", grupa, videja)],
             ST[eks == "LAT09" & programma == "visi" & grupa %in% c("pamatskola", "vidusskola", "valsts ģimnāzija", "vakara / tālmācības"), .(gads_sak, eks = "9. kl. latviešu valoda (A; no 2022./23. CE visiem)", grupa, videja)],
             ST[eks == "VLL" & programma == "visi" & grupa %in% c("vidusskola", "ģimnāzija", "valsts ģimnāzija", "vakara / tālmācības", "profesionālā"), .(gads_sak, eks = "12. kl. latviešu valoda CE (līdz 2021./22.)", grupa, videja)],
             ST[eks == "MAT" & programma == "visi" & grupa %in% c("vidusskola", "ģimnāzija", "valsts ģimnāzija", "vakara / tālmācības", "profesionālā"), .(gads_sak, eks = "12. kl. matemātika CE (līdz 2021./22.)", grupa, videja)],
             ST[eks == "MAT09" & programma == "visi" & grupa %in% c("pamatskola", "vidusskola", "valsts ģimnāzija", "vakara / tālmācības"), .(gads_sak, eks = "9. kl. matemātika (no 2022./23. CE visiem)", grupa, videja)])
p3 <- ggplot(st9, aes(gads_sak, videja, colour = grupa)) + geom_vline(xintercept = 2022, linetype = "dotted", colour = "grey50") + geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  facet_wrap(~ eks, scales = "free_y") +
  scale_colour_manual(NULL, values = c("pamatskola" = "#5d6d7e", "vidusskola" = "#0b5394", "ģimnāzija" = "#7a57c7", "valsts ģimnāzija" = "#c0392b", "vakara / tālmācības" = "#e67e22", "profesionālā" = "#1b7f5a")) +
  scale_x_continuous(breaks = seq(2009, 2024, 2), labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "Kopvērtējums pa skolu tipiem", y = "vidējā izpilde %", x = "mācību gads", caption = CAPT) + TEMA
ggsave(file.path(OUT, "plots/81_ce_skolu_tipi.png"), p3, width = 12, height = 8, dpi = 200, bg = "white")

# P4 ICC
icc_p <- ICC[programma == "visi" & eks %in% c("LV9", "VLL", "MAT", "LAT09", "MAT09", "LATOL", "MATOL")]
icc_p[, eks_nos := EKS_LBL[eks]]
pisa_icc <- fread(file.path(OUT, "daudzlimenu_icc.csv"))[grupa == "LVA A", .(gads_sak = gads - 1L, icc, eks_nos = "PISA lasīšana, latviešu valodas plūsma (69d)")]
p4 <- ggplot(icc_p, aes(gads_sak, icc, colour = eks_nos)) + geom_vline(xintercept = 2022, linetype = "dotted", colour = "grey50") + geom_line(linewidth = 0.8) + geom_point(size = 2) +
  geom_point(data = pisa_icc, aes(gads_sak, icc), colour = "black", shape = 4, size = 3.5, stroke = 1.2) + geom_line(data = pisa_icc, aes(gads_sak, icc), colour = "black", linetype = "dotted") +
  scale_x_continuous(breaks = seq(2003, 2024, 1), labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  scale_colour_brewer(NULL, palette = "Dark2") + guides(colour = guide_legend(ncol = 2)) +
  labs(title = "Starpskolu ICC valsts eksāmenos (× = PISA lasīšana, latviešu valodas plūsma, m. g. = PISA gads − 1)", y = "ICC (skolu dispersijas daļa)", x = "mācību gads", caption = CAPT) +
  TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path(OUT, "plots/81_ce_icc.png"), p4, width = 12, height = 7, dpi = 200, bg = "white")

# P5 daļas
p5a <- ggplot(D9, aes(gads_sak, videja, colour = dala_nos)) + geom_vline(data = luzumi, aes(xintercept = x), linetype = "dotted", colour = "grey50") + geom_line(linewidth = 0.9) + geom_point() +
  scale_colour_manual(NULL, values = c("lasīšana / 1. daļa" = "#0b5394", "rakstīšana / tekstveide" = "#e67e22", "runāšana / mutvārdu daļa" = "#1b7f5a")) +
  scale_x_continuous(breaks = 2008:2024, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "9. kl. latviešu valoda (A; no 2022./23. CE visiem): daļas", y = "vidējā izpilde %", x = NULL) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
p5b <- ggplot(D12, aes(gads_sak, videja, colour = dala_nos)) + geom_line(linewidth = 0.9) + geom_point() +
  scale_colour_manual(NULL, values = c("1. daļa: zināšanas un pamatprasmes" = "#7a57c7", "2. daļa: teksta izpratne" = "#0b5394", "3. daļa: tekstveide" = "#e67e22")) +
  scale_x_continuous(breaks = 2008:2021, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "12. kl. latviešu valodas CE (VLL, 2008./09.–2021./22.): daļas", y = "vidējā izpilde %", x = "mācību gads", caption = CAPT) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path(OUT, "plots/81_ce_dalas.png"), p5a / p5b, width = 11, height = 9, dpi = 200, bg = "white")

# P6 sadalījums
sd_p <- SD[programma == "visi" & eks %in% c("VLL", "LATOL", "LAT09", "LV9", "MAT", "MAT09", "MATOL") & metode == "mikrodati"]
sd_p[, eks_nos := EKS_LBL[eks]]
sd_l <- melt(sd_p[, .(gads_sak, eks_nos, `zem 40 %` = zem40, `80 % un vairāk` = virs80, `P10` = p10, `P90` = p90)], id.vars = c("gads_sak", "eks_nos"))
p6 <- ggplot(sd_l, aes(gads_sak, value, colour = eks_nos)) + geom_line() + geom_point(size = 1.5) + facet_wrap(~ variable, scales = "free_y") +
  scale_colour_brewer(NULL, palette = "Dark2") + guides(colour = guide_legend(ncol = 2)) +
  scale_x_continuous(breaks = seq(2011, 2024, 1), labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "Sadalījuma gali: daļa zem 40 %, daļa 80 %+, P10 un P90 (kārtotāju līmeņa dati)", y = NULL, x = "mācību gads", caption = CAPT) + TEMA + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(file.path(OUT, "plots/81_ce_sadalijums.png"), p6, width = 12, height = 8, dpi = 200, bg = "white")

# P7 A − B
p7 <- ggplot(ABd[!is.na(A_B)], aes(gads_sak, A_B)) + geom_col(fill = KR["A"]) + geom_text(aes(label = round(A_B, 1)), vjust = -0.3, size = 3) +
  scale_x_continuous(breaks = 2008:2021, labels = function(x) sprintf("%02d/%02d", x %% 100, (x + 1) %% 100)) +
  labs(title = "12. kl. latviešu valodas CE: A − B (latviešu programmu skolas − mazākumtautību programmu skolas), tas pats darbs", y = "pp", x = "mācību gads",
       subtitle = "B plūsmai latviešu valoda ir otrā valoda; no 2020./21. mazākumtautību skolas pārkodētas kā 'abas programmas', tāpēc rinda beidzas 2019./20.", caption = CAPT) + TEMA
ggsave(file.path(OUT, "plots/81_ce_ab_starpiba.png"), p7, width = 10, height = 5.5, dpi = 200, bg = "white")
cat("Grafiki saglabāti: output/pisa2025/plots/81_ce_*.png\n")

# === 8. KOPSAVILKUMS =========================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("8. Kopsavilkums\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Mikrodatu rindas (latviešu valoda un matemātika, 9. un 12. kl.):", nrow(MD), "; gadi:", uniqueN(MD$gads_sak), "; skolas (nosaukumi):", uniqueN(MD$skola), "\n")
cat("Laikrindas rindas:", nrow(LR), "; sērijas paaudžu grafikā:", uniqueN(ser$serija), "\n")
cat("\nPISA paaudžu vietas (rangs no sērijas gadu skaita), 9. kl. latviešu valoda A lasīšana / 1. daļa:\n")
print(ser[grepl("lasīšana", serija) & pisa_paaudze != "", .(pisa_paaudze, dz_gads, gads, videja = round(videja, 1), z = round(z, 2), rangs, gadu_sk)])
cat("\n12. kl. latviešu valoda A kopā:\n")
print(ser[grepl("kopā, A", serija) & pisa_paaudze != "", .(pisa_paaudze, dz_gads, gads, videja = round(videja, 1), z = round(z, 2), rangs, gadu_sk)])

cat("\nSkripts pabeigts:", format(Sys.time()), "\n")
sink()
close(log_con)
