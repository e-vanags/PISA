# ============================================================================
# 14. PISA 2025 mikrodatu inventarizācija un Latvijas datu sagatavošana
# ============================================================================
#
# Mērķis. Pirms jebkādas analīzes noskaidrot, kas 2025. cikla publiskajos
# failos patiešām ir, proti, cik Latvijas skolēnu un skolu, kādi mainīgie,
# kuri no 2022. gada auditētajiem 18 mainīgajiem ir saglabājušies, un vai ir
# rādītāji, kas mēra testa pildīšanas iesaisti un lasīšanas fluenci.
#
# Ievade. Atarhivētie OECD publiskās lietošanas faili (.sav). Tie NAV Google
# Drive mapē, jo kopā aizņem vairākus GB. Ceļu norāda mainīgais SAV_DIR.
#
#   CY09_MS_STU_PUF.sav      skolēnu anketa, 755721 rinda, 90 valstis
#   CY09_MS_STU_TT_PUF.sav   skolēnu trenda fails
#   CY09_MS_SCH_PUF.sav      skolu anketa
#   CY09_MS_TCH_PUF.sav      skolotāju anketa
#
# Neietver COG un COG_PROCESS failus. Atarhivēti tie aizņem 13.9 GB un
# 12.3 GB, tāpēc tos apstrādā atsevišķi uz ārējā diska.
#
# Izvade.
#   data/pisa2025/processed/pisa_lva_2025_stu.rds
#   data/pisa2025/processed/pisa_lva_2025_tt.rds
#   data/pisa2025/processed/pisa_lva_2025_sch.rds
#   output/pisa2025/mainigie_2025.csv
#   output/pisa2025/mainigie_2022_pret_2025.csv
#   output/pisa2025/logs/14_datu_inventarizacija_output.txt
#
# Šis skripts neko neanalizē un neizdara secinājumus. Tas tikai konstatē,
# kas datos ir.
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("data/pisa2025/processed", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/14_datu_inventarizacija_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Pakotnes ielādētas.\n\n")


# === 1. KONFIGURĀCIJA ========================================================
cat(strrep("=", 70), "\n")
cat("1. Konfigurācija\n")
cat(strrep("=", 70), "\n\n")

# Atarhivēto .sav failu mape. Mainīt, ja faili ir citur, piemēram, uz ārējā diska.
SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", unset = "")
if (!nzchar(SAV_DIR)) {
  # Kandidātu vietas, kur meklēt atarhivētos .sav failus, ja vides mainīgais
  # nav uzstādīts. Failus tur ārpus Google Drive, jo tie aizņem vairākus GB.
  kandidati <- c(
    file.path(path.expand("~"), "pisa2025_sav"),
    file.path(path.expand("~"), "Documents", "pisa2025_sav"),
    "/Volumes/PISA/pisa2025_sav",
    file.path(tempdir(), "pisa2025"))
  atrasts <- kandidati[file.exists(file.path(kandidati, "CY09_MS_STU_PUF.sav"))]
  SAV_DIR <- if (length(atrasts)) atrasts[1] else kandidati[1]
}

VALSTS <- "LVA"

faili <- c(
  stu = "CY09_MS_STU_PUF.sav",
  tt  = "CY09_MS_STU_TT_PUF.sav",
  sch = "CY09_MS_SCH_PUF.sav",
  tch = "CY09_MS_TCH_PUF.sav"
)

cat("Failu mape: ", SAV_DIR, "\n")
cat("Mērķa valsts:", VALSTS, "\n\n")

for (nos in names(faili)) {
  ceļš <- file.path(SAV_DIR, faili[[nos]])
  cat(sprintf("  %-4s %-28s %s\n", nos, faili[[nos]],
              if (file.exists(ceļš))
                sprintf("%.2f GB", file.size(ceļš) / 1024^3)
              else "TRŪKST"))
}
cat("\n")

trūkstošie <- faili[!file.exists(file.path(SAV_DIR, faili))]
if (length(trūkstošie) > 0) {
  stop("Trūkst failu: ", paste(trūkstošie, collapse = ", "),
       ". Atarhivējiet tos vai norādiet citu SAV_DIR.")
}


# === 2. PALĪGFUNKCIJAS =======================================================

# Nolasa tikai vienas valsts rindas, nevis visu failu. PISA failos rindas ir
# kārtotas pēc valsts, tāpēc pietiek atrast pirmo un pēdējo rindu un nolasīt
# tikai to intervālu. Ja rindas nav nepārtrauktas, funkcija to pasaka un
# nolasa pilnu failu.
# Daļa 2025. cikla failu nav derīgā UTF-8 kodējumā, tāpēc read_sav bez
# norādīta kodējuma tiem atsakās strādāt. Tad lieto latin1. Tas atver failu,
# bet nelatīņu rakstzīmes etiķetēs var izskatīties nepareizi, ko skripts
# katrā tādā gadījumā pasaka.
lasa_sav <- function(ceļš, ...) {
  r <- try(read_sav(ceļš, ...), silent = TRUE)
  if (!inherits(r, "try-error")) return(r)
  cat("  Piezīme: fails nav derīgs UTF-8, lasu ar latin1 kodējumu.\n")
  read_sav(ceļš, ..., encoding = "latin1")
}

nolasa_valsti <- function(ceļš, valsts = VALSTS) {
  cnt <- lasa_sav(ceļš, col_select = "CNT")
  idx <- which(as.character(cnt$CNT) == valsts)
  if (length(idx) == 0) return(NULL)

  nepārtraukti <- identical(idx, seq(min(idx), max(idx)))
  if (nepārtraukti) {
    d <- lasa_sav(ceļš, skip = min(idx) - 1L, n_max = length(idx))
  } else {
    cat("  Piezīme: rindas nav nepārtrauktas, nolasu pilnu failu.\n")
    d <- lasa_sav(ceļš)
    d <- d[as.character(d$CNT) == valsts, ]
  }
  attr(d, "rindas_kopa") <- nrow(cnt)
  attr(d, "valstu_skaits") <- length(unique(cnt$CNT))
  d
}

# Mainīgo saraksts ar SPSS etiķetēm.
vardnica_no <- function(d, avots) {
  data.frame(
    avots     = avots,
    mainigais = names(d),
    etikete   = vapply(d, function(v) {
      lab <- attr(v, "label")
      if (is.null(lab)) NA_character_ else as.character(lab)
    }, character(1)),
    tips      = vapply(d, function(v) class(v)[1], character(1)),
    na_dala   = round(vapply(d, function(v) mean(is.na(v)), numeric(1)), 3),
    stringsAsFactors = FALSE, row.names = NULL
  )
}


# === 3. LATVIJAS DATU IZGŪŠANA ===============================================
cat(strrep("=", 70), "\n")
cat("3. Latvijas datu izgūšana\n")
cat(strrep("=", 70), "\n\n")

dati <- list()
vardnica <- list()

for (nos in names(faili)) {
  cat("Fails:", faili[[nos]], "\n")
  t0 <- Sys.time()
  d <- nolasa_valsti(file.path(SAV_DIR, faili[[nos]]))

  if (is.null(d)) {
    cat("  Latvija šajā failā NAV.\n\n")
    next
  }

  cat(sprintf("  Kopā failā: %s rindas, %s valstis\n",
              format(attr(d, "rindas_kopa"), big.mark = " "),
              attr(d, "valstu_skaits")))
  cat(sprintf("  Latvija:    %s rindas, %s kolonnas\n",
              format(nrow(d), big.mark = " "), ncol(d)))
  if ("CNTSCHID" %in% names(d)) {
    cat(sprintf("  Skolas:     %s\n", length(unique(d$CNTSCHID))))
  }
  cat(sprintf("  Laiks:      %.1f s\n\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  dati[[nos]] <- d
  vardnica[[nos]] <- vardnica_no(d, nos)
}


# === 4. SAGLABĀŠANA ==========================================================
cat(strrep("=", 70), "\n")
cat("4. Saglabāšana\n")
cat(strrep("=", 70), "\n\n")

for (nos in names(dati)) {
  ceļš <- file.path("data/pisa2025/processed",
                    sprintf("pisa_lva_2025_%s.rds", nos))
  saveRDS(dati[[nos]], ceļš)
  cat(sprintf("  %-45s %.1f MB\n", ceļš, file.size(ceļš) / 1024^2))
}
cat("\n")

vardnica_visa <- do.call(rbind, vardnica)
write.csv(vardnica_visa, "output/pisa2025/mainigie_2025.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  output/pisa2025/mainigie_2025.csv:", nrow(vardnica_visa),
    "mainīgie\n\n")


# === 5. SALĪDZINĀJUMS AR 2022. GADA MAINĪGAJIEM ==============================
cat(strrep("=", 70), "\n")
cat("5. Vai 2022. gada auditētie mainīgie ir arī 2025. gadā\n")
cat(strrep("=", 70), "\n\n")

# 18 mainīgie no BN vadītā modeļa, sk. docs/06_projekta_stavoklis.md
auditetie_2022 <- c(
  "MATHEFF", "ST268Q04JA", "ANXMAT",
  "HOMEPOS", "HISEI", "ST006Q03JA", "ST256Q03JA",
  "PQMCAR",
  "ST294Q03JA", "ST272Q01JA", "FAMCON", "ST290Q09WA",
  "IC173Q04JA", "ICTWKDY",
  "DISCLIM", "NEGSCLIM",
  "PV1MATH", "PV1READ"
)

visi_2025 <- unique(vardnica_visa$mainigais)

salidz <- data.frame(
  mainigais_2022 = auditetie_2022,
  ir_2025 = auditetie_2022 %in% visi_2025,
  avots_2025 = vapply(auditetie_2022, function(m) {
    a <- vardnica_visa$avots[vardnica_visa$mainigais == m]
    if (length(a) == 0) NA_character_ else paste(unique(a), collapse = ",")
  }, character(1)),
  stringsAsFactors = FALSE, row.names = NULL
)

print(salidz, row.names = FALSE)
cat("\nSaglabājušies:", sum(salidz$ir_2025), "no", nrow(salidz), "\n")
if (any(!salidz$ir_2025)) {
  cat("Nav atrasti:", paste(salidz$mainigais_2022[!salidz$ir_2025],
                            collapse = ", "), "\n")
  cat("PIEZĪME. Trūkstošs nosaukums nenozīmē, ka konstrukts ir pazudis.",
      "Starp cikliem mainīgos pārsauc. Katrs trūkstošais jāpārbauda pret",
      "2025. gada codebook.\n")
}
cat("\n")

write.csv(salidz, "output/pisa2025/mainigie_2022_pret_2025.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# === 6. IESAISTES, LAIKA UN FLUENCES RĀDĪTĀJU MEKLĒŠANA ======================
cat(strrep("=", 70), "\n")
cat("6. Kandidāti iesaistes, laika un fluences mērīšanai\n")
cat(strrep("=", 70), "\n\n")

atslegvardi <- list(
  laiks     = "TIME|TIMING|SECOND|DURATION|MINUT",
  iesaiste  = "ENGAG|DISENG|EFFORT|RAPID|GUESS|SPEED|OMIT|NOTREACH|NOT_REACH",
  fluence   = "FLUEN|FLUENC|RFA|DECOD|WORD",
  piepule   = "ST0?3[0-9]Q|EFFORT"
)

for (grupa in names(atslegvardi)) {
  trap <- vardnica_visa[
    grepl(atslegvardi[[grupa]], vardnica_visa$mainigais, ignore.case = TRUE) |
      grepl(atslegvardi[[grupa]], vardnica_visa$etikete, ignore.case = TRUE), ]
  cat("--- ", grupa, ": ", nrow(trap), " trāpījumi ---\n", sep = "")
  if (nrow(trap) > 0) {
    r <- utils::head(trap[, c("avots", "mainigais", "etikete", "na_dala")], 40)
    print(r, row.names = FALSE)
    if (nrow(trap) > 40) cat("  ... vēl", nrow(trap) - 40, "\n")
  }
  cat("\n")
}


# === 7. PLAUSIBLE VALUES UN SVARI ============================================
cat(strrep("=", 70), "\n")
cat("7. Plausible values un svari\n")
cat(strrep("=", 70), "\n\n")

pv <- grep("^PV[0-9]+", visi_2025, value = TRUE)
domeni <- unique(sub("^PV[0-9]+", "", pv))
cat("PV mainīgie:", length(pv), "\n")
cat("Domēni:", paste(domeni, collapse = ", "), "\n\n")

svari <- grep("^W_", visi_2025, value = TRUE)
cat("Svaru mainīgie:", length(svari), "\n")
cat(paste(utils::head(svari, 20), collapse = ", "), "\n")
if (length(svari) > 20) cat("... vēl", length(svari) - 20, "\n")
cat("\n")


# === 8. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("8. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")

for (nos in names(dati)) {
  cat(sprintf("  %-4s %s rindas, %s kolonnas\n", nos,
              format(nrow(dati[[nos]]), big.mark = " "), ncol(dati[[nos]])))
}
cat("\nIzveidotie faili:\n")
cat("  data/pisa2025/processed/pisa_lva_2025_*.rds\n")
cat("  output/pisa2025/mainigie_2025.csv\n")
cat("  output/pisa2025/mainigie_2022_pret_2025.csv\n")
cat("  ", output_file, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")


# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
