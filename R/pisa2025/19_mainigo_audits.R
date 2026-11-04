# ============================================================================
# 19. Pilns PISA 2025 mainīgo audits pirms modelēšanas
# ============================================================================
#
# Mērķis. Pirms jebkāda sistēmas modeļa uzbūvēšanas noskaidrot, kas Latvijas
# 2025. gada datos vispār ir lietojams, cik tur ir gatavu indeksu, cik ir
# neapstrādātu anketas jautājumu, kuri bloki ir tukši un kas ir salīdzināms
# ar 2022. gadu.
#
# Šis skripts neko nemodelē. Tas sagatavo katalogu, pēc kura izlemj, ko likt
# modelī.
#
# Klasifikācija:
#   ident      identifikatori un administratīvie lauki
#   svars      galīgais svars un 80 replikātsvari
#   pv         plausible values, 11 domēni x 10
#   indekss    OECD atvasinātie rādītāji, galvenokārt WLE skalas
#   jautajums  neapstrādāti anketas jautājumi, grupēti blokos
#   laiks      atbilžu laiki no TT faila
#
# Ievade:  data/pisa2025/processed/pisa_lva_2025_{stu,sch,tt}.rds
#          data/pisa2022/processed/pisa_lva_2022_stu.rds
# Izvade:  output/pisa2025/mainigo_katalogs.csv
#          output/pisa2025/jautajumu_bloki.csv
#          output/pisa2025/logs/19_mainigo_audits_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/19_mainigo_audits_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

required_packages <- c("haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Pakotnes ielādētas.\n\n")


# === 1. DATI =================================================================
cat(strrep("=", 70), "\n")
cat("1. Dati\n")
cat(strrep("=", 70), "\n\n")

d <- list(
  stu = readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds"),
  sch = readRDS("data/pisa2025/processed/pisa_lva_2025_sch.rds"),
  tt  = readRDS("data/pisa2025/processed/pisa_lva_2025_tt.rds")
)
d22 <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")
mainigie_2022 <- names(d22)

for (nos in names(d)) {
  cat(sprintf("  %-4s %s rindas, %s kolonnas\n", nos,
              format(nrow(d[[nos]]), big.mark = " "), ncol(d[[nos]])))
}
cat("  2022 salīdzinājumam:", ncol(d22), "kolonnas\n\n")


# === 2. KLASIFIKĀCIJA ========================================================
cat(strrep("=", 70), "\n")
cat("2. Mainīgo klasifikācija\n")
cat(strrep("=", 70), "\n\n")

IDENT <- c("CNT", "CNTRYID", "CNTSCHID", "CNTSTUID", "CYC", "NatCen",
           "STRATUM", "SUBNATIO", "REGION", "OECD", "ADMINMODE", "BOOKID",
           "VER_DAT", "UNIT", "SENWT", "WVARSTRR", "LANGTEST_COG",
           "LANGTEST_QQQ", "Option_LDW", "Option_FLA", "Option_ICTQ",
           "Option_PQ", "Option_UH", "SCHLTYPE", "VER_SCH", "VER_STU")

klasifice <- function(nosaukumi) {
  k <- rep("indekss", length(nosaukumi))
  k[nosaukumi %in% IDENT] <- "ident"
  k[grepl("^W_", nosaukumi)] <- "svars"
  k[grepl("^PV[0-9]+", nosaukumi)] <- "pv"
  k[grepl("_TT$", nosaukumi)] <- "laiks"
  # Anketas jautājums ir divi burti, trīs cipari, burts Q, piemēram ST250Q01JA
  k[grepl("^[A-Z]{2}[0-9]{3}Q", nosaukumi)] <- "jautajums"
  # Atvasinātie jautājumu varianti, piemēram ST001D01T
  k[grepl("^[A-Z]{2}[0-9]{3}D", nosaukumi)] <- "jautajums"
  k
}

# Bloka sakne, proti, pirmie pieci simboli anketas jautājumiem.
bloka_sakne <- function(nosaukumi) {
  ifelse(grepl("^[A-Z]{2}[0-9]{3}", nosaukumi), substr(nosaukumi, 1, 5), NA)
}

# Jautājuma kopīgā daļa pirms domuzīmes etiķetē.
stumbrs <- function(et) {
  x <- sub(" - .*$", "", et)
  substr(x, 1, 120)
}

katalogs <- list()
for (nos in names(d)) {
  x <- d[[nos]]
  nm <- names(x)
  et <- vapply(x, function(v) {
    lab <- attr(v, "label")
    if (is.null(lab)) NA_character_ else as.character(lab)
  }, character(1))
  na_dala <- vapply(x, function(v) mean(is.na(v)), numeric(1))
  # Daļa labelled mainīgo zem etiķetēm glabā tekstu, tāpēc tos nedrīkst
  # pārvērst skaitļos. Šī funkcija tādus atgriež kā NA.
  skaitliski <- function(v) {
    b <- tryCatch(haven::zap_labels(v), error = function(e) v)
    if (!is.numeric(b)) return(rep(NA_real_, length(v)))
    as.numeric(b)
  }
  vid <- vapply(x, function(v) {
    z <- skaitliski(v)
    if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
  }, numeric(1))
  sd_ <- vapply(x, function(v) {
    z <- skaitliski(v)
    if (all(is.na(z))) NA_real_ else stats::sd(z, na.rm = TRUE)
  }, numeric(1))
  limeni <- vapply(seq_along(nm), function(i) {
    v <- x[[i]]
    length(unique(v[!is.na(v)]))
  }, integer(1))

  katalogs[[nos]] <- data.frame(
    avots = nos, mainigais = nm, kategorija = klasifice(nm),
    bloks = bloka_sakne(nm), etikete = et,
    na_dala = round(na_dala, 3), limeni = limeni,
    videjais = round(vid, 2), sd = round(sd_, 2),
    ir_2022 = nm %in% mainigie_2022,
    stringsAsFactors = FALSE, row.names = NULL)
}
kat <- do.call(rbind, katalogs)
kat$lietojams <- kat$na_dala < 0.5

cat("Visi mainīgie pa kategorijām un avotiem:\n")
print(table(kat$kategorija, kat$avots))
cat("\nLietojamie, proti, NA zem 50 %:\n")
print(table(kat$kategorija[kat$lietojams], kat$avots[kat$lietojams]))
cat("\n")

write.csv(kat, "output/pisa2025/mainigo_katalogs.csv", row.names = FALSE,
          fileEncoding = "UTF-8")


# === 3. INDEKSI ==============================================================
cat(strrep("=", 70), "\n")
cat("3. Gatavie indeksi, kas Latvijai ir lietojami\n")
cat(strrep("=", 70), "\n\n")

ind <- kat[kat$kategorija == "indekss" & kat$lietojams, ]
ind <- ind[order(ind$avots, ind$mainigais), ]
cat("Kopā:", nrow(ind), "no tiem jauni pret 2022:", sum(!ind$ir_2022), "\n\n")
print(ind[, c("avots", "mainigais", "etikete", "na_dala", "videjais", "sd",
              "ir_2022")], row.names = FALSE, right = FALSE)
cat("\n")


# === 4. JAUTĀJUMU BLOKI ======================================================
cat(strrep("=", 70), "\n")
cat("4. Anketas jautājumu bloki\n")
cat(strrep("=", 70), "\n\n")

jaut <- kat[kat$kategorija == "jautajums" & !is.na(kat$bloks), ]
bl <- do.call(rbind, lapply(split(jaut, list(jaut$avots, jaut$bloks),
                                  drop = TRUE), function(g) {
  data.frame(avots = g$avots[1], bloks = g$bloks[1],
             jautajumi = nrow(g),
             lietojami = sum(g$lietojams),
             na_min = min(g$na_dala), na_max = max(g$na_dala),
             ir_2022 = sum(g$ir_2022),
             stumbrs = stumbrs(g$etikete[1]),
             stringsAsFactors = FALSE)
}))
bl <- bl[order(bl$avots, -bl$lietojami), ]
write.csv(bl, "output/pisa2025/jautajumu_bloki.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

for (a in c("stu", "sch")) {
  b <- bl[bl$avots == a & bl$lietojami > 0, ]
  cat("--- ", a, ": ", nrow(b), " lietojami bloki, ", sum(b$lietojami),
      " jautājumi ---\n", sep = "")
  print(b[, c("bloks", "jautajumi", "lietojami", "ir_2022", "stumbrs")],
        row.names = FALSE, right = FALSE)
  cat("\n")
  tuksi <- bl[bl$avots == a & bl$lietojami == 0, ]
  if (nrow(tuksi) > 0) {
    cat("Tukšie bloki (Latvijai nav administrēti):",
        paste(tuksi$bloks, collapse = ", "), "\n\n")
  }
}


# === 5. NEPĀRTRAUKTĪBA AR 2022 ===============================================
cat(strrep("=", 70), "\n")
cat("5. Nepārtrauktība ar 2022. gadu\n")
cat(strrep("=", 70), "\n\n")

liet <- kat[kat$lietojams & kat$kategorija %in% c("indekss", "jautajums"), ]
cat("Lietojami saturiskie mainīgie 2025:", nrow(liet), "\n")
cat("  no tiem ar to pašu nosaukumu 2022:", sum(liet$ir_2022), "\n")
cat("  jauni 2025. ciklā:", sum(!liet$ir_2022), "\n\n")

cat("2022. gadā lietotie, kas 2025. gadā vairs nav lietojami:\n")
liet22 <- mainigie_2022[grepl("^[A-Z]{2}[0-9]{3}Q", mainigie_2022) |
                          mainigie_2022 %in% c("MATHEFF", "ANXMAT", "DISCLIM",
                                               "PQMCAR", "FAMCON", "ICTWKDY",
                                               "ESCS", "HOMEPOS", "HISEI")]
zuduši <- setdiff(liet22, kat$mainigais[kat$lietojams])
cat("  kopā", length(zuduši), "no", length(liet22), "\n\n")


# === 6. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("6. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")
cat("Izveidotie faili:\n")
cat("  output/pisa2025/mainigo_katalogs.csv\n")
cat("  output/pisa2025/jautajumu_bloki.csv\n")
cat("  ", output_file, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
