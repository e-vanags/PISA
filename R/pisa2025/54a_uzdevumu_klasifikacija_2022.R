# 54a. Lasīšanas uzdevumu klasifikācija no PISA 2022 tehniskā ziņojuma A pielikuma
#
# Avots: data/pisa_tehniskie/PISA2022_TR_AnnexA_items.xlsx (StatLink
# https://stat.link/5nw02t, PISA 2022 Technical Report, Annex A "Item Pool
# Classification tables"), lapas "Reading-CBA" (tabula A.4) un "Reading
# Fluency-CBA" (A.5). Uzdevumu saturs ir slepens; publiska ir tikai klasifikācija.
#
# Virsraksts ir divās rindās ar apvienotām šūnām; tās tiek apvienotas (1. rinda
# aizpildīta uz priekšu) un kolonnas atrastas pēc nosaukuma, ne pozīcijas.
# Uzdevuma ID CR560Q10S / DR545Q04C tiek pārveidots par mūsu nosaukumu CR560Q10.
#
# Izvade: output/pisa2025/uzdevumu_klasifikacija_2022.csv

f <- "data/pisa_tehniskie/PISA2022_TR_AnnexA_items.xlsx"
sh <- readxl::excel_sheets(f)

lasa <- function(lapa) {
  x <- as.data.frame(suppressMessages(readxl::read_excel(f, sheet = lapa, col_names = FALSE)))
  h1 <- as.character(unlist(x[1, ])); h2 <- as.character(unlist(x[2, ]))
  for (i in seq_along(h1)[-1]) if (is.na(h1[i])) h1[i] <- h1[i - 1]
  nm <- gsub("[[:space:]]+", " ", ifelse(is.na(h2), h1, paste(h1, h2, sep = ": ")))
  d <- x[-(1:2), , drop = FALSE]
  names(d) <- make.unique(nm)
  d <- d[!is.na(d[[1]]) & grepl("R[0-9]{3}Q", d[[1]]), , drop = FALSE]
  cat("==", lapa, ":", nrow(d), "uzdevumi\n   kolonnas:", paste(names(d), collapse = " || "), "\n")
  d
}
kol <- function(d, pat) {
  k <- grep(pat, names(d), value = TRUE)[1]
  # Tabulā ir gan "Multiple Choice - ...", gan "Multiple Choice  - ..." (divas atstarpes).
  if (is.na(k)) rep(NA_character_, nrow(d)) else trimws(gsub("[[:space:]]+", " ", as.character(d[[k]])))
}
standarts <- function(d, lapa) {
  data.frame(
    uzd = sub("^[CD]R(.*)[SC]$", "CR\\1", d[[1]]), id_2022 = d[[1]], lapa = lapa,
    vieniba = kol(d, "^Unit Name"), vienibas_tips = kol(d, "^Unit Type"),
    posms = kol(d, "^Stage$|^Stage:"), formats = kol(d, "^Item Format"),
    situacija = kol(d, "^Situation"), teksta_tips = kol(d, "^Text Type"),
    teksta_formats = kol(d, "^Text Format"), process = kol(d, "^Cognitive Process$"),
    process_skala = kol(d, "^Cognitive Process Subscale"), avots = kol(d, "^Source Subscale"),
    struktura = kol(d, "^Text Structure of Unit"), izcelsme = kol(d, "^Unit Origin$"),
    vardi = suppressWarnings(as.numeric(kol(d, "^Number of Words"))),
    slipums = suppressWarnings(as.numeric(kol(d, "Slope$"))),
    grutiba = suppressWarnings(as.numeric(kol(d, "Difficulty$"))),
    limenis = kol(d, "^Level"), stringsAsFactors = FALSE)
}

lr <- sh[grepl("^Reading", sh) & grepl("CBA", sh) & !grepl("Fluency", sh)][1]
lf <- sh[grepl("Fluency", sh)][1]
out <- rbind(standarts(lasa(lr), lr), standarts(lasa(lf), lf))

uzd <- utils::read.csv("output/pisa2025/kopigo_uzdevumu_etiketes.csv")$uzd
cat("\nKlasifikācijā kopā:", nrow(out), " no mūsu 196 atrasti:", sum(uzd %in% out$uzd), "\n")
cat("Neatrasti:", paste(setdiff(uzd, out$uzd), collapse = " "), "\n\n")
m <- out[out$uzd %in% uzd, ]
for (k in c("vienibas_tips", "posms", "formats", "process_skala", "avots", "teksta_formats", "struktura"))
  { cat(k, ":\n"); print(table(m[[k]], useNA = "ifany")) }
cat("grūtība (logit):", paste(round(stats::quantile(m$grutiba, c(0, .25, .5, .75, 1), na.rm = TRUE), 2), collapse = " "), "\n")
utils::write.csv(out, "output/pisa2025/uzdevumu_klasifikacija_2022.csv", row.names = FALSE)
cat("Saglabāts: output/pisa2025/uzdevumu_klasifikacija_2022.csv\n")
