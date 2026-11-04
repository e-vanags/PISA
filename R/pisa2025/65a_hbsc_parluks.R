# ============================================================================
# 65a. HBSC datu pārlūka apkopojumi: simptomi 11, 13 un 15 gados (docs/61)
# ============================================================================
#
# Ievade: data/hbsc/hbsc_browser_data.csv, hbsc_browser_metadata.csv
#         (HBSC Data Browser, 2014, 2018, 2022; lejupielādēts 2026-09-12)
# Kolonnas: HBSC_id, Country, Survey_year, Age_group, Girl, Boy, sig_gender,
#   sort_gender (abu dzimumu vidējais), Age_group_total, Measure_type.
#
# Izvade: data/hbsc/hbsc_simptomi_8valstis.csv (garā tabula LVA + 7 valstīm)
#         output/pisa2025/logs/65a_hbsc_parluks_output.txt
# Paaudzes: 2022. gada 13 gadīgie ≈ PISA 2025; 2018. gada 11 gadīgie ≈ PISA 2022;
# 2018. gada 15 gadīgie un 2014. gada 11 gadīgie ≈ PISA 2018.
# ============================================================================

while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/65a_hbsc_parluks_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

d <- utils::read.csv("data/hbsc/hbsc_browser_data.csv", stringsAsFactors = FALSE, check.names = FALSE)[, 1:10]
d <- d[nzchar(d$HBSC_id) & nzchar(d$Country), ]
m <- utils::read.csv("data/hbsc/hbsc_browser_metadata.csv", stringsAsFactors = FALSE, check.names = FALSE)
m <- m[nzchar(m$HBSC_id), c("HBSC_id", "Short_name", "Measure_type", "Measure_description")]
VALSTIS <- c(Latvia = "LVA", Estonia = "EST", Lithuania = "LTU", Poland = "POL", Finland = "FIN",
             Czechia = "CZE", Hungary = "HUN", Germany = "DEU")
d <- d[d$Country %in% names(VALSTIS), ]
d$cnt <- VALSTIS[d$Country]; d$vecums <- as.integer(sub("-year-olds", "", d$Age_group))
num <- function(x) suppressWarnings(as.numeric(x))
d$meitenes <- num(d$Girl); d$zeni <- num(d$Boy); d$kopa <- num(d$sort_gender)
d$radītājs <- m$Short_name[match(d$HBSC_id, m$HBSC_id)]
out <- d[, c("cnt", "Survey_year", "vecums", "HBSC_id", "radītājs", "Measure_type", "meitenes", "zeni", "kopa")]
names(out)[2] <- "gads"
utils::write.csv(out, "data/hbsc/hbsc_simptomi_8valstis.csv", row.names = FALSE)
cat("Rindas (8 valstis):", nrow(out), "| rādītāji:", length(unique(out$HBSC_id)), "\n")
cat("Pieejamība Latvijai pa gadiem:\n"); print(table(out$gads[out$cnt == "LVA"]))

GALVENIE <- c("School satisfaction", "Schoolwork pressure", "Teacher support", "Classmate support", "Peer support",
              "Family support", "Problematic social media use", "Continuous online contact", "Life satisfaction",
              "WHO-5", "Feeling lonely", "Multiple health complaints", "sleep difficulties", "feeling low",
              "Bullying: being bullied", "Cyberbullying: being bullied", "Breakfast", "Moderate-to-vigorous", "Self-efficacy")
cat("\n", strrep("=", 100), "\nGalvenie simptomi: Latvija pret 7 valstu vidējo (abi dzimumi; % vai vidējais)\n", strrep("=", 100), "\n", sep = "")
for (g in GALVENIE) {
  ids <- unique(out$HBSC_id[grepl(g, out$radītājs, ignore.case = TRUE)])
  for (id in ids) {
    x <- out[out$HBSC_id == id, ]
    cat(sprintf("\n  %s [%s, %s]\n", x$radītājs[1], id, x$Measure_type[1]))
    for (v in c(11, 13, 15)) {
      cel <- vapply(c(2014, 2018, 2022), function(y) {
        l <- x$kopa[x$cnt == "LVA" & x$vecums == v & x$gads == y]; s <- x$kopa[x$cnt != "LVA" & x$vecums == v & x$gads == y]
        if (!length(l) || is.na(l)) return(sprintf("%16s", "–"))
        sprintf("%5.1f (%+5.1f)  ", l, l - mean(s, na.rm = TRUE)) }, "")
      cat(sprintf("    %2d gadi: %s\n", v, paste(cel, collapse = "")))
    }
  }
}
cat("\n  Formāts: Latvija (Latvija − 7 valstu vidējais) 2014 | 2018 | 2022.\n",
    " Paaudzes: 13 g. 2022 ≈ PISA 2025; 11 g. 2018 ≈ PISA 2022; 15 g. 2018 ≈ PISA 2018.\n")
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
