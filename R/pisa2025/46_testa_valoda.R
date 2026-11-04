# ============================================================================
# 46. Testa valoda pa plūsmām, PISA 2015-2025
# ============================================================================
#
# JAUTĀJUMS. Kurā ciklā B plūsmas skolēni sāka pildīt PISA testu latviešu
# valodā? Bez tā B plūsmas 2022 -> 2025 pārmaiņu nevar interpretēt, jo tajā
# vienlaikus mainās gan lasītprasme, gan testa valoda.
#
# MAINĪGAIS. `LANGTEST_COG`, kognitīvā testa valoda, pieejama no PISA 2015.
# Vecajos ciklos 2003-2012 testa valoda NAV reģistrēta: skolēnu failos tās
# nav, uzdevumu failos ir tikai bukleta numurs `BOOKID`, kas abās valodās ir
# vienāds. PISA 2000 uzdevumu failā `SUBNATIO` ir etiķete "Language community
# ID", bet Latvijai tā visiem ir "00".
#
# Plūsmas pēc `LANGN`: 325 latviešu, 495 krievu, tāpat kā skriptā 43.
# Īpatsvari svērti ar `W_FSTUWT`.
#
# Izvade:  output/pisa2025/testa_valoda.csv
#          output/pisa2025/logs/46_testa_valoda_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/46_testa_valoda_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)

library(haven)
FAILI <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav",
           "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
           "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV")
nolasa <- function(g) {
  if (g == "2025") {
    d <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")
    names(d) <- toupper(names(d))
    return(d[, c("CNT", "LANGN", "LANGTEST_COG", "W_FSTUWT")])
  }
  d <- read_sav(FAILI[[g]], col_select = c("CNT", "LANGN", "LANGTEST_COG", "W_FSTUWT"))
  d[as.character(d$CNT) == "LVA", ]
}

rez <- list()
cat(sprintf("%-5s %-6s %6s %11s %11s\n", "gads", "grupa", "n", "latviski %", "krieviski %"))
for (g in c("2015", "2018", "2022", "2025")) {
  d <- nolasa(g)
  ln <- as.numeric(d$LANGN)
  tl <- as.character(as_factor(d$LANGTEST_COG))
  w <- as.numeric(d$W_FSTUWT)
  gr <- ifelse(ln == 325, "A", ifelse(ln == 495, "B", NA))
  for (k in c("A", "B")) {
    i <- !is.na(gr) & gr == k & !is.na(tl)
    lv <- 100 * sum(w[i & grepl("Latvian", tl)]) / sum(w[i])
    ru <- 100 * sum(w[i & grepl("Russian", tl)]) / sum(w[i])
    cat(sprintf("%-5s %-6s %6d %10.1f %% %10.1f %%\n", g, k, sum(i), lv, ru))
    rez[[length(rez) + 1]] <- data.frame(gads = g, grupa = k, n = sum(i),
                                         latviski = lv, krieviski = ru)
  }
}
cat("\n2003-2012: testa valoda datos nav reģistrēta.\n")
write.csv(do.call(rbind, rez), "output/pisa2025/testa_valoda.csv", row.names = FALSE)
cat("Izveidots: output/pisa2025/testa_valoda.csv\n")
while (sink.number() > 0) sink()
close(log_con)
