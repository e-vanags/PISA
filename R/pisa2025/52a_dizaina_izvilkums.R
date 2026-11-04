# 52a. Lasīšanas testa dizaina mainīgie, Latvija 2022 un 2025 (docs/51, 6. solis)
#
# RDESIGN (lasīšanas adaptīvā dizaina variants), RCORE_TEST un RCORE_PERF
# (kodola posms un sniegums tajā, pēc kā notiek maršrutēšana), BOOKID un
# ADMINMODE. Tie ir tikai COG failos, un 2025. gada COG fails ir 14 GB, tāpēc
# atsevišķs izvilkums ar kešu.
#
# Izvade: data/pisa2025/processed/lasisanas_dizains.rds

suppressPackageStartupMessages(library(haven))

V <- c("CNT", "CNTSTUID", "BOOKID", "ADMINMODE", "RDESIGN", "RCORE_TEST", "RCORE_PERF")
SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
F <- c("2022" = "data/pisa2022/raw/CY08MSP_STU_COG.SAV",
       "2025" = file.path(SAV_DIR, "CY09_MS_COG_20260806.sav"))

out <- list()
for (g in names(F)) {
  v <- intersect(V, names(read_sav(F[[g]], n_max = 0)))
  d <- read_sav(F[[g]], col_select = tidyselect::all_of(v))
  d <- d[as.character(d$CNT) == "LVA", ]
  x <- data.frame(gads = g, CNTSTUID = as.numeric(d$CNTSTUID))
  for (k in setdiff(v, c("CNT", "CNTSTUID"))) {
    x[[k]] <- if (is.character(d[[k]])) as.character(d[[k]]) else as.numeric(d[[k]])
    if (inherits(d[[k]], "haven_labelled")) x[[paste0(k, "_LAB")]] <- as.character(as_factor(d[[k]]))
  }
  out[[g]] <- x
  cat(g, ":", nrow(x), "Latvijas skolēni\n")
  for (k in setdiff(v, c("CNT", "CNTSTUID"))) {
    z <- if (paste0(k, "_LAB") %in% names(x)) x[[paste0(k, "_LAB")]] else x[[k]]
    cat("  ", k, ":\n")
    print(utils::head(sort(table(z, useNA = "ifany"), decreasing = TRUE), 12))
  }
}
saveRDS(out, "data/pisa2025/processed/lasisanas_dizains.rds")
cat("Saglabāts: data/pisa2025/processed/lasisanas_dizains.rds\n")
