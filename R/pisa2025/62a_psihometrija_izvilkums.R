# ============================================================================
# 62a. Lasīšanas uzdevumu dati psihometrijai: 2015, 2018, 2022, 2025
#      Latvija un Igaunija (docs/46 Slānis 6; docs/59)
# ============================================================================
#
# Visi vērtētie lasīšanas uzdevumi (CR…S; DR…C bez apakšdaļas burta, kā 53a),
# atbilžu izvēles uzdevumu neapstrādātās atbildes (DR…R, varianti A–D = 1–4),
# testa valoda un adaptīvā dizaina mainīgie. Igaunija ir salīdzinājumam: tai arī
# ir krievu testa versija (2015–2022), tāpēc tā ir dabisks etalons Latvijas
# valodu versiju ekvivalencei.
#
# Faili ir sakārtoti pēc valsts. Vispirms nolasa tikai CNT, pēc tam tikai LVA un
# EST rindu blokus (skip / n_max), lai 2025. gada 14 GB failu nevajadzētu turēt
# atmiņā.
#
# KODĒJUMS matricā K (vesels skaitlis), pēc etiķetes teksta kā 00_kopigas_funkcijas.R:
#    0 bez kredīta, 1 daļējs kredīts, 2 pilns kredīts (vienpunkta uzdevumos pilns = 1),
#   -1 neatbildēts (PISA vērtē kā 0), -2 nav sasniegts (PISA nevērtē),
#   -3 nederīgs / tehniska kļūda, NA nav uzdots.
# Neapstrādātā atbilde R: 1–6 izvēlētais variants (A–F), 9 neatbildēts, NA citādi.
#
# Izvade: data/pisa2025/processed/psihometrija_<gads>.rds
#         list(sk, K, mx, R, uzd, avots)
# ============================================================================

suppressPackageStartupMessages(library(haven))

VALSTIS <- c("LVA", "EST")
SAV25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_COG.sav",
       "2018" = "data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav",
       "2022" = "data/pisa2022/raw/CY08MSP_STU_COG.SAV",
       "2025" = file.path(SAV25, "CY09_MS_COG_20260806.sav"))
SK <- c("CNT", "CNTSTUID", "BOOKID", "LANGTEST_COG", "ADMINMODE", "Option_Read", "RDESIGN",
        "RCORE_TEST", "RCORE_PERF", "RS1_TEST", "RS1_LEV", "RS2_TEST", "RS2_LEV")

vertetie <- function(n) {
  k1 <- sub("^CR(.*)S$", "\\1", grep("^CR[0-9]{3}Q[0-9]{2}[A-Z]?S$", n, value = TRUE))
  k2 <- sub("^DR(.*)C$", "\\1", grep("^DR[0-9]{3}Q[0-9]{2}C$", n, value = TRUE))
  c(stats::setNames(paste0("CR", k1, "S"), k1),
    stats::setNames(paste0("DR", setdiff(k2, k1), "C"), setdiff(k2, k1)))
}
kods <- function(x) {
  lab <- as.character(as_factor(x)); o <- rep(NA_integer_, length(lab))
  o[grepl("No credit", lab, ignore.case = TRUE)] <- 0L
  o[grepl("Partial credit", lab, ignore.case = TRUE)] <- 1L
  o[grepl("Full credit", lab, ignore.case = TRUE)] <- 2L
  o[grepl("No Response", lab, ignore.case = TRUE)] <- -1L
  o[grepl("Not Reached", lab, ignore.case = TRUE)] <- -2L
  o[grepl("Invalid|Technical", lab, ignore.case = TRUE)] <- -3L
  o
}
raw_kods <- function(x) {
  lab <- trimws(as.character(as_factor(x))); o <- rep(NA_integer_, length(lab))
  for (k in 1:6) o[lab == LETTERS[k]] <- k
  o[grepl("No Response", lab, ignore.case = TRUE)] <- 9L
  o
}
lab_chr <- function(x) if (inherits(x, "haven_labelled")) as.character(as_factor(x)) else as.character(x)

for (g in names(F)) {
  out <- sprintf("data/pisa2025/processed/psihometrija_%s.rds", g)
  if (file.exists(out)) { cat(g, ": jau ir", out, "\n"); next }
  t0 <- Sys.time()
  n <- names(read_sav(F[[g]], n_max = 0))
  v <- vertetie(n)
  raw <- intersect(paste0("DR", names(v), "R"), n); names(raw) <- sub("^DR(.*)R$", "\\1", raw)
  kols <- c(intersect(SK, n), unname(v), unname(raw))
  cnt <- as.character(read_sav(F[[g]], col_select = "CNT")$CNT)
  bl <- lapply(VALSTIS, function(c0) { i <- which(cnt == c0); if (!length(i)) NULL else {
    if (any(diff(i) != 1)) stop(g, " ", c0, ": rindas nav vienā blokā"); range(i) } })
  d <- do.call(rbind, lapply(Filter(Negate(is.null), bl), function(b)
    read_sav(F[[g]], user_na = TRUE, col_select = tidyselect::all_of(kols), skip = b[1] - 1, n_max = b[2] - b[1] + 1)))
  rm(cnt)
  K <- vapply(d[unname(v)], kods, integer(nrow(d))); colnames(K) <- names(v)
  poly <- apply(K, 2, function(x) any(x == 1L, na.rm = TRUE))
  for (j in which(!poly)) K[K[, j] %in% 2L, j] <- 1L
  mx <- ifelse(poly, 2L, 1L)
  R <- if (length(raw)) { r <- vapply(d[unname(raw)], raw_kods, integer(nrow(d))); colnames(r) <- names(raw)
    r[, colSums(r >= 1L & r <= 6L, na.rm = TRUE) > 0, drop = FALSE] } else NULL
  sk <- data.frame(CNT = as.character(d$CNT), CNTSTUID = as.numeric(d$CNTSTUID), stringsAsFactors = FALSE)
  for (k in setdiff(intersect(SK, names(d)), c("CNT", "CNTSTUID"))) {
    sk[[k]] <- suppressWarnings(as.numeric(haven::zap_labels(d[[k]]))); sk[[paste0(k, "_LAB")]] <- lab_chr(d[[k]]) }
  saveRDS(list(sk = sk, K = K, mx = mx, R = R, uzd = names(v), avots = v), out)
  adm <- colSums(!is.na(K))
  cat(sprintf("%s: %s | uzdevumi %d (daļēja kredīta %d), ar neapstrādātu atbildi %d | uzdoti vismaz 100 reizes %d | %.1f min\n",
      g, paste(names(table(sk$CNT)), table(sk$CNT), sep = " ", collapse = ", "), ncol(K), sum(poly),
      if (is.null(R)) 0 else ncol(R), sum(adm >= 100), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat("   testa valoda:", paste(names(table(paste(sk$CNT, sk$LANGTEST_COG_LAB))), table(paste(sk$CNT, sk$LANGTEST_COG_LAB)), collapse = "; "), "\n")
  cat("   kodu sadalījums (visas uzdotās atbildes):", paste(names(table(K)), table(K), sep = ":", collapse = " "), "\n")
}
