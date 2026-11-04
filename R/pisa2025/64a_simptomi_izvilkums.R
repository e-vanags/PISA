# ============================================================================
# 64a. Simptomu modelis (docs/46 Slānis 5, 2. kārta), 1. solis:
#      PISA 2015–2025 anketu mainīgie
# ============================================================================
#
# Visi skolēnu (2015 arī QQ2 papildfails) un skolu anketu mainīgie Latvijai un
# septiņām salīdzināmajām valstīm (EST, LTU, POL, FIN, CZE, HUN, DEU). Faili ir
# sakārtoti pēc valsts: vispirms nolasa tikai CNT, pēc tam katras valsts rindu bloku.
# Lietotāja trūkstošie kodi (95–99 u. c.) kļūst par NA (haven noklusējums).
#
# Izvade: data/pisa2025/processed/simptomi_<gads>.rds   list(stu, sch)
#         output/pisa2025/simptomi_inventars_jaunie.csv
# ============================================================================

VALSTIS <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
S25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
F <- list(
  "2015" = c(STU = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav", QQ2 = "data/pisa2015/raw/CY6_MS_CMB_STU_QQ2.sav",
             SCH = "data/pisa2015/raw/CY6_MS_CMB_SCH_QQQ.sav"),
  "2018" = c(STU = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav", SCH = "data/pisa2018/raw/SCH/CY07_MSU_SCH_QQQ.sav"),
  "2022" = c(STU = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV", SCH = "data/pisa2022/raw/CY08MSP_SCH_QQQ.SAV"),
  "2025" = c(STU = file.path(S25, "CY09_MS_STU_PUF.sav"), SCH = file.path(S25, "CY09_MS_SCH_PUF.sav")))

lasa_valstis <- function(f) {
  cnt <- as.character(haven::read_sav(f, col_select = "CNT")$CNT)
  bl <- lapply(VALSTIS, function(c0) { i <- which(cnt == c0); if (!length(i)) return(NULL)
    if (any(diff(i) != 1)) stop(basename(f), " ", c0, ": rindas nav vienā blokā"); range(i) })
  do.call(rbind, lapply(Filter(Negate(is.null), bl), function(b)
    haven::read_sav(f, skip = b[1] - 1, n_max = b[2] - b[1] + 1)))
}
etiketes <- function(d) vapply(d, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else paste(l, collapse = " ") }, "")

inv <- list()
for (g in names(F)) {
  out <- sprintf("data/pisa2025/processed/simptomi_%s.rds", g)
  t0 <- Sys.time()
  if (file.exists(out)) { X <- readRDS(out); cat(g, ": nolasīts kešs\n") } else {
    stu <- lasa_valstis(F[[g]][["STU"]])
    if ("QQ2" %in% names(F[[g]])) {
      q2 <- lasa_valstis(F[[g]][["QQ2"]]); q2 <- q2[, c("CNTSTUID", setdiff(names(q2), names(stu)))]
      stu <- merge(stu, q2, by = "CNTSTUID", all.x = TRUE, sort = FALSE)
    }
    sch <- lasa_valstis(F[[g]][["SCH"]])
    X <- list(stu = stu, sch = sch); saveRDS(X, out)
  }
  for (tips in c("stu", "sch")) {
    d <- X[[tips]]; cnt <- as.character(d$CNT); lv <- cnt == "LVA"; sal <- !lv
    lab <- etiketes(d)
    der <- vapply(d, function(x) !is.na(x) & (if (is.character(x)) nzchar(trimws(x)) else TRUE), logical(nrow(d)))
    inv[[length(inv) + 1]] <- data.frame(gads = g, fails = toupper(tips), mainigais = names(d), etikete = unname(lab),
      tips = vapply(d, function(x) if (is.character(x)) "a" else "num", ""), lva_n = sum(lv),
      lva_derigi_pct = round(100 * colMeans(der[lv, , drop = FALSE]), 1),
      sal_derigi_pct = round(100 * colMeans(der[sal, , drop = FALSE]), 1),
      sal_valstis_ar_datiem = vapply(seq_along(d), function(j) length(unique(cnt[sal & der[, j]])), 1L),
      lva_vertibu_skaits = vapply(d, function(x) length(unique(x[lv & !is.na(x)])), 1L), stringsAsFactors = FALSE)
  }
  cat(sprintf("%s: skolēni %s | skolas %d | mainīgie STU %d, SCH %d | %.1f min\n", g,
      paste(names(table(X$stu$CNT)), table(X$stu$CNT), collapse = " "), nrow(X$sch), ncol(X$stu), ncol(X$sch),
      as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
inv <- do.call(rbind, inv)
utils::write.csv(inv, "output/pisa2025/simptomi_inventars_jaunie.csv", row.names = FALSE)
cat("Inventārs: output/pisa2025/simptomi_inventars_jaunie.csv,", nrow(inv), "rindas\n")
