# ============================================================================
# 69b. Slānis 11: simptomu skalu mērījuma invariance starp cikliem un valstīm
# ============================================================================
#
# Vai anketas konstrukti mēra to pašu 2003. un 2025. gadā (un Latvijā pret citām valstīm)?
# Tas ir priekšnoteikums tīkla modelim (69c) un konstruktu salīdzināšanai laikā.
#
# Skalas (vairāku vienumu, saskaņotas 64d/64e virzienā):
#   PIEDERIBA: K_PIED_* (piederība skolai; 5 vienumi 2003–2025, 6 no 2012);
#   ATTIEKSME: E_SKOLA_* (attieksme pret skolu; 4 vienumi 2003, 2009, 2012, 2022, 2025).
# Metode: daudzgrupu CFA (lavaan, WLSMV, ordinālie vienumi): konfigurālā → metriskā
#   (slodzes) → skalārā (sliekšņi) invariance; kritērijs ΔCFI ≤ 0,01 un ΔRMSEA ≤ 0,015
#   (Chen 2007). Grupas: (a) LVA A pa cikliem; (b) 2025. gadā 8 valstis (LVA = A plūsma).
#   Bez svariem (WLSMV ar svariem lavaan neatbalsta); izlase uz ≤ 3000 skolēniem grupā.
# Papildus: daļējā invariance — kurš vienums pārkāpj (modifikācijas indeksi skalārajā).
#
# Izvade: output/pisa2025/invariance_kopsavilkums.csv, invariance_vienumi.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(lavaan) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/69b_invariance_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n"); set.seed(20260913)

A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
SK <- list(PIEDERIBA = c("K_PIED_ATSTUMTS", "K_PIED_DRAUGI", "K_PIED_NEVEIKLS", "K_PIED_PIEDERU", "K_PIED_VIENTULS", "K_PIED_PATIK_CITIEM"),
           ATTIEKSME = c("E_SKOLA_DEVA_PARLIECIBU", "E_SKOLA_LAIKA_IZSKIESANA", "E_SKOLA_MAZ_SAGATAVO", "E_SKOLA_NODERIGA_DARBAM"))
sagat <- function(d, v) { d <- copy(d)[, c("grupa_cfa", v), with = FALSE]
  for (k in v) { x <- d[[k]]; x[!is.na(x) & (x < 1 | x > 4)] <- NA; set(d, j = k, value = x) }
  d <- d[rowSums(!is.na(d[, ..v])) == length(v)]
  d[, .SD[sample(.N, min(.N, 3000))], by = grupa_cfa] }
izpilde <- function(d, v, nos) {
  ok <- character(); for (k in v) { u <- d[, uniqueN(na.omit(get(k))), by = grupa_cfa]$V1; if (length(u) && all(u >= 3)) ok <- c(ok, k) }
  if (length(ok) < 3) { cat("  ", nos, ": par maz vienumu (", length(ok), ")\n"); return(NULL) }
  mod <- paste("F =~", paste(ok, collapse = " + "))
  fits <- list()
  for (lv in c("configural", "metric", "scalar")) {
    ge <- switch(lv, configural = character(), metric = "loadings", scalar = c("loadings", "thresholds"))
    fits[[lv]] <- tryCatch(cfa(mod, data = as.data.frame(d), group = "grupa_cfa", ordered = ok, estimator = "WLSMV", group.equal = ge, parameterization = "theta"),
                           error = function(e) { cat("   kļūda", lv, ":", conditionMessage(e), "\n"); NULL })
  }
  fm <- rbindlist(lapply(names(fits), function(lv) { f <- fits[[lv]]; if (is.null(f)) return(NULL)
    m <- fitMeasures(f, c("cfi.scaled", "rmsea.scaled", "srmr", "chisq.scaled", "df.scaled"))
    data.table(skala = nos, limenis = lv, cfi = round(m[["cfi.scaled"]], 3), rmsea = round(m[["rmsea.scaled"]], 3), srmr = round(m[["srmr"]], 3), chisq = round(m[["chisq.scaled"]], 1), df = m[["df.scaled"]]) }))
  fm[, `:=`(d_cfi = c(NA, diff(cfi)), d_rmsea = c(NA, diff(rmsea)))][, iztur := ifelse(is.na(d_cfi), NA, d_cfi >= -0.01 & d_rmsea <= 0.015)]
  fm[, `:=`(vienumi = length(ok), grupas = uniqueN(d$grupa_cfa), n = nrow(d))]
  print(fm, row.names = FALSE)
  # Kurš vienums pārkāpj skalāro invarianci: lielākie modifikācijas indeksi sliekšņiem un slodzēm
  vi <- NULL
  if (!is.null(fits$scalar)) { mi <- tryCatch(as.data.table(modindices(fits$scalar))[op %in% c("=~", "|")], error = function(e) NULL)
    if (!is.null(mi) && nrow(mi)) { vi <- mi[, .(mi = sum(mi)), by = .(vienums = ifelse(op == "=~", rhs, lhs), grupa = group)][order(-mi)]
      vi[, skala := nos]; cat("  Vienumi ar lielāko invariance pārkāpumu (MI summa):\n"); print(head(vi, 8), row.names = FALSE) } }
  list(fm = fm, vi = vi)
}
OUT <- list(); VI <- list()
for (sn in names(SK)) {
  v <- SK[[sn]]
  cat("\n", strrep("=", 90), "\n", sn, ": (a) latviešu valodas plūsma pa cikliem\n", strrep("=", 90), "\n", sep = "")
  d <- A[grupa == "LVA A"][, grupa_cfa := gads]
  if (sn == "PIEDERIBA") d <- d[gads != "2000"]
  d <- sagat(d, v[v %in% names(d)]); d <- d[, if (.N >= 300) .SD, by = grupa_cfa]
  x <- izpilde(d, v[v %in% names(d)], paste(sn, "cikli")); if (!is.null(x)) { OUT[[length(OUT) + 1]] <- x$fm; VI[[length(VI) + 1]] <- x$vi }
  cat("\n", strrep("=", 90), "\n", sn, ": (b) 2025. gads pa valstīm (LVA = A plūsma)\n", strrep("=", 90), "\n", sep = "")
  d <- A[gads == "2025" & grupa %in% c("LVA A", "CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")][, grupa_cfa := grupa]
  d <- sagat(d, v[v %in% names(d)]); d <- d[, if (.N >= 300) .SD, by = grupa_cfa]
  x <- izpilde(d, v[v %in% names(d)], paste(sn, "valstis 2025")); if (!is.null(x)) { OUT[[length(OUT) + 1]] <- x$fm; VI[[length(VI) + 1]] <- x$vi }
  cat("\n", strrep("=", 90), "\n", sn, ": (c) 2022. gads pa valstīm\n", strrep("=", 90), "\n", sep = "")
  d <- A[gads == "2022" & grupa %in% c("LVA A", "CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")][, grupa_cfa := grupa]
  d <- sagat(d, v[v %in% names(d)]); d <- d[, if (.N >= 300) .SD, by = grupa_cfa]
  x <- izpilde(d, v[v %in% names(d)], paste(sn, "valstis 2022")); if (!is.null(x)) { OUT[[length(OUT) + 1]] <- x$fm; VI[[length(VI) + 1]] <- x$vi }
}
fwrite(rbindlist(OUT), "output/pisa2025/invariance_kopsavilkums.csv"); fwrite(rbindlist(VI, fill = TRUE), "output/pisa2025/invariance_vienumi.csv")
cat("\nPabeigts:", format(Sys.time()), "\n"); while (sink.number() > 0) sink(); close(log_con)
