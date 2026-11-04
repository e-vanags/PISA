# ============================================================================
# 69c. Slānis 10: simptomu tīkla modelis (EBICglasso) un tīklu salīdzināšana
# ============================================================================
#
# Tīkls: daļējo korelāciju tīkls (Gaussian graphical model, EBICglasso, γ = 0,5) starp
# saskaņotajiem konstruktiem un lasīšanas rezultātu (PVREAD). Mezgli: manuālie konstrukti
# un daži galvenie indeksi ar aizpildītību ≥ 70 % katrā ciklā un valstī 2015–2025, bez
# nominālajiem. Korelācija: Spīrmena, pa pāriem pilnās vērtības; n = mazākais pāra n.
# Grupas: LVA A un 7 salīdzinājuma valstis kopā, cikli 2015, 2018, 2022, 2025.
# Rādītāji: stipruma un gaidāmās ietekmes centralitāte; PVREAD tiešie kaimiņi; tilti starp
#   jomām (daļa no mezgla stipruma, kas iet uz citām jomām).
# Salīdzināšana (NCT analogs, permutācijas, 200 reizes): (a) LVA A pret 7 valstīm tajā pašā
#   ciklā; (b) LVA A 2022 pret 2025. Statistikas: globālais stiprums, lielākā malas
#   starpība, PVREAD kaimiņu stipruma starpība. Permutē grupu etiķetes skolēniem.
#
# Izvade: output/pisa2025/tikls_malas.csv, tikls_centralitate.csv, tikls_salidzinajums.csv,
#         output/pisa2025/plots/69c_tikls_<grupa>_<gads>.png
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(qgraph) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/69c_tikla_modelis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n"); set.seed(20260913)
dir.create("output/pisa2025/plots", showWarnings = FALSE)

A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A <- A[!is.na(PVREAD) & !is.na(w) & w > 0 & gads %in% c("2015", "2018", "2022", "2025")]
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
A <- A[grupa %in% c("LVA A", SALID)][, kopa := ifelse(grupa == "LVA A", "LVA A", "7 valstis")]
KART <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, .(domens = domens[1], apraksts = apraksts[1]), by = konstrukts]
META <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD", "Q_TRUKST", "grupa", "kopa")
NOMIN <- c("AUTO_PROGN", "AUTO_COBN_F", "AUTO_COBN_M", "AUTO_COBN_S", "AUTO_OCOD1", "AUTO_OCOD2", "AUTO_OCOD3", "AUTO_BSMJ", "AUTO_RANDUNIT", "AUTO_TESTLANG",
           "AUTO_ST022Q01TA", "AUTO_ST011D17TA", "AUTO_ST011D18TA", "AUTO_ST011D19TA", "AUTO_ST251D08JA", "AUTO_ST251D09JA", "AUTO_AGE", "AUTO_GRADE", "G_DZIMUMS")
kand <- setdiff(names(A), c(META, NOMIN)); kand <- kand[!grepl("^SK_|^AUTO_SC|^AUTO_PRO|^AUTO_ST[0-9]|^AUTO_IC[0-9]|__B$", kand)]   # tikai skolēnu konstrukti un indeksi
aizp <- A[, lapply(.SD, function(x) mean(!is.na(x))), by = .(gads, kopa), .SDcols = kand]
ok <- kand[sapply(kand, function(k) all(aizp[[k]] >= 0.7))]
ok <- ok[sapply(ok, function(k) A[, uniqueN(na.omit(get(k)))] >= 3)]
MEZGLI <- c("PVREAD", ok)
cat("Mezgli:", length(MEZGLI), "\n"); print(data.table(mezgls = ok, domens = KART$domens[match(ok, KART$konstrukts)], apraksts = substr(KART$apraksts[match(ok, KART$konstrukts)], 1, 50)), nrows = 100)
dom_of <- function(p) { d <- KART$domens[match(p, KART$konstrukts)]; d[p == "PVREAD"] <- "iznakums"; d[is.na(d)] <- "cits"; d }

tikls <- function(d) {
  M <- as.matrix(d[, ..MEZGLI]); C <- suppressWarnings(cor(M, method = "spearman", use = "pairwise.complete.obs"))
  C[is.na(C)] <- 0; n <- min(crossprod(!is.na(M))[upper.tri(C)])
  W <- tryCatch(EBICglasso(C, n = n, gamma = 0.5, threshold = TRUE), error = function(e) { C2 <- as.matrix(Matrix::nearPD(C, corr = TRUE)$mat); EBICglasso(C2, n = n, gamma = 0.5, threshold = TRUE) })
  dimnames(W) <- list(MEZGLI, MEZGLI); W
}
stat <- function(W) { s <- rowSums(abs(W)); ei <- rowSums(W); list(globals = sum(abs(W[upper.tri(W)])), strength = s, ei = ei, pv = W["PVREAD", ]) }

MAL <- list(); CEN <- list(); TIKLI <- list()
for (g in c("2015", "2018", "2022", "2025")) for (k in c("LVA A", "7 valstis")) {
  d <- A[gads == g & kopa == k]; W <- tikls(d); TIKLI[[paste(g, k)]] <- W; s <- stat(W)
  e <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
  MAL[[paste(g, k)]] <- data.table(gads = g, kopa = k, no = MEZGLI[e[, 1]], uz = MEZGLI[e[, 2]], svars = W[e])
  dom <- dom_of(MEZGLI); tilts <- sapply(seq_along(MEZGLI), function(i) { w <- abs(W[i, ]); if (sum(w) == 0) 0 else sum(w[dom != dom[i]]) / sum(w) })
  CEN[[paste(g, k)]] <- data.table(gads = g, kopa = k, mezgls = MEZGLI, domens = dom, stiprums = s$strength, gaid_ietekme = s$ei, mala_uz_PVREAD = s$pv, tilts = tilts, n = nrow(d))
  cat(sprintf("%s %-9s n = %5d, malas %3d, globālais stiprums %.2f, PVREAD kaimiņi: %s\n", g, k, nrow(d), nrow(e), s$globals,
              paste(sprintf("%s %+.2f", names(sort(s$pv[s$pv != 0 & names(s$pv) != "PVREAD"], decreasing = TRUE))[1:min(6, sum(s$pv != 0) - 1)], sort(s$pv[s$pv != 0 & names(s$pv) != "PVREAD"], decreasing = TRUE)[1:min(6, sum(s$pv != 0) - 1)]), collapse = ", ")))
  png(sprintf("output/pisa2025/plots/69c_tikls_%s_%s.png", gsub(" ", "", k), g), 1600, 1600, res = 150)
  grp <- split(seq_along(MEZGLI), dom); qgraph(W, layout = "spring", groups = grp, labels = sub("^(AUTO_|[A-Z]+_)", "", MEZGLI), label.cex = 1.1, vsize = 4.5, legend.cex = 0.5,
                                            title = sprintf("Simptomu tīkls, %s, %s (n = %d)", k, g, nrow(d)), theme = "colorblind"); dev.off()
}
fwrite(rbindlist(MAL), "output/pisa2025/tikls_malas.csv"); CENT <- rbindlist(CEN); fwrite(CENT, "output/pisa2025/tikls_centralitate.csv")

cat("\n", strrep("=", 100), "\nCentralitāte (stiprums) LVA A 2025: 12 centrālākie mezgli; un mala uz PVREAD\n", strrep("=", 100), "\n", sep = "")
print(CENT[gads == "2025" & kopa == "LVA A"][order(-stiprums)][1:12, .(mezgls, domens, stiprums = round(stiprums, 2), gaid_ietekme = round(gaid_ietekme, 2), mala_uz_PVREAD = round(mala_uz_PVREAD, 3), tilts = round(tilts, 2))], row.names = FALSE)
cat("\nPVREAD tiešie kaimiņi pa cikliem (LVA A / 7 valstis), daļējā korelācija:\n")
print(dcast(CENT[mezgls != "PVREAD" & mala_uz_PVREAD != 0], mezgls ~ gads + kopa, value.var = "mala_uz_PVREAD", fun.aggregate = function(x) round(x[1], 3)), row.names = FALSE)

# Permutāciju salīdzinājums
salidz <- function(d1, d2, nos, B = 200) {
  W1 <- tikls(d1); W2 <- tikls(d2); s1 <- stat(W1); s2 <- stat(W2)
  T0 <- c(globals = s2$globals - s1$globals, max_mala = max(abs(W2 - W1)), pv_stiprums = sum(abs(s2$pv)) - sum(abs(s1$pv)))
  pool <- rbind(d1, d2); n1 <- nrow(d1)
  TB <- replicate(B, { i <- sample(nrow(pool)); Wa <- tikls(pool[i[1:n1]]); Wb <- tikls(pool[i[-(1:n1)]]); sa <- stat(Wa); sb <- stat(Wb)
    c(sb$globals - sa$globals, max(abs(Wb - Wa)), sum(abs(sb$pv)) - sum(abs(sa$pv))) })
  p <- c(mean(abs(TB[1, ]) >= abs(T0[1])), mean(TB[2, ] >= T0[2]), mean(abs(TB[3, ]) >= abs(T0[3])))
  em <- which(abs(W2 - W1) == max(abs(W2 - W1)), arr.ind = TRUE)[1, ]
  data.table(salidzinajums = nos, statistika = names(T0), vertiba = round(T0, 3), p_perm = p, lielaka_mala = paste(MEZGLI[em[1]], "–", MEZGLI[em[2]]))
}
cat("\n", strrep("=", 100), "\nTīklu salīdzināšana (permutācijas, 200)\n", strrep("=", 100), "\n", sep = "")
SAL <- list()
for (g in c("2022", "2025")) SAL[[g]] <- salidz(A[gads == g & kopa == "7 valstis"][sample(.N, min(.N, 8000))], A[gads == g & kopa == "LVA A"], paste("LVA A pret 7 valstīm", g))
SAL[["laiks"]] <- salidz(A[gads == "2022" & kopa == "LVA A"], A[gads == "2025" & kopa == "LVA A"], "LVA A 2022 → 2025")
SAL[["laiks7"]] <- salidz(A[gads == "2022" & kopa == "7 valstis"][sample(.N, 6000)], A[gads == "2025" & kopa == "7 valstis"][sample(.N, 6000)], "7 valstis 2022 → 2025")
SALT <- rbindlist(SAL); fwrite(SALT, "output/pisa2025/tikls_salidzinajums.csv"); print(SALT, row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n"); while (sink.number() > 0) sink(); close(log_con)
