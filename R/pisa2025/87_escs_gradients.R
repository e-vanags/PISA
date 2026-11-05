# ============================================================================
# 87. Sociāli ekonomiskais fons (ESCS) kā pētāmais jautājums: gradients,
#     ceturtdaļas, starpskolu segregācija, salīdzināmība, agrīnais signāls
#     (Slānis 18, docs/46; hipotēze H27)
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMI
#   1. Vai lasīšanas rezultāta saistība ar ESCS (gradients) latviešu valodas
#      plūsmā laikā mainās, un kā tā izskatās pret 7 salīdzinājuma valstīm.
#   2. Vai 2018 līdz 2025 kritums ir koncentrēts zemākajās ESCS ceturtdaļās.
#   3. Vai starpskolu sociālā segregācija (ESCS dispersijas daļa starp skolām)
#      aug, un cik liels ir skolas vidējā ESCS konteksta efekts.
#   4. Cik salīdzināms ir ESCS 2025. gadā: trūkstošie, komponentes, un vai
#      ceturtdaļu aina saglabājas, dalot pēc HISEI vai HOMEPOS atsevišķi.
#   5. Vai kaut kas no tā bija redzams 2015. vai 2018. gadā (agrīnais signāls).
#
# DATI: data/pisa2025/processed/simptomi_apvienots.rds (2000 līdz 2025, 8 valstis;
#   2000. gadā Latvijai ESCS nav, tāpēc no 2003). ESCS katrā ciklā ir OECD
#   skalā ar vidējo 0 un SD 1 tam ciklam, tāpēc starp valstīm vienā ciklā tas
#   ir salīdzināms, bet līmenis starp cikliem ne; slīpums (punkti uz vienu
#   OECD SD) un ceturtdaļas (ranga mēri grupas iekšienē) ir salīdzināmi laikā
#   ar atrunu par 2025. gada pārstandartizāciju (00_ab_dati.R, docs/71).
# GRUPAS: 8 valstis (visi skolēni), Latvijas latviešu (A) un krievu (B)
#   valodas plūsma.
# KĻŪDAS: skolu klasteru sāknēšana (B replikāti, PV1) plus imputācijas
#   dispersija starp PV (Rubin). Replikātsvaru apvienotajā failā nav.
#
# IZVADE: output/pisa2025/escs_gradienti.csv (visi rādītāji ar SE),
#   escs_komponentes.csv, escs_trukstosie.csv, escs_ceturtdalas_robustums.csv,
#   plots/87_*.png, logs/87_escs_gradients_output.txt
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
if (!file.exists("R/pisa2025") && file.exists("../../R/pisa2025")) setwd("../..")
output_file <- "output/pisa2025/logs/87_escs_gradients_output.txt"
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākts:", format(Sys.time()), "\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
set.seed(87)
B_REP <- 60          # sāknēšanas replikāti (skolas ar atlikšanu)
L2 <- 407.47         # 2. kompetences līmeņa apakšējā robeža lasīšanā
SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
NOS <- c(LVA = "Latvija, visi", `LVA A` = "Latvija, latviešu valodas plūsma", `LVA B` = "Latvija, krievu valodas plūsma",
         EST = "Igaunija", LTU = "Lietuva", POL = "Polija", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", DEU = "Vācija",
         `7 valstis` = "7 valstu vidējais")

# === 1. DATI =================================================================
cat(paste(rep("=", 70), collapse = ""), "\n1. Dati\n", paste(rep("=", 70), collapse = ""), "\n\n")
X <- as.data.table(readRDS("data/pisa2025/processed/simptomi_apvienots.rds"))
PV <- grep("^PV[0-9]+READ$", names(X), value = TRUE)
keep <- c("gads", "cnt", "skola", "w", "plusma", PV, "AUTO_ESCS", "AUTO_HISEI", "AUTO_PAREDINT", "AUTO_HOMEPOS")
X <- X[, intersect(keep, names(X)), with = FALSE]
for (v in names(X)) if (inherits(X[[v]], "haven_labelled")) X[[v]] <- unclass(X[[v]])
X <- X[gads != "2000" & !is.na(w) & w > 0]
X[, gads := as.integer(gads)]
setnames(X, c("AUTO_ESCS", "AUTO_HISEI", "AUTO_PAREDINT", "AUTO_HOMEPOS"), c("escs", "hisei", "pared", "homepos"))
# vecajos ciklos trūkstošo kodi 997 līdz 999
for (v in c("escs", "hisei", "pared", "homepos")) X[!is.na(get(v)) & get(v) >= 900, (v) := NA_real_]
X[, grupa := cnt]
LA <- X[cnt == "LVA" & plusma == "A"][, grupa := "LVA A"]
LB <- X[cnt == "LVA" & plusma == "B"][, grupa := "LVA B"]
X <- rbind(X, LA, LB)
X[, skola := paste(cnt, gads, skola)]
cat("Rindas pa gadiem un grupām (visi skolēni):\n")
print(dcast(X[, .N, by = .(grupa, gads)], grupa ~ gads, value.var = "N"))
npv_gads <- X[, .(npv = sum(sapply(.SD, function(p) any(!is.na(p))))), by = gads, .SDcols = PV][order(gads)]
cat("\nPV skaits pa gadiem:\n"); print(npv_gads)

# --- palīgfunkcijas ----------------------------------------------------------
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; cw <- cumsum(w) / sum(w); x[which(cw >= p)[1]] }
wmean <- function(x, w) sum(x * w) / sum(w)
wvar <- function(x, w) { m <- wmean(x, w); sum(w * (x - m)^2) / sum(w) }
wcov <- function(x, y, w) { sum(w * (x - wmean(x, w)) * (y - wmean(y, w))) / sum(w) }

# Visi rādītāji vienai grupai un ciklam, vienam PV; d satur y (PV), escs, w, skola
stat_fun <- function(d) {
  y <- d$y; e <- d$escs; w <- d$w
  out <- c()
  # gradients
  ve <- wvar(e, w); vy <- wvar(y, w); cv <- wcov(e, y, w)
  out["slipums"] <- cv / ve
  out["r2"] <- cv^2 / (ve * vy)
  out["escs_vid"] <- wmean(e, w); out["escs_sd"] <- sqrt(ve)
  # ceturtdaļas grupas iekšienē
  q <- c(wq(e, w, .25), wq(e, w, .50), wq(e, w, .75))
  cet <- findInterval(e, q) + 1L
  top <- wq(y, w, .75)
  for (k in 1:4) {
    s <- cet == k
    out[paste0("Q", k, "_vid")] <- wmean(y[s], w[s])
    out[paste0("Q", k, "_p10")] <- wq(y[s], w[s], .10)
    out[paste0("Q", k, "_zem2")] <- 100 * wmean(y[s] < L2, w[s])
  }
  out["plaisa_Q4_Q1"] <- out["Q4_vid"] - out["Q1_vid"]
  out["noturigie"] <- 100 * wmean(y[cet == 1] >= top, w[cet == 1])
  # starpskolu segregācija: starpskolu dispersijas daļa (svērta)
  sm <- d[, .(me = wmean(escs, w), my = wmean(y, w), sw = sum(w)), by = skola]
  out["icc_escs"] <- wvar(sm$me, sm$sw) / ve
  out["icc_lasisana"] <- wvar(sm$my, sm$sw) / vy
  # konteksta efekts: y ~ escs + skolas vidējais escs (svērtā MKM)
  es <- sm$me[match(d$skola, sm$skola)]
  Xm <- cbind(1, e, es); W <- w
  b <- tryCatch(solve(crossprod(Xm * sqrt(W)), crossprod(Xm * sqrt(W), y * sqrt(W))), error = function(err) matrix(NA_real_, 3, 1))
  out["slipums_individ"] <- b[2, 1]; out["konteksts_skola"] <- b[3, 1]
  out
}

# Novērtējums ar PV un skolu klasteru sāknēšanu
novertet <- function(D) {
  D <- D[!is.na(escs)]
  pvs <- PV[sapply(PV, function(p) any(!is.na(D[[p]])))]
  est <- sapply(pvs, function(p) stat_fun(D[, .(y = get(p), escs, w, skola)]))
  punkts <- rowMeans(est); imp <- apply(est, 1, var)
  sk <- unique(D$skola); d1 <- D[, .(y = get(pvs[1]), escs, w, skola)]
  idx <- split(seq_len(nrow(d1)), d1$skola)
  boot <- replicate(B_REP, { s <- sample(sk, length(sk), replace = TRUE); r <- unlist(idx[s], use.names = FALSE)
    dd <- d1[r]; dd[, skola := rep(seq_along(s), lengths(idx[s]))]; stat_fun(dd) })
  samp <- apply(boot, 1, var)
  se <- sqrt(samp + (1 + 1 / length(pvs)) * imp)
  data.table(raditajs = names(punkts), est = punkts, se = se, n = nrow(D), n_skolas = length(sk), npv = length(pvs))
}

# === 2. GRADIENTS, CETURTDAĻAS, SEGREGĀCIJA PA GRUPĀM UN CIKLIEM ===============
cat("\n", paste(rep("=", 70), collapse = ""), "\n2. Gradients, ceturtdaļas, segregācija: visas grupas un cikli\n", paste(rep("=", 70), collapse = ""), "\n\n")
cat("UZMANĪBU: sāknēšana", B_REP, "replikāti x", length(unique(X$grupa)) * length(unique(X$gads)), "šūnas; var aizņemt dažas minūtes.\n")
cells <- unique(X[, .(grupa, gads)])[order(grupa, gads)]
RES <- rbindlist(lapply(seq_len(nrow(cells)), function(i) {
  g <- cells$grupa[i]; y <- cells$gads[i]
  r <- novertet(X[grupa == g & gads == y]); r[, `:=`(grupa = g, gads = y)]; r
}))
# 7 valstu vidējais (vienkāršais; SE no valstu SE)
V7 <- RES[grupa %in% SAL, .(est = mean(est), se = sqrt(sum(se^2)) / .N, n = sum(n), n_skolas = sum(n_skolas), npv = npv[1], grupa = "7 valstis"), by = .(raditajs, gads)]
RES <- rbind(RES, V7[, names(RES), with = FALSE])
RES[, nosaukums := NOS[grupa]]
setcolorder(RES, c("grupa", "nosaukums", "gads", "raditajs", "est", "se", "n", "n_skolas", "npv"))
fwrite(RES[, .(grupa, nosaukums, gads, raditajs, est = round(est, 3), se = round(se, 3), n, n_skolas, npv)], "output/pisa2025/escs_gradienti.csv")
cat("Saglabāts: output/pisa2025/escs_gradienti.csv\n\n")

tab <- function(rad, grupas = c("LVA A", "LVA", "LVA B", "7 valstis", SAL), dig = 1) {
  d <- dcast(RES[raditajs == rad & grupa %in% grupas], grupa ~ gads, value.var = "est")
  d[, grupa := factor(grupa, levels = grupas)]; d <- d[order(grupa)]
  for (v in setdiff(names(d), "grupa")) d[[v]] <- round(d[[v]], dig); d
}
cat("2.1. ESCS gradienta slīpums, punkti uz vienu OECD SD (ESCS tā cikla skalā):\n"); print(tab("slipums"), row.names = FALSE)
cat("\n2.2. Gradienta stiprums R2:\n"); print(tab("r2", dig = 3), row.names = FALSE)
cat("\n2.3. Plaisa starp augstāko un zemāko ESCS ceturtdaļu (grupas iekšienē), punkti:\n"); print(tab("plaisa_Q4_Q1"), row.names = FALSE)
cat("\n2.4. Zemākās ESCS ceturtdaļas vidējais:\n"); print(tab("Q1_vid"), row.names = FALSE)
cat("\n2.5. Augstākās ESCS ceturtdaļas vidējais:\n"); print(tab("Q4_vid"), row.names = FALSE)
cat("\n2.6. Zem 2. līmeņa daļa zemākajā ESCS ceturtdaļā, %:\n"); print(tab("Q1_zem2"), row.names = FALSE)
cat("\n2.7. Zem 2. līmeņa daļa augstākajā ESCS ceturtdaļā, %:\n"); print(tab("Q4_zem2"), row.names = FALSE)
cat("\n2.8. Noturīgie skolēni: zemākās ESCS ceturtdaļas skolēnu daļa valsts augšējā rezultātu ceturtdaļā, %:\n"); print(tab("noturigie"), row.names = FALSE)
cat("\n2.9. ESCS dispersijas daļa starp skolām (segregācija):\n"); print(tab("icc_escs", dig = 3), row.names = FALSE)
cat("\n2.10. Lasīšanas dispersijas daļa starp skolām:\n"); print(tab("icc_lasisana", dig = 3), row.names = FALSE)
cat("\n2.11. Skolas vidējā ESCS konteksta efekts (punkti uz vienu SD skolas vidējā, pie vienāda skolēna ESCS):\n"); print(tab("konteksts_skola"), row.names = FALSE)

# === 3. KRITUMS PA CETURTDAĻĀM 2018 -> 2022 -> 2025 ==========================
cat("\n", paste(rep("=", 70), collapse = ""), "\n3. Pārmaiņa pa ESCS ceturtdaļām\n", paste(rep("=", 70), collapse = ""), "\n\n")
parm <- function(g, no, lidz, rad) {
  a <- RES[grupa == g & gads == no & raditajs == rad]; b <- RES[grupa == g & gads == lidz & raditajs == rad]
  data.table(grupa = g, periods = paste(no, "->", lidz), raditajs = rad, parmaina = b$est - a$est, se = sqrt(a$se^2 + b$se^2))
}
PAR <- rbindlist(lapply(c("LVA A", "LVA", "LVA B", "7 valstis", SAL), function(g) rbindlist(lapply(list(c(2018, 2022), c(2022, 2025), c(2018, 2025), c(2012, 2018)), function(p)
  rbindlist(lapply(c(paste0("Q", 1:4, "_vid"), paste0("Q", 1:4, "_zem2"), "plaisa_Q4_Q1", "slipums"), function(r) parm(g, p[1], p[2], r)))))))
PAR[, z := parmaina / se]
fwrite(PAR[, .(grupa, periods, raditajs, parmaina = round(parmaina, 2), se = round(se, 2), z = round(z, 2))], "output/pisa2025/escs_parmainas.csv")
for (p in c("2018 -> 2025", "2018 -> 2022", "2022 -> 2025", "2012 -> 2018")) {
  cat("\nVidējā pārmaiņa pa ceturtdaļām,", p, "(punkti; iekavās SE):\n")
  d <- PAR[periods == p & raditajs %in% paste0("Q", 1:4, "_vid")]
  d[, txt := sprintf("%.1f (%.1f)", parmaina, se)]
  print(dcast(d, grupa ~ raditajs, value.var = "txt"), row.names = FALSE)
}
cat("\nH27 tests, latviešu valodas plūsma 2018 -> 2025: Q1 pārmaiņa mīnus Q4 pārmaiņa:\n")
a <- PAR[grupa == "LVA A" & periods == "2018 -> 2025"]
dq <- a[raditajs == "Q1_vid", parmaina] - a[raditajs == "Q4_vid", parmaina]
dse <- sqrt(a[raditajs == "Q1_vid", se]^2 + a[raditajs == "Q4_vid", se]^2)
cat(sprintf("  Q1 - Q4 pārmaiņu starpība: %.1f punkti (SE %.1f, z = %.2f); H27 prognoze: zem -10\n", dq, dse, dq / dse))
cat(sprintf("  Slīpuma pārmaiņa 2018 -> 2025: %.1f (SE %.1f); plaisas Q4-Q1 pārmaiņa: %.1f (SE %.1f)\n",
            a[raditajs == "slipums", parmaina], a[raditajs == "slipums", se], a[raditajs == "plaisa_Q4_Q1", parmaina], a[raditajs == "plaisa_Q4_Q1", se]))

# === 4. SALĪDZINĀMĪBA: TRŪKSTOŠIE, KOMPONENTES, ROBUSTUMS ===================
cat("\n", paste(rep("=", 70), collapse = ""), "\n4. ESCS salīdzināmība\n", paste(rep("=", 70), collapse = ""), "\n\n")
TR <- X[, .(n = .N, trukst_escs = 100 * wmean(is.na(escs), w), trukst_hisei = 100 * wmean(is.na(hisei), w), trukst_pared = 100 * wmean(is.na(pared), w),
            trukst_homepos = 100 * wmean(is.na(homepos), w),
            lasisana_ar_escs = wmean(rowMeans(.SD[, PV[1:5], with = FALSE])[!is.na(escs)], w[!is.na(escs)]),
            lasisana_bez_escs = if (any(is.na(escs))) wmean(rowMeans(.SD[, PV[1:5], with = FALSE])[is.na(escs)], w[is.na(escs)]) else NA_real_), by = .(grupa, gads)]
TR[, starpiba := lasisana_bez_escs - lasisana_ar_escs]
for (v in c("trukst_escs", "trukst_hisei", "trukst_pared", "trukst_homepos", "lasisana_ar_escs", "lasisana_bez_escs", "starpiba")) TR[[v]] <- round(TR[[v]], 1)
fwrite(TR[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)], "output/pisa2025/escs_trukstosie.csv")
cat("4.1. Trūkstošā ESCS daļa, % (svērta):\n"); print(dcast(TR[grupa %in% c("LVA A", "LVA", "LVA B", SAL)], grupa ~ gads, value.var = "trukst_escs"), row.names = FALSE)
cat("\n4.2. Lasīšana skolēniem bez ESCS mīnus ar ESCS, punkti (profils; negatīvs = trūkstošie vājāki):\n"); print(dcast(TR[grupa %in% c("LVA A", "LVA", "LVA B", SAL)], grupa ~ gads, value.var = "starpiba"), row.names = FALSE)
KO <- X[, .(escs = wmean(escs[!is.na(escs)], w[!is.na(escs)]), hisei = wmean(hisei[!is.na(hisei)], w[!is.na(hisei)]), pared = wmean(pared[!is.na(pared)], w[!is.na(pared)]),
            homepos = wmean(homepos[!is.na(homepos)], w[!is.na(homepos)])), by = .(grupa, gads)]
KO[, `:=`(escs = round(escs, 3), hisei = round(hisei, 1), pared = round(pared, 1), homepos = round(homepos, 2))]
fwrite(KO, "output/pisa2025/escs_komponentes.csv")
cat("\n4.3. HISEI vidējais pa cikliem:\n"); print(dcast(KO[grupa %in% c("LVA A", "LVA", SAL)], grupa ~ gads, value.var = "hisei"), row.names = FALSE)
cat("\n4.4. HOMEPOS vidējais pa cikliem:\n"); print(dcast(KO[grupa %in% c("LVA A", "LVA", SAL)], grupa ~ gads, value.var = "homepos"), row.names = FALSE)
cat("\n4.5. Vecāku izglītība (gadi, PAREDINT) vidējais pa cikliem:\n"); print(dcast(KO[grupa %in% c("LVA A", "LVA", SAL)], grupa ~ gads, value.var = "pared"), row.names = FALSE)

# robustums: ceturtdaļas pēc HISEI un HOMEPOS atsevišķi (PV vidējais, bez SE)
cet_stat <- function(d, var) {
  d <- d[!is.na(get(var))]; e <- d[[var]]; w <- d$w
  q <- c(wq(e, w, .25), wq(e, w, .50), wq(e, w, .75)); cet <- findInterval(e, q) + 1L
  pvs <- PV[sapply(PV, function(p) any(!is.na(d[[p]])))]
  rbindlist(lapply(1:4, function(k) { s <- cet == k
    data.table(cet = k, vid = mean(sapply(pvs, function(p) wmean(d[[p]][s], w[s]))), zem2 = mean(sapply(pvs, function(p) 100 * wmean(d[[p]][s] < L2, w[s])))) }))
}
ROB <- rbindlist(lapply(c("LVA A", "LVA", "EST", "LTU"), function(g) rbindlist(lapply(c(2018, 2022, 2025), function(y) rbindlist(lapply(c("escs", "hisei", "homepos", "pared"), function(v) {
  r <- cet_stat(X[grupa == g & gads == y], v); r[, `:=`(grupa = g, gads = y, dalijums = v)]; r }))))))
fwrite(ROB[, .(grupa, gads, dalijums, cet, vid = round(vid, 1), zem2 = round(zem2, 1))], "output/pisa2025/escs_ceturtdalas_robustums.csv")
cat("\n4.6. Robustums: Q1 un Q4 vidējā pārmaiņa 2018 -> 2025 pēc dažādiem dalījumiem:\n")
rb <- dcast(ROB[cet %in% c(1, 4)], grupa + dalijums + cet ~ gads, value.var = "vid")
rb[, parmaina_2018_2025 := round(`2025` - `2018`, 1)]
print(dcast(rb, grupa + dalijums ~ cet, value.var = "parmaina_2018_2025"), row.names = FALSE)

# === 5. AGRĪNAIS SIGNĀLS: LATVIJA PRET 7 VALSTĪM PA CIKLIEM ==================
cat("\n", paste(rep("=", 70), collapse = ""), "\n5. Agrīnais signāls: latviešu valodas plūsma mīnus 7 valstu vidējais\n", paste(rep("=", 70), collapse = ""), "\n\n")
AS <- merge(RES[grupa == "LVA A", .(gads, raditajs, lva = est, lva_se = se)], RES[grupa == "7 valstis", .(gads, raditajs, v7 = est, v7_se = se)], by = c("gads", "raditajs"))
AS[, `:=`(starpiba = lva - v7, se = sqrt(lva_se^2 + v7_se^2))]
fwrite(AS[, .(gads, raditajs, lva = round(lva, 2), v7 = round(v7, 2), starpiba = round(starpiba, 2), se = round(se, 2))], "output/pisa2025/escs_agrinais_signals.csv")
for (r in c("slipums", "plaisa_Q4_Q1", "Q1_vid", "Q4_vid", "Q1_zem2", "icc_escs", "noturigie")) {
  cat("\n", r, ": LVA A, 7 valstis, starpība (SE) pa cikliem\n")
  d <- AS[raditajs == r][order(gads)]
  print(d[, .(gads, lva = round(lva, 2), v7 = round(v7, 2), starpiba = round(starpiba, 2), se = round(se, 2))], row.names = FALSE)
}

# === 6. GRAFIKI ==============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n6. Grafiki\n", paste(rep("=", 70), collapse = ""), "\n\n")
th <- theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title.position = "plot")
G1 <- RES[raditajs %in% c("slipums", "plaisa_Q4_Q1", "icc_escs", "noturigie") & grupa %in% c("LVA A", "7 valstis", SAL)]
G1[, kr := fifelse(grupa == "LVA A", "Latvija, latviešu valodas plūsma", fifelse(grupa == "7 valstis", "7 valstu vidējais", "salīdzinājuma valsts"))]
G1[, raditajs := factor(raditajs, levels = c("slipums", "plaisa_Q4_Q1", "icc_escs", "noturigie"),
                        labels = c("Gradienta slīpums, punkti uz 1 SD", "Plaisa Q4 mīnus Q1, punkti", "ESCS dispersijas daļa starp skolām", "Noturīgie skolēni, %"))]
p1 <- ggplot(G1, aes(gads, est, group = grupa, colour = kr, linewidth = kr, alpha = kr)) + geom_line() + geom_point(size = 1.6) +
  facet_wrap(~raditajs, scales = "free_y") +
  scale_colour_manual(values = c(`Latvija, latviešu valodas plūsma` = "#b3261e", `7 valstu vidējais` = "#0b0b0b", `salīdzinājuma valsts` = "#9aa0a6"), name = NULL) +
  scale_linewidth_manual(values = c(1.4, 1.1, 0.6), guide = "none") + scale_alpha_manual(values = c(1, 1, 0.7), guide = "none") +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) +
  labs(title = "Sociālā fona gradients un segregācija lasīšanā, PISA 2003 līdz 2025", x = NULL, y = NULL,
       caption = "ESCS katrā ciklā OECD skalā; ceturtdaļas grupas iekšienē. Avots: OECD PISA mikrodati; R/pisa2025/87") + th
ggsave("output/pisa2025/plots/87_escs_gradients.png", p1, width = 11, height = 7, dpi = 200, bg = "white")
G2 <- RES[raditajs %in% paste0("Q", 1:4, "_vid") & grupa %in% c("LVA A", "7 valstis")]
G2[, cet := factor(sub("_vid", "", raditajs), labels = c("zemākā ceturtdaļa", "2. ceturtdaļa", "3. ceturtdaļa", "augstākā ceturtdaļa"))]
p2 <- ggplot(G2, aes(gads, est, colour = cet)) + geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, fill = cet), alpha = .12, colour = NA) +
  geom_line(linewidth = 1) + geom_point() + facet_wrap(~NOS[grupa]) + scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) +
  scale_colour_brewer(palette = "RdYlBu", direction = 1, name = "ESCS ceturtdaļa") + scale_fill_brewer(palette = "RdYlBu", direction = 1, guide = "none") +
  labs(title = "Lasīšanas vidējais pa ESCS ceturtdaļām", subtitle = "95 % intervāls: skolu klasteru sāknēšana un PV imputācijas dispersija", x = NULL, y = "PISA punkti",
       caption = "Avots: OECD PISA mikrodati; R/pisa2025/87") + th
ggsave("output/pisa2025/plots/87_escs_ceturtdalas.png", p2, width = 11, height = 6, dpi = 200, bg = "white")
G3 <- PAR[periods == "2018 -> 2025" & raditajs %in% paste0("Q", 1:4, "_vid") & grupa %in% c("LVA A", "LVA", "7 valstis", SAL)]
G3[, cet := factor(sub("_vid", "", raditajs), labels = c("Q1 zemākā", "Q2", "Q3", "Q4 augstākā"))]
G3[, grupa := factor(grupa, levels = c("LVA A", "LVA", "7 valstis", SAL), labels = NOS[c("LVA A", "LVA", "7 valstis", SAL)])]
p3 <- ggplot(G3, aes(cet, parmaina, fill = cet)) + geom_col() + geom_errorbar(aes(ymin = parmaina - 1.96 * se, ymax = parmaina + 1.96 * se), width = .25) +
  geom_hline(yintercept = 0) + facet_wrap(~grupa, ncol = 5) + scale_fill_brewer(palette = "RdYlBu", guide = "none") +
  labs(title = "Lasīšanas pārmaiņa 2018 līdz 2025 pa ESCS ceturtdaļām", x = NULL, y = "punkti", caption = "Avots: OECD PISA mikrodati; R/pisa2025/87") + th +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("output/pisa2025/plots/87_escs_parmaina_ceturtdalas.png", p3, width = 12, height = 6, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/87_escs_gradients.png, 87_escs_ceturtdalas.png, 87_escs_parmaina_ceturtdalas.png\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili: escs_gradienti.csv, escs_parmainas.csv, escs_trukstosie.csv, escs_komponentes.csv, escs_ceturtdalas_robustums.csv, escs_agrinais_signals.csv, plots/87_*.png\n")
cat("Beigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
