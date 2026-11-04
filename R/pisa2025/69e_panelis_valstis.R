# ============================================================================
# 69e. Slānis 11: simptomu un rezultāta pārmaiņu panelis visām PISA valstīm 2015–2025
# ============================================================================
#
# JAUTĀJUMS: 8 valstu simptomu modelim jauda ir maza. Vai visām PISA valstīm (~70) simptomu
# vidējo pārmaiņas paskaidro lasīšanas vidējā pārmaiņas, un vai Latvija (un tās latviešu
# valodas plūsma) ir izņēmums?
#
# DATI: pilnie PISA STU faili 2015, 2018, 2022, 2025 (visas valstis); rādītāji, kas ir visos
#   četros ciklos vai vismaz trijos: ESCS, HOMEPOS, HISEI, BELONG, TEACHSUP, ICTRES, GRADE,
#   IMMIG (1 = vietējais), REPEAT, dzimums, dzīves apmierinātība (ST016), grāmatas > 200
#   (ST013 2015/18: kods ≥ 5; ST255 2022/25: kods ≥ 6), EFFORT1/2 un PAREDINT (2018+),
#   EXPECEDU, SKIPPING, TARDYSD, FEELSAFE, BULLIED, FAMSUP, EXERPRAC (2022+).
#   Valsts × cikls svērtais vidējais (W_FSTUWT), lasīšana = 10 PV vidējais. Latvijai arī
#   latviešu valodas plūsma (LANGN 325) kā atsevišķa vienība "LVA A".
# MODEĻI (pārejas 2015→2018, 2018→2022, 2022→2025; ~70 valstis × 3):
#   (1) Δread = pārejas FE + β·Δsimptomi (tikai rādītāji, kas ir visos ciklos), ar valstu
#       klasteriem; R² un koeficienti; Latvijas un LVA A atlikumi pa pārejām.
#   (2) Ex ante: β no pārejām < T, prognoze pārejai T no faktiskās Δsimptomi (diagnoze) un
#       no iepriekšējās pārejas Δsimptomi (impulss). Salīdzinājums ar naivo un ar pārejas FE.
#   (3) Līmeņu modelis ar valstu un ciklu FE (within).
#
# Izvade: data/pisa2025/processed/panelis_valstis.csv, output/pisa2025/panelis_valstis_modeli.csv,
#         panelis_valstis_atlikumi.csv, logs/69e_panelis_valstis_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(haven) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/69e_panelis_valstis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
S25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav", "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
       "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV", "2025" = file.path(S25, "CY09_MS_STU_PUF.sav"))
RAD <- c("ESCS", "HOMEPOS", "HISEI", "BELONG", "TEACHSUP", "ICTRES", "GRADE", "IMMIG", "REPEAT", "ST004D01T", "ST016Q01NA", "ST013Q01TA", "ST255Q01JA",
         "EFFORT1", "EFFORT2", "PAREDINT", "EXPECEDU", "SKIPPING", "TARDYSD", "FEELSAFE", "BULLIED", "FAMSUP", "EXERPRAC", "LANGN")
PV <- paste0("PV", 1:10, "READ")
num <- function(x) suppressWarnings(as.numeric(zap_labels(x)))
wm <- function(x, w) { i <- !is.na(x) & !is.na(w); if (!any(i)) NA_real_ else sum(w[i] * x[i]) / sum(w[i]) }

FP <- "data/pisa2025/processed/panelis_valstis.csv"
if (file.exists(FP)) PAN <- fread(FP, colClasses = c(gads = "character")) else {
  PAN <- list()
  for (g in names(F)) {
    t0 <- Sys.time(); nm <- names(read_sav(F[[g]], n_max = 0)); up <- toupper(nm)
    cols <- nm[up %in% c("CNT", "OECD", "W_FSTUWT", PV, RAD)]
    X <- as.data.table(read_sav(F[[g]], col_select = all_of(cols))); names(X) <- toupper(names(X))
    for (k in setdiff(names(X), c("CNT", "OECD"))) set(X, j = k, value = num(X[[k]]))
    X[, READ := rowMeans(.SD, na.rm = TRUE), .SDcols = intersect(PV, names(X))]
    X[, gram200 := if (g %in% c("2015", "2018") && "ST013Q01TA" %in% names(X)) as.numeric(ST013Q01TA >= 5) else if ("ST255Q01JA" %in% names(X)) as.numeric(ST255Q01JA >= 6) else NA_real_]
    X[, meitene := if ("ST004D01T" %in% names(X)) as.numeric(ST004D01T == 1) else NA_real_]
    X[, vietejais := if ("IMMIG" %in% names(X)) as.numeric(IMMIG == 1) else NA_real_]
    X[, atkartoja := if ("REPEAT" %in% names(X)) as.numeric(REPEAT == 1) else NA_real_]
    X[, vieniba := as.character(CNT)]
    if ("LANGN" %in% names(X)) X <- rbind(X, X[CNT == "LVA" & LANGN == 325][, vieniba := "LVA A"])
    vars <- intersect(c("READ", "ESCS", "HOMEPOS", "HISEI", "BELONG", "TEACHSUP", "ICTRES", "GRADE", "ST016Q01NA", "EFFORT1", "EFFORT2", "PAREDINT", "EXPECEDU", "SKIPPING", "TARDYSD", "FEELSAFE", "BULLIED", "FAMSUP", "EXERPRAC"), names(X))
    a <- X[, c(lapply(.SD, function(v) wm(v, W_FSTUWT)), .(gram200 = wm(gram200, W_FSTUWT), meitene = wm(meitene, W_FSTUWT), vietejais = wm(vietejais, W_FSTUWT), atkartoja = wm(atkartoja, W_FSTUWT), n = .N)),
           by = vieniba, .SDcols = vars][, gads := g]
    PAN[[g]] <- a; rm(X); gc(verbose = FALSE)
    cat(sprintf("  %s: %d vienības, %d rādītāji, %.1f min\n", g, nrow(a), ncol(a) - 3, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  PAN <- rbindlist(PAN, fill = TRUE); fwrite(PAN, FP)
}
cat("Panelis:", nrow(PAN), "vienība × cikls;", uniqueN(PAN$vieniba), "vienības\n")

# ---------------------------------------------------------------------------
# Pārejas
# ---------------------------------------------------------------------------
G <- c("2015", "2018", "2022", "2025")
# Dzīves apmierinātība (nav 2015), ICTRES (trūkst 40 % 2022/25) un dzimums (15 % 2025) izslēgti, lai paliktu ~70 valstis
SIMPT_VISI <- c("ESCS", "HOMEPOS", "HISEI", "BELONG", "TEACHSUP", "GRADE", "gram200", "vietejais", "atkartoja")
SIMPT_18 <- c(SIMPT_VISI, "EFFORT1", "EFFORT2", "PAREDINT")
D <- list()
for (i in 1:3) { a <- PAN[gads == G[i]]; b <- PAN[gads == G[i + 1]]; m <- merge(a, b, by = "vieniba", suffixes = c("_1", "_2"))
  m <- m[n_1 >= 1000 & n_2 >= 1000]
  d <- m[, .(vieniba, pareja = paste(G[i], G[i + 1], sep = "→"), dREAD = READ_2 - READ_1, READ_1)]
  for (k in c(SIMPT_18)) if (paste0(k, "_1") %in% names(m)) d[[paste0("d_", k)]] <- m[[paste0(k, "_2")]] - m[[paste0(k, "_1")]]
  D[[i]] <- d }
D <- rbindlist(D, fill = TRUE)
cat("Pārejas ar pilniem datiem (bez LVA A):", D[vieniba != "LVA A" & complete.cases(D[, paste0("d_", SIMPT_VISI), with = FALSE]), .N], "\n")
fwrite(D, "output/pisa2025/panelis_valstis_parejas.csv")

# Standartizē Δ pa pārejām pēc valstu SD (koeficienti salīdzināmi)
V <- D[vieniba != "LVA A" & !is.na(dREAD)]; V <- V[complete.cases(V[, paste0("d_", SIMPT_VISI), with = FALSE])]
Z <- copy(V); for (k in paste0("d_", SIMPT_VISI)) Z[[k]] <- (Z[[k]] - mean(Z[[k]])) / sd(Z[[k]])
f1 <- as.formula(paste("dREAD ~ pareja +", paste(paste0("d_", SIMPT_VISI), collapse = " + ")))
m1 <- lm(f1, data = Z); m0 <- lm(dREAD ~ pareja, data = Z)
cat("\n", strrep("=", 100), "\n(1) Δlasīšana ~ pārejas FE + Δsimptomi (standartizēti), visas valstis\n", strrep("=", 100), "\n", sep = "")
cat(sprintf("  n = %d pārejas, %d valstis; R² tikai FE %.3f, ar simptomiem %.3f (adj. %.3f)\n", nrow(Z), uniqueN(Z$vieniba), summary(m0)$r.squared, summary(m1)$r.squared, summary(m1)$adj.r.squared))
co <- as.data.table(summary(m1)$coefficients, keep.rownames = "termins")[grepl("^d_", termins)][order(-abs(`t value`))]
setnames(co, c("termins", "beta", "se", "t", "p")); print(co[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], row.names = FALSE)
# klasteru (valstu) SE ar sāknēšanu pa valstīm
set.seed(1); vs <- unique(Z$vieniba)
B <- replicate(500, { s <- Z[vieniba %in% sample(vs, replace = TRUE)]; coef(lm(f1, data = s))[paste0("d_", SIMPT_VISI)] })
co2 <- data.table(termins = paste0("d_", SIMPT_VISI), beta = coef(m1)[paste0("d_", SIMPT_VISI)], se_klast = apply(B, 1, sd))[, z := beta / se_klast]
cat("  Ar valstu sāknēšanas SE (500):\n"); print(co2[order(-abs(z))][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], row.names = FALSE)
Z[, atl := residuals(m1)]; res <- Z[vieniba == "LVA", .(pareja, dREAD = round(dREAD, 1), atlikums = round(atl, 1))]
sdres <- sd(Z$atl); cat(sprintf("\n  Atlikumu SD %.1f. Latvija (visa): \n", sdres)); print(res, row.names = FALSE)
# LVA A atlikums: prognoze ar m1 uz LVA A Δ
LA <- D[vieniba == "LVA A"]; for (k in paste0("d_", SIMPT_VISI)) LA[[k]] <- (LA[[k]] - mean(V[[k]])) / sd(V[[k]])
LA[, pred := predict(m1, newdata = LA)][, atl := dREAD - pred]
cat("  Latviešu valodas plūsma (prognoze ar visu valstu modeli):\n"); print(LA[, .(pareja, dREAD = round(dREAD, 1), pred = round(pred, 1), atlikums = round(atl, 1))], row.names = FALSE)
# rangs
Z[, rangs := frank(atl), by = pareja]; cat("  Latvijas atlikuma rangs (1 = negatīvākais) no", uniqueN(Z$vieniba), ":"); print(Z[vieniba == "LVA", .(pareja, rangs)], row.names = FALSE)

cat("\n", strrep("=", 100), "\n(2) Ex ante: koeficienti no agrākām pārejām; prognoze pārejai T\n", strrep("=", 100), "\n", sep = "")
EX <- list()
for (T in c("2018→2022", "2022→2025")) {
  PREV <- c("2018→2022" = "2015→2018", "2022→2025" = "2018→2022"); AGR <- list("2018→2022" = "2015→2018", "2022→2025" = c("2015→2018", "2018→2022"))
  tr <- Z[pareja %in% AGR[[T]]]; te <- Z[pareja == T]
  mt <- lm(as.formula(paste("dREAD ~", paste(paste0("d_", SIMPT_VISI), collapse = " + "))), data = tr)
  te[, pred_diag := predict(mt, newdata = te) - mean(predict(mt, newdata = te)) + mean(tr$dREAD)]   # kopīgais līmenis = agrāko pāreju vidējais
  # impulss: iepriekšējās pārejas Δsimptomi
  prev <- Z[pareja == PREV[[T]]]
  te <- merge(te, prev[, c("vieniba", paste0("d_", SIMPT_VISI)), with = FALSE], by = "vieniba", suffixes = c("", "_prev"))
  Xp <- te[, paste0("d_", SIMPT_VISI, "_prev"), with = FALSE]; setnames(Xp, paste0("d_", SIMPT_VISI))
  te[, pred_imp := predict(mt, newdata = Xp) - mean(predict(mt, newdata = Xp)) + mean(tr$dREAD)]
  te[, naiv := mean(tr$dREAD)]
  te[, `:=`(rel_f = dREAD - mean(dREAD), rel_d = pred_diag - mean(pred_diag), rel_i = pred_imp - mean(pred_imp))]
  s <- te[, .(pareja = T, valstis = .N, mae_naiv = mean(abs(dREAD - naiv)), mae_diag = mean(abs(dREAD - pred_diag)), mae_imp = mean(abs(dREAD - pred_imp)),
              rel_kor_diag = cor(rel_f, rel_d), rel_kor_imp = cor(rel_f, rel_i), rel_mae_naiv = mean(abs(rel_f)), rel_mae_diag = mean(abs(rel_f - rel_d)), rel_mae_imp = mean(abs(rel_f - rel_i)))]
  print(s[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], row.names = FALSE)
  cat("  Latvija:"); print(te[vieniba == "LVA", .(dREAD = round(dREAD, 1), rel_f = round(rel_f, 1), rel_diag = round(rel_d, 1), rel_imp = round(rel_i, 1))], row.names = FALSE)
  EX[[T]] <- s
}
fwrite(rbind(co2[, .(modelis = "FE + Δsimptomi", termins, beta, se = se_klast)], fill = TRUE), "output/pisa2025/panelis_valstis_modeli.csv")
fwrite(rbindlist(EX), "output/pisa2025/panelis_valstis_ex_ante.csv"); fwrite(Z[, .(vieniba, pareja, dREAD, atl, rangs)], "output/pisa2025/panelis_valstis_atlikumi.csv")

cat("\n", strrep("=", 100), "\n(3) Līmeņu modelis: READ ~ valsts FE + cikla FE + simptomi (within)\n", strrep("=", 100), "\n", sep = "")
L <- PAN[vieniba != "LVA A" & n >= 1000 & complete.cases(PAN[, SIMPT_VISI, with = FALSE])]
L <- L[, if (.N == 4) .SD, by = vieniba]
for (k in SIMPT_VISI) L[[paste0("w_", k)]] <- L[[k]] - ave(L[[k]], L$vieniba)
L[, w_READ := READ - ave(READ, vieniba)]
m3 <- lm(as.formula(paste("w_READ ~ gads +", paste(paste0("w_", SIMPT_VISI), collapse = " + "))), data = L)
cat(sprintf("  %d valstis × 4 cikli; within R² %.3f\n", uniqueN(L$vieniba), summary(m3)$r.squared))
print(as.data.table(summary(m3)$coefficients, keep.rownames = "termins")[grepl("^w_", termins)][, .(termins, beta = round(Estimate, 2), t = round(`t value`, 1))][order(-abs(t))], row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n"); while (sink.number() > 0) sink(); close(log_con)
