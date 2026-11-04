# ============================================================================
# 64f. Simptomu modelis, 3. solis: karte "simptomi → prasme" un pārbaude visos ciklos
# ============================================================================
#
# JAUTĀJUMS. Ja karti no simptomiem uz lasītprasmi iemāca tikai uz agrākajiem cikliem,
# vai tā, zinot cikla T simptomus, pareizi paredz cikla T lasītprasmi katrā valstī un
# latviešu valodas plūsmā? Un kuri simptomi nes paredzēto 2022 → 2025 pārmaiņu?
#
# Tā vēl NAV ex ante prognoze: cikla T simptomi ir mērīti tajā pašā dienā, kad tests
# (diagnoze). Ex ante ir 5. solis (docs/61). Šeit pārbauda, vai simptomi vispār
# izskaidro līmeņus un pārmaiņas, un vai karte ir stabila laikā.
#
# DATI: data/pisa2025/processed/simptomi_apvienots.rds (64e): 9 cikli, LVA + 7 valstis.
# MĒRĶIS: lasīšanas PV vidējais skolēnam (PVREAD).
# PAZĪMES: visi saskaņotie konstrukti + Q_TRUKST (anketas trūkums). Valsts un gads
#   netiek doti (lai modelis skaidro ar simptomiem, ne ar valsts vai laika "etiķeti").
# MODELIS: XGBoost (gradienta boosting; trūkstošās vērtības pēc dizaina apstrādā pats),
#   svari: skolēna svars, normēts, lai katrs valsts × cikls sver vienādi.
# PĀRBAUDE (slīdošs sākumpunkts): mērķa cikliem 2006, 2009, 2012, 2015, 2018, 2022, 2025
#   apmāca uz visiem agrākajiem cikliem un paredz mērķa ciklu. Salīdzina ar naivo
#   (iepriekšējā cikla faktiskais). Novērtē valstīs un latviešu valodas plūsmā.
# SKAIDROJUMS: SHAP ieguldījumi 2025. gada modelim (apmācīts ≤ 2022): latviešu valodas
#   plūsmas vidējā SHAP pārmaiņa 2022 → 2025 pa pazīmēm un jomām.
#
# Izvade: output/pisa2025/simptomi_modelis_parbaude.csv, simptomi_modelis_shap_2025.csv
#         output/pisa2025/logs/64f_simptomu_modelis_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64f_simptomu_modelis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)

A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A <- A[!is.na(PVREAD) & !is.na(w) & w > 0]
META <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD")
PAZ <- setdiff(names(A), META)
A[, wn := w / sum(w) * 1000, by = .(gads, cnt)]
A[, grupa := ifelse(cnt == "LVA" & !is.na(plusma), paste0("LVA ", plusma), cnt)]
cat("Skolēni:", nrow(A), "| pazīmes:", length(PAZ), "\n")

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
MERKI <- c("2006", "2009", "2012", "2015", "2018", "2022", "2025")
PAR <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.5,
            min_child_weight = 20, nthread = max(1, parallel::detectCores() - 2), eval_metric = "rmse")
mat <- function(d) as.matrix(d[, ..PAZ])
wm <- function(x, w) sum(x * w) / sum(w)

apmacit <- function(tr) {
  iv <- sample(nrow(tr), round(0.1 * nrow(tr)))
  dtr <- xgb.DMatrix(mat(tr[-iv]), label = tr$PVREAD[-iv], weight = tr$wn[-iv], missing = NA)
  dva <- xgb.DMatrix(mat(tr[iv]), label = tr$PVREAD[iv], weight = tr$wn[iv], missing = NA)
  m0 <- xgb.train(PAR, dtr, nrounds = 3000, evals = list(val = dva), early_stopping_rounds = 50, verbose = 0)
  nb <- xgb.attr(m0, "best_iteration"); nb <- if (is.null(nb)) 3000 else as.integer(nb) + 1
  xgb.train(PAR, xgb.DMatrix(mat(tr), label = tr$PVREAD, weight = tr$wn, missing = NA), nrounds = nb, verbose = 0)
}

rez <- list(); M25 <- NULL
for (Tg in MERKI) {
  t0 <- Sys.time()
  tr <- A[gads < Tg]; te <- A[gads == Tg]; iepr <- GADI[match(Tg, GADI) - 1]
  m <- apmacit(tr)
  te[, pred := predict(m, xgb.DMatrix(mat(te), missing = NA))]
  pr <- A[gads == iepr][, pred := predict(m, xgb.DMatrix(mat(.SD), missing = NA)), .SDcols = PAZ]
  kop <- merge(te[, .(fakt = wm(PVREAD, wn), pred = wm(pred, wn), n = .N), by = grupa],
               pr[, .(fakt_iepr = wm(PVREAD, wn), pred_iepr = wm(pred, wn)), by = grupa], by = "grupa", all.x = TRUE)
  kop[, `:=`(merkis = Tg, iepr = iepr, kluda_modelis = fakt - pred, kluda_naivais = fakt - fakt_iepr,
             parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr)]
  r2 <- 1 - sum(te$wn * (te$PVREAD - te$pred)^2) / sum(te$wn * (te$PVREAD - wm(te$PVREAD, te$wn))^2)
  kop[, r2_skoleni := r2]
  rez[[Tg]] <- kop
  cat(sprintf("\n%s (apmācīts %s–%s, %d skolēni, %d koki, %.1f min) skolēnu R² = %.3f\n", Tg, GADI[1], iepr, nrow(tr),
              xgb.get.num.boosted.rounds(m), as.numeric(difftime(Sys.time(), t0, units = "mins")), r2))
  print(kop[order(grupa), .(grupa, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                            naivais = round(kluda_naivais, 1), parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))], row.names = FALSE)
  if (Tg == "2025") { M25 <- m; TE25 <- te; PR22 <- pr }
}
R <- rbindlist(rez)
fwrite(R, "output/pisa2025/simptomi_modelis_parbaude.csv")

cat("\n", strrep("=", 100), "\nKopsavilkums: vidējā absolūtā kļūda valstu līmenī (valstis, bez plūsmām)\n", strrep("=", 100), "\n", sep = "")
V <- R[!grepl("^LVA ", grupa)]
print(V[, .(modelis = round(mean(abs(kluda_modelis)), 1), naivais = round(mean(abs(kluda_naivais), na.rm = TRUE), 1),
            parm_korelacija = round(cor(parm_fakt, parm_pred, use = "complete.obs"), 2)), by = merkis])
cat("\n  parm_korelacija: vai modeļa paredzētā pārmaiņa (no simptomu pārmaiņas) sakrīt ar faktisko pa valstīm.\n")
cat("\n  Latviešu valodas plūsma:\n")
print(R[grupa == "LVA A", .(merkis, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                            parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))], row.names = FALSE)

# SHAP: 2025 modelis, latviešu valodas plūsma 2022 → 2025
cat("\n", strrep("=", 100), "\nSHAP: kuri simptomi nes paredzēto pārmaiņu latviešu valodas plūsmā 2022 → 2025\n", strrep("=", 100), "\n", sep = "")
shap_vid <- function(d) { d <- d[grupa == "LVA A"]; s <- predict(M25, xgb.DMatrix(mat(d), missing = NA), predcontrib = TRUE)
  colSums(s * d$wn) / sum(d$wn) }
s22 <- shap_vid(PR22); s25 <- shap_vid(TE25)
SH <- data.table(pazime = names(s25), shap_2022 = s22[names(s25)], shap_2025 = s25)[, starpiba := shap_2025 - shap_2022]
KART <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, .(domens = domens[1], apraksts = apraksts[1]), by = konstrukts]
SH <- merge(SH, KART, by.x = "pazime", by.y = "konstrukts", all.x = TRUE)
SH[pazime == "Q_TRUKST", `:=`(domens = "anketas trūkums", apraksts = "Anketas trūkstošo atbilžu daļa")]
SH[pazime == "BIAS", `:=`(domens = "konstante", apraksts = "Modeļa konstante")]
SH[is.na(domens), domens := "cits"]
fwrite(SH[order(starpiba)], "output/pisa2025/simptomi_modelis_shap_2025.csv")
cat(sprintf("  Paredzētā pārmaiņa (SHAP summa): %+.1f; faktiskā: %+.1f\n", sum(SH$starpiba),
            R[grupa == "LVA A" & merkis == "2025", parm_fakt]))
cat("\n  Pa jomām:\n"); print(SH[, .(starpiba = round(sum(starpiba), 1), pazimes = .N), by = domens][order(starpiba)], row.names = FALSE)
cat("\n  20 pazīmes ar lielāko negatīvo ieguldījumu:\n")
print(head(SH[order(starpiba), .(pazime, domens, starpiba = round(starpiba, 2), apraksts = substr(apraksts, 1, 70))], 20), row.names = FALSE)
cat("\n  10 pazīmes ar lielāko pozitīvo ieguldījumu:\n")
print(head(SH[order(-starpiba), .(pazime, domens, starpiba = round(starpiba, 2), apraksts = substr(apraksts, 1, 70))], 10), row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
