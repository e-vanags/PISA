# ============================================================================
# 64g. Simptomu modelis, 3.b solis: kāpēc 64f paredz sistemātiski par zemu?
# ============================================================================
#
# NOVĒROJUMS (64f): slīdošā sākumpunkta pārbaudē modelis katrā mērķa ciklā visām
# valstīm paredz par 20–110 punktiem par zemu, un paredzētā pārmaiņa ir negatīva
# visur. Tas nav simptomu signāls, bet mērījuma artefakts.
#
# HIPOTĒZE: apvienotajā tabulā NA nozīmē divas dažādas lietas:
#   (a) strukturāls trūkums: jautājums šajā ciklā vai valstī nav uzdots (vai 2012. gadā
#       nebija skolēna anketas formā); tas nesaka neko par skolēnu;
#   (b) neatbildēts jautājums: skolēns jautājumu redzēja, bet neatbildēja; tas ir
#       vājāku lasītāju pazīme.
# XGBoost katram sadalījumam iemācās vienu NA virzienu. Apmācībā, kur konstrukts ir
# uzdots, NA gandrīz tikai (b), tātad NA → "vājš lasītājs". Mērķa ciklā, kur jautājums
# vairs netiek uzdots, NA ir visiem (a), bet modelis to lasa kā (b) un sodu piešķir visiem.
#
# PĀRBAUDE:
#   1. SHAP: paredzētās pārmaiņas (iepriekšējais → mērķa cikls) sadalījums pēc pazīmes
#      statusa valstī: uzdota abos ciklos / tikai iepriekšējā (izkrīt) / tikai mērķa
#      (jauna) / nevienā. Ja hipotēze pareiza, izkritušās pazīmes nes lielāko daļu kļūdas.
#   2. B variants: strukturāls trūkums paliek NA; neatbildēts jautājums tiek kodēts
#      ar atsevišķu vērtību (−999), ko koks var nodalīt. Strukturāls nozīmē: šūnā
#      cikls × valsts × anketas forma (2012 QUESTID) NA daļa ≥ 90 %. Visa slīdošā
#      pārbaude atkārtota ar B variantu, salīdzinājums ar A (64f).
#
# Izvade: output/pisa2025/simptomi_modelis_trukumu_shap.csv
#         output/pisa2025/simptomi_modelis_parbaude_B.csv, simptomi_modelis_shap_2025_B.csv
#         output/pisa2025/logs/64g_trukstoso_diagnostika_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64g_trukstoso_diagnostika_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)

A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A <- A[!is.na(PVREAD) & !is.na(w) & w > 0]
META <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD")
PAZ <- setdiff(names(A), META)
# Nominālie kodi (valsts programma, dzimšanas valsts, ISCO profesiju kodi, testa valoda,
# dispersijas vienība, valstij specifiskas mantas): to skaitliskā vērtība nav salīdzināma
# starp valstīm un cikliem, tātad tie nav simptomi. 64f tie bija iekļauti; AUTO_PROGN
# bija lielākais negatīvais SHAP ieguldījums 2025. gadā. Abi varianti šeit ir bez tiem.
NOMINALI <- c("AUTO_PROGN", "AUTO_COBN_F", "AUTO_COBN_M", "AUTO_COBN_S", "AUTO_OCOD1", "AUTO_OCOD2", "AUTO_OCOD3",
              "AUTO_BSMJ", "AUTO_RANDUNIT", "AUTO_TESTLANG", "AUTO_ST022Q01TA", "AUTO_ST011D17TA", "AUTO_ST011D18TA",
              "AUTO_ST011D19TA", "AUTO_ST251D08JA", "AUTO_ST251D09JA")
PAZ <- setdiff(PAZ, NOMINALI)
cat("Izslēgti nominālie kodi:", sum(NOMINALI %in% names(A)), "| pazīmes:", length(PAZ), "\n")
KON <- setdiff(PAZ, "Q_TRUKST")
A[, wn := w / sum(w) * 1000, by = .(gads, cnt)]
# "LVA ?" = Latvijas skolēni, kuru valodas plūsma nav nosakāma (64f tabulā tie bija "LVA")
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]

q12 <- fread("data/pisa_veci/qqq_STU_2012.csv", select = c("CNT", "SCHOOLID", "STIDSTD", "QUESTID"), colClasses = "character")
q12[, skolens := paste(CNT, SCHOOLID, STIDSTD)]
A[, forma := "0"]
A[gads == "2012", forma := q12$QUESTID[match(skolens, q12$skolens)]]
cat("2012. gada skolēna anketas formas (QUESTID):\n"); print(A[gads == "2012", .N, by = forma][order(forma)])
rm(q12)

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
MERKI <- c("2006", "2009", "2012", "2015", "2018", "2022", "2025")
PAR <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.5,
            min_child_weight = 20, nthread = max(1, parallel::detectCores() - 2), eval_metric = "rmse")
wm <- function(x, w) sum(x * w) / sum(w)

# ---------------------------------------------------------------------------
# Trūkuma struktūra
# ---------------------------------------------------------------------------
na_dala <- function(by) A[, lapply(.SD, function(x) mean(is.na(x))), by = by, .SDcols = KON]
ND_F <- na_dala(c("gads", "cnt", "forma"))            # šūnas strukturālajam trūkumam
ND_V <- melt(na_dala(c("gads", "cnt")), id.vars = c("gads", "cnt"), variable.name = "pazime", value.name = "na")
ND_V[, pazime := as.character(pazime)][, uzdots := na < 0.9]

cat("\nKonstruktu skaits, kas uzdoti (NA < 90 %), pa cikliem (LVA / citu valstu mediāna):\n")
print(ND_V[, .(uzdoti = sum(uzdots)), by = .(gads, cnt)][, .(LVA = uzdoti[cnt == "LVA"], mediana = median(uzdoti)), by = gads])
cat("\nNeatbildēto daļa uzdotajos konstruktos (vidēji, %), pa cikliem:\n")
print(ND_V[uzdots == TRUE, .(neatbildets = round(100 * mean(na), 1)), by = .(gads)][order(gads)])
rot <- ND_V[gads == "2012" & na > 0.25 & na < 0.6, .N, by = cnt]
cat("\n2012: konstrukti ar NA 25–60 % valstī (rotācijas pazīme):", paste(rot$cnt, rot$N, collapse = "; "), "\n")

# ---------------------------------------------------------------------------
# Modeļa rīki
# ---------------------------------------------------------------------------
mat <- function(d) as.matrix(d[, ..PAZ])
apmacit <- function(tr) {
  iv <- sample(nrow(tr), round(0.1 * nrow(tr)))
  dtr <- xgb.DMatrix(mat(tr[-iv]), label = tr$PVREAD[-iv], weight = tr$wn[-iv], missing = NA)
  dva <- xgb.DMatrix(mat(tr[iv]), label = tr$PVREAD[iv], weight = tr$wn[iv], missing = NA)
  m0 <- xgb.train(PAR, dtr, nrounds = 3000, evals = list(val = dva), early_stopping_rounds = 50, verbose = 0)
  nb <- xgb.attr(m0, "best_iteration"); nb <- if (is.null(nb)) 3000 else as.integer(nb) + 1
  xgb.train(PAR, xgb.DMatrix(mat(tr), label = tr$PVREAD, weight = tr$wn, missing = NA), nrounds = nb, verbose = 0)
}
shap_vid <- function(m, d) {
  s <- predict(m, xgb.DMatrix(mat(d), missing = NA), predcontrib = TRUE)
  colSums(s * d$wn) / sum(d$wn)
}
# SHAP pārmaiņas sadalījums pēc pazīmes statusa valstī
shap_klases <- function(m, D, Tg, iepr) {
  rbindlist(lapply(sort(unique(D[gads == Tg]$grupa)), function(g) {
    d1 <- D[gads == iepr & grupa == g]; d2 <- D[gads == Tg & grupa == g]
    if (nrow(d1) < 50 || nrow(d2) < 50) return(NULL)
    v1 <- shap_vid(m, d1); v2 <- shap_vid(m, d2); c0 <- d2$cnt[1]
    um <- ND_V[gads == Tg & cnt == c0]; ui <- ND_V[gads == iepr & cnt == c0]
    p <- names(v2); a <- um$uzdots[match(p, um$pazime)]; b <- ui$uzdots[match(p, ui$pazime)]
    st <- ifelse(p == "BIAS", "konstante", ifelse(p == "Q_TRUKST", "anketas trūkuma daļa",
          ifelse(a & b, "uzdota abos ciklos", ifelse(b & !a, "izkrīt (tikai iepriekšējā)",
          ifelse(a & !b, "jauna (tikai mērķa)", "nevienā")))))
    data.table(merkis = Tg, grupa = g, statuss = st, starpiba = v2 - v1)[, .(starpiba = sum(starpiba), pazimes = .N), by = .(merkis, grupa, statuss)]
  }))
}
parbaude <- function(D, nosaukums, shap_merki = character()) {
  rez <- list(); SK <- list(); M25 <- NULL
  for (Tg in MERKI) {
    t0 <- Sys.time()
    tr <- D[gads < Tg]; iepr <- GADI[match(Tg, GADI) - 1]
    m <- apmacit(tr); rm(tr); gc(verbose = FALSE)
    te <- D[gads == Tg][, pred := predict(m, xgb.DMatrix(mat(.SD), missing = NA)), .SDcols = PAZ]
    pr <- D[gads == iepr][, pred := predict(m, xgb.DMatrix(mat(.SD), missing = NA)), .SDcols = PAZ]
    kop <- merge(te[, .(fakt = wm(PVREAD, wn), pred = wm(pred, wn), n = .N), by = grupa],
                 pr[, .(fakt_iepr = wm(PVREAD, wn), pred_iepr = wm(pred, wn)), by = grupa], by = "grupa", all.x = TRUE)
    kop[, `:=`(variants = nosaukums, merkis = Tg, iepr = iepr, kluda_modelis = fakt - pred, kluda_naivais = fakt - fakt_iepr,
               parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr,
               r2_skoleni = 1 - sum(te$wn * (te$PVREAD - te$pred)^2) / sum(te$wn * (te$PVREAD - wm(te$PVREAD, te$wn))^2))]
    rez[[Tg]] <- kop
    cat(sprintf("  %s %s: skolēnu R² = %.3f, vidējā kļūda valstīs %+.1f, |kļūda| %.1f (%.1f min)\n", nosaukums, Tg, kop$r2_skoleni[1],
                mean(kop[!grepl("^LVA", grupa)]$kluda_modelis), mean(abs(kop[!grepl("^LVA", grupa)]$kluda_modelis)),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    if (Tg %in% shap_merki) SK[[Tg]] <- shap_klases(m, D, Tg, iepr)
    if (Tg == "2025") M25 <- m
    rm(te, pr); gc(verbose = FALSE)
  }
  list(R = rbindlist(rez), SK = rbindlist(SK), M25 = M25)
}
kops <- function(R) R[!grepl("^LVA", grupa), .(vid_kluda = round(mean(kluda_modelis), 1), abs_kluda = round(mean(abs(kluda_modelis)), 1),
                                               naivais = round(mean(abs(kluda_naivais), na.rm = TRUE), 1),
                                               parm_kor = round(cor(parm_fakt, parm_pred, use = "complete.obs"), 2),
                                               r2 = round(r2_skoleni[1], 3)), by = merkis]

# ---------------------------------------------------------------------------
# 1. A variants (kā 64f): kuras pazīmes nes kļūdu
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n1. A variants (visi NA vienādi, kā 64f): pārmaiņas SHAP sadalījums pēc pazīmes statusa\n", strrep("=", 100), "\n", sep = "")
RA <- parbaude(A, "A", shap_merki = c("2006", "2012", "2025"))
SKA <- RA$SK[, variants := "A"]
print(dcast(SKA, merkis + grupa ~ statuss, value.var = "starpiba", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. B variants: neatbildēts ≠ nav uzdots
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\n2. B variants: neatbildēts jautājums = −999, strukturāls trūkums = NA\n", strrep("=", 100), "\n", sep = "")
nd_m <- as.matrix(ND_F[, ..KON]) >= 0.9
cid <- match(paste(A$gads, A$cnt, A$forma), paste(ND_F$gads, ND_F$cnt, ND_F$forma))
iz <- 0
for (k in KON) {
  i <- which(is.na(A[[k]]) & !nd_m[cid, k])
  if (length(i)) { set(A, i, k, if (is.integer(A[[k]])) -999L else -999); iz <- iz + length(i) }
}
cat(sprintf("Pārkodētas neatbildētās šūnas: %s (%.1f %% no visām konstruktu šūnām)\n", format(iz, big.mark = " "), 100 * iz / (nrow(A) * length(KON))))
RB <- parbaude(A, "B", shap_merki = c("2006", "2012", "2025"))
SKB <- RB$SK[, variants := "B"]
print(dcast(SKB, merkis + grupa ~ statuss, value.var = "starpiba", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
fwrite(rbind(SKA, SKB), "output/pisa2025/simptomi_modelis_trukumu_shap.csv")
fwrite(RB$R, "output/pisa2025/simptomi_modelis_parbaude_B.csv")

cat("\n", strrep("=", 100), "\n3. Salīdzinājums: valstu līmenī (bez Latvijas plūsmām)\n", strrep("=", 100), "\n", sep = "")
cat("  vid_kluda: vidējā kļūda (fakt − pred), abs_kluda: vidējā absolūtā kļūda, naivais: iepriekšējā cikla faktiskais,\n",
    " parm_kor: paredzētās un faktiskās pārmaiņas korelācija pa valstīm, r2: skolēnu līmeņa R² mērķa ciklā\n")
print(merge(kops(RA$R), kops(RB$R), by = "merkis", suffixes = c(".A", ".B")), row.names = FALSE)
cat("\n  Latviešu valodas plūsma (B variants):\n")
print(RB$R[grupa == "LVA A", .(merkis, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                               naivais = round(kluda_naivais, 1), parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))], row.names = FALSE)
cat("\n  Visas grupas 2025 (B variants):\n")
print(RB$R[merkis == "2025", .(grupa, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                               parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))][order(grupa)], row.names = FALSE)

# SHAP 2025, B variants: latviešu valodas plūsma 2022 → 2025 pa jomām
cat("\n", strrep("=", 100), "\n4. B variants, SHAP: latviešu valodas plūsma 2022 → 2025\n", strrep("=", 100), "\n", sep = "")
s22 <- shap_vid(RB$M25, A[gads == "2022" & grupa == "LVA A"]); s25 <- shap_vid(RB$M25, A[gads == "2025" & grupa == "LVA A"])
SH <- data.table(pazime = names(s25), shap_2022 = s22[names(s25)], shap_2025 = s25)[, starpiba := shap_2025 - shap_2022]
KART <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, .(domens = domens[1], apraksts = apraksts[1]), by = konstrukts]
SH <- merge(SH, KART, by.x = "pazime", by.y = "konstrukts", all.x = TRUE)
SH[pazime == "Q_TRUKST", `:=`(domens = "anketas trūkums", apraksts = "Anketas trūkstošo atbilžu daļa")]
SH[pazime == "BIAS", `:=`(domens = "konstante", apraksts = "Modeļa konstante")]
SH[is.na(domens), domens := "cits"]
u25 <- ND_V[gads == "2025" & cnt == "LVA"]; u22 <- ND_V[gads == "2022" & cnt == "LVA"]
SH[, statuss := fifelse(u25$uzdots[match(pazime, u25$pazime)] & u22$uzdots[match(pazime, u22$pazime)], "abos", "ne abos", na = "—")]
fwrite(SH[order(starpiba)], "output/pisa2025/simptomi_modelis_shap_2025_B.csv")
cat(sprintf("  Paredzētā pārmaiņa (SHAP summa): %+.1f; faktiskā: %+.1f\n", sum(SH$starpiba), RB$R[grupa == "LVA A" & merkis == "2025", parm_fakt]))
cat("\n  Pa pazīmes statusu:\n"); print(SH[, .(starpiba = round(sum(starpiba), 1), pazimes = .N), by = statuss], row.names = FALSE)
cat("\n  Pa jomām (tikai pazīmes, kas uzdotas abos ciklos):\n")
print(SH[statuss == "abos", .(starpiba = round(sum(starpiba), 1), pazimes = .N), by = domens][order(starpiba)], row.names = FALSE)
cat("\n  20 pazīmes ar lielāko negatīvo ieguldījumu:\n")
print(head(SH[order(starpiba), .(pazime, domens, statuss, starpiba = round(starpiba, 2), apraksts = substr(apraksts, 1, 60))], 20), row.names = FALSE)
cat("\n  10 pazīmes ar lielāko pozitīvo ieguldījumu:\n")
print(head(SH[order(-starpiba), .(pazime, domens, statuss, starpiba = round(starpiba, 2), apraksts = substr(apraksts, 1, 60))], 10), row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
