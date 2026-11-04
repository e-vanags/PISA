# ============================================================================
# 64h. Simptomu modelis, 3.c solis: kopīgo pazīmju modelis (galvenais variants)
# ============================================================================
#
# KĀPĒC (64f, 64g): apvienotajā modelī pazīme, kas mērķa ciklā vairs nav uzdota, tiek
# padota kā NA, un koks to novirza patvaļīgā virzienā. Rezultāts: sistemātiski par zemu
# paredzēts līmenis (2006: +60, 2012: +87 punkti vidēji valstīs). 64g B variants, kurā
# neatbildēts jautājums (−999) ir nodalīts no neuzdota (NA), 2009, 2012, 2022 un 2025
# kļūdu samazina līdz 12–16 punktiem, bet 2006, 2015 un 2018 paliek 29–65 punkti. SHAP
# rāda, ka tās nes tieši izkritušās pazīmes.
#
# RISINĀJUMS: samazināto pazīmju modelis (reduced-feature model; Saar-Tsechansky &
# Provost, 2007, JMLR 8: 1625–1657): katram mērķa ciklam T modeli apmāca tikai ar
# pazīmēm, kas uzdotas gan ciklā T, gan iepriekšējā ciklā T−1 visās tajos piedalījušās
# valstīs. Tad
#   * mērķa ciklā nav neviena strukturāli trūkstoša pazīmes lauka (izņemot skolas vai
#     skolēna neatbildēšanu, kas kodēta −999);
#   * paredzētā pārmaiņa T−1 → T tiek aprēķināta ar vienu un to pašu pazīmju kopu, tātad
#     SHAP sadalījums ir tīrs: tas rāda, kuru simptomu pārmaiņa nes paredzēto pārmaiņu.
# Q_TRUKST pārrēķināts katram mērķim: neatbildēto (−999) daļa kopīgajās pazīmēs.
#
# Tā vēl NAV ex ante prognoze (simptomi un tests mērīti vienā reizē); ex ante ir 5. solis.
#
# SHAP sadalījums: katram mērķim un grupai vidējā SHAP pārmaiņa pa pazīmēm; latviešu
# valodas plūsma salīdzināta ar 7 salīdzinājuma valstu vidējo (starpība = "kas Latvijā
# mainījās citādi").
#
# Izvade: output/pisa2025/simptomi_modelis_C_parbaude.csv, simptomi_modelis_C_shap.csv
#         output/pisa2025/logs/64h_simptomu_modelis_kopigas_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64h_simptomu_modelis_kopigas_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)

A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A <- A[!is.na(PVREAD) & !is.na(w) & w > 0]
META <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD")
NOMINALI <- c("AUTO_PROGN", "AUTO_COBN_F", "AUTO_COBN_M", "AUTO_COBN_S", "AUTO_OCOD1", "AUTO_OCOD2", "AUTO_OCOD3",
              "AUTO_BSMJ", "AUTO_RANDUNIT", "AUTO_TESTLANG", "AUTO_ST022Q01TA", "AUTO_ST011D17TA", "AUTO_ST011D18TA",
              "AUTO_ST011D19TA", "AUTO_ST251D08JA", "AUTO_ST251D09JA")   # nominālie kodi (skat. 64g)
A[, Q_TRUKST := NULL]
KON <- setdiff(names(A), c(META, NOMINALI))
A[, wn := w / sum(w) * 1000, by = .(gads, cnt)]
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")

# 2012. gada anketas forma (rotācija): strukturāla trūkuma šūnas
q12 <- fread("data/pisa_veci/qqq_STU_2012.csv", select = c("CNT", "SCHOOLID", "STIDSTD", "QUESTID"), colClasses = "character")
q12[, skolens := paste(CNT, SCHOOLID, STIDSTD)]
A[, forma := "0"][gads == "2012", forma := q12$QUESTID[match(skolens, q12$skolens)]]
rm(q12)

# Neatbildēts (−999) pret neuzdotu (NA): šūna cikls × valsts × forma ar NA ≥ 90 % = neuzdots
ND_F <- A[, lapply(.SD, function(x) mean(is.na(x))), by = .(gads, cnt, forma), .SDcols = KON]
ND_V <- melt(A[, lapply(.SD, function(x) mean(is.na(x))), by = .(gads, cnt), .SDcols = KON],
             id.vars = c("gads", "cnt"), variable.name = "pazime", value.name = "na")
ND_V[, pazime := as.character(pazime)][, uzdots := na < 0.9]
nd_m <- as.matrix(ND_F[, ..KON]) >= 0.9
cid <- match(paste(A$gads, A$cnt, A$forma), paste(ND_F$gads, ND_F$cnt, ND_F$forma))
for (k in KON) {
  i <- which(is.na(A[[k]]) & !nd_m[cid, k])
  if (length(i)) set(A, i, k, if (is.integer(A[[k]])) -999L else -999)
}
rm(nd_m, cid); gc(verbose = FALSE)

# Pazīme pieejama ciklā, ja uzdota visās tajā piedalījušās valstīs
PIEEJ <- ND_V[, .(visas = all(uzdots)), by = .(gads, pazime)][visas == TRUE]
kopigas <- function(Tg, iepr) sort(intersect(PIEEJ[gads == Tg]$pazime, PIEEJ[gads == iepr]$pazime))

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
MERKI <- c("2006", "2009", "2012", "2015", "2018", "2022", "2025")
PAR <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.5,
            min_child_weight = 20, nthread = max(1, parallel::detectCores() - 2), eval_metric = "rmse")
wm <- function(x, w) sum(x * w) / sum(w)
mat <- function(d, F) {
  m <- as.matrix(d[, ..F])
  q <- rowMeans(m == -999, na.rm = TRUE); q[is.nan(q)] <- NA
  cbind(m, Q_TRUKST = q)
}
apmacit <- function(tr, F) {
  iv <- sample(nrow(tr), round(0.1 * nrow(tr)))
  X <- mat(tr, F)
  dtr <- xgb.DMatrix(X[-iv, ], label = tr$PVREAD[-iv], weight = tr$wn[-iv], missing = NA)
  dva <- xgb.DMatrix(X[iv, ], label = tr$PVREAD[iv], weight = tr$wn[iv], missing = NA)
  m0 <- xgb.train(PAR, dtr, nrounds = 3000, evals = list(val = dva), early_stopping_rounds = 50, verbose = 0)
  nb <- xgb.attr(m0, "best_iteration"); nb <- if (is.null(nb)) 3000 else as.integer(nb) + 1
  xgb.train(PAR, xgb.DMatrix(X, label = tr$PVREAD, weight = tr$wn, missing = NA), nrounds = nb, verbose = 0)
}
grupu_shap <- function(m, X, d) {
  S <- predict(m, xgb.DMatrix(X, missing = NA), predcontrib = TRUE)
  rowsum(S * d$wn, d$grupa) / as.vector(rowsum(d$wn, d$grupa))
}

cat("Kopīgās pazīmes (uzdotas abos ciklos visās valstīs):\n")
for (Tg in MERKI) { ie <- GADI[match(Tg, GADI) - 1]; cat(sprintf("  %s ← %s: %d pazīmes\n", Tg, ie, length(kopigas(Tg, ie)))) }

rez <- list(); SHL <- list()
for (Tg in MERKI) {
  t0 <- Sys.time(); iepr <- GADI[match(Tg, GADI) - 1]; F <- kopigas(Tg, iepr)
  m <- apmacit(A[gads < Tg], F); gc(verbose = FALSE)
  te <- A[gads == Tg]; pr <- A[gads == iepr]
  Xte <- mat(te, F); Xpr <- mat(pr, F)
  te[, pred := predict(m, xgb.DMatrix(Xte, missing = NA))]
  pr[, pred := predict(m, xgb.DMatrix(Xpr, missing = NA))]
  kop <- merge(te[, .(fakt = wm(PVREAD, wn), pred = wm(pred, wn), n = .N), by = grupa],
               pr[, .(fakt_iepr = wm(PVREAD, wn), pred_iepr = wm(pred, wn)), by = grupa], by = "grupa", all.x = TRUE)
  kop[, `:=`(merkis = Tg, iepr = iepr, pazimes = length(F), kluda_modelis = fakt - pred, kluda_naivais = fakt - fakt_iepr,
             parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr,
             r2_skoleni = 1 - sum(te$wn * (te$PVREAD - te$pred)^2) / sum(te$wn * (te$PVREAD - wm(te$PVREAD, te$wn))^2))]
  rez[[Tg]] <- kop
  g2 <- grupu_shap(m, Xte, te); g1 <- grupu_shap(m, Xpr, pr); gg <- intersect(rownames(g1), rownames(g2))
  D <- g2[gg, , drop = FALSE] - g1[gg, , drop = FALSE]
  SHL[[Tg]] <- data.table(merkis = Tg, grupa = rep(gg, ncol(D)), pazime = rep(colnames(D), each = length(gg)), starpiba = as.vector(D))
  v <- kop[grupa %in% SALID]
  cat(sprintf("\n%s ← %s (%d pazīmes, %d koki, %.1f min): skolēnu R² = %.3f; valstīs vidējā kļūda %+.1f, |kļūda| %.1f, naivais %.1f\n",
              Tg, iepr, length(F), xgb.get.num.boosted.rounds(m), as.numeric(difftime(Sys.time(), t0, units = "mins")),
              kop$r2_skoleni[1], mean(v$kluda_modelis), mean(abs(v$kluda_modelis)), mean(abs(v$kluda_naivais), na.rm = TRUE)))
  print(kop[order(grupa), .(grupa, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                            parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))], row.names = FALSE)
  rm(te, pr, Xte, Xpr, m); gc(verbose = FALSE)
}
R <- rbindlist(rez)
fwrite(R, "output/pisa2025/simptomi_modelis_C_parbaude.csv")

KART <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, .(domens = domens[1], apraksts = apraksts[1]), by = konstrukts]
SH <- merge(rbindlist(SHL), KART, by.x = "pazime", by.y = "konstrukts", all.x = TRUE)
SH[pazime == "Q_TRUKST", `:=`(domens = "anketas trūkums", apraksts = "Neatbildēto daļa kopīgajās pazīmēs")]
SH[pazime == "BIAS", `:=`(domens = "konstante", apraksts = "Modeļa konstante")]
SH[is.na(domens), domens := "cits"]
fwrite(SH, "output/pisa2025/simptomi_modelis_C_shap.csv")

# ---------------------------------------------------------------------------
# Kopsavilkumi
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 100), "\nSalīdzinājums ar 64f (A, ar nominālajiem kodiem) un 64g (B): 7 valstis bez Latvijas\n", strrep("=", 100), "\n", sep = "")
kops <- function(R, v) R[grupa %in% SALID, .(vid_kluda = round(mean(kluda_modelis), 1), abs_kluda = round(mean(abs(kluda_modelis)), 1),
                                            naivais = round(mean(abs(kluda_naivais), na.rm = TRUE), 1),
                                            parm_kor = round(cor(parm_fakt, parm_pred, use = "complete.obs"), 2)), by = merkis][, variants := v]
RA <- fread("output/pisa2025/simptomi_modelis_parbaude.csv", colClasses = c(merkis = "character"))
RB <- fread("output/pisa2025/simptomi_modelis_parbaude_B.csv", colClasses = c(merkis = "character"))
K3 <- rbind(kops(RA, "A"), kops(RB, "B"), kops(R, "C"))
print(dcast(K3, merkis ~ variants, value.var = c("vid_kluda", "abs_kluda", "parm_kor")), row.names = FALSE)
cat("\n  Naivā kļūda (iepriekšējā cikla faktiskais):", paste(kops(R, "C")$merkis, kops(R, "C")$naivais, collapse = "; "), "\n")
v <- R[grupa %in% SALID & !is.na(parm_fakt)]
cat(sprintf("\n  Visi mērķi kopā (7 valstis, %d pāri): pārmaiņas korelācija %.2f; |pārmaiņas kļūda| modelis %.1f, naivais (nulle) %.1f\n",
            nrow(v), cor(v$parm_fakt, v$parm_pred), mean(abs(v$parm_fakt - v$parm_pred)), mean(abs(v$parm_fakt))))

cat("\n", strrep("=", 100), "\nLatviešu valodas plūsma (C variants)\n", strrep("=", 100), "\n", sep = "")
SV <- R[grupa %in% SALID, .(salid_parm_fakt = mean(parm_fakt, na.rm = TRUE), salid_parm_pred = mean(parm_pred, na.rm = TRUE)), by = merkis]
LA <- merge(R[grupa == "LVA A"], SV, by = "merkis")
print(LA[, .(merkis, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
             parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1),
             rel_fakt = round(parm_fakt - salid_parm_fakt, 1), rel_pred = round(parm_pred - salid_parm_pred, 1))], row.names = FALSE)
cat("  rel_*: latviešu valodas plūsmas pārmaiņa mīnus 7 valstu vidējā pārmaiņa (faktiskā un simptomu paredzētā)\n")
cat(sprintf("  Relatīvās pārmaiņas korelācija pa mērķiem: %.2f\n", LA[, cor(parm_fakt - salid_parm_fakt, parm_pred - salid_parm_pred)]))

dom <- SH[, .(starpiba = sum(starpiba)), by = .(merkis, grupa, domens)]
dl <- dom[grupa == "LVA A"]; ds <- dom[grupa %in% SALID, .(salid = mean(starpiba)), by = .(merkis, domens)]
DD <- merge(dl, ds, by = c("merkis", "domens"), all = TRUE)
for (x in c("starpiba", "salid")) set(DD, which(is.na(DD[[x]])), x, 0)
DD[, rel := starpiba - salid]
cat("\n  Paredzētās pārmaiņas SHAP sadalījums pa jomām, latviešu valodas plūsma:\n")
print(dcast(DD, domens ~ merkis, value.var = "starpiba", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
cat("\n  Tas pats relatīvi pret 7 valstu vidējo (LVA A − salīdzinājuma valstis):\n")
print(dcast(DD, domens ~ merkis, value.var = "rel", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)

for (Tg in c("2022", "2025")) {
  f <- merge(SH[merkis == Tg & grupa == "LVA A", .(pazime, domens, apraksts, lva = starpiba)],
             SH[merkis == Tg & grupa %in% SALID, .(salid = mean(starpiba)), by = pazime], by = "pazime")[, rel := lva - salid]
  cat(sprintf("\n  %s: 15 pazīmes ar lielāko negatīvo relatīvo ieguldījumu (LVA A − 7 valstis)\n", Tg))
  print(head(f[order(rel), .(pazime, domens, lva = round(lva, 2), salid = round(salid, 2), rel = round(rel, 2), apraksts = substr(apraksts, 1, 60))], 15), row.names = FALSE)
  cat(sprintf("\n  %s: 8 pazīmes ar lielāko pozitīvo relatīvo ieguldījumu\n", Tg))
  print(head(f[order(-rel), .(pazime, domens, lva = round(lva, 2), salid = round(salid, 2), rel = round(rel, 2), apraksts = substr(apraksts, 1, 60))], 8), row.names = FALSE)
}
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
