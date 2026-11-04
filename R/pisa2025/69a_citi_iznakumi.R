# ============================================================================
# 69a. Slānis 9: simptomu modelis ar citiem iznākumiem
# ============================================================================
#
# Tas pats C variants (64h: kopīgās pazīmes, −999 = neatbildēts, slīdošs sākumpunkts),
# bet iznākums nav lasīšanas vidējais:
#   zemL2  : PVREAD < 407,47 (binārs, logistiskais mērķis) — vai simptomi apakšgalu paredz
#            labāk nekā vidējo;
#   L4plus : PVREAD ≥ 552,89 (binārs) — augšgals;
#   q10/q50/q90 : kvantiļu regresija (reg:quantileerror) — vai simptomu karte atšķiras
#            sadalījuma galos; grupas rādītājs = paredzētā nosacītā kvantile vidēji, pret
#            faktisko grupas kvantili;
#   MATH, SCIE : matemātikas un dabaszinātņu PV vidējais ar tām pašām pazīmēm — pazīme, kas
#            paredz tikai lasīšanu, ir lasīšanas simptoms.
# Mērķi 2018, 2022, 2025 (laika dēļ); eta 0,1, ≤ 1500 koki.
# Novērtējums: grupas līmenī (valstis, LVA A) faktiskais pret paredzēto, pārmaiņa un
# relatīvā pārmaiņa; SHAP pa jomām LVA A relatīvi pret 7 valstīm; lasīšanas un matemātikas
# SHAP salīdzinājums pa pazīmēm.
#
# Izvade: output/pisa2025/citi_iznakumi_parbaude.csv, citi_iznakumi_shap.csv,
#         data/pisa2025/processed/simptomi_iznakumi.rds
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("69a_citi_iznakumi")
D <- sagatavot_datus(0.9); A <- D$A; PIEEJ <- pieejamiba(D$ND_V); rm(D); gc(verbose = FALSE)

# Matemātikas un dabaszinātņu PV vidējais pēc skolēna atslēgas (kā 64e)
FI <- "data/pisa2025/processed/simptomi_iznakumi.rds"
if (file.exists(FI)) IZ <- readRDS(FI) else {
  IZ <- list()
  for (g in c("2003", "2006", "2009", "2012")) {
    s <- fread(sprintf("data/pisa_veci/qqq_STU_%s.csv", g), colClasses = "character")
    pm <- grep("^PV[0-9]+MATH$", names(s), value = TRUE); ps <- grep("^PV[0-9]+SCIE$", names(s), value = TRUE)
    IZ[[g]] <- data.table(skolens = paste(s$CNT, s$SCHOOLID, s$STIDSTD),
                          PVMATH = rowMeans(sapply(pm, function(v) suppressWarnings(as.numeric(s[[v]]))), na.rm = TRUE),
                          PVSCIE = if (length(ps)) rowMeans(sapply(ps, function(v) suppressWarnings(as.numeric(s[[v]]))), na.rm = TRUE) else NA_real_)
  }
  for (g in c("2015", "2018", "2022", "2025")) {
    s <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))$stu
    pm <- grep("^PV[0-9]+MATH$", names(s), value = TRUE); ps <- grep("^PV[0-9]+SCIE$", names(s), value = TRUE)
    IZ[[g]] <- data.table(skolens = paste(g, sk_num(s$CNTSTUID)),
                          PVMATH = rowMeans(sapply(pm, function(v) sk_num(s[[v]])), na.rm = TRUE),
                          PVSCIE = rowMeans(sapply(ps, function(v) sk_num(s[[v]])), na.rm = TRUE))
  }
  IZ <- rbindlist(IZ); saveRDS(IZ, FI)
}
A <- merge(A, IZ, by = "skolens", all.x = TRUE)
A[, `:=`(zemL2 = as.integer(PVREAD < 407.47), L4plus = as.integer(PVREAD >= 552.89))]
cat("Iznākumi: zem 2. līmeņa", round(100 * A[, wm(zemL2, wn)], 1), "%; 4.+ līmenis", round(100 * A[, wm(L4plus, wn)], 1),
    "%; PVMATH pieejams", round(100 * mean(!is.na(A$PVMATH)), 1), "%; PVSCIE", round(100 * mean(!is.na(A$PVSCIE)), 1), "%\n")

MERKI3 <- c("2018", "2022", "2025")
par01 <- PAR_C; par01$eta <- 0.1
wq <- function(x, w, p) { i <- !is.na(x); x <- x[i]; w <- w[i]; o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }
IZNAK <- list(
  list(nos = "zemL2", y = "zemL2", par = modifyList(par01, list(objective = "binary:logistic", eval_metric = "logloss")), fakt = function(d) wm(d$zemL2, d$wn) * 100, sk = 100),
  list(nos = "L4plus", y = "L4plus", par = modifyList(par01, list(objective = "binary:logistic", eval_metric = "logloss")), fakt = function(d) wm(d$L4plus, d$wn) * 100, sk = 100),
  list(nos = "q10", y = "PVREAD", par = modifyList(par01, list(objective = "reg:quantileerror", quantile_alpha = 0.1, eval_metric = "rmse")), fakt = function(d) wq(d$PVREAD, d$wn, 0.1), sk = 1),
  list(nos = "q50", y = "PVREAD", par = modifyList(par01, list(objective = "reg:quantileerror", quantile_alpha = 0.5, eval_metric = "rmse")), fakt = function(d) wq(d$PVREAD, d$wn, 0.5), sk = 1),
  list(nos = "q90", y = "PVREAD", par = modifyList(par01, list(objective = "reg:quantileerror", quantile_alpha = 0.9, eval_metric = "rmse")), fakt = function(d) wq(d$PVREAD, d$wn, 0.9), sk = 1),
  list(nos = "MATH", y = "PVMATH", par = par01, fakt = function(d) wm(d$PVMATH, d$wn), sk = 1),
  list(nos = "SCIE", y = "PVSCIE", par = par01, fakt = function(d) wm(d$PVSCIE, d$wn), sk = 1),
  list(nos = "READ", y = "PVREAD", par = par01, fakt = function(d) wm(d$PVREAD, d$wn), sk = 1))

REZ <- list(); SHL <- list()
for (Tg in MERKI3) for (iz in IZNAK) {
  t0 <- Sys.time(); iepr <- iepr_gads(Tg); pz <- kopigas(PIEEJ, Tg, iepr)
  IZK <- c("PVREAD", "zemL2", "L4plus", "PVMATH", "PVSCIE")
  B <- A[gads <= Tg & !is.na(get(iz$y)), c("gads", "grupa", "wn", IZK, pz), with = FALSE]; B[, y := get(iz$y)]
  m <- apmacit(B[gads < Tg], pz, iz$par, merkis = "y", nmax = 1500)
  te <- B[gads == Tg]; pr <- B[gads == iepr]; Xte <- mat(te, pz); Xpr <- mat(pr, pz)
  te[, pred := predict(m, xgb.DMatrix(Xte, missing = NA)) * iz$sk]; pr[, pred := predict(m, xgb.DMatrix(Xpr, missing = NA)) * iz$sk]
  ff <- iz$fakt
  kop <- merge(te[, .(fakt = ff(.SD), pred = wm(pred, wn)), by = grupa, .SDcols = c(IZK, "wn")],
               pr[, .(fakt_iepr = ff(.SD), pred_iepr = wm(pred, wn)), by = grupa, .SDcols = c(IZK, "wn")], by = "grupa", all.x = TRUE)
  kop[, `:=`(iznakums = iz$nos, merkis = Tg, iepr = iepr, pazimes = length(pz), parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr)]
  REZ[[paste(Tg, iz$nos)]] <- kop
  g2 <- grupu_shap(m, Xte, te); g1 <- grupu_shap(m, Xpr, pr); gg <- intersect(rownames(g1), rownames(g2)); Dd <- g2[gg, , drop = FALSE] - g1[gg, , drop = FALSE]
  SHL[[paste(Tg, iz$nos)]] <- data.table(iznakums = iz$nos, merkis = Tg, grupa = rep(gg, ncol(Dd)), pazime = rep(colnames(Dd), each = length(gg)), starpiba = as.vector(Dd) * iz$sk)
  v <- kop[grupa %in% SALID]
  cat(sprintf("%s %-6s (%d pazīmes, %.1f min): 7 valstu |kļūda| %.2f; pārmaiņas kor %.2f; LVA A fakt %+.2f pred %+.2f; rel fakt %+.2f pred %+.2f\n", Tg, iz$nos, length(pz),
              as.numeric(difftime(Sys.time(), t0, units = "mins")), mean(abs(v$fakt - v$pred)), kor(v$parm_fakt, v$parm_pred),
              kop[grupa == "LVA A", parm_fakt], kop[grupa == "LVA A", parm_pred], kop[grupa == "LVA A", parm_fakt] - mean(v$parm_fakt, na.rm = TRUE), kop[grupa == "LVA A", parm_pred] - mean(v$parm_pred, na.rm = TRUE)))
  rm(m, te, pr, Xte, Xpr, B); gc(verbose = FALSE)
}
# Faktiskie zem 2. līmeņa / 4.+ ir daļa (%), paredzētais arī % (varbūtību vidējais); kvantilēm punkti.
R <- rbindlist(REZ); SH <- rbindlist(SHL)
fwrite(R, "output/pisa2025/citi_iznakumi_parbaude.csv"); fwrite(SH, "output/pisa2025/citi_iznakumi_shap.csv")

cat("\n", strrep("=", 100), "\nKopsavilkums pa iznākumiem: 7 valstis (grupu līmenis)\n", strrep("=", 100), "\n", sep = "")
V <- R[grupa %in% SALID & !is.na(parm_fakt)]
V[, `:=`(rel_f = parm_fakt - mean(parm_fakt), rel_p = parm_pred - mean(parm_pred)), by = .(iznakums, merkis)]
print(V[, .(pari = .N, limena_mae = round(mean(abs(fakt - pred)), 2), parm_mae = round(mean(abs(parm_fakt - parm_pred)), 2), naivais = round(mean(abs(parm_fakt)), 2),
            parm_kor = round(kor(parm_fakt, parm_pred), 2), rel_kor = round(kor(rel_f, rel_p), 2), rel_mae = round(mean(abs(rel_f - rel_p)), 2), rel_naiv = round(mean(abs(rel_f)), 2)), by = iznakums], row.names = FALSE)
cat("\nLatviešu valodas plūsma: pārmaiņa un relatīvā pārmaiņa (fakt / pred) pa iznākumiem un mērķiem\n")
S7 <- V[, .(sf = mean(parm_fakt), sp = mean(parm_pred)), by = .(iznakums, merkis)]
LA <- merge(R[grupa == "LVA A"], S7, by = c("iznakums", "merkis"))[, `:=`(rel_fakt = parm_fakt - sf, rel_pred = parm_pred - sp)]
print(dcast(LA, iznakums ~ merkis, value.var = c("parm_fakt", "parm_pred", "rel_fakt", "rel_pred"), fun.aggregate = function(x) round(x[1], 2)), row.names = FALSE)

cat("\n", strrep("=", 100), "\nSHAP pa jomām 2025, LVA A relatīvi pret 7 valstīm, pa iznākumiem\n", strrep("=", 100), "\n", sep = "")
dom <- SH[merkis == "2025", .(s = sum(starpiba)), by = .(iznakums, grupa, domens = jomas(pazime))]
DD <- merge(dom[grupa == "LVA A", .(iznakums, domens, lva = s)], dom[grupa %in% SALID, .(salid = mean(s)), by = .(iznakums, domens)], by = c("iznakums", "domens"), all = TRUE)
for (x in c("lva", "salid")) set(DD, which(is.na(DD[[x]])), x, 0); DD[, rel := lva - salid]
print(dcast(DD, domens ~ iznakums, value.var = "rel", fun.aggregate = function(x) round(sum(x), 2)), row.names = FALSE)

cat("\n", strrep("=", 100), "\nLasīšanas specifiskie simptomi 2025: LVA A relatīvais SHAP ieguldījums lasīšanā pret matemātiku\n", strrep("=", 100), "\n", sep = "")
f <- function(iz) merge(SH[merkis == "2025" & iznakums == iz & grupa == "LVA A", .(pazime, lva = starpiba)],
                        SH[merkis == "2025" & iznakums == iz & grupa %in% SALID, .(salid = mean(starpiba)), by = pazime], by = "pazime")[, .(pazime, rel = lva - salid)]
X <- merge(f("READ"), f("MATH"), by = "pazime", suffixes = c("_read", "_math")); X <- merge(X, f("SCIE")[, .(pazime, rel_scie = rel)], by = "pazime")
X[, `:=`(domens = jomas(pazime), apraksts = apraksts(pazime), spec = rel_read - (rel_math + rel_scie) / 2)]
cat("  Korelācija starp pazīmju relatīvajiem ieguldījumiem lasīšanā un matemātikā:", round(X[, kor(rel_read, rel_math)], 2), "; dabaszinātnēs:", round(X[, kor(rel_read, rel_scie)], 2), "\n")
cat("  Lasīšanai specifiskākie negatīvie (rel_read − vid(mat, dab)):\n")
print(head(X[order(spec), .(pazime, domens, rel_read = round(rel_read, 2), rel_math = round(rel_math, 2), rel_scie = round(rel_scie, 2), spec = round(spec, 2), apraksts)], 15), row.names = FALSE)
cat("  Kopīgie (visās jomās) negatīvie:\n")
print(head(X[order(rel_read + rel_math + rel_scie), .(pazime, domens, rel_read = round(rel_read, 2), rel_math = round(rel_math, 2), rel_scie = round(rel_scie, 2), apraksts)], 10), row.names = FALSE)
beigt_zurnalu()
