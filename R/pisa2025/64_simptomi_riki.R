# ============================================================================
# 64_simptomi_riki.R: kopīgās funkcijas simptomu modeļa skriptiem 64i–64m
# ============================================================================
#
# Dati kā 64h C variantā (docs/61, 3c):
#   * apvienotā skolēnu tabula (64e): 9 cikli 2000–2025, LVA + 7 valstis;
#   * nominālie kodi izslēgti;
#   * neatbildēts jautājums = −999; neuzdots = NA (NA daļa ≥ slieksnis šūnā
#     cikls × valsts × 2012. gada anketas forma);
#   * pazīme pieejama ciklā, ja uzdota visās (vai vismaz min_valstu) tajā
#     piedalījušās valstīs;
#   * pēc izvēles replikāciju svari 2015–2025 (BRR, Fay 0,5, 80 replikas),
#     normēti ar to pašu koeficientu kā galvenais svars (katra valsts × cikls = 1000).
# Modelis: XGBoost regresija, mērķis PVREAD (lasīšanas PV vidējais), svari wn.
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(xgboost) })

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
MERKI <- c("2006", "2009", "2012", "2015", "2018", "2022", "2025")
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
SALID5 <- c("CZE", "DEU", "FIN", "HUN", "POL")
META <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD")
NOMINALI <- c("AUTO_PROGN", "AUTO_COBN_F", "AUTO_COBN_M", "AUTO_COBN_S", "AUTO_OCOD1", "AUTO_OCOD2", "AUTO_OCOD3",
              "AUTO_BSMJ", "AUTO_RANDUNIT", "AUTO_TESTLANG", "AUTO_ST022Q01TA", "AUTO_ST011D17TA", "AUTO_ST011D18TA",
              "AUTO_ST011D19TA", "AUTO_ST251D08JA", "AUTO_ST251D09JA")
RW <- paste0("RW", 1:80)
PAR_C <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.5,
              min_child_weight = 20, nthread = max(1, parallel::detectCores() - 2), eval_metric = "rmse")
KART <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, .(domens = domens[1], apraksts = apraksts[1]), by = konstrukts]

iepr_gads <- function(Tg, k = 1) GADI[match(Tg, GADI) - k]
sk_num <- function(x) suppressWarnings(as.numeric(if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x))
wm <- function(x, w) sum(x * w) / sum(w)
brr_var <- function(E) colSums(sweep(E[-1, , drop = FALSE], 2, E[1, ])^2) / 20   # 80 replikas, Fay 0,5
jomas <- function(p) {
  d <- KART$domens[match(sub("__B$", "", p), KART$konstrukts)]   # __B = konstrukta daļa pēc skalas maiņas (64e)
  d[p == "Q_TRUKST"] <- "anketas trūkums"; d[p == "BIAS"] <- "konstante"; d[is.na(d)] <- "cits"; d
}
apraksts <- function(p) {
  a <- KART$apraksts[match(sub("__B$", "", p), KART$konstrukts)]
  a[p == "Q_TRUKST"] <- "Neatbildēto daļa kopīgajās pazīmēs"; a[is.na(a)] <- ""; substr(a, 1, 60)
}
sakt_zurnalu <- function(nos) {
  while (sink.number() > 0) sink()
  f <- sprintf("output/pisa2025/logs/%s_output.txt", nos)
  con <- file(f, open = "wt", encoding = "UTF-8"); sink(con, split = TRUE)
  assign(".zurn_con", con, envir = .GlobalEnv)
  cat("Izvade tiek saglabāta:", f, "\n\n")
}
beigt_zurnalu <- function() {
  cat("\nPabeigts:", format(Sys.time()), "\n")
  while (sink.number() > 0) sink(); close(get(".zurn_con", envir = .GlobalEnv))
}

# ---------------------------------------------------------------------------
# Datu sagatavošana
# ---------------------------------------------------------------------------
sagatavot_datus <- function(slieksnis = 0.9, replikas = FALSE) {
  A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
  A <- A[!is.na(PVREAD) & !is.na(w) & w > 0]
  A[, Q_TRUKST := NULL]
  A[, kf := 1000 / sum(w), by = .(gads, cnt)][, wn := w * kf]
  A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
  q12 <- fread("data/pisa_veci/qqq_STU_2012.csv", select = c("CNT", "SCHOOLID", "STIDSTD", "QUESTID"), colClasses = "character")
  q12[, skolens := paste(CNT, SCHOOLID, STIDSTD)]
  A[, forma := "0"][gads == "2012", forma := q12$QUESTID[match(skolens, q12$skolens)]]
  KON <- setdiff(names(A), c(META, NOMINALI, "kf", "wn", "grupa", "forma"))
  ND_F <- A[, lapply(.SD, function(x) mean(is.na(x))), by = .(gads, cnt, forma), .SDcols = KON]
  ND_V <- melt(A[, lapply(.SD, function(x) mean(is.na(x))), by = .(gads, cnt), .SDcols = KON],
               id.vars = c("gads", "cnt"), variable.name = "pazime", value.name = "na")
  ND_V[, pazime := as.character(pazime)][, uzdots := na < slieksnis]
  nd_m <- as.matrix(ND_F[, ..KON]) >= slieksnis
  cid <- match(paste(A$gads, A$cnt, A$forma), paste(ND_F$gads, ND_F$cnt, ND_F$forma))
  for (k in KON) {
    i <- which(is.na(A[[k]]) & !nd_m[cid, k])
    if (length(i)) set(A, i, k, if (is.integer(A[[k]])) -999L else -999)
  }
  rm(nd_m, cid, ND_F); gc(verbose = FALSE)
  if (replikas) {
    for (r in RW) set(A, j = r, value = NA_real_)
    for (g in c("2015", "2018", "2022", "2025")) {
      X <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))$stu
      nm <- names(X); rw <- grep("^W_FSTURWT[0-9]+$", nm, value = TRUE, ignore.case = TRUE)
      rw <- rw[order(as.integer(sub("^W_FSTURWT", "", toupper(rw))))]
      id <- paste(g, sk_num(X[[grep("^CNTSTUID$", nm, ignore.case = TRUE, value = TRUE)[1]]]))
      ia <- which(A$gads == g); mm <- match(A$skolens[ia], id)
      cat(sprintf("  Replikāciju svari %s: %d no %d skolēniem\n", g, sum(!is.na(mm)), length(ia)))
      for (j in seq_along(RW)) set(A, ia, RW[j], sk_num(X[[rw[j]]])[mm] * A$kf[ia])
      rm(X); gc(verbose = FALSE)
    }
  }
  list(A = A, KON = KON, ND_V = ND_V)
}

pieejamiba <- function(ND_V, min_valstu = Inf)
  ND_V[, .(ok = sum(uzdots) >= min(min_valstu, .N)), by = .(gads, pazime)][ok == TRUE, .(gads, pazime)]
kopigas <- function(PIEEJ, g1, g2, atlase = NULL) {
  f <- intersect(PIEEJ[gads == g1]$pazime, PIEEJ[gads == g2]$pazime)
  if (!is.null(atlase)) f <- intersect(f, atlase)
  sort(f)
}

# Pazīmju grupas: WLE indeksi, ne-jautājumi, mājas mantu saime
wle_konstrukti <- function() {
  K <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")
  I <- rbind(fread("output/pisa2025/simptomi_inventars_veci.csv", colClasses = "character")[, .(gads, fails, mainigais, etikete)],
             fread("output/pisa2025/simptomi_inventars_jaunie.csv", colClasses = "character")[, .(gads, fails, mainigais, etikete)])
  I[, M := toupper(mainigais)]
  wle_nos <- unique(I[grepl("WLE", etikete)]$M)
  K[, M := toupper(mainigais)]
  K <- merge(K, unique(I[, .(gads, fails, M, etikete)], by = c("gads", "fails", "M")), by = c("gads", "fails", "M"), all.x = TRUE)
  K[, jaut := grepl("^(ST|SC|IC|EC|PA|WB|FL|TC|CT|FT)[0-9]{2,3}[QD][0-9]{2}", M)]
  K[, wle := grepl("WLE", etikete) | (M %in% wle_nos) | (!jaut & grepl("[Ii]ndex of|ESCS|[Ss]cale", etikete))]
  list(wle = sort(unique(K[wle == TRUE]$konstrukts)), ne_jautajumi = sort(unique(K[jaut == FALSE]$konstrukts)),
       mantas = sort(unique(K[M %in% c("HOMEPOS", "ESCS", "WEALTH", "CULTPOSS", "HEDRES", "ICTRES", "ICTHOME")]$konstrukts)), K = K)
}

# ---------------------------------------------------------------------------
# Modelis
# ---------------------------------------------------------------------------
mat <- function(d, pz, sent = -999) {
  m <- as.matrix(d[, ..pz])
  q <- rowMeans(m == sent, na.rm = TRUE); q[is.nan(q)] <- NA
  cbind(m, Q_TRUKST = q)
}
apmacit <- function(tr, pz, par = PAR_C, seed = 20260912, merkis = "PVREAD", nmax = 3000, sent = -999) {
  set.seed(seed); par$seed <- seed
  X <- mat(tr, pz, sent); y <- tr[[merkis]]; iv <- sample(nrow(tr), round(0.1 * nrow(tr)))
  dtr <- xgb.DMatrix(X[-iv, ], label = y[-iv], weight = tr$wn[-iv], missing = NA)
  dva <- xgb.DMatrix(X[iv, ], label = y[iv], weight = tr$wn[iv], missing = NA)
  m0 <- xgb.train(par, dtr, nrounds = nmax, evals = list(val = dva), early_stopping_rounds = 50, verbose = 0)
  nb <- xgb.attr(m0, "best_iteration"); nb <- if (is.null(nb)) nmax else as.integer(nb) + 1
  xgb.train(par, xgb.DMatrix(X, label = y, weight = tr$wn, missing = NA), nrounds = nb, verbose = 0)
}
grupu_shap <- function(m, X, d) {
  S <- predict(m, xgb.DMatrix(X, missing = NA), predcontrib = TRUE)
  rowsum(S * d$wn, d$grupa) / as.vector(rowsum(d$wn, d$grupa))
}

# Slīdoša sākumpunkta pārbaude (C variants ar iespējām)
#   atlase: atļautās pazīmes; centret: pazīmes un mērķis centrēti pa grupām ar apmācības
#   ciklu vidējiem (tikai valsts iekšējā dinamika); merkis: PVREAD vai viens PV.
slidosa_parbaude <- function(A, PIEEJ, merki = MERKI, atlase = NULL, centret = FALSE, par = PAR_C, seed = 20260912,
                             merkis = "PVREAD", nmax = 3000, zinot = TRUE) {
  rez <- list(); SHL <- list()
  for (Tg in merki) {
    t0 <- Sys.time(); iepr <- iepr_gads(Tg); pz <- kopigas(PIEEJ, Tg, iepr, atlase)
    B <- A[gads <= Tg, unique(c("gads", "grupa", "wn", "PVREAD", merkis, pz)), with = FALSE]
    B[, y := get(merkis)][, mu_y := 0]
    sent <- -999
    if (centret) {
      mu <- B[gads < Tg, .(m = sum(y * wn) / sum(wn)), by = grupa]
      B[, mu_y := mu$m[match(grupa, mu$grupa)]]
      B <- B[!is.na(mu_y)][, y := y - mu_y]
      sent <- -1e7; trr <- B$gads < Tg
      for (k in pz) {
        x <- as.numeric(B[[k]]); s <- !is.na(x) & x == -999; ok <- !is.na(x) & !s; t <- ok & trr
        mk <- tapply(x[t] * B$wn[t], B$grupa[t], sum) / tapply(B$wn[t], B$grupa[t], sum)
        mg <- as.numeric(mk[B$grupa])
        x[ok] <- x[ok] - mg[ok]; x[s] <- sent
        set(B, j = k, value = x)
      }
    }
    m <- apmacit(B[gads < Tg], pz, par, seed, "y", nmax, sent)
    te <- B[gads == Tg]; pr <- B[gads == iepr]
    Xte <- mat(te, pz, sent); Xpr <- mat(pr, pz, sent)
    te[, pred := predict(m, xgb.DMatrix(Xte, missing = NA)) + mu_y]
    pr[, pred := predict(m, xgb.DMatrix(Xpr, missing = NA)) + mu_y]
    kop <- merge(te[, .(fakt = wm(PVREAD, wn), pred = wm(pred, wn), n = .N), by = grupa],
                 pr[, .(fakt_iepr = wm(PVREAD, wn), pred_iepr = wm(pred, wn)), by = grupa], by = "grupa", all.x = TRUE)
    kop[, `:=`(merkis = Tg, iepr = iepr, pazimes = length(pz), kluda_modelis = fakt - pred, kluda_naivais = fakt - fakt_iepr,
               parm_fakt = fakt - fakt_iepr, parm_pred = pred - pred_iepr,
               r2_skoleni = 1 - sum(te$wn * (te$PVREAD - te$pred)^2) / sum(te$wn * (te$PVREAD - wm(te$PVREAD, te$wn))^2))]
    rez[[Tg]] <- kop
    g2 <- grupu_shap(m, Xte, te); g1 <- grupu_shap(m, Xpr, pr); gg <- intersect(rownames(g1), rownames(g2))
    Dd <- g2[gg, , drop = FALSE] - g1[gg, , drop = FALSE]
    SHL[[Tg]] <- data.table(merkis = Tg, grupa = rep(gg, ncol(Dd)), pazime = rep(colnames(Dd), each = length(gg)), starpiba = as.vector(Dd))
    if (zinot) {
      v <- kop[grupa %in% SALID]
      cat(sprintf("\n%s ← %s (%d pazīmes, %d koki, %.1f min): skolēnu R² = %.3f; valstīs vid. kļūda %+.1f, |kļūda| %.1f, naivais %.1f\n",
                  Tg, iepr, length(pz), xgb.get.num.boosted.rounds(m), as.numeric(difftime(Sys.time(), t0, units = "mins")),
                  kop$r2_skoleni[1], mean(v$kluda_modelis), mean(abs(v$kluda_modelis)), mean(abs(v$kluda_naivais), na.rm = TRUE)))
      print(kop[order(grupa), .(grupa, fakt = round(fakt, 1), pred = round(pred, 1), kluda = round(kluda_modelis, 1),
                                parm_fakt = round(parm_fakt, 1), parm_pred = round(parm_pred, 1))], row.names = FALSE)
    }
    rm(B, te, pr, Xte, Xpr, m); gc(verbose = FALSE)
  }
  list(R = rbindlist(rez), SH = rbindlist(SHL))
}

# ---------------------------------------------------------------------------
# Kopsavilkumi
# ---------------------------------------------------------------------------
# Droša korelācija: NA, ja pilnu pāru < 3 vai izkliede 0 (64m kopsavilkums apstājās 2026-09-13)
kor <- function(a, b) { i <- is.finite(a) & is.finite(b); if (sum(i) < 3 || sd(a[i]) == 0 || sd(b[i]) == 0) NA_real_ else cor(a[i], b[i]) }
metrikas <- function(R, grupas = SALID) {
  v <- R[grupa %in% grupas]
  pm <- v[, .(abs_kluda = mean(abs(kluda_modelis), na.rm = TRUE), vid_kluda = mean(kluda_modelis, na.rm = TRUE),
              naivais = mean(abs(kluda_naivais), na.rm = TRUE), parm_kor = kor(parm_fakt, parm_pred)), by = merkis]
  vv <- v[!is.na(parm_fakt) & !is.na(parm_pred)]
  vv[, `:=`(rel_f = parm_fakt - mean(parm_fakt), rel_p = parm_pred - mean(parm_pred)), by = merkis]
  kop <- vv[, .(pari = .N, parm_kor = kor(parm_fakt, parm_pred), parm_mae = mean(abs(parm_fakt - parm_pred)),
                naivais_mae = mean(abs(parm_fakt)), virziens = mean(sign(parm_fakt) == sign(parm_pred)),
                rel_kor = kor(rel_f, rel_p), rel_mae = mean(abs(rel_f - rel_p)), rel_naivais = mean(abs(rel_f)),
                rel_virziens = mean(sign(rel_f) == sign(rel_p)))]
  list(pa_merki = pm, kop = kop)
}
lva_rel <- function(R, sal = SALID) {
  s <- R[grupa %in% sal, .(sf = mean(parm_fakt, na.rm = TRUE), sp = mean(parm_pred, na.rm = TRUE)), by = merkis]
  x <- merge(R[grupa == "LVA A", .(merkis, fakt, pred, kluda = kluda_modelis, parm_fakt, parm_pred)], s, by = "merkis")
  x[, `:=`(rel_fakt = parm_fakt - sf, rel_pred = parm_pred - sp)][, c("sf", "sp") := NULL][]
}
shap_jomas <- function(SH, sal = SALID) {
  dom <- SH[, .(s = sum(starpiba)), by = .(merkis, grupa, domens = jomas(pazime))]
  DD <- merge(dom[grupa == "LVA A", .(merkis, domens, lva = s)], dom[grupa %in% sal, .(salid = mean(s)), by = .(merkis, domens)],
              by = c("merkis", "domens"), all = TRUE)
  for (x in c("lva", "salid")) set(DD, which(is.na(DD[[x]])), x, 0)
  DD[, rel := lva - salid][]
}
drukat_salidzinajumu <- function(saraksts) {
  M <- rbindlist(lapply(names(saraksts), function(n) metrikas(saraksts[[n]])$pa_merki[, variants := n]))
  cat("\n  Līmeņa kļūda un pārmaiņas korelācija pa mērķiem (7 valstis):\n")
  print(dcast(M, merkis ~ variants, value.var = c("abs_kluda", "parm_kor"), fun.aggregate = function(x) round(x, 2)), row.names = FALSE)
  K <- rbindlist(lapply(names(saraksts), function(n) metrikas(saraksts[[n]])$kop[, variants := n]))
  cat("\n  Visi mērķi kopā (7 valstis): pārmaiņa un relatīvā pārmaiņa (valsts − 7 valstu vidējais)\n")
  print(K[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], row.names = FALSE)
  L <- rbindlist(lapply(names(saraksts), function(n) lva_rel(saraksts[[n]])[, variants := n]))
  cat("\n  Latviešu valodas plūsma: paredzētā pārmaiņa un relatīvā pārmaiņa pret 7 valstīm\n")
  print(dcast(L, merkis ~ variants, value.var = c("parm_pred", "rel_pred"), fun.aggregate = function(x) round(x, 1)), row.names = FALSE)
  cat("  Faktiskā:\n")
  print(lva_rel(saraksts[[1]])[, .(merkis, parm_fakt = round(parm_fakt, 1), rel_fakt = round(rel_fakt, 1))], row.names = FALSE)
}
