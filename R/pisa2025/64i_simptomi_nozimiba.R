# ============================================================================
# 64i. Simptomu modelis: kuras pazīmes ir statistiski nozīmīgas (C variants)
# ============================================================================
#
# XGBoost pats p vērtības nedod, un pazīmju atlasi tas neveic: modelī paliek visas
# kopīgās pazīmes. Nozīmību šeit novērtē divos veidos, abos ar PISA pareizo dispersiju:
# 80 BRR replikas (Fay 0,5) un 10 PV (Rubina kombinēšana). Tas iespējams mērķiem
# 2018, 2022, 2025 (replikāciju svari 2015–2025).
#
#   1. PERMUTĀCIJU SVARĪGUMS mērķa ciklā (ārpus apmācības): cik pieaug vidējā kvadrātiskā
#      kļūda (ΔMSE, punkti²), ja pazīmes vērtības sajauc starp skolēniem. Pazīmei un visai
#      jomai (jomas pazīmes sajauc kopā). Visām valstīm kopā un tikai latviešu valodas
#      plūsmai (sajauc plūsmas iekšienē). H0: ΔMSE ≤ 0 (vienpusējs), Benjamini–Hochberg.
#   2. IEGULDĪJUMS PARedzētajā pārmaiņā: katras pazīmes vidējā SHAP pārmaiņa T−1 → T
#      latviešu valodas plūsmā un relatīvi pret 7 (un 5) salīdzinājuma valstu vidējo.
#      SE no replikām (neatkarīgu izlašu dispersijas summējas). H0: ieguldījums = 0,
#      divpusējs, Benjamini–Hochberg. Tā ir izlases nenoteiktība pie dota modeļa;
#      modeļa nenoteiktību (apmācības nejaušība) vērtē 64m (sēklas).
#
# Modeļi saglabāti: data/pisa2025/processed/modeli/C_<gads>.ubj
# Izvade: output/pisa2025/simptomi_C_nozimiba_svarigums.csv, simptomi_C_nozimiba_ieguldijumi.csv
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("64i_simptomi_nozimiba")
D <- sagatavot_datus(0.9, replikas = TRUE); A <- D$A; PIEEJ <- pieejamiba(D$ND_V); rm(D); gc(verbose = FALSE)
PV <- paste0("PV", 1:10, "READ")
dir.create("data/pisa2025/processed/modeli", showWarnings = FALSE, recursive = TRUE)

# ΔMSE ar BRR un PV: (y − a)² − (y − b)² = a² − b² − 2y(a − b)
imp_novert <- function(a, b, W, Y) {
  den <- colSums(W)
  E <- as.vector(crossprod(W, a^2 - b^2)) / den - 2 * crossprod(W, Y * (a - b)) / den
  thp <- E[1, ]
  vs <- mean(colSums(sweep(E[-1, , drop = FALSE], 2, thp)^2) / 20); vi <- 1.1 * var(thp)
  mse0 <- mean(as.vector(crossprod(W[, 1], (Y - b)^2)) / den[1])
  c(est = mean(thp), se = sqrt(vs + vi), mse0 = mse0)
}

IMP <- list(); IEG <- list()
for (Tg in c("2018", "2022", "2025")) {
  t0 <- Sys.time(); iepr <- iepr_gads(Tg); pz <- kopigas(PIEEJ, Tg, iepr)
  fm <- sprintf("data/pisa2025/processed/modeli/C_%s.ubj", Tg)
  m <- if (file.exists(fm)) xgb.load(fm) else { mm <- apmacit(A[gads < Tg], pz); xgb.save(mm, fm); mm }
  saveRDS(pz, sprintf("data/pisa2025/processed/modeli/C_%s_pazimes.rds", Tg))
  cat(sprintf("\n%s\n%s ← %s: %d pazīmes, %d koki (%.1f min)\n%s\n", strrep("=", 100), Tg, iepr, length(pz),
              xgb.get.num.boosted.rounds(m), as.numeric(difftime(Sys.time(), t0, units = "mins")), strrep("=", 100)))

  # 1. Permutāciju svarīgums
  te <- A[gads == Tg]; Xte <- mat(te, pz); b <- predict(m, xgb.DMatrix(Xte, missing = NA))
  W <- as.matrix(te[, c("wn", RW), with = FALSE]); Y <- as.matrix(te[, ..PV])
  la <- which(te$grupa == "LVA A")
  vien <- colnames(Xte); dom <- jomas(vien)
  kopas <- c(setNames(as.list(vien), vien), setNames(split(vien, dom), paste0("joma: ", sort(unique(dom)))))
  set.seed(as.integer(Tg))
  rez <- vector("list", length(kopas))
  for (i in seq_along(kopas)) {
    cols <- kopas[[i]]
    Xp <- Xte; Xp[, cols] <- Xte[sample(nrow(Xte)), cols, drop = FALSE]
    a <- predict(m, xgb.DMatrix(Xp, missing = NA)); rm(Xp)
    Xq <- Xte[la, , drop = FALSE]; Xq[, cols] <- Xq[sample(nrow(Xq)), cols, drop = FALSE]
    aq <- predict(m, xgb.DMatrix(Xq, missing = NA))
    v1 <- imp_novert(a, b, W, Y); v2 <- imp_novert(aq, b[la], W[la, , drop = FALSE], Y[la, , drop = FALSE])
    rez[[i]] <- data.table(merkis = Tg, vieniba = names(kopas)[i], tips = if (startsWith(names(kopas)[i], "joma: ")) "joma" else "pazīme",
                           grupa = c("visas valstis", "LVA A"), dmse = c(v1[["est"]], v2[["est"]]), se = c(v1[["se"]], v2[["se"]]),
                           mse0 = c(v1[["mse0"]], v2[["mse0"]]))
  }
  I <- rbindlist(rez)[, z := dmse / se][, p := pnorm(z, lower.tail = FALSE)]
  I[, q := p.adjust(p, "BH"), by = .(grupa, tips)]
  I[, `:=`(domens = jomas(vieniba), apraksts = apraksts(vieniba), dmse_pct = 100 * dmse / mse0)]
  IMP[[Tg]] <- I

  for (g in c("visas valstis", "LVA A")) {
    x <- I[grupa == g & tips == "pazīme"]
    cat(sprintf("\n  Permutāciju svarīgums, %s (MSE bez sajaukšanas %.0f): nozīmīgas pazīmes (BH q < 0,05): %d no %d\n",
                g, x$mse0[1], sum(x$q < 0.05), nrow(x)))
    print(head(x[order(-dmse), .(pazime = vieniba, domens, dMSE = round(dmse, 1), se = round(se, 1), `%` = round(dmse_pct, 2),
                                 z = round(z, 1), q = signif(q, 2), apraksts)], if (g == "LVA A") 15 else 25), row.names = FALSE)
  }
  cat("\n  Jomu svarīgums (visas jomas pazīmes sajauktas kopā):\n")
  print(dcast(I[tips == "joma"], vieniba ~ grupa, value.var = c("dmse", "z"), fun.aggregate = function(x) round(x, 1))[order(-`dmse_visas valstis`)], row.names = FALSE)

  # 2. Ieguldījums paredzētajā pārmaiņā (SHAP) ar BRR SE
  pr <- A[gads == iepr]; Xpr <- mat(pr, pz)
  Ste <- predict(m, xgb.DMatrix(Xte, missing = NA), predcontrib = TRUE)
  Spr <- predict(m, xgb.DMatrix(Xpr, missing = NA), predcontrib = TRUE)
  K <- setdiff(colnames(Ste), "BIAS"); Ste <- Ste[, K]; Spr <- Spr[, K]
  dk <- jomas(K); ud <- sort(unique(dk))
  Agg <- cbind(diag(length(K)), sapply(ud, function(d) as.numeric(dk == d)), 1)
  nm <- c(K, paste0("joma: ", ud), "kopā")
  vid <- function(S, d, g) {
    r <- which(d$grupa == g); if (length(r) < 30) return(NULL)
    Wg <- as.matrix(d[r, c("wn", RW), with = FALSE])
    (crossprod(Wg, S[r, , drop = FALSE]) / colSums(Wg)) %*% Agg
  }
  est <- list(); vr <- list()
  for (g in c("LVA A", SALID)) {
    e2 <- vid(Ste, te, g); e1 <- vid(Spr, pr, g); if (is.null(e1) || is.null(e2)) next
    est[[g]] <- e2[1, ] - e1[1, ]; vr[[g]] <- brr_var(e2) + brr_var(e1)
  }
  vm <- function(gs) list(e = Reduce(`+`, est[gs]) / length(gs), v = Reduce(`+`, vr[gs]) / length(gs)^2)
  S7 <- vm(intersect(SALID, names(est))); S5 <- vm(intersect(SALID5, names(est)))
  IE <- data.table(merkis = Tg, vieniba = nm, tips = c(rep("pazīme", length(K)), rep("joma", length(ud)), "kopā"),
                   lva = est[["LVA A"]], lva_se = sqrt(vr[["LVA A"]]), salid = S7$e, salid_se = sqrt(S7$v),
                   rel = est[["LVA A"]] - S7$e, rel_se = sqrt(vr[["LVA A"]] + S7$v),
                   rel5 = est[["LVA A"]] - S5$e, rel5_se = sqrt(vr[["LVA A"]] + S5$v))
  IE[, `:=`(z = rel / rel_se, z_lva = lva / lva_se)][, `:=`(p = 2 * pnorm(-abs(z)), p_lva = 2 * pnorm(-abs(z_lva)))]
  IE[, `:=`(q = p.adjust(p, "BH"), q_lva = p.adjust(p_lva, "BH")), by = tips]
  IE[, `:=`(domens = jomas(vieniba), apraksts = apraksts(vieniba))]
  IEG[[Tg]] <- IE

  cat(sprintf("\n  Paredzētā pārmaiņa %s → %s (SHAP summa): LVA A %+.1f (SE %.1f); 7 valstis %+.1f (SE %.1f); relatīvi %+.1f (SE %.1f); pret 5 valstīm %+.1f (SE %.1f)\n",
              iepr, Tg, IE[tips == "kopā", lva], IE[tips == "kopā", lva_se], IE[tips == "kopā", salid], IE[tips == "kopā", salid_se],
              IE[tips == "kopā", rel], IE[tips == "kopā", rel_se], IE[tips == "kopā", rel5], IE[tips == "kopā", rel5_se]))
  cat("\n  Pa jomām (LVA A pārmaiņa, relatīvi pret 7 valstīm, z):\n")
  print(IE[tips == "joma", .(joma = vieniba, lva = round(lva, 1), lva_se = round(lva_se, 1), rel = round(rel, 1),
                             rel_se = round(rel_se, 1), z = round(z, 1), q = signif(q, 2))][order(rel)], row.names = FALSE)
  x <- IE[tips == "pazīme"]
  cat(sprintf("\n  Pazīmes ar nozīmīgu relatīvo ieguldījumu (BH q < 0,05): %d no %d; nozīmīgu LVA A ieguldījumu: %d\n",
              sum(x$q < 0.05), nrow(x), sum(x$q_lva < 0.05)))
  print(head(x[order(p), .(pazime = vieniba, domens, lva = round(lva, 2), rel = round(rel, 2), rel_se = round(rel_se, 2),
                           z = round(z, 1), q = signif(q, 2), rel5 = round(rel5, 2), apraksts)], 20), row.names = FALSE)
  rm(te, pr, Xte, Xpr, Ste, Spr, W, Y, m); gc(verbose = FALSE)
}
fwrite(rbindlist(IMP), "output/pisa2025/simptomi_C_nozimiba_svarigums.csv")
fwrite(rbindlist(IEG), "output/pisa2025/simptomi_C_nozimiba_ieguldijumi.csv")
beigt_zurnalu()
