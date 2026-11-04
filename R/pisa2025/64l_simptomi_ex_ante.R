# ============================================================================
# 64l. Simptomu modelis, 5. solis: ex ante prognoze no simptomu tendencēm
# ============================================================================
#
# JAUTĀJUMS: ja pirms cikla T būtu zināmi tikai simptomi līdz ciklam T−1 (un to
# tendence T−2 → T−1), vai karte "simptomi → prasme" paredzētu T rezultāta pārmaiņu?
#
# DIZAINS katram mērķim T (2006–2025):
#   * pazīmes: uzdotas gan T−1, gan T−2 visās valstīs (T cikla anketa nav vajadzīga);
#   * karte f: C variants (neatbildēts = −999), apmācīts uz cikliem < T;
#   * scenārijs "nemainās": X_T = X_{T−1}  → prognozētā pārmaiņa 0 (tas ir naivais);
#   * scenārijs "sava tendence": katrai grupai (valsts; Latvijā plūsma) un pazīmei
#     X_T = X_{T−1} + δ, kur δ = (grupas vidējais T−1 − T−2) × (T − (T−1)) / ((T−1) − (T−2));
#     vērtības ierobežotas ar apmācības diapazonu;
#   * scenārijs "kopīgā tendence": δ = 7 valstu vidējā tendence (visām grupām viena);
#   * prognozētā pārmaiņa = vid f(X̂_T) − vid f(X_{T−1}); līmenis = faktiskais T−1 + pārmaiņa;
#   * salīdzinājumam: rezultāta drifts (faktiskā pārmaiņa T−2 → T−1, pārskalota).
# SHAP: prognozētās pārmaiņas sadalījums pa pazīmēm un jomām (sava tendence).
#
# Izvade: output/pisa2025/simptomi_ex_ante_prognozes.csv, simptomi_ex_ante_shap.csv,
#         simptomi_ex_ante_tendences.csv
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("64l_simptomi_ex_ante")
D <- sagatavot_datus(0.9); A <- D$A; PIEEJ <- pieejamiba(D$ND_V); rm(D); gc(verbose = FALSE)

gm <- function(d, k) {
  x <- d[[k]]; ok <- !is.na(x) & x != -999
  tapply(x[ok] * d$wn[ok], d$grupa[ok], sum) / tapply(d$wn[ok], d$grupa[ok], sum)
}
rez <- list(); SHL <- list(); TEND <- list()
for (Tg in MERKI) {
  t0 <- Sys.time(); g1 <- iepr_gads(Tg, 1); g2 <- iepr_gads(Tg, 2)
  s <- (as.numeric(Tg) - as.numeric(g1)) / (as.numeric(g1) - as.numeric(g2))
  pz <- kopigas(PIEEJ, g1, g2)
  m <- apmacit(A[gads < Tg], pz)
  d1 <- A[gads == g1]; d2 <- A[gads == g2]; X1 <- mat(d1, pz); gr <- d1$grupa
  X1s <- X1; X1c <- X1; idx <- which(A$gads < Tg)
  for (k in pz) {
    m1 <- gm(d1, k); m2 <- gm(d2, k)
    dl <- (m1 - m2[names(m1)]) * s; dl[is.na(dl)] <- 0
    kd <- mean(dl[intersect(names(dl), SALID)])
    x <- A[[k]][idx]; ok <- !is.na(x) & x != -999; lo <- min(x[ok]); hi <- max(x[ok])
    v <- which(!is.na(X1[, k]) & X1[, k] != -999)
    dd <- dl[gr[v]]; dd[is.na(dd)] <- 0
    X1s[v, k] <- pmin(pmax(X1[v, k] + dd, lo), hi)
    X1c[v, k] <- pmin(pmax(X1[v, k] + kd, lo), hi)
    TEND[[paste(Tg, k)]] <- data.table(merkis = Tg, pazime = k, grupa = names(dl), delta = as.numeric(dl))
  }
  p0 <- predict(m, xgb.DMatrix(X1, missing = NA)); p1 <- predict(m, xgb.DMatrix(X1s, missing = NA))
  pc <- predict(m, xgb.DMatrix(X1c, missing = NA))
  d1[, `:=`(p0 = p0, p1 = p1, pc = pc)]
  kop <- Reduce(function(a, b) merge(a, b, by = "grupa", all.x = TRUE), list(
    A[gads == Tg, .(fakt = wm(PVREAD, wn)), by = grupa],
    d1[, .(fakt_iepr = wm(PVREAD, wn), prog_parm = wm(p1, wn) - wm(p0, wn), prog_parm_kop = wm(pc, wn) - wm(p0, wn)), by = grupa],
    d2[, .(fakt_iepr2 = wm(PVREAD, wn)), by = grupa]))
  kop[, `:=`(merkis = Tg, iepr = g1, pazimes = length(pz), parm_fakt = fakt - fakt_iepr, drift = (fakt_iepr - fakt_iepr2) * s)]
  rez[[Tg]] <- kop
  S0 <- predict(m, xgb.DMatrix(X1, missing = NA), predcontrib = TRUE)
  S1 <- predict(m, xgb.DMatrix(X1s, missing = NA), predcontrib = TRUE)
  G <- rowsum((S1 - S0) * d1$wn, d1$grupa) / as.vector(rowsum(d1$wn, d1$grupa))
  SHL[[Tg]] <- data.table(merkis = Tg, grupa = rep(rownames(G), ncol(G)), pazime = rep(colnames(G), each = nrow(G)), starpiba = as.vector(G))
  cat(sprintf("\n%s (simptomi %s un %s, %d pazīmes, %.1f min)\n", Tg, g2, g1, length(pz), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  print(kop[order(grupa), .(grupa, parm_fakt = round(parm_fakt, 1), sava_tend = round(prog_parm, 1), kopiga_tend = round(prog_parm_kop, 1),
                            drifts = round(drift, 1))], row.names = FALSE)
  rm(m, d1, d2, X1, X1s, X1c, S0, S1); gc(verbose = FALSE)
}
R <- rbindlist(rez); SH <- rbindlist(SHL)
fwrite(R, "output/pisa2025/simptomi_ex_ante_prognozes.csv"); fwrite(SH, "output/pisa2025/simptomi_ex_ante_shap.csv")
fwrite(rbindlist(TEND), "output/pisa2025/simptomi_ex_ante_tendences.csv")

cat("\n", strrep("=", 100), "\nNovērtējums: 7 valstis, visi mērķi\n", strrep("=", 100), "\n", sep = "")
v <- R[grupa %in% SALID & !is.na(parm_fakt)]
nov <- function(x, nos) v[!is.na(get(x)), .(prognoze = nos, pari = .N, mae = mean(abs(parm_fakt - get(x))), kor = cor(parm_fakt, get(x)),
                                             virziens = mean(sign(parm_fakt) == sign(get(x))))]
v[, nulle := 0]
print(rbind(nov("nulle", "nemainās (naivais)"), nov("drift", "rezultāta drifts"), nov("prog_parm", "simptomi: sava tendence"),
            nov("prog_parm_kop", "simptomi: kopīgā tendence"))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], row.names = FALSE)
cat("\n  Pa mērķiem (MAE):\n")
print(v[, .(nulle = round(mean(abs(parm_fakt)), 1), drifts = round(mean(abs(parm_fakt - drift), na.rm = TRUE), 1),
            sava = round(mean(abs(parm_fakt - prog_parm)), 1), kopiga = round(mean(abs(parm_fakt - prog_parm_kop)), 1),
            vid_fakt = round(mean(parm_fakt), 1), vid_sava = round(mean(prog_parm), 1), vid_kopiga = round(mean(prog_parm_kop), 1)), by = merkis], row.names = FALSE)

cat("\n  Latviešu valodas plūsma (relatīvi = mīnus 7 valstu vidējais):\n")
sv <- R[grupa %in% SALID, .(sf = mean(parm_fakt, na.rm = TRUE), sp = mean(prog_parm), sd = mean(drift, na.rm = TRUE)), by = merkis]
L <- merge(R[grupa == "LVA A"], sv, by = "merkis")
print(L[, .(merkis, parm_fakt = round(parm_fakt, 1), sava_tend = round(prog_parm, 1), drifts = round(drift, 1),
            rel_fakt = round(parm_fakt - sf, 1), rel_simpt = round(prog_parm - sp, 1), rel_drifts = round(drift - sd, 1))], row.names = FALSE)
cat(sprintf("  Relatīvās pārmaiņas virziens trāpīts: simptomi %d/%d, drifts %d/%d\n",
            L[, sum(sign(parm_fakt - sf) == sign(prog_parm - sp))], nrow(L), L[!is.na(drift), sum(sign(parm_fakt - sf) == sign(drift - sd))], L[!is.na(drift), .N]))

cat("\n  Prognozētās pārmaiņas SHAP pa jomām, latviešu valodas plūsma relatīvi pret 7 valstīm:\n")
print(dcast(shap_jomas(SH), domens ~ merkis, value.var = "rel", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
for (Tg in c("2022", "2025")) {
  f <- merge(SH[merkis == Tg & grupa == "LVA A", .(pazime, lva = starpiba)], SH[merkis == Tg & grupa %in% SALID, .(salid = mean(starpiba)), by = pazime], by = "pazime")
  f[, `:=`(rel = lva - salid, domens = jomas(pazime), apraksts = apraksts(pazime))]
  cat(sprintf("\n  %s ex ante: 12 pazīmes ar lielāko |relatīvo| ieguldījumu\n", Tg))
  print(head(f[order(-abs(rel)), .(pazime, domens, lva = round(lva, 2), salid = round(salid, 2), rel = round(rel, 2), apraksts)], 12), row.names = FALSE)
}
beigt_zurnalu()
