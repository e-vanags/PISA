# ============================================================================
# 67b. Paaudžu modelis (docs/61 C līmenis), 2. solis: latentā paaudžu trajektorija
# ============================================================================
#
# JAUTĀJUMS: vai visi pētījumi, kas mēra to pašu dzimšanas paaudzi agrākā vecumā (PIRLS,
# TIMSS 4. kl., ICCS, ICILS 8. kl.), kopā ar iepriekšējiem PISA cikliem paredz, kā mainīsies
# valsts (un latviešu valodas plūsmas) PISA rezultāts relatīvi pret citām valstīm?
#
# MODELIS (katram rādītājam atsevišķi: vidējais, zemo sniegumu daļa, P10):
#   1. Divvirzienu fiksētie efekti ar svariem 1/(SE² + 1):
#        y[k, s, c] = α[s, c] + δ[k, s] + r[k, s, c]
#      α: pētījuma cikla kopīgais līmenis (izņem kopīgās pārmaiņas un valstu kopas maiņu);
#      δ: valsts nobīde katrā pētījumā (piem., Latvija PIRLS vienmēr augstāk nekā PISA).
#      r: valsts paaudzes novirze, kas mainās laikā.
#   2. Stāvokļa telpas modelis (KFAS) katrai valstij pa dzimšanas gadiem 1980–2015:
#        r[k, s, b] = θ[k, b] + ε,  Var ε = SE² + σ²[s]
#        θ[k, b] = θ[k, b−1] + η,   Var η = q
#      q un σ²[s] kopīgi visām valstīm, maksimālā ticamība (visu valstu logLik summa).
#   3. Prognoze: valsts relatīvā PISA pārmaiņa T−1 → T = θ̂[k, b_T] − r[k, PISA, T−1],
#      centrēta pa valstīm. θ̂ ir izlīdzinātais stāvoklis no datiem, kas publicēti pirms
#      PISA T testa (marts). Tātad TIMSS 2019, ICCS 2022, ICILS 2023, PIRLS 2021 ir
#      izmantojami PISA 2025 prognozei, PISA 2025 pats nav.
#   4. Salīdzinājums: naivais (pārmaiņa 0); tas pats modelis tikai ar PISA datiem;
#      pilnais modelis ar IEA. Starpība starp pēdējiem diviem = IEA pievienotā informācija.
# VIENĪBAS: A kopa = 8 valstis (Latvija kopā); B kopa = Latvijas vietā latviešu valodas
#   plūsma (PISA plūsma un IEA "LVA A").
# NĀKOTNE: prognoze PISA 2029 (dzimuši 2013) no visiem datiem līdz PISA 2025.
#
# Izvade: output/pisa2025/paaudzu_modelis_parbaude.csv, paaudzu_modelis_2029.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(KFAS) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/67b_paaudzu_modelis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

D <- fread("data/pisa2025/processed/paaudzu_panelis.csv")
D[, b := floor(dz_gads + 0.5)][, serija := fifelse(petijums == "PISA lasīšana", "PISA", petijums)]
D <- D[is.finite(est) & is.finite(se)]
VAL8 <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
KOPAS <- list(`A: valstis` = VAL8, `B: LVA A` = c(setdiff(VAL8, "LVA"), "LVA A"))
SER <- c("PISA", "PIRLS lasīšana", "TIMSS4 matemātika", "TIMSS4 dabaszinātnes", "ICCS pilsoniskās zināšanas", "ICILS datorpratība")
PISA_GADI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025, 2029)
MERKI <- c(2006, 2009, 2012, 2015, 2018, 2022, 2025)
YRS <- 1980:2015
bdz <- function(T) floor(T - 15.8 + 0.5)

fe_resid <- function(X) {
  X <- copy(X)[, `:=`(sc = paste(serija, gads), ks = paste(grupa, serija))]
  f <- lm(est ~ 0 + factor(sc) + factor(ks), data = X, weights = 1 / (se^2 + 1))
  X[, r := est - fitted(f)]
}
buvet <- function(Xk, ser, q, s2) {
  p <- length(ser); Y <- matrix(NA_real_, length(YRS), p); H <- array(0, c(p, p, length(YRS)))
  for (j in 1:p) for (t in seq_along(YRS)) H[j, j, t] <- 1
  if (nrow(Xk)) {
    a <- Xk[, .(r = mean(r), se2 = mean(se^2)), by = .(b, serija)]
    for (i in seq_len(nrow(a))) { t <- match(a$b[i], YRS); j <- match(a$serija[i], ser)
      if (!is.na(t) && !is.na(j)) { Y[t, j] <- a$r[i]; H[j, j, t] <- a$se2[i] + s2[j] } }
  }
  SSModel(Y ~ -1 + SSMcustom(Z = matrix(1, p, 1), T = matrix(1), R = matrix(1), Q = matrix(q), a1 = matrix(0), P1inf = matrix(1)), H = H)
}
novertet <- function(X, vien) {
  ser <- intersect(SER, unique(X$serija))
  nll <- function(par) {
    q <- exp(par[1]); s2 <- exp(par[-1])
    -sum(sapply(vien, function(k) { ll <- tryCatch(logLik(buvet(X[grupa == k], ser, q, s2)), error = function(e) -1e10); if (is.finite(ll)) ll else -1e10 }))
  }
  o <- optim(c(log(4), rep(log(9), length(ser))), nll, method = "Nelder-Mead", control = list(maxit = 3000, reltol = 1e-8))
  list(ser = ser, q = exp(o$par[1]), s2 = setNames(exp(o$par[-1]), ser), ll = -o$value)
}
prognoze <- function(X, vien, h, T) {
  Tp <- max(PISA_GADI[PISA_GADI < T]); iT <- match(bdz(T), YRS)
  rbindlist(lapply(vien, function(k) {
    ks <- KFS(buvet(X[grupa == k], h$ser, h$q, h$s2), smoothing = "state")
    rp <- X[grupa == k & serija == "PISA" & gads == Tp, r]
    data.table(grupa = k, th = ks$alphahat[iT, 1], th_se = sqrt(ks$V[1, 1, iT]), r_iepr = if (length(rp)) rp[1] else NA_real_)
  }))[, prog := th - r_iepr]
}
faktiska <- function(rad, T, vien) {
  Tp <- max(PISA_GADI[PISA_GADI < T])
  x <- merge(D[serija == "PISA" & radītājs == rad & gads == T & grupa %in% vien, .(grupa, y2 = est)],
             D[serija == "PISA" & radītājs == rad & gads == Tp & grupa %in% vien, .(grupa, y1 = est)], by = "grupa")
  x[, .(grupa, fakt = (y2 - mean(y2)) - (y1 - mean(y1)), fakt_abs = y2 - y1)]
}

REZ <- list(); HIP <- list(); t0 <- Sys.time()
for (rad in c("vid", "zems", "p10")) for (kn in names(KOPAS)) for (T in MERKI) for (iea in c(TRUE, FALSE)) {
  vien <- KOPAS[[kn]]
  X <- D[radītājs == rad & grupa %in% vien & publ < sprintf("%d-03", T)]
  if (!iea) X <- X[serija == "PISA"]
  X <- fe_resid(X)
  h <- novertet(X, vien)
  pr <- prognoze(X, vien, h, T)
  fk <- faktiska(rad, T, vien)
  x <- merge(pr, fk, by = "grupa")[!is.na(prog)]
  x[, prog_rel := prog - mean(prog)]
  REZ[[paste(rad, kn, T, iea)]] <- x[, `:=`(radītājs = rad, kopa = kn, merkis = T, modelis = if (iea) "PISA + IEA" else "tikai PISA")]
  HIP[[paste(rad, kn, T, iea)]] <- data.table(radītājs = rad, kopa = kn, merkis = T, modelis = if (iea) "PISA + IEA" else "tikai PISA",
                                              q = h$q, t(h$s2), n_obs = nrow(X))
}
cat(sprintf("Novērtēšana: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
R <- rbindlist(REZ, fill = TRUE); H <- rbindlist(HIP, fill = TRUE)
fwrite(R, "output/pisa2025/paaudzu_modelis_parbaude.csv")
cat("\nHiperparametri (vidējais pa mērķiem): q = stāvokļa dispersija gadā; σ² = pētījuma papildu troksnis\n")
print(H[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 1)), by = .(radītājs, kopa, modelis), .SDcols = setdiff(names(H), c("radītājs", "kopa", "merkis", "modelis"))], row.names = FALSE)

cat("\n", strrep("=", 100), "\nEx ante pārbaude: valsts relatīvā PISA pārmaiņa (visi mērķi 2006–2025)\n", strrep("=", 100), "\n", sep = "")
M <- R[, .(pari = .N, mae_modelis = mean(abs(fakt - prog_rel)), mae_naivais = mean(abs(fakt)),
           kor = cor(fakt, prog_rel), virziens = mean(sign(fakt) == sign(prog_rel))), by = .(radītājs, kopa, modelis)]
print(M[order(radītājs, kopa, modelis), lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], row.names = FALSE)
cat("\n  Pa mērķiem (vidējais, A kopa): MAE modelis / naivais\n")
print(dcast(R[radītājs == "vid" & kopa == "A: valstis", .(mae = round(mean(abs(fakt - prog_rel)), 1), naiv = round(mean(abs(fakt)), 1)), by = .(merkis, modelis)],
            merkis ~ modelis, value.var = c("mae", "naiv")), row.names = FALSE)

cat("\n", strrep("=", 100), "\nLatvija un latviešu valodas plūsma: relatīvā pārmaiņa, faktiskā un prognozētā\n", strrep("=", 100), "\n", sep = "")
L <- R[grupa %in% c("LVA", "LVA A")]
print(dcast(L, radītājs + grupa + merkis + round(fakt, 1) ~ modelis, value.var = "prog_rel", fun.aggregate = function(v) round(v[1], 1)), row.names = FALSE)
cat("\n  Virziens trāpīts (Latvija, visi rādītāji un mērķi):\n")
print(L[, .(pari = .N, virziens = sum(sign(fakt) == sign(prog_rel)), mae = round(mean(abs(fakt - prog_rel)), 1), naiv = round(mean(abs(fakt)), 1)), by = .(grupa, modelis)], row.names = FALSE)

cat("\n", strrep("=", 100), "\nNākotne: PISA 2025 → 2029 (dzimuši 2013), visi dati līdz PISA 2025\n", strrep("=", 100), "\n", sep = "")
FUT <- list()
for (rad in c("vid", "zems", "p10")) for (kn in names(KOPAS)) {
  vien <- KOPAS[[kn]]; X <- fe_resid(D[radītājs == rad & grupa %in% vien]); h <- novertet(X, vien)
  pr <- prognoze(X, vien, h, 2029)[, prog_rel := prog - mean(prog, na.rm = TRUE)]
  FUT[[paste(rad, kn)]] <- pr[, `:=`(radītājs = rad, kopa = kn)]
}
FUT <- rbindlist(FUT); fwrite(FUT, "output/pisa2025/paaudzu_modelis_2029.csv")
print(FUT[grupa %in% c("LVA", "LVA A", "EST", "LTU"), .(radītājs, kopa, grupa, prog_rel = round(prog_rel, 1), th_se = round(th_se, 1))], row.names = FALSE)
cat("  prog_rel: prognozētā relatīvā pārmaiņa 2025 → 2029 (vidējam punkti; zemo daļai procentpunkti, + = vairāk zemo)\n")
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
