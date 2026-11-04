# ============================================================================
# 67c. Paaudžu modelis: kurš pētījums palīdz un kurš traucē (izslēgšanas pārbaude)
# ============================================================================
#
# 67b rezultāts: visu IEA pētījumu pievienošana PISA paaudžu modelim valstu relatīvās
# pārmaiņas prognozi pasliktina (MAE 7,9 pret 6,7 tikai PISA, 7,0 naivais), bet Latvijai
# uzlabo. Šeit pētījumus pievieno pa vienam, lai redzētu, kurš nes signālu un kurš
# troksni. Modelis un ex ante noteikums tieši kā 67b (funkcijas atkārtotas).
#
# Izvade: output/pisa2025/paaudzu_modelis_petijumi.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(KFAS) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/67c_paaudzu_modelis_petijumi_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

D <- fread("data/pisa2025/processed/paaudzu_panelis.csv")
D[, b := floor(dz_gads + 0.5)][, serija := fifelse(petijums == "PISA lasīšana", "PISA", petijums)]
D <- D[is.finite(est) & is.finite(se)]
VAL8 <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
KOPAS <- list(`A: valstis` = VAL8, `B: LVA A` = c(setdiff(VAL8, "LVA"), "LVA A"))
SER <- c("PISA", "PIRLS lasīšana", "TIMSS4 matemātika", "TIMSS4 dabaszinātnes", "ICCS pilsoniskās zināšanas", "ICILS datorpratība")
VARI <- list(`tikai PISA` = "PISA", `PISA + PIRLS` = c("PISA", "PIRLS lasīšana"),
             `PISA + TIMSS` = c("PISA", "TIMSS4 matemātika", "TIMSS4 dabaszinātnes"),
             `PISA + ICCS` = c("PISA", "ICCS pilsoniskās zināšanas"),
             `PISA + PIRLS + ICCS` = c("PISA", "PIRLS lasīšana", "ICCS pilsoniskās zināšanas"), visi = SER)
PISA_GADI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025, 2029)
MERKI <- c(2006, 2009, 2012, 2015, 2018, 2022, 2025)
YRS <- 1980:2015
bdz <- function(T) floor(T - 15.8 + 0.5)
fe_resid <- function(X) {
  X <- copy(X)[, `:=`(sc = paste(serija, gads), ks = paste(grupa, serija))]
  f <- lm(est ~ 0 + factor(sc) + factor(ks), data = X, weights = 1 / (se^2 + 1)); X[, r := est - fitted(f)]
}
buvet <- function(Xk, ser, q, s2) {
  p <- length(ser); Y <- matrix(NA_real_, length(YRS), p); H <- array(0, c(p, p, length(YRS)))
  for (j in 1:p) for (t in seq_along(YRS)) H[j, j, t] <- 1
  if (nrow(Xk)) { a <- Xk[, .(r = mean(r), se2 = mean(se^2)), by = .(b, serija)]
    for (i in seq_len(nrow(a))) { t <- match(a$b[i], YRS); j <- match(a$serija[i], ser)
      if (!is.na(t) && !is.na(j)) { Y[t, j] <- a$r[i]; H[j, j, t] <- a$se2[i] + s2[j] } } }
  SSModel(Y ~ -1 + SSMcustom(Z = matrix(1, p, 1), T = matrix(1), R = matrix(1), Q = matrix(q), a1 = matrix(0), P1inf = matrix(1)), H = H)
}
novertet <- function(X, vien) {
  ser <- intersect(SER, unique(X$serija))
  nll <- function(par) { q <- exp(par[1]); s2 <- exp(par[-1])
    -sum(sapply(vien, function(k) { ll <- tryCatch(logLik(buvet(X[grupa == k], ser, q, s2)), error = function(e) -1e10); if (is.finite(ll)) ll else -1e10 })) }
  o <- optim(c(log(4), rep(log(9), length(ser))), nll, method = "Nelder-Mead", control = list(maxit = 3000, reltol = 1e-8))
  list(ser = ser, q = exp(o$par[1]), s2 = setNames(exp(o$par[-1]), ser))
}
prognoze <- function(X, vien, h, T) {
  Tp <- max(PISA_GADI[PISA_GADI < T]); iT <- match(bdz(T), YRS)
  rbindlist(lapply(vien, function(k) { ks <- KFS(buvet(X[grupa == k], h$ser, h$q, h$s2), smoothing = "state")
    rp <- X[grupa == k & serija == "PISA" & gads == Tp, r]
    data.table(grupa = k, th = ks$alphahat[iT, 1], r_iepr = if (length(rp)) rp[1] else NA_real_) }))[, prog := th - r_iepr]
}
faktiska <- function(rad, T, vien) {
  Tp <- max(PISA_GADI[PISA_GADI < T])
  x <- merge(D[serija == "PISA" & radītājs == rad & gads == T & grupa %in% vien, .(grupa, y2 = est)],
             D[serija == "PISA" & radītājs == rad & gads == Tp & grupa %in% vien, .(grupa, y1 = est)], by = "grupa")
  x[, .(grupa, fakt = (y2 - mean(y2)) - (y1 - mean(y1)))]
}

REZ <- list(); t0 <- Sys.time()
for (rad in c("vid", "zems")) for (kn in names(KOPAS)) for (vn in names(VARI)) for (T in MERKI) {
  vien <- KOPAS[[kn]]
  X <- fe_resid(D[radītājs == rad & grupa %in% vien & serija %in% VARI[[vn]] & publ < sprintf("%d-03", T)])
  h <- novertet(X, vien)
  x <- merge(prognoze(X, vien, h, T), faktiska(rad, T, vien), by = "grupa")[!is.na(prog)][, prog_rel := prog - mean(prog)]
  REZ[[paste(rad, kn, vn, T)]] <- x[, `:=`(radītājs = rad, kopa = kn, variants = vn, merkis = T, q = h$q, s2_pisa = h$s2[["PISA"]])]
}
cat(sprintf("Novērtēšana: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
R <- rbindlist(REZ); fwrite(R, "output/pisa2025/paaudzu_modelis_petijumi.csv")
R[, variants := factor(variants, levels = names(VARI))]

cat("\n", strrep("=", 100), "\nVisas valstis, visi mērķi 2006–2025: relatīvās PISA pārmaiņas prognoze\n", strrep("=", 100), "\n", sep = "")
print(R[, .(pari = .N, mae = round(mean(abs(fakt - prog_rel)), 2), naivais = round(mean(abs(fakt)), 2), kor = round(cor(fakt, prog_rel), 2),
            virziens = round(mean(sign(fakt) == sign(prog_rel)), 2), s2_pisa = round(mean(s2_pisa), 1)), by = .(radītājs, kopa, variants)][order(radītājs, kopa, variants)], row.names = FALSE)
cat("\n  Tikai mērķi, kuriem ir IEA informācija par to pašu paaudzi vai tās tuvumu (2012, 2022, 2025):\n")
print(R[merkis %in% c(2012, 2022, 2025), .(pari = .N, mae = round(mean(abs(fakt - prog_rel)), 2), naivais = round(mean(abs(fakt)), 2),
                                           kor = round(cor(fakt, prog_rel), 2)), by = .(radītājs, kopa, variants)][order(radītājs, kopa, variants)], row.names = FALSE)
cat("\n", strrep("=", 100), "\nLatvija un latviešu valodas plūsma\n", strrep("=", 100), "\n", sep = "")
L <- R[grupa %in% c("LVA", "LVA A")]
print(L[, .(pari = .N, virziens = sum(sign(fakt) == sign(prog_rel)), mae = round(mean(abs(fakt - prog_rel)), 1), naivais = round(mean(abs(fakt)), 1)),
        by = .(radītājs, grupa, variants)][order(radītājs, grupa, variants)], row.names = FALSE)
cat("\n  2025. gada prognoze (faktiskā → prognozētā):\n")
print(dcast(L[merkis == 2025], radītājs + grupa + round(fakt, 1) ~ variants, value.var = "prog_rel", fun.aggregate = function(v) round(v[1], 1)), row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
