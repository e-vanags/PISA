# ============================================================================
# 88. Sociālais fons garajā ķēdē: starpposmi pa ceturtdaļām, mediācija pa
#     cikliem un krituma dekompozīcija (Slānis 18, 2. kārta, docs/91)
# PISA datu analīze 2025
# ============================================================================
#
# HIPOTĒZE (H28): sociālais fons kritumā piedalās ne tieši, bet caur
#   starpposmiem (grāmatas mājās, lasīšanas paradumi, ierīces), un šī
#   netiešā daļa laikā mainās.
# TRĪS DAĻAS
#   1. Atvienošanās: starpposmu līmenis pa ESCS ceturtdaļām katrā ciklā.
#      Ja grāmatas un lasītprieks zūd visās ceturtdaļās, tā ir vispārēja
#      pārmaiņa; ja tikai zemākajās, fons ir ķēdē.
#   2. Ķēde: katrā ciklā fona kopējā saistība ar lasīšanu sadalīta tiešajā
#      un netiešajā daļā caur grāmatām (visi cikli), lasītprieku (2009, 2018)
#      un laiku tiešsaistē (2012 līdz 2018). Lineāri ceļu modeļi ar svariem.
#   3. Dekompozīcija (Oaxaca un Blinder, apvienotie koeficienti): cik punktu
#      no lasīšanas pārmaiņas 2003 -> 2018, 2009 -> 2018, 2018 -> 2025 atbilst
#      fona un starpposmu sadalījuma pārmaiņai (sastāvs) un cik tam, ka pie
#      tāda paša fona lasa citādi (koeficienti).
# DATI: simptomi_apvienots.rds; ESCS no 2003; lasītprieks un lasīšanas laiks
#   tikai 2000, 2009, 2018; datoru skaits 2006 līdz 2018; minūtes tiešsaistē
#   2012 līdz 2018; 2022 un 2025 ierīču datu šajā failā nav.
# KĻŪDAS: skolu klasteru sāknēšana (PV1) plus PV imputācijas dispersija.
# IZVADE: output/pisa2025/escs_starpposmi_ceturtdalas.csv, escs_kede.csv,
#   escs_dekompozicija.csv, plots/88_*.png, logs/88_escs_kede_output.txt
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
if (!file.exists("R/pisa2025") && file.exists("../../R/pisa2025")) setwd("../..")
output_file <- "output/pisa2025/logs/88_escs_kede_output.txt"
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākts:", format(Sys.time()), "\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
set.seed(88)
B_REP <- 60
SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
NOS <- c(LVA = "Latvija, visi", `LVA A` = "Latvija, latviešu valodas plūsma", `LVA B` = "Latvija, krievu valodas plūsma",
         EST = "Igaunija", LTU = "Lietuva", POL = "Polija", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", DEU = "Vācija", `7 valstis` = "7 valstu vidējais")

# === 1. DATI =================================================================
cat(paste(rep("=", 70), collapse = ""), "\n1. Dati\n", paste(rep("=", 70), collapse = ""), "\n\n")
X <- as.data.table(readRDS("data/pisa2025/processed/simptomi_apvienots.rds"))
PV <- grep("^PV[0-9]+READ$", names(X), value = TRUE)
keep <- c("gads", "cnt", "skola", "w", "plusma", PV, "AUTO_ESCS", "AUTO_HISEI", "L_GRAMATAS_MAJAS", "L_IDX_LASITPRIEKS", "L_LAIKS_PRIEKAM",
          "D_INTERNETS_MAJAS", "D_DATORI_SKAITS", "D_INTERNETS_DARBDIENA")
X <- X[, intersect(keep, names(X)), with = FALSE]
for (v in names(X)) if (inherits(X[[v]], "haven_labelled")) X[[v]] <- unclass(X[[v]])
X <- X[!is.na(w) & w > 0]; X[, gads := as.integer(gads)]
setnames(X, c("AUTO_ESCS", "AUTO_HISEI", "L_GRAMATAS_MAJAS", "L_IDX_LASITPRIEKS", "L_LAIKS_PRIEKAM", "D_INTERNETS_MAJAS", "D_DATORI_SKAITS", "D_INTERNETS_DARBDIENA"),
         c("escs", "hisei", "gramatas", "prieks", "laiks", "internets", "datori", "minutes"))
for (v in c("escs", "hisei", "gramatas", "prieks", "laiks", "internets", "datori", "minutes")) X[!is.na(get(v)) & get(v) >= 900, (v) := NA_real_]
X[!is.na(gramatas) & (gramatas < 1 | gramatas > 6), gramatas := NA_real_]
X[, `:=`(gram_virs100 = as.numeric(gramatas >= 4), nelasa = as.numeric(laiks == 1), inet = as.numeric(internets == 1), datori2 = as.numeric(datori >= 3))]
# laiks tiešsaistē darbdienā ārpus skolas: kategorijas 1 nemaz, 2 līdz 30 min, 3 31 līdz 60 min, 4 1 līdz 2 h, 5 2 līdz 4 h, 6 4 līdz 6 h, 7 vairāk nekā 6 h -> aptuvenas stundas
X[!is.na(minutes) & (minutes < 1 | minutes > 7), minutes := NA_real_]
X[, virs4h := as.numeric(minutes >= 6)]
X[, minutes := c(0, 0.25, 0.75, 1.5, 3, 5, 7)[minutes]]
X[, grupa := cnt]
LA <- X[cnt == "LVA" & plusma == "A"][, grupa := "LVA A"]; LB <- X[cnt == "LVA" & plusma == "B"][, grupa := "LVA B"]
X <- rbind(X, LA, LB); X[, skola := paste(cnt, gads, skola)]
cat("Pieejamība (Latvija, % nav NA) pa cikliem:\n")
print(X[cnt == "LVA", lapply(.SD, function(z) round(100 * mean(!is.na(z)))), by = gads, .SDcols = c("escs", "gramatas", "prieks", "laiks", "inet", "datori2", "minutes")][order(gads)])

wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; cw <- cumsum(w) / sum(w); x[which(cw >= p)[1]] }
wmean <- function(x, w) { ok <- !is.na(x); sum(x[ok] * w[ok]) / sum(w[ok]) }
wlm <- function(y, Xm, w) { sw <- sqrt(w); b <- tryCatch(solve(crossprod(Xm * sw), crossprod(Xm * sw, y * sw)), error = function(e) matrix(NA_real_, ncol(Xm), 1)); as.vector(b) }
ceturtdala <- function(e, w) { q <- c(wq(e, w, .25), wq(e, w, .50), wq(e, w, .75)); findInterval(e, q) + 1L }

# === 2. ATVIENOŠANĀS: STARPPOSMI PA ESCS CETURTDAĻĀM ==========================
cat("\n", paste(rep("=", 70), collapse = ""), "\n2. Starpposmi pa ESCS ceturtdaļām\n", paste(rep("=", 70), collapse = ""), "\n\n")
MED <- c("gram_virs100", "gramatas", "prieks", "nelasa", "inet", "datori2", "minutes", "virs4h")
D2 <- X[!is.na(escs) & gads >= 2003]
D2[, cet := ceturtdala(escs, w), by = .(grupa, gads)]
S2 <- D2[, c(list(n = .N), lapply(.SD, function(z) if (all(is.na(z))) NA_real_ else wmean(z, w))), by = .(grupa, gads, cet), .SDcols = MED]
S2[, gram_virs100 := 100 * gram_virs100]; S2[, nelasa := 100 * nelasa]; S2[, inet := 100 * inet]; S2[, datori2 := 100 * datori2]; S2[, virs4h := 100 * virs4h]
V7 <- S2[grupa %in% SAL, c(list(n = sum(n)), lapply(.SD, mean, na.rm = TRUE)), by = .(gads, cet), .SDcols = MED][, grupa := "7 valstis"]
S2 <- rbind(S2, V7[, names(S2), with = FALSE])
fwrite(S2[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], "output/pisa2025/escs_starpposmi_ceturtdalas.csv")
tab2 <- function(var, g, dig = 1) { d <- dcast(S2[grupa == g & !is.na(get(var))], cet ~ gads, value.var = var); for (v in setdiff(names(d), "cet")) d[[v]] <- round(d[[v]], dig); d }
for (g in c("LVA A", "7 valstis")) {
  cat("\n---", NOS[g], "---\n")
  cat("Vairāk nekā 100 grāmatu mājās, % pa ESCS ceturtdaļām (1 zemākā, 4 augstākā):\n"); print(tab2("gram_virs100", g), row.names = FALSE)
  cat("Lasītprieka indekss pa ceturtdaļām:\n"); print(tab2("prieks", g, 2), row.names = FALSE)
  cat("Nelasa priekam, %:\n"); print(tab2("nelasa", g), row.names = FALSE)
  cat("Divi vai vairāk datori mājās, %:\n"); print(tab2("datori2", g), row.names = FALSE)
  cat("Stundas tiešsaistē darbdienā ārpus skolas (aptuveni, no kategorijām):\n"); print(tab2("minutes", g, 2), row.names = FALSE)
  cat("Vairāk nekā 4 stundas tiešsaistē darbdienā, %:\n"); print(tab2("virs4h", g), row.names = FALSE)
}
# plaisa starp ceturtdaļām grāmatās: Q4 mīnus Q1
cat("\nGrāmatu (> 100) plaisa Q4 mīnus Q1, pp:\n")
PL <- dcast(S2[cet %in% c(1, 4) & grupa %in% c("LVA A", "LVA", "7 valstis", SAL)], grupa + gads ~ cet, value.var = "gram_virs100")[, plaisa := `4` - `1`]
print(dcast(PL, grupa ~ gads, value.var = "plaisa")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)], row.names = FALSE)

# === 3. ĶĒDE: TIEŠĀ UN NETIEŠĀ DAĻA PA CIKLIEM ===============================
cat("\n", paste(rep("=", 70), collapse = ""), "\n3. Ķēde: fons -> grāmatas -> lasītprieks (-> laiks tiešsaistē) -> lasīšana\n", paste(rep("=", 70), collapse = ""), "\n\n")
# Ceļu modelis vienam PV; d satur y, escs, gramatas, prieks (var būt NA visi), minutes (var būt NA visi), w, skola
kede_fun <- function(d, ar_prieku, ar_min) {
  e <- d$escs; g <- d$gramatas; w <- d$w; y <- d$y; out <- c()
  tau <- wlm(y, cbind(1, e), w)[2]; out["kopejais"] <- tau
  a <- wlm(g, cbind(1, e), w)[2]
  if (!ar_prieku && !ar_min) { b3 <- wlm(y, cbind(1, e, g), w); out["tiesais"] <- b3[2]; out["caur_gramatam"] <- a * b3[3]; out["caur_prieku"] <- NA; out["caur_tiessaisti"] <- NA }
  else if (ar_prieku && !ar_min) { p <- d$prieks; bp <- wlm(p, cbind(1, e, g), w); b3 <- wlm(y, cbind(1, e, g, p), w)
    out["tiesais"] <- b3[2]; out["caur_gramatam"] <- a * b3[3] + a * bp[3] * b3[4]; out["caur_prieku"] <- bp[2] * b3[4]; out["caur_tiessaisti"] <- NA }
  else if (!ar_prieku && ar_min) { m <- d$minutes; bm <- wlm(m, cbind(1, e, g), w); b3 <- wlm(y, cbind(1, e, g, m), w)
    out["tiesais"] <- b3[2]; out["caur_gramatam"] <- a * b3[3] + a * bm[3] * b3[4]; out["caur_prieku"] <- NA; out["caur_tiessaisti"] <- bm[2] * b3[4] }
  else { p <- d$prieks; m <- d$minutes; bp <- wlm(p, cbind(1, e, g), w); bm <- wlm(m, cbind(1, e, g, p), w); b3 <- wlm(y, cbind(1, e, g, p, m), w)
    out["tiesais"] <- b3[2]; out["caur_gramatam"] <- a * b3[3] + a * bp[3] * b3[4] + a * bm[3] * b3[5]; out["caur_prieku"] <- bp[2] * b3[4] + bp[2] * bm[4] * b3[5]; out["caur_tiessaisti"] <- bm[2] * b3[5] }
  out["netiesais"] <- sum(out[c("caur_gramatam", "caur_prieku", "caur_tiessaisti")], na.rm = TRUE)
  out["netiesa_dala"] <- out["netiesais"] / tau
  out["a_gramatas_uz_escs"] <- a
  out
}
kede_novertet <- function(D, ar_prieku, ar_min) {
  vars <- c("escs", "gramatas", if (ar_prieku) "prieks", if (ar_min) "minutes")
  D <- D[complete.cases(D[, vars, with = FALSE])]
  if (nrow(D) < 200) return(NULL)
  pvs <- PV[sapply(PV, function(p) any(!is.na(D[[p]])))]
  mk <- function(p) D[, .(y = get(p), escs, gramatas, prieks = if (ar_prieku) prieks else NA_real_, minutes = if (ar_min) minutes else NA_real_, w, skola)]
  est <- sapply(pvs, function(p) kede_fun(mk(p), ar_prieku, ar_min)); punkts <- rowMeans(est); imp <- apply(est, 1, var)
  d1 <- mk(pvs[1]); sk <- unique(d1$skola); idx <- split(seq_len(nrow(d1)), d1$skola)
  boot <- replicate(B_REP, { s <- sample(sk, length(sk), replace = TRUE); kede_fun(d1[unlist(idx[s], use.names = FALSE)], ar_prieku, ar_min) })
  se <- sqrt(apply(boot, 1, var) + (1 + 1 / length(pvs)) * imp)
  data.table(raditajs = names(punkts), est = punkts, se = se, n = nrow(D))
}
cells <- CJ(grupa = c("LVA A", "LVA", "LVA B", SAL), gads = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))
cat("UZMANĪBU: sāknēšana", B_REP, "replikāti x", nrow(cells), "šūnas x līdz 3 varianti; var aizņemt dažas minūtes.\n")
KE <- rbindlist(lapply(seq_len(nrow(cells)), function(i) {
  g <- cells$grupa[i]; y <- cells$gads[i]; D <- X[grupa == g & gads == y]
  r <- rbindlist(list(
    { r <- kede_novertet(D, FALSE, FALSE); if (!is.null(r)) r[, variants := "grāmatas"] },
    if (y %in% c(2009, 2018)) { r <- kede_novertet(D, TRUE, FALSE); if (!is.null(r)) r[, variants := "grāmatas, lasītprieks"] },
    if (y %in% c(2012, 2015, 2018)) { r <- kede_novertet(D, FALSE, TRUE); if (!is.null(r)) r[, variants := "grāmatas, laiks tiešsaistē"] },
    if (y == 2018) { r <- kede_novertet(D, TRUE, TRUE); if (!is.null(r)) r[, variants := "grāmatas, lasītprieks, laiks tiešsaistē"] }
  ))
  if (is.null(r) || nrow(r) == 0) return(NULL)
  r[, `:=`(grupa = g, gads = y)]
}))
V7k <- KE[grupa %in% SAL, .(est = mean(est), se = sqrt(sum(se^2)) / .N, n = sum(n), grupa = "7 valstis"), by = .(raditajs, variants, gads)]
KE <- rbind(KE, V7k[, names(KE), with = FALSE]); KE[, nosaukums := NOS[grupa]]
fwrite(KE[, .(grupa, nosaukums, gads, variants, raditajs, est = round(est, 3), se = round(se, 3), n)], "output/pisa2025/escs_kede.csv")
tabk <- function(g, variants_, rad, dig = 1) { d <- dcast(KE[grupa == g & variants == variants_ & raditajs == rad], grupa ~ gads, value.var = "est"); for (v in setdiff(names(d), "grupa")) d[[v]] <- round(d[[v]], dig); d }
for (g in c("LVA A", "7 valstis", "LVA")) {
  cat("\n---", NOS[g], ", variants tikai grāmatas (visi cikli) ---\n")
  for (r in c("kopejais", "tiesais", "caur_gramatam", "netiesa_dala", "a_gramatas_uz_escs")) { cat(r, ":\n"); print(tabk(g, "grāmatas", r, if (r %in% c("netiesa_dala", "a_gramatas_uz_escs")) 2 else 1), row.names = FALSE) }
}
cat("\nPilnie varianti (SE iekavās):\n")
PK <- KE[variants != "grāmatas" & grupa %in% c("LVA A", "7 valstis") & raditajs %in% c("kopejais", "tiesais", "caur_gramatam", "caur_prieku", "caur_tiessaisti", "netiesa_dala")]
PK[, txt := sprintf("%.2f (%.2f)", est, se)]
print(dcast(PK, grupa + gads + variants ~ raditajs, value.var = "txt"), row.names = FALSE)

# === 4. DEKOMPOZĪCIJA: SASTĀVS PRET KOEFICIENTIEM =============================
cat("\n", paste(rep("=", 70), collapse = ""), "\n4. Oaxaca un Blinder dekompozīcija (apvienotie koeficienti)\n", paste(rep("=", 70), collapse = ""), "\n\n")
ob_fun <- function(d0, d1, vars) {
  # d0 sākuma periods, d1 beigu periods; y, vars, w
  m0 <- sapply(vars, function(v) wmean(d0[[v]], d0$w)); m1 <- sapply(vars, function(v) wmean(d1[[v]], d1$w))
  y0 <- wmean(d0$y, d0$w); y1 <- wmean(d1$y, d1$w)
  dd <- rbind(d0[, c("y", vars, "w"), with = FALSE][, per := 0], d1[, c("y", vars, "w"), with = FALSE][, per := 1])
  Xm <- as.matrix(cbind(1, dd[, vars, with = FALSE], dd$per)); bp <- wlm(dd$y, Xm, dd$w)[2:(1 + length(vars))]
  sast <- (m1 - m0) * bp
  c(parmaina = y1 - y0, sastavs = sum(sast), koeficienti = (y1 - y0) - sum(sast), setNames(sast, paste0("sastavs_", vars)), setNames(m1 - m0, paste0("dX_", vars)))
}
ob_novertet <- function(g, g0, g1, vars) {
  D0 <- X[grupa == g & gads == g0]; D1 <- X[grupa == g & gads == g1]
  D0 <- D0[complete.cases(D0[, vars, with = FALSE])]; D1 <- D1[complete.cases(D1[, vars, with = FALSE])]
  if (nrow(D0) < 200 || nrow(D1) < 200) return(NULL)
  pvs <- PV[sapply(PV, function(p) any(!is.na(D0[[p]])) && any(!is.na(D1[[p]])))]
  mk <- function(D, p) D[, c(list(y = get(p), w = w, skola = skola), mget(vars))]
  est <- sapply(pvs, function(p) ob_fun(mk(D0, p), mk(D1, p), vars)); punkts <- rowMeans(est); imp <- apply(est, 1, var)
  a0 <- mk(D0, pvs[1]); a1 <- mk(D1, pvs[1]); i0 <- split(seq_len(nrow(a0)), a0$skola); i1 <- split(seq_len(nrow(a1)), a1$skola)
  boot <- replicate(B_REP, { s0 <- sample(names(i0), length(i0), TRUE); s1 <- sample(names(i1), length(i1), TRUE); ob_fun(a0[unlist(i0[s0], use.names = FALSE)], a1[unlist(i1[s1], use.names = FALSE)], vars) })
  se <- sqrt(apply(boot, 1, var) + (1 + 1 / length(pvs)) * imp)
  data.table(grupa = g, periods = paste(g0, "->", g1), mainigie = paste(vars, collapse = " + "), raditajs = names(punkts), est = punkts, se = se, n0 = nrow(D0), n1 = nrow(D1))
}
PER <- list(list(2003, 2018, c("escs", "gramatas")), list(2018, 2025, c("escs", "gramatas")), list(2003, 2025, c("escs", "gramatas")),
            list(2009, 2018, c("escs", "gramatas", "prieks")), list(2012, 2018, c("escs", "gramatas", "minutes")), list(2015, 2018, c("escs", "gramatas", "minutes")))
OB <- rbindlist(lapply(c("LVA A", "LVA", SAL), function(g) rbindlist(lapply(PER, function(p) ob_novertet(g, p[[1]], p[[2]], p[[3]])))))
V7o <- OB[grupa %in% SAL, .(est = mean(est), se = sqrt(sum(se^2)) / .N, n0 = sum(n0), n1 = sum(n1), grupa = "7 valstis"), by = .(periods, mainigie, raditajs)]
OB <- rbind(OB, V7o[, names(OB), with = FALSE])
fwrite(OB[, .(grupa, periods, mainigie, raditajs, est = round(est, 3), se = round(se, 3), n0, n1)], "output/pisa2025/escs_dekompozicija.csv")
for (g in c("LVA A", "7 valstis", "LVA")) {
  cat("\n---", NOS[g], "--- (punkti; SE iekavās)\n")
  d <- OB[grupa == g & !grepl("^dX_", raditajs)]; d[, txt := sprintf("%.1f (%.1f)", est, se)]
  print(dcast(d, periods + mainigie ~ raditajs, value.var = "txt"), row.names = FALSE)
}
cat("\nSastāva pārmaiņas (dX, beigu mīnus sākuma vidējais), latviešu valodas plūsma:\n")
print(dcast(OB[grupa == "LVA A" & grepl("^dX_", raditajs)], periods + mainigie ~ raditajs, value.var = "est")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], row.names = FALSE)

# === 5. GRAFIKI ==============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n5. Grafiki\n", paste(rep("=", 70), collapse = ""), "\n\n")
th <- theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title.position = "plot")
G1 <- S2[grupa %in% c("LVA A", "7 valstis")]; G1[, cet := factor(cet, labels = c("zemākā ceturtdaļa", "2.", "3.", "augstākā ceturtdaļa"))]
G1m <- melt(G1[, .(grupa, gads, cet, `Vairāk nekā 100 grāmatu mājās, %` = gram_virs100, `Nelasa priekam, %` = nelasa, `Divi vai vairāk datori, %` = datori2, `Stundas tiešsaistē darbdienā` = minutes)], id.vars = c("grupa", "gads", "cet"))
p1 <- ggplot(G1m[!is.na(value)], aes(gads, value, colour = cet)) + geom_line(linewidth = 1) + geom_point() + facet_grid(variable ~ NOS[grupa], scales = "free_y") +
  scale_colour_brewer(palette = "RdYlBu", name = "ESCS ceturtdaļa") + scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) +
  labs(title = "Starpposmi pa sociālā fona ceturtdaļām", x = NULL, y = NULL, caption = "Avots: OECD PISA mikrodati; R/pisa2025/88") + th
ggsave("output/pisa2025/plots/88_escs_starpposmi.png", p1, width = 10, height = 11, dpi = 200, bg = "white")
G2 <- KE[variants == "grāmatas" & grupa %in% c("LVA A", "7 valstis") & raditajs %in% c("kopejais", "tiesais", "caur_gramatam")]
G2[, raditajs := factor(raditajs, levels = c("kopejais", "tiesais", "caur_gramatam"), labels = c("kopējā saistība", "tiešā daļa", "netiešā caur grāmatām"))]
p2 <- ggplot(G2, aes(gads, est, colour = raditajs)) + geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, fill = raditajs), alpha = .12, colour = NA) + geom_line(linewidth = 1) + geom_point() +
  facet_wrap(~NOS[grupa]) + scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) + scale_colour_manual(values = c("#0b0b0b", "#2a78d6", "#eb6834"), name = NULL) + scale_fill_manual(values = c("#0b0b0b", "#2a78d6", "#eb6834"), guide = "none") +
  labs(title = "Sociālā fona saistība ar lasīšanu: tiešā un netiešā daļa caur grāmatām mājās", y = "punkti uz vienu ESCS SD", x = NULL, caption = "Avots: OECD PISA mikrodati; R/pisa2025/88") + th
ggsave("output/pisa2025/plots/88_escs_kede.png", p2, width = 11, height = 5.5, dpi = 200, bg = "white")
G3 <- OB[grupa %in% c("LVA A", "7 valstis") & raditajs %in% c("parmaina", "sastavs", "koeficienti") & mainigie == "escs + gramatas"]
G3[, raditajs := factor(raditajs, levels = c("parmaina", "sastavs", "koeficienti"), labels = c("kopējā pārmaiņa", "sastāvs (fons, grāmatas)", "koeficienti (pie tāda paša fona)"))]
p3 <- ggplot(G3, aes(periods, est, fill = raditajs)) + geom_col(position = position_dodge(.8), width = .7) + geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), position = position_dodge(.8), width = .25) +
  geom_hline(yintercept = 0) + facet_wrap(~NOS[grupa]) + scale_fill_manual(values = c("#0b0b0b", "#eb6834", "#2a78d6"), name = NULL) +
  labs(title = "Lasīšanas pārmaiņas dekompozīcija: sastāvs pret koeficientiem", x = NULL, y = "punkti", caption = "Avots: OECD PISA mikrodati; R/pisa2025/88") + th
ggsave("output/pisa2025/plots/88_escs_dekompozicija.png", p3, width = 11, height = 5.5, dpi = 200, bg = "white")
cat("Saglabāts: plots/88_escs_starpposmi.png, 88_escs_kede.png, 88_escs_dekompozicija.png\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili: escs_starpposmi_ceturtdalas.csv, escs_kede.csv, escs_dekompozicija.csv, plots/88_*.png\n")
cat("Beigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
