# ============================================================================
# 89. Skolu faktors: starpskolu atšķirības lasītprasmē PISA 2003 līdz 2025 un
#     9. klases centralizētajos eksāmenos 2022./23. līdz 2024./25.
#     (Slānis 19, docs/92; hipotēze H29)
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS: cik liela daļa skolēnu rezultātu atšķirību ir starp skolām, cik
#   no tās skaidro skolu sociālais sastāvs un apdzīvotā vieta, vai starpskolu
#   atšķirības laikā aug, un cik stabilas tās ir no gada uz gadu (tas ir, vai
#   tās ir noturīgs skolas efekts vai gada troksnis).
# A DAĻA, PISA 2003 līdz 2025 (latviešu valodas plūsma, Latvija, 7 valstis):
#   A1 starpskolu dispersijas daļa (ICC) un tās sadalījums: skolu vidējā ESCS
#      izskaidrotā daļa un atlikusī skolas daļa;
#   A2 apdzīvotās vietas plaisas Latvijā (ciems, mazpilsēta, pilsēta, Rīga)
#      ar un bez ESCS kontroles pa cikliem;
#   A3 skolu vidējo izkliede (P90 mīnus P10) un skolas iekšējā izkliede.
# B DAĻA, VIAA 9. klases centralizētie eksāmeni latviešu valodā un matemātikā
#   (pilna populācija, skolu nosaukumi; A plūsmas eksāmens centralizēts tikai
#   no 2022./23.):
#   B1 ICC pa gadiem un cik no tā skaidro urbanizācija un skolas tips;
#   B2 skolu vidējo izkliede, Rīgas skolu izkliede un piemēri;
#   B3 stabilitāte: skolu vidējo korelācija starp gadiem;
#   B4 urbanizācijas un skolu tipu plaisas pa gadiem.
# DATI: simptomi_apvienots.rds; PISA skolu anketas (apdzīvotās vietas
#   mainīgais: 2003 SC01Q01, 2006 SC07Q01, 2009 SC04Q01, 2012 SC03Q01, no 2015
#   SC001Q01TA; 1 ciems < 3 tūkst., 2 mazpilsēta 3 līdz 15 tūkst., 3 pilsēta
#   15 līdz 100 tūkst., 4 lielpilsēta 100 tūkst. līdz 1 milj., Latvijā Rīga);
#   data/valsts_parbaudes_darbi/mikrodati/processed/ce_mikrodati_2012_2025.csv.gz
#   (R/pisa2025/81, docs/81).
# KĻŪDAS A daļā: skolu klasteru sāknēšana un PV dispersija; B daļa ir pilna
#   populācija, SE netiek rēķinātas.
# IZVADE: output/pisa2025/skolu_pisa.csv, skolu_pisa_urban.csv, skolu_ce.csv,
#   skolu_ce_riga.csv, skolu_ce_stabilitate.csv, plots/89_*.png,
#   logs/89_skolu_faktors_output.txt
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
if (!file.exists("R/pisa2025") && file.exists("../../R/pisa2025")) setwd("../..")
output_file <- "output/pisa2025/logs/89_skolu_faktors_output.txt"
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākts:", format(Sys.time()), "\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2", "haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
set.seed(89)
B_REP <- 60
SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
NOS <- c(LVA = "Latvija, visi", `LVA A` = "Latvija, latviešu valodas plūsma", `LVA B` = "Latvija, krievu valodas plūsma",
         EST = "Igaunija", LTU = "Lietuva", POL = "Polija", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", DEU = "Vācija", `7 valstis` = "7 valstu vidējais")
URB <- c("ciems, līdz 3 tūkst.", "mazpilsēta, 3 līdz 15 tūkst.", "pilsēta, 15 līdz 100 tūkst.", "Rīga (virs 100 tūkst.)")
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; cw <- cumsum(w) / sum(w); x[which(cw >= p)[1]] }
wmean <- function(x, w) { ok <- !is.na(x); sum(x[ok] * w[ok]) / sum(w[ok]) }
wvar <- function(x, w) { m <- wmean(x, w); sum(w * (x - m)^2) / sum(w) }
wlm <- function(y, Xm, w) { sw <- sqrt(w); b <- tryCatch(solve(crossprod(Xm * sw), crossprod(Xm * sw, y * sw)), error = function(e) matrix(NA_real_, ncol(Xm), 1)); as.vector(b) }

# === 1. DATI =================================================================
cat(paste(rep("=", 70), collapse = ""), "\n1. Dati\n", paste(rep("=", 70), collapse = ""), "\n\n")
X <- as.data.table(readRDS("data/pisa2025/processed/simptomi_apvienots.rds"))
PV <- grep("^PV[0-9]+READ$", names(X), value = TRUE)
X <- X[, c("gads", "cnt", "skola", "w", "plusma", PV, "AUTO_ESCS"), with = FALSE]
for (v in names(X)) if (inherits(X[[v]], "haven_labelled")) X[[v]] <- unclass(X[[v]])
X <- X[!is.na(w) & w > 0 & gads != "2000"]; X[, gads := as.integer(gads)]; setnames(X, "AUTO_ESCS", "escs"); X[!is.na(escs) & escs >= 900, escs := NA_real_]
X[, sid := as.integer(sub("^\\S+ ", "", skola))]
# apdzīvotā vieta Latvijas skolām
urb_old <- rbindlist(lapply(list(c(2003, "SC01Q01"), c(2006, "SC07Q01"), c(2009, "SC04Q01"), c(2012, "SC03Q01")), function(p) {
  o <- fread(sprintf("data/pisa_veci/qqq_SCH_%s.csv", p[1]), select = c("CNT", "SCHOOLID", p[2]))[CNT == "LVA"]
  data.table(gads = as.integer(p[1]), sid = as.integer(o$SCHOOLID), urban = as.integer(o[[p[2]]])) }))
urb_new <- rbindlist(list(
  { o <- as.data.table(read_sav("data/pisa2015/raw/CY6_MS_CMB_SCH_QQQ.sav", col_select = c("CNT", "CNTSCHID", "SC001Q01TA")))[CNT == "LVA"]; data.table(gads = 2015L, sid = as.integer(o$CNTSCHID), urban = as.integer(o$SC001Q01TA)) },
  { o <- as.data.table(read_sav("data/pisa2018/raw/SCH/CY07_MSU_SCH_QQQ.sav", col_select = c("CNT", "CNTSCHID", "SC001Q01TA")))[CNT == "LVA"]; data.table(gads = 2018L, sid = as.integer(o$CNTSCHID), urban = as.integer(o$SC001Q01TA)) },
  { o <- as.data.table(read_sav("data/pisa2022/raw/CY08MSP_SCH_QQQ.SAV", col_select = c("CNT", "CNTSCHID", "SC001Q01TA")))[CNT == "LVA"]; data.table(gads = 2022L, sid = as.integer(o$CNTSCHID), urban = as.integer(o$SC001Q01TA)) },
  { o <- as.data.table(readRDS("data/pisa2025/processed/pisa_lva_2025_sch.rds")); data.table(gads = 2025L, sid = as.integer(o$CNTSCHID), urban = as.integer(unclass(o$SC001Q01TA))) }))
URBT <- rbind(urb_old, urb_new)[!is.na(urban) & urban %in% 1:5]; URBT[urban == 5, urban := 4L]
X <- merge(X, URBT, by = c("gads", "sid"), all.x = TRUE); X[cnt != "LVA", urban := NA_integer_]
cat("Apdzīvotās vietas pieejamība Latvijā pa cikliem, skolēnu % ar vērtību:\n"); print(X[cnt == "LVA", .(n = .N, ar_urban = round(100 * mean(!is.na(urban)), 1)), by = gads][order(gads)])
cat("Latvijas skolēnu sadalījums pa apdzīvotās vietas tipiem (svērts), %:\n"); print(dcast(X[cnt == "LVA" & !is.na(urban), .(p = round(100 * sum(w) / sum(X[cnt == "LVA" & gads == .BY$gads & !is.na(urban), w]), 1)), by = .(gads, urban)], urban ~ gads, value.var = "p"))
X[, grupa := cnt]; LA <- X[cnt == "LVA" & plusma == "A"][, grupa := "LVA A"]; LB <- X[cnt == "LVA" & plusma == "B"][, grupa := "LVA B"]
X <- rbind(X, LA, LB); X[, skola := paste(cnt, gads, skola)]

# === 2. A DAĻA: PISA STARPSKOLU DISPERSIJA UN TĀS AVOTI =======================
cat("\n", paste(rep("=", 70), collapse = ""), "\n2. PISA: starpskolu dispersija, skolu sastāvs, apdzīvotā vieta\n", paste(rep("=", 70), collapse = ""), "\n\n")
stat_fun <- function(d) {
  # d: y, escs, w, skola, urban
  y <- d$y; w <- d$w; out <- c()
  vy <- wvar(y, w)
  sm <- d[, .(my = wmean(y, w), me = if (all(is.na(escs))) NA_real_ else wmean(escs, w), sw = sum(w), n = .N, urb = urban[1]), by = skola]
  vb <- wvar(sm$my, sm$sw); out["icc"] <- vb / vy
  out["sk_vid_P90_P10"] <- wq(sm$my, sm$sw, .90) - wq(sm$my, sm$sw, .10)
  out["sk_ieks_sd"] <- sqrt(vy - vb)
  # cik no starpskolu dispersijas skaidro skolas vidējais ESCS
  ok <- !is.na(sm$me)
  if (sum(ok) > 10) { b <- wlm(sm$my[ok], cbind(1, sm$me[ok]), sm$sw[ok]); res <- sm$my[ok] - b[1] - b[2] * sm$me[ok]
    r2 <- 1 - wvar(res, sm$sw[ok]) / wvar(sm$my[ok], sm$sw[ok]); out["escs_skaidro_starpskolu"] <- r2; out["icc_atlikums"] <- out["icc"] * (1 - r2); out["slipums_skolas_escs"] <- b[2] } else { out["escs_skaidro_starpskolu"] <- NA; out["icc_atlikums"] <- NA; out["slipums_skolas_escs"] <- NA }
  # apdzīvotā vieta (tikai Latvijai)
  if (any(!is.na(d$urban))) {
    for (k in 1:4) { s <- !is.na(d$urban) & d$urban == k; out[paste0("urb", k, "_vid")] <- if (sum(s) > 30) wmean(y[s], w[s]) else NA }
    out["urb_Riga_ciems"] <- out["urb4_vid"] - out["urb1_vid"]
    s <- !is.na(d$urban) & !is.na(d$escs); Xm <- cbind(1, d$escs[s], d$urban[s] == 2, d$urban[s] == 3, d$urban[s] == 4); b <- wlm(y[s], Xm, w[s])
    out["urb_Riga_ciems_escs"] <- b[5]; out["urb_pilseta_ciems_escs"] <- b[4]; out["urb_mazpils_ciems_escs"] <- b[3]
    # urbanizācija skolu līmenī: cik no starpskolu dispersijas skaidro apdzīvotā vieta
    u <- !is.na(sm$urb); if (sum(u) > 10) { Xu <- cbind(1, sm$urb[u] == 2, sm$urb[u] == 3, sm$urb[u] == 4); bu <- wlm(sm$my[u], Xu, sm$sw[u]); ru <- sm$my[u] - Xu %*% bu
      out["urban_skaidro_starpskolu"] <- 1 - wvar(as.vector(ru), sm$sw[u]) / wvar(sm$my[u], sm$sw[u]) } else out["urban_skaidro_starpskolu"] <- NA
  } else { for (k in 1:4) out[paste0("urb", k, "_vid")] <- NA; out["urb_Riga_ciems"] <- NA; out["urb_Riga_ciems_escs"] <- NA; out["urb_pilseta_ciems_escs"] <- NA; out["urb_mazpils_ciems_escs"] <- NA; out["urban_skaidro_starpskolu"] <- NA }
  out
}
novertet <- function(D) {
  pvs <- PV[sapply(PV, function(p) any(!is.na(D[[p]])))]
  mk <- function(p) D[, .(y = get(p), escs, w, skola, urban)]
  est <- sapply(pvs, function(p) stat_fun(mk(p))); punkts <- rowMeans(est); imp <- apply(est, 1, var)
  d1 <- mk(pvs[1]); sk <- unique(d1$skola); idx <- split(seq_len(nrow(d1)), d1$skola)
  boot <- replicate(B_REP, { s <- sample(sk, length(sk), replace = TRUE); dd <- d1[unlist(idx[s], use.names = FALSE)]; dd[, skola := rep(seq_along(s), lengths(idx[s]))]; stat_fun(dd) })
  se <- sqrt(apply(boot, 1, var) + (1 + 1 / length(pvs)) * imp)
  data.table(raditajs = names(punkts), est = punkts, se = se, n = nrow(D), n_skolas = length(sk))
}
cells <- unique(X[, .(grupa, gads)])[order(grupa, gads)]
cat("UZMANĪBU: sāknēšana", B_REP, "replikāti x", nrow(cells), "šūnas; var aizņemt dažas minūtes.\n")
RES <- rbindlist(lapply(seq_len(nrow(cells)), function(i) { r <- novertet(X[grupa == cells$grupa[i] & gads == cells$gads[i]]); r[, `:=`(grupa = cells$grupa[i], gads = cells$gads[i])]; r }))
V7 <- RES[grupa %in% SAL, .(est = mean(est), se = sqrt(sum(se^2)) / .N, n = sum(n), n_skolas = sum(n_skolas), grupa = "7 valstis"), by = .(raditajs, gads)]
RES <- rbind(RES, V7[, names(RES), with = FALSE]); RES[, nosaukums := NOS[grupa]]
fwrite(RES[, .(grupa, nosaukums, gads, raditajs, est = round(est, 3), se = round(se, 3), n, n_skolas)], "output/pisa2025/skolu_pisa.csv")
tab <- function(rad, grupas = c("LVA A", "LVA", "LVA B", "7 valstis", SAL), dig = 2) { d <- dcast(RES[raditajs == rad & grupa %in% grupas], grupa ~ gads, value.var = "est"); d[, grupa := factor(grupa, levels = grupas)]; d <- d[order(grupa)]; for (v in setdiff(names(d), "grupa")) d[[v]] <- round(d[[v]], dig); d }
cat("2.1. Starpskolu dispersijas daļa lasīšanā (ICC):\n"); print(tab("icc"), row.names = FALSE)
cat("\n2.2. Cik no starpskolu dispersijas skaidro skolas vidējais ESCS (R2 skolu līmenī):\n"); print(tab("escs_skaidro_starpskolu"), row.names = FALSE)
cat("\n2.3. Atlikusī skolas daļa no kopējās dispersijas (ICC x (1 - R2)):\n"); print(tab("icc_atlikums"), row.names = FALSE)
cat("\n2.4. Skolu vidējo izkliede: P90 mīnus P10, punkti:\n"); print(tab("sk_vid_P90_P10", dig = 0), row.names = FALSE)
cat("\n2.5. Skolas iekšējā standartnovirze, punkti:\n"); print(tab("sk_ieks_sd", dig = 0), row.names = FALSE)
cat("\n2.6. Latvija: lasīšana pa apdzīvotās vietas tipiem (1 ciems, 2 mazpilsēta, 3 pilsēta, 4 Rīga), latviešu valodas plūsma:\n")
for (k in 1:4) { cat(URB[k], ":\n"); print(tab(paste0("urb", k, "_vid"), c("LVA A", "LVA", "LVA B"), 0), row.names = FALSE) }
cat("\n2.7. Rīga mīnus ciems, punkti, bez kontroles un ar ESCS kontroli:\n"); print(tab("urb_Riga_ciems", c("LVA A", "LVA", "LVA B"), 0), row.names = FALSE); print(tab("urb_Riga_ciems_escs", c("LVA A", "LVA", "LVA B"), 0), row.names = FALSE)
cat("\n2.8. Cik no starpskolu dispersijas Latvijā skaidro apdzīvotā vieta vien:\n"); print(tab("urban_skaidro_starpskolu", c("LVA A", "LVA", "LVA B")), row.names = FALSE)
UR <- RES[grepl("^urb", raditajs) & grupa %in% c("LVA A", "LVA", "LVA B")]
fwrite(UR[, .(grupa, gads, raditajs, est = round(est, 2), se = round(se, 2), n)], "output/pisa2025/skolu_pisa_urban.csv")

# === 3. B DAĻA: 9. KLASES CENTRALIZĒTIE EKSĀMENI PA SKOLĀM ====================
cat("\n", paste(rep("=", 70), collapse = ""), "\n3. VIAA 9. klases centralizētie eksāmeni 2022./23. līdz 2024./25.\n", paste(rep("=", 70), collapse = ""), "\n\n")
CE <- fread("data/valsts_parbaudes_darbi/mikrodati/processed/ce_mikrodati_2012_2025.csv.gz", encoding = "UTF-8")[klase == 9 & gads_sak >= 2022 & eks %in% c("LAT09", "MAT09") & !is.na(kop)]
CE <- CE[!grepl("Papildtermiņš", eks_nos)]
CE[, mg := paste0(gads_sak, "./", substr(gads_sak + 1, 3, 4), ".")]
CE[, tips_nos := c(`2` = "pamatskola", `3` = "vidusskola", `4` = "vakara, tālmācības", `6` = "valsts ģimnāzija", `7` = "profesionālā", `8` = "speciālā")[as.character(tips)]]
CE[, urban_nos := c(`1` = "Rīga", `2` = "valstspilsētas", `3` = "pilsētas", `4` = "lauki")[as.character(urban)]]
cat("Kārtotāji pa eksāmeniem un gadiem:\n"); print(dcast(CE[, .N, by = .(eks, mg)], eks ~ mg, value.var = "N"))
cat("Vidējais un SD, %:\n"); print(CE[, .(vid = round(mean(kop), 1), sd = round(sd(kop), 1), skolas = uniqueN(skola)), by = .(eks, mg)][order(eks, mg)])
# B1 ICC un tā avoti
icc_fun <- function(d) {
  vy <- var(d$kop); sm <- d[, .(my = mean(kop), n = .N, urban = urban[1], tips = tips[1]), by = skola][n >= 5]
  vb <- weighted.mean((sm$my - weighted.mean(sm$my, sm$n))^2, sm$n); icc <- vb / vy
  ok <- !is.na(sm$urban) & !is.na(sm$tips); f <- lm(my ~ factor(urban) + factor(tips), data = sm[ok], weights = n); r2 <- summary(f)$r.squared
  fu <- lm(my ~ factor(urban), data = sm[ok], weights = n); ft <- lm(my ~ factor(tips), data = sm[ok], weights = n)
  c(icc = icc, urban_tips_skaidro = r2, urban_skaidro = summary(fu)$r.squared, tips_skaidro = summary(ft)$r.squared, icc_atlikums = icc * (1 - r2), sk_P90_P10 = unname(quantile(sm$my, .9) - quantile(sm$my, .1)), sk_sd = sd(sm$my), skolas = nrow(sm))
}
B1 <- CE[, as.list(icc_fun(.SD)), by = .(eks, mg)][order(eks, mg)]
cat("\n3.1. ICC un tā avoti (skolas ar vismaz 5 kārtotājiem):\n"); print(B1[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], row.names = FALSE)
# B2 Rīga
RG <- CE[urban == 1]
rg_fun <- function(d) { sm <- d[, .(my = mean(kop), n = .N), by = skola][n >= 10]; vb <- weighted.mean((sm$my - weighted.mean(sm$my, sm$n))^2, sm$n); c(skolas = nrow(sm), P10 = unname(quantile(sm$my, .1)), P50 = unname(quantile(sm$my, .5)), P90 = unname(quantile(sm$my, .9)), min = min(sm$my), max = max(sm$my), icc = vb / var(d$kop)) }
B2 <- RG[, as.list(rg_fun(.SD)), by = .(eks, mg)][order(eks, mg)]
cat("\n3.2. Rīgas skolas: skolu vidējo sadalījums, % (skolas ar vismaz 10 kārtotājiem):\n"); print(B2[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)], row.names = FALSE)
SK <- CE[, .(vid = mean(kop), n = .N, urban_nos = urban_nos[1], tips_nos = tips_nos[1], pasv = pasv[1]), by = .(eks, mg, skola)]
fwrite(SK[n >= 10][, .(eks, mg, skola, pasv, urban_nos, tips_nos, n, vid = round(vid, 1))], "output/pisa2025/skolu_ce.csv")
rl <- SK[eks == "LAT09" & mg == "2024./25." & urban_nos == "Rīga" & n >= 20][order(-vid)]
cat("\nRīga, latviešu valoda 9. kl. 2024./25., skolas ar vismaz 20 kārtotājiem (", nrow(rl), "): augstākās 8 un zemākās 8:\n", sep = "")
print(rl[1:8, .(skola, tips_nos, n, vid = round(vid, 1))], row.names = FALSE); print(rl[(nrow(rl) - 7):nrow(rl), .(skola, tips_nos, n, vid = round(vid, 1))], row.names = FALSE)
pieM <- c("Rīgas Valsts 1. ģimnāzija", "Āgenskalna Valsts ģimnāzija", "Rīgas Centra humanitārā vidusskola", "Rīgas Teikas vidusskola", "Rīgas Purvciema vidusskola", "Rīgas Imantas vidusskola", "Rīgas Zolitūdes ģimnāzija")
piem <- SK[eks == "LAT09" & skola %in% pieM | (eks == "LAT09" & grepl("Vecmīlgrāv|31\\. vidusskola|Ziemeļvalstu", skola))]
cat("\nPiemēri (latviešu valoda 9. kl., % pa gadiem):\n"); print(dcast(piem, skola + tips_nos ~ mg, value.var = "vid")[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)], row.names = FALSE)
fwrite(rl[, .(skola, tips_nos, n, vid = round(vid, 1))], "output/pisa2025/skolu_ce_riga.csv")
# B3 stabilitāte
st_fun <- function(e, m1, m2) { a <- SK[eks == e & mg == m1 & n >= 15]; b <- SK[eks == e & mg == m2 & n >= 15]; m <- merge(a[, .(skola, v1 = vid)], b[, .(skola, v2 = vid)], by = "skola"); data.table(eks = e, no = m1, lidz = m2, skolas = nrow(m), korelacija = cor(m$v1, m$v2), spearman = cor(m$v1, m$v2, method = "spearman")) }
B3 <- rbindlist(lapply(c("LAT09", "MAT09"), function(e) rbindlist(list(st_fun(e, "2022./23.", "2023./24."), st_fun(e, "2023./24.", "2024./25."), st_fun(e, "2022./23.", "2024./25.")))))
cat("\n3.3. Skolu vidējo stabilitāte starp gadiem (skolas ar vismaz 15 kārtotājiem abos gados):\n"); print(B3[, .(eks, no, lidz, skolas, korelacija = round(korelacija, 2), spearman = round(spearman, 2))], row.names = FALSE)
fwrite(B3, "output/pisa2025/skolu_ce_stabilitate.csv")
# B4 plaisas
cat("\n3.4. Vidējais pa urbanizācijas tipiem, % (latviešu valoda 9. kl.):\n"); print(dcast(CE[eks == "LAT09" & !is.na(urban_nos), .(v = round(mean(kop), 1)), by = .(mg, urban_nos)], urban_nos ~ mg, value.var = "v"), row.names = FALSE)
cat("\n3.5. Vidējais pa skolu tipiem, % (latviešu valoda 9. kl.):\n"); print(dcast(CE[eks == "LAT09" & !is.na(tips_nos), .(v = round(mean(kop), 1), n = .N), by = .(mg, tips_nos)], tips_nos ~ mg, value.var = "v"), row.names = FALSE)
cat("\n3.6. Vidējais pa urbanizācijas tipiem, % (matemātika 9. kl.):\n"); print(dcast(CE[eks == "MAT09" & !is.na(urban_nos), .(v = round(mean(kop), 1)), by = .(mg, urban_nos)], urban_nos ~ mg, value.var = "v"), row.names = FALSE)

# === 4. GRAFIKI ==============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n4. Grafiki\n", paste(rep("=", 70), collapse = ""), "\n\n")
th <- theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title.position = "plot")
G1 <- RES[raditajs %in% c("icc", "icc_atlikums", "escs_skaidro_starpskolu", "sk_vid_P90_P10") & grupa %in% c("LVA A", "7 valstis", SAL)]
G1[, kr := fifelse(grupa == "LVA A", "Latvija, latviešu valodas plūsma", fifelse(grupa == "7 valstis", "7 valstu vidējais", "salīdzinājuma valsts"))]
G1[, raditajs := factor(raditajs, levels = c("icc", "escs_skaidro_starpskolu", "icc_atlikums", "sk_vid_P90_P10"), labels = c("Starpskolu dispersijas daļa", "Cik no tās skaidro skolas ESCS", "Atlikusī skolas daļa", "Skolu vidējo P90 mīnus P10, punkti"))]
p1 <- ggplot(G1, aes(gads, est, group = grupa, colour = kr, linewidth = kr, alpha = kr)) + geom_line() + geom_point(size = 1.6) + facet_wrap(~raditajs, scales = "free_y") +
  scale_colour_manual(values = c(`Latvija, latviešu valodas plūsma` = "#b3261e", `7 valstu vidējais` = "#0b0b0b", `salīdzinājuma valsts` = "#9aa0a6"), name = NULL) +
  scale_linewidth_manual(values = c(1.4, 1.1, 0.6), guide = "none") + scale_alpha_manual(values = c(1, 1, 0.7), guide = "none") + scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) +
  labs(title = "Skolu faktors lasīšanā, PISA 2003 līdz 2025", x = NULL, y = NULL, caption = "Avots: OECD PISA mikrodati; R/pisa2025/89") + th
ggsave("output/pisa2025/plots/89_skolu_pisa.png", p1, width = 11, height = 7, dpi = 200, bg = "white")
G2 <- RES[raditajs %in% paste0("urb", 1:4, "_vid") & grupa == "LVA A"]; G2[, vieta := factor(URB[as.integer(substr(raditajs, 4, 4))], levels = URB)]
p2 <- ggplot(G2, aes(gads, est, colour = vieta)) + geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, fill = vieta), alpha = .1, colour = NA) + geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) + scale_colour_brewer(palette = "Dark2", name = NULL) + scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(title = "Lasīšana pēc skolas apdzīvotās vietas, latviešu valodas plūsma", y = "PISA punkti", x = NULL, caption = "Avots: OECD PISA mikrodati un skolu anketas; R/pisa2025/89") + th
ggsave("output/pisa2025/plots/89_skolu_urban.png", p2, width = 10, height = 5.5, dpi = 200, bg = "white")
G3 <- SK[eks == "LAT09" & n >= 10 & !is.na(urban_nos)]; G3[, urban_nos := factor(urban_nos, levels = c("Rīga", "valstspilsētas", "pilsētas", "lauki"))]
p3 <- ggplot(G3, aes(urban_nos, vid)) + geom_boxplot(outlier.size = .6, fill = "#e8eef8") + facet_wrap(~mg) + labs(title = "9. klases latviešu valodas eksāmens: skolu vidējie pēc urbanizācijas", subtitle = "Katrs punkts ir skola (vismaz 10 kārtotāji); kaste: vidējā puse skolu", x = NULL, y = "skolas vidējais, %", caption = "Avots: VIAA kārtotāju līmeņa dati; R/pisa2025/89") + th
ggsave("output/pisa2025/plots/89_skolu_ce_urban.png", p3, width = 11, height = 5.5, dpi = 200, bg = "white")
cat("Saglabāts: plots/89_skolu_pisa.png, 89_skolu_urban.png, 89_skolu_ce_urban.png\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili: skolu_pisa.csv, skolu_pisa_urban.csv, skolu_ce.csv, skolu_ce_riga.csv, skolu_ce_stabilitate.csv, plots/89_*.png\n")
cat("Beigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
