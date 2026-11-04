# ============================================================================
# 83. Pārmaiņu ātruma griesti un grīdas: sasniegumu pārmaiņu robežjosla visās
#     PISA valstīs 2000–2025 un Latvijas pozīcija tajā
#     (docs/46 Slānis 15, A un C daļa; H22, H23; docs/84)
# ============================================================================
#
# KĀPĒC. Latviešu valodas plūsmas 2018 → 2025 kritums (koriģēti 496 → 471) ir
# jāieliek visu valstu pārmaiņu sadalījumā: vai tas ir ekstrēms vai parasts
# ātrums valstij šādā līmenī (H22), un cik gadu vajag, lai pie vēsturiskajiem
# kāpuma griestiem atgrieztos 2018. gada līmenī (H23).
#
# Ievade:
#   data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx
#     Table I.B1.2a.36 (dabaszinātnes 2006–2025), .37 (lasīšana 2000–2025),
#     .38 (matemātika 2003–2025): vidējais un SE visām valstīm;
#     Table I.B1.2a.40: lasīšanas procentiles P10, P25, P50, P75, P90 pa cikliem
#   data/pisa_tehniskie/PISA2025_Vol1_AnnexA2_population.xlsx Table I.A2.1:
#     aptvēruma indekss CI3 pa cikliem (ietvara maiņu marķēšanai)
#   output/pisa2025/saites_kludas.csv (78. skripts): saites kļūdas pa ciklu pāriem
#   output/pisa2025/korigetie_raditaji.csv (72. skripts): A plūsmas oficiālā un
#     koriģētā rinda
#
# Izvade:
#   output/pisa2025/aploksne_pari.csv          visas valstis × jomas × ciklu pāri (1 līdz 4 ciklu logi)
#   output/pisa2025/aploksne_troksnis.csv      nejaušo efektu modelis pa jomām un pāriem
#   output/pisa2025/aploksne_kvantiles.csv     robežjosla pa līmeņa joslām
#   output/pisa2025/aploksne_kvantiles_rq.csv  kvantiļu regresija pret sākuma līmeni
#   output/pisa2025/aploksne_serijas.csv       garākās vienvirziena sērijas
#   output/pisa2025/aploksne_ekstremi.csv      lielākie kāpumi un kritumi
#   output/pisa2025/aploksne_latvija.csv       Latvijas un A plūsmas procentiles
#   output/pisa2025/aploksne_atgriesanas.csv   H23 atgriešanās horizonts
#   output/pisa2025/aploksne_kapumi_vesture.csv valstis ar vajadzīgo kāpumu 3 līdz 7 gados
#   output/pisa2025/plots/83_aploksne_lasisana.png, 83_aploksne_mat_dab.png,
#     83_ilgstosas_parmainas.png, 83_atgriesanas_horizonts.png
#   output/pisa2025/logs/83_parmainu_aploksne_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(readxl); library(ggplot2) })
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/83_parmainu_aploksne_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
t_sakums <- Sys.time()
options(width = 160)

metafor_ok  <- requireNamespace("metafor", quietly = TRUE)
quantreg_ok <- requireNamespace("quantreg", quietly = TRUE)
cat("metafor:", metafor_ok, "; quantreg:", quantreg_ok, "\n\n")

P_B1 <- "data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx"
P_A2 <- "data/pisa_tehniskie/PISA2025_Vol1_AnnexA2_population.xlsx"
KV <- c(0.05, 0.25, 0.50, 0.75, 0.95)
JOSLAS <- function(l) cut(l, c(-Inf, 420, 470, 520, Inf), labels = c("< 420", "420 līdz 470", "470 līdz 520", "> 520"), right = FALSE)
kv <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE, type = 7, names = FALSE))
procentile <- function(ref, x) if (is.na(x) || length(ref) < 5) NA_real_ else 100 * (mean(ref < x) + 0.5 * mean(ref == x))

# === 1. Dati =================================================================
cat(strrep("=", 70), "\n1. Dati: OECD trendu tabulas, aptvērums, saites kļūdas\n", strrep("=", 70), "\n\n", sep = "")

# 1a. Vidējie un SE pa cikliem
lasit_tabulu <- function(lapa, joma) {
  x <- suppressMessages(as.data.frame(read_excel(P_B1, sheet = lapa, col_names = FALSE)))
  cr <- which(apply(x, 1, function(r) any(grepl("^PISA 20[0-9][0-9]$", r))))[1]
  pos <- which(grepl("^PISA 20[0-9][0-9]$", x[cr, ]))
  cikli <- as.integer(sub("PISA ", "", x[cr, pos]))
  d <- x[(cr + 2):nrow(x), ]
  d <- d[!is.na(d[, 1]) & !grepl("^OECD average", d[, 1]) & (d[, 2] %in% c("m", "c") | !is.na(suppressWarnings(as.numeric(d[, 2])))), ]
  rbindlist(lapply(seq_along(cikli), function(i) data.table(
    joma = joma, valsts = sub("\\*$", "", trimws(d[, 1])), zvaigzne_2025 = grepl("\\*$", trimws(d[, 1])), cikls = cikli[i],
    videjais = suppressWarnings(as.numeric(d[, pos[i]])), se = suppressWarnings(as.numeric(d[, pos[i] + 1])))))
}
T <- rbind(lasit_tabulu("Table I.B1.2a.36", "SCIE"), lasit_tabulu("Table I.B1.2a.37", "READ"), lasit_tabulu("Table I.B1.2a.38", "MATH"))
T <- T[!is.na(videjais) & !is.na(se)]
cat("Valstu × ciklu novērojumi ar vidējo un SE:\n"); print(dcast(T[, .N, by = .(joma, cikls)], joma ~ cikls, value.var = "N"))
cat("Valstis:", uniqueN(T$valsts), "; ar zvaigznīti (2025 izlases standarti nav izpildīti):", paste(unique(T[zvaigzne_2025 == TRUE, valsts]), collapse = ", "), "\n")
cat("Latvija lasīšanā:\n"); print(T[joma == "READ" & valsts == "Latvia", .(cikls, videjais = round(videjais, 1), se = round(se, 2))])

# 1b. Lasīšanas procentiles (tabula 40)
x40 <- suppressMessages(as.data.frame(read_excel(P_B1, sheet = "Table I.B1.2a.40", col_names = FALSE)))
cr <- which(apply(x40, 1, function(r) any(grepl("^PISA 20[0-9][0-9]$", r))))[1]
pos <- which(grepl("^PISA 20[0-9][0-9]$", x40[cr, ])); cikli40 <- as.integer(sub("PISA ", "", x40[cr, pos]))
d40 <- x40[(cr + 3):nrow(x40), ]
d40 <- d40[!is.na(d40[, 1]) & !grepl("^OECD average", d40[, 1]) & (d40[, 2] %in% c("m", "c") | !is.na(suppressWarnings(as.numeric(d40[, 2])))), ]
PCT <- rbindlist(lapply(seq_along(cikli40), function(i) rbindlist(lapply(0:4, function(k) data.table(
  joma = "READ", raditajs = c("P10", "P25", "P50", "P75", "P90")[k + 1], valsts = sub("\\*$", "", trimws(d40[, 1])), cikls = cikli40[i],
  videjais = suppressWarnings(as.numeric(d40[, pos[i] + 2 * k])), se = suppressWarnings(as.numeric(d40[, pos[i] + 2 * k + 1])))))))
PCT <- PCT[!is.na(videjais) & !is.na(se)]
cat("Lasīšanas procentiļu novērojumi:", nrow(PCT), "; Latvija P10:", paste(PCT[valsts == "Latvia" & raditajs == "P10", paste0(cikls, ": ", round(videjais))], collapse = "; "), "\n")

# 1c. Faktiskie testa gadi (PISA 2000+, 2009+, PISA-D): no tabulu piezīmēm
G2001 <- c("Albania", "Argentina", "Bulgaria", "Chile", "Indonesia", "North Macedonia", "Peru", "Thailand")
G2002 <- c("Hong Kong (China)", "Israel", "Romania")
G2010 <- c("Costa Rica", "Georgia", "Malaysia", "Malta", "Mauritius", "Moldova", "United Arab Emirates")
G2017 <- c("Cambodia", "Ecuador", "Guatemala", "Paraguay", "Zambia")
faktiskais_gads <- function(valsts, cikls) fifelse(cikls == 2000 & valsts %in% G2001, 2001L,
                                            fifelse(cikls == 2000 & valsts %in% G2002, 2002L,
                                            fifelse(cikls == 2009 & valsts %in% G2010, 2010L,
                                            fifelse(cikls == 2018 & valsts %in% G2017, 2017L, as.integer(cikls)))))
T[, gads := faktiskais_gads(valsts, cikls)]; PCT[, gads := faktiskais_gads(valsts, cikls)]

# 1d. Aptvēruma indekss CI3 (Annex A2, tabula I.A2.1)
xa2 <- suppressMessages(as.data.frame(read_excel(P_A2, sheet = "Table I.A2.1", col_names = FALSE)))
cr <- which(apply(xa2, 1, function(r) any(grepl("^PISA 20[0-9][0-9]$", r))))[1]
pos <- which(grepl("^PISA 20[0-9][0-9]$", xa2[cr, ])); ciklia2 <- as.integer(sub("PISA ", "", xa2[cr, pos]))
ci3_kol <- sapply(pos, function(p) { j <- which(grepl("^Coverage Index 3", xa2[cr + 1, ])); j[j >= p][1] })
da2 <- xa2[(cr + 4):nrow(xa2), ]; da2 <- da2[!is.na(da2[, 1]) & !is.na(suppressWarnings(as.numeric(da2[, 2]))) | (!is.na(da2[, 1]) & da2[, 2] %in% "m"), ]
CI <- rbindlist(lapply(seq_along(ciklia2), function(i) data.table(valsts = sub("\\*$", "", trimws(da2[, 1])), cikls = ciklia2[i],
                                                                   ci3 = suppressWarnings(as.numeric(da2[, ci3_kol[i]])))))
CI <- CI[!is.na(ci3)]
cat("Aptvēruma indekss CI3: ", nrow(CI), " valstu × ciklu; cikli ", paste(sort(unique(CI$cikls)), collapse = " "), "\n", sep = "")
nesak <- setdiff(unique(T$valsts), unique(CI$valsts)); cat("Valstis trendu tabulās bez CI3 rindas:", paste(nesak, collapse = "; "), "\n")
cat("Latvija CI3:", paste(CI[valsts == "Latvia", paste0(cikls, ": ", round(ci3, 2))], collapse = "; "), "\n")

# 1e. Saites kļūdas
LE <- fread("output/pisa2025/saites_kludas.csv")
le_lookup <- function(joma_, no_, uz_) {
  l <- LE[joma == joma_]
  e <- l[no == no_ & uz == uz_]
  if (nrow(e)) return(list(LE = e$LE[1], veids = "tieša"))
  l[, att := abs(no - no_) + abs(uz - uz_) + 0.1 * (uz != uz_)]
  e <- l[order(att)][1]
  list(LE = e$LE, veids = sprintf("tuvākā (%d → %d)", e$no, e$uz))
}

# === 2. Pāri: pārmaiņa, pārmaiņa gadā, SE, marķējumi ==========================
cat("\n", strrep("=", 70), "\n2. Ciklu pāri (1 līdz 4 ciklu logi)\n", strrep("=", 70), "\n\n", sep = "")
MARK <- list(READ = list(c(2012, 2015, "datorizācija"), c(2015, 2018, "adaptīvā lasīšana"), c(2018, 2022, "Covid")),
             MATH = list(c(2012, 2015, "datorizācija"), c(2018, 2022, "Covid")),
             SCIE = list(c(2012, 2015, "datorizācija"), c(2018, 2022, "Covid")))
CIKLI <- list(READ = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025), MATH = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025),
              SCIE = c(2006, 2009, 2012, 2015, 2018, 2022, 2025))
markeri <- function(joma_, no_, uz_) {
  m <- MARK[[joma_]]; s <- sapply(m, function(z) as.integer(z[1]) >= no_ & as.integer(z[2]) <= uz_)
  if (any(s)) paste(sapply(m[s], `[`, 3), collapse = "; ") else ""
}
veidot_parus <- function(D, max_logs = 4) {
  D <- D[order(joma, valsts, cikls)]
  rbindlist(lapply(split(D, by = c("joma", "valsts")), function(d) {
    if (nrow(d) < 2) return(NULL)
    cik <- CIKLI[[sub("_.*", "", d$joma[1])]]
    rbindlist(lapply(1:(nrow(d) - 1), function(i) rbindlist(lapply((i + 1):nrow(d), function(j) {
      logs <- match(d$cikls[j], cik) - match(d$cikls[i], cik)
      if (logs > max_logs) return(NULL)
      # gadi: nominālie ciklu gadi (3 vai 4), lai logi būtu salīdzināmi; faktiskie testa gadi (PISA 2000+, 2009+,
      # PISA-D 2017) atzīmēti kolonnā gadi_faktiski un piezīmē
      data.table(joma = d$joma[1], valsts = d$valsts[1], no = d$cikls[i], uz = d$cikls[j], logs = logs,
                 gadi = d$cikls[j] - d$cikls[i], gadi_faktiski = d$gads[j] - d$gads[i],
                 l_no = d$videjais[i], l_uz = d$videjais[j], se_no = d$se[i], se_uz = d$se[j])
    }))))
  }))
}
PA <- veidot_parus(T)
PA[, c("LE", "le_veids") := { r <- le_lookup(joma, no, uz); list(r$LE, r$veids) }, by = .(joma, no, uz)]
PA[, `:=`(parmaina = l_uz - l_no, gada = (l_uz - l_no) / gadi, se_parm = sqrt(se_no^2 + se_uz^2 + LE^2), se_izlase = sqrt(se_no^2 + se_uz^2))]
PA[, z := parmaina / se_parm]
PA[, markeri := markeri(joma, no, uz), by = .(joma, no, uz)]
PA[, markets := markeri != ""]
PA <- merge(PA, CI[, .(valsts, no = cikls, ci3_no = ci3)], by = c("valsts", "no"), all.x = TRUE)
PA <- merge(PA, CI[, .(valsts, uz = cikls, ci3_uz = ci3)], by = c("valsts", "uz"), all.x = TRUE)
PA[, d_ci3 := ci3_uz - ci3_no]
PA[, aptveruma_maina := !is.na(d_ci3) & abs(d_ci3) >= 0.10]
PA[, pluss := fifelse(no == 2000 & valsts %in% c(G2001, G2002), "PISA 2000+", fifelse((no == 2009 | uz == 2009) & valsts %in% G2010, "PISA 2009+",
             fifelse((no == 2018 | uz == 2018) & valsts %in% G2017, "PISA-D 2017", "")))]
PA[pluss != "", pluss := sprintf("%s (faktiski %d gadi)", pluss, gadi_faktiski)]
PA <- merge(PA, unique(T[, .(valsts, zvaigzne_2025)]), by = "valsts")
PA[, zvaigzne_2025 := zvaigzne_2025 & uz == 2025]
PA[, josla := JOSLAS(l_no)]
setcolorder(PA, c("joma", "valsts", "no", "uz", "logs", "gadi"))
cat("Pāru skaits pa jomām un logiem:\n"); print(dcast(PA[, .N, by = .(joma, logs)], joma ~ logs, value.var = "N"))
cat("Secīgu pāru skaits pa jomām un pāriem (marķētie ar +):\n")
print(PA[logs == 1, .(n = .N, markets = markets[1], LE = round(LE[1], 2), le_veids = le_veids[1]), by = .(joma, no, uz)][order(joma, no)])
cat("Pāri ar netiešu (tuvāko) saites kļūdu:\n"); print(unique(PA[le_veids != "tieša", .(joma, no, uz, LE, le_veids)])[order(joma, no, uz)])
cat("Aptvēruma maiņa |ΔCI3| ≥ 0,10 (secīgi pāri):", PA[logs == 1 & aptveruma_maina == TRUE, .N], "no", PA[logs == 1, .N], "\n")

# === 3. Trokšņa attīrīšana: nejaušo efektu modelis un empīriskais Beijess ====
cat("\n", strrep("=", 70), "\n3. Trokšņa attīrīšana (nejaušo efektu modelis pa jomām un pāriem)\n", strrep("=", 70), "\n\n", sep = "")
re_modelis <- function(d, v) {
  ok <- is.finite(d) & is.finite(v); k <- sum(ok)
  out <- list(mu = NA_real_, tau2 = NA_real_, metode = NA_character_, saraut = rep(NA_real_, length(d)), k = k)
  if (k < 5) return(out)
  dd <- d[ok]; vv <- v[ok]; fit <- NULL
  if (metafor_ok) fit <- tryCatch(suppressWarnings(metafor::rma(yi = dd, vi = vv, method = "REML")), error = function(e) NULL)
  if (!is.null(fit)) { mu <- as.numeric(fit$b[1]); tau2 <- fit$tau2; met <- "REML (metafor)" } else {
    w <- 1 / vv; mu_fe <- sum(w * dd) / sum(w); Q <- sum(w * (dd - mu_fe)^2); C <- sum(w) - sum(w^2) / sum(w)
    tau2 <- max(0, (Q - (k - 1)) / C); w2 <- 1 / (vv + tau2); mu <- sum(w2 * dd) / sum(w2); met <- "DerSimonian-Laird"
  }
  s <- rep(NA_real_, length(d)); s[ok] <- mu + tau2 / (tau2 + vv) * (dd - mu)
  list(mu = mu, tau2 = tau2, metode = met, saraut = s, k = k)
}
# pa jomām un konkrētiem pāriem (kad ≥ 8 valstu), citādi pa jomām un logu klasēm
PA[, grupa := paste(joma, no, uz)]
PA[, n_gr := .N, by = grupa]
PA[n_gr < 8, grupa := paste(joma, "logs", logs)]
PA[, c("gada_saraut", "tau2", "mu_re", "re_metode") := {
  r <- re_modelis(gada, (se_parm / gadi)^2)
  list(r$saraut, r$tau2, r$mu, r$metode)
}, by = grupa]
TR <- PA[, .(n = .N, logs = logs[1], metode = re_metode[1], mu_gada = mu_re[1], tau2_gada = tau2[1],
             var_nov = var(gada), videjais_se2 = mean((se_parm / gadi)^2), videjais_se2_izlase = mean((se_izlase / gadi)^2),
             sd_nov = sd(gada), sd_saraut = sd(gada_saraut)), by = .(joma, grupa)]
TR[, `:=`(troksna_dala = pmin(1, videjais_se2 / var_nov), troksna_dala_izlase = pmin(1, videjais_se2_izlase / var_nov),
          troksna_dala_re = pmin(1, pmax(0, 1 - tau2_gada / var_nov)))]
setorder(TR, joma, logs, grupa)
fwrite(TR, "output/pisa2025/aploksne_troksnis.csv")
cat("Trokšņa daļa novērotajā pārmaiņu gadā dispersijā (secīgi pāri):\n")
print(TR[logs == 1 & !grepl("logs", grupa), .(joma, grupa, n, metode, sd_nov = round(sd_nov, 2), sd_saraut = round(sd_saraut, 2),
        troksnis_se_LE = round(troksna_dala, 2), troksnis_izlase = round(troksna_dala_izlase, 2), troksnis_re = round(troksna_dala_re, 2))])
KOPS <- PA[logs == 1, .(var_nov = var(gada), se2 = mean((se_parm / gadi)^2), se2_izl = mean((se_izlase / gadi)^2), var_saraut = var(gada_saraut)), by = joma]
KOPS[, `:=`(troksnis_se_LE = se2 / var_nov, troksnis_izlase = se2_izl / var_nov, dispersijas_kritums = 1 - var_saraut / var_nov)]
cat("\nKopā pa jomām (secīgi pāri, visi pāri kopā):\n"); print(KOPS[, c(list(joma = joma), lapply(.SD, round, 3)), .SDcols = -"joma"])
cat("Saglabāts: output/pisa2025/aploksne_troksnis.csv\n")
fwrite(PA, "output/pisa2025/aploksne_pari.csv"); cat("Saglabāts: output/pisa2025/aploksne_pari.csv (", nrow(PA), " rindas)\n", sep = "")

# === 4. Robežjosla atkarībā no līmeņa ==========================================
cat("\n", strrep("=", 70), "\n4. Robežjosla pa līmeņa joslām un kvantiļu regresija\n", strrep("=", 70), "\n\n", sep = "")
PAl <- rbind(PA[, .(joma, valsts, no, uz, logs, gadi, l_no, josla, markets, gada, kopa = "visi")],
             PA[markets == FALSE, .(joma, valsts, no, uz, logs, gadi, l_no, josla, markets, gada, kopa = "bez marķētajiem")])
PAl <- rbind(PAl[, attirits := "nē"],
             merge(PAl, PA[, .(joma, valsts, no, uz, gada_saraut)], by = c("joma", "valsts", "no", "uz"))[, .(joma, valsts, no, uz, logs, gadi, l_no, josla, markets, gada = gada_saraut, kopa, attirits = "jā")])
KVT <- rbind(
  PAl[valsts != "Latvia", .(n = .N, P5 = kv(gada, .05), P25 = kv(gada, .25), P50 = kv(gada, .5), P75 = kv(gada, .75), P95 = kv(gada, .95)), by = .(joma, logs, kopa, attirits, josla)],
  PAl[valsts != "Latvia", .(josla = "visas", n = .N, P5 = kv(gada, .05), P25 = kv(gada, .25), P50 = kv(gada, .5), P75 = kv(gada, .75), P95 = kv(gada, .95)), by = .(joma, logs, kopa, attirits)])
KVT[, josla := factor(josla, levels = c("visas", "< 420", "420 līdz 470", "470 līdz 520", "> 520"))]
setorder(KVT, joma, logs, kopa, attirits, josla)
fwrite(KVT, "output/pisa2025/aploksne_kvantiles.csv")
cat("Lasīšana, secīgi pāri, pārmaiņa gadā pa līmeņa joslām (bez Latvijas):\n")
print(KVT[joma == "READ" & logs == 1, .(kopa, attirits, josla, n, P5 = round(P5, 2), P25 = round(P25, 2), P50 = round(P50, 2), P75 = round(P75, 2), P95 = round(P95, 2))])
cat("\nLasīšana, 2 ciklu logi (6 līdz 7 gadi), pārmaiņa gadā pa līmeņa joslām:\n")
print(KVT[joma == "READ" & logs == 2, .(kopa, attirits, josla, n, P5 = round(P5, 2), P50 = round(P50, 2), P95 = round(P95, 2))])
cat("\nMatemātika un dabaszinātnes, secīgi pāri, visi, neattīrīti:\n")
print(KVT[joma != "READ" & logs == 1 & kopa == "visi" & attirits == "nē", .(joma, josla, n, P5 = round(P5, 2), P50 = round(P50, 2), P95 = round(P95, 2))])
cat("Saglabāts: output/pisa2025/aploksne_kvantiles.csv\n")

# kvantiļu regresija: gada ~ sākuma līmenis + pāra dummy
RQ <- NULL; RQ_GRID <- NULL
if (quantreg_ok) {
  grid <- seq(350, 600, by = 5)
  for (j in c("READ", "MATH", "SCIE")) for (lg in 1:2) for (kp in c("visi", "bez marķētajiem")) for (at in c("nē", "jā")) {
    d <- PAl[joma == j & logs == lg & kopa == kp & attirits == at & valsts != "Latvia"]
    d[, paris := factor(paste(no, uz))]
    if (nrow(d) < 40 || uniqueN(d$paris) < 2) next
    fit <- tryCatch(suppressWarnings(quantreg::rq(gada ~ l_no + paris, tau = KV, data = d)), error = function(e) NULL)
    if (is.null(fit)) next
    cf <- coef(fit)                              # rindas: (Intercept), l_no, paris...
    # marginālā prognoze: vidējais pār pāru dummy (svērts ar pāru biežumu)
    pw <- prop.table(table(d$paris)); dl <- levels(d$paris)
    dummy_ef <- sapply(seq_along(KV), function(q) sum(sapply(dl, function(p) if (p == dl[1]) 0 else cf[paste0("paris", p), q]) * pw))
    pred <- sapply(seq_along(KV), function(q) cf["(Intercept)", q] + cf["l_no", q] * grid + dummy_ef[q])
    colnames(pred) <- paste0("P", KV * 100)
    RQ_GRID <- rbind(RQ_GRID, data.table(joma = j, logs = lg, kopa = kp, attirits = at, limenis = grid, pred))
    RQ <- rbind(RQ, data.table(joma = j, logs = lg, kopa = kp, attirits = at, n = nrow(d), kvantile = paste0("P", KV * 100),
                               slipums_uz_100 = round(100 * cf["l_no", ], 3)))
  }
  fwrite(RQ_GRID, "output/pisa2025/aploksne_kvantiles_rq.csv")
  cat("\nKvantiļu regresija (quantreg::rq, gada ~ līmenis + pāra dummy): līmeņa slīpums uz 100 punktiem, secīgi pāri:\n")
  print(dcast(RQ[logs == 1], joma + kopa + attirits + n ~ kvantile, value.var = "slipums_uz_100"))
  cat("\nMarginālās prognozes lasīšanai pie 450, 485, 520 (visi, neattīrīti; secīgi pāri):\n")
  print(RQ_GRID[joma == "READ" & logs == 1 & kopa == "visi" & attirits == "nē" & limenis %in% c(450, 485, 520), lapply(.SD, round, 2), .SDcols = c("limenis", "P5", "P25", "P50", "P75", "P95")])
  cat("Saglabāts: output/pisa2025/aploksne_kvantiles_rq.csv\n")
} else cat("quantreg nav instalēts: kvantiļu regresija izlaista, lieto joslu kvantiles.\n")

# === 5. Ilgstošais ātrums ====================================================
cat("\n", strrep("=", 70), "\n5. Ilgstošais ātrums: sērijas, logi, asimetrija, AR(1)\n", strrep("=", 70), "\n\n", sep = "")
# 5a. Garākās vienvirziena sērijas (secīgas pārmaiņas vienā virzienā)
S1 <- PA[logs == 1][order(joma, valsts, no)]
SER <- S1[, {
  s <- sign(parmaina); r <- rle(s)
  beigas <- cumsum(r$lengths); sakums <- beigas - r$lengths + 1
  data.table(virziens = ifelse(r$values > 0, "kāpums", "kritums"), cikli = r$lengths, no = no[sakums], uz = uz[beigas],
             gadi = uz[beigas] - no[sakums] + 0, l_no = l_no[sakums], l_uz = l_uz[beigas], parmaina = l_uz[beigas] - l_no[sakums])
}, by = .(joma, valsts)]
SER[, gada := parmaina / gadi]
SER[, markets := mapply(function(j, a, b) markeri(j, a, b) != "", joma, no, uz)]
setorder(SER, joma, -cikli, parmaina)
fwrite(SER, "output/pisa2025/aploksne_serijas.csv")
cat("Vienvirziena sēriju garums (secīgu ciklu skaits) pa jomām un virzieniem:\n")
print(dcast(SER[, .N, by = .(joma, virziens, cikli)], joma + virziens ~ cikli, value.var = "N", fill = 0))
cat("\nGarākās krituma sērijas lasīšanā (≥ 3 cikli):\n")
print(SER[joma == "READ" & virziens == "kritums" & cikli >= 3, .(valsts, cikli, no, uz, l_no = round(l_no), l_uz = round(l_uz), parmaina = round(parmaina, 1), gada = round(gada, 2))][order(gada)])
cat("\nGarākās kāpuma sērijas lasīšanā (≥ 3 cikli):\n")
print(SER[joma == "READ" & virziens == "kāpums" & cikli >= 3, .(valsts, cikli, no, uz, l_no = round(l_no), l_uz = round(l_uz), parmaina = round(parmaina, 1), gada = round(gada, 2))][order(-gada)])
cat("Latvija sērijas:\n"); print(SER[valsts == "Latvia", .(joma, virziens, cikli, no, uz, parmaina = round(parmaina, 1), gada = round(gada, 2))])

# 5b. Logu sadalījumi pēc gadu skaita
LOGI <- PA[valsts != "Latvia" & gadi %in% c(6, 7, 9, 10, 12, 13), .(n = .N, P5 = kv(parmaina, .05), P50 = kv(parmaina, .5), P95 = kv(parmaina, .95),
           gada_P5 = kv(gada, .05), gada_P50 = kv(gada, .5), gada_P95 = kv(gada, .95), min = min(parmaina), max = max(parmaina)), by = .(joma, gadi)][order(joma, gadi)]
cat("\nLogu sadalījumi pēc gadu skaita (kopējā pārmaiņa un gadā; visas valstis, bez Latvijas):\n"); print(LOGI[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

# 5c. Kāpuma un krituma asimetrija
ASIM <- PA[valsts != "Latvia", .(n = .N, dala_kritumu = mean(gada < 0), kapums_P50 = kv(gada[gada > 0], .5), kapums_P95 = kv(gada[gada > 0], .95),
           kritums_P50 = kv(-gada[gada < 0], .5), kritums_P95 = kv(-gada[gada < 0], .95), kapums_max = max(gada), kritums_max = -min(gada)), by = .(joma, logs)][order(joma, logs)]
cat("\nKāpuma un krituma asimetrija (pārmaiņa gadā; kritumi kā pozitīvi lielumi):\n"); print(ASIM[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
ASIMb <- PA[valsts != "Latvia" & markets == FALSE, .(n = .N, dala_kritumu = mean(gada < 0), kapums_P95 = kv(gada[gada > 0], .95), kritums_P95 = kv(-gada[gada < 0], .95)), by = .(joma, logs)][order(joma, logs)]
cat("Tas pats bez marķētajiem cikliem:\n"); print(ASIMb[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

# 5d. AR(1) uz secīgām pārmaiņām (momentums pret atgriešanos pie vidējā)
AR <- merge(S1[, .(joma, valsts, vid = uz, d1 = gada, d1s = gada_saraut, se_vid = se_uz)], S1[, .(joma, valsts, vid = no, d2 = gada, d2s = gada_saraut)], by = c("joma", "valsts", "vid"))
ARr <- AR[, {
  f1 <- lm(d2 ~ d1); f2 <- lm(d2s ~ d1s)
  # kopīgā vidējā cikla izlases kļūda rada mākslīgu negatīvu korelāciju: aptuveni -SE_vid^2/(gadi^2 var(d1))
  .(n = .N, ar1_nov = coef(f1)[2], ar1_se = summary(f1)$coefficients[2, 2], ar1_saraut = coef(f2)[2], ar1_saraut_se = summary(f2)$coefficients[2, 2],
    maksl_negativs = -mean(se_vid^2 / 9) / var(d1), kor_nov = cor(d1, d2))
}, by = joma]
cat("\nAR(1) uz secīgām pārmaiņām gadā (d_t ~ d_t-1):\n"); print(ARr[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("Piezīme: negatīvs koeficients daļēji ir kopīgā vidējā cikla izlases kļūdas artefakts (kolonna maksl_negativs ir tā aptuvenais lielums).\n")

# 5e. Top 15 kāpumi un kritumi (secīgi pāri, pēc pārmaiņas gadā)
piezime <- function(pluss, aptv, d_ci3, mark, zv) {
  p <- c(if (pluss != "") pluss, if (aptv) sprintf("aptvērums CI3 %+.2f", d_ci3), if (mark != "") paste("marķēts:", mark), if (zv) "2025 izlases standarti nav izpildīti")
  paste(p, collapse = "; ")
}
PA[, piezime := mapply(piezime, pluss, aptveruma_maina, d_ci3, markeri, zvaigzne_2025)]
EX <- rbind(PA[logs == 1][order(joma, -gada), .SD[1:15], by = joma][, veids := "kāpums"],
            PA[logs == 1][order(joma, gada), .SD[1:15], by = joma][, veids := "kritums"])
EX <- EX[, .(joma, veids, valsts, no, uz, gadi, l_no = round(l_no, 1), l_uz = round(l_uz, 1), parmaina = round(parmaina, 1), gada = round(gada, 2), se_parm = round(se_parm, 2), piezime)]
fwrite(EX, "output/pisa2025/aploksne_ekstremi.csv")
cat("\nTop 15 kāpumi lasīšanā (secīgi pāri, gadā):\n"); print(EX[joma == "READ" & veids == "kāpums"])
cat("\nTop 15 kritumi lasīšanā (secīgi pāri, gadā):\n"); print(EX[joma == "READ" & veids == "kritums"])
cat("Saglabāts: output/pisa2025/aploksne_ekstremi.csv, aploksne_serijas.csv\n")

# === 6. Latvijas pozīcija =====================================================
cat("\n", strrep("=", 70), "\n6. Latvijas pozīcija robežjoslā\n", strrep("=", 70), "\n\n", sep = "")
KR <- fread("output/pisa2025/korigetie_raditaji.csv", encoding = "UTF-8")
setnames(KR, 1, "raditajs")
KRv <- KR[raditajs == "vid"]
A_PARI <- data.table(no = c(2015, 2018, 2022, 2018, 2015), uz = c(2018, 2022, 2025, 2025, 2025))
a_rinda <- function(kol, sekol, nos) rbindlist(lapply(seq_len(nrow(A_PARI)), function(i) {
  a <- KRv[gads == A_PARI$no[i]]; b <- KRv[gads == A_PARI$uz[i]]
  le <- le_lookup("READ", A_PARI$no[i], A_PARI$uz[i])$LE
  logs <- match(A_PARI$uz[i], CIKLI$READ) - match(A_PARI$no[i], CIKLI$READ)
  data.table(joma = "READ", valsts = nos, no = A_PARI$no[i], uz = A_PARI$uz[i], logs = logs, gadi = A_PARI$uz[i] - A_PARI$no[i],
             l_no = a[[kol]], l_uz = b[[kol]], se_no = a[[sekol]], se_uz = b[[sekol]], LE = le)
}))
LVA_A <- rbind(a_rinda("neapstr", "neapstr_se", "Latvija, latviešu valodas plūsma (oficiāli)"),
               a_rinda("korigets", "korigets_se", "Latvija, latviešu valodas plūsma (koriģēti)"))
LVA_A[, `:=`(parmaina = l_uz - l_no, gada = (l_uz - l_no) / gadi, se_parm = sqrt(se_no^2 + se_uz^2 + LE^2))]
# koriģētās rindas pārmaiņu SE no 72. skripta (2018. gada nobīde pāri 2015./2018. robežai un iesaiste 2025)
KP <- fread("output/pisa2025/korigetie_raditaji_periodi.csv", encoding = "UTF-8"); setnames(KP, 1, "raditajs")
for (i in seq_len(nrow(LVA_A))) if (grepl("koriģēti", LVA_A$valsts[i])) {
  s <- KP[raditajs == "vid" & no == LVA_A$no[i] & lidz == LVA_A$uz[i], izm_korig_se]; if (length(s)) LVA_A$se_parm[i] <- s[1]
} else { s <- KP[raditajs == "vid" & no == LVA_A$no[i] & lidz == LVA_A$uz[i], izm_neapstr_se]; if (length(s)) LVA_A$se_parm[i] <- s[1] }
LVA_O <- PA[valsts == "Latvia", .(joma, valsts = "Latvija, visa valsts (oficiāli)", no, uz, logs, gadi, l_no, l_uz, se_no, se_uz, LE, parmaina, gada, se_parm)]
LV <- rbind(LVA_O, LVA_A)
LV[, markeri := markeri(joma, no, uz), by = .(joma, no, uz)]
cat("Latvijas rindas (lasīšana):\n"); print(LV[joma == "READ", .(valsts, no, uz, gadi, l_no = round(l_no, 1), l_uz = round(l_uz, 1), parmaina = round(parmaina, 1), gada = round(gada, 2), se_parm = round(se_parm, 2))])

# procentiles atsauces klasē: tāds pats logu skaits, sākuma līmenis ± 30 punktu; četri varianti
ref_proc <- function(j, lg, l0, x, kp, at, plat = 30) {
  r <- PAl[joma == j & logs == lg & kopa == kp & attirits == at & valsts != "Latvia" & abs(l_no - l0) <= plat, gada]
  list(n = length(r), P5 = kv(r, .05), P50 = kv(r, .5), P95 = kv(r, .95), proc = procentile(r, x))
}
saraut_lv <- function(j, no_, uz_, lg, x, se) {   # Latvijas pārmaiņas saraušana ar tās pašas grupas tau2
  g <- PA[joma == j & no == no_ & uz == uz_]; if (!nrow(g)) g <- PA[joma == j & logs == lg]
  tau2 <- g$tau2[1]; mu <- g$mu_re[1]; if (is.na(tau2)) return(NA_real_)
  mu + tau2 / (tau2 + se^2) * (x - mu)
}
LVP <- rbindlist(lapply(seq_len(nrow(LV)), function(i) rbindlist(lapply(c("visi", "bez marķētajiem"), function(kp) rbindlist(lapply(c("nē", "jā"), function(at) {
  x <- LV$gada[i]; if (at == "jā") x <- saraut_lv(LV$joma[i], LV$no[i], LV$uz[i], LV$logs[i], x, LV$se_parm[i] / LV$gadi[i])
  r <- ref_proc(LV$joma[i], LV$logs[i], LV$l_no[i], x, kp, at)
  r_pair <- PAl[joma == LV$joma[i] & no == LV$no[i] & uz == LV$uz[i] & kopa == kp & attirits == at & valsts != "Latvia", gada]
  data.table(LV[i, .(joma, valsts, no, uz, logs, gadi, l_no, parmaina, gada)], kopa = kp, attirits = at, gada_vertets = x,
             ref_n = r$n, ref_P5 = r$P5, ref_P50 = r$P50, ref_P95 = r$P95, procentile = r$proc,
             para_n = length(r_pair), para_procentile = procentile(r_pair, x), zem_P5 = !is.na(r$proc) & x < r$P5)
}))))))
# rq procentile (nosacīti uz līmeni, ar pāra dummy): kvantiļu virkne, pie kuras prognoze pārsniedz Latvijas vērtību
if (quantreg_ok) {
  taus <- seq(0.02, 0.98, by = 0.02)
  LVP[, rq_procentile := {
    d <- PAl[joma == .BY$joma & logs == .BY$logs & kopa == .BY$kopa & attirits == .BY$attirits & valsts != "Latvia"]; d[, paris := factor(paste(no, uz))]
    out <- rep(NA_real_, .N)
    if (nrow(d) >= 40) {
      fit <- tryCatch(suppressWarnings(quantreg::rq(gada ~ l_no + paris, tau = taus, data = d)), error = function(e) NULL)
      if (!is.null(fit)) {
        cf <- coef(fit); pw <- prop.table(table(d$paris)); dl <- levels(d$paris)
        de <- sapply(seq_along(taus), function(q) sum(sapply(dl, function(p) if (p == dl[1]) 0 else cf[paste0("paris", p), q]) * pw))
        out <- sapply(seq_len(.N), function(i) { pr <- cf["(Intercept)", ] + cf["l_no", ] * l_no[i] + de; k <- which(pr >= gada_vertets[i])[1]; if (is.na(k)) 99 else 100 * taus[k] })
      }
    }
    out
  }, by = .(joma, logs, kopa, attirits)]
}
fwrite(LVP, "output/pisa2025/aploksne_latvija.csv")
cat("\nLatvijas procentiles atsauces klasē (tāds pats ciklu logs, sākuma līmenis ± 30; lasīšana):\n")
print(LVP[joma == "READ" & (no >= 2012 | uz == 2025), .(valsts = substr(valsts, 1, 40), no, uz, kopa, attirits, gada = round(gada_vertets, 2), ref_n, P5 = round(ref_P5, 2), P50 = round(ref_P50, 2), P95 = round(ref_P95, 2),
                            procentile = round(procentile, 1), rq_proc = if (quantreg_ok) round(rq_procentile, 0) else NA, zem_P5)], nrows = 200)
cat("\nLatvijas visas valsts procentiles matemātikā un dabaszinātnēs (visi, neattīrīti):\n")
print(LVP[joma != "READ" & kopa == "visi" & attirits == "nē", .(joma, no, uz, gada = round(gada, 2), ref_n, P5 = round(ref_P5, 2), P50 = round(ref_P50, 2), procentile = round(procentile, 1))])

# lasīšanas procentiles P10, P50, P90: Latvijas visas valsts pārmaiņu procentile (secīgi pāri un 2018 → 2025)
PP <- veidot_parus(PCT[, .(joma = paste0("READ_", raditajs), valsts, cikls, gads, videjais, se)], max_logs = 2)
PP[, gada := (l_uz - l_no) / gadi]
PPL <- PP[valsts == "Latvia" & (logs == 1 | (no == 2018 & uz == 2025)), .(raditajs = joma, no, uz, l_no = round(l_no), l_uz = round(l_uz), parmaina = round(l_uz - l_no, 1), gada = round(gada, 2))]
PPL <- rbindlist(lapply(seq_len(nrow(PPL)), function(i) {
  r <- PP[joma == PPL$raditajs[i] & no == PPL$no[i] & uz == PPL$uz[i] & valsts != "Latvia" & abs(l_no - PPL$l_no[i]) <= 30, gada]
  cbind(PPL[i], data.table(ref_n = length(r), P5 = round(kv(r, .05), 2), P50 = round(kv(r, .5), 2), P95 = round(kv(r, .95), 2), procentile = round(procentile(r, PPL$gada[i]), 1)))
}))
cat("\nLatvijas visas valsts sadalījuma rādītāju pārmaiņas gadā un procentile (atsauce: tas pats pāris, sākuma līmenis ± 30):\n")
print(PPL[raditajs %in% c("READ_P10", "READ_P50", "READ_P90") & no >= 2012][order(raditajs, no)])
fwrite(PPL, "output/pisa2025/aploksne_latvija_procentiles.csv")

# H22 verdikts: A plūsma 2018 → 2025 koriģēti pret P5 valstīm 470 līdz 500 līmenī (2 ciklu logi)
cat("\n--- H22 verdikts ---\n")
H22 <- rbindlist(lapply(c("visi", "bez marķētajiem"), function(kp) rbindlist(lapply(c("nē", "jā"), function(at) rbindlist(lapply(c("koriģēti", "oficiāli"), function(rinda) {
  lv <- LV[grepl(paste0("plūsma \\(", rinda, "\\)"), valsts) & no == 2018 & uz == 2025]
  x <- lv$gada; if (at == "jā") x <- saraut_lv("READ", 2018, 2025, 2, x, lv$se_parm / 7)
  r <- PAl[joma == "READ" & logs == 2 & kopa == kp & attirits == at & valsts != "Latvia" & l_no >= 470 & l_no < 500, gada]
  r67 <- PAl[joma == "READ" & logs == 2 & kopa == kp & attirits == at & valsts != "Latvia" & l_no >= 470 & l_no < 500 & gadi %in% c(6, 7), gada]
  data.table(rinda = rinda, kopa = kp, attirits = at, gada_A = round(x, 2), ref_n = length(r), P5 = round(kv(r, .05), 2), P50 = round(kv(r, .5), 2), P95 = round(kv(r, .95), 2),
             procentile = round(procentile(r, x), 1), zem_P5 = x < kv(r, .05), ref_n_6_7 = length(r67), P5_6_7 = round(kv(r67, .05), 2), procentile_6_7 = round(procentile(r67, x), 1), min_ref = round(min(r), 2))
}))))))
print(H22)
h22_kor <- H22[rinda == "koriģēti"]
cat(sprintf("\nH22: koriģētās A plūsmas 2018 → 2025 ātrums zem P5 %d no %d variantos (visi/bez marķētajiem × neattīrīti/attīrīti); procentiles %s.\n",
            sum(h22_kor$zem_P5), nrow(h22_kor), paste(h22_kor$procentile, collapse = " / ")))
fwrite(H22, "output/pisa2025/aploksne_h22.csv")

# === 7. H23: atgriešanās horizonts ===========================================
cat("\n", strrep("=", 70), "\n7. H23: atgriešanās horizonts\n", strrep("=", 70), "\n\n", sep = "")
vajag <- data.table(rinda = c("A plūsma koriģēti", "A plūsma oficiāli", "visa valsts oficiāli"),
                    l2018 = c(KRv[gads == 2018, korigets], KRv[gads == 2018, neapstr], T[joma == "READ" & valsts == "Latvia" & cikls == 2018, videjais]),
                    l2025 = c(KRv[gads == 2025, korigets], KRv[gads == 2025, neapstr], T[joma == "READ" & valsts == "Latvia" & cikls == 2025, videjais]))
vajag[, vajadzigs := l2018 - l2025]
cat("Vajadzīgais kāpums līdz 2018. gada līmenim:\n"); print(vajag[, lapply(.SD, round, 1), by = rinda])
KAP <- rbindlist(lapply(c("visi", "bez marķētajiem"), function(kp) rbindlist(lapply(c("nē", "jā"), function(at) rbindlist(lapply(1:3, function(lg) {
  r <- PAl[joma == "READ" & logs == lg & kopa == kp & attirits == at & valsts != "Latvia" & l_no >= 440 & l_no <= 500]
  r2 <- PAl[joma == "READ" & logs == lg & kopa == kp & attirits == at & valsts != "Latvia" & l_no >= 460 & l_no <= 480]
  data.table(kopa = kp, attirits = at, logs = lg, gadi = paste(sort(unique(r$gadi)), collapse = " līdz "), n = nrow(r),
             P50 = kv(r$gada, .5), P75 = kv(r$gada, .75), P95 = kv(r$gada, .95), max = max(r$gada), n_460_480 = nrow(r2), P95_460_480 = kv(r2$gada, .95))
}))))))
vaj <- vajag[rinda == "A plūsma koriģēti", vajadzigs]
KAP[, `:=`(vajadzigs = vaj, gadi_P50 = fifelse(P50 > 0, vaj / P50, Inf), gadi_P95 = fifelse(P95 > 0, vaj / P95, Inf), gadi_max = fifelse(max > 0, vaj / max, Inf),
           lidz_2029_vajag_gada = vaj / 4, P95_pietiek_2029 = P95 >= vaj / 4, max_pietiek_2029 = max >= vaj / 4)]
fwrite(KAP, "output/pisa2025/aploksne_atgriesanas.csv")
cat("\nKāpuma robežjosla valstīm 440 līdz 500 līmenī (lasīšana, pārmaiņa gadā) un gadi līdz atgriešanai pie koriģētā vajadzīgā kāpuma:\n")
print(KAP[, .(kopa, attirits, logs, gadi, n, P50 = round(P50, 2), P95 = round(P95, 2), max = round(max, 2), gadi_P50 = round(gadi_P50, 1), gadi_P95 = round(gadi_P95, 1), gadi_max = round(gadi_max, 1), P95_2029 = P95_pietiek_2029, P95_460_480 = round(P95_460_480, 2))])
cat(sprintf("\nLīdz 2029 (4 gadi) vajag %.2f punktus gadā; līdz 2032 (7 gadi) %.2f gadā.\n", vaj / 4, vaj / 7))

# vēsturiskais saraksts: valstis, kas kāpušas par ≥ vajadzīgo 3 līdz 7 gados no 440 līdz 500 līmeņa
VEST <- PA[joma == "READ" & valsts != "Latvia" & l_no >= 440 & l_no <= 500 & gadi >= 3 & gadi <= 7 & parmaina >= vaj][order(-gada)]
VEST <- VEST[, .(valsts, no, uz, gadi, l_no = round(l_no, 1), l_uz = round(l_uz, 1), parmaina = round(parmaina, 1), gada = round(gada, 2), se_parm = round(se_parm, 2), piezime)]
fwrite(VEST, "output/pisa2025/aploksne_kapumi_vesture.csv")
cat(sprintf("\nValstis ar kāpumu ≥ %.1f punktiem 3 līdz 7 gados no 440 līdz 500 līmeņa (lasīšana): %d gadījumi, %d valstis; bez piezīmēm (aptvērums, marķējums, 2000+): %d\n",
            vaj, nrow(VEST), uniqueN(VEST$valsts), sum(VEST$piezime == "")))
print(VEST)
VEST_MS <- PA[joma != "READ" & valsts != "Latvia" & l_no >= 440 & l_no <= 500 & gadi >= 3 & gadi <= 7 & parmaina >= vaj]
cat(sprintf("Tas pats matemātikā un dabaszinātnēs: %d gadījumi (%s)\n", nrow(VEST_MS), paste(unique(paste(VEST_MS$valsts, VEST_MS$no, VEST_MS$uz)), collapse = "; ")))

# === 8. Grafiki ==============================================================
cat("\n", strrep("=", 70), "\n8. Grafiki\n", strrep("=", 70), "\n\n", sep = "")
KRAS <- c(citas = "#9AA0A8", lv = "#B3261E", a = "#1F5FBF")
JOMU_NOS <- c(READ = "Lasīšana", MATH = "Matemātika", SCIE = "Dabaszinātnes")
apl_grafiks <- function(jomas, fails, virsraksts) {
  d <- PA[joma %in% jomas & logs == 1 & valsts != "Latvia"]
  d[, forma := ifelse(markets, "marķēts cikls (datorizācija, adaptīvā lasīšana, Covid)", "parasts cikls")]
  lv <- LV[joma %in% jomas & logs == 1]
  lv[, grupa := ifelse(grepl("visa valsts", valsts), "Latvija, visa valsts (oficiāli)", ifelse(grepl("koriģēti", valsts), "latviešu valodas plūsma (koriģēti)", "latviešu valodas plūsma (oficiāli)"))]
  lv[, etik := paste0(no, "→", uz)]
  lv <- lv[!grepl("plūsma \\(oficiāli\\)", grupa)]
  d[, joma_nos := JOMU_NOS[joma]]; lv[, joma_nos := JOMU_NOS[joma]]
  g <- ggplot(d, aes(x = l_no, y = gada)) +
    geom_hline(yintercept = 0, colour = "#666666", linewidth = 0.3) +
    geom_point(aes(shape = forma), colour = KRAS["citas"], size = 1.6, alpha = 0.7)
  if (!is.null(RQ_GRID)) {
    q <- melt(RQ_GRID[joma %in% jomas & logs == 1 & kopa == "visi" & attirits == "nē"], id.vars = c("joma", "limenis"), measure.vars = c("P5", "P25", "P50", "P75", "P95"), variable.name = "kvantile")
    q[, joma_nos := JOMU_NOS[joma]]
    q[, lt := ifelse(kvantile %in% c("P5", "P95"), "P5 un P95", ifelse(kvantile == "P50", "P50", "P25 un P75"))]
    g <- g + geom_line(data = q, aes(x = limenis, y = value, group = kvantile, linetype = lt), colour = "#333333", linewidth = 0.5)
  } else {
    q <- KVT[joma %in% jomas & logs == 1 & kopa == "visi" & attirits == "nē" & josla != "visas"]
    q[, joma_nos := JOMU_NOS[joma]]; q[, x := c(400, 445, 495, 545)[as.integer(josla) - 1]]
    g <- g + geom_point(data = q, aes(x = x, y = P5), shape = 95, size = 8) + geom_point(data = q, aes(x = x, y = P95), shape = 95, size = 8) + geom_point(data = q, aes(x = x, y = P50), shape = 95, size = 8)
  }
  g + geom_point(data = lv, aes(x = l_no, y = gada, colour = grupa), size = 3) +
    (if (requireNamespace("ggrepel", quietly = TRUE)) ggrepel::geom_text_repel(data = lv, aes(x = l_no, y = gada, label = etik, colour = grupa), size = 2.8, show.legend = FALSE, max.overlaps = 30, seed = 1)
     else geom_text(data = lv, aes(x = l_no, y = gada, label = etik, colour = grupa), size = 2.8, vjust = -0.9, show.legend = FALSE)) +
    scale_colour_manual(values = c("Latvija, visa valsts (oficiāli)" = unname(KRAS["lv"]), "latviešu valodas plūsma (koriģēti)" = unname(KRAS["a"])), name = NULL) +
    scale_shape_manual(values = c("parasts cikls" = 16, "marķēts cikls (datorizācija, adaptīvā lasīšana, Covid)" = 17), name = "Citas valstis") +
    scale_linetype_manual(values = c("P5 un P95" = "dashed", "P25 un P75" = "dotted", "P50" = "solid"), name = "Kvantiļu regresija") +
    facet_wrap(~ joma_nos, ncol = 2) +
    labs(title = virsraksts, subtitle = "Secīgi ciklu pāri, visas PISA valstis; pārmaiņa gadā pret sākuma līmeni. Kvantiles no regresijas ar pāra fiksētajiem efektiem.",
         x = "Sākuma cikla vidējais (punkti)", y = "Pārmaiņa gadā (punkti)") +
    coord_cartesian(xlim = c(320, 600), ylim = c(-15, 15)) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom", legend.box = "vertical", panel.grid.minor = element_blank())
}
png("output/pisa2025/plots/83_aploksne_lasisana.png", width = 2400, height = 1800, res = 200)
print(apl_grafiks("READ", NULL, "Lasīšanas pārmaiņu robežjosla un Latvijas pozīcija")); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_aploksne_lasisana.png\n")
png("output/pisa2025/plots/83_aploksne_mat_dab.png", width = 3000, height = 1700, res = 200)
print(apl_grafiks(c("MATH", "SCIE"), NULL, "Matemātikas un dabaszinātņu pārmaiņu robežjosla un Latvijas pozīcija")); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_aploksne_mat_dab.png\n")

# ilgstošās pārmaiņas: 2 ciklu logi (6 līdz 7 gadi), gadā, lasīšana
d7 <- PA[joma == "READ" & logs == 2 & valsts != "Latvia"]
d7[, kopa := ifelse(markets, "ar marķētu ciklu", "bez marķēta cikla")]
lv7 <- LV[joma == "READ" & no == 2018 & uz == 2025]
lv7[, grupa := ifelse(grepl("visa valsts", valsts), "Latvija, visa valsts (oficiāli)", ifelse(grepl("koriģēti", valsts), "latviešu valodas plūsma (koriģēti)", "latviešu valodas plūsma (oficiāli)"))]
lv7 <- lv7[!grepl("plūsma \\(oficiāli\\)", grupa)]
d7b <- d7[l_no >= 470 & l_no < 500]
p_ilg <- ggplot(d7, aes(x = gada)) +
  geom_histogram(aes(fill = kopa), breaks = seq(-8, 8, 0.5), colour = "white", linewidth = 0.2, position = "stack") +
  geom_vline(data = lv7, aes(xintercept = gada, colour = grupa), linewidth = 1) +
  geom_text(data = lv7, aes(x = gada, y = Inf, label = paste0(ifelse(grepl("visa valsts", grupa), "visa valsts (oficiāli)", "A plūsma (koriģēti)"), ": ", round(gada, 1), " gadā"), colour = grupa), vjust = c(2, 4), hjust = -0.05, size = 3, show.legend = FALSE) +
  geom_vline(xintercept = kv(d7b$gada, c(.05, .95)), linetype = "dashed", colour = "#333333") +
  scale_fill_manual(values = c("bez marķēta cikla" = "#9AA0A8", "ar marķētu ciklu" = "#C9CDD3"), name = "Citas valstis") +
  scale_colour_manual(values = c("Latvija, visa valsts (oficiāli)" = unname(KRAS["lv"]), "latviešu valodas plūsma (koriģēti)" = unname(KRAS["a"])), name = NULL) +
  labs(title = "Ilgstošas pārmaiņas: 2 ciklu logi (6 līdz 7 gadi) lasīšanā, pārmaiņa gadā",
       subtitle = sprintf("Visas valstis (%d logi); svītras: P5 un P95 valstīm 470 līdz 500 līmenī (%d logi): %.2f un %.2f gadā", nrow(d7), nrow(d7b), kv(d7b$gada, .05), kv(d7b$gada, .95)),
       x = "Pārmaiņa gadā (punkti)", y = "Logu skaits") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
png("output/pisa2025/plots/83_ilgstosas_parmainas.png", width = 2400, height = 1400, res = 200); print(p_ilg); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_ilgstosas_parmainas.png\n")

# atgriešanās horizonts
k0 <- KAP[kopa == "visi" & attirits == "nē"]
traj <- rbindlist(list(
  data.table(scenarijs = sprintf("P50 kāpums (%.1f gadā)", k0[logs == 2, P50]), gads = 2025:2040, limenis = KRv[gads == 2025, korigets] + k0[logs == 2, P50] * (0:15)),
  data.table(scenarijs = sprintf("P95 kāpums 6 līdz 7 gadu logos (%.1f gadā)", k0[logs == 2, P95]), gads = 2025:2040, limenis = KRv[gads == 2025, korigets] + k0[logs == 2, P95] * (0:15)),
  data.table(scenarijs = sprintf("P95 kāpums 3 līdz 4 gadu logos (%.1f gadā)", k0[logs == 1, P95]), gads = 2025:2040, limenis = KRv[gads == 2025, korigets] + k0[logs == 1, P95] * (0:15)),
  data.table(scenarijs = sprintf("vēsturiskais maksimums 6 līdz 7 gadu logos (%.1f gadā)", k0[logs == 2, max]), gads = 2025:2040, limenis = KRv[gads == 2025, korigets] + k0[logs == 2, max] * (0:15))))
traj[, scenarijs := factor(scenarijs, levels = unique(scenarijs))]
vest <- KRv[, .(gads, limenis = korigets)]
p_atg <- ggplot() +
  geom_hline(yintercept = KRv[gads == 2018, korigets], linetype = "dashed", colour = KRAS["a"]) +
  annotate("text", x = 2004, y = KRv[gads == 2018, korigets] + 1.5, label = sprintf("2018. gada koriģētais līmenis %.1f", KRv[gads == 2018, korigets]), hjust = 0, size = 3, colour = KRAS["a"]) +
  geom_vline(xintercept = 2029, colour = "#888888", linetype = "dotted") + annotate("text", x = 2029.2, y = 452, label = "2029", hjust = 0, size = 3, colour = "#555555") +
  geom_line(data = vest, aes(x = gads, y = limenis), colour = KRAS["a"], linewidth = 0.9) + geom_point(data = vest, aes(x = gads, y = limenis), colour = KRAS["a"], size = 2) +
  geom_line(data = traj, aes(x = gads, y = limenis, linetype = scenarijs), colour = "#333333", linewidth = 0.6) +
  scale_linetype_manual(values = c("solid", "longdash", "dashed", "dotted"), name = "Scenārijs no 2025 (valstis 440 līdz 500 līmenī, lasīšana)") +
  coord_cartesian(xlim = c(2003, 2040), ylim = c(450, 510)) +
  labs(title = "Atgriešanās horizonts: latviešu valodas plūsmas koriģētā rinda un vēsturiskie kāpuma ātrumi",
       subtitle = sprintf("Vajadzīgais kāpums %.1f punkti; pie P50 %s, pie P95 (6 līdz 7 gadu logi) %s gadi, pie maksimuma %s gadi", vaj,
                          if (is.finite(k0[logs == 2, gadi_P50])) sprintf("%.0f gadi", k0[logs == 2, gadi_P50]) else "nekad (P50 ≤ 0)", sprintf("%.0f", k0[logs == 2, gadi_P95]), sprintf("%.0f", k0[logs == 2, gadi_max])),
       x = NULL, y = "Lasīšana, koriģētais vidējais (punkti)") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", legend.direction = "vertical", panel.grid.minor = element_blank())
png("output/pisa2025/plots/83_atgriesanas_horizonts.png", width = 2400, height = 1500, res = 200); print(p_atg); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_atgriesanas_horizonts.png\n")

# === 9. Kopsavilkums =========================================================
cat("\n", strrep("=", 70), "\n9. Kopsavilkums\n", strrep("=", 70), "\n\n", sep = "")
k1 <- KVT[joma == "READ" & logs == 1 & kopa == "visi" & attirits == "nē" & josla == "470 līdz 520"]
cat(sprintf("Lasīšana, secīgi pāri, 470 līdz 520 līmenis (visi, neattīrīti; n = %d): P5 %.2f, P50 %.2f, P95 %.2f gadā\n", k1$n, k1$P5, k1$P50, k1$P95))
k1b <- KVT[joma == "READ" & logs == 1 & kopa == "bez marķētajiem" & attirits == "jā" & josla == "470 līdz 520"]
cat(sprintf("  bez marķētajiem, attīrīti (n = %d): P5 %.2f, P50 %.2f, P95 %.2f gadā\n", k1b$n, k1b$P5, k1b$P50, k1b$P95))
cat(sprintf("Troksnis (SE un LE) novērotajā secīgo pārmaiņu dispersijā: READ %.0f %%, MATH %.0f %%, SCIE %.0f %% (tikai izlases SE: %.0f / %.0f / %.0f %%)\n",
            100 * KOPS[joma == "READ", troksnis_se_LE], 100 * KOPS[joma == "MATH", troksnis_se_LE], 100 * KOPS[joma == "SCIE", troksnis_se_LE],
            100 * KOPS[joma == "READ", troksnis_izlase], 100 * KOPS[joma == "MATH", troksnis_izlase], 100 * KOPS[joma == "SCIE", troksnis_izlase]))
cat("H22:\n"); print(H22[, .(rinda, kopa, attirits, gada_A, ref_n, P5, procentile, zem_P5)])
cat("H23 (visi, neattīrīti):\n"); print(k0[, .(logs, gadi, n, P50 = round(P50, 2), P95 = round(P95, 2), max = round(max, 2), gadi_P50 = round(gadi_P50, 1), gadi_P95 = round(gadi_P95, 1), P95_pietiek_2029, max_pietiek_2029)])
cat("\nIzveidotie faili: aploksne_pari.csv, aploksne_troksnis.csv, aploksne_kvantiles.csv, aploksne_kvantiles_rq.csv, aploksne_serijas.csv, aploksne_ekstremi.csv,\n",
    "  aploksne_latvija.csv, aploksne_latvija_procentiles.csv, aploksne_h22.csv, aploksne_atgriesanas.csv, aploksne_kapumi_vesture.csv,\n",
    "  plots/83_aploksne_lasisana.png, 83_aploksne_mat_dab.png, 83_ilgstosas_parmainas.png, 83_atgriesanas_horizonts.png\n")
cat("Laiks:", round(as.numeric(difftime(Sys.time(), t_sakums, units = "mins")), 1), "min\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
