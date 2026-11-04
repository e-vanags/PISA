# ============================================================================
# 84. Mērķu scenāriji PISA 2029, 2032 un 2035: robežjoslas pārnese uz IZM
#     mērķiem latviešu valodas plūsmai (docs/46 Slānis 15, 3. kārta; docs/85)
# ============================================================================
#
# KĀPĒC. Latvijas mērķi (zem 15 % zem 2. līmeņa, ap 10 % 5. un 6. līmenī) un
# ES 2030 mērķi (zem 15 % vāji, vismaz 12 % izcili) ir deklaratīvi. Robežjosla
# (83. skripts, docs/84) rāda, cik ātri valstis ar līdzīgu sākuma līmeni
# vēsturiski ir mainījušās. Šeit robežjosla pārnesta uz scenāriju tabulu:
# katrai jomai, abām sadalījuma malām un trim kontrolpunktiem pieci scenāriji
# (P10, P25, P50, P75, P95 no vēsturiskā pārmaiņu sadalījuma), lai mērķus
# varētu piesaistīt tam, kas vēsturē ir noticis, ne tam, ko gribētos.
#
# Ievade:
#   output/pisa2025/aploksne_pari.csv (83. skripts): visu valstu ciklu pāri
#     ar vidējā pārmaiņu gadā, sākuma līmeni un marķējumiem
#   data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx:
#     tabulas I.B1.2a.36/.37/.38 (vidējie, arī OECD average-23 rindas) un
#     I.B1.2a.33/.34/.35 (zem 2. līmeņa un 5. līmeņa un virs daļas pa cikliem)
#   output/pisa2025/korigetie_raditaji.csv (72. skripts): koriģētā A rinda
#   output/pisa2025/agrie_signali_limeni.csv (55. skripts): pārbaudei
#   R/pisa2025/00_ab_dati.R un data/pisa2025/processed/ab_citas_jomas.rds:
#     A plūsmas PV lasīšanā, matemātikā un dabaszinātnēs 2003 līdz 2025
#
# Izvade (output/pisa2025/):
#   merku_scenariji_a_limeni.csv    A plūsmas vidējie un līmeņu daļas pa cikliem, trīs jomas
#   merku_scenariji_videjie.csv     vidējā scenāriji pa jomām, skalām, atsauces kopām
#   merku_scenariji_regresija.csv   daļa ~ vidējais regresijas un Latvijas sakarība
#   merku_scenariji_dalas.csv       daļu scenāriji: pārnese no vidējā (b) un tiešā robežjosla (a)
#   merku_scenariji_relativi.csv    atstatums no OECD-23 un tā scenāriji
#   merku_scenariji_escs.csv        ESCS zemākā ceturtdaļa un labs sniegums vismaz vienā jomā
#   merku_scenariji_vesture.csv     valstis ar ≥ 10 pp zem L2 samazinājumu vai ≥ 5 pp L5+ pieaugumu
#   merku_scenariji_izm.csv         galvenā tabula IZM
#   merku_scenariji_sasniedzamiba.csv  kad 15 %, 10 %, 12 % kļūst sasniedzami
#   plots/84_merku_scenariji.png
#   logs/84_merku_scenariji_output.txt
# ============================================================================

# readxl un ggplot2 (un līdz ar to vctrs) drīkst pievienot tikai PĒC ab_dati():
# ar ielādētu vctrs as.character() uz haven_labelled kolonnām 00_ab_dati.R nestrādā (kā 72. skriptā).
suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .p <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(.p)) setwd(normalizePath(file.path(dirname(.p), "..", "..")))
}
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/84_merku_scenariji_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n\n")
t_sakums <- Sys.time()
options(width = 170)

source("R/pisa2025/00_ab_dati.R")

P_B1 <- "data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx"
JOMAS <- c(READ = "lasīšana", MATH = "matemātika", SCIE = "dabaszinātnes")
L2 <- c(READ = 407.47, MATH = 420.07, SCIE = 409.54)
L3 <- c(READ = 480.18, MATH = 482.38, SCIE = 484.14)
L4 <- c(READ = 552.89, MATH = 544.68, SCIE = 558.73)
L5 <- c(READ = 625.61, MATH = 606.99, SCIE = 633.33)
CIKLI <- list(READ = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025),
              MATH = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025),
              SCIE = c(2006, 2009, 2012, 2015, 2018, 2022, 2025))
MARK <- list(READ = list(c(2012, 2015), c(2015, 2018), c(2018, 2022)),
             MATH = list(c(2012, 2015), c(2018, 2022)), SCIE = list(c(2012, 2015), c(2018, 2022)))
KV  <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
KVN <- paste0("P", c("05", "10", "25", "50", "75", "90", "95"))
# Scenāriji: vidējam un L5+ (augstāk = labāk) kvantile tieši; zem L2 (augstāk = sliktāk) spoguļota
SCEN <- data.table(scenarijs = c("Ļoti slikts", "Slikts gadījums", "Tipiska valsts", "Labākā ceturtdaļa", "Vēsturiskais maksimums"),
                   kv_augsup = c("P10", "P25", "P50", "P75", "P95"), kv_lejup = c("P90", "P75", "P50", "P25", "P05"))
KP <- data.table(gads = c(2029L, 2032L, 2035L), logs = 1:3, gadi = c(4, 7, 10))
JOSLA_VID <- 30; JOSLA_DALA <- 5; MIN_N <- 10
MERKI <- data.table(mala = c("zem_L2", "L5", "L5"), merkis = c(15, 10, 12),
                    nosaukums = c("Latvija un ES: zem 15 % zem 2. līmeņa", "Latvija: ap 10 % 5. un 6. līmenī", "ES 2030: vismaz 12 % izcili"))
kv <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE, type = 7, names = FALSE))
num <- function(x) suppressWarnings(as.numeric(x))
f1 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 1))
sadala <- function(n, t) cat("\n", strrep("=", 78), "\n", n, ". ", t, "\n", strrep("=", 78), "\n\n", sep = "")
markets <- function(joma_, no_, uz_) { m <- MARK[[joma_]]; any(sapply(m, function(z) z[1] >= no_ & z[2] <= uz_)) }

# === 1. A plūsmas līmeņi trīs jomās no PV ====================================
sadala(1, "Latviešu valodas plūsma: vidējais, zem 2. līmeņa, 5. līmenis un virs, 2003 līdz 2025, trīs jomas (PV, BRR, Rubins)")

D <- ab_dati()
suppressPackageStartupMessages({ library(readxl); library(ggplot2) })
KJ <- readRDS("data/pisa2025/processed/ab_citas_jomas.rds")
KOR <- fread("output/pisa2025/korigetie_raditaji.csv", encoding = "UTF-8")
MSAT <- KOR[raditajs == "vid" & gads == 2025, kor_msat]
IES  <- KOR[raditajs == "vid" & gads == 2025, kor_ies]
cat(sprintf("Korekcijas no 72. skripta: instrumenta nobīde 2018 līdz 2025 +%.2f punkti (attiecināta uz PV), iesaiste 2025 +%.2f (tikai vidējam)\n\n", MSAT, IES))

pv_joma <- function(g, joma) {
  if (joma == "READ") return(D[[g]]$pv)
  if (g %in% VECIE) {
    d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
    return(lapply(paste0("PV", 1:5, joma), function(k) as.numeric(d[[k]])))
  }
  x <- KJ[[g]]; r <- match(as.numeric(D[[g]]$sk$id), x$CNTSTUID)
  lapply(paste0("PV", 1:10, joma), function(k) x[[k]][r])
}
wq1 <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }
dala <- function(lim, virs = FALSE) if (virs) function(x, w) 100 * sum(w * (x >= lim)) / sum(w) else function(x, w) 100 * sum(w * (x < lim)) / sum(w)
GADI_J <- list(READ = names(D), MATH = names(D), SCIE = setdiff(names(D), "2003"))

AL <- list()
for (j in names(JOMAS)) for (g in GADI_J[[j]]) {
  sk <- D[[g]]$sk
  a <- sk$grupa %in% "A"
  pvj <- pv_joma(g, j)
  if (any(vapply(pvj, function(p) all(is.na(p[a])), TRUE))) { cat("  trūkst PV:", j, g, "\n"); next }
  q25 <- wq1(sk$escs[a & !is.na(sk$escs)], sk$w[a & !is.na(sk$escs)], 0.25)
  grupas <- list("A visi" = a, "A ESCS zemākā 1/4" = a & !is.na(sk$escs) & sk$escs <= q25)
  for (gr in names(grupas)) {
    i <- grupas[[gr]]
    pv <- lapply(pvj, `[`, i); w <- sk$w[i]; rw <- lapply(D[[g]]$rw, `[`, i)
    ok <- !is.na(pv[[1]]); pv <- lapply(pv, `[`, ok); w <- w[ok]; rw <- lapply(rw, `[`, ok)
    ST <- list(vid = VID, zem_L2 = dala(L2[[j]]), L5 = dala(L5[[j]], TRUE), L3_virs = dala(L3[[j]], TRUE))
    for (k in names(ST)) {
      r <- rubin_brr(pv, w, rw, ST[[k]])
      AL[[length(AL) + 1]] <- data.table(joma = j, gads = as.integer(g), grupa = gr, skala = "oficiāli", raditajs = k, n = sum(ok), vertiba = r[["est"]], se = r[["se"]])
      if (j == "READ" && g %in% c("2018", "2022", "2025") && k != "vid") {
        r2 <- rubin_brr(lapply(pv, `+`, MSAT), w, rw, ST[[k]])
        AL[[length(AL) + 1]] <- data.table(joma = j, gads = as.integer(g), grupa = gr, skala = "koriģēti", raditajs = k, n = sum(ok), vertiba = r2[["est"]], se = r2[["se"]])
      }
    }
  }
}
AL <- rbindlist(AL)
# Koriģētais lasīšanas vidējais no 72. skripta (nobīde + iesaiste), lai nebūtu divu definīciju
kv_vid <- KOR[raditajs == "vid", .(joma = "READ", gads = as.integer(gads), grupa = "A visi", skala = "koriģēti", raditajs = "vid", n = n, vertiba = korigets, se = korigets_se)]
AL <- rbind(AL, kv_vid[gads >= 2018])
# Pirms 2018 koriģētā = oficiālā (nobīde attiecas tikai uz 2018 un vēlāk)
AL <- rbind(AL, AL[joma == "READ" & skala == "oficiāli" & gads < 2018][, skala := "koriģēti"])
setorder(AL, joma, grupa, skala, raditajs, gads)

cat("A visi, oficiālā skala (vērtība, iekavās SE):\n")
w1 <- dcast(AL[grupa == "A visi" & skala == "oficiāli" & raditajs != "L3_virs", .(joma, gads, raditajs, v = sprintf("%.1f (%.1f)", vertiba, se))], joma + gads ~ raditajs, value.var = "v")
print(w1[, .(joma, gads, vid, zem_L2, L5)], nrows = 100)
cat("\nLasīšana koriģētā skala 2018 līdz 2025:\n")
print(dcast(AL[joma == "READ" & grupa == "A visi" & skala == "koriģēti" & gads >= 2018, .(gads, raditajs, v = sprintf("%.1f", vertiba))], gads ~ raditajs, value.var = "v"))

# Pārbaude pret 55. skriptu (agrie_signali_limeni.csv) un 72. skriptu (koriģētā zem L2)
AS <- fread("output/pisa2025/agrie_signali_limeni.csv", encoding = "UTF-8")
chk <- merge(AL[joma == "READ" & skala == "oficiāli" & raditajs %in% c("vid", "zem_L2", "L5"), .(gads, grupa, raditajs = fifelse(raditajs == "L5", "L5_un_virs", raditajs), v84 = vertiba)],
             AS[, .(gads = as.integer(gads), grupa, raditajs = `radītājs`, v55 = vertiba)], by = c("gads", "grupa", "raditajs"))
cat(sprintf("\nPārbaude pret 55. skriptu: %d rindas, lielākā starpība %.4f\n", nrow(chk), max(abs(chk$v84 - chk$v55))))
chk2 <- merge(AL[joma == "READ" & grupa == "A visi" & skala == "koriģēti" & raditajs == "zem_L2", .(gads, v84 = vertiba)], KOR[raditajs == "zem_L2", .(gads = as.integer(gads), v72 = korigets)], by = "gads")
cat(sprintf("Pārbaude koriģētā zem L2 pret 72. skriptu: lielākā starpība %.4f\n", max(abs(chk2$v84 - chk2$v72))))
fwrite(AL, "output/pisa2025/merku_scenariji_a_limeni.csv", bom = TRUE)

# 2025. gada sākuma punkti
S0 <- AL[grupa == "A visi" & gads == 2025, .(joma, skala, raditajs, vertiba)]
sakums <- function(j, sk, k) S0[joma == j & skala == sk & raditajs == k, vertiba]
SKALAS <- list(READ = c("oficiāli", "koriģēti"), MATH = "oficiāli", SCIE = "oficiāli")
cat("\nSākuma punkti 2025:\n")
print(dcast(S0[raditajs != "L3_virs"], joma + skala ~ raditajs, value.var = "vertiba"))

# === 2. OECD B1 tabulas: vidējie un daļas visām valstīm =======================
sadala(2, "OECD B1 tabulas: vidējie (.36/.37/.38) un līmeņu daļas (.33/.34/.35), visas valstis un OECD average-23")

lasit_videjos <- function(lapa, joma) {
  x <- suppressMessages(as.data.frame(read_excel(P_B1, sheet = lapa, col_names = FALSE)))
  cr <- which(apply(x, 1, function(r) any(grepl("^PISA 20[0-9][0-9]$", r))))[1]
  pos <- which(grepl("^PISA 20[0-9][0-9]$", x[cr, ])); cikli <- as.integer(sub("PISA ", "", x[cr, pos]))
  d <- x[(cr + 2):nrow(x), ]
  d <- d[!is.na(d[, 1]) & (d[, 2] %in% c("m", "c") | !is.na(num(d[, 2]))), ]
  rbindlist(lapply(seq_along(cikli), function(i) data.table(joma = joma, valsts = sub("\\*$", "", trimws(d[, 1])), zvaigzne = grepl("\\*$", trimws(d[, 1])),
                                                            cikls = cikli[i], videjais = num(d[, pos[i]]), se = num(d[, pos[i] + 1]))))
}
lasit_dalas <- function(lapa, joma) {
  x <- suppressMessages(as.data.frame(read_excel(P_B1, sheet = lapa, col_names = FALSE)))
  cr <- which(apply(x, 1, function(r) any(grepl("^Proficiency level in PISA 20[0-9][0-9]$", r))))[1]
  pos <- which(grepl("^Proficiency level in PISA 20[0-9][0-9]$", x[cr, ])); cikli <- as.integer(sub(".*PISA ", "", x[cr, pos]))
  stopifnot(grepl("Below Level 2", x[cr + 1, pos[1]]), grepl("Level 5 or above", x[cr + 1, pos[1] + 2]))
  d <- x[(cr + 3):nrow(x), ]
  d <- d[!is.na(d[, 1]) & (d[, 2] %in% c("m", "c") | !is.na(num(d[, 2]))), ]
  rbindlist(lapply(seq_along(cikli), function(i) data.table(joma = joma, valsts = sub("\\*$", "", trimws(d[, 1])), zvaigzne = grepl("\\*$", trimws(d[, 1])),
                                                            cikls = cikli[i], zem_L2 = num(d[, pos[i]]), zem_L2_se = num(d[, pos[i] + 1]),
                                                            L5 = num(d[, pos[i] + 2]), L5_se = num(d[, pos[i] + 3]))))
}
TV <- rbind(lasit_videjos("Table I.B1.2a.36", "SCIE"), lasit_videjos("Table I.B1.2a.37", "READ"), lasit_videjos("Table I.B1.2a.38", "MATH"))
TD <- rbind(lasit_dalas("Table I.B1.2a.33", "SCIE"), lasit_dalas("Table I.B1.2a.34", "READ"), lasit_dalas("Table I.B1.2a.35", "MATH"))
TV[, oecd := grepl("^OECD average", valsts)]; TD[, oecd := grepl("^OECD average", valsts)]
TV <- TV[!is.na(videjais)]; TD <- TD[!is.na(zem_L2) & !is.na(L5)]
OECD23_V <- TV[valsts == "OECD average-23", .(joma, cikls, oecd23_vid = videjais)]
OECD23_D <- TD[valsts == "OECD average-23", .(joma, cikls, oecd23_zem_L2 = zem_L2, oecd23_L5 = L5)]
cat("Vidējie: valstu × ciklu novērojumi (bez OECD rindām):\n"); print(dcast(TV[oecd == FALSE, .N, by = .(joma, cikls)], joma ~ cikls, value.var = "N"))
cat("Daļas: valstu × ciklu novērojumi (bez OECD rindām):\n"); print(dcast(TD[oecd == FALSE, .N, by = .(joma, cikls)], joma ~ cikls, value.var = "N"))
cat("\nOECD average-23 vidējais:\n"); print(dcast(OECD23_V, joma ~ cikls, value.var = "oecd23_vid"))
cat("OECD average-23 zem L2:\n"); print(dcast(OECD23_D, joma ~ cikls, value.var = "oecd23_zem_L2"))
cat("OECD average-23 L5+:\n"); print(dcast(OECD23_D, joma ~ cikls, value.var = "oecd23_L5"))
cat("\nLatvija (visa valsts, tikai pārbaudei, mērķos nelieto) 2025: ")
print(TD[valsts == "Latvia" & cikls == 2025, .(joma, zem_L2, L5)])
o23 <- fread("output/pisa2025/oecd_videjais_lasisana.csv")
cat(sprintf("Pārbaude: OECD-23 lasīšana no oecd_videjais_lasisana.csv sakrīt ar tabulu .37: %s\n",
            isTRUE(all.equal(o23$oecd_23, OECD23_V[joma == "READ"][match(o23$gads, cikls), oecd23_vid], tolerance = 1e-3))))

# === 3. Vidējā robežjosla un scenāriji ========================================
sadala(3, "Vidējā scenāriji: robežjosla valstīm ar sākuma līmeni ± 30 ap A plūsmas 2025 līmeni, logi 4, 7, 10 gadi")

PARI <- fread("output/pisa2025/aploksne_pari.csv", encoding = "UTF-8")
PARI <- PARI[valsts != "Latvia" & logs <= 3]
PARI[, kopa_covid := no >= 2018]
cat("Pāri (bez Latvijas, logi 1 līdz 3):\n"); print(dcast(PARI[, .N, by = .(joma, logs)], joma ~ logs, value.var = "N"))
cat("Covid laikmeta pāri (sākums ≥ 2018):\n"); print(dcast(PARI[kopa_covid == TRUE, .N, by = .(joma, logs, no, uz)], joma + no + uz ~ logs, value.var = "N"))

KOPAS <- c("visi", "bez marķētajiem", "Covid laikmets (sākums ≥ 2018)")
atlasit_kopu <- function(dt, kopa) switch(kopa, "visi" = dt, "bez marķētajiem" = dt[markets == FALSE], "Covid laikmets (sākums ≥ 2018)" = dt[kopa_covid == TRUE])
kvantiles_tab <- function(dt, var, lim0, josla_pl) {
  lim_kol <- names(lim0); lim0 <- unname(lim0)
  rbindlist(lapply(KOPAS, function(kp) rbindlist(lapply(1:3, function(lg) {
    x <- atlasit_kopu(dt[logs == lg & abs(get(lim_kol) - lim0) <= josla_pl], kp)
    v <- x[[var]]
    q <- if (length(v) >= MIN_N) kv(v, KV) else rep(NA_real_, length(KV))
    data.table(kopa = kp, logs = lg, n = length(v))[, (KVN) := as.list(q)][]
  }))))
}
VS <- list(); VQ <- list()
for (j in names(JOMAS)) for (sk in SKALAS[[j]]) {
  l0 <- sakums(j, sk, "vid")
  q <- kvantiles_tab(PARI[joma == j], "gada", c(l_no = l0), JOSLA_VID)
  q[, `:=`(joma = j, skala = sk, limenis_2025 = l0)]
  VQ[[length(VQ) + 1]] <- q
  s <- merge(q, KP, by = "logs")
  s <- s[, {
    r <- .SD
    rbindlist(lapply(seq_len(nrow(SCEN)), function(i) data.table(scenarijs = SCEN$scenarijs[i], kvantile = SCEN$kv_augsup[i],
      gada = r[[SCEN$kv_augsup[i]]], vertiba = l0 + gadi * r[[SCEN$kv_augsup[i]]])))
  }, by = .(joma, skala, kopa, logs, gads, gadi, n)]
  VS[[length(VS) + 1]] <- s
}
VQ <- rbindlist(VQ); VS <- rbindlist(VS)
VS[, scenarijs := factor(scenarijs, levels = SCEN$scenarijs)]
for (j in names(JOMAS)) for (sk in SKALAS[[j]]) {
  cat(sprintf("\n%s, %s, 2025 līmenis %.1f. Pārmaiņa gadā (kvantiles) pa logiem un kopām:\n", JOMAS[[j]], sk, sakums(j, sk, "vid")))
  print(VQ[joma == j & skala == sk, c("kopa", "logs", "n", KVN), with = FALSE][, (KVN) := lapply(.SD, round, 2), .SDcols = KVN])
  cat("Scenāriju vidējie (kopa: visi):\n")
  print(dcast(VS[joma == j & skala == sk & kopa == "visi"], scenarijs ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))
}
fwrite(VS[order(joma, skala, kopa, gads, scenarijs)], "output/pisa2025/merku_scenariji_videjie.csv", bom = TRUE)

# === 4. Pārnese no vidējā uz daļām (b): regresija daļa ~ vidējais + vidējais² ==
sadala(4, "Pārnese (b): daļa ~ vidējais + vidējais² visās valstīs × ciklos; pārbaude pret Latvijas A plūsmas 2003 līdz 2025 sakarību")

MD <- merge(TV[oecd == FALSE & valsts != "Latvia"], TD[oecd == FALSE, .(joma, valsts, cikls, zem_L2, L5)], by = c("joma", "valsts", "cikls"))
REG <- list(); FIT <- list()
for (j in names(JOMAS)) for (mala in c("zem_L2", "L5")) {
  d <- MD[joma == j]
  m <- lm(reformulate(c("videjais", "I(videjais^2)"), mala), data = d)
  b <- coef(m); FIT[[paste(j, mala)]] <- m
  f <- function(v) pmin(100, pmax(0, b[1] + b[2] * v + b[3] * v^2))
  # Latvijas A plūsmas vēsturiskā sakarība (oficiālā skala, visi cikli)
  a <- dcast(AL[joma == j & grupa == "A visi" & skala == "oficiāli" & raditajs %in% c("vid", mala)], gads ~ raditajs, value.var = "vertiba")
  ml <- lm(reformulate("vid", mala), data = a)
  for (sk in SKALAS[[j]]) {
    l0 <- sakums(j, sk, "vid"); d0 <- sakums(j, sk, mala)
    REG[[length(REG) + 1]] <- data.table(joma = j, mala = mala, skala = sk, n_valstu_ciklu = nrow(d), b0 = b[1], b1 = b[2], b2 = b[3],
      R2 = summary(m)$r.squared, rmse = sqrt(mean(resid(m)^2)),
      slipums_pp_uz_10p_starpt = 10 * (b[2] + 2 * b[3] * l0), slipums_pp_uz_10p_latvija = 10 * coef(ml)[2], latvija_slipuma_se_uz_10p = 10 * summary(ml)$coefficients[2, 2], latvija_R2 = summary(ml)$r.squared,
      limenis_2025 = l0, dala_2025 = d0, prognoze_2025 = f(l0), nobide_2025 = d0 - f(l0))
  }
}
REG <- rbindlist(REG)
cat("Regresijas koeficienti, slīpums pie A plūsmas 2025 līmeņa (pp uz 10 punktiem), Latvijas vēsturiskais slīpums un 2025 nobīde (faktiskā daļa mīnus prognoze):\n")
print(REG[, .(joma, mala, skala, n = n_valstu_ciklu, R2 = round(R2, 3), rmse = round(rmse, 2), starpt_pp_10p = round(slipums_pp_uz_10p_starpt, 2),
              LV_pp_10p = round(slipums_pp_uz_10p_latvija, 2), LV_se = round(latvija_slipuma_se_uz_10p, 2), LV_R2 = round(latvija_R2, 2),
              limenis = round(limenis_2025, 1), dala = round(dala_2025, 1), progn = round(prognoze_2025, 1), nobide = round(nobide_2025, 1))])
fwrite(REG, "output/pisa2025/merku_scenariji_regresija.csv", bom = TRUE)

parnese <- function(j, mala, sk, v) {
  b <- coef(FIT[[paste(j, mala)]]); f <- function(x) b[1] + b[2] * x + b[3] * x^2
  l0 <- sakums(j, sk, "vid"); d0 <- sakums(j, sk, mala)
  list(neapstr = pmin(100, pmax(0, f(v))), piesaistits = pmin(100, pmax(0, d0 + f(v) - f(l0))))
}
DB <- VS[, {
  p <- lapply(c("zem_L2", "L5"), function(mala) { r <- parnese(joma[1], mala, skala[1], vertiba); data.table(mala = mala, dala_neapstr = r$neapstr, dala = r$piesaistits, scenarijs = scenarijs, gads = gads, kopa = kopa, n = n, videjais = vertiba) })
  rbindlist(p)
}, by = .(joma, skala)]
DB[, cels := "b: pārnese no vidējā (piesaistīta 2025 daļai)"]

# === 5. Tiešā daļu robežjosla (a) ============================================
sadala(5, "Tiešā daļu robežjosla (a): valstu daļu pārmaiņas pa logiem valstīm ar līdzīgu sākuma daļu (± 5 pp)")

veidot_dalu_parus <- function(td) {
  rbindlist(lapply(split(td, by = c("joma", "valsts")), function(x) {
    x <- x[order(cikls)]; ck <- CIKLI[[x$joma[1]]]
    if (nrow(x) < 2) return(NULL)
    cmb <- as.data.table(t(combn(nrow(x), 2)))
    rbindlist(lapply(seq_len(nrow(cmb)), function(k) {
      a <- x[cmb$V1[k]]; b <- x[cmb$V2[k]]
      lg <- match(b$cikls, ck) - match(a$cikls, ck)
      if (is.na(lg) || lg > 3) return(NULL)
      data.table(joma = a$joma, valsts = a$valsts, no = a$cikls, uz = b$cikls, logs = lg, gadi = b$cikls - a$cikls,
                 zem_L2_no = a$zem_L2, zem_L2_uz = b$zem_L2, d_zem_L2 = b$zem_L2 - a$zem_L2, d_zem_L2_gada = (b$zem_L2 - a$zem_L2) / (b$cikls - a$cikls),
                 L5_no = a$L5, L5_uz = b$L5, d_L5 = b$L5 - a$L5, d_L5_gada = (b$L5 - a$L5) / (b$cikls - a$cikls),
                 markets = markets(a$joma, a$cikls, b$cikls), kopa_covid = a$cikls >= 2018, zvaigzne = b$zvaigzne)
    }))
  }))
}
DP <- veidot_dalu_parus(TD[oecd == FALSE & valsts != "Latvia"])
DP <- merge(DP, TV[oecd == FALSE, .(joma, valsts, no = cikls, l_no = videjais)], by = c("joma", "valsts", "no"), all.x = TRUE)
DP <- merge(DP, PARI[, .(joma, valsts, no, uz, aptveruma_maina, pluss, d_ci3)], by = c("joma", "valsts", "no", "uz"), all.x = TRUE)
cat("Daļu pāri (bez Latvijas):\n"); print(dcast(DP[, .N, by = .(joma, logs)], joma ~ logs, value.var = "N"))

DA <- list(); DQ <- list()
for (j in names(JOMAS)) for (sk in SKALAS[[j]]) for (ml in c("zem_L2", "L5")) {
  d0 <- sakums(j, sk, ml)
  q <- kvantiles_tab(DP[joma == j], paste0("d_", ml, "_gada"), setNames(d0, paste0(ml, "_no")), JOSLA_DALA)
  q[, `:=`(joma = j, skala = sk, mala = ml, dala_2025 = d0)]
  DQ[[length(DQ) + 1]] <- q
  s <- merge(q, KP, by = "logs")
  kvk <- if (ml == "zem_L2") SCEN$kv_lejup else SCEN$kv_augsup
  s <- s[, { r <- .SD
    rbindlist(lapply(seq_len(nrow(SCEN)), function(i) data.table(scenarijs = SCEN$scenarijs[i], kvantile = kvk[i], gada = r[[kvk[i]]],
                                                                 dala = pmin(100, pmax(0, d0 + gadi * r[[kvk[i]]])))))
  }, by = .(joma, skala, mala, kopa, logs, gads, gadi, n)]
  DA[[length(DA) + 1]] <- s
}
DQ <- rbindlist(DQ); DA <- rbindlist(DA)
DA[, `:=`(scenarijs = factor(scenarijs, levels = SCEN$scenarijs), cels = "a: tiešā daļu robežjosla")]
for (j in names(JOMAS)) for (ml in c("zem_L2", "L5")) {
  cat(sprintf("\n%s, %s, oficiāli, 2025 daļa %.1f %%. Pārmaiņa gadā (pp):\n", JOMAS[[j]], ml, sakums(j, "oficiāli", ml)))
  print(DQ[joma == j & skala == "oficiāli" & mala == ml, c("kopa", "logs", "n", KVN), with = FALSE][, (KVN) := lapply(.SD, round, 2), .SDcols = KVN])
}

# Apvienotā daļu tabula (b galvenā, a pārbaude)
DAL <- rbind(DB[, .(joma, skala, mala, kopa, gads, scenarijs, cels, n, videjais, dala_neapstr, dala)],
             DA[, .(joma, skala, mala, kopa, gads, scenarijs, cels, n, videjais = NA_real_, dala_neapstr = NA_real_, dala)])
DAL[, mala_nos := fifelse(mala == "zem_L2", "zem 2. līmeņa, %", "5. un 6. līmenis, %")]
fwrite(DAL[order(joma, skala, mala, cels, kopa, gads, scenarijs)], "output/pisa2025/merku_scenariji_dalas.csv", bom = TRUE)
cat("\nDaļu scenāriji (kopa: visi), b = pārnese no vidējā, a = tiešā robežjosla:\n")
for (j in names(JOMAS)) for (sk in SKALAS[[j]]) for (ml in c("zem_L2", "L5")) {
  cat(sprintf("\n%s, %s, %s (2025: %.1f %%):\n", JOMAS[[j]], sk, ml, sakums(j, sk, ml)))
  x <- DAL[joma == j & skala == sk & mala == ml & kopa == "visi"]
  x[, cels_k := fifelse(grepl("^b", cels), "b", "a")]
  print(dcast(x, scenarijs ~ cels_k + gads, value.var = "dala", fun.aggregate = function(v) round(v[1], 1)))
}

# === 6. Relatīvie mērķi: atstatums no OECD-23 ================================
sadala(6, "Relatīvie mērķi: A plūsmas atstatums no OECD average-23 un tā scenāriji (kopīgais trends neitralizēts)")

PR <- merge(PARI, OECD23_V[, .(joma, no = cikls, o_no = oecd23_vid)], by = c("joma", "no"))
PR <- merge(PR, OECD23_V[, .(joma, uz = cikls, o_uz = oecd23_vid)], by = c("joma", "uz"))
PR[, rel_gada := gada - (o_uz - o_no) / gadi]
ATST <- list(); RS <- list(); RQ <- list()
for (j in names(JOMAS)) {
  o25 <- OECD23_V[joma == j & cikls == 2025, oecd23_vid]
  # Vēsture: A plūsmas atstatums pa cikliem
  h <- merge(AL[joma == j & grupa == "A visi" & raditajs == "vid", .(gads, skala, vertiba)], OECD23_V[joma == j, .(gads = cikls, oecd23 = oecd23_vid)], by = "gads")
  h[, atstatums := vertiba - oecd23]
  hd <- merge(AL[joma == j & grupa == "A visi" & raditajs %in% c("zem_L2", "L5"), .(gads, skala, raditajs, vertiba)], OECD23_D[joma == j, .(gads = cikls, oecd23_zem_L2, oecd23_L5)], by = "gads")
  hd[, `:=`(oecd23 = fifelse(raditajs == "zem_L2", oecd23_zem_L2, oecd23_L5))][, atstatums := vertiba - oecd23]
  ATST[[length(ATST) + 1]] <- rbind(h[, .(joma = j, raditajs = "vid", skala, gads, vertiba, oecd23, atstatums)], hd[, .(joma = j, raditajs, skala, gads, vertiba, oecd23, atstatums)])
  for (sk in SKALAS[[j]]) {
    l0 <- sakums(j, sk, "vid"); g0 <- l0 - o25
    q <- kvantiles_tab(PR[joma == j], "rel_gada", c(l_no = l0), JOSLA_VID)
    q[, `:=`(joma = j, skala = sk, atstatums_2025 = g0)]
    # Daļa valstu, kurām atstatums no OECD-23 nepasliktinājās
    q[, dala_nepasliktinas := sapply(seq_len(.N), function(i) { x <- atlasit_kopu(PR[joma == j & logs == q$logs[i] & abs(l_no - l0) <= JOSLA_VID], q$kopa[i]); if (nrow(x) >= MIN_N) 100 * mean(x$rel_gada >= 0) else NA_real_ })]
    RQ[[length(RQ) + 1]] <- q
    s <- merge(q, KP, by = "logs")
    s <- s[, { r <- .SD
      rbindlist(lapply(seq_len(nrow(SCEN)), function(i) data.table(scenarijs = SCEN$scenarijs[i], kvantile = SCEN$kv_augsup[i], rel_gada = r[[SCEN$kv_augsup[i]]], atstatums = g0 + gadi * r[[SCEN$kv_augsup[i]]])))
    }, by = .(joma, skala, kopa, logs, gads, gadi, n, atstatums_2025)]
    RS[[length(RS) + 1]] <- s
  }
}
ATST <- rbindlist(ATST); RS <- rbindlist(RS); RQ <- rbindlist(RQ)
cat("A plūsmas atstatums no OECD-23 pa cikliem (vidējais; skala oficiāli, lasīšanai arī koriģēti):\n")
print(dcast(ATST[raditajs == "vid"], joma + skala ~ gads, value.var = "atstatums", fun.aggregate = function(x) round(x[1], 1)))
cat("Atstatums zem L2 (pp, pozitīvs = Latvijā vairāk vājo):\n")
print(dcast(ATST[raditajs == "zem_L2"], joma + skala ~ gads, value.var = "atstatums", fun.aggregate = function(x) round(x[1], 1)))
cat("Atstatums L5+ (pp, negatīvs = Latvijā mazāk izcilo):\n")
print(dcast(ATST[raditajs == "L5"], joma + skala ~ gads, value.var = "atstatums", fun.aggregate = function(x) round(x[1], 1)))
cat("\nRelatīvās pārmaiņas gadā (valsts mīnus OECD-23) kvantiles un daļa valstu, kam atstatums nepasliktinājās:\n")
print(RQ[, c("joma", "skala", "kopa", "logs", "n", KVN, "dala_nepasliktinas"), with = FALSE][, c(KVN, "dala_nepasliktinas") := lapply(.SD, round, 1), .SDcols = c(KVN, "dala_nepasliktinas")])
cat("\nAtstatuma scenāriji (kopa: visi):\n")
print(dcast(RS[kopa == "visi"], joma + skala + scenarijs ~ gads, value.var = "atstatums", fun.aggregate = function(x) round(x[1], 1)))
fwrite(rbind(ATST[, .(joma, skala, raditajs, gads, kopa = "vēsture", scenarijs = NA_character_, vertiba, oecd23, atstatums)],
             RS[, .(joma, skala, raditajs = "vid", gads, kopa, scenarijs, vertiba = NA_real_, oecd23 = NA_real_, atstatums)]),
       "output/pisa2025/merku_scenariji_relativi.csv", bom = TRUE)
fwrite(RQ, "output/pisa2025/merku_scenariji_relativi_kvantiles.csv", bom = TRUE)

# === 7. Nelabvēlīgas izcelsmes skolēni (ES c mērķis) ==========================
sadala(7, "ESCS zemākā ceturtdaļa: zem 2. līmeņa pa jomām un labs sniegums (3. līmenis vai virs) vismaz vienā jomā")

# Labs sniegums vismaz vienā jomā: 3. līmenis vai virs lasīšanā, matemātikā vai dabaszinātnēs (arī 4. līmenis vai virs)
VJ <- list()
for (g in setdiff(names(D), "2003")) {
  sk <- D[[g]]$sk; a <- sk$grupa %in% "A"
  pvs <- lapply(names(JOMAS), function(j) pv_joma(g, j)); M <- min(lengths(pvs))
  q25 <- wq1(sk$escs[a & !is.na(sk$escs)], sk$w[a & !is.na(sk$escs)], 0.25)
  grupas <- list("A visi" = a, "A ESCS zemākā 1/4" = a & !is.na(sk$escs) & sk$escs <= q25)
  for (gr in names(grupas)) {
    i <- grupas[[gr]]
    for (lim in c("L3", "L4")) {
      LM <- get(lim)
      ind <- lapply(seq_len(M), function(m) as.numeric(pvs[[1]][[m]] >= LM[["READ"]] | pvs[[2]][[m]] >= LM[["MATH"]] | pvs[[3]][[m]] >= LM[["SCIE"]]))
      ok <- i & !is.na(ind[[1]])
      r <- rubin_brr(lapply(ind, `[`, ok), sk$w[ok], lapply(D[[g]]$rw, `[`, ok), function(x, w) 100 * sum(w * x) / sum(w))
      VJ[[length(VJ) + 1]] <- data.table(gads = as.integer(g), grupa = gr, raditajs = paste0(lim, "_virs_viena_joma"), n = sum(ok), vertiba = r[["est"]], se = r[["se"]])
    }
  }
}
VJ <- rbindlist(VJ)
ESCS <- rbind(AL[grupa == "A ESCS zemākā 1/4" & skala == "oficiāli" & raditajs %in% c("zem_L2", "L5", "L3_virs"), .(joma, gads, grupa, raditajs, n, vertiba, se)],
              AL[grupa == "A visi" & skala == "oficiāli" & raditajs %in% c("zem_L2", "L3_virs"), .(joma, gads, grupa, raditajs, n, vertiba, se)],
              VJ[, .(joma = "vismaz viena", gads, grupa, raditajs, n, vertiba, se)])
ESCS[, escs_2025_salidzinamiba := fifelse(gads == 2025 & grepl("ESCS", grupa), "apšaubāma (docs/71 3. jautājums)", "")]
cat("Zem 2. līmeņa, ESCS zemākā ceturtdaļa (A plūsma, katra cikla iekšienē):\n")
print(dcast(ESCS[raditajs == "zem_L2" & grupa == "A ESCS zemākā 1/4"], joma ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))
cat("Zem 2. līmeņa, A visi:\n")
print(dcast(ESCS[raditajs == "zem_L2" & grupa == "A visi"], joma ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))
cat("3. līmenis vai virs pa jomām, ESCS zemākā ceturtdaļa:\n")
print(dcast(ESCS[raditajs == "L3_virs" & grupa == "A ESCS zemākā 1/4"], joma ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))
cat("Labs sniegums vismaz vienā jomā (3. līmenis vai virs; 4. līmenis vai virs), A visi un ESCS zemākā ceturtdaļa:\n")
print(dcast(VJ, raditajs + grupa ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))
cat("\nPiezīme: ESCS 2025 salīdzināmība ir apšaubāma (HISEI lēciens, HOMEPOS kritums, trūkstošo ESCS desmitkāršošanās; docs/71 3. jautājums),\n",
    "tāpēc ES c mērķis (vismaz 25 % nelabvēlīgas izcelsmes skolēnu ar labu sniegumu vismaz vienā jomā) pagaidām nav uzticami mērāms 2025. gada datos.\n")
fwrite(ESCS[order(joma, grupa, raditajs, gads)], "output/pisa2025/merku_scenariji_escs.csv", bom = TRUE)

# === 8. Vēsturiskā pārbaude ====================================================
sadala(8, "Vēsturiskā pārbaude: valstis no līdzīga līmeņa (± 30) 6 līdz 10 gados ar zem L2 samazinājumu ≥ 10 pp vai L5+ pieaugumu ≥ 5 pp")

VH <- list()
for (j in names(JOMAS)) {
  l0 <- sakums(j, "oficiāli", "vid")
  x <- DP[joma == j & logs %in% 2:3 & !is.na(l_no) & abs(l_no - l0) <= JOSLA_VID]
  x[, `:=`(zem_L2_10pp = d_zem_L2 <= -10, L5_5pp = d_L5 >= 5)]
  cat(sprintf("\n%s (sākuma līmenis %.0f ± %d): logi 2 un 3, n = %d pāri, %d valstis\n", JOMAS[[j]], l0, JOSLA_VID, nrow(x), uniqueN(x$valsts)))
  for (lg in 2:3) {
    y <- x[logs == lg]
    cat(sprintf("  logs %d (%s gadi): n = %d pāri, %d valstis; zem L2 ≥ 10 pp samazinājums: %d pāri (%d valstis, %d bez marķētiem cikliem); L5+ ≥ 5 pp pieaugums: %d pāri (%d valstis, %d bez marķētiem)\n",
                lg, if (lg == 2) "6 līdz 7" else "9 līdz 10", nrow(y), uniqueN(y$valsts), sum(y$zem_L2_10pp), uniqueN(y[zem_L2_10pp == TRUE, valsts]), uniqueN(y[zem_L2_10pp == TRUE & markets == FALSE, valsts]),
                sum(y$L5_5pp), uniqueN(y[L5_5pp == TRUE, valsts]), uniqueN(y[L5_5pp == TRUE & markets == FALSE, valsts])))
  }
  # Salīdzinājumam: visas valstis neatkarīgi no līmeņa
  z <- DP[joma == j & logs == 3]
  cat(sprintf("  visi līmeņi, logs 3: n = %d; zem L2 ≥ 10 pp: %d pāri (%d valstis); L5+ ≥ 5 pp: %d pāri (%d valstis)\n", nrow(z), sum(z$d_zem_L2 <= -10), uniqueN(z[d_zem_L2 <= -10, valsts]), sum(z$d_L5 >= 5), uniqueN(z[d_L5 >= 5, valsts])))
  VH[[length(VH) + 1]] <- x[zem_L2_10pp == TRUE | L5_5pp == TRUE]
}
VH <- rbindlist(VH)
VH[, piezime := trimws(paste(fifelse(markets, "marķēts cikls", ""), fifelse(!is.na(aptveruma_maina) & aptveruma_maina, sprintf("aptvēruma maiņa (CI3 %+.2f)", d_ci3), ""),
                              fifelse(!is.na(pluss) & pluss != "", as.character(pluss), ""), fifelse(zvaigzne, "2025 izlases standarti nav izpildīti", ""), sep = "; "))]
VH[, piezime := gsub("(; )+", "; ", gsub("^(; )+|(; )+$", "", piezime))]
VH[, kriterijs := fifelse(zem_L2_10pp & L5_5pp, "abi", fifelse(zem_L2_10pp, "zem L2 ≥ 10 pp", "L5+ ≥ 5 pp"))]
VH[, pec_2012 := no >= 2012]
setorder(VH, joma, -logs, d_zem_L2)
cat("\nSaraksts:\n")
print(VH[, .(joma, valsts, no, uz, gadi, l_no = round(l_no), zem_L2 = sprintf("%.1f → %.1f", zem_L2_no, zem_L2_uz), L5 = sprintf("%.1f → %.1f", L5_no, L5_uz), kriterijs, piezime)], nrows = 200)
cat(sprintf("\nGadījumi ar sākumu 2012. gadā vai vēlāk: %d no %d (valstis: %s)\n", sum(VH$pec_2012), nrow(VH), paste(unique(VH[pec_2012 == TRUE, paste(valsts, no)]), collapse = "; ")))
fwrite(VH[, .(joma, valsts, no, uz, logs, gadi, l_no, zem_L2_no, zem_L2_uz, d_zem_L2, L5_no, L5_uz, d_L5, kriterijs, markets, pec_2012, piezime)], "output/pisa2025/merku_scenariji_vesture.csv", bom = TRUE)

# === 9. Tabula IZM un sasniedzamība ===========================================
sadala(9, "Tabula IZM: joma × mala × kontrolpunkts × pieci scenāriji (b, oficiālā skala, kopa: visi) un mērķu sasniedzamība")

IZM <- DAL[grepl("^b", cels) & kopa == "visi", .(joma, joma_nos = JOMAS[joma], skala, mala, mala_nos, gads, scenarijs, videjais, dala)]
IZM_W <- dcast(IZM, joma_nos + skala + mala_nos + gads ~ scenarijs, value.var = "dala", fun.aggregate = function(x) round(x[1], 1))
IZM_W[, joma_nos := factor(joma_nos, levels = JOMAS)]; setorder(IZM_W, joma_nos, skala, mala_nos, gads)
IZM_W <- merge(IZM_W, S0[raditajs %in% c("zem_L2", "L5"), .(joma_nos = JOMAS[joma], skala, mala_nos = fifelse(raditajs == "zem_L2", "zem 2. līmeņa, %", "5. un 6. līmenis, %"), dala_2025 = round(vertiba, 1))], by = c("joma_nos", "skala", "mala_nos"))
setcolorder(IZM_W, c("joma_nos", "skala", "mala_nos", "dala_2025", "gads"))
setorder(IZM_W, joma_nos, skala, mala_nos, gads)
cat("Galvenā tabula (daļas %, b ceļš, kopa: visi):\n"); print(IZM_W, nrows = 100)
fwrite(IZM_W, "output/pisa2025/merku_scenariji_izm.csv", bom = TRUE)

# Sasniedzamība: pirmais kontrolpunkts, kurā mērķis sasniegts; ekstrapolētais gads ar loga 3 ātrumu (lineāri, tikai orientējoši)
SAS <- list()
for (cl in c("b", "a")) for (j in names(JOMAS)) for (sk in SKALAS[[j]]) for (i in seq_len(nrow(MERKI))) {
  ml <- MERKI$mala[i]; m <- MERKI$merkis[i]
  for (sc in SCEN$scenarijs) {
    x <- DAL[joma == j & skala == sk & mala == ml & kopa == "visi" & scenarijs == sc & substr(cels, 1, 1) == cl][order(gads)]
    d0 <- sakums(j, sk, ml)
    sasn <- if (ml == "zem_L2") x$dala <= m else x$dala >= m
    pirmais <- if (any(sasn)) x$gads[which(sasn)[1]] else NA_integer_
    atrums <- (x[gads == 2035, dala] - d0) / 10
    vajag <- m - d0
    eks <- if (is.na(atrums) || atrums == 0 || sign(atrums) != sign(vajag)) NA_real_ else 2025 + vajag / atrums
    SAS[[length(SAS) + 1]] <- data.table(cels = cl, joma = j, joma_nos = JOMAS[[j]], skala = sk, mala = ml, merkis = m, merka_nos = MERKI$nosaukums[i], scenarijs = sc, dala_2025 = d0,
                                         d2029 = x[gads == 2029, dala], d2032 = x[gads == 2032, dala], d2035 = x[gads == 2035, dala],
                                         pirmais_kontrolpunkts = pirmais, atrums_pp_gada_2025_2035 = atrums,
                                         ekstrapoletais_gads = eks, verdikts = fifelse(!is.na(pirmais), sprintf("sasniedzams %d", pirmais), fifelse(is.na(eks), "nekad (virziens pretējs vai nulle)",
                                                                                 fifelse(eks <= 2035.5, sprintf("uz robežas ap %.0f", eks), fifelse(eks <= 2040, sprintf("pēc 2035 (ap %.0f)", eks), sprintf("tālu aiz horizonta (ap %.0f)", eks))))))
  }
}
SAS <- rbindlist(SAS)
cat("\nSasniedzamība (b ceļš, oficiālā skala):\n")
print(dcast(SAS[cels == "b" & skala == "oficiāli"], joma_nos + mala + merkis ~ scenarijs, value.var = "verdikts"))
cat("\nSasniedzamība (b ceļš, lasīšana koriģētā skala):\n")
print(dcast(SAS[cels == "b" & skala == "koriģēti"], joma_nos + mala + merkis ~ scenarijs, value.var = "verdikts"))
cat("\nSasniedzamība (a ceļš, oficiālā skala, pārbaude):\n")
print(dcast(SAS[cels == "a" & skala == "oficiāli"], joma_nos + mala + merkis ~ scenarijs, value.var = "verdikts"))
fwrite(SAS, "output/pisa2025/merku_scenariji_sasniedzamiba.csv", bom = TRUE)

# Ieteicamais komplekts: grīda = P25 scenārijs ("Slikts gadījums"), virziens = P75 ("Labākā ceturtdaļa")
cat("\nIeteicamais mērķu komplekts (oficiālā skala, b ceļš): grīda = zem kuras nedrīkst (P25 scenārijs), virziens = P75 scenārijs\n")
IK <- IZM[skala == "oficiāli" & scenarijs %in% c("Slikts gadījums", "Labākā ceturtdaļa", "Tipiska valsts")]
IK[, loma := fifelse(scenarijs == "Slikts gadījums", "grīda", fifelse(scenarijs == "Labākā ceturtdaļa", "virziens", "tipiski"))]
print(dcast(IK, joma_nos + mala_nos ~ loma + gads, value.var = "dala", fun.aggregate = function(x) round(x[1], 1)))
cat("\nVidējā grīda un virziens (oficiālā skala, kopa: visi):\n")
print(dcast(VS[skala == "oficiāli" & kopa == "visi" & scenarijs %in% c("Slikts gadījums", "Labākā ceturtdaļa")], joma + scenarijs ~ gads, value.var = "vertiba", fun.aggregate = function(x) round(x[1], 1)))

# === 10. Grafiks ================================================================
sadala(10, "Grafiks: vēsture 2003 līdz 2025 un scenāriju vēdeklis 2025 līdz 2035, trīs jomas × divas malas")

HIS <- AL[grupa == "A visi" & skala == "oficiāli" & raditajs %in% c("zem_L2", "L5"), .(joma, mala = raditajs, gads, dala = vertiba)]
FAN <- DB[skala == "oficiāli" & kopa == "visi", .(joma, mala, gads, scenarijs, dala)]
s25 <- S0[raditajs %in% c("zem_L2", "L5") & skala == "oficiāli", .(joma, mala = raditajs, gads = 2025L, dala = vertiba)]
s25 <- s25[rep(seq_len(.N), each = nrow(SCEN))][, scenarijs := rep(SCEN$scenarijs, times = .N / nrow(SCEN))]
FAN <- rbind(FAN, s25[, .(joma, mala, gads, scenarijs, dala)])
FANW <- dcast(FAN, joma + mala + gads ~ scenarijs, value.var = "dala")
setnames(FANW, SCEN$scenarijs, c("p10", "p25", "p50", "p75", "p95"))
lab_joma <- JOMAS; lab_mala <- c(zem_L2 = "Zem 2. līmeņa, %", L5 = "5. un 6. līmenis, %")
HIS[, `:=`(joma_f = factor(lab_joma[joma], levels = lab_joma), mala_f = factor(lab_mala[mala], levels = lab_mala))]
FANW[, `:=`(joma_f = factor(lab_joma[joma], levels = lab_joma), mala_f = factor(lab_mala[mala], levels = lab_mala))]
ML <- data.table(mala_f = factor(c(lab_mala[["zem_L2"]], lab_mala[["L5"]], lab_mala[["L5"]]), levels = lab_mala), merkis = c(15, 10, 12), nos = c("mērķis 15 %", "mērķis 10 %", "ES 12 %"))
ML <- ML[rep(seq_len(.N), each = 3)][, joma_f := factor(rep(lab_joma, 3), levels = lab_joma)]
LBL <- FANW[gads == 2035, .(joma_f, mala_f, gads, p10, p25, p50, p75, p95)]
LBL <- melt(LBL, id.vars = c("joma_f", "mala_f", "gads"), variable.name = "kv", value.name = "dala")
LBL[, nos := c(p10 = "P10 ļoti slikts", p25 = "P25 slikts", p50 = "P50 tipiska valsts", p75 = "P75 labākā ceturtdaļa", p95 = "P95 vēsturiskais maksimums")[as.character(kv)]]
g <- ggplot() +
  geom_ribbon(data = FANW, aes(x = gads, ymin = p10, ymax = p95), fill = "#9ecae1", alpha = 0.45) +
  geom_ribbon(data = FANW, aes(x = gads, ymin = p25, ymax = p75), fill = "#4292c6", alpha = 0.5) +
  geom_line(data = FANW, aes(x = gads, y = p50), colour = "#08306b", linewidth = 0.7, linetype = "22") +
  geom_line(data = FANW, aes(x = gads, y = p95), colour = "#08306b", linewidth = 0.4, linetype = "12") +
  geom_hline(data = ML, aes(yintercept = merkis), colour = "#b2182b", linewidth = 0.5) +
  geom_text(data = ML, aes(x = 2003, y = merkis, label = nos), colour = "#b2182b", size = 2.6, vjust = -0.4, hjust = 0) +
  geom_line(data = HIS, aes(x = gads, y = dala), colour = "#252525", linewidth = 0.8) +
  geom_point(data = HIS, aes(x = gads, y = dala), colour = "#252525", size = 1.6) +
  geom_text(data = LBL, aes(x = gads + 0.3, y = dala, label = nos), size = 2.2, hjust = 0, colour = "#08306b") +
  facet_grid(mala_f ~ joma_f, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2022, 2029, 2035), limits = c(2003, 2044), expand = c(0, 0)) +
  labs(title = "Latviešu valodas plūsma: vēsture 2003 līdz 2025 un scenāriju vēdeklis 2025 līdz 2035",
       subtitle = "Scenāriji: pārnese no vidējā (daļa ~ vidējais + vidējais², piesaistīta 2025. gada daļai), robežjosla valstīm ar sākuma līmeni ± 30; gaišā josla P10 līdz P95, tumšā P25 līdz P75, raustītā mediāna, punktētā P95",
       x = NULL, y = NULL, caption = "Oficiālā skala (bez 2018. gada instrumenta nobīdes korekcijas). Avots: OECD PISA 2025 I sējums B1 pielikums, 83. un 84. skripts.") +
  theme_minimal(base_size = 10) + theme(strip.placement = "outside", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank(), plot.title.position = "plot", plot.caption = element_text(hjust = 0))
ggsave("output/pisa2025/plots/84_merku_scenariji.png", g, width = 13, height = 7.5, dpi = 160, bg = "white")
cat("Saglabāts: output/pisa2025/plots/84_merku_scenariji.png\n")

# === 11. Kopsavilkums ==========================================================
sadala(11, "Kopsavilkums")
cat("Faili:\n  merku_scenariji_a_limeni.csv, merku_scenariji_videjie.csv, merku_scenariji_regresija.csv, merku_scenariji_dalas.csv,\n",
    " merku_scenariji_relativi.csv, merku_scenariji_relativi_kvantiles.csv, merku_scenariji_escs.csv, merku_scenariji_vesture.csv,\n",
    " merku_scenariji_izm.csv, merku_scenariji_sasniedzamiba.csv, plots/84_merku_scenariji.png\n")
cat(sprintf("\nIlgums: %.1f min\n", as.numeric(difftime(Sys.time(), t_sakums, units = "mins"))))

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
