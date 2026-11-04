# ============================================================================
# 72. Koriģēto rādītāju gala tabula un koriģētā josla, A plūsma
# PISA datu analīze 2025 (docs/51, 5. posms; rezultāti docs/67)
# ============================================================================
#
# MĒRĶIS. Salikt latviešu valodas plūsmas (A plūsmas) rindu 2003–2025 ar
# visām korekcijām, kas docs/51 plānā ir gatavas, vienā tabulā un vienā
# grafikā. B plūsma (krievu valodas plūsma) netiek koriģēta (docs/51 lēmums).
#
# SLĀŅI (katrs atsevišķi, saskaitīti tikai tie, kas labo nobīdi)
#   (a) 2018. gada instrumenta nobīde (docs/55, korekcijas/07_msat.csv):
#       +9,6 punkti 2018., 2022. un 2025. gada līmeņiem. Zīmes konvencija:
#       "nobīde −9,6" (skala zemāka par trenda uzdevumu ainu) un "korekcija
#       +9,6" ir viens un tas pats. Vienmērīga pa spēju kvintilēm (docs/55
#       5. sad.), tāpēc attiecināma arī uz P10, P90 un daļu zem 2. līmeņa.
#       Statuss: DROŠA (identificēta datos, SE 2,0, nav EST un LTU).
#   (b) Iesaistes korekcija 2025 (docs/54, korekcijas/06_iesaiste.csv):
#       +1,9 punkti tikai vidējam; robežas 0 (oficiālā vērtēšana) līdz +9,1
#       (lielākais R3/R3M variants). Statuss: DROŠA virzienā, lielums ar
#       modeļa nenoteiktību; SE šeit = robežu platums / 3,92, tā ir modeļa,
#       ne izlases nenoteiktība. Uz P10, P90 un daļu zem 2. līmeņa nav
#       attiecināma vienmērīgi (ātrās atbildes koncentrējas vājos lasītājos).
#   (c) Sastāva standartizācija S2 un S3 (docs/52, sastava_standartizacija.csv):
#       NESUMMĒ ar (a) un (b), jo tā nelabo nobīdi, bet maina jautājumu
#       (docs/51 1. sad.). Rādīta atsevišķā kolonnā.
#   (d) Izslēgšanas nobīde pie rho = 0,5 (docs/50 6a, formula kā 48. skriptā):
#       HIPOTĒTISKA, jo rho nav identificējams; rādīta atsevišķi kā
#       "koriģēts ar hipotētisko slāni".
#   (e) Aptvērums 2025 (docs/51 8. solis): datu nav (IZM/VIIS), NAV IZDARĪTS.
#   (f) Testa valoda: attiecas tikai uz B plūsmu, ārpus tvēruma.
#
# SAITES KĻŪDAS. Visas no vienotās tabulas output/pisa2025/saites_kludas.csv
# (78. skripts): blakus cikliem oficiālās; pret 2025 atvasinātās no OECD 2025
# Vol. I B1 tabulām (2022 → 2025 1,094; 2018 → 2025 1,832; 2015 → 2025 3,791;
# 2003 → 2025 5,363; sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md; līdz
# 2026-09-13 šeit bija pagaidu 3 un sqrt(LE_2022² + 3²)). Salīdzinājumiem pret
# 2022 atvasinātās (docs/49 4.1): 2018 1,49; 2015 3,58; 2012 5,99; 2009 4,60;
# 2006 8,61; 2003 5,30. Oficiālā 1,47 un atvasinātā 1,49 pārim 2018–2022
# atšķiras par 0,02, z 2,31 abos gadījumos. Salīdzinājumā ar citām valstīm
# saites kļūda ir kopīga visām valstīm uz vienas skalas un starpībā izkrīt.
#
# SE. Līmeņiem pilnā Rubina dispersija ar BRR katrai PV (00_ab_dati.R).
# Koriģētā līmeņa SE = sqrt(SE_izlase^2 + SE_msat^2 + SE_iesaiste^2).
# Pārmaiņām SE = sqrt(V1 + V2 + LE^2 + korekciju dispersija, kas abos ciklos
# atšķiras). MSAT korekcija 2018–2025 pārmaiņās izkrīt, jo tā ir kopīga.
#
# Ievade:  00_ab_dati.R (mikrodati), output/pisa2025/korekcijas/06, 07,
#          sastava_standartizacija.csv, izlases_kvalitate.csv, a_plusmas_sd.csv,
#          saites_kludas_oficialas.csv, agrie_signali_limeni.csv (validācija),
#          salidzinajuma_kopa_valodas_plusmas.csv, iesaiste_josla.csv
# Izvade:  output/pisa2025/korigetie_raditaji.csv            gala tabula
#          output/pisa2025/korigetie_raditaji_slani.csv      katra korekcija
#          output/pisa2025/korigetie_raditaji_salidzinajums.csv  pret 7 valstīm
#          output/pisa2025/plots/72_korigeta_josla.png       grafiks, 200 dpi
#          output/pisa2025/logs/72_korigetie_raditaji_output.txt
# ============================================================================

# --- Darba direktorija -------------------------------------------------------
# Projekta skripti tiek palaisti no projekta saknes. Ja tā nav, mēģina to
# atrast no skripta atrašanās vietas (RStudio vai Rscript).
if (!file.exists("R/pisa2025/00_ab_dati.R")) {
  cels <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) NULL)
  if (is.null(cels) || !nzchar(cels)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) cels <- sub("^--file=", "", a[1])
  }
  if (!is.null(cels) && nzchar(cels)) setwd(file.path(dirname(cels), "..", ".."))
}
if (!file.exists("R/pisa2025/00_ab_dati.R")) stop("Palaid no projekta saknes mapes.")

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
for (m in c("output/pisa2025/logs", "output/pisa2025/plots")) dir.create(m, showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/72_korigetie_raditaji_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n\n")
options(width = 200)

# --- Pakotnes ----------------------------------------------------------------
# ggplot2 (un līdz ar to vctrs) drīkst pievienot tikai PĒC ab_dati(): ar
# ielādētu vctrs as.numeric() uz haven_labelled kolonnām 00_ab_dati.R nestrādā.
required_packages <- c("data.table", "ggplot2")
# system.file() pārbauda esamību, neielādējot pakotni (requireNamespace ielādētu vctrs).
for (pkg in required_packages) {
  if (!nzchar(system.file(package = pkg))) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}
suppressPackageStartupMessages(library(data.table))
source("R/pisa2025/00_ab_dati.R")
cat("Pakotnes pārbaudītas; ggplot2 tiek pievienots 6. sadaļā.\n\n")

sadala <- function(n, t) { cat(strrep("=", 78), "\n", n, ". ", t, "\n", strrep("=", 78), "\n\n", sep = "") }
zv <- function(z) ifelse(!is.na(z) & abs(z) > 1.96, "*", " ")
f1 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 1))
Z975 <- stats::qnorm(0.975)

GADI <- c("2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
MSAT_GADI <- c("2018", "2022", "2025")
LIM_L2 <- 407.47
wq1 <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }
STAT <- list(vid = VID,
             P10 = function(x, w) wq1(x, w, 0.10),
             P90 = function(x, w) wq1(x, w, 0.90),
             zem_L2 = function(x, w) 100 * sum(w * (x < LIM_L2)) / sum(w))
RAD <- names(STAT)
RAD_NOS <- c(vid = "vidējais", P10 = "P10", P90 = "P90", zem_L2 = "zem 2. līmeņa, %")

# === 1. NEAPSTRĀDĀTĀ A PLŪSMAS RINDA =========================================
sadala(1, "Neapstrādātā A plūsmas rinda 2003–2025 (pilnā Rubina dispersija, BRR katrai PV)")
D <- ab_dati()
A <- lapply(GADI, function(g) ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% "A")); names(A) <- GADI
rm(D)
# Statistika ar PV nobīdītām par `nob` punktiem (korekciju attiecināšanai uz sadalījumu)
stat_nob <- function(s, k, nob = 0) rubin_brr(lapply(s$pv, `+`, nob), s$sk$w, s$rw, STAT[[k]])
# Daļas jutīgums pret 1 punkta nobīdi (pp uz punktu), centrālā starpība, vidējota pa PV
blivums <- function(s, nob = 0) mean(vapply(s$pv, function(p)
  (STAT$zem_L2(p + nob - 1, s$sk$w) - STAT$zem_L2(p + nob + 1, s$sk$w)) / 2, numeric(1)))

R0 <- list()
for (g in GADI) for (k in RAD) {
  r <- stat_nob(A[[g]], k)
  R0[[length(R0) + 1]] <- data.frame(gads = g, raditajs = k, n = nrow(A[[g]]$sk),
    neapstr = r[["est"]], neapstr_se = r[["se"]],
    blivums = if (k == "zem_L2") blivums(A[[g]]) else 1, stringsAsFactors = FALSE)
}
R0 <- do.call(rbind, R0)

# Validācija pret 55. skripta izvadi (tas pats ielādētājs, tā pati formula)
L55 <- fread("output/pisa2025/agrie_signali_limeni.csv", colClasses = c(gads = "character"), encoding = "UTF-8")
L55 <- L55[grupa == "A visi" & `radītājs` %in% RAD]
v <- merge(R0, as.data.frame(L55)[, c("gads", "radītājs", "vertiba", "se")], by.x = c("gads", "raditajs"), by.y = c("gads", "radītājs"))
cat(sprintf("Validācija pret agrie_signali_limeni.csv: lielākā starpība aplēsē %.4f, SE %.4f (%d rindas)\n\n",
            max(abs(v$neapstr - v$vertiba)), max(abs(v$neapstr_se - v$se)), nrow(v)))
cat(sprintf("  %-5s %6s  %8s %6s  %8s %6s  %8s %6s  %8s %6s\n", "gads", "n", "vid", "SE", "P10", "SE", "P90", "SE", "zem L2", "SE"))
for (g in GADI) { x <- R0[R0$gads == g, ]; z <- function(k, c) x[x$raditajs == k, c]
  cat(sprintf("  %-5s %6d  %8.1f %6.2f  %8.1f %6.2f  %8.1f %6.2f  %8.1f %6.2f\n", g, z("vid", "n"),
              z("vid", "neapstr"), z("vid", "neapstr_se"), z("P10", "neapstr"), z("P10", "neapstr_se"),
              z("P90", "neapstr"), z("P90", "neapstr_se"), z("zem_L2", "neapstr"), z("zem_L2", "neapstr_se"))) }

# === 2. SAITES KĻŪDAS ========================================================
sadala(2, "Saites kļūdas")
LE_OF <- fread("output/pisa2025/saites_kludas.csv")[joma == "READ"]   # viens avots, 78. skripts
le_iepr <- function(g1, g2) { i <- which(LE_OF$no == as.numeric(g1) & LE_OF$uz == as.numeric(g2))
  if (length(i)) c(le = LE_OF$LE[i[1]], avots = LE_OF$veids[i[1]]) else c(le = 3, avots = "pagaidu 3") }
# Pret 2022: atvasinātās (docs/49 4.1: LE = sqrt(SE_OECD^2 - SE_mūsu^2) no tabulas I.B1.5.5), 2018 ar atvasināto 1,49
# (tabulā oficiālā 1,47); 2022 -> 2025 no tabulas (OECD 2025 Vol. I atvasinātā 1,094; līdz 2026-09-13 pagaidu 3)
LE_2022 <- c("2003" = 5.30, "2006" = 8.61, "2009" = 4.60, "2012" = 5.99, "2015" = 3.58, "2018" = 1.49, "2025" = as.numeric(le_iepr("2022", "2025")[["le"]]))
cat("  Blakus cikli (oficiālās; 2022 -> 2025 atvasinātā no OECD 2025 Vol. I):\n")
for (i in 1:7) { p <- le_iepr(GADI[i], GADI[i + 1]); cat(sprintf("    %s -> %s  LE %.3f  (%s)\n", GADI[i], GADI[i + 1], as.numeric(p[["le"]]), p[["avots"]])) }
cat("  Pret 2022 (atvasinātās, docs/49; 2025 no OECD 2025 Vol. I):\n   ",
    paste(sprintf("%s: %.2f", names(LE_2022), LE_2022), collapse = "  "), "\n")
cat("  Nesakritība 2018 -> 2022: oficiālā 1,47 (blakus ciklu pāriem, kā 55. skriptā) pret atvasināto 1,49\n",
    " (pret-2022 salīdzinājumiem). Starpība 0,02 punkti; A vidējā 2018 -> 2022 z ir 2,32 un 2,31, secinājums nemainās.\n\n")

# === 3. KOREKCIJU SLĀŅI ======================================================
sadala(3, "Korekciju slāņi")
SL <- list()
slanis <- function(gads, raditajs, korekcija, grupa, nobide, se, apaksa, augsa, statuss, avots, pienemumi)
  data.frame(gads = gads, raditajs = raditajs, korekcija = korekcija, grupa = grupa, nobide = nobide, se = se,
             apaksa = apaksa, augsa = augsa, statuss = statuss, avots = avots, pienemumi = pienemumi, stringsAsFactors = FALSE)

# (a) 2018. gada instrumenta nobīde ------------------------------------------
K7 <- fread("output/pisa2025/korekcijas/07_msat.csv", encoding = "UTF-8")
M_NOB <- K7[["nobīde"]][1]; M_LOW <- K7[["apakšējā"]][1]; M_HIGH <- K7[["augšējā"]][1]
M_SE <- (M_HIGH - M_LOW) / (2 * Z975)
cat(sprintf("(a) 2018. gada instrumenta nobīde: korekcija %+.2f (95 %%: %+.2f līdz %+.2f), SE %.2f; 2018, 2022, 2025.\n",
            M_NOB, M_LOW, M_HIGH, M_SE))
cat("    Zīmju konvencija: skalas nobīde −9,6 (2018. gada skala zemāka par trenda uzdevumu ainu) = līmeņa korekcija +9,6.\n")
cat("    Vidējam, P10 un P90 nobīde vienmērīga (docs/55: kvintilēs +6…+12); daļai zem 2. līmeņa pārrēķināta no PV + 9,6.\n\n")
MS <- list()
for (g in MSAT_GADI) for (k in RAD) {
  if (k == "zem_L2") {
    r1 <- stat_nob(A[[g]], k, M_NOB); d1 <- blivums(A[[g]], M_NOB); r0 <- R0[R0$gads == g & R0$raditajs == k, ]
    nob <- r1[["est"]] - r0$neapstr; se_k <- d1 * M_SE
    MS[[length(MS) + 1]] <- data.frame(gads = g, raditajs = k, kor_msat = nob, kor_msat_se = se_k,
                                       kor_msat_apaksa = -(d1 * M_HIGH), kor_msat_augsa = -(d1 * M_LOW),
                                       se_izlase_msat = r1[["se"]], blivums_msat = d1)
    pien <- sprintf("daļa zem 407,47 no PV + 9,62; jutīgums %.3f pp uz punktu", d1)
  } else {
    r0 <- R0[R0$gads == g & R0$raditajs == k, ]
    MS[[length(MS) + 1]] <- data.frame(gads = g, raditajs = k, kor_msat = M_NOB, kor_msat_se = M_SE,
                                       kor_msat_apaksa = M_LOW, kor_msat_augsa = M_HIGH,
                                       se_izlase_msat = r0$neapstr_se, blivums_msat = 1)
    pien <- "vienmērīga nobīde; docs/55 5. sad."
  }
  x <- MS[[length(MS)]]
  SL[[length(SL) + 1]] <- slanis(g, k, "2018. gada instrumenta nobīde", "mērījuma artefakts", x$kor_msat, x$kor_msat_se,
                                 x$kor_msat_apaksa, x$kor_msat_augsa, "droša", "docs/55; korekcijas/07_msat.csv", pien)
}
MS <- do.call(rbind, MS)
x <- MS[MS$raditajs == "zem_L2", ]
cat(sprintf("    Daļa zem 2. līmeņa: %s\n\n", paste(sprintf("%s %+.2f pp (SE %.2f)", x$gads, x$kor_msat, x$kor_msat_se), collapse = "; ")))

# (b) Iesaistes korekcija 2025 ------------------------------------------------
K6 <- fread("output/pisa2025/korekcijas/06_iesaiste.csv", encoding = "UTF-8")
k6 <- K6[grepl("R3M", korekcija)]
I_NOB <- k6[["nobīde"]][1]; I_LOW <- k6[["apakšējā"]][1]; I_HIGH <- k6[["augšējā"]][1]
I_SE <- (I_HIGH - I_LOW) / (2 * Z975)
J <- fread("output/pisa2025/iesaiste_josla.csv", encoding = "UTF-8")
j3 <- J[noteikums %in% c("R3", "R3M")]
cat(sprintf("(b) Iesaistes korekcija 2025 (R3M, 10 %%): %+.2f; robežas %+.2f līdz %+.2f; modeļa SE %.2f (robežas / 3,92).\n",
            I_NOB, I_LOW, I_HIGH, I_SE))
cat("    Robežu nesakritības skaidrojums (docs/62 8. sad. 11. p.): R3/R3M variantu korekcijas iesaiste_josla.csv ir\n")
cat(sprintf("    %+.2f līdz %+.2f (koriģētā pārmaiņa %+.1f līdz %+.1f); 52. skripts apakšējo robežu liek 0 (oficiālā vērtēšana),\n",
            min(j3$korekcija), max(j3$korekcija), min(j3$koriget_izm), max(j3$koriget_izm)))
cat("    tāpēc '0…+9' un '−18,5…−10,2' ir viena un tā pati josla, viena ar 0, otra bez. Abas ir pareizas.\n")
cat("    Tikai vidējam: R3M korekcija koncentrējas vājos lasītājos (docs/54 5. sad.), vienmērīga nobīde P10 un daļai\n",
    "   zem 2. līmeņa būtu nepareiza; sadalījuma rādītājiem šis slānis netiek piemērots.\n\n")
SL[[length(SL) + 1]] <- slanis("2025", "vid", "iesaiste (R3M, 10 %)", "mērījuma artefakts", I_NOB, I_SE, I_LOW, I_HIGH,
                               "droša (maza, lielums ar modeļa nenoteiktību)", "docs/54; korekcijas/06_iesaiste.csv",
                               "apakšējā 0 = oficiālā vērtēšana; augšējā = lielākais R3/R3M variants; SE ir modeļa josla / 3,92")
for (k in c("P10", "P90", "zem_L2"))
  SL[[length(SL) + 1]] <- slanis("2025", k, "iesaiste (R3M, 10 %)", "mērījuma artefakts", NA, NA, NA, NA,
                                 "nav piemērota", "docs/54", "korekcija nav vienmērīga pa sadalījumu; nav aplēsta sadalījuma rādītājiem")

# (c) Sastāva standartizācija (atsevišķi, nesummē) ---------------------------
SS <- unique(fread("output/pisa2025/sastava_standartizacija.csv", colClasses = c(gads = "character"), encoding = "UTF-8")[grupa == "A"])
cat("(c) Sastāva standartizācija (docs/52): S2 dzimums x klase visos ciklos; S3 + ESCS trenda skalā + IMMIG 2012–2022.\n",
    "   Nesummē ar (a) un (b): tā atbild uz citu jautājumu. Tikai vidējam.\n")
for (vv in c("S2", "S3")) for (i in which(SS$variants == vv)) {
  SL[[length(SL) + 1]] <- slanis(SS$gads[i], "vid", paste("sastāvs", vv), "sastāva standartizācija", SS$nobide[i], NA, NA, NA,
                                 "cits jautājums, nesummē", "docs/52; sastava_standartizacija.csv",
                                 if (vv == "S2") "atsauce: 2003–2025 vidējais dzimuma x klases sadalījums" else "atsauce: 2012–2022 vidējais; ESCS trenda skalā")
}
cat(sprintf("    S2 nobīdes: %s\n", paste(sprintf("%s %+.1f", SS$gads[SS$variants == "S2"], SS$nobide[SS$variants == "S2"]), collapse = "; ")))
cat(sprintf("    S3 nobīdes: %s\n\n", paste(sprintf("%s %+.1f", SS$gads[SS$variants == "S3"], SS$nobide[SS$variants == "S3"]), collapse = "; ")))

# (d) Izslēgšanas nobīde pie rho = 0,5 (hipotētiska) -------------------------
Q <- fread("output/pisa2025/izlases_kvalitate.csv", encoding = "UTF-8")
EXCL <- setNames(Q[["izslēgšana_kopā_pct"]] / 100, as.character(Q$gads))
E25 <- c(0.0321, 0.0507)
SDA <- fread("output/pisa2025/a_plusmas_sd.csv")[, gads := as.character(gads)]
nobide_izsl <- function(e, rho, s) rho * s * stats::dnorm(stats::qnorm(e)) / (1 - e)
cat("(d) Izslēgšanas nobīde (docs/50 6a): līmenis pārvērtēts par rho * SD * phi(Phi^-1(e)) / (1 - e); rho = 0,5 (OECD piemērs),\n",
    "   rho = 0 kā apakšējā robeža; 2025 izslēgšana nav publicēta, diapazons 3,21–5,07 %. HIPOTĒTISKA, rho nav identificējams.\n")
IZ <- list()
for (g in GADI) {
  s <- SDA[gads == g, sd_A]
  if (g == "2025") { e <- mean(E25); lo <- -nobide_izsl(max(E25), 0.5, s); e_txt <- "3,21–5,07 (pieņemts)" }
  else { e <- EXCL[[g]]; lo <- -nobide_izsl(e, 0.5, s); e_txt <- sprintf("%.2f", 100 * e) }
  nob <- -nobide_izsl(e, 0.5, s)
  IZ[[length(IZ) + 1]] <- data.frame(gads = g, kor_izsl = nob, kor_izsl_apaksa = lo, kor_izsl_augsa = 0, izsl_pct = 100 * e)
  SL[[length(SL) + 1]] <- slanis(g, "vid", "izslēgšana, rho = 0,5", "izlases nobīde", nob, NA, lo, 0, "hipotētiska",
                                 "docs/50 6a; izlases_kvalitate.csv; formula kā 48. skriptā",
                                 sprintf("izslēgšana %s %%, SD_A %.1f; apakšējā = rho 0,5 (2025: pie 5,07 %%), augšējā = rho 0", e_txt, s))
  cat(sprintf("    %s izslēgšana %-22s nobīde %+.1f\n", g, e_txt, nob))
}
IZ <- do.call(rbind, IZ)
cat("\n(e) Aptvērums 2025 (docs/51 8. solis, indekss 0,81): datu par neaptvertajiem nav (IZM/VIIS), korekcija NAV IZDARĪTA.\n",
    "   Virziens: ja neaptvertie ir vājāki, 2025. gada līmenis ir pārvērtēts un kritums lielāks.\n")
cat("(f) Testa valoda: tikai B plūsmai, ārpus tvēruma.\n\n")
SL <- do.call(rbind, SL)
fwrite(SL, "output/pisa2025/korigetie_raditaji_slani.csv", bom = TRUE)

# === 4. GALA TABULA ==========================================================
sadala(4, "Gala tabula: līmeņi, korekcijas, pārmaiņas")
T <- merge(R0, MS, by = c("gads", "raditajs"), all.x = TRUE)
T$kor_msat[is.na(T$kor_msat)] <- 0; T$kor_msat_se[is.na(T$kor_msat_se)] <- 0
T$se_izlase_msat <- ifelse(is.na(T$se_izlase_msat), T$neapstr_se, T$se_izlase_msat)
T$kor_ies <- ifelse(T$gads == "2025" & T$raditajs == "vid", I_NOB, 0)
T$kor_ies_se <- ifelse(T$gads == "2025" & T$raditajs == "vid", I_SE, 0)
T$kor_ies_apaksa <- ifelse(T$gads == "2025" & T$raditajs == "vid", I_LOW, NA)
T$kor_ies_augsa <- ifelse(T$gads == "2025" & T$raditajs == "vid", I_HIGH, NA)
T$korigets <- T$neapstr + T$kor_msat + T$kor_ies
T$korigets_se_izlase <- T$se_izlase_msat
T$korigets_se <- sqrt(T$korigets_se_izlase^2 + T$kor_msat_se^2 + T$kor_ies_se^2)
T$korigets_ti_zem <- T$korigets - Z975 * T$korigets_se
T$korigets_ti_augs <- T$korigets + Z975 * T$korigets_se
T$neapstr_ti_zem <- T$neapstr - Z975 * T$neapstr_se
T$neapstr_ti_augs <- T$neapstr + Z975 * T$neapstr_se
# Hipotētiskais slānis (tikai vidējam) un sastāva standartizācija (tikai vidējam)
T <- merge(T, IZ[, c("gads", "kor_izsl", "kor_izsl_apaksa")], by = "gads", all.x = TRUE)
T$kor_izsl[T$raditajs != "vid"] <- NA; T$kor_izsl_apaksa[T$raditajs != "vid"] <- NA
T$korigets_hip <- T$korigets + T$kor_izsl
s2 <- SS[variants == "S2", .(gads, sastavs_S2 = nobide, standartizets_S2 = vid_kor, standartizets_S2_se = se_kor)]
s3 <- SS[variants == "S3", .(gads, sastavs_S3 = nobide, standartizets_S3 = vid_kor, standartizets_S3_se = se_kor)]
T <- merge(T, as.data.frame(s2), by = "gads", all.x = TRUE); T <- merge(T, as.data.frame(s3), by = "gads", all.x = TRUE)
for (k in c("sastavs_S2", "standartizets_S2", "standartizets_S2_se", "sastavs_S3", "standartizets_S3", "standartizets_S3_se"))
  T[[k]][T$raditajs != "vid"] <- NA
T <- T[order(match(T$raditajs, RAD), T$gads), ]

# Pārmaiņas. Korekciju dispersija tiek pievienota tikai tad, ja korekcija abos ciklos atšķiras.
parmaina <- function(g1, g2, k, le) {
  a <- T[T$gads == g1 & T$raditajs == k, ]; b <- T[T$gads == g2 & T$raditajs == k, ]
  sk <- if (k == "zem_L2") mean(c(a$blivums, b$blivums)) else 1
  v_le <- (le * sk)^2
  v_kor <- 0
  if (xor(g1 %in% MSAT_GADI, g2 %in% MSAT_GADI)) v_kor <- v_kor + (if (g2 %in% MSAT_GADI) b$kor_msat_se else a$kor_msat_se)^2
  if (xor(g1 == "2025", g2 == "2025")) v_kor <- v_kor + (if (g2 == "2025") b$kor_ies_se else a$kor_ies_se)^2
  d0 <- b$neapstr - a$neapstr; s0 <- sqrt(a$neapstr_se^2 + b$neapstr_se^2 + v_le)
  d1 <- b$korigets - a$korigets; s1 <- sqrt(a$korigets_se_izlase^2 + b$korigets_se_izlase^2 + v_le + v_kor)
  list(izm_neapstr = d0, izm_neapstr_se = s0, izm_neapstr_z = d0 / s0, izm_korig = d1, izm_korig_se = s1, izm_korig_z = d1 / s1,
       le = le, var_izlase = a$korigets_se_izlase^2 + b$korigets_se_izlase^2, var_kor = v_kor,
       var_izlase_neapstr = a$neapstr_se^2 + b$neapstr_se^2)
}
for (nm in c("iepr_no", "iepr_le", "iepr_izm_neapstr", "iepr_izm_neapstr_se", "iepr_izm_neapstr_z", "iepr_izm_korig", "iepr_izm_korig_se", "iepr_izm_korig_z",
             "p2022_le", "p2022_izm_neapstr", "p2022_izm_neapstr_se", "p2022_izm_neapstr_z", "p2022_izm_korig", "p2022_izm_korig_se", "p2022_izm_korig_z"))
  T[[nm]] <- NA_real_
T$iepr_no <- NA_character_
for (i in seq_len(nrow(T))) {
  g <- T$gads[i]; k <- T$raditajs[i]; j <- match(g, GADI)
  if (j > 1) { g1 <- GADI[j - 1]; le <- as.numeric(le_iepr(g1, g)[["le"]]); p <- parmaina(g1, g, k, le)
    T$iepr_no[i] <- g1; T$iepr_le[i] <- le
    T$iepr_izm_neapstr[i] <- p$izm_neapstr; T$iepr_izm_neapstr_se[i] <- p$izm_neapstr_se; T$iepr_izm_neapstr_z[i] <- p$izm_neapstr_z
    T$iepr_izm_korig[i] <- p$izm_korig; T$iepr_izm_korig_se[i] <- p$izm_korig_se; T$iepr_izm_korig_z[i] <- p$izm_korig_z }
  if (g != "2022") { le <- LE_2022[[g]]; p <- parmaina("2022", g, k, le)
    T$p2022_le[i] <- le
    T$p2022_izm_neapstr[i] <- p$izm_neapstr; T$p2022_izm_neapstr_se[i] <- p$izm_neapstr_se; T$p2022_izm_neapstr_z[i] <- p$izm_neapstr_z
    T$p2022_izm_korig[i] <- p$izm_korig; T$p2022_izm_korig_se[i] <- p$izm_korig_se; T$p2022_izm_korig_z[i] <- p$izm_korig_z }
}
T$raditajs_nos <- RAD_NOS[T$raditajs]
KOL <- c("raditajs", "raditajs_nos", "gads", "n", "neapstr", "neapstr_se", "neapstr_ti_zem", "neapstr_ti_augs",
         "kor_msat", "kor_msat_se", "kor_msat_apaksa", "kor_msat_augsa", "kor_ies", "kor_ies_se", "kor_ies_apaksa", "kor_ies_augsa",
         "korigets", "korigets_se_izlase", "korigets_se", "korigets_ti_zem", "korigets_ti_augs",
         "kor_izsl", "kor_izsl_apaksa", "korigets_hip", "sastavs_S2", "standartizets_S2", "standartizets_S2_se",
         "sastavs_S3", "standartizets_S3", "standartizets_S3_se", "blivums",
         "iepr_no", "iepr_le", "iepr_izm_neapstr", "iepr_izm_neapstr_se", "iepr_izm_neapstr_z", "iepr_izm_korig", "iepr_izm_korig_se", "iepr_izm_korig_z",
         "p2022_le", "p2022_izm_neapstr", "p2022_izm_neapstr_se", "p2022_izm_neapstr_z", "p2022_izm_korig", "p2022_izm_korig_se", "p2022_izm_korig_z")
T <- T[, KOL]
fwrite(T, "output/pisa2025/korigetie_raditaji.csv", bom = TRUE)

cat("Kolonnas: neapstr = oficiālā skala; kor_msat = 2018. gada nobīde (+9,6 no 2018); kor_ies = iesaiste (2025 vid);\n",
    "korigets = neapstr + kor_msat + kor_ies (drošie slāņi); korigets_hip = + izslēgšana rho 0,5 (hipotētisks);\n",
    "standartizets_S2/S3 = sastāva standartizācija (cits jautājums, nesummē). SE pārmaiņām ietver LE.\n\n")
for (k in RAD) {
  cat(sprintf("--- %s ---\n", RAD_NOS[[k]]))
  cat(sprintf("  %-5s %8s %6s | %6s %6s | %8s %6s  %15s | %8s | %8s | %-22s | %-22s\n", "gads", "neapstr", "SE", "MSAT", "ies.",
              "korig.", "SE", "95 % josla", "hipot.", "S2", "pret iepr. neapstr (SE)", "pret iepr. korig (SE)"))
  x <- T[T$raditajs == k, ]
  for (i in seq_len(nrow(x))) with(x[i, ], cat(sprintf("  %-5s %8.1f %6.2f | %6s %6s | %8.1f %6.2f  %7.1f…%-7.1f | %8s | %8s | %-22s | %-22s\n",
    gads, neapstr, neapstr_se, ifelse(kor_msat == 0, "", sprintf("%+.1f", kor_msat)), ifelse(kor_ies == 0, "", sprintf("%+.1f", kor_ies)),
    korigets, korigets_se, korigets_ti_zem, korigets_ti_augs, f1(korigets_hip), f1(standartizets_S2),
    ifelse(is.na(iepr_izm_neapstr), "", sprintf("%+.1f (%.1f)%s", iepr_izm_neapstr, iepr_izm_neapstr_se, zv(iepr_izm_neapstr_z))),
    ifelse(is.na(iepr_izm_korig), "", sprintf("%+.1f (%.1f)%s", iepr_izm_korig, iepr_izm_korig_se, zv(iepr_izm_korig_z))))))
  cat("\n")
}
cat("Pārmaiņas pret 2022 (vidējais), atvasinātā LE:\n")
x <- T[T$raditajs == "vid" & T$gads != "2022", ]
for (i in seq_len(nrow(x))) with(x[i, ], cat(sprintf("  2022 -> %s  LE %.2f  neapstrādāti %+6.1f (%.1f)%s   koriģēti %+6.1f (%.1f)%s\n",
  gads, p2022_le, p2022_izm_neapstr, p2022_izm_neapstr_se, zv(p2022_izm_neapstr_z), p2022_izm_korig, p2022_izm_korig_se, zv(p2022_izm_korig_z))))

# Garākie periodi vidējam (tekstam docs/67)
cat("\nGarākie periodi, vidējais (LE no saites_kludas.csv; pret 2025 OECD 2025 Vol. I atvasinātās, līdz 2026-09-13 sqrt(LE_2022^2 + 3^2)):\n")
le_g <- function(g1, g2) c(g1, g2, as.numeric(le_iepr(g1, g2)[["le"]]))
LE_GAR <- list(le_g("2015", "2018"), le_g("2018", "2022"), le_g("2022", "2025"), le_g("2015", "2022"), le_g("2018", "2025"),
               le_g("2015", "2025"), le_g("2003", "2025"), le_g("2003", "2018"), le_g("2012", "2018"))
GAR <- list()
for (p in LE_GAR) for (k in RAD) { r <- parmaina(p[1], p[2], k, as.numeric(p[3]))
  GAR[[length(GAR) + 1]] <- data.frame(raditajs = k, no = p[1], lidz = p[2], le = as.numeric(p[3]), izm_neapstr = r$izm_neapstr, izm_neapstr_se = r$izm_neapstr_se,
    izm_korig = r$izm_korig, izm_korig_se = r$izm_korig_se, var_izlase = r$var_izlase, var_kor = r$var_kor, var_izlase_neapstr = r$var_izlase_neapstr) }
GAR <- do.call(rbind, GAR)
x <- GAR[GAR$raditajs == "vid", ]
for (i in seq_len(nrow(x))) with(x[i, ], cat(sprintf("  %s -> %s  LE %.2f  neapstrādāti %+6.1f (%.1f)%s   koriģēti %+6.1f (%.1f)%s\n",
  no, lidz, le, izm_neapstr, izm_neapstr_se, zv(izm_neapstr / izm_neapstr_se), izm_korig, izm_korig_se, zv(izm_korig / izm_korig_se))))
fwrite(GAR, "output/pisa2025/korigetie_raditaji_periodi.csv", bom = TRUE)

# === 5. SALĪDZINĀJUMS AR 7 VALSTĪM ===========================================
sadala(5, "Latvijai specifiskā daļa pēc korekcijām: pret 7 salīdzinājuma valstīm, abas kopas")
SK <- fread("output/pisa2025/salidzinajuma_kopa_valodas_plusmas.csv", colClasses = c(gads = "character"), encoding = "UTF-8")
SK <- SK[!grepl("LVA", cnt)]
KOPAS <- unique(SK$kopa)
cat("Kopas:", paste(KOPAS, collapse = "; "), "\n")
cat("Valstis pa gadiem:", paste(sapply(GADI, function(g) sprintf("%s: %d", g, length(unique(SK[gads == g & kopa == KOPAS[1], cnt])))), collapse = "; "), "\n")
cat("Saites kļūda relatīvajās pārmaiņās izkrīt (kopīga visām valstīm uz vienas skalas); SE ietver A izlases un korekciju\n",
    "dispersiju un 7 valstu vidējā izlases dispersiju. 2003. gadā tikai 5 valstis (bez EST, LTU).\n\n")
K7 <- list()
for (kp in KOPAS) for (g in GADI) { x <- SK[kopa == kp & gads == g]
  K7[[length(K7) + 1]] <- data.frame(kopa = kp, gads = g, n_valstis = nrow(x), vid7 = mean(x$vid), vid7_se = sqrt(sum(x$se^2)) / nrow(x)) }
K7 <- do.call(rbind, K7)
PER <- list(c("2015", "2018", 3.93), c("2018", "2022", 1.47), c("2022", "2025", 3), c("2018", "2025", sqrt(1.49^2 + 9)),
            c("2015", "2025", sqrt(3.58^2 + 9)), c("2003", "2025", sqrt(5.30^2 + 9)))
REL <- list()
cat(sprintf("  %-22s %-12s %8s %8s %8s | %-14s | %-14s\n", "kopa", "periods", "A neap.", "A kor.", "7 valst.", "rel. neap. (SE)", "rel. kor. (SE)"))
for (kp in KOPAS) for (p in PER) {
  c1 <- SK[kopa == kp & gads == p[1]]; c2 <- SK[kopa == kp & gads == p[2]]; cc <- intersect(c1$cnt, c2$cnt)
  c1 <- c1[cnt %in% cc]; c2 <- c2[cnt %in% cc]; c2 <- c2[match(c1$cnt, cnt)]
  d7 <- mean(c2$vid - c1$vid); s7 <- sqrt(sum(c1$se^2 + c2$se^2)) / length(cc)
  r <- parmaina(p[1], p[2], "vid", as.numeric(p[3]))
  rel0 <- r$izm_neapstr - d7; se0 <- sqrt(r$var_izlase_neapstr + s7^2)
  rel1 <- r$izm_korig - d7; se1 <- sqrt(r$var_izlase + r$var_kor + s7^2)
  REL[[length(REL) + 1]] <- data.frame(kopa = kp, no = p[1], lidz = p[2], n_valstis = length(cc), valstis = paste(sort(cc), collapse = " "),
    A_neapstr = r$izm_neapstr, A_neapstr_se_le = r$izm_neapstr_se, A_korig = r$izm_korig, A_korig_se_le = r$izm_korig_se,
    valstis7 = d7, valstis7_se = s7, rel_neapstr = rel0, rel_neapstr_se = se0, rel_neapstr_z = rel0 / se0,
    rel_korig = rel1, rel_korig_se = se1, rel_korig_z = rel1 / se1)
  cat(sprintf("  %-22s %s -> %s %+8.1f %+8.1f %+8.1f | %+6.1f (%.1f)%s | %+6.1f (%.1f)%s\n", kp, p[1], p[2], r$izm_neapstr, r$izm_korig, d7,
              rel0, se0, zv(rel0 / se0), rel1, se1, zv(rel1 / se1)))
}
REL <- do.call(rbind, REL)
fwrite(REL, "output/pisa2025/korigetie_raditaji_salidzinajums.csv", bom = TRUE)
cat("\n  Piezīme: 7 valstu vidējais nav koriģēts par to izslēgšanu, instrumentu vai iesaisti; korekcijas ir tikai A plūsmas pusē.\n\n")

# === 6. GRAFIKS ==============================================================
sadala(6, "Grafiks: neapstrādātā un koriģētā A plūsmas rinda ar 95 % joslām, 7 valstu vidējais fonā")
suppressPackageStartupMessages(library(ggplot2))
V <- T[T$raditajs == "vid", ]
G <- rbind(
  data.frame(serija = "A plūsma, neapstrādāta", gads = as.numeric(V$gads), y = V$neapstr, lo = V$neapstr_ti_zem, hi = V$neapstr_ti_augs),
  data.frame(serija = "A plūsma, koriģēta", gads = as.numeric(V$gads), y = V$korigets, lo = V$korigets_ti_zem, hi = V$korigets_ti_augs),
  data.frame(serija = "7 valstis, visi skolēni", gads = as.numeric(K7$gads[K7$kopa == "visi skolēni"]), y = K7$vid7[K7$kopa == "visi skolēni"], lo = NA, hi = NA),
  data.frame(serija = "7 valstis, galvenā mājas valoda", gads = as.numeric(K7$gads[K7$kopa == "galvenā valoda"]), y = K7$vid7[K7$kopa == "galvenā valoda"], lo = NA, hi = NA))
LIM_S <- c("A plūsma, neapstrādāta", "A plūsma, koriģēta", "7 valstis, visi skolēni", "7 valstis, galvenā mājas valoda")
G$serija <- factor(G$serija, levels = LIM_S)
KR <- c("#898781", "#2a78d6", "#eb6834", "#0f8a6e"); names(KR) <- LIM_S
LT <- c("solid", "solid", "dotted", "dashed"); names(LT) <- LIM_S
SH <- c(21, 19, 17, 15); names(SH) <- LIM_S
lab_end <- G[G$gads == 2025, ]; lab_end$lab <- c("neapstrādāta", "koriģēta", "7 valstis, visi", "7 valstis, galv. valoda")[match(lab_end$serija, LIM_S)]
# Tiešās etiķetes labajā malā atbīda, ja divas sērijas 2025. gadā ir tuvāk par 4 punktiem
lab_end <- lab_end[order(lab_end$y), ]
for (i in seq_len(nrow(lab_end))[-1]) if (lab_end$y[i] - lab_end$y[i - 1] < 4) lab_end$y[i] <- lab_end$y[i - 1] + 4
v18 <- V[V$gads == "2018", ]; v25 <- V[V$gads == "2025", ]
ANOT <- data.frame(
  x = c(2019.6, 2023.3), y = c(max(G$y, na.rm = TRUE) + 3, min(G$lo, na.rm = TRUE) - 1),
  lab = c(sprintf("2018–2025: +%s instrumenta nobīde\n(docs/55; droša, SE %s)", gsub("\\.", ",", sprintf("%.1f", M_NOB)), gsub("\\.", ",", sprintf("%.1f", M_SE))),
          sprintf("2025: +%s iesaiste\n(docs/54; robežas 0…+%s)", gsub("\\.", ",", sprintf("%.1f", I_NOB)), gsub("\\.", ",", sprintf("%.0f", I_HIGH)))))
pl <- ggplot() +
  annotate("rect", xmin = 2017.4, xmax = 2025.6, ymin = -Inf, ymax = Inf, fill = "#2a78d6", alpha = 0.05) +
  geom_ribbon(data = G[G$serija == "A plūsma, neapstrādāta", ], aes(gads, ymin = lo, ymax = hi), fill = "#898781", alpha = 0.22) +
  geom_ribbon(data = G[G$serija == "A plūsma, koriģēta", ], aes(gads, ymin = lo, ymax = hi), fill = "#2a78d6", alpha = 0.18) +
  geom_line(data = G, aes(gads, y, colour = serija, linetype = serija), linewidth = 0.9) +
  geom_point(data = G, aes(gads, y, colour = serija, shape = serija), size = 2.6, fill = "white", stroke = 0.9) +
  geom_segment(data = data.frame(x = 2018, y1 = v18$neapstr, y2 = v18$korigets), aes(x = x, xend = x, y = y1, yend = y2),
               colour = "#2a78d6", arrow = arrow(length = unit(0.18, "cm")), linewidth = 0.5) +
  geom_segment(data = data.frame(x = 2025, y1 = v25$neapstr + M_NOB, y2 = v25$korigets), aes(x = x, xend = x, y = y1, yend = y2),
               colour = "#2a78d6", arrow = arrow(length = unit(0.14, "cm")), linewidth = 0.5) +
  geom_label(data = ANOT, aes(x, y, label = lab), size = 2.9, hjust = c(0.5, 0.5), vjust = c(0, 1), linewidth = 0, fill = "white", alpha = 0.9, colour = "#1d4f8f", lineheight = 0.9) +
  geom_text(data = lab_end, aes(2025.35, y, label = lab, colour = serija), hjust = 0, size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = KR, name = NULL) + scale_linetype_manual(values = LT, name = NULL) + scale_shape_manual(values = SH, name = NULL) +
  scale_x_continuous("PISA cikls", breaks = as.numeric(GADI), limits = c(2002.5, 2029)) +
  scale_y_continuous("Lasītprasme, PISA punkti") +
  labs(title = "Latviešu valodas plūsmas lasītprasme 2003–2025: neapstrādātā un koriģētā rinda",
       subtitle = paste0("Joslas = 95 % intervāls (koriģētajai ietver arī korekciju SE). Koriģētā = oficiālā + 2018. gada instrumenta nobīde (+",
                         gsub("\\.", ",", sprintf("%.1f", M_NOB)), ", 2018–2025) + iesaiste (+", gsub("\\.", ",", sprintf("%.1f", I_NOB)),
                         ", tikai 2025).\nSastāva standartizācija un izslēgšanas jutīgums nav iekļauti (docs/67). 7 valstis: EST, LTU, POL, FIN, CZE, HUN, DEU (2003: bez EST, LTU), bez korekcijām."),
       caption = "Avots: R/pisa2025/72_korigetie_raditaji.R; docs/51, 54, 55, 67. Saites kļūda joslās nav (tā attiecas uz starpciklu pārmaiņām, sk. tabulu).") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey40", hjust = 0), legend.position = "bottom", plot.subtitle = element_text(size = 9.5, lineheight = 1.05))
ggsave("output/pisa2025/plots/72_korigeta_josla.png", pl, width = 11.5, height = 7, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/72_korigeta_josla.png\n\n")

# === 7. KOPSAVILKUMS =========================================================
sadala(7, "Kopsavilkums")
v <- T[T$raditajs == "vid", ]
cat("A plūsmas vidējais (neapstrādāts -> koriģēts, SE):\n")
for (i in seq_len(nrow(v))) cat(sprintf("  %s  %6.1f -> %6.1f (%.2f)\n", v$gads[i], v$neapstr[i], v$korigets[i], v$korigets_se[i]))
cat("\nKoriģētās pārmaiņas vidējam: ", paste(sprintf("%s -> %s %+.1f (%.1f)%s", v$iepr_no[-1], v$gads[-1], v$iepr_izm_korig[-1], v$iepr_izm_korig_se[-1],
    zv(v$iepr_izm_korig_z[-1])), collapse = "; "), "\n", sep = "")
cat("\nIzveidotie faili:\n  output/pisa2025/korigetie_raditaji.csv\n  output/pisa2025/korigetie_raditaji_slani.csv\n",
    " output/pisa2025/korigetie_raditaji_periodi.csv\n  output/pisa2025/korigetie_raditaji_salidzinajums.csv\n",
    " output/pisa2025/plots/72_korigeta_josla.png\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
