# ============================================================================
# 68. Ilgtermiņa brīdinājuma pazīmes kā efekta lielumi
# ============================================================================
#
# JAUTĀJUMS (lietotājs, 2026-09-13): vai pazīmes, kas politikas veidotājiem bija jāredz
# pirms 2025. gada, izteiktas efekta lielumos, ir "gana būtiskas"? Starp posmiem efekti
# nav lieli, bet var būt "klīniski" nozīmīgi.
#
# MĒRVIENĪBAS
#   d   = starpība / SD (skolēnu līmeņa svērtā SD; PISA A plūsmai no ciklu vidējās SD,
#         IEA no Latvijas skolēnu PV). Procentilēm tā pati vienība (kvantiles nobīde SD).
#   h   = Koena h proporcijām: 2·asin√p₂ − 2·asin√p₁; SE ar delta metodi.
#   RR  = p₂/p₁ (relatīvais risks) daļām.
#   gadi = starpība / 20 punkti (OECD PISA 2022 I sēj., Box I.5.1: 20 punkti ≈ 15 gadīgo
#         vidējais gada mācību pieaugums); IEA skalām ≈ 0,24 SD (Bloom et al. 2008, 8.–9. kl.).
# SE: no izvades failiem; PISA starpciklu SE jau ietver saites kļūdu (55. skripts);
#     d SE = SE(Δ)/SD (SD nenoteiktība ignorēta).
#
# ETALONI (docs/63): Kraft 2020 (RCT: <0,05 mazs, 0,05–0,20 vidējs, ≥0,20 liels; mediāna
#   0,10); Bloom et al. 2008 (gada pieaugums lasīšanā 8→9 kl. 0,24; NAEP plaisas 0,45–0,83);
#   Cohen 1988 (0,2/0,5/0,8).
#
# Izvade: output/pisa2025/efekta_lielumi.csv, logs/68_efekta_lielumi_output.txt
# ============================================================================

suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/68_efekta_lielumi_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
options(width = 220)

GADS_PUNKTI <- 20      # OECD: 20 punkti ≈ viens mācību gads
BLOOM_8_9 <- 0.24      # gada pieaugums lasīšanā 8.–9. kl., SD
h_fun <- function(p1, p2) 2 * asin(sqrt(p2)) - 2 * asin(sqrt(p1))
h_se <- function(p1, s1, p2, s2) sqrt(s1^2 / (p1 * (1 - p1)) + s2^2 / (p2 * (1 - p2)))
klase <- function(d) { a <- abs(d); ifelse(a < 0.05, "Kraft: mazs", ifelse(a < 0.20, "Kraft: vidējs", "Kraft: liels")) }
rinda <- function(bloks, raditajs, no, lidz, vertiba_no, vertiba_lidz, starp, se, veids, es, es_se, gadi = NA_real_, piez = "")
  data.table(bloks, raditajs, no, lidz, vertiba_no = round(vertiba_no, 2), vertiba_lidz = round(vertiba_lidz, 2), starp = round(starp, 2),
             se = round(se, 2), z = round(starp / se, 2), veids, es = round(es, 3), es_se = round(es_se, 3),
             es_ti_zem = round(es - 1.96 * es_se, 3), es_ti_augs = round(es + 1.96 * es_se, 3), gadi = round(gadi, 2), klase = klase(es), piez)
R <- list()

# ---------------------------------------------------------------------------
# A. PISA, latviešu valodas plūsma: līmenis, augšgals, apakšgals
# ---------------------------------------------------------------------------
SD <- fread("output/pisa2025/a_plusmas_sd.csv")[, gads := as.character(gads)]
L <- fread("output/pisa2025/agrie_signali_limeni.csv", colClasses = c(gads = "character"))[grupa == "A visi"]
IZ <- fread("output/pisa2025/agrie_signali_izmainas.csv", colClasses = c(no = "character", lidz = "character"))[grupa == "A visi"]
psd <- function(g1, g2) sqrt((SD[gads == g1, sd_A]^2 + SD[gads == g2, sd_A]^2) / 2)
PARI <- list(c("2003", "2015"), c("2003", "2018"), c("2003", "2022"), c("2003", "2025"), c("2015", "2022"), c("2018", "2022"), c("2022", "2025"))
# Saites kļūdas pāriem, kuru nav 55. skripta izvadē: no vienotās tabulas (78. skripts; oficiālās, atvasinātās pret
# 2022 (docs/49) un pret 2025 (OECD 2025 Vol. I, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md)). Līdz 2026-09-13
# šeit bija 2003 -> 2018 = 5,39 (oficiālā ir 7,77) un 2003 -> 2025 = sqrt(5,30² + 3²) = 6,09 (tagad 5,363).
LE_TAB <- fread("output/pisa2025/saites_kludas.csv")[joma == "READ"]
LE_PAP <- setNames(LE_TAB$LE, paste(LE_TAB$no, LE_TAB$uz))
izm <- function(k, g1, g2) {
  x <- IZ[radītājs == k & no == g1 & lidz == g2]
  if (nrow(x)) return(x[, .(izm, se, le)])
  a <- L[gads == g1 & radītājs == k]; b <- L[gads == g2 & radītājs == k]; le <- LE_PAP[[paste(g1, g2)]]
  if (!nrow(a) || !nrow(b) || is.null(le)) return(NULL)
  data.table(izm = b$vertiba - a$vertiba, se = sqrt(a$se^2 + b$se^2 + (le * a$le_uz_punktu)^2), le = le)   # le_uz_punktu: LE mērogs daļām
}
for (p in PARI) {
  s <- psd(p[1], p[2])
  for (k in c("vid", "P90", "P95", "P10", "P50")) {
    x <- izm(k, p[1], p[2]); if (is.null(x)) next
    v1 <- L[gads == p[1] & radītājs == k, vertiba]; v2 <- L[gads == p[2] & radītājs == k, vertiba]
    R[[length(R) + 1]] <- rinda("A. PISA A plūsma", k, p[1], p[2], v1, v2, x$izm, x$se, "d (SD)", x$izm / s, x$se / s, x$izm / GADS_PUNKTI,
                                sprintf("SD %.0f; LE %.2f", s, x$le))
  }
  for (k in c("L5_un_virs", "L4_un_virs", "zem_L2")) {
    x <- izm(k, p[1], p[2]); if (is.null(x)) next
    a <- L[gads == p[1] & radītājs == k]; b <- L[gads == p[2] & radītājs == k]
    p1 <- a$vertiba / 100; p2 <- b$vertiba / 100
    R[[length(R) + 1]] <- rinda("A. PISA A plūsma", k, p[1], p[2], a$vertiba, b$vertiba, x$izm, x$se, "h (Koena)", h_fun(p1, p2), h_se(p1, a$se / 100, p2, b$se / 100),
                                NA, sprintf("RR %.2f; LE %.2f", p2 / p1, x$le))
  }
}

# ---------------------------------------------------------------------------
# B. Lasīšanas paradumi un grāmatas mājās (proporcijas)
# ---------------------------------------------------------------------------
P <- fread("output/pisa2025/paradumi.csv", colClasses = c(gads = "character"), encoding = "UTF-8")
par_h <- function(rad, gr, g1, g2, bloks) {
  a <- P[radītājs == rad & grupa == gr & gads == g1]; b <- P[radītājs == rad & grupa == gr & gads == g2]
  if (!nrow(a) || !nrow(b)) return(NULL)
  p1 <- a$pct / 100; p2 <- b$pct / 100; se <- sqrt(a$se^2 + b$se^2)
  rinda(bloks, paste0(rad, " [", gr, "]"), g1, g2, a$pct, b$pct, b$pct - a$pct, se, "h (Koena)", h_fun(p1, p2), h_se(p1, a$se / 100, p2, b$se / 100),
        NA, sprintf("RR %.2f", p2 / p1))
}
for (rad in c("nelasa priekam", "lasa priekam > 30 min dienā", "lasu tikai tad, ja jālasa", "lasīšana ir mans hobijs", "lasa daiļliteratūra vismaz vairākas reizes mēnesī")) {
  R[[length(R) + 1]] <- par_h(rad, "Latvija", "2000", "2009", "B. Paradumi")
  R[[length(R) + 1]] <- par_h(rad, "Latvija", "2000", "2018", "B. Paradumi")
  R[[length(R) + 1]] <- par_h(rad, "A", "2009", "2018", "B. Paradumi")
}
for (rad in c("grāmatas: vairāk nekā 200 (2000: >250)", "grāmatas: ne vairāk kā 10")) for (p in list(c("2003", "2012"), c("2003", "2018"), c("2003", "2025"), c("2018", "2025")))
  R[[length(R) + 1]] <- par_h(rad, "A", p[1], p[2], "B. Grāmatas")

# ---------------------------------------------------------------------------
# C. IEA: PIRLS latviešu valodas plūsma 2016 → 2021 un relatīvās vietas zudums 4. → 9. kl.
# ---------------------------------------------------------------------------
S <- readRDS("data/iea/processed/iea_skoleni.rds")
S[cnt == "LVA", lat := is.na(test_val) | test_val == "" | grepl("latv|<test language>", test_val, ignore.case = TRUE)]
wsd <- function(x, w) { i <- !is.na(x) & !is.na(w); m <- sum(w[i] * x[i]) / sum(w[i]); sqrt(sum(w[i] * (x[i] - m)^2) / sum(w[i])) }
iea_sd <- function(st_, g_, dm, tikai_A = TRUE) { d <- S[st == st_ & g == g_ & cnt == "LVA" & (!tikai_A | (lat & vm_ja %in% 1))]
  mean(sapply(1:5, function(i) wsd(d[[paste0("pv_", dm, i)]], d$w))) }
PI <- fread("output/pisa2025/latviesu_plusma_pirls.csv", colClasses = c(gads = "character"))
PP <- fread("output/pisa2025/latviesu_plusma_pirls_percentiles.csv", colClasses = c(gads = "character"))
sd_p <- sqrt((iea_sd("PIRLS", 2016, "las")^2 + iea_sd("PIRLS", 2021, "las")^2) / 2)
a <- PI[gads == "2016"]; b <- PI[gads == "2021"]
R[[length(R) + 1]] <- rinda("C. PIRLS A plūsma", "vid (4. kl.)", "2016", "2021", a$pamat, b$pamat, b$pamat - a$pamat, sqrt(a$pamat_se^2 + b$pamat_se^2), "d (SD)",
                            (b$pamat - a$pamat) / sd_p, sqrt(a$pamat_se^2 + b$pamat_se^2) / sd_p, (b$pamat - a$pamat) / sd_p / BLOOM_8_9,
                            sprintf("SD %.0f (LVA A skolēnu); gadi = d / 0,24 (Bloom 8.–9. kl.)", sd_p))
for (q in c(5, 10, 50, 90)) { a <- PP[gads == "2016" & percentile == q]; b <- PP[gads == "2021" & percentile == q]
  R[[length(R) + 1]] <- rinda("C. PIRLS A plūsma", paste0("P", q, " (4. kl.)"), "2016", "2021", a$aplese, b$aplese, b$aplese - a$aplese, sqrt(a$se^2 + b$se^2), "d (SD)",
                              (b$aplese - a$aplese) / sd_p, sqrt(a$se^2 + b$se^2) / sd_p, NA, sprintf("SD %.0f", sd_p)) }
a <- PI[gads == "2016"]; b <- PI[gads == "2021"]   # zem 400: SE nav failā; tuvinājums binomiāls
p1 <- a$zem400 / 100; p2 <- b$zem400 / 100; s1 <- sqrt(p1 * (1 - p1) / a$n) * 1.5; s2 <- sqrt(p2 * (1 - p2) / b$n) * 1.5
R[[length(R) + 1]] <- rinda("C. PIRLS A plūsma", "zem 400 (4. kl.)", "2016", "2021", a$zem400, b$zem400, b$zem400 - a$zem400, 100 * sqrt(s1^2 + s2^2), "h (Koena)",
                            h_fun(p1, p2), h_se(p1, s1, p2, s2), NA, sprintf("RR %.1f; SE binomiāls × 1,5 (dizaina efekts)", p2 / p1))
# Relatīvās vietas zudums 4. → 9. kl.: tā pati paaudze, fiksēta valstu kopa abos mērījumos (58b:
# lasīšanai 14 valstis, matemātikai 12). Vienība: valstu vidējo SD (z pa valstīm), nevis skolēnu SD.
# Skolēnu SD vienībās to nevar izteikt tieši, jo IEA un PISA valstu izkliede pret skolēnu izkliedi atšķiras.
KF <- fread("output/pisa2025/iea_kohortas_fiksets.csv")
for (i in seq_len(nrow(KF))) R[[length(R) + 1]] <- rinda("C. Relatīvā vieta 4. → 9. kl. (fiksēta kopa)", sprintf("%s → PISA %s", KF$iea[i], KF$pisa[i]),
  sub(".* ", "", KF$iea[i]), as.character(KF$pisa[i]), KF$z_iea_A[i], KF$z_pisa_A[i], KF$dz_A[i], NA, "Δz (valstu SD)", KF$dz_A[i], NA, NA,
  sprintf("%s; %d valstis; visa Latvija Δz %.2f", KF$joma[i], KF$kopa_n[i], KF$dz[i]))

# ---------------------------------------------------------------------------
# D. PIAAC 2023: latviešu valodas plūsmas jaunieši pret 7 valstīm (jau SD vienībās)
# ---------------------------------------------------------------------------
PA <- fread("output/pisa2025/piaac_paaudzes_pisa.csv")[joma == "LIT" & kopa == "7 valstis" & lv_grupa == "A"]
for (i in seq_len(nrow(PA))) R[[length(R) + 1]] <- rinda("D. PIAAC A plūsma pret 7 valstīm", paste("lasītprasme, vecums", PA$grupa[i]), "2023", "",
  NA, NA, PA$piaac_starp[i], PA$piaac_se[i], "d (SD)", PA$piaac_sd[i], PA$piaac_se[i] * PA$piaac_sd[i] / PA$piaac_starp[i], NA, sprintf("≈ PISA %s paaudze", PA$pisa_gads[i]))

# ---------------------------------------------------------------------------
E <- rbindlist(R); fwrite(E, "output/pisa2025/efekta_lielumi.csv")
cat(strrep("=", 120), "\nEfekta lielumi. es = d (SD) vai h; ti = 95 % intervāls; gadi = mācību gadi (PISA: 20 p.; PIRLS: 0,24 SD)\n", strrep("=", 120), "\n", sep = "")
for (b in unique(E$bloks)) { cat("\n", b, "\n", sep = ""); print(E[bloks == b, .(raditajs, no, lidz, vertiba_no, vertiba_lidz, starp, z, veids, es, es_ti_zem, es_ti_augs, gadi, klase, piez)], row.names = FALSE) }
cat("\n", strrep("=", 120), "\nEtaloni\n", strrep("=", 120), "\n", sep = "")
cat("  Kraft 2020 (1942 RCT efekti): <0,05 mazs, 0,05–0,20 vidējs, ≥0,20 liels; mediāna 0,10; P25 ≈ 0,00, P75 ≈ 0,25; lielās izlasēs (>2000) mediāna 0,03\n",
    "  Bloom et al. 2008: gada pieaugums lasīšanā 6–7 kl. 0,23, 7–8 0,26, 8–9 0,24, 9–10 0,19, 11–12 0,06; NAEP plaisas 8. kl.: melnādainie–baltie −0,80, maznodrošinātie −0,66, zēni–meitenes −0,28\n",
    "  OECD PISA 2022: 20 punkti ≈ gada mācību pieaugums 15 gadīgajiem; 10 punktu kritums lasīšanā ≈ pusgads\n",
    "  Cohen 1988: 0,2 mazs, 0,5 vidējs, 0,8 liels (vispārīgi, ne izglītības intervencijām)\n", sep = "")
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
