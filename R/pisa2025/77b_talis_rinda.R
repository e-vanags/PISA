# ============================================================================
# 77b. Slānis 5, 2. kārta. Skolotāju puses simptomu rinda: TALIS 2013 → 2018 → 2024
#      (ISCED 2). Latvija pret salīdzinājuma valstīm katrā ciklā.
# PISA datu analīze 2025
# ============================================================================
#
# Dati: OECD, TALIS 2013/2018/2024 Database.
#   2013: data/TALIS/2013/raw/BTGINTT2.sav (skolotāji), BCGINTT2.sav (direktori)
#   2018: data/TALIS/2018/raw/BTGINTT3.sav, BCGINTT3.sav
#   2024: data/TALIS/2024/raw/btgintt4.sav, bcgintt4.sav ("NoESE" = bez vides un
#         ilgtspējas izglītības moduļa, TALIS 2024 User Guide, saīsinājumu saraksts)
# Svari: skolotājiem TCHWGT (visos ciklos), direktoriem SCHWGT (2013, 2018) un
#   SCHWGTC (2024). Standartkļūda: BRR ar Feja korekciju, k = 0,5, G = 100
#   (TRWGT1–100; SRWGT1–100 2013/2018, CRWGT1–100 2024):
#   Var = Σ_g (t_g − t)² / (G (1 − k)²) = Σ(...) / 25.
#   Pārbaudīts: TALIS 2018 User Guide 3.3.3; TALIS 2024 User Guide 3. nod. ("Fay's
#   adjustment (factor 0.5)", 100 replikāti); 2013 — empīriski (replikāta / gala
#   svara attiecības ir 0,5 un 1,5 visos trīs ciklos; 2013 rokasgrāmatas PDF OECD
#   arhīva lapa neatdod, sk. data/TALIS/2013/AVOTS.md).
# TALIS 2024 skolotāju anketa ir rotēta (3 formas, IDTQUEST): daļu vienumu atbildēja
#   2/3 skolotāju (trūkst pēc dizaina, MCAR; User Guide 2. nod.). Lieto TCHWGT un
#   BRR bez korekcijas; n šiem vienumiem ir ~2/3 no izlases.
# Salīdzinājuma valstis (kopa EST, LTU, FIN, CZE, HUN, POL; DEU nevienā ciklā):
#   2013: EST, FIN, CZE, POL (LTU, HUN nepiedalījās) — 5 valstis kopā ar LVA
#   2018: EST, LTU, FIN, CZE, HUN (POL nepiedalījās) — 6
#   2024: EST, LTU, FIN, CZE, HUN, POL — 7
#   Stabilais kodols visos ciklos: EST, FIN, CZE (jutīguma pārbaude pārmaiņām).
# Vērtējums katrā ciklā: SARKANS = Latvija sliktākā vai 2. sliktākā no n valstīm un
#   starpība pret pārējo valstu vidējo ar |z| > 1,96; ZAĻŠ simetriski. Vienumi, ne
#   OECD skalas (skalas centrētas katrā valstī).
#
# Ko rēķina:
#  (a) vienumu atbilstības karte starp cikliem (output/pisa2025/talis_vienumu_karte.csv)
#  (b) svērtie rādītāji ar BRR SE katrai valstij katrā ciklā (talis_rinda_raditaji.csv)
#  (c) pārmaiņas 2013 → 2018 → 2024 Latvijā un salīdzinājuma valstīs, starpību
#      starpība ar SE un z; hroniskie un jaunie sarkanie (talis_rinda_parmainas.csv)
#  (d) paaudzes: kuru PISA paaudzi kurš TALIS cikls apraksta
#  (e) direktoru resursu trūkums blakus PISA SC017 rindai 2015–2025
#      (talis_rinda_sc017.csv)
# Izvade: output/pisa2025/talis_vienumu_karte.csv, talis_rinda_raditaji.csv,
#   talis_rinda_vertejums.csv, talis_rinda_parmainas.csv, talis_rinda_sc017.csv,
#   output/pisa2025/plots/77b_talis_rinda_*.png, logs/77b_talis_rinda_output.txt
# ============================================================================

suppressMessages({library(haven); library(data.table); library(ggplot2)})

# --- Darba direktorija: projekta sakne ---------------------------------------
if (!dir.exists("output/pisa2025")) {
  sk <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) NULL)
  if (!is.null(sk) && nzchar(sk)) setwd(dirname(dirname(dirname(sk))))
}
stopifnot(dir.exists("output/pisa2025"))

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/77b_talis_rinda_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("77b. TALIS 2013 → 2018 → 2024 rinda,", format(Sys.time()), "\n")
cat("Darba direktorija:", getwd(), "\n\n")

CIKLI <- c(2013, 2018, 2024)
SALID_KOPA <- c("EST", "LTU", "FIN", "CZE", "HUN", "POL", "DEU")
KODOLS <- c("EST", "FIN", "CZE")
VNOS <- c(LVA = "Latvija", EST = "Igaunija", LTU = "Lietuva", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", POL = "Polija", DEU = "Vācija")
G <- 100; FAY <- 0.5
FAILI <- list(
  `2013` = list(t = "data/TALIS/2013/raw/BTGINTT2.sav", c = "data/TALIS/2013/raw/BCGINTT2.sav", tw = "TCHWGT", tr = "TRWGT", cw = "SCHWGT", cr = "SRWGT"),
  `2018` = list(t = "data/TALIS/2018/raw/BTGINTT3.sav", c = "data/TALIS/2018/raw/BCGINTT3.sav", tw = "TCHWGT", tr = "TRWGT", cw = "SCHWGT", cr = "SRWGT"),
  `2024` = list(t = "data/TALIS/2024/raw/btgintt4.sav", c = "data/TALIS/2024/raw/bcgintt4.sav", tw = "TCHWGT", tr = "TRWGT", cw = "SCHWGTC", cr = "CRWGT"))
# PISA paaudzes katrā TALIS ciklā (ISCED 2 Latvijā = 7.–9. klase)
PAAUDZES <- data.table(
  cikls = CIKLI, macibu_gads = c("2012./13.", "2017./18.", "2023./24."),
  publicets = c("2014-06", "2019-06", "2025-10"),
  pisa_tiesi = c("PISA 2015 paaudze (dz. 1999) 7. klasē; PISA 2012 paaudze jau 10. kl.",
                 "PISA 2018 paaudze (dz. 2002) 9. klasē",
                 "PISA 2025 paaudze (dz. 2009) 8. klasē"),
  pisa_velak = c("PISA 2018 paaudze 4. klasē (ISCED 2 2015/16–2017/18)",
                 "PISA 2022 paaudze 5. klasē, PISA 2025 paaudze 2. klasē",
                 "—"),
  x_lab = c("TALIS 2013\n(PISA 2015 paaudze 7. kl.)", "TALIS 2018\n(PISA 2018 paaudze 9. kl.)", "TALIS 2024\n(PISA 2025 paaudze 8. kl.)"))

# ============================================================================
# 0. Palīgfunkcijas
# ============================================================================
num <- function(x) as.numeric(zap_labels(x))
vienums <- function(x, maks = 6) { x <- num(x); x[!is.na(x) & (x > maks | x < 1)] <- NA; x }
skala <- function(x, maks = 900) { x <- num(x); x[!is.na(x) & x >= maks] <- NA; x }
dala <- function(x, nosac, maks = 6) { x <- vienums(x, maks); ifelse(is.na(x), NA, as.numeric(nosac(x))) }
ge <- function(k) function(x) x >= k
eq <- function(k) function(x) x == k
le <- function(k) function(x) x <= k
gt <- function(k) function(x) x > k
inn <- function(v) function(x) x %in% v

brr <- function(x, W) {
  ok <- !is.na(x)
  n <- sum(ok)
  if (n < 10) return(c(est = NA_real_, se = NA_real_, n = n))
  xs <- x[ok]; Ws <- W[ok, , drop = FALSE]
  m <- colSums(Ws * xs) / colSums(Ws)
  se <- sqrt(sum((m[-1] - m[1])^2) / (G * (1 - FAY)^2))
  c(est = unname(m[1]), se = se, n = n)
}
brr_grupa <- function(X, W) {
  r <- t(vapply(seq_len(ncol(X)), function(j) brr(X[, j], W), numeric(3)))
  data.table(kods = colnames(X), est = r[, 1], se = r[, 2], n = as.integer(r[, 3]))
}

# specifikācijas konstruktori: katrs atgriež list(vars, f)
S1 <- function(v, cond, maks = 6) list(vars = v, f = function(D) dala(D[[v]], cond, maks))
SN <- function(v, maks = 900, augsa = Inf) list(vars = v, f = function(D) { x <- skala(D[[v]], maks); x[!is.na(x) & x > augsa] <- NA; x })
SNc <- function(v, cond, maks = 900) list(vars = v, f = function(D) { x <- skala(D[[v]], maks); ifelse(is.na(x), NA, as.numeric(cond(x))) })
SANY <- function(vs, vals = 1, maks = 6) list(vars = vs, f = function(D) {
  M <- sapply(vs, function(v) vienums(D[[v]], maks)); if (is.null(dim(M))) M <- matrix(M, ncol = length(vs))
  apply(M, 1, function(z) if (all(is.na(z))) NA else as.numeric(any(z %in% vals))) })
SF <- function(vs, f) list(vars = vs, f = f)

# ============================================================================
# 1. Vienumu karte (a): definīcijas katram ciklam
# ============================================================================
cat(strrep("=", 70), "\n1. Vienumu karte starp cikliem\n", strrep("=", 70), "\n\n", sep = "")
DEF <- list()
# tips_cikli: cikli, kuru vērtības drīkst salīdzināt savā starpā (hronisko / jauno
# klasifikācijai un pārmaiņu sarakstiem); pēc noklusējuma visi cikli, kuros vienums ir
d <- function(kods, apraksts, joma, limenis, veids, virziens, s13 = NULL, s18 = NULL, s24 = NULL,
              salidz = "vienāds formulējums un skala", piezime = "", tips_cikli = NULL) {
  if (is.null(tips_cikli)) tips_cikli <- CIKLI[!sapply(list(s13, s18, s24), is.null)]
  DEF[[kods]] <<- list(kods = kods, apraksts = apraksts, joma = joma, limenis = limenis, veids = veids, virziens = virziens,
                       s = list(`2013` = s13, `2018` = s18, `2024` = s24), salidz = salidz, piezime = piezime,
                       tips_cikli = paste(tips_cikli, collapse = ","))
}
T <- "skolotājs"; C <- "direktors"
AGR <- ge(3)   # piekrīt / pilnīgi piekrīt; diezgan daudz / ļoti
# ---- A. Sastāvs un slodze --------------------------------------------------
d("vec_50plus", "Skolotāji 50 gadi un vecāki, daļa", "A. Sastāvs", T, "daļa", -1,
  S1("TCHAGEGR", ge(5)), S1("TCHAGEGR", ge(5)), S1("T4TAGEGR", ge(4)), "pārkodējums",
  "2013/2018 TCHAGEGR 6 grupas (5 = 50–59, 6 = 60+); 2024 T4TAGEGR 5 grupas (4 = 50–59, 5 = 60+)")
d("vec_60plus", "Skolotāji 60 gadi un vecāki, daļa", "A. Sastāvs", T, "daļa", -1,
  S1("TCHAGEGR", eq(6)), S1("TCHAGEGR", eq(6)), S1("T4TAGEGR", eq(5)), "pārkodējums", "vecuma grupas, sk. vec_50plus")
d("vec_lidz30", "Skolotāji jaunāki par 30, daļa", "A. Sastāvs", T, "daļa", 1,
  S1("TCHAGEGR", le(2)), S1("TCHAGEGR", le(2)), S1("T4TAGEGR", eq(1)), "pārkodējums", "2013/2018 grupas 1–2 (< 25, 25–29); 2024 grupa 1 (līdz 29)")
d("sievietes", "Sievietes, daļa", "A. Sastāvs", T, "daļa", 0, S1("TT2G01", eq(1)), S1("TT3G01", eq(1)), S1("TT4G01", eq(1)))
d("stazs_vid", "Darba stāžs skolotāja profesijā, gadi (vid.)", "A. Sastāvs", T, "vid", 0,
  SN("TT2G05B", 97), SN("TT3G11B", 997), NULL, "2024 nav", "2024 publiskajā failā stāžs tikai grupēts (T4TYEXPTT)")
d("stazs_le5", "Stāžs 5 gadi vai mazāk, daļa", "A. Sastāvs", T, "daļa", 0,
  SNc("TT2G05B", le(5), 97), SNc("TT3G11B", le(5), 997), S1("T4TYEXPTT", eq(1)), "pārkodējums", "2024 T4TYEXPTT grupa 1 = ≤ 5 gadi")
d("stazs_gt20", "Stāžs vairāk nekā 20 gadi, daļa", "A. Sastāvs", T, "daļa", 0,
  SNc("TT2G05B", gt(20), 97), SNc("TT3G11B", gt(20), 997), S1("T4TYEXPTT", eq(4)), "pārkodējums", "2024 T4TYEXPTT grupa 4 = > 20 gadi")
d("pastavigs", "Pastāvīgs darba līgums, daļa", "A. Sastāvs", T, "daļa", 1, S1("TT2G06", eq(1)), S1("TT3G09", eq(1)), S1("TT4G09", eq(1)),
  piezime = "2024: forma C nav (2/3 izlases)")
d("pilna_slodze", "Pilna slodze šajā skolā (> 90 %), daļa", "A. Sastāvs", T, "daļa", 1,
  S1("TT2G03", eq(1)), S1("TT3G10A", eq(1)), S1("T4TEMPWH", eq(1)), "pārkodējums", "2024 T4TEMPWH grupēts: 1 = > 90 %; forma C nav")
d("slodze_zem50", "Slodze šajā skolā zem 50 %, daļa", "A. Sastāvs", T, "daļa", -1,
  S1("TT2G03", eq(4)), S1("TT3G10A", eq(4)), NULL, "2024 nav", "2024 slodze tikai divās grupās")
d("cita_skola", "Strādā par skolotāju arī citā skolā, daļa", "A. Sastāvs", T, "daļa", -1, S1("TT2G07", eq(1)), S1("TT3G12", eq(1)), S1("TT4G10", eq(1)))
d("stundas_kopa", "Darba stundas nedēļā kopā šajā skolā (vid.)", "A. Sastāvs", T, "vid", 0,
  SN("TT2G16", 997, 100), SN("TT3G16", 997, 100), SN("TT4G14", 998, 100), piezime = "2024: forma C nav")
d("stundas_maca", "Mācīšanas stundas nedēļā (vid.)", "A. Sastāvs", T, "vid", 0,
  SN("TT2G17", 997, 100), SN("TT3G17", 997, 100), SN("TT4G15", 998, 100), piezime = "2024: forma C nav")
# ---- B. Profesionālā attīstība --------------------------------------------
d("pd_jebkura", "Piedalījies jebkādā prof. attīstībā pēdējos 12 mēnešos, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("PPDACT", eq(2)), SANY(paste0("TT3G22", LETTERS[1:10])), SANY(paste0("TT4G20", LETTERS[1:10]), 1:3), "pārkodējums",
  "2013 atvasinātais PPDACT; 2018 jebkurš no TT3G22A–J; 2024 jebkurš no TT4G20A–J (klātienē, tiešsaistē vai abi)")
d("pd_kursi", "Kursi, semināri vai darbnīcas, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21A1", eq(1)), SANY(c("TT3G22A", "TT3G22B")), S1("TT4G20A", inn(1:3)), "pārkodējums",
  "2013 'kursi un darbnīcas'; 2018 klātienes VAI tiešsaistes kursi; 2024 jebkurā režīmā")
d("pd_konference", "Izglītības konferences, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21B1", eq(1)), S1("TT3G22C", eq(1)), S1("TT4G20B", inn(1:3)), "pārkodējums", "2024 jebkurā režīmā")
d("pd_tikls", "Dalība skolotāju tīklā, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21G", eq(1)), S1("TT3G22H", eq(1)), S1("TT4G20H", inn(1:3)), "pārkodējums", "2024 jebkurā režīmā")
d("pd_citas_skolas", "Vērošanas vizītes citās skolās, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21C1", eq(1)), S1("TT3G22E", eq(1)), S1("TT4G20D", inn(1:3)), "pārkodējums", "2024 jebkurā režīmā")
d("pd_kvalif", "Formāla kvalifikācijas programma, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21F", eq(1)), S1("TT3G22D", eq(1)), S1("TT4G20C", inn(1:3)), "pārkodējums", "2024 jebkurā režīmā")
d("pd_koleg_verosana", "Kolēģu vērošana / koučings, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G21I", eq(1)), S1("TT3G22G", eq(1)), S1("TT4G20G", inn(1:3)), "nesalīdzināms",
  "2013 'mentorings un koučings'; 2018 'kolēģu vai pašvērošana un koučings' (plašāks); 2024 'koučings' (+ atsevišķi TT4G20F refleksija par vērojumiem) — trīs dažādi vienumi", tips_cikli = integer(0))
PD_JOMAS <- c(A = "priekšmeta zināšanas", B = "priekšmeta didaktika", C = "mācību saturs (programma)", D = "skolēnu vērtēšana",
  E = "IKT / digitālie rīki mācīšanai", F = "uzvedība un klases vadība", H = "individualizēta mācīšana", I = "skolēni ar īpašām vajadzībām",
  J = "daudzkultūru / daudzvalodu vide", K = "starppriekšmetu prasmes", L = "vērtējumu analīze un lietošana", M = "sadarbība ar vecākiem")
PD13 <- c(A = "A", B = "B", C = "C", D = "D", E = "E", F = "F", H = "H", I = "I", J = "J", K = "K", L = NA, M = NA)
PD24 <- c(A = "A", B = "B", C = "C", D = "D", E = "E", F = "H", H = "J", I = "K", J = "L", K = NA, L = "M", M = "N")
for (k in names(PD_JOMAS)) local({ kk <- k
  s13d <- if (is.na(PD13[kk])) NULL else S1(paste0("TT2G22", PD13[kk], "1"), eq(1))
  s13v <- if (is.na(PD13[kk])) NULL else S1(paste0("TT2G26", PD13[kk]), eq(4))
  s24d <- if (is.na(PD24[kk])) NULL else S1(paste0("TT4G21", PD24[kk]), eq(1))
  s24v <- if (is.na(PD24[kk])) NULL else S1(paste0("TT4G24", PD24[kk]), eq(4))
  sal <- if (kk == "E") "aptuvens" else if (is.na(PD13[kk]) || is.na(PD24[kk])) "daļēji" else "vienāds formulējums un skala"
  pz <- if (kk == "E") "2013/2018 'IKT prasmes mācīšanai'; 2024 'pedagoģiskās prasmes digitālo resursu integrēšanai'" else
        if (is.na(PD13[kk])) "2013 nav" else if (is.na(PD24[kk])) "2024 nav" else ""
  d(paste0("pd_dal_", kk), paste0("Piedalījās prof. attīstībā: ", PD_JOMAS[kk], ", daļa (no dalībniekiem)"), "B. Prof. attīstība", T, "daļa", 1,
    s13d, S1(paste0("TT3G23", kk), eq(1)), s24d, sal, pz)
  d(paste0("pd_vaj_", kk), paste0("Augsta vajadzība pēc prof. attīstības: ", PD_JOMAS[kk], ", daļa"), "B. Prof. attīstība", T, "daļa", -1,
    s13v, S1(paste0("TT3G27", kk), eq(4)), s24v, sal, paste(pz, if (!is.na(PD24[kk])) "2024: forma A nav" else ""))
})
d("mentors", "Pašlaik ir norīkots mentors, daļa", "B. Prof. attīstība", T, "daļa", 1, S1("TT2G20A", eq(1)), S1("TT3G21A", eq(1)), S1("TT4G19A", eq(1)))
d("ievad_form", "Piedalījies formālā ievadprogrammā, daļa", "B. Prof. attīstība", T, "daļa", 1,
  S1("TT2G19A", eq(1)), SANY(c("TT3G19A1", "TT3G19A2")), S1("TT4G17A", eq(1)), "pārkodējums", "2018 atzīmēts 'jā, pirmajā darbā' vai 'jā, šajā skolā'")
# ---- C. Apmierinātība (piekrīt / pilnīgi piekrīt) --------------------------
d("alga_apm", "Apmierināts ar algu, daļa", "C. Apmierinātība", T, "daļa", 1, NULL, S1("TT3G54A", AGR), S1("TT4G79A", AGR), "2013 nav", "2024: forma C nav")
d("vertets_sab", "Uzskata, ka profesija sabiedrībā tiek vērtēta, daļa", "C. Apmierinātība", T, "daļa", 1, S1("TT2G46H", AGR), S1("TT3G53H", AGR), S1("TT4G78H", AGR))
d("izveletos_atkal", "Izvēlētos skolotāja darbu atkal, daļa", "C. Apmierinātība", T, "daļa", 1, S1("TT2G46B", AGR), S1("TT3G53B", AGR), S1("TT4G78B", AGR))
d("nozelo", "Nožēlo, ka kļuva par skolotāju, daļa", "C. Apmierinātība", T, "daļa", -1, S1("TT2G46D", AGR), S1("TT3G53D", AGR), S1("TT4G78D", AGR))
d("cita_prof", "Domā, vai nebūtu bijis labāk izvēlēties citu profesiju, daļa", "C. Apmierinātība", T, "daļa", -1, S1("TT2G46F", AGR), S1("TT3G53F", AGR), S1("TT4G78F", AGR))
d("apm_kopa", "Kopumā apmierināts ar darbu, daļa", "C. Apmierinātība", T, "daļa", 1, S1("TT2G46J", AGR), S1("TT3G53J", AGR), S1("TT4G78J", AGR))
d("prieksrocibas", "Skolotāja darba priekšrocības pārsniedz trūkumus, daļa", "C. Apmierinātība", T, "daļa", 1, S1("TT2G46A", AGR), S1("TT3G53A", AGR), S1("TT4G78A", AGR))
d("maina_skolu", "Vēlētos pāriet uz citu skolu, daļa", "C. Apmierinātība", T, "daļa", -1, S1("TT2G46C", AGR), S1("TT3G53C", AGR), S1("TT4G78C", AGR))
d("patik_skola", "Patīk strādāt šajā skolā, daļa", "C. Apmierinātība", T, "daļa", 1, S1("TT2G46E", AGR), S1("TT3G53E", AGR), S1("TT4G78E", AGR))
# ---- D. Stress (diezgan daudz / ļoti); 2013 nav ----------------------------
d("stress_daudz", "Izjūt stresu darbā diezgan daudz vai ļoti, daļa", "D. Stress", T, "daļa", -1, NULL, S1("TT3G51A", AGR), S1("TT4G76A", AGR), "2013 nav", "stresa modulis ieviests 2018")
d("stress_gara_vesel", "Darbs negatīvi ietekmē garīgo veselību, daļa", "D. Stress", T, "daļa", -1, NULL, S1("TT3G51C", AGR), S1("TT4G76C", AGR), "2013 nav")
d("stress_fiz_vesel", "Darbs negatīvi ietekmē fizisko veselību, daļa", "D. Stress", T, "daļa", -1, NULL, S1("TT3G51D", AGR), S1("TT4G76D", AGR), "2013 nav")
d("laiks_personigais", "Darbs atstāj laiku personīgajai dzīvei, daļa", "D. Stress", T, "daļa", 1, NULL, S1("TT3G51B", AGR), S1("TT4G76B", AGR), "2013 nav")
STRESS <- c(A = "pārāk daudz stundu sagatavošanas", B = "pārāk daudz stundu", C = "pārāk daudz labošanas", D = "pārāk daudz administratīvā darba",
  E = "papildu pienākumi kolēģu prombūtnes dēļ", F = "atbildība par skolēnu sasniegumiem", G = "disciplīnas uzturēšana", H = "skolēnu iebiedēšana vai apvainošana",
  I = "mainīgās iestāžu prasības", J = "vecāku bažu risināšana", K = "stundu pielāgošana skolēniem ar īpašām vajadzībām")
SKODI <- c(A = "sagatav", B = "stundas", C = "labosana", D = "admin", E = "aizvieto", F = "atbildiba", G = "disc", H = "iebied", I = "prasibas", J = "vecaki", K = "spec")
for (k in names(STRESS)) local({ kk <- k
  d(paste0("stress_", SKODI[kk]), paste0("Stress: ", STRESS[kk], " (diezgan daudz / ļoti), daļa"), "D. Stress", T, "daļa", -1,
    NULL, S1(paste0("TT3G52", kk), AGR), S1(paste0("TT4G77", kk), AGR), "2013 nav", "2024: forma C nav")
})
d("stress_progr_mainas", "Stress: sekot mācību programmas pārmaiņām skolā, daļa", "D. Stress", T, "daļa", -1, NULL, NULL, S1("TT4G77M", AGR), "tikai 2024", "jauns vienums 2024; forma C nav")
d("stress_labklajiba", "Stress: atbildība par skolēnu labklājību, daļa", "D. Stress", T, "daļa", -1, NULL, NULL, S1("TT4G77L", AGR), "tikai 2024", "forma C nav")
d("stress_pl", "Stress: sekot līdzi profesionālajai mācīšanās, daļa", "D. Stress", T, "daļa", -1, NULL, NULL, S1("TT4G77P", AGR), "tikai 2024", "forma C nav")
d("stress_traucejumi", "Stress: pielāgot darbu negaidītu traucējumu dēļ, daļa", "D. Stress", T, "daļa", -1, NULL, NULL, S1("TT4G77O", AGR), "tikai 2024", "forma C nav")
MAINA <- c(A = "skolā ievieš pārāk daudz pārmaiņu iniciatīvu", B = "esmu noguris no visām pārmaiņām skolā", C = "mums prasa mainīt pārāk daudz",
  D = "šķiet, ka vienmēr prasa kaut ko mainīt", E = "gribētu stabilitātes periodu pirms nākamajām pārmaiņām", F = "prasa ieviest pārmaiņas bez vajadzīgajiem resursiem")
for (k in names(MAINA)) local({ kk <- k
  d(paste0("maina_", tolower(kk)), paste0("Pārmaiņu nogurums: ", MAINA[kk], " (piekrīt), daļa"), "D. Stress", T, "daļa", -1,
    NULL, NULL, S1(paste0("TT4G72", kk), AGR), "tikai 2024", "jauns modulis 2024; forma B nav")
})
# ---- E. Nodoms palikt (2013 nav) -------------------------------------------
d("gadi_paliks", "Plānotie atlikušie gadi profesijā (vid.)", "E. Nodoms palikt", T, "vid", 1, NULL, SN("TT3G50", 997), SN("TT4G74", 998), "2013 nav", "2024: forma C nav")
d("pamest_5", "Plāno strādāt par skolotāju vēl ≤ 5 gadus, daļa", "E. Nodoms palikt", T, "daļa", -1, NULL, SNc("TT3G50", le(5), 997), SNc("TT4G74", le(5), 998), "2013 nav", "2024: forma C nav")
d("paliek_7", "Plāno strādāt vēl ≥ 7 gadus, daļa", "E. Nodoms palikt", T, "daļa", 1, NULL, SNc("TT3G50", ge(7), 997), SNc("TT4G74", ge(7), 998), "2013 nav", "2024: forma C nav")
d("pamest_5_zem50", "Plāno strādāt vēl ≤ 5 gadus, skolotāji līdz 49 g., daļa", "E. Nodoms palikt", T, "daļa", -1, NULL,
  SF(c("TT3G50", "TCHAGEGR"), function(D) { x <- skala(D$TT3G50, 997); a <- vienums(D$TCHAGEGR); r <- ifelse(is.na(x), NA, as.numeric(x <= 5)); r[!is.na(a) & a >= 5] <- NA; r }),
  SF(c("TT4G74", "T4TAGEGR"), function(D) { x <- skala(D$TT4G74, 998); a <- vienums(D$T4TAGEGR); r <- ifelse(is.na(x), NA, as.numeric(x <= 5)); r[!is.na(a) & a >= 4] <- NA; r }),
  "2013 nav", "2024: forma C nav")
# ---- F. Laiks stundā -------------------------------------------------------
d("laiks_maciba", "Stundas laika daļa faktiskai mācīšanai, % (vid.)", "F. Laiks stundā", T, "vid", 1, SN("TT2G39C", 996, 100), SN("TT3G39C", 996, 100), SN("TT4G50C", 998, 100), piezime = "2024: forma A nav")
d("laiks_disciplina", "Stundas laika daļa kārtības uzturēšanai, % (vid.)", "F. Laiks stundā", T, "vid", -1, SN("TT2G39B", 996, 100), SN("TT3G39B", 996, 100), SN("TT4G50B", 998, 100), piezime = "2024: forma A nav")
d("laiks_admin", "Stundas laika daļa administratīviem uzdevumiem, % (vid.)", "F. Laiks stundā", T, "vid", -1, SN("TT2G39A", 996, 100), SN("TT3G39A", 996, 100), SN("TT4G50A", 998, 100), piezime = "2024: forma A nav")
d("disc_gaida_ilgi", "Stundas sākumā ilgi jāgaida, līdz skolēni nomierinās, daļa", "F. Laiks stundā", T, "daļa", -1,
  S1("TT2G41A", AGR), S1("TT3G41A", AGR), S1("TT4G54B", AGR), "aptuvens", "2013/2018 piekrišanas skala (piekrīt); 2024 apjoma skala (diezgan daudz / ļoti) — 2024 nav salīdzināms ar iepriekšējiem; forma A nav", tips_cikli = c(2013, 2018))
d("disc_zaude_laiku", "Zaudē daudz laika skolēnu traucējumu dēļ, daļa", "F. Laiks stundā", T, "daļa", -1,
  S1("TT2G41C", AGR), S1("TT3G41C", AGR), S1("TT4G54D", AGR), "aptuvens", "skala mainīta 2024, sk. disc_gaida_ilgi", tips_cikli = c(2013, 2018))
# ---- G. Vērtēšana un atgriezeniskā saite (bieži / vienmēr) -----------------
d("vert_rakstiski", "Rakstiska atgriezeniskā saite papildus atzīmei (bieži / vienmēr), daļa", "G. Vērtēšana", T, "daļa", 1,
  S1("TT2G43D", AGR), S1("TT3G43B", AGR), S1("TT4G53C", AGR), "2024 cits formulējums", "2024 'mutiska VAI rakstiska atgriezeniskā saite' — nav salīdzināms ar 2013/2018; forma A nav", tips_cikli = c(2013, 2018))
d("vert_pasverte", "Skolēni paši vērtē savu progresu (bieži / vienmēr), daļa", "G. Vērtēšana", T, "daļa", 1, S1("TT2G43E", AGR), S1("TT3G43C", AGR), S1("TT4G53D", AGR), piezime = "2024: forma A nav")
d("vert_vero", "Vēro skolēnus uzdevumu laikā un sniedz tūlītēju atgriezenisko saiti (bieži / vienmēr), daļa", "G. Vērtēšana", T, "daļa", 1, S1("TT2G43F", AGR), S1("TT3G43D", AGR), S1("TT4G53E", AGR), piezime = "2024: forma A nav")
d("vert_pats", "Paša izstrādāti pārbaudes darbi (bieži / vienmēr), daļa", "G. Vērtēšana", T, "daļa", 0, S1("TT2G43A", AGR), S1("TT3G43A", AGR), NULL, "2024 nav", "2024 formulējums 'pārbaude tēmas beigās' atšķiras")
# ---- H. Sadarbība (vismaz reizi mēnesī) ------------------------------------
d("sad_kopmaca", "Māca kopā vienā klasē vismaz reizi mēnesī, daļa", "H. Sadarbība", T, "daļa", 1, S1("TT2G33A", ge(5)), S1("TT3G33A", ge(5)), S1("TT4G26A", ge(5)))
d("sad_vero", "Vēro kolēģu stundas un sniedz atgriezenisko saiti vismaz reizi mēnesī, daļa", "H. Sadarbība", T, "daļa", 1, S1("TT2G33B", ge(5)), S1("TT3G33B", ge(5)), S1("TT4G26B", ge(5)))
d("sad_materiali", "Apmainās ar mācību materiāliem vismaz reizi mēnesī, daļa", "H. Sadarbība", T, "daļa", 1, S1("TT2G33D", ge(5)), S1("TT3G33D", ge(5)), S1("TT4G26D", ge(5)))
d("sad_diskusijas", "Apspriež konkrētu skolēnu mācīšanos vismaz reizi mēnesī, daļa", "H. Sadarbība", T, "daļa", 1, S1("TT2G33E", ge(5)), S1("TT3G33E", ge(5)), S1("TT4G26E", ge(5)))
d("sad_prof_maciba", "Piedalās kopīgā profesionālā mācīšanās vismaz reizi mēnesī, daļa", "H. Sadarbība", T, "daļa", 1, S1("TT2G33H", ge(5)), S1("TT3G33H", ge(5)), S1("TT4G26G", ge(5)))
# ---- I. Klases sastāvs -----------------------------------------------------
d("klase_vaji30", "Klasē > 30 % skolēnu ar zemiem sasniegumiem, daļa", "I. Klases sastāvs", T, "daļa", -1, S1("TT2G35B", ge(4)), S1("TT3G35B", ge(4)), S1("TT4G47C", ge(4), 7), "pārkodējums", "2024 7 kategorijas, ≥ 4 = > 30 %; forma A nav")
d("klase_spec11", "Klasē > 10 % skolēnu ar īpašām vajadzībām, daļa", "I. Klases sastāvs", T, "daļa", 0, S1("TT2G35C", ge(3)), S1("TT3G35C", ge(3)), S1("TT4G47E", ge(3), 7), "pārkodējums", "2024 7 kategorijas; forma A nav")
d("klase_uzved11", "Klasē > 10 % skolēnu ar uzvedības problēmām, daļa", "I. Klases sastāvs", T, "daļa", -1, S1("TT2G35D", ge(3)), S1("TT3G35D", ge(3)), S1("TT4G47F", ge(3), 7), "pārkodējums", "2024 7 kategorijas; forma A nav")
d("klase_lielums", "Mērķa klases lielums, skolēni (vid.)", "I. Klases sastāvs", T, "vid", 0, SN("TT2G38", 97, 60), SN("TT3G38", 997, 60), NULL, "2024 nav", "2024 tikai grupēts (T4TCSIZE)")
# ---- J. Direktori: resursu trūkums ----------------------------------------
TR <- c(A = "kvalificētu skolotāju trūkums", B = "trūkst skolotāju ar kompetenci darbam ar īpašām vajadzībām", D = "mācību materiālu trūkums vai nepiemērotība",
  E = "digitālo tehnoloģiju (2013: datoru) trūkums vai nepiemērotība", F = "nepietiekams interneta pieslēgums", G = "bibliotēkas materiālu trūkums",
  H = "atbalsta personāla trūkums", I = "mācību telpu trūkums vai nepiemērotība", J = "fiziskās infrastruktūras trūkums vai nepiemērotība",
  K = "trūkst skolotāju ar kompetenci daudzvalodu vidē", L = "trūkst skolotāju ar kompetenci darbam ar sociāli nelabvēlīgiem skolēniem", N = "trūkst laika mācību vadībai")
TR13 <- c(A = "A", B = "B", D = "D", E = "E", F = "F", G = "H", H = "I", I = NA, J = NA, K = NA, L = NA, N = NA)
for (k in names(TR)) local({ kk <- k
  s13 <- if (is.na(TR13[kk])) NULL else S1(paste0("TC2G31", TR13[kk]), eq(4), 4)
  s13v <- if (is.na(TR13[kk])) NULL else SF(paste0("TC2G31", TR13[kk]), function(D) vienums(D[[paste0("TC2G31", TR13[kk])]], 4))
  pz13 <- if (is.na(TR13[kk])) "2013 nav" else if (kk == "E") "2013 'datori mācīšanai'; 2018/2024 'digitālās tehnoloģijas'" else ""
  d(paste0("tr_loti_", kk), paste0("Traucē mācīšanu ļoti (4): ", TR[kk], ", skolu daļa"), "J. Resursu trūkums", C, "daļa", -1,
    s13, S1(paste0("TC3G29", kk), eq(4), 4), S1(paste0("TC4G40", kk), eq(4), 4),
    if (is.na(TR13[kk])) "2013 nav" else if (kk == "E") "aptuvens" else "pārkodējums",
    paste("2013 skala 'nemaz / ļoti maz / zināmā mērā / ļoti', 2018/2024 'nemaz / zināmā mērā / diezgan daudz / ļoti' — salīdzināma tikai augstākā kategorija.", pz13),
    tips_cikli = if (kk == "E") c(2018, 2024) else NULL)
  d(paste0("tr_", kk), paste0("Traucē mācīšanu diezgan daudz vai ļoti (3–4): ", TR[kk], ", skolu daļa"), "J. Resursu trūkums", C, "daļa", -1,
    NULL, S1(paste0("TC3G29", kk), ge(3), 4), S1(paste0("TC4G40", kk), ge(3), 4), "2013 cita skala", "2013 3. kategorija ir 'zināmā mērā', tāpēc 2013 nav rēķināts")
  d(paste0("tr_vid_", kk), paste0("Traucē mācīšanu (1 nemaz … 4 ļoti): ", TR[kk], " (vid.)"), "J. Resursu trūkums", C, "vid", -1,
    s13v, SF(paste0("TC3G29", kk), function(D) vienums(D[[paste0("TC3G29", kk)]], 4)), SF(paste0("TC4G40", kk), function(D) vienums(D[[paste0("TC4G40", kk)]], 4)),
    if (is.na(TR13[kk])) "2013 nav" else "aptuvens", "vidējais 1–4; 2013 skalas vidējās kategorijas citādas (aptuvens, 2013 pārmaiņu klasifikācijā nelieto)", tips_cikli = c(2018, 2024))
})
d("STRATIO", "Skolēni uz vienu skolotāju (vid.)", "J. Resursu trūkums", C, "vid", 0, SN("STRATIO", 997, 100), SN("STRATIO", 997, 100), SN("T4STRATIO", 998, 100))
d("TPRATIO", "Skolotāji uz vienu pedagoģiskā atbalsta darbinieku (vid.)", "J. Resursu trūkums", C, "vid", -1, SN("TPRATIO", 997, 200), SN("TPRATIO", 997, 200), SN("T4TPRATIO", 998, 200))
d("dir_jauno_dala", "Jauno skolotāju attiecība pret skolotāju skaitu skolā (vid.)", "J. Resursu trūkums", C, "vid", 0, NULL, NULL, SN("T4TTNRATIO", 998, 5), "tikai 2024")
d("dir_aizgajuso_dala", "Aizgājušo skolotāju attiecība pret skolotāju skaitu skolā (vid.)", "J. Resursu trūkums", C, "vid", -1, NULL, NULL, SN("T4TTLRATIO", 998, 5), "tikai 2024")
# ---- K. Direktori: sastāvs, klimats, vadība, mentori -----------------------
d("dir_50plus", "Direktori 50 gadi un vecāki, daļa", "K. Direktori", C, "daļa", 0, S1("PRAGEGR", ge(4)), S1("PRAGEGR", ge(3)), S1("T4PAGEGR", ge(3)), "pārkodējums",
  "2013 PRAGEGR 5 grupas (4 = 50–59); 2018 PRAGEGR un 2024 T4PAGEGR 4 grupas (3 = 50–59)")
d("dir_sievietes", "Direktores sievietes, daļa", "K. Direktori", C, "daļa", 0, S1("TC2G01", eq(1)), S1("TC3G01", eq(1)), S1("TC4G01", eq(1)))
d("sast_citaval10", "Skolā > 10 % skolēnu ar citu dzimto valodu, daļa", "K. Direktori", C, "daļa", 0, S1("TC2G15A", ge(3)), S1("TC3G17A", ge(3)), S1("TC4G21B", ge(3), 7), "pārkodējums", "2024 7 kategorijas")
d("sast_spec10", "Skolā > 10 % skolēnu ar īpašām vajadzībām, daļa", "K. Direktori", C, "daļa", 0, S1("TC2G15B", ge(3)), S1("TC3G17B", ge(3)), S1("TC4G21C", ge(3), 7), "pārkodējums", "2024 7 kategorijas")
d("sast_nelabv30", "Skolā > 30 % skolēnu no sociāli nelabvēlīgām ģimenēm, daļa", "K. Direktori", C, "daļa", 0, S1("TC2G15C", ge(4)), S1("TC3G17C", ge(4)), S1("TC4G21D", ge(4), 7), "pārkodējums", "2024 7 kategorijas")
d("mentor_nav", "Skolā nav mentoru programmas, daļa", "K. Direktori", C, "daļa", -1, S1("TC2G36", eq(4), 4), S1("TC3G34", eq(4), 4), S1("TC4G33", eq(4), 4))
d("mentor_visi", "Mentoru programma pieejama visiem skolotājiem, daļa", "K. Direktori", C, "daļa", 1, S1("TC2G36", eq(3), 4), S1("TC3G34", eq(3), 4), S1("TC4G33", eq(3), 4))
d("ievad_skola", "Skolā ir formāla ievadprogramma jaunajiem skolotājiem, daļa", "K. Direktori", C, "daļa", 1, S1("TC2G33A", eq(1)), S1("TC3G31A", eq(1)), S1("TC4G31A", inn(1:2), 3), "pārkodējums", "2024 'visiem jaunajiem skolā' vai 'tikai jaunajiem profesijā'")
d("nirg_ned", "Ņirgāšanās / iebiedēšana starp skolēniem vismaz reizi nedēļā, skolu daļa", "K. Direktori", C, "daļa", -1, S1("TC2G32E", ge(4), 5), S1("TC3G30B", ge(4), 5), S1("TC4G41B", ge(4), 5), "aptuvens",
  "2013 'iebiedēšana vai verbāla vardarbība starp skolēniem'; 2018/2024 'iebiedēšana vai ņirgāšanās'")
d("klim_sadarb", "Skolā ir sadarbības kultūra ar savstarpēju atbalstu (piekrīt), daļa", "K. Direktori", C, "daļa", 1, S1("TC2G22E", AGR), S1("TC3G26F", AGR), S1("TC4G38E", AGR))
d("gaidas_augstas", "Skolotājiem ir augstas gaidas par skolēnu sasniegumiem (diezgan / ļoti), daļa", "K. Direktori", C, "daļa", 1, NULL, S1("TC3G27C", AGR), S1("TC4G39C", AGR), "2013 nav")
d("vecaki_atbalsta", "Vecāki atbalsta skolēnu sasniegumus (diezgan / ļoti), daļa", "K. Direktori", C, "daļa", 1, NULL, S1("TC3G27D", AGR), S1("TC4G39D", AGR), "2013 nav")
d("vad_vero", "Direktors bieži vai ļoti bieži vēro stundas, daļa", "K. Direktori", C, "daļa", 1, S1("TC2G21B", AGR), S1("TC3G22B", AGR), S1("TC4G27B", AGR))
d("vad_atgr", "Direktors bieži sniedz skolotājiem atgriezenisko saiti pēc vērojumiem, daļa", "K. Direktori", C, "daļa", 1, NULL, S1("TC3G22C", AGR), S1("TC4G27C", AGR), "2013 nav")
d("vad_sadarb", "Direktors bieži atbalsta skolotāju sadarbību, daļa", "K. Direktori", C, "daļa", 1, S1("TC2G21C", AGR), S1("TC3G22D", AGR), S1("TC4G27D", AGR))
d("vad_atbild", "Direktors bieži rūpējas, lai skolotāji jūtas atbildīgi par rezultātiem, daļa", "K. Direktori", C, "daļa", 1, S1("TC2G21E", AGR), S1("TC3G22F", AGR), S1("TC4G27F", AGR))
# ---- L. Direktoru darbs ----------------------------------------------------
d("dir_stress_admin", "Direktora stress: pārāk daudz administratīvā darba, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, S1("TC3G43B", AGR), S1("TC4G51B", AGR), "2013 nav")
d("dir_stress_prasibas", "Direktora stress: mainīgās iestāžu prasības, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, S1("TC3G43G", AGR), S1("TC4G51G", AGR), "2013 nav")
d("dir_stress_atbildiba", "Direktora stress: atbildība par skolēnu sasniegumiem, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, S1("TC3G43D", AGR), S1("TC4G51D", AGR), "2013 nav")
d("dir_stress_spec", "Direktora stress: skolēnu ar īpašām vajadzībām uzņemšana, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, S1("TC3G43I", AGR), S1("TC4G51I", AGR), "2013 nav")
d("dir_stress_daudz", "Direktors izjūt stresu darbā diezgan daudz vai ļoti, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, NULL, S1("TC4G50A", AGR), "tikai 2024")
d("dir_alga_apm", "Direktors apmierināts ar algu, daļa", "L. Direktora darbs", C, "daļa", 1, NULL, S1("TC3G45A", AGR), S1("TC4G53A", AGR), "2013 nav")
d("dir_vairak_atbalsta", "Direktoram vajag vairāk atbalsta no iestādēm, daļa", "L. Direktora darbs", C, "daļa", -1, NULL, S1("TC3G45D", AGR), S1("TC4G53D", AGR), "2013 nav")
d("dir_vertets_sab", "Direktors uzskata, ka skolotāja profesija sabiedrībā tiek vērtēta, daļa", "L. Direktora darbs", C, "daļa", 1, S1("TC2G39G", AGR), S1("TC3G44H", AGR), S1("TC4G52H", AGR))
d("dir_izveletos_atkal", "Direktors izvēlētos šo darbu atkal, daļa", "L. Direktora darbs", C, "daļa", 1, S1("TC2G39B", AGR), S1("TC3G44B", AGR), S1("TC4G52B", AGR))

KARTE <- rbindlist(lapply(DEF, function(x) data.table(
  limenis = x$limenis, joma = x$joma, kods = x$kods, raditajs = x$apraksts, veids = x$veids, virziens = x$virziens,
  v2013 = if (is.null(x$s$`2013`)) NA_character_ else paste(x$s$`2013`$vars, collapse = "; "),
  v2018 = if (is.null(x$s$`2018`)) NA_character_ else paste(x$s$`2018`$vars, collapse = "; "),
  v2024 = if (is.null(x$s$`2024`)) NA_character_ else paste(x$s$`2024`$vars, collapse = "; "),
  ciklu_skaits = sum(!sapply(x$s, is.null)), salidzinamiba = x$salidz, salidzinami_cikli = x$tips_cikli, piezime = trimws(x$piezime))))
setorder(KARTE, limenis, joma, kods)
fwrite(KARTE, "output/pisa2025/talis_vienumu_karte.csv")
cat("Rādītāju skaits:", nrow(KARTE), "; skolotāju:", sum(KARTE$limenis == T), "; direktoru:", sum(KARTE$limenis == C), "\n")
print(KARTE[, .N, by = .(ciklu_skaits, salidzinamiba)][order(-ciklu_skaits, salidzinamiba)])
cat("Visos 3 ciklos:", sum(KARTE$ciklu_skaits == 3), "; 2018 un 2024:", sum(KARTE$ciklu_skaits == 2 & !is.na(KARTE$v2018) & !is.na(KARTE$v2024)),
    "; tikai 2024:", sum(KARTE$ciklu_skaits == 1), "\n")
cat("Saglabāts: output/pisa2025/talis_vienumu_karte.csv\n\n")

# ============================================================================
# 2. Rādītāji katrā ciklā (b)
# ============================================================================
cat(strrep("=", 70), "\n2. Datu ielāde un rādītāji katrā ciklā\n", strrep("=", 70), "\n\n", sep = "")
rekina_ciklu <- function(cikls) {
  F <- FAILI[[as.character(cikls)]]
  out <- list()
  for (lim in c(T, C)) {
    defs <- Filter(function(x) x$limenis == lim && !is.null(x$s[[as.character(cikls)]]), DEF)
    vars <- unique(unlist(lapply(defs, function(x) x$s[[as.character(cikls)]]$vars)))
    fails <- if (lim == T) F$t else F$c
    w <- if (lim == T) F$tw else F$cw; rw <- if (lim == T) F$tr else F$cr
    hdr <- names(read_sav(fails, n_max = 0))
    trukst <- setdiff(vars, hdr)
    if (length(trukst)) stop("Trūkst mainīgo failā ", fails, ": ", paste(trukst, collapse = ", "))
    cols <- c("CNTRY", "IDSCHOOL", w, paste0(rw, 1:G), vars)
    t0 <- Sys.time()
    D <- as.data.table(read_sav(fails, col_select = all_of(cols)))
    D <- D[CNTRY %in% c("LVA", SALID_KOPA)]
    D[, wgt := num(get(w))]
    n0 <- nrow(D); D <- D[!is.na(wgt) & wgt > 0]
    cat(sprintf("%d %s: %s ielādēts (%s s), %d rindas, %d bez svara izmestas\n", cikls, lim, basename(fails),
                format(round(difftime(Sys.time(), t0, units = "secs"))), n0, n0 - nrow(D)))
    print(D[, .(n = .N, skolas = uniqueN(IDSCHOOL)), by = CNTRY][order(CNTRY)])
    X <- sapply(defs, function(x) x$s[[as.character(cikls)]]$f(D)); colnames(X) <- names(defs)
    W <- as.matrix(D[, c("wgt", paste0(rw, 1:G)), with = FALSE])
    r <- rbindlist(lapply(sort(unique(D$CNTRY)), function(cn) {
      i <- which(D$CNTRY == cn)
      rr <- brr_grupa(X[i, , drop = FALSE], W[i, , drop = FALSE]); rr[, cnt := cn]; rr
    }))
    r[, `:=`(cikls = cikls, limenis = lim)]
    out[[lim]] <- r
  }
  rbindlist(out)
}
REZ <- rbindlist(lapply(CIKLI, rekina_ciklu))
REZ <- merge(REZ, KARTE[, .(kods, raditajs, joma, veids, virziens, salidzinamiba)], by = "kods")
REZ[, valsts := VNOS[cnt]]
setcolorder(REZ, c("cikls", "limenis", "joma", "kods", "raditajs", "veids", "virziens", "salidzinamiba", "cnt", "valsts", "est", "se", "n"))
setorder(REZ, limenis, joma, kods, cikls, cnt)
fwrite(REZ, "output/pisa2025/talis_rinda_raditaji.csv")
cat("\nSaglabāts: output/pisa2025/talis_rinda_raditaji.csv (", nrow(REZ), " rindas)\n", sep = "")
cat("\nValstis katrā ciklā:\n"); print(REZ[!is.na(est), .(valstis = paste(sort(unique(cnt)), collapse = ", ")), by = cikls])

# ============================================================================
# 3. Vērtējums katrā ciklā (sarkanie / zaļie) — pret visām pieejamajām valstīm
#    un pret stabilo kodolu (EST, FIN, CZE)
# ============================================================================
cat("\n", strrep("=", 70), "\n3. Vērtējums: Latvija pret salīdzinājuma valstīm katrā ciklā\n", strrep("=", 70), "\n\n", sep = "")
vertejums <- function(R, kopa = NULL, rangs_ok = TRUE) {
  R <- R[!is.na(est)]
  if (!is.null(kopa)) R <- R[cnt %in% c("LVA", kopa)]
  V <- R[, {
    lv <- .SD[cnt == "LVA"]; ci <- .SD[cnt != "LVA"]
    if (nrow(lv) != 1 || nrow(ci) < 2) NULL else {
      sal_vid <- mean(ci$est); sal_se <- sqrt(sum(ci$se^2)) / nrow(ci)
      st <- lv$est - sal_vid; st_se <- sqrt(lv$se^2 + sal_se^2)
      v <- virziens[1]; visi <- .SD[, .(cnt, est)]
      if (v >= 0) setorder(visi, -est) else setorder(visi, est)
      .(lva = lv$est, lva_se = lv$se, n_lva = lv$n, sal_vid = sal_vid, sal_se = sal_se, sal_min = min(ci$est), sal_max = max(ci$est),
        sal_valstis = paste(sort(ci$cnt), collapse = ","), n_valstis = nrow(ci) + 1L, starpiba = st, starp_se = st_se, z = st / st_se,
        rangs_labums = which(visi$cnt == "LVA"))
    }
  }, by = .(cikls, limenis, joma, kods, raditajs, veids, virziens, salidzinamiba)]
  V[, nozimigs := abs(z) > 1.96]
  V[, karogs := fifelse(virziens == 0, "aprakstošs",
               fifelse(rangs_labums >= n_valstis - 1 & nozimigs, "SARKANS",
               fifelse(rangs_labums <= 2 & nozimigs, "ZAĻŠ", "vidū")))]
  V
}
VERT <- vertejums(REZ)
VERTK <- vertejums(REZ, KODOLS)[, .(cikls, limenis, kods, kodols_vid = sal_vid, kodols_se = sal_se, kodols_z = z, kodols_karogs = karogs)]
VERT <- merge(VERT, VERTK, by = c("cikls", "limenis", "kods"), all.x = TRUE)
setorder(VERT, limenis, joma, kods, cikls)
fwrite(VERT, "output/pisa2025/talis_rinda_vertejums.csv")
cat("Saglabāts: output/pisa2025/talis_rinda_vertejums.csv\n")
cat("Kritērijs: SARKANS = Latvija sliktākā vai 2. sliktākā no n valstīm (2013: 5, 2018: 6, 2024: 7) un |z| > 1,96 pret pārējo vidējo.\n")
cat("Kodola (EST, FIN, CZE) karogs: tas pats kritērijs 4 valstu kopā — tikai jutīguma pārbaudei (2. sliktākā no 4 ir vājš kritērijs).\n\n")
fmt <- function(x, veids) ifelse(veids == "daļa", sprintf("%.1f %%", 100 * x), sprintf("%.2f", x))
for (ck in CIKLI) {
  cat("\n", strrep("#", 60), "\nCIKLS ", ck, " (valstis: ", VERT[cikls == ck]$sal_valstis[1], ")\n", sep = "")
  print(VERT[cikls == ck, .N, by = .(limenis, karogs)][order(limenis, karogs)])
  for (kg in c("SARKANS", "ZAĻŠ")) {
    cat("\n--- ", kg, " ", ck, " ---\n", sep = "")
    S <- VERT[cikls == ck & karogs == kg][order(limenis, -abs(z))]
    for (i in seq_len(nrow(S))) with(S[i], cat(sprintf("  [%s] %s (%s): LVA %s, citas %s [%s; %s], z = %.1f, rangs %d/%d%s\n",
      substr(limenis, 1, 3), raditajs, kods, fmt(lva, veids), fmt(sal_vid, veids), fmt(sal_min, veids), fmt(sal_max, veids), z, rangs_labums, n_valstis,
      if (!is.na(kodols_karogs) && kodols_karogs != kg) paste0(" [kodols: ", kodols_karogs, "]") else "")))
  }
}

# ============================================================================
# 4. Pārmaiņas 2013 → 2018 → 2024 (c): Latvija, salīdzinājuma vidējais, starpību starpība
# ============================================================================
cat("\n", strrep("=", 70), "\n4. Pārmaiņas 2013 → 2018 → 2024\n", strrep("=", 70), "\n\n", sep = "")
W <- dcast(VERT, limenis + joma + kods + raditajs + veids + virziens + salidzinamiba ~ cikls,
           value.var = c("lva", "lva_se", "sal_vid", "sal_se", "kodols_vid", "kodols_se", "z", "karogs", "n_valstis", "rangs_labums"))
parm <- function(W, a, b) {
  d_lva <- W[[paste0("lva_", b)]] - W[[paste0("lva_", a)]]; d_lva_se <- sqrt(W[[paste0("lva_se_", b)]]^2 + W[[paste0("lva_se_", a)]]^2)
  d_sal <- W[[paste0("sal_vid_", b)]] - W[[paste0("sal_vid_", a)]]; d_sal_se <- sqrt(W[[paste0("sal_se_", b)]]^2 + W[[paste0("sal_se_", a)]]^2)
  d_kod <- W[[paste0("kodols_vid_", b)]] - W[[paste0("kodols_vid_", a)]]; d_kod_se <- sqrt(W[[paste0("kodols_se_", b)]]^2 + W[[paste0("kodols_se_", a)]]^2)
  dd <- d_lva - d_sal; dd_se <- sqrt(d_lva_se^2 + d_sal_se^2)
  ddk <- d_lva - d_kod; ddk_se <- sqrt(d_lva_se^2 + d_kod_se^2)
  p <- paste0(substr(a, 3, 4), "_", substr(b, 3, 4))
  W[, (paste0("d_lva_", p)) := d_lva]; W[, (paste0("d_lva_se_", p)) := d_lva_se]; W[, (paste0("z_lva_", p)) := d_lva / d_lva_se]
  W[, (paste0("d_sal_", p)) := d_sal]; W[, (paste0("d_sal_se_", p)) := d_sal_se]
  W[, (paste0("dd_", p)) := dd]; W[, (paste0("dd_se_", p)) := dd_se]; W[, (paste0("z_dd_", p)) := dd / dd_se]
  W[, (paste0("dd_kodols_", p)) := ddk]; W[, (paste0("z_dd_kodols_", p)) := ddk / ddk_se]
  invisible(W)
}
parm(W, "2013", "2018"); parm(W, "2018", "2024"); parm(W, "2013", "2024")
# Hroniskie un jaunie sarkanie: tikai ciklos, kuros vienums ir salīdzināms (salidzinami_cikli);
# nesalīdzināma cikla karogs klasifikācijā = "nav mērīts"
W <- merge(W, KARTE[, .(kods, salidzinami_cikli)], by = "kods")
W[, karogs_2013 := fifelse(grepl("2013", salidzinami_cikli), karogs_2013, NA_character_)]
W[, karogs_2018 := fifelse(grepl("2018", salidzinami_cikli), karogs_2018, NA_character_)]
W[, karogs_2024 := fifelse(grepl("2024", salidzinami_cikli), karogs_2024, NA_character_)]
W[, tips := fcase(
  salidzinami_cikli == "", "nesalīdzināms starp cikliem",
  karogs_2018 == "SARKANS" & karogs_2013 == "SARKANS" & karogs_2024 == "SARKANS", "hronisks (sarkans 2013, 2018, 2024)",
  karogs_2018 == "SARKANS" & karogs_2013 == "SARKANS" & !is.na(karogs_2024) & karogs_2024 != "SARKANS", "hronisks 2013–2018, 2024 vairs ne",
  karogs_2018 == "SARKANS" & karogs_2013 == "SARKANS" & is.na(karogs_2024), "hronisks 2013–2018 (2024 nav mērīts)",
  karogs_2018 == "SARKANS" & !is.na(karogs_2013) & karogs_2013 != "SARKANS" & karogs_2024 == "SARKANS", "jauns 2018, paliek 2024",
  karogs_2018 == "SARKANS" & !is.na(karogs_2013) & karogs_2013 != "SARKANS" & !is.na(karogs_2024) & karogs_2024 != "SARKANS", "jauns 2018, 2024 vairs ne",
  karogs_2018 == "SARKANS" & !is.na(karogs_2013) & karogs_2013 != "SARKANS" & is.na(karogs_2024), "jauns 2018 (2024 nav mērīts)",
  karogs_2018 == "SARKANS" & is.na(karogs_2013) & karogs_2024 == "SARKANS", "sarkans 2018 un 2024 (2013 nav mērīts)",
  karogs_2018 == "SARKANS" & is.na(karogs_2013) & !is.na(karogs_2024) & karogs_2024 != "SARKANS", "sarkans 2018, 2024 vairs ne (2013 nav mērīts)",
  karogs_2018 == "SARKANS" & is.na(karogs_2013) & is.na(karogs_2024), "sarkans 2018 (citos ciklos nav mērīts)",
  !is.na(karogs_2018) & karogs_2018 != "SARKANS" & karogs_2024 == "SARKANS", "jauns 2024",
  is.na(karogs_2018) & karogs_2024 == "SARKANS", "sarkans 2024 (2018 nav mērīts)",
  karogs_2013 == "SARKANS" & !is.na(karogs_2018) & karogs_2018 != "SARKANS", "sarkans 2013, 2018 vairs ne",
  default = "nav sarkans")]
setorder(W, limenis, joma, kods)
fwrite(W, "output/pisa2025/talis_rinda_parmainas.csv")
cat("Saglabāts: output/pisa2025/talis_rinda_parmainas.csv (", nrow(W), " rādītāji)\n", sep = "")
cat("\nSarkano rādītāju tipi (skolotāji un direktori):\n"); print(W[, .N, by = tips][order(-N)])

izdr_rinda <- function(S) for (i in seq_len(nrow(S))) with(S[i], {
  f <- function(x) if (is.na(x)) "—" else fmt(x, veids)
  cat(sprintf("  %s (%s) [%s]\n     LVA %s → %s → %s | citas %s → %s → %s | z %s / %s / %s | karogs %s / %s / %s\n",
    raditajs, kods, substr(limenis, 1, 3), f(lva_2013), f(lva_2018), f(lva_2024), f(sal_vid_2013), f(sal_vid_2018), f(sal_vid_2024),
    ifelse(is.na(z_2013), "—", sprintf("%.1f", z_2013)), ifelse(is.na(z_2018), "—", sprintf("%.1f", z_2018)), ifelse(is.na(z_2024), "—", sprintf("%.1f", z_2024)),
    ifelse(is.na(karogs_2013), "—", karogs_2013), ifelse(is.na(karogs_2018), "—", karogs_2018), ifelse(is.na(karogs_2024), "—", karogs_2024)))
  if (!is.na(dd_18_24)) cat(sprintf("     Δ LVA 2018→2024 %s (z %.1f); Δ citas %s; starpību starpība %s (z %.1f; kodols z %.1f)\n",
    f(d_lva_18_24), z_lva_18_24, f(d_sal_18_24), f(dd_18_24), z_dd_18_24, z_dd_kodols_18_24))
  if (!is.na(dd_13_18)) cat(sprintf("     Δ LVA 2013→2018 %s (z %.1f); Δ citas %s; starpību starpība %s (z %.1f; kodols z %.1f)\n",
    f(d_lva_13_18), z_lva_13_18, f(d_sal_13_18), f(dd_13_18), z_dd_13_18, z_dd_kodols_13_18))
})
for (tp in unique(W[!tips %in% c("nav sarkans", "nesalīdzināms starp cikliem")][order(tips)]$tips)) { cat("\n--- ", tp, " ---\n", sep = ""); izdr_rinda(W[tips == tp][order(limenis, -abs(z_2018))]) }
cat("\nNesalīdzināmi starp cikliem (karogs katrā ciklā ir, bet pārmaiņas neinterpretē):", paste(W[tips == "nesalīdzināms starp cikliem"]$kods, collapse = ", "), "\n")

cat("\n--- Lielākās Latvijas pārmaiņas 2018 → 2024 pret salīdzinājuma valstīm (|z starpību starpībai| > 2,58; virziens: + = Latvijā pieauga vairāk; tikai salīdzināmi vienumi) ---\n")
DD <- W[!is.na(z_dd_18_24) & abs(z_dd_18_24) > 2.58 & grepl("2018", salidzinami_cikli) & grepl("2024", salidzinami_cikli)][order(-abs(z_dd_18_24))]
for (i in seq_len(nrow(DD))) with(DD[i], cat(sprintf("  %s (%s): LVA %s → %s, citas %s → %s; starpību starpība %s (z %.1f), %s\n",
  raditajs, kods, fmt(lva_2018, veids), fmt(lva_2024, veids), fmt(sal_vid_2018, veids), fmt(sal_vid_2024, veids), fmt(dd_18_24, veids), z_dd_18_24,
  fifelse(dd_18_24 * virziens > 0, "Latvijai labvēlīgi", fifelse(virziens == 0, "aprakstošs", "Latvijai nelabvēlīgi")))))
cat("\n--- Lielākās Latvijas pārmaiņas 2013 → 2018 pret salīdzinājuma valstīm (|z| > 2,58) ---\n")
DD <- W[!is.na(z_dd_13_18) & abs(z_dd_13_18) > 2.58 & grepl("2013", salidzinami_cikli) & grepl("2018", salidzinami_cikli)][order(-abs(z_dd_13_18))]
for (i in seq_len(nrow(DD))) with(DD[i], cat(sprintf("  %s (%s): LVA %s → %s, citas %s → %s; starpību starpība %s (z %.1f), %s\n",
  raditajs, kods, fmt(lva_2013, veids), fmt(lva_2018, veids), fmt(sal_vid_2013, veids), fmt(sal_vid_2018, veids), fmt(dd_13_18, veids), z_dd_13_18,
  fifelse(dd_13_18 * virziens > 0, "Latvijai labvēlīgi", fifelse(virziens == 0, "aprakstošs", "Latvijai nelabvēlīgi")))))

cat("\n--- Pilna tabula: Latvija un salīdzinājuma vidējais trīs ciklos (daļas %) ---\n")
PT <- W[, .(limenis = substr(limenis, 1, 3), kods, lva13 = fmt(lva_2013, veids), lva18 = fmt(lva_2018, veids), lva24 = fmt(lva_2024, veids),
            cit13 = fmt(sal_vid_2013, veids), cit18 = fmt(sal_vid_2018, veids), cit24 = fmt(sal_vid_2024, veids),
            k13 = substr(karogs_2013, 1, 3), k18 = substr(karogs_2018, 1, 3), k24 = substr(karogs_2024, 1, 3))]
print(PT, nrows = 500)

# ============================================================================
# 5. Paaudzes (d)
# ============================================================================
cat("\n", strrep("=", 70), "\n5. TALIS cikli un PISA paaudzes\n", strrep("=", 70), "\n\n", sep = "")
print(PAAUDZES[, .(cikls, macibu_gads, publicets, pisa_tiesi, pisa_velak)])
cat("\nTALIS 2024 dati (2023./24. m. g.) apraksta skolotāju korpusu, kas mācīja PISA 2025 paaudzi 8. klasē, gadu pirms PISA 2025 testa\n")
cat("(2025. gada pavasaris). OECD tos publicēja 2025-10, 11 mēnešus pirms PISA 2025 rezultātiem (2026-09-08).\n")
cat("Vai tie bija brīdinājums: sk. 4. sadaļas 'jauns 2024' un 'hronisks' sarakstus un docs/72_talis_2013_2024.md.\n")
pal <- VERT[limenis == T & kods %in% c("paliek_7", "pamest_5", "pamest_5_zem50", "vec_50plus", "vec_60plus", "vec_lidz30", "stazs_le5", "gadi_paliks")]
cat("\nKorpusa noturība un vecums pa cikliem (LVA / citas):\n")
print(dcast(pal[, .(cikls, kods, v = paste0(fmt(lva, veids), " / ", fmt(sal_vid, veids), " (", karogs, ")"))], kods ~ cikls, value.var = "v"))

# ============================================================================
# 6. Direktoru resursu trūkums blakus PISA SC017 (e)
# ============================================================================
cat("\n", strrep("=", 70), "\n6. TALIS resursu trūkums blakus PISA SC017 2015–2025\n", strrep("=", 70), "\n\n", sep = "")
P <- fread("output/pisa2025/skola2030_simptomi.csv")
PISA_MAP <- data.table(pisa = c("AUTO_SC017Q01NA", "AUTO_SC017Q03NA", "AUTO_SC017Q05NA", "AUTO_SC017Q07NA"),
                       talis = c("tr_vid_A", "tr_vid_H", "tr_vid_D", "tr_vid_J"),
                       simptoms = c("trūkst skolotāju", "trūkst atbalsta personāla", "trūkst mācību materiālu", "trūkst infrastruktūras"))
SC_T <- merge(VERT[limenis == C & kods %in% PISA_MAP$talis, .(talis = kods, cikls, talis_lva = lva, talis_lva_se = lva_se, talis_citas = sal_vid, talis_citas_min = sal_min, talis_citas_max = sal_max, talis_n_valstis = n_valstis, talis_karogs = karogs)], PISA_MAP, by = "talis")
SC_P <- merge(P[, .(pisa = mainigais, gads, pisa_lva_A = lva, pisa_lva_A_se = lva_se, pisa_7v = sal_vid)], PISA_MAP, by = "pisa")
SC <- rbind(SC_T[, .(simptoms, avots = "TALIS direktori (visas ISCED 2 skolas)", gads = cikls, lva = talis_lva, lva_se = talis_lva_se, citas = talis_citas, citas_min = talis_citas_min, citas_max = talis_citas_max, n_valstis = talis_n_valstis, karogs = talis_karogs)],
            SC_P[, .(simptoms, avots = "PISA direktori (latviešu valodas plūsma)", gads, lva = pisa_lva_A, lva_se = pisa_lva_A_se, citas = pisa_7v, citas_min = NA_real_, citas_max = NA_real_, n_valstis = 8L, karogs = NA_character_)])
setorder(SC, simptoms, avots, gads)
fwrite(SC, "output/pisa2025/talis_rinda_sc017.csv")
cat("Skala abos 1 = netraucē nemaz … 4 = traucē ļoti (TALIS 2013 vidējās kategorijas citādas, sk. karti). PISA: 7 salīdzinājuma valstis (ar DEU, POL).\n")
print(SC[, .(simptoms, avots, gads, lva = round(lva, 2), lva_se = round(lva_se, 3), citas = round(citas, 2), karogs)])
cat("Saglabāts: output/pisa2025/talis_rinda_sc017.csv\n")
cat("\nTALIS 'traucē ļoti' (4) skolu daļa, salīdzināma visos ciklos:\n")
print(dcast(VERT[limenis == C & kods %in% c("tr_loti_A", "tr_loti_D", "tr_loti_H", "tr_loti_B", "tr_loti_E", "tr_loti_J"), .(kods, cikls, v = paste0(fmt(lva, veids), " / ", fmt(sal_vid, veids), " (", karogs, ")"))], kods ~ cikls, value.var = "v"))

# ============================================================================
# 7. Grafiki
# ============================================================================
cat("\n", strrep("=", 70), "\n7. Grafiki\n", strrep("=", 70), "\n\n", sep = "")
tema <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.title.position = "plot")
XLAB <- c(`2013` = "TALIS\n2013", `2018` = "TALIS\n2018", `2024` = "TALIS\n2024")
AV <- "Avots: OECD, TALIS 2013/2018/2024 Database"
PAR <- "x: TALIS cikls; PISA paaudzes tobrīd 7.–9. klasē: 2013 = PISA 2015 paaudze (7. kl.), 2018 = PISA 2018 paaudze (9. kl.), 2024 = PISA 2025 paaudze (8. kl.)."
L <- VERT[, .(cikls, limenis, kods, raditajs, veids, virziens, karogs, lva, lva_se, sal_vid, sal_se, n_valstis)]
L <- melt(L, id.vars = c("cikls", "limenis", "kods", "raditajs", "veids", "virziens", "karogs", "n_valstis"),
          measure.vars = list(est = c("lva", "sal_vid"), se = c("lva_se", "sal_se")), variable.name = "kas")
L[, kas := factor(fifelse(kas == 1, "Latvija", "salīdzinājuma valstu vidējais"), levels = c("Latvija", "salīdzinājuma valstu vidējais"))]
L[, `:=`(x = fifelse(veids == "daļa", 100 * est, est), xse = fifelse(veids == "daļa", 100 * se, se))]
L[, cikls_f := factor(XLAB[as.character(cikls)], levels = XLAB)]
KR <- c(Latvija = "#c0392b", `salīdzinājuma valstu vidējais` = "#2c7fb8")
rindas_grafiks <- function(kodi, virsr, fails, ncol = 4, h = 7, w = 12) {
  G5 <- L[kods %in% kodi]
  G5[, lab := factor(raditajs, levels = unique(KARTE[kods %in% kodi][match(kodi, kods)]$raditajs))]
  G5[, lab := gsub(", daļa$| \\(vid\\.\\)$", "", lab)]
  G5[, lab := factor(lab, levels = unique(lab[order(match(kods, kodi))]))]
  SR <- G5[kas == "Latvija" & karogs == "SARKANS"]
  p <- ggplot(G5, aes(x = cikls_f, y = x, colour = kas, group = kas)) +
    geom_ribbon(aes(ymin = x - 1.96 * xse, ymax = x + 1.96 * xse, fill = kas), alpha = 0.15, colour = NA) +
    geom_line() + geom_point(size = 2) +
    geom_point(data = SR, shape = 21, size = 4.5, stroke = 1.1, colour = "#c0392b", fill = NA) +
    facet_wrap(~ lab, scales = "free_y", ncol = ncol, labeller = label_wrap_gen(38)) +
    scale_colour_manual(values = KR, name = NULL) + scale_fill_manual(values = KR, name = NULL) +
    labs(x = NULL, y = "% (daļas) vai vidējais", title = virsr,
         subtitle = paste0("ISCED 2; 95 % intervāls (BRR-Fay); aplis = Latvija sarkana (sliktākā vai 2. sliktākā, |z| > 1,96).\nSalīdzinājums: 2013 EST, FIN, CZE, POL; 2018 EST, LTU, FIN, CZE, HUN; 2024 EST, LTU, FIN, CZE, HUN, POL. ", PAR),
         caption = AV) +
    tema + theme(strip.text = element_text(size = 8), axis.text.x = element_text(size = 7), plot.subtitle = element_text(size = 8))
  ggsave(fails, p, width = w, height = h, dpi = 200)
  cat("Saglabāts:", fails, "\n")
}
# 7a. Hroniskie un jaunie sarkanie
SARK <- W[!tips %in% c("nav sarkans", "nesalīdzināms starp cikliem")][order(limenis, tips, -abs(fifelse(is.na(z_2018), z_2024, z_2018)))]
rindas_grafiks(SARK[limenis == T]$kods, "TALIS: Latvijas sarkanie skolotāju rādītāji 2013 → 2018 → 2024 (hroniskie un jaunie)",
               "output/pisa2025/plots/77b_talis_rinda_sarkanie_skolotaji.png", ncol = 5, h = max(6, 2.2 * ceiling(nrow(SARK[limenis == T]) / 5) + 1.5), w = 14)
if (nrow(SARK[limenis == C])) rindas_grafiks(SARK[limenis == C]$kods, "TALIS: Latvijas sarkanie direktoru rādītāji 2013 → 2018 → 2024",
               "output/pisa2025/plots/77b_talis_rinda_sarkanie_direktori.png", ncol = 4, h = max(5, 2.2 * ceiling(nrow(SARK[limenis == C]) / 4) + 1.5), w = 12)
# 7b. Stress un noturība 2018 → 2024
rindas_grafiks(c("stress_daudz", "stress_gara_vesel", "stress_atbildiba", "stress_disc", "stress_prasibas", "stress_admin", "stress_stundas", "stress_vecaki",
                 "alga_apm", "izveletos_atkal", "vertets_sab", "pamest_5", "paliek_7", "gadi_paliks", "vec_50plus", "vec_lidz30"),
               "TALIS: stress, atzinība, nodoms palikt un vecums, Latvija pret salīdzinājuma valstīm",
               "output/pisa2025/plots/77b_talis_rinda_stress_noturiba.png", ncol = 4, h = 9, w = 12)
# 7c. Prof. attīstības vajadzības trīs ciklos
rindas_grafiks(paste0("pd_vaj_", names(PD_JOMAS)), "TALIS: augsta vajadzība pēc profesionālās attīstības pa jomām, 2013 → 2018 → 2024",
               "output/pisa2025/plots/77b_talis_rinda_pd_vajadzibas.png", ncol = 4, h = 7.5, w = 12)
# 7d. Slodze, laiks stundā, vērtēšana, sadarbība
rindas_grafiks(c("pilna_slodze", "cita_skola", "stundas_kopa", "stundas_maca", "laiks_maciba", "laiks_disciplina", "vert_rakstiski", "vert_pasverte",
                 "sad_prof_maciba", "sad_vero", "pd_kursi", "mentors"),
               "TALIS: slodze, laiks stundā, vērtēšana, sadarbība, 2013 → 2018 → 2024",
               "output/pisa2025/plots/77b_talis_rinda_slodze_prakses.png", ncol = 4, h = 7.5, w = 12)
# 7e. Direktoru resursu trūkums + PISA SC017
SCg <- copy(SC); SCg[, avots_f := factor(avots)]
SCg <- melt(SCg, id.vars = c("simptoms", "avots", "gads"), measure.vars = c("lva", "citas"), variable.name = "kas", value.name = "v")
SCg[, kas := factor(fifelse(kas == "lva", "Latvija", "salīdzinājuma valstu vidējais"), levels = names(KR))]
SCg[, se := fifelse(kas == "Latvija", SC[match(paste(SCg$simptoms, SCg$avots, SCg$gads), paste(simptoms, avots, gads))]$lva_se, NA_real_)]
p <- ggplot(SCg, aes(x = gads, y = v, colour = kas, linetype = avots, shape = avots)) +
  geom_line() + geom_point(size = 2.2) +
  geom_errorbar(data = SCg[!is.na(se)], aes(ymin = v - 1.96 * se, ymax = v + 1.96 * se), width = 0.4, show.legend = FALSE) +
  facet_wrap(~ simptoms, ncol = 4) + scale_colour_manual(values = KR, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2015, 2018, 2022, 2024.5), labels = c("TALIS\n2013", "PISA\n2015", "TALIS/PISA\n2018", "PISA\n2022", "TALIS 2024/\nPISA 2025")) +
  labs(x = NULL, y = "1 = netraucē nemaz … 4 = traucē ļoti (vid.)", linetype = NULL, shape = NULL,
       title = "Direktoru vērtētais resursu trūkums: TALIS 2013–2024 (visas ISCED 2 skolas) blakus PISA SC017 2015–2025 (latviešu valodas plūsma)",
       subtitle = "TALIS 2013 skala ar citām vidējām kategorijām (aptuvens). PISA salīdzinājums: 7 valstis (ar DEU, POL). TALIS 2024 = 2023./24. m. g. (PISA 2025 paaudze 8. klasē), PISA 2025 = 2025. g. pavasaris.",
       caption = paste0(AV, "; OECD PISA 2015–2025 Database")) +
  tema + theme(axis.text.x = element_text(size = 7), legend.box = "vertical", plot.subtitle = element_text(size = 8))
ggsave("output/pisa2025/plots/77b_talis_rinda_resursi_sc017.png", p, width = 13, height = 5.5, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77b_talis_rinda_resursi_sc017.png\n")
# 7f. z karte: visi rādītāji, kas ir vismaz 2 ciklos
Z <- VERT[virziens != 0 & kods %in% VERT[, .N, by = kods][N >= 2]$kods]
Z[, z_slikt := -z * virziens]   # + = Latvijai sliktāk
Z[, z_slikt := pmax(pmin(z_slikt, 12), -12)]
Z[, lab := gsub(", daļa$| \\(vid\\.\\)$|, skolu daļa$", "", raditajs)]
Z[, lab := paste0(lab, " [", substr(limenis, 1, 3), "]")]
Z[, lab := factor(lab, levels = rev(unique(Z[order(joma, kods)]$lab)))]
Z[, cikls_f := factor(XLAB[as.character(cikls)], levels = XLAB)]
p <- ggplot(Z, aes(x = cikls_f, y = lab, fill = z_slikt)) + geom_tile(colour = "white") +
  geom_text(aes(label = fifelse(karogs == "SARKANS", "S", fifelse(karogs == "ZAĻŠ", "Z", ""))), size = 2.6) +
  scale_fill_gradient2(low = "#27ae60", mid = "grey95", high = "#c0392b", midpoint = 0, limits = c(-12, 12), name = "z (Latvija pret citām; + = Latvijai sliktāk; robežots ±12)") +
  labs(x = NULL, y = NULL, title = "TALIS: Latvijas novirze no salīdzinājuma valstīm katrā ciklā (z), rādītāji vismaz 2 ciklos",
       subtitle = paste0("S = sarkans, Z = zaļš (Latvija sliktākā / labākā vai 2., |z| > 1,96).\n", sub("; 2018", ";\n2018", PAR)), caption = AV) +
  tema + theme(axis.text.y = element_text(size = 6.5), axis.text.x = element_text(size = 8), legend.key.width = unit(1.5, "cm"))
ggsave("output/pisa2025/plots/77b_talis_rinda_z_karte.png", p, width = 10, height = max(8, 0.16 * uniqueN(Z$lab) + 2), dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77b_talis_rinda_z_karte.png\n")
# 7g. 2024 tikai: pārmaiņu nogurums un jaunie stresa vienumi, 7 valstis
V24 <- REZ[cikls == 2024 & kods %in% c(paste0("maina_", letters[1:6]), "stress_progr_mainas", "stress_labklajiba", "stress_pl", "stress_traucejumi", "dir_aizgajuso_dala", "dir_jauno_dala")]
V24[, lab := gsub(", daļa$| \\(vid\\.\\)$|Pārmaiņu nogurums: |Stress: ", "", raditajs)]
V24[, x := fifelse(veids == "daļa", 100 * est, est)]; V24[, xse := fifelse(veids == "daļa", 100 * se, se)]
V24[, lab := factor(lab, levels = unique(lab))]
VKR <- c(Latvija = "#c0392b", Igaunija = "#2c7fb8", Lietuva = "#7fcdbb", Somija = "#41ab5d", Čehija = "#984ea3", Ungārija = "#ff7f00", Polija = "#a65628")
p <- ggplot(V24, aes(x = x, y = valsts, colour = valsts)) +
  geom_errorbar(aes(xmin = x - 1.96 * xse, xmax = x + 1.96 * xse), width = 0.2, orientation = "y") + geom_point(size = 2.2) +
  facet_wrap(~ lab, scales = "free_x", ncol = 4, labeller = label_wrap_gen(40)) + scale_colour_manual(values = VKR, guide = "none") +
  labs(x = "% (piekrīt / diezgan daudz vai ļoti) vai attiecība", y = NULL, title = "TALIS 2024 jaunie vienumi: pārmaiņu nogurums, programmas pārmaiņu stress, skolotāju aizplūšana (2023./24. m. g.)",
       subtitle = "ISCED 2; 95 % intervāls (BRR-Fay). Pārmaiņu nogurums: forma B nav (2/3 izlases). PISA 2025 paaudze tobrīd 8. klasē.", caption = AV) + tema + theme(strip.text = element_text(size = 8))
ggsave("output/pisa2025/plots/77b_talis_rinda_2024_parmainas.png", p, width = 12, height = 7, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77b_talis_rinda_2024_parmainas.png\n")

# ============================================================================
# 8. Kopsavilkums
# ============================================================================
cat("\n", strrep("=", 70), "\n8. Kopsavilkums\n", strrep("=", 70), "\n\n", sep = "")
cat("Rādītāji:", nrow(KARTE), "(visos 3 ciklos:", sum(KARTE$ciklu_skaits == 3), ").\n")
print(VERT[, .N, by = .(cikls, limenis, karogs)][order(cikls, limenis, karogs)])
cat("\nSarkano tipi:\n"); print(W[tips != "nav sarkans", .(kods, tips)][order(tips)], nrows = 200)
cat("\nIzvades faili:\n  output/pisa2025/talis_vienumu_karte.csv\n  output/pisa2025/talis_rinda_raditaji.csv\n  output/pisa2025/talis_rinda_vertejums.csv\n")
cat("  output/pisa2025/talis_rinda_parmainas.csv\n  output/pisa2025/talis_rinda_sc017.csv\n  output/pisa2025/plots/77b_talis_rinda_*.png\n  ", output_file, "\n")
cat("\nAvots: OECD, TALIS 2013/2018/2024 Database.\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
