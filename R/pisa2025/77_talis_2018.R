# ============================================================================
# 77. Slānis 5, 2. kārta. Skolotāju puses simptomi: TALIS 2018 (ISCED 2)
#     Latvija pret 5 salīdzinājuma valstīm (EST, LTU, FIN, CZE, HUN)
# PISA datu analīze 2025
# ============================================================================
#
# Dati: OECD, TALIS 2018 Database (data/TALIS/2018/raw/BTGINTT3.sav skolotāji,
#       BCGINTT3.sav direktori). Latvija: 2 315 skolotāji 135 skolās, 137 direktori.
#       Anketas aizpildītas 2017./18. mācību gadā.
# Svari: skolotājiem TCHWGT, direktoriem SCHWGT. Standartkļūda: BRR ar Feja
#       korekciju, k = 0,5, G = 100 replikāti (TRWGT1–100 / SRWGT1–100):
#       Var = 1 / (G (1 − k)^2) * sum (t_g − t)^2 = sum(...) / 25
#       (TALIS 2018 User Guide, 3.3.3. sadaļa).
# Salīdzinājuma valstis: EST, LTU, FIN, CZE, HUN (DEU un POL 2018. gadā
#       nepiedalījās). Kopā 6 valstis.
# TALIS skalas (T3*): visas, izņemot T3TEAM, ir centrētas katrā valstī (tikai
#       metriska vai konfigurāla invariance), tāpēc starp valstīm salīdzina
#       tikai anketas vienumus; skalas lieto Latvijas iekšējiem salīdzinājumiem.
#
# Ko rēķina:
#  1. Skolotāju rādītāji 6 valstīs: sastāvs, slodze, profesionālā attīstība
#     (dalība un vajadzības), pašefektivitāte, apmierinātība, stress, nodoms
#     palikt, mācīšanas prakses, laiks stundā, vērtēšana, sadarbība, klases
#     sastāvs. Lasīšanas/valodas skolotāji (TT3G15A2) atsevišķi.
#  2. Direktoru rādītāji: resursu trūkums (TC3G29), klimats, vadība, novērtēšana.
#     TALIS TC3G29 blakus PISA 2018 SC017 (output/pisa2025/skola2030_simptomi.csv).
#  3. Latvijas iekšējā heterogenitāte: vieta, skolas lielums, citas dzimtās
#     valodas skolēnu daļa (TC3G17A; mācību valodas mainīgā datos NAV), skolotāju
#     vecums, lasīšanas skolotāji.
#  4. Sarkanie / zaļie rādītāji: Latvija sliktākā vai 2. sliktākā (labākā vai
#     2. labākā) no 6 un starpība pret 5 valstu vidējo nozīmīga (|z| > 1,96).
#  5. Saikne ar paaudzēm: 2017./18. m. g. PISA 2018 paaudze (dz. 2002) bija
#     9. klasē, PISA 2022 (dz. 2006) 5. klasē, PISA 2025 (dz. 2009) 2. klasē.
#     ISCED 2 Latvijā = 7.–9. klase. PISA 2025 paaudze 7.–9. klasē mācījās
#     2022./23.–2024./25.; rēķina, cik 2018. gada skolotāju plānoja vēl strādāt.
#
# Izvade: output/pisa2025/talis2018_raditaji.csv
#         output/pisa2025/talis2018_vertejums.csv
#         output/pisa2025/talis2018_latvija_grupas.csv
#         output/pisa2025/talis2018_pisa_sc017.csv
#         output/pisa2025/plots/77_talis_*.png
#         output/pisa2025/logs/77_talis_output.txt
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
output_file <- "output/pisa2025/logs/77_talis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("77. TALIS 2018 skolotāju puses simptomi,", format(Sys.time()), "\n")
cat("Darba direktorija:", getwd(), "\n\n")

VALSTIS <- c("LVA", "EST", "LTU", "FIN", "CZE", "HUN")
VNOS <- c(LVA = "Latvija", EST = "Igaunija", LTU = "Lietuva", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija")
SALID <- setdiff(VALSTIS, "LVA")
G <- 100; FAY <- 0.5

# ============================================================================
# 0. Palīgfunkcijas
# ============================================================================
num <- function(x) { x <- as.numeric(zap_labels(x)); x }
# vienumu kodi 1..6 (7, 8, 9, 96–99, 996–999 ir trūkstošie; haven tos jau
# pārvērš NA, bet drošības pēc)
vienums <- function(x, maks = 6) { x <- num(x); x[!is.na(x) & (x > maks | x < 1)] <- NA; x }
skala <- function(x) { x <- num(x); x[!is.na(x) & x > 900] <- NA; x }
dala <- function(x, nosac) { x <- vienums(x); ifelse(is.na(x), NA, as.numeric(nosac(x))) }

# BRR-Fay novērtējums svērtam vidējam (arī daļai, ja x ir 0/1)
brr <- function(x, W) {
  ok <- !is.na(x)
  n <- sum(ok)
  if (n < 10) return(c(est = NA_real_, se = NA_real_, n = n))
  xs <- x[ok]; Ws <- W[ok, , drop = FALSE]
  m <- colSums(Ws * xs) / colSums(Ws)
  se <- sqrt(sum((m[-1] - m[1])^2) / (G * (1 - FAY)^2))
  c(est = unname(m[1]), se = se, n = n)
}

# visi rādītāji vienai grupai: X matrica (n x p), W (n x 101)
brr_grupa <- function(X, W) {
  r <- t(vapply(seq_len(ncol(X)), function(j) brr(X[, j], W), numeric(3)))
  data.table(kods = colnames(X), est = r[, 1], se = r[, 2], n = as.integer(r[, 3]))
}

# ============================================================================
# 1. Datu ielāde
# ============================================================================
cat(strrep("=", 70), "\n1. Datu ielāde\n", strrep("=", 70), "\n\n", sep = "")

T_KOLONNAS <- c("CNTRY", "IDSCHOOL", "IDTEACH", "IDLANG", "TCHAGEGR", "TT3G01", "TT3G03", "TT3G09",
  "TT3G10A", "TT3G10B", "TT3G12", "TT3G11B", "TT3G15A2", "TT3G16", "TT3G17",
  paste0("TT3G06", c("E", "F", "H"), "1"),
  paste0("TT3G22", LETTERS[1:10]), paste0("TT3G23", LETTERS[1:14]), "TT3G25",
  paste0("TT3G27", LETTERS[1:14]), paste0("TT3G28", LETTERS[1:7]),
  "TT3G19A3", "TT3G21A", "TT3G29A4", "TT3G30",
  paste0("TT3G33", LETTERS[1:8]), paste0("TT3G34", LETTERS[1:13]),
  paste0("TT3G35", LETTERS[1:8]), "TT3G37", "TT3G38",
  paste0("TT3G39", c("A", "B", "C")), paste0("TT3G41", c("A", "B", "C", "D")),
  paste0("TT3G42", LETTERS[1:16]), paste0("TT3G43", c("A", "B", "C", "D")),
  "TT3G50", paste0("TT3G51", c("A", "B", "C", "D")), paste0("TT3G52", LETTERS[1:11]),
  paste0("TT3G53", LETTERS[1:10]), paste0("TT3G54", LETTERS[1:5]), paste0("TT3G55", LETTERS[1:9]),
  "T3SELF", "T3SEENG", "T3SEINS", "T3SECLS", "T3JOBSA", "T3JSENV", "T3JSPRO", "T3VALP",
  "T3WLOAD", "T3STBEH", "T3WELS", "T3CLASM", "T3COGAC", "T3CLAIN", "T3TPRA", "T3DISC",
  "T3COOP", "T3EXCH", "T3COLES", "T3PDPED", "T3PDIV", "T3EFFPD", "T3PDBAR", "T3STUD",
  "T3TEAM", "T3STAKE", "T3SATAT",
  "TCHWGT", paste0("TRWGT", 1:100))

t0 <- Sys.time()
TT <- as.data.table(read_sav("data/TALIS/2018/raw/BTGINTT3.sav", col_select = all_of(T_KOLONNAS)))
TT <- TT[CNTRY %in% VALSTIS]
cat("Skolotāju fails ielādēts:", format(round(difftime(Sys.time(), t0, units = "secs"))), "s\n")
print(TT[, .(skolotaji = .N, skolas = uniqueN(IDSCHOOL)), by = CNTRY][order(CNTRY)])
cat("\nLatvijas anketas valoda (IDLANG):\n"); print(table(num(TT[CNTRY == "LVA"]$IDLANG)))
cat("  54 = Latvian (Latvia). Krievu anketas versijas Latvijā nav; mācību valodas mainīgā datos nav.\n\n")

C_KOLONNAS <- c("CNTRY", "IDSCHOOL", "PRAGEGR", "TC3G01", "TC3G10", "SCHLOC", "NENRSTUD", "TC3G12",
  "TC3G13A", "TC3G13B", paste0("TC3G17", LETTERS[1:5]), paste0("TC3G22", LETTERS[1:11]),
  "TC3G23A", "TC3G24A6", paste0("TC3G25", LETTERS[1:8]), paste0("TC3G26", LETTERS[1:11]),
  paste0("TC3G27", LETTERS[1:7]), paste0("TC3G28", LETTERS[1:4]), paste0("TC3G29", LETTERS[1:15]),
  paste0("TC3G30", LETTERS[1:7]), "TC3G31A", "TC3G34", paste0("TC3G43", LETTERS[1:9]),
  paste0("TC3G44", LETTERS[1:10]), paste0("TC3G45", LETTERS[1:5]),
  "STRATIO", "TPRATIO", "T3PLACRE", "T3PLACMA", "T3PLACPE", "T3PLACSN", "T3PLEADS", "T3PACAD",
  "T3PDELI", "T3PORGIN", "T3PJOBSA", "T3PWLOAD", "T3PCOM", "T3PLEADP",
  "SCHWGT", paste0("SRWGT", 1:100))
CC <- as.data.table(read_sav("data/TALIS/2018/raw/BCGINTT3.sav", col_select = all_of(C_KOLONNAS)))
CC <- CC[CNTRY %in% VALSTIS]
cat("Direktoru fails ielādēts.\n"); print(CC[, .N, by = CNTRY][order(CNTRY)])

# ============================================================================
# 2. Skolotāju rādītāju definīcijas
# ============================================================================
cat("\n", strrep("=", 70), "\n2. Skolotāju rādītāji\n", strrep("=", 70), "\n\n", sep = "")

# virziens: +1 augstāks = labāk, -1 augstāks = sliktāk, 0 aprakstošs (vērtējumu nedod)
# veids: "daļa" (0–1), "vid" (vidējais), "skala" (TALIS IRT/CFA skala, vid ~ 10)
def_t <- list()
d <- function(kods, apraksts, joma, veids, virziens, f) def_t[[kods]] <<- list(apraksts = apraksts, joma = joma, veids = veids, virziens = virziens, f = f)
ge <- function(k) function(x) x >= k
eq <- function(k) function(x) x == k
le <- function(k) function(x) x <= k

# A. Sastāvs un slodze
d("vec_50plus", "Skolotāji 50 gadi un vecāki, daļa", "A. Sastāvs", "daļa", -1, function(D) dala(D$TCHAGEGR, ge(5)))
d("vec_60plus", "Skolotāji 60 gadi un vecāki, daļa", "A. Sastāvs", "daļa", -1, function(D) dala(D$TCHAGEGR, ge(6)))
d("vec_lidz30", "Skolotāji jaunāki par 30, daļa", "A. Sastāvs", "daļa", 1, function(D) dala(D$TCHAGEGR, le(2)))
d("sievietes", "Sievietes, daļa", "A. Sastāvs", "daļa", 0, function(D) dala(D$TT3G01, eq(1)))
d("stazs_vid", "Darba stāžs skolotāja profesijā, gadi (vid.)", "A. Sastāvs", "vid", 0, function(D) skala(D$TT3G11B))
d("stazs_le5", "Stāžs 5 gadi vai mazāk, daļa", "A. Sastāvs", "daļa", 0, function(D) { x <- skala(D$TT3G11B); ifelse(is.na(x), NA, as.numeric(x <= 5)) })
d("stazs_ge20", "Stāžs 20 gadi vai vairāk, daļa", "A. Sastāvs", "daļa", 0, function(D) { x <- skala(D$TT3G11B); ifelse(is.na(x), NA, as.numeric(x >= 20)) })
d("magistrs", "Maģistra vai augstāks grāds (ISCED 7–8), daļa", "A. Sastāvs", "daļa", 1, function(D) { x <- num(D$TT3G03); x[x > 7] <- NA; ifelse(is.na(x), NA, as.numeric(x >= 6)) })
d("pastavigs", "Pastāvīgs darba līgums, daļa", "A. Sastāvs", "daļa", 1, function(D) dala(D$TT3G09, eq(1)))
d("pilna_slodze", "Pilna slodze šajā skolā (> 90 %), daļa", "A. Sastāvs", "daļa", 1, function(D) dala(D$TT3G10A, eq(1)))
d("slodze_zem50", "Slodze šajā skolā zem 50 %, daļa", "A. Sastāvs", "daļa", -1, function(D) dala(D$TT3G10A, eq(4)))
d("pilna_kopa", "Pilna slodze visos darbos kopā, daļa", "A. Sastāvs", "daļa", 1, function(D) dala(D$TT3G10B, eq(1)))
d("cita_skola", "Strādā par skolotāju arī citā skolā, daļa", "A. Sastāvs", "daļa", -1, function(D) dala(D$TT3G12, eq(1)))
d("stundas_kopa", "Darba stundas nedēļā kopā (vid.)", "A. Sastāvs", "vid", 0, function(D) { x <- skala(D$TT3G16); x[x > 100] <- NA; x })
d("stundas_maca", "Mācīšanas stundas nedēļā (vid.)", "A. Sastāvs", "vid", 0, function(D) { x <- skala(D$TT3G17); x[x > 100] <- NA; x })

# B. Profesionālā attīstība
d("pd_jebkura", "Piedalījies jebkādā prof. attīstībā pēdējā gadā, daļa", "B. Prof. attīstība", "daļa", 1,
  function(D) { M <- sapply(paste0("TT3G22", LETTERS[1:10]), function(v) vienums(D[[v]])); r <- apply(M, 1, function(z) if (all(is.na(z))) NA else as.numeric(any(z == 1, na.rm = TRUE))); r })
d("pd_klatiene", "Kursi vai semināri klātienē, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G22A, eq(1)))
d("pd_tikls", "Dalība skolotāju tīklā, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G22H, eq(1)))
d("pd_koleg_verosana", "Kolēģu vērošana un koučings, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G22G, eq(1)))
d("pd_ietekme", "Prof. attīstība pozitīvi ietekmēja mācīšanu, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G25, eq(1)))
d("T3EFFPD", "Efektīva prof. attīstība (skala)", "B. Prof. attīstība", "skala", 1, function(D) skala(D$T3EFFPD))
PD_JOMAS <- c(A = "priekšmeta zināšanas", B = "priekšmeta didaktika", C = "mācību saturs (programma)", D = "skolēnu vērtēšana",
  E = "IKT mācīšanai", F = "uzvedība un klases vadība", H = "individualizēta mācīšana", I = "skolēni ar īpašām vajadzībām",
  J = "daudzkultūru / daudzvalodu vide", K = "starppriekšmetu prasmes", L = "vērtējumu analīze un lietošana", M = "sadarbība ar vecākiem")
for (k in names(PD_JOMAS)) {
  local({ kk <- k
    d(paste0("pd_dal_", kk), paste0("Piedalījās prof. attīstībā: ", PD_JOMAS[kk], ", daļa"), "B. Prof. attīstība", "daļa", 1, function(D) dala(D[[paste0("TT3G23", kk)]], eq(1)))
    d(paste0("pd_vaj_", kk), paste0("Augsta vajadzība pēc prof. attīstības: ", PD_JOMAS[kk], ", daļa"), "B. Prof. attīstība", "daļa", -1, function(D) dala(D[[paste0("TT3G27", kk)]], eq(4)))
  })
}
d("T3PDPED", "Vajadzība pēc prof. attīstības priekšmetā un pedagoģijā (skala)", "B. Prof. attīstība", "skala", -1, function(D) skala(D$T3PDPED))
d("T3PDIV", "Vajadzība pēc prof. attīstības darbam ar dažādību (skala)", "B. Prof. attīstība", "skala", -1, function(D) skala(D$T3PDIV))
d("T3PDBAR", "Šķēršļi prof. attīstībai (skala)", "B. Prof. attīstība", "skala", -1, function(D) skala(D$T3PDBAR))
d("pd_skersl_laiks", "Šķērslis: nesaskan ar darba grafiku (piekrīt), daļa", "B. Prof. attīstība", "daļa", -1, function(D) dala(D$TT3G28D, ge(3)))
d("pd_skersl_darga", "Šķērslis: pārāk dārga (piekrīt), daļa", "B. Prof. attīstība", "daļa", -1, function(D) dala(D$TT3G28B, ge(3)))
d("pd_skersl_piedav", "Šķērslis: nav atbilstoša piedāvājuma (piekrīt), daļa", "B. Prof. attīstība", "daļa", -1, function(D) dala(D$TT3G28F, ge(3)))
d("pd_skersl_stimuli", "Šķērslis: nav stimulu (piekrīt), daļa", "B. Prof. attīstība", "daļa", -1, function(D) dala(D$TT3G28G, ge(3)))
d("izgl_jaukts", "Formālajā izglītībā: mācīšana dažādu spēju klasē, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G06E1, eq(1)))
d("izgl_ikt", "Formālajā izglītībā: IKT lietošana mācīšanā, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G06H1, eq(1)))
d("izgl_daudzval", "Formālajā izglītībā: daudzkultūru / daudzvalodu vide, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G06F1, eq(1)))
d("ievad_nav", "Nav bijis formālas ievadprogrammas pirmajā darbā, daļa", "B. Prof. attīstība", "daļa", -1, function(D) dala(D$TT3G19A3, eq(1)))
d("mentors", "Pašlaik ir norīkots mentors, daļa", "B. Prof. attīstība", "daļa", 1, function(D) dala(D$TT3G21A, eq(1)))

# C. Pašefektivitāte, apmierinātība, stress, nodoms palikt
d("T3SELF", "Pašefektivitāte kopā (skala)", "C. Pašefektivitāte", "skala", 1, function(D) skala(D$T3SELF))
d("T3SEENG", "Pašefektivitāte: skolēnu iesaiste (skala)", "C. Pašefektivitāte", "skala", 1, function(D) skala(D$T3SEENG))
d("T3SEINS", "Pašefektivitāte: mācīšana (skala)", "C. Pašefektivitāte", "skala", 1, function(D) skala(D$T3SEINS))
d("T3SECLS", "Pašefektivitāte: klases vadība (skala)", "C. Pašefektivitāte", "skala", 1, function(D) skala(D$T3SECLS))
d("se_motivet", "Spēj motivēt skolēnus ar zemu interesi (diezgan / ļoti), daļa", "C. Pašefektivitāte", "daļa", 1, function(D) dala(D$TT3G34E, ge(3)))
d("se_kritiski", "Spēj palīdzēt domāt kritiski (diezgan / ļoti), daļa", "C. Pašefektivitāte", "daļa", 1, function(D) dala(D$TT3G34G, ge(3)))
d("se_dazadas_str", "Spēj variēt mācīšanas stratēģijas (diezgan / ļoti), daļa", "C. Pašefektivitāte", "daļa", 1, function(D) dala(D$TT3G34L, ge(3)))
d("se_digit", "Spēj atbalstīt mācīšanos ar digitālām tehnoloģijām (diezgan / ļoti), daļa", "C. Pašefektivitāte", "daļa", 1, function(D) dala(D$TT3G34M, ge(3)))
d("T3JOBSA", "Darba apmierinātība kopā (skala)", "D. Apmierinātība", "skala", 1, function(D) skala(D$T3JOBSA))
d("T3JSENV", "Apmierinātība ar darba vidi (skala)", "D. Apmierinātība", "skala", 1, function(D) skala(D$T3JSENV))
d("T3JSPRO", "Apmierinātība ar profesiju (skala)", "D. Apmierinātība", "skala", 1, function(D) skala(D$T3JSPRO))
d("T3VALP", "Profesijas vērtība un ietekme uz politiku (skala)", "D. Apmierinātība", "skala", 1, function(D) skala(D$T3VALP))
d("vertets_sab", "Uzskata, ka profesija sabiedrībā tiek vērtēta (piekrīt), daļa", "D. Apmierinātība", "daļa", 1, function(D) dala(D$TT3G53H, ge(3)))
d("alga_apm", "Apmierināts ar algu (piekrīt), daļa", "D. Apmierinātība", "daļa", 1, function(D) dala(D$TT3G54A, ge(3)))
d("izveletos_atkal", "Izvēlētos skolotāja darbu atkal (piekrīt), daļa", "D. Apmierinātība", "daļa", 1, function(D) dala(D$TT3G53B, ge(3)))
d("nozelo", "Nožēlo, ka kļuva par skolotāju (piekrīt), daļa", "D. Apmierinātība", "daļa", -1, function(D) dala(D$TT3G53D, ge(3)))
d("cita_prof", "Domā, vai nebūtu bijis labāk izvēlēties citu profesiju (piekrīt), daļa", "D. Apmierinātība", "daļa", -1, function(D) dala(D$TT3G53F, ge(3)))
d("T3WLOAD", "Darba slodzes stress (skala)", "E. Stress", "skala", -1, function(D) skala(D$T3WLOAD))
d("T3STBEH", "Skolēnu uzvedības stress (skala)", "E. Stress", "skala", -1, function(D) skala(D$T3STBEH))
d("T3WELS", "Labklājība un stress darbā (skala; augstāks = vairāk stresa)", "E. Stress", "skala", -1, function(D) skala(D$T3WELS))
d("stress_daudz", "Izjūt stresu darbā diezgan daudz vai ļoti, daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G51A, ge(3)))
d("stress_gara_vesel", "Darbs negatīvi ietekmē garīgo veselību (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G51C, ge(3)))
d("stress_admin", "Stress: pārāk daudz administratīvā darba (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52D, ge(3)))
d("stress_prasibas", "Stress: mainīgās iestāžu prasības (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52I, ge(3)))
d("stress_spec", "Stress: stundu pielāgošana skolēniem ar īpašām vajadzībām (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52K, ge(3)))
d("stress_disc", "Stress: disciplīnas uzturēšana (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52G, ge(3)))
d("stress_atbildiba", "Stress: atbildība par skolēnu sasniegumiem (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52F, ge(3)))
d("stress_vecaki", "Stress: vecāku bažu risināšana (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52J, ge(3)))
d("stress_stundas", "Stress: pārāk daudz stundu (diezgan / ļoti), daļa", "E. Stress", "daļa", -1, function(D) dala(D$TT3G52B, ge(3)))
d("pamest_5", "Plāno strādāt par skolotāju vēl ≤ 5 gadus, daļa", "F. Nodoms palikt", "daļa", -1, function(D) { x <- skala(D$TT3G50); ifelse(is.na(x), NA, as.numeric(x <= 5)) })
d("pamest_5_zem50", "Plāno strādāt vēl ≤ 5 gadus, skolotāji līdz 49 g., daļa", "F. Nodoms palikt", "daļa", -1,
  function(D) { x <- skala(D$TT3G50); a <- vienums(D$TCHAGEGR); r <- ifelse(is.na(x), NA, as.numeric(x <= 5)); r[!is.na(a) & a >= 5] <- NA; r })
d("paliek_7", "Plāno strādāt vēl ≥ 7 gadus (līdz 2024./25., kad PISA 2025 paaudze ir 9. klasē), daļa", "F. Nodoms palikt", "daļa", 1,
  function(D) { x <- skala(D$TT3G50); ifelse(is.na(x), NA, as.numeric(x >= 7)) })
d("gadi_paliks", "Plānotie atlikušie gadi profesijā (vid.)", "F. Nodoms palikt", "vid", 1, function(D) skala(D$TT3G50))

# G. Mācīšanas prakses, laiks, disciplīna, vērtēšana, klase
d("T3COGAC", "Kognitīvā aktivizēšana (skala)", "G. Mācīšanas prakses", "skala", 1, function(D) skala(D$T3COGAC))
d("T3CLAIN", "Skaidra instrukcija (skala)", "G. Mācīšanas prakses", "skala", 1, function(D) skala(D$T3CLAIN))
d("T3CLASM", "Klases vadības prakses (skala)", "G. Mācīšanas prakses", "skala", 0, function(D) skala(D$T3CLASM))
d("T3TPRA", "Mācīšanas prakses kopā (skala)", "G. Mācīšanas prakses", "skala", 0, function(D) skala(D$T3TPRA))
d("prakse_kritiski", "Uzdevumi, kas prasa kritisko domāšanu (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42F, ge(3)))
d("prakse_bez_risin", "Uzdevumi bez acīmredzama risinājuma (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42E, ge(3)))
d("prakse_grupas", "Darbs mazās grupās ar kopīgu risinājumu (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42G, ge(3)))
d("prakse_pasi", "Skolēni paši izvēlas risināšanas procedūras (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42H, ge(3)))
d("prakse_ikdiena", "Atsauce uz ikdienas problēmām (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42M, ge(3)))
d("prakse_vingrina", "Vingrināšanās, līdz visi saprot (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 0, function(D) dala(D$TT3G42N, ge(3)))
d("prakse_projekti", "Projekti, kas prasa vismaz nedēļu (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42O, ge(3)))
d("prakse_ikt", "Skolēni lieto IKT projektiem vai klases darbam (bieži / vienmēr), daļa", "G. Mācīšanas prakses", "daļa", 1, function(D) dala(D$TT3G42P, ge(3)))
d("laiks_maciba", "Stundas laika daļa faktiskai mācīšanai, % (vid.)", "H. Laiks stundā", "vid", 1, function(D) { x <- skala(D$TT3G39C); x[x > 100] <- NA; x })
d("laiks_disciplina", "Stundas laika daļa kārtības uzturēšanai, % (vid.)", "H. Laiks stundā", "vid", -1, function(D) { x <- skala(D$TT3G39B); x[x > 100] <- NA; x })
d("laiks_admin", "Stundas laika daļa administratīviem uzdevumiem, % (vid.)", "H. Laiks stundā", "vid", -1, function(D) { x <- skala(D$TT3G39A); x[x > 100] <- NA; x })
d("T3DISC", "Disciplīnas problēmas klasē (skala; augstāks = vairāk traucējumu)", "H. Laiks stundā", "skala", -1, function(D) skala(D$T3DISC))
d("disc_zaude_laiku", "Zaudē daudz laika skolēnu traucējumu dēļ (piekrīt), daļa", "H. Laiks stundā", "daļa", -1, function(D) dala(D$TT3G41C, ge(3)))
d("disc_gaida_ilgi", "Stundas sākumā ilgi jāgaida, līdz skolēni nomierinās (piekrīt), daļa", "H. Laiks stundā", "daļa", -1, function(D) dala(D$TT3G41A, ge(3)))
d("vert_pats", "Vērtēšana: paša izstrādāti pārbaudes darbi (bieži / vienmēr), daļa", "I. Vērtēšana", "daļa", 0, function(D) dala(D$TT3G43A, ge(3)))
d("vert_rakstiski", "Vērtēšana: rakstiska atgriezeniskā saite papildus atzīmei (bieži / vienmēr), daļa", "I. Vērtēšana", "daļa", 1, function(D) dala(D$TT3G43B, ge(3)))
d("vert_pasverte", "Vērtēšana: skolēni paši vērtē savu progresu (bieži / vienmēr), daļa", "I. Vērtēšana", "daļa", 1, function(D) dala(D$TT3G43C, ge(3)))
d("vert_vero", "Vērtēšana: vēro skolēnus uzdevumu laikā un sniedz tūlītēju atgriezenisko saiti (bieži / vienmēr), daļa", "I. Vērtēšana", "daļa", 1, function(D) dala(D$TT3G43D, ge(3)))
d("klase_lielums", "Mērķa klases lielums, skolēni (vid.)", "J. Klases sastāvs", "vid", 0, function(D) { x <- skala(D$TT3G38); x[x > 60] <- NA; x })
d("klase_vaji30", "Klasē > 30 % skolēnu ar zemiem sasniegumiem, daļa", "J. Klases sastāvs", "daļa", -1, function(D) dala(D$TT3G35B, ge(4)))
d("klase_uzved11", "Klasē > 10 % skolēnu ar uzvedības problēmām, daļa", "J. Klases sastāvs", "daļa", -1, function(D) dala(D$TT3G35D, ge(3)))
d("klase_spec11", "Klasē > 10 % skolēnu ar īpašām vajadzībām, daļa", "J. Klases sastāvs", "daļa", 0, function(D) dala(D$TT3G35C, ge(3)))
d("klase_nelabv30", "Klasē > 30 % skolēnu no sociāli nelabvēlīgām ģimenēm, daļa", "J. Klases sastāvs", "daļa", 0, function(D) dala(D$TT3G35E, ge(4)))
d("klase_citaval11", "Klasē > 10 % skolēnu ar citu dzimto valodu, daļa", "J. Klases sastāvs", "daļa", 0, function(D) dala(D$TT3G35A, ge(3)))

# K. Sadarbība, atgriezeniskā saite, skolas klimats, prioritātes
d("T3COOP", "Sadarbība kopā (skala)", "K. Sadarbība", "skala", 1, function(D) skala(D$T3COOP))
d("T3EXCH", "Apmaiņa un koordinācija (skala)", "K. Sadarbība", "skala", 1, function(D) skala(D$T3EXCH))
d("T3COLES", "Profesionālā sadarbība stundās (skala)", "K. Sadarbība", "skala", 1, function(D) skala(D$T3COLES))
d("sad_kopmaca", "Māca kopā vienā klasē vismaz reizi mēnesī, daļa", "K. Sadarbība", "daļa", 1, function(D) dala(D$TT3G33A, ge(5)))
d("sad_vero", "Vēro kolēģu stundas un sniedz atgriezenisko saiti vismaz reizi mēnesī, daļa", "K. Sadarbība", "daļa", 1, function(D) dala(D$TT3G33B, ge(5)))
d("sad_materiali", "Apmainās ar mācību materiāliem vismaz reizi mēnesī, daļa", "K. Sadarbība", "daļa", 1, function(D) dala(D$TT3G33D, ge(5)))
d("sad_prof_maciba", "Piedalās kopīgā profesionālā mācīšanās vismaz reizi mēnesī, daļa", "K. Sadarbība", "daļa", 1, function(D) dala(D$TT3G33H, ge(5)))
d("atgr_nav_verosana", "Nekad nav saņēmis atgriezenisko saiti pēc stundas vērošanas, daļa", "K. Sadarbība", "daļa", -1, function(D) dala(D$TT3G29A4, eq(1)))
d("atgr_ietekme", "Atgriezeniskā saite pozitīvi ietekmēja mācīšanu, daļa", "K. Sadarbība", "daļa", 1, function(D) dala(D$TT3G30, eq(1)))
d("T3STUD", "Skolotāju un skolēnu attiecības (skala)", "L. Skolas klimats", "skala", 1, function(D) skala(D$T3STUD))
d("T3TEAM", "Komandas inovativitāte (skala)", "L. Skolas klimats", "skala", 1, function(D) skala(D$T3TEAM))
d("T3STAKE", "Iesaistīto pušu līdzdalība (skala)", "L. Skolas klimats", "skala", 1, function(D) skala(D$T3STAKE))
d("prior_materiali", "Izdevumu prioritāte: mācību materiāli (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55B, eq(3)))
d("prior_atbalsta_pers", "Izdevumu prioritāte: atbalsta personāls administratīvā sloga mazināšanai (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55I, eq(3)))
d("prior_spec", "Izdevumu prioritāte: skolēnu ar īpašām vajadzībām atbalsts (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55F, eq(3)))
d("prior_alga", "Izdevumu prioritāte: skolotāju algas (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55H, eq(3)))
d("prior_klases", "Izdevumu prioritāte: mazākas klases (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55D, eq(3)))
d("prior_ikt", "Izdevumu prioritāte: IKT (ļoti svarīgi), daļa", "M. Prioritātes", "daļa", 0, function(D) dala(D$TT3G55A, eq(3)))

DEF_T <- rbindlist(lapply(names(def_t), function(k) data.table(kods = k, raditajs = def_t[[k]]$apraksts, joma = def_t[[k]]$joma,
                                                                veids = def_t[[k]]$veids, virziens = def_t[[k]]$virziens)))
cat("Skolotāju rādītāju skaits:", nrow(DEF_T), "\n")

# rādītāju matrica
XT <- sapply(names(def_t), function(k) def_t[[k]]$f(TT))
WT <- as.matrix(TT[, c("TCHWGT", paste0("TRWGT", 1:100)), with = FALSE])
TT[, lasisanas := vienums(TT3G15A2) == 1]
cat("\nLasīšanas / valodas skolotāji (TT3G15A2 = māca lasīšanu, rakstīšanu, literatūru šajā mācību gadā):\n")
print(TT[, .(n = .N, lasisanas = sum(lasisanas, na.rm = TRUE), dala = round(mean(lasisanas, na.rm = TRUE), 3)), by = CNTRY][order(CNTRY)])

rez_t <- rbindlist(lapply(VALSTIS, function(cn) {
  i <- which(TT$CNTRY == cn)
  r <- brr_grupa(XT[i, , drop = FALSE], WT[i, , drop = FALSE]); r[, `:=`(cnt = cn, populacija = "visi skolotāji")]
  j <- which(TT$CNTRY == cn & TT$lasisanas %in% TRUE)
  r2 <- brr_grupa(XT[j, , drop = FALSE], WT[j, , drop = FALSE]); r2[, `:=`(cnt = cn, populacija = "lasīšanas / valodas skolotāji")]
  rbind(r, r2)
}))
rez_t <- merge(rez_t, DEF_T, by = "kods")
rez_t[, limenis := "skolotājs"]

# ============================================================================
# 3. Direktoru rādītāju definīcijas
# ============================================================================
cat("\n", strrep("=", 70), "\n3. Direktoru rādītāji\n", strrep("=", 70), "\n\n", sep = "")
def_c <- list()
dc <- function(kods, apraksts, joma, veids, virziens, f) def_c[[kods]] <<- list(apraksts = apraksts, joma = joma, veids = veids, virziens = virziens, f = f)
TR <- c(A = "kvalificētu skolotāju trūkums", B = "trūkst skolotāju ar kompetenci darbam ar īpašām vajadzībām", D = "mācību materiālu (mācību grāmatu) trūkums vai nepiemērotība",
  E = "digitālo tehnoloģiju trūkums vai nepiemērotība", F = "nepietiekams interneta pieslēgums", G = "bibliotēkas materiālu trūkums", H = "atbalsta personāla trūkums",
  I = "mācību telpu trūkums vai nepiemērotība", J = "fiziskās infrastruktūras trūkums vai nepiemērotība", K = "trūkst skolotāju ar kompetenci daudzvalodu vidē",
  L = "trūkst skolotāju ar kompetenci darbam ar sociāli nelabvēlīgiem skolēniem", N = "trūkst laika mācību vadībai", O = "trūkst laika darbam ar skolēniem")
for (k in names(TR)) local({ kk <- k
  dc(paste0("tr_", kk), paste0("Traucē mācīšanu diezgan daudz vai ļoti: ", TR[kk], ", skolu daļa"), "N. Resursu trūkums", "daļa", -1, function(D) dala(D[[paste0("TC3G29", kk)]], ge(3)))
  dc(paste0("tr_vid_", kk), paste0("Traucē mācīšanu (1 nemaz … 4 ļoti): ", TR[kk], " (vid.)"), "N. Resursu trūkums", "vid", -1, function(D) vienums(D[[paste0("TC3G29", kk)]], 4))
})
dc("T3PLACRE", "Resursu trūkums (skala 1–3)", "N. Resursu trūkums", "vid", -1, function(D) skala(D$T3PLACRE))
dc("T3PLACMA", "Materiālo resursu trūkums (skala 1–3)", "N. Resursu trūkums", "vid", -1, function(D) skala(D$T3PLACMA))
dc("T3PLACPE", "Pedagoģiskā personāla trūkums (skala 1–3)", "N. Resursu trūkums", "vid", -1, function(D) skala(D$T3PLACPE))
dc("T3PLACSN", "Personāla darbam ar īpašām vajadzībām trūkums (skala)", "N. Resursu trūkums", "skala", -1, function(D) skala(D$T3PLACSN))
dc("TPRATIO", "Skolotāji uz vienu pedagoģiskā atbalsta darbinieku (vid.)", "N. Resursu trūkums", "vid", -1, function(D) { x <- skala(D$TPRATIO); x[x > 200] <- NA; x })
dc("STRATIO", "Skolēni uz vienu skolotāju (vid.)", "N. Resursu trūkums", "vid", 0, function(D) { x <- skala(D$STRATIO); x[x > 100] <- NA; x })
dc("dir_50plus", "Direktori 50 gadi un vecāki, daļa", "O. Direktori", "daļa", 0, function(D) dala(D$PRAGEGR, ge(3)))
dc("dir_sievietes", "Direktores sievietes, daļa", "O. Direktori", "daļa", 0, function(D) dala(D$TC3G01, eq(1)))
dc("sast_citaval10", "Skolā > 10 % skolēnu ar citu dzimto valodu, daļa", "O. Direktori", "daļa", 0, function(D) dala(D$TC3G17A, ge(3)))
dc("sast_spec10", "Skolā > 10 % skolēnu ar īpašām vajadzībām, daļa", "O. Direktori", "daļa", 0, function(D) dala(D$TC3G17B, ge(3)))
dc("sast_nelabv30", "Skolā > 30 % skolēnu no sociāli nelabvēlīgām ģimenēm, daļa", "O. Direktori", "daļa", 0, function(D) dala(D$TC3G17C, ge(4)))
dc("klim_sadarb", "Skolā ir sadarbības kultūra ar savstarpēju atbalstu (piekrīt), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G26F, ge(3)))
dc("klim_kopatb", "Skolā ir kopīgas atbildības kultūra (piekrīt), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G26D, ge(3)))
dc("klim_noteikumi", "Uzvedības noteikumus piemēro konsekventi (piekrīt), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G26H, ge(3)))
dc("gaidas_augstas", "Skolotājiem ir augstas gaidas par skolēnu sasniegumiem (diezgan / ļoti), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G27C, ge(3)))
dc("vecaki_atbalsta", "Vecāki atbalsta skolēnu sasniegumus (diezgan / ļoti), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G27D, ge(3)))
dc("skoleni_grib", "Skolēni vēlas mācīties labi (diezgan / ļoti), daļa", "P. Skolas klimats", "daļa", 1, function(D) dala(D$TC3G27F, ge(3)))
dc("T3PACAD", "Akadēmiskais spiediens (skala)", "P. Skolas klimats", "skala", 1, function(D) skala(D$T3PACAD))
dc("nirg_ned", "Ņirgāšanās starp skolēniem vismaz reizi nedēļā, skolu daļa", "P. Skolas klimats", "daļa", -1, function(D) dala(D$TC3G30B, ge(4)))
dc("skolot_iebied_men", "Skolotāju iebiedēšana vai apvainošana vismaz reizi mēnesī, skolu daļa", "P. Skolas klimats", "daļa", -1, function(D) dala(D$TC3G30D, ge(3)))
dc("T3PDELI", "Skolas delinkvence un vardarbība (skala)", "P. Skolas klimats", "skala", -1, function(D) skala(D$T3PDELI))
dc("T3PORGIN", "Organizācijas inovativitāte (skala)", "P. Skolas klimats", "skala", 1, function(D) skala(D$T3PORGIN))
dc("T3PLEADS", "Mācību vadība (skala)", "Q. Vadība un novērtēšana", "skala", 1, function(D) skala(D$T3PLEADS))
dc("vad_vero", "Direktors bieži vai ļoti bieži vēro stundas, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G22B, ge(3)))
dc("vad_atgr", "Direktors bieži sniedz skolotājiem atgriezenisko saiti pēc vērojumiem, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G22C, ge(3)))
dc("vad_sadarb", "Direktors bieži atbalsta skolotāju sadarbību jaunu prakšu izstrādei, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G22D, ge(3)))
dc("vad_atbild", "Direktors bieži rūpējas, lai skolotāji jūtas atbildīgi par rezultātiem, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G22F, ge(3)))
dc("nov_katru_gadu", "Direktors novērtē skolotājus vismaz reizi gadā, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G23A, ge(4)))
dc("nov_att_plans", "Pēc novērtēšanas parasti vai vienmēr izstrādā attīstības plānu, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G25B, ge(3)))
dc("nov_mentors", "Pēc novērtēšanas parasti vai vienmēr norīko mentoru, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G25D, ge(3)))
dc("nov_atlaisana", "Novērtēšanai dažreiz vai biežāk seko atlaišana vai līguma nepagarināšana, daļa", "Q. Vadība un novērtēšana", "daļa", 0, function(D) dala(D$TC3G25H, ge(2)))
dc("ievad_form", "Skolā ir formāla ievadprogramma jaunajiem skolotājiem, daļa", "Q. Vadība un novērtēšana", "daļa", 1, function(D) dala(D$TC3G31A, eq(1)))
dc("mentor_nav", "Skolā nav mentoru programmas, daļa", "Q. Vadība un novērtēšana", "daļa", -1, function(D) dala(D$TC3G34, eq(4)))
dc("T3PJOBSA", "Direktora darba apmierinātība (skala)", "R. Direktora darbs", "skala", 1, function(D) skala(D$T3PJOBSA))
dc("T3PWLOAD", "Direktora darba slodzes stress (skala)", "R. Direktora darbs", "skala", -1, function(D) skala(D$T3PWLOAD))
dc("dir_stress_admin", "Direktora stress: pārāk daudz administratīvā darba (diezgan / ļoti), daļa", "R. Direktora darbs", "daļa", -1, function(D) dala(D$TC3G43B, ge(3)))
dc("dir_stress_prasibas", "Direktora stress: mainīgās iestāžu prasības (diezgan / ļoti), daļa", "R. Direktora darbs", "daļa", -1, function(D) dala(D$TC3G43G, ge(3)))
dc("dir_stress_spec", "Direktora stress: skolēnu ar īpašām vajadzībām uzņemšana (diezgan / ļoti), daļa", "R. Direktora darbs", "daļa", -1, function(D) dala(D$TC3G43I, ge(3)))
dc("dir_vairak_atbalsta", "Direktoram vajag vairāk atbalsta no iestādēm (piekrīt), daļa", "R. Direktora darbs", "daļa", -1, function(D) dala(D$TC3G45D, ge(3)))
dc("dir_vertets_sab", "Direktors uzskata, ka skolotāja profesija sabiedrībā tiek vērtēta (piekrīt), daļa", "R. Direktora darbs", "daļa", 1, function(D) dala(D$TC3G44H, ge(3)))
dc("dir_alga_apm", "Direktors apmierināts ar algu (piekrīt), daļa", "R. Direktora darbs", "daļa", 1, function(D) dala(D$TC3G45A, ge(3)))

DEF_C <- rbindlist(lapply(names(def_c), function(k) data.table(kods = k, raditajs = def_c[[k]]$apraksts, joma = def_c[[k]]$joma,
                                                                veids = def_c[[k]]$veids, virziens = def_c[[k]]$virziens)))
cat("Direktoru rādītāju skaits:", nrow(DEF_C), "\n")
XC <- sapply(names(def_c), function(k) def_c[[k]]$f(CC))
WC <- as.matrix(CC[, c("SCHWGT", paste0("SRWGT", 1:100)), with = FALSE])
rez_c <- rbindlist(lapply(VALSTIS, function(cn) {
  i <- which(CC$CNTRY == cn)
  r <- brr_grupa(XC[i, , drop = FALSE], WC[i, , drop = FALSE]); r[, `:=`(cnt = cn, populacija = "direktori")]; r
}))
rez_c <- merge(rez_c, DEF_C, by = "kods")
rez_c[, limenis := "direktors"]

# ============================================================================
# 4. Apvienotā tabula un vērtējums (sarkanie / zaļie)
# ============================================================================
cat("\n", strrep("=", 70), "\n4. Vērtējums: Latvija pret 5 valstīm\n", strrep("=", 70), "\n\n", sep = "")
REZ <- rbind(rez_t, rez_c)
REZ[, valsts := VNOS[cnt]]
setcolorder(REZ, c("limenis", "populacija", "joma", "kods", "raditajs", "veids", "virziens", "cnt", "valsts", "est", "se", "n"))
setorder(REZ, limenis, populacija, joma, kods, cnt)
fwrite(REZ, "output/pisa2025/talis2018_raditaji.csv")
cat("Saglabāts: output/pisa2025/talis2018_raditaji.csv (", nrow(REZ), " rindas)\n", sep = "")

vertejums <- function(R) {
  R <- R[!is.na(est)]
  V <- R[, {
    lv <- .SD[cnt == "LVA"]; ci <- .SD[cnt != "LVA"]
    if (nrow(lv) != 1 || nrow(ci) < 3) NULL else {
      sal_vid <- mean(ci$est); sal_se <- sqrt(sum(ci$se^2)) / nrow(ci)
      st <- lv$est - sal_vid; st_se <- sqrt(lv$se^2 + sal_se^2)
      # rangs pēc "labuma": 1 = labākā
      v <- virziens[1]
      visi <- .SD[, .(cnt, est)]
      if (v >= 0) setorder(visi, -est) else setorder(visi, est)
      rangs <- which(visi$cnt == "LVA")
      .(lva = lv$est, lva_se = lv$se, n_lva = lv$n, sal_vid = sal_vid, sal_se = sal_se, sal_min = min(ci$est), sal_max = max(ci$est),
        n_valstis = nrow(ci) + 1, starpiba = st, starp_se = st_se, z = st / st_se, rangs_labums = rangs,
        rangs_augstums = which(.SD[order(-est)]$cnt == "LVA"))
    }
  }, by = .(limenis, populacija, joma, kods, raditajs, veids, virziens)]
  V[, nozimigs := abs(z) > 1.96]
  # TALIS IRT/CFA skalas (T3*) ar tikai metrisku vai konfigurālu invarianci ir centrētas
  # katrā valstī (svērtais vidējais visās valstīs identisks); starp valstīm salīdzināma
  # ir tikai skala ar skalāro invarianci visās populācijās (T3TEAM). Pārējās skalas der
  # tikai Latvijas iekšējiem grupu salīdzinājumiem.
  V[, salidzinams := !(veids == "skala" & !kods %in% SKALAS_SALIDZ)]
  V[, karogs := fifelse(!salidzinams, "nesalīdzināms",
               fifelse(virziens == 0, "aprakstošs",
               fifelse(rangs_labums >= n_valstis - 1 & nozimigs, "SARKANS",
               fifelse(rangs_labums <= 2 & nozimigs, "ZAĻŠ", "vidū"))))]
  V
}
SKALAS_SALIDZ <- c("T3TEAM")
cat("Pārbaude: TALIS skalu svērto vidējo izkliede starp 6 valstīm (centrētas skalas: SD ≈ 0)\n")
SKP <- REZ[veids == "skala" & populacija %in% c("visi skolotāji", "direktori"), .(sd_valstis = round(sd(est, na.rm = TRUE), 4), n_valstis = sum(!is.na(est))), by = .(kods, raditajs)][order(-sd_valstis)]
print(SKP, nrows = 100)
cat("Starp valstīm salīdzināmas skalas (skalārā invariance visās populācijās):", paste(SKALAS_SALIDZ, collapse = ", "), "\n")
cat("Pārējās T3 skalas vērtējumā atzīmētas kā nesalīdzināmas; tās lietotas tikai Latvijas grupu salīdzinājumā (6. sadaļa).\n\n")
VERT <- vertejums(REZ)
fwrite(VERT, "output/pisa2025/talis2018_vertejums.csv")
cat("Saglabāts: output/pisa2025/talis2018_vertejums.csv\n\n")

fmt <- function(x, veids) ifelse(veids == "daļa", sprintf("%.1f %%", 100 * x), sprintf("%.2f", x))
izdruka <- function(V, virsr) {
  cat("\n---", virsr, "---\n")
  if (nrow(V) == 0) { cat("(nav)\n"); return(invisible()) }
  for (i in seq_len(nrow(V))) with(V[i], cat(sprintf("  [%s] %s (%s): LVA %s (SE %s), 5 valstis %s [%s; %s], z = %.2f, rangs %d/%d\n",
    karogs, raditajs, kods, fmt(lva, veids), fmt(lva_se, veids), fmt(sal_vid, veids), fmt(sal_min, veids), fmt(sal_max, veids), z, rangs_labums, n_valstis)))
}
for (pop in c("visi skolotāji", "direktori", "lasīšanas / valodas skolotāji")) {
  cat("\n", strrep("#", 60), "\n", toupper(pop), "\n", sep = "")
  izdruka(VERT[populacija == pop & karogs == "SARKANS"], "SARKANIE (Latvija sliktākā vai 2. sliktākā no 6, |z| > 1,96)")
  izdruka(VERT[populacija == pop & karogs == "ZAĻŠ"], "ZAĻIE (Latvija labākā vai 2. labākā no 6, |z| > 1,96)")
  izdruka(VERT[populacija == pop & karogs == "vidū"], "VIDŪ (ne sarkans, ne zaļš)")
  izdruka(VERT[populacija == pop & karogs == "aprakstošs"], "APRAKSTOŠIE (bez vērtējuma)")
  izdruka(VERT[populacija == pop & karogs == "nesalīdzināms"], "NESALĪDZINĀMS STARP VALSTĪM (skala centrēta valstī)")
}

# pilna tabula pa valstīm
cat("\n\n--- Pilna tabula: visi skolotāji, 6 valstis (est; daļas %) ---\n")
W6 <- dcast(REZ[populacija == "visi skolotāji"], joma + kods + raditajs + veids ~ cnt, value.var = "est")
W6 <- W6[, c("joma", "kods", "raditajs", "veids", VALSTIS), with = FALSE]
for (cn in VALSTIS) W6[, (cn) := ifelse(veids == "daļa", round(100 * get(cn), 1), round(get(cn), 2))]
print(W6[, -"veids"], nrows = 500)
cat("\n--- Pilna tabula: direktori, 6 valstis ---\n")
W6c <- dcast(REZ[populacija == "direktori"], joma + kods + raditajs + veids ~ cnt, value.var = "est")
W6c <- W6c[, c("joma", "kods", "raditajs", "veids", VALSTIS), with = FALSE]
for (cn in VALSTIS) W6c[, (cn) := ifelse(veids == "daļa", round(100 * get(cn), 1), round(get(cn), 2))]
print(W6c[, -"veids"], nrows = 500)

# ============================================================================
# 5. TALIS TC3G29 blakus PISA 2018 SC017 (Latvija)
# ============================================================================
cat("\n", strrep("=", 70), "\n5. Direktoru resursu trūkums: TALIS 2018 TC3G29 pret PISA 2018 SC017\n", strrep("=", 70), "\n\n", sep = "")
P <- fread("output/pisa2025/skola2030_simptomi.csv")
PISA_MAP <- data.table(pisa = c("AUTO_SC017Q01NA", "AUTO_SC017Q03NA", "AUTO_SC017Q05NA", "AUTO_SC017Q07NA"),
                       talis = c("tr_vid_A", "tr_vid_H", "tr_vid_D", "tr_vid_J"),
                       simptoms = c("trūkst skolotāju", "trūkst atbalsta personāla", "trūkst mācību materiālu", "trūkst infrastruktūras"))
SC <- merge(PISA_MAP, P[gads == 2018, .(pisa = mainigais, pisa_lva_A = lva, pisa_lva_se = lva_se, pisa_7v = sal_vid)], by = "pisa")
SC <- merge(SC, VERT[populacija == "direktori", .(talis = kods, talis_lva = lva, talis_lva_se = lva_se, talis_5v = sal_vid, talis_min = sal_min, talis_max = sal_max)], by = "talis")
SC <- merge(SC, P[gads %in% c(2022, 2025), .(pisa = mainigais, gads, lva)][, dcast(.SD, pisa ~ gads, value.var = "lva")][, .(pisa, pisa_lva_A_2022 = `2022`, pisa_lva_A_2025 = `2025`)], by = "pisa")
SC <- SC[, .(simptoms, talis_kods = talis, talis_lva = round(talis_lva, 2), talis_lva_se = round(talis_lva_se, 3), talis_5v = round(talis_5v, 2),
             talis_min = round(talis_min, 2), talis_max = round(talis_max, 2),
             pisa_kods = pisa, pisa_lva_A_2018 = round(pisa_lva_A, 2), pisa_lva_A_se = round(pisa_lva_se, 3), pisa_7v_2018 = round(pisa_7v, 2),
             pisa_lva_A_2022 = round(pisa_lva_A_2022, 2), pisa_lva_A_2025 = round(pisa_lva_A_2025, 2))]
fwrite(SC, "output/pisa2025/talis2018_pisa_sc017.csv")
cat("Skala abos 1 = netraucē nemaz … 4 = traucē ļoti. TALIS: visas Latvijas ISCED 2 skolas, direktoru svari.\n")
cat("PISA: latviešu valodas plūsma, svērts pēc skolēniem; 7 valstis (ar DEU, POL).\n")
print(SC)
cat("Saglabāts: output/pisa2025/talis2018_pisa_sc017.csv\n")

# ============================================================================
# 6. Latvijas iekšējā heterogenitāte
# ============================================================================
cat("\n", strrep("=", 70), "\n6. Latvijas iekšējā heterogenitāte\n", strrep("=", 70), "\n\n", sep = "")
cat("Mācību valodas vai skolas tipa mainīgā TALIS 2018 anketā nav (pārbaudītas latviešu PQ un TQ anketas,\n")
cat("IDLANG = 54 visiem). Aizstājējs ir skolas skolēnu sastāvs: direktora atbilde TC3G17A (skolēnu daļa,\n")
cat("kuru dzimtā valoda atšķiras no mācību valodas). Tas NAV mācību valoda; skolas ar krievu mācībvalodu\n")
cat("2017./18. g. mācīja bilingvāli, tāpēc grupas nevar interpretēt kā plūsmas.\n\n")

SK <- CC[CNTRY == "LVA", .(IDSCHOOL, SCHLOC = vienums(SCHLOC, 3), NENRSTUD = vienums(NENRSTUD, 5), CITAVAL = vienums(TC3G17A, 5), TC3G10 = vienums(TC3G10, 5))]
L <- TT[CNTRY == "LVA"]
L <- merge(L, SK, by = "IDSCHOOL", all.x = TRUE)
cat("Direktora atbildes pieejamas skolotājiem:", sum(!is.na(L$SCHLOC)), "no", nrow(L), "\n")
L[, g_vieta := fcase(SCHLOC == 1, "lauki (≤ 3 000 iedz.)", SCHLOC == 2, "pilsēta (3 001–100 000)", SCHLOC == 3, "lielpilsēta (> 100 000)")]
L[, g_lielums := fcase(NENRSTUD == 1, "< 250 skolēni", NENRSTUD == 2, "250–499", NENRSTUD >= 3, "≥ 500")]
L[, g_citaval := fcase(CITAVAL <= 2, "≤ 10 % citas dzimtās valodas", CITAVAL == 3, "11–30 %", CITAVAL >= 4, "> 30 %")]
L[, g_vecums := fcase(vienums(TCHAGEGR) <= 3, "līdz 39 g.", vienums(TCHAGEGR) == 4, "40–49 g.", vienums(TCHAGEGR) >= 5, "50+ g.")]
L[, g_lasisana := fifelse(lasisanas %in% TRUE, "lasīšanas / valodas skolotāji", "citi skolotāji")]
cat("\nSkolotāju sadalījums grupās (nesvērts):\n")
for (g in c("g_vieta", "g_lielums", "g_citaval", "g_vecums", "g_lasisana")) { cat(g, ": "); print(table(L[[g]], useNA = "ifany")) }

XL <- sapply(names(def_t), function(k) def_t[[k]]$f(L))
WL <- as.matrix(L[, c("TCHWGT", paste0("TRWGT", 1:100)), with = FALSE])
GR_T <- rbindlist(lapply(c("g_vieta", "g_lielums", "g_citaval", "g_vecums", "g_lasisana"), function(g) rbindlist(lapply(sort(unique(na.omit(L[[g]]))), function(gv) {
  i <- which(L[[g]] == gv)
  r <- brr_grupa(XL[i, , drop = FALSE], WL[i, , drop = FALSE]); r[, `:=`(dimensija = g, grupa = gv, limenis = "skolotājs")]; r
}))))
# direktoru rādītāji pēc vietas, lieluma, sastāva
CL <- CC[CNTRY == "LVA"]
CL[, g_vieta := fcase(vienums(SCHLOC, 3) == 1, "lauki (≤ 3 000 iedz.)", vienums(SCHLOC, 3) == 2, "pilsēta (3 001–100 000)", vienums(SCHLOC, 3) == 3, "lielpilsēta (> 100 000)")]
CL[, g_lielums := fcase(vienums(NENRSTUD, 5) == 1, "< 250 skolēni", vienums(NENRSTUD, 5) == 2, "250–499", vienums(NENRSTUD, 5) >= 3, "≥ 500")]
CL[, g_citaval := fcase(vienums(TC3G17A, 5) <= 2, "≤ 10 % citas dzimtās valodas", vienums(TC3G17A, 5) == 3, "11–30 %", vienums(TC3G17A, 5) >= 4, "> 30 %")]
XCL <- sapply(names(def_c), function(k) def_c[[k]]$f(CL))
WCL <- as.matrix(CL[, c("SCHWGT", paste0("SRWGT", 1:100)), with = FALSE])
GR_C <- rbindlist(lapply(c("g_vieta", "g_lielums", "g_citaval"), function(g) rbindlist(lapply(sort(unique(na.omit(CL[[g]]))), function(gv) {
  i <- which(CL[[g]] == gv)
  r <- brr_grupa(XCL[i, , drop = FALSE], WCL[i, , drop = FALSE]); r[, `:=`(dimensija = g, grupa = gv, limenis = "direktors")]; r
}))))
GR <- rbind(merge(GR_T, DEF_T, by = "kods"), merge(GR_C, DEF_C, by = "kods"))
GR[, dimensija := c(g_vieta = "skolas atrašanās vieta", g_lielums = "skolas lielums", g_citaval = "citas dzimtās valodas skolēnu daļa (direktors)",
                    g_vecums = "skolotāja vecums", g_lasisana = "lasīšanas / valodas skolotāji")[dimensija]]
setcolorder(GR, c("limenis", "dimensija", "grupa", "joma", "kods", "raditajs", "veids", "virziens", "est", "se", "n"))
setorder(GR, limenis, dimensija, joma, kods, grupa)
fwrite(GR, "output/pisa2025/talis2018_latvija_grupas.csv")
cat("\nSaglabāts: output/pisa2025/talis2018_latvija_grupas.csv (", nrow(GR), " rindas)\n", sep = "")

# nozīmīgākās grupu atšķirības: katrā dimensijā max−min pret kopējo SE
GR[, `:=`(gmax = max(est, na.rm = TRUE), gmin = min(est, na.rm = TRUE)), by = .(limenis, dimensija, kods)]
GD <- GR[, {
  o <- order(est); .(grupa_min = grupa[o[1]], est_min = est[o[1]], grupa_max = grupa[o[.N]], est_max = est[o[.N]],
                     z = (est[o[.N]] - est[o[1]]) / sqrt(se[o[.N]]^2 + se[o[1]]^2), n_min = min(n))
}, by = .(limenis, dimensija, joma, kods, raditajs, veids, virziens)]
GD <- GD[!is.na(z) & n_min >= 30]
cat("\n--- Grupu atšķirības Latvijā ar |z| > 2,58 (lielākā mīnus mazākā grupa) ---\n")
GDs <- GD[abs(z) > 2.58][order(dimensija, -abs(z))]
for (i in seq_len(nrow(GDs))) with(GDs[i], cat(sprintf("  %s | %s: %s %s … %s %s (z = %.1f)\n", dimensija, raditajs, grupa_min, fmt(est_min, veids), grupa_max, fmt(est_max, veids), z)))

# ============================================================================
# 7. Saikne ar paaudzēm
# ============================================================================
cat("\n", strrep("=", 70), "\n7. Saikne ar PISA paaudzēm\n", strrep("=", 70), "\n\n", sep = "")
PA <- data.table(paaudze = c("PISA 2018", "PISA 2022", "PISA 2025"), dz_gads = c(2002, 2006, 2009),
                 klase_2017_18 = c("9. klase", "5. klase", "2. klase"),
                 isced2_gadi = c("2015/16–2017/18", "2019/20–2021/22", "2022/23–2024/25"),
                 talis_attalums = c("tieši (TALIS skolotāji māca šo paaudzi)", "2–4 gadi pēc TALIS", "5–7 gadi pēc TALIS"))
print(PA)
cat("\nTALIS 2018 ir vienīgais skolotāju mērījums starp 2013 un 2024; tas apraksta skolotāju korpusu, kas 2017./18. g.\n")
cat("mācīja PISA 2018 paaudzi un pēc tam (ar aizplūšanu un papildināšanu) PISA 2022 un 2025 paaudzes.\n")
pal <- VERT[populacija == "visi skolotāji" & kods %in% c("paliek_7", "pamest_5", "vec_50plus", "vec_60plus", "gadi_paliks")]
cat("\nSkolotāju korpusa noturība līdz 2024./25. (PISA 2025 paaudze 9. klasē):\n")
print(pal[, .(kods, raditajs, lva = round(lva, 3), sal_vid = round(sal_vid, 3), sal_min = round(sal_min, 3), sal_max = round(sal_max, 3), z = round(z, 2), rangs_labums, karogs)])

# ============================================================================
# 8. Grafiki
# ============================================================================
cat("\n", strrep("=", 70), "\n8. Grafiki\n", strrep("=", 70), "\n\n", sep = "")
tema <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.title.position = "plot")
VKRASAS <- c(Latvija = "#c0392b", Igaunija = "#2c7fb8", Lietuva = "#7fcdbb", Somija = "#41ab5d", Čehija = "#984ea3", Ungārija = "#ff7f00")

# 8a. Sarkanie un zaļie: standartizēta starpība
SZ <- VERT[karogs %in% c("SARKANS", "ZAĻŠ") & populacija %in% c("visi skolotāji", "direktori")]
SZ[, d_std := fifelse(veids == "daļa", 100 * starpiba, starpiba)]
SZ[, lab := paste0(raditajs, " [", populacija, "]")]
SZ[, lab := gsub(", daļa \\[", " [", lab)]
SZ[, virz_lab := fifelse(virziens > 0, "augstāks = labāk", "augstāks = sliktāk")]
SZ[, sliktak := (starpiba * virziens) < 0]
SZ <- SZ[order(karogs, -abs(z))]
SZ[, lab := factor(lab, levels = rev(lab))]
p <- ggplot(SZ, aes(x = z, y = lab, colour = karogs)) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, colour = "grey60") + geom_vline(xintercept = 0, colour = "grey40") +
  geom_point(size = 2.5) +
  geom_text(aes(label = ifelse(veids == "daļa", sprintf("%+.0f pp", 100 * starpiba), sprintf("%+.2f", starpiba))), hjust = ifelse(SZ$z > 0, -0.3, 1.3), size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = c(SARKANS = "#c0392b", ZAĻŠ = "#27ae60"), name = NULL) +
  scale_x_continuous("z = (Latvija − 5 valstu vidējais) / SE", expand = expansion(mult = 0.25)) +
  labs(y = NULL, title = "TALIS 2018: rādītāji, kur Latvija ir starp 2 sliktākajām (sarkans) vai 2 labākajām (zaļš) no 6 valstīm",
       subtitle = "ISCED 2 skolotāji un direktori; salīdzinājums ar EST, LTU, FIN, CZE, HUN; BRR-Fay SE; pp = procentpunkti. Avots: OECD, TALIS 2018 Database") + tema
ggsave("output/pisa2025/plots/77_talis_sarkanie_zalie.png", p, width = 12, height = max(6, 0.22 * nrow(SZ) + 2), dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77_talis_sarkanie_zalie.png\n")

# 8b. Profesionālās attīstības vajadzības (augsta vajadzība) pa jomām, 6 valstis
PDV <- REZ[populacija == "visi skolotāji" & grepl("^pd_vaj_", kods)]
PDV[, joma_lab := PD_JOMAS[sub("pd_vaj_", "", kods)]]
PDV[, joma_lab := factor(joma_lab, levels = PDV[cnt == "LVA"][order(est)]$joma_lab)]
p <- ggplot(PDV, aes(x = 100 * est, y = joma_lab, colour = valsts)) +
  geom_point(aes(size = valsts == "Latvija")) + scale_size_manual(values = c(`TRUE` = 3.5, `FALSE` = 2), guide = "none") +
  scale_colour_manual(values = VKRASAS, name = NULL) +
  labs(x = "Skolotāju daļa ar augstu vajadzību, %", y = NULL, title = "Augsta vajadzība pēc profesionālās attīstības pa jomām, TALIS 2018",
       subtitle = "ISCED 2 skolotāji; svērts, TCHWGT. Avots: OECD, TALIS 2018 Database") + tema
ggsave("output/pisa2025/plots/77_talis_pd_vajadzibas.png", p, width = 9, height = 5.5, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77_talis_pd_vajadzibas.png\n")

# 8c. Direktoru resursu trūkums, 6 valstis + PISA 2018 (LVA A)
TRD <- REZ[populacija == "direktori" & grepl("^tr_[A-Z]$", kods)]
TRD[, lab := TR[sub("tr_", "", kods)]]
TRD[, lab := factor(lab, levels = TRD[cnt == "LVA"][order(est)]$lab)]
p <- ggplot(TRD, aes(x = 100 * est, y = lab, colour = valsts)) +
  geom_point(aes(size = valsts == "Latvija")) + scale_size_manual(values = c(`TRUE` = 3.5, `FALSE` = 2), guide = "none") +
  scale_colour_manual(values = VKRASAS, name = NULL) +
  labs(x = "Skolu daļa, kur trūkums traucē mācīšanu diezgan daudz vai ļoti, %", y = NULL,
       title = "Direktoru vērtētais resursu trūkums, TALIS 2018 (TC3G29)",
       subtitle = "ISCED 2 skolas; svērts, SCHWGT. Avots: OECD, TALIS 2018 Database") + tema
ggsave("output/pisa2025/plots/77_talis_resursi.png", p, width = 9, height = 5.5, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77_talis_resursi.png\n")

# 8d. Laiks stundā un stress / apmierinātība (izvēlēti rādītāji)
IZV <- c("laiks_maciba", "laiks_disciplina", "laiks_admin", "stress_daudz", "stress_admin", "stress_prasibas", "vertets_sab", "alga_apm", "pamest_5", "paliek_7", "vec_50plus", "cita_skola")
S4 <- REZ[populacija == "visi skolotāji" & kods %in% IZV]
S4[, lab := factor(raditajs, levels = DEF_T[kods %in% IZV][match(IZV, kods)]$raditajs)]
S4[, x := fifelse(veids == "daļa", 100 * est, est)]
S4[, xse := fifelse(veids == "daļa", 100 * se, se)]
p <- ggplot(S4, aes(x = x, y = valsts, colour = valsts)) +
  geom_errorbarh(aes(xmin = x - 1.96 * xse, xmax = x + 1.96 * xse), height = 0.2) + geom_point(size = 2.5) +
  facet_wrap(~ lab, scales = "free_x", ncol = 3, labeller = label_wrap_gen(45)) +
  scale_colour_manual(values = VKRASAS, guide = "none") +
  labs(x = "% (daļas) vai vidējais", y = NULL, title = "Skolotāju laiks, stress, atzinība un nodoms palikt, TALIS 2018",
       subtitle = "ISCED 2 skolotāji; 95 % intervāls (BRR-Fay). Avots: OECD, TALIS 2018 Database") + tema + theme(strip.text = element_text(size = 8))
ggsave("output/pisa2025/plots/77_talis_stress_laiks.png", p, width = 11, height = 8, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77_talis_stress_laiks.png\n")

# 8e. Latvijas grupas: izvēlēti rādītāji pēc vietas un sastāva
IZG <- c("pd_vaj_H", "stress_daudz", "laiks_maciba", "pamest_5", "T3COGAC", "vert_rakstiski")
IZG_LAB <- c(pd_vaj_H = "augsta vajadzība:\nindividualizēta mācīšana, %", stress_daudz = "izjūt stresu diezgan\ndaudz vai ļoti, %",
             laiks_maciba = "laiks faktiskai\nmācīšanai, %", pamest_5 = "plāno strādāt\nvēl ≤ 5 gadus, %",
             T3COGAC = "kognitīvā aktivizēšana\n(skala)", vert_rakstiski = "rakstiska atgriezeniskā\nsaite bieži, %")
G5 <- GR[limenis == "skolotājs" & kods %in% IZG & dimensija %in% c("skolas atrašanās vieta", "citas dzimtās valodas skolēnu daļa (direktors)", "skolas lielums", "skolotāja vecums")]
G5[, x := fifelse(veids == "daļa", 100 * est, est)]
G5[, xse := fifelse(veids == "daļa", 100 * se, se)]
G5[, lab := factor(IZG_LAB[kods], levels = IZG_LAB[IZG])]
G5[, dim_lab := factor(sub(" \\(direktors\\)", "\n(direktora atbilde)", dimensija),
                       levels = c("skolas atrašanās vieta", "skolas lielums", "citas dzimtās valodas skolēnu daļa\n(direktora atbilde)", "skolotāja vecums"))]
p <- ggplot(G5, aes(x = x, y = grupa, colour = dim_lab)) +
  geom_errorbarh(aes(xmin = x - 1.96 * xse, xmax = x + 1.96 * xse), height = 0.2) + geom_point(size = 2.2) +
  facet_grid(dim_lab ~ lab, scales = "free", space = "free_y") +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(x = NULL, y = NULL, title = "Latvijas skolotāju rādītāji pēc skolas vietas, lieluma, skolēnu sastāva un skolotāja vecuma, TALIS 2018",
       subtitle = "ISCED 2 skolotāji, TCHWGT, 95 % intervāls (BRR-Fay). Citas dzimtās valodas daļa ir direktora atbilde TC3G17A, ne mācību valoda. Avots: OECD, TALIS 2018 Database") +
  tema + theme(strip.text = element_text(size = 8), strip.text.y = element_text(angle = 0), axis.text.y = element_text(size = 8), panel.spacing = unit(0.8, "lines"))
ggsave("output/pisa2025/plots/77_talis_latvija_grupas.png", p, width = 14, height = 7.5, dpi = 200)
cat("Saglabāts: output/pisa2025/plots/77_talis_latvija_grupas.png\n")

# ============================================================================
# 9. Kopsavilkums
# ============================================================================
cat("\n", strrep("=", 70), "\n9. Kopsavilkums\n", strrep("=", 70), "\n\n", sep = "")
cat("Rādītāju skaits:", nrow(DEF_T), "skolotāju,", nrow(DEF_C), "direktoru.\n")
print(VERT[populacija %in% c("visi skolotāji", "direktori"), .N, by = .(populacija, karogs)][order(populacija, karogs)])
cat("\nIzvades faili:\n  output/pisa2025/talis2018_raditaji.csv\n  output/pisa2025/talis2018_vertejums.csv\n  output/pisa2025/talis2018_latvija_grupas.csv\n")
cat("  output/pisa2025/talis2018_pisa_sc017.csv\n  output/pisa2025/plots/77_talis_{sarkanie_zalie,pd_vajadzibas,resursi,stress_laiks,latvija_grupas}.png\n")
cat("  output/pisa2025/logs/77_talis_output.txt\n")
cat("\nAvots: OECD, TALIS 2018 Database.\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
