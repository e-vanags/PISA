# ============================================================================
# 71. Slānis 1: izdevumi uz vienu skolēnu pret sniegumu lasīšanā, 2000–2025
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS (docs/46, Slānis 1). OECD PISA 2025 I sējuma attēls I.1.3 rāda dabaszinātņu
# sniegumu pret kumulatīvajiem izdevumiem uz vienu skolēnu 6–15 gadu vecumā (USD PPP).
# Mūsu jautājums ir par lasīšanu: (2) tas pats grafiks lasīšanai 2022. un 2025. gadā;
# (3) izdevumu un lasīšanas trajektorija Latvijai, Lietuvai, Igaunijai un 5 pārējām
# salīdzinājuma valstīm no 2000. gada; (4) Latvijas atlikums pret līkni katrā ciklā un
# vai tas laikā mainās; (5) sasaiste ar docs/45 hronoloģiju.
#
# DATI.
#   OECD Education at a Glance, izdevumi uz vienu pilna laika ekvivalenta skolēnu
#     (DSD_EAG_UOE_FIN@DF_UOE_INDIC_FIN_PERSTUD, 3.2), izvilkums
#     data/izdevumi/raw/oecd_eag_perstud_usd_ppp.csv: USD PPP faktiskajās (V) un
#     salīdzināmajās 2020. gada (Q) cenās, kā arī % no IKP uz iedzīvotāju; 1995, 2000,
#     2005, 2008–2023; 45 valstis.
#   Eurostat educ_uoe_fini04 (visu avotu izdevumi uz skolēnu, PPS un % no IKP uz
#     iedzīvotāju) un educ_uoe_fine09 (publiskie), 2012–2023, ISCED 1 un 2.
#   Pasaules Banka SE.XPD.PRIM.PC.ZS, SE.XPD.SECO.PC.ZS (valdības izdevumi uz skolēnu,
#     % no IKP uz iedzīvotāju), NY.GDP.PCAP.PP.KD (IKP uz iedzīvotāju, PPP, 2021. g.
#     starptautiskie $), 2000–2025.
#   PISA valstu vidējie lasīšanā 2000–2025: output/pisa2025/pisa_valstu_videjie.csv
#     (58a skripts; 2000–2012 learningtower, 2015–2025 pilnie faili, 10 PV).
#   Latvijas latviešu valodas plūsma: output/pisa2025/ab_rinda_no_2003.csv.
#
# METODE.
#   Kumulatīvie izdevumi 6–15 gadu vecumā tuvināti kā 6 × ISCED 1 + 4 × ISCED 2 gada
#   izdevumi uz skolēnu (OECD lieto valstu teorētiskos ilgumus; šeit vienāds sadalījums
#   visām valstīm). Finanšu gads = PISA gads − 2 (tuvākais pieejamais), kā OECD PISA
#   2022 attēlā I.4.15 (finanšu gads 2020). Līkne: lasīšana ~ a + b·ln(izdevumi), OLS,
#   kā OECD. Atlikums = faktiskais − prognozētais. SE: sāknēšana pa valstīm (2000
#   atkārtojumi) plus valsts vidējā izlases SE.
#   Reālā rinda 2000–2023: OECD salīdzināmo cenu USD PPP rinda, atpakaļ pagarināta ar
#   Pasaules Bankas daļu × IKP uz iedzīvotāju PPP, piesaistot pēc pārklāšanās gadu
#   vidējās attiecības. Kohortas kumulatīvie izdevumi: summa pa vecumiem 6–15 gadā
#   (PISA gads − 16 + vecums), 6–11 pēc ISCED 1, 12–15 pēc ISCED 2.
#
# IZVADE.
#   output/pisa2025/izdevumi_skersgriezums.csv, izdevumi_atlikumi_pa_cikliem.csv,
#   izdevumi_rinda_realie.csv, izdevumi_kohortu_kumulativie.csv,
#   izdevumi_latvija_hronologija.csv, izdevumi_eurostat_skersgriezums.csv,
#   plots/71a..71e_*.png, logs/71_izdevumi_pret_sniegumu_output.txt
# ============================================================================

# --- Darba direktorija -------------------------------------------------------
if (!dir.exists("R/pisa2025")) {
  p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
  if (nzchar(p)) setwd(file.path(dirname(p), "..", ".."))
}
stopifnot(dir.exists("R/pisa2025"))

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/71_izdevumi_pret_sniegumu_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2", "ggrepel", "jsonlite", "scales")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")
set.seed(20260913)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("data/izdevumi/processed", showWarnings = FALSE, recursive = TRUE)

SAL7 <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
VAL8 <- c("LVA", SAL7)
CIKLI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)
B_SAKN <- 2000
sadala <- function(k) cat(paste(rep("=", 78), collapse = ""), "\n", k, "\n", paste(rep("=", 78), collapse = ""), "\n\n")
pf <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = " ")
tema <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
                                              legend.position = "bottom", strip.text = element_text(face = "bold"))
KR8 <- c(LVA = "#c0392b", EST = "#1f77b4", LTU = "#2ca02c", POL = "#9467bd", FIN = "#17becf", CZE = "#8c564b", HUN = "#e377c2", DEU = "#7f7f7f")
NOS8 <- c(LVA = "Latvija", EST = "Igaunija", LTU = "Lietuva", POL = "Polija", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", DEU = "Vācija")

# === 1. DATU IELĀDE ==========================================================
sadala("1. Datu ielāde")

## 1a. OECD EAG izvilkums
OE <- fread("data/izdevumi/raw/oecd_eag_perstud_usd_ppp.csv")
OE <- OE[EXP_SOURCE == "_T" & EXP_DESTINATION == "INST_EDU" & EXPENDITURE_TYPE == "DIR_EXP" &
         EDUCATION_LEV %in% c("ISCED11_1", "ISCED11_2", "ISCED11_1T4", "ISCED11_2_3")]
OE[, limenis := fcase(EDUCATION_LEV == "ISCED11_1", "ED1", EDUCATION_LEV == "ISCED11_2", "ED2",
                      EDUCATION_LEV == "ISCED11_1T4", "ED1_4", EDUCATION_LEV == "ISCED11_2_3", "ED2_3")]
OE[, mers := fcase(UNIT_MEASURE == "USD_PPP_ST" & PRICE_BASE == "V", "usd_ppp_fakt",
                   UNIT_MEASURE == "USD_PPP_ST" & PRICE_BASE == "Q", "usd_ppp_2020",
                   UNIT_MEASURE == "PT_B1GQ_POP", "pc_ikp_iedz", default = NA_character_)]
OE <- OE[!is.na(mers), .(cnt = REF_AREA, limenis, mers, gads = as.integer(TIME_PERIOD), vertiba = as.numeric(OBS_VALUE))]
OE <- unique(OE, by = c("cnt", "limenis", "mers", "gads"))
cat("OECD: ", nrow(OE), " novērojumi, ", uniqueN(OE$cnt), " valstis, gadi ",
    paste(range(OE$gads), collapse = "–"), "\n", sep = "")
cat("  Valstis:", paste(sort(unique(OE$cnt)), collapse = " "), "\n")
oe_gadi <- OE[mers == "usd_ppp_2020" & limenis == "ED1", .N, by = gads][order(gads)]
cat("  ISCED 1 salīdzināmo cenu novērojumi pa gadiem:", paste(oe_gadi[, paste0(gads, ":", N)], collapse = " "), "\n")
fwrite(OE, "data/izdevumi/processed/oecd_izdevumi_uz_skolenu_tidy.csv")

## 1b. Eurostat
GEO <- c(AT = "AUT", BE = "BEL", BG = "BGR", CY = "CYP", CZ = "CZE", DE = "DEU", DK = "DNK", EE = "EST", EL = "GRC",
         ES = "ESP", FI = "FIN", FR = "FRA", HR = "HRV", HU = "HUN", IE = "IRL", IT = "ITA", LT = "LTU", LU = "LUX",
         LV = "LVA", MT = "MLT", NL = "NLD", PL = "POL", PT = "PRT", RO = "ROU", SE = "SWE", SI = "SVN", SK = "SVK",
         IS = "ISL", LI = "LIE", NO = "NOR", CH = "CHE", UK = "GBR", TR = "TUR", MK = "MKD", RS = "SRB", ME = "MNE",
         AL = "ALB", BA = "BIH", XK = "KSV", MD = "MDA", UA = "UKR", GE = "GEO", JP = "JPN", US = "USA", KR = "KOR")
lasa_estat <- function(f) {
  x <- fread(f)
  x <- x[!is.na(OBS_VALUE)]
  x[, cnt := GEO[geo]]
  x[!is.na(cnt)]
}
ES4 <- lasa_estat("data/izdevumi/raw/educ_uoe_fini04.csv")[sector == "TOT_SEC" & isced11 %in% c("ED1", "ED2", "ED1_2", "ED34")]
ES4 <- ES4[, .(cnt, limenis = isced11, vieniba = unit, gads = as.integer(TIME_PERIOD), vertiba = as.numeric(OBS_VALUE))]
ES9 <- lasa_estat("data/izdevumi/raw/educ_uoe_fine09.csv")[isced11 %in% c("ED1", "ED2", "ED1_2")]
ES9 <- ES9[, .(cnt, limenis = isced11, vieniba = unit, gads = as.integer(TIME_PERIOD), vertiba = as.numeric(OBS_VALUE))]
cat("Eurostat fini04 (visi avoti): ", nrow(ES4), " nov., ", uniqueN(ES4$cnt), " valstis, ", paste(range(ES4$gads), collapse = "–"), "\n", sep = "")
cat("Eurostat fine09 (publiskie):  ", nrow(ES9), " nov., ", uniqueN(ES9$cnt), " valstis\n", sep = "")

## 1c. Pasaules Banka
lasa_wb <- function(f, nos) {
  j <- jsonlite::fromJSON(f)[[2]]
  data.table(cnt = j$countryiso3code, gads = as.integer(j$date), vertiba = as.numeric(j$value), radit = nos)[!is.na(vertiba) & nzchar(cnt)]
}
WB <- rbind(lasa_wb("data/izdevumi/raw/wb_SE.XPD.PRIM.PC.ZS.json", "prim_pc"),
            lasa_wb("data/izdevumi/raw/wb_SE.XPD.SECO.PC.ZS.json", "seco_pc"),
            lasa_wb("data/izdevumi/raw/wb_SE.XPD.TOTL.GD.ZS.json", "izgl_pc_ikp"),
            lasa_wb("data/izdevumi/raw/wb_NY.GDP.PCAP.PP.KD.json", "ikp_iedz_ppp"))
cat("Pasaules Banka: ", nrow(WB), " nov.; pa rādītājiem: ", paste(WB[, .N, by = radit][, paste0(radit, "=", N)], collapse = ", "), "\n", sep = "")

## 1d. PISA
PV <- fread("output/pisa2025/pisa_valstu_videjie.csv")[, .(gads = as.integer(gads), cnt, lasisana, matematika, dabaszinatnes, n)]
AB <- fread("output/pisa2025/ab_rinda_no_2003.csv")[, .(gads = as.integer(gads), grupa, vid, vid_se)]
cat("PISA valstu vidējie: ", nrow(PV), " rindas, cikli ", paste(sort(unique(PV$gads)), collapse = " "), "\n", sep = "")
cat("Latvija lasīšanā (visi):", PV[cnt == "LVA"][order(gads), paste0(gads, ":", pf(lasisana))] |> paste(collapse = " "), "\n")
cat("Latviešu valodas plūsma:", AB[grupa == "A"][order(gads), paste0(gads, ":", pf(vid))] |> paste(collapse = " "), "\n\n")
# Latvijas valsts vidējā izlases SE pa cikliem; 2000. gadam docs/43 tipiskā SE 3,2
SE_LV <- AB[grupa == "visi", .(gads, se = vid_se)]
SE_LV <- rbind(data.table(gads = 2000L, se = 3.2), SE_LV)
SE_A <- AB[grupa == "A", .(gads, vid, se = vid_se)]

# === 2. ŠĶĒRSGRIEZUMS 2022 UN 2025 (OECD ATTĒLA ATKĀRTOJUMS LASĪŠANAI) =======
sadala("2. Šķērsgriezums 2022 un 2025: lasīšana pret kumulatīvajiem izdevumiem 6–15 g.")

# Kumulatīvie izdevumi 6 × ED1 + 4 × ED2; ja viens līmenis trūkst, 10 × ED1_4 (atzīmēts)
kumul <- function(D) {
  W <- dcast(D, cnt + gads ~ limenis, value.var = "vertiba")
  for (k in c("ED1", "ED2", "ED1_4")) if (!k %in% names(W)) W[, (k) := NA_real_]
  W[, kumul := 6 * ED1 + 4 * ED2]
  W[, tuvinajums := "6×ED1+4×ED2"]
  W[is.na(kumul) & !is.na(ED1_4), `:=`(kumul = 10 * ED1_4, tuvinajums = "10×ED1_4")]
  W[!is.na(kumul)]
}
tuvakais_gads <- function(pieejami, merkis) {
  if (!length(pieejami)) return(NA_integer_)
  d <- abs(pieejami - merkis); k <- which(d == min(d)); pieejami[k][which.min(pieejami[k])]
}
# Izvēlas katrai valstij finanšu gadu, kas tuvākais mērķim
ar_fin_gadu <- function(K, merkis) {
  K[, .SD[gads == tuvakais_gads(gads, merkis)], by = cnt][, fin_gads := gads][]
}
liknes_fit <- function(D, x = "kumul", y = "lasisana") {
  f <- lm(reformulate(paste0("log(", x, ")"), y), data = D)
  list(f = f, r2 = summary(f)$r.squared, n = nrow(D), a = coef(f)[1], b = coef(f)[2])
}
# Sāknēšana: atlikuma SE valstij `kods` (līknes nenoteiktība) plus valsts vidējā izlases SE
saknesana_atlikums <- function(D, kods, se_valsts, B = B_SAKN, x = "kumul", y = "lasisana") {
  f <- liknes_fit(D, x, y)$f
  xv <- D[cnt == kods][[x]]; yv <- D[cnt == kods][[y]]
  if (!length(xv)) return(list(atl = NA_real_, se = NA_real_, se_likne = NA_real_))
  atl <- yv - predict(f, newdata = setNames(data.frame(xv), x))
  n <- nrow(D); pr <- numeric(B)
  for (b in seq_len(B)) {
    Db <- D[sample.int(n, n, replace = TRUE)]
    fb <- tryCatch(lm(reformulate(paste0("log(", x, ")"), y), data = Db), error = function(e) NULL)
    pr[b] <- if (is.null(fb)) NA_real_ else predict(fb, newdata = setNames(data.frame(xv), x))
  }
  se_l <- sd(pr, na.rm = TRUE)
  list(atl = atl, se = sqrt(se_l^2 + se_valsts^2), se_likne = se_l)
}

OE_V <- OE[mers == "usd_ppp_fakt"]
OE_Q <- OE[mers == "usd_ppp_2020"]
K_V <- kumul(OE_V); K_Q <- kumul(OE_Q)
OECD_VALSTIS <- c("AUS", "AUT", "BEL", "CAN", "CHE", "CHL", "COL", "CRI", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC",
                  "HUN", "IRL", "ISL", "ISR", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "MEX", "NLD", "NOR", "NZL", "POL", "PRT", "SVK",
                  "SVN", "SWE", "TUR", "USA")

skers <- list(); SK_REZ <- list()
specifikacijas <- list(
  list(nos = "2022, fin. gads 2020, faktiskās cenas", cikls = 2022, fy = 2020, K = K_V, mers = "usd_ppp_fakt", galv = TRUE),
  list(nos = "2025, fin. gads 2023, faktiskās cenas", cikls = 2025, fy = 2023, K = K_V, mers = "usd_ppp_fakt", galv = TRUE),
  list(nos = "2025, fin. gads 2022 (kā OECD attēlā I.1.3), faktiskās cenas", cikls = 2025, fy = 2022, K = K_V, mers = "usd_ppp_fakt", galv = FALSE),
  list(nos = "2022, fin. gads 2020, 2020. g. cenas", cikls = 2022, fy = 2020, K = K_Q, mers = "usd_ppp_2020", galv = FALSE),
  list(nos = "2025, fin. gads 2023, 2020. g. cenas", cikls = 2025, fy = 2023, K = K_Q, mers = "usd_ppp_2020", galv = FALSE))
for (s in specifikacijas) {
  D <- merge(ar_fin_gadu(s$K, s$fy), PV[gads == s$cikls, .(cnt, lasisana, dabaszinatnes, matematika)], by = "cnt")
  D <- D[abs(fin_gads - s$fy) <= 1]
  fit <- liknes_fit(D); fit_o <- liknes_fit(D[cnt %in% OECD_VALSTIS])
  D[, prognoze := predict(fit$f, newdata = D)][, atlikums := lasisana - prognoze]
  cat(sprintf("%s: n = %d valstis, R² = %.3f (OECD valstis n = %d, R² = %.3f); līkne: %.1f + %.1f·ln(izd.)\n",
              s$nos, fit$n, fit$r2, fit_o$n, fit_o$r2, fit$a, fit$b))
  if (s$galv) {
    fd <- liknes_fit(D, y = "dabaszinatnes"); fm <- liknes_fit(D, y = "matematika")
    cat(sprintf("   pārbaudei: dabaszinātnes R² = %.3f, matemātika R² = %.3f (OECD: 2025 dabasz. attēlā I.1.3; 2022 mat. I.4.15 54 %%)\n", fd$r2, fm$r2))
  }
  cat("   8 valstu pozīcija (kumulatīvie USD, lasīšana, prognoze, atlikums):\n")
  for (k in VAL8) {
    r <- D[cnt == k]
    if (nrow(r)) cat(sprintf("     %-3s %-9s fin.g. %d  %8s  %6.1f  %6.1f  %+6.1f  (%s)\n", k, NOS8[k], r$fin_gads, pf(r$kumul, 0), r$lasisana, r$prognoze, r$atlikums, r$tuvinajums))
    else cat(sprintf("     %-3s %-9s nav datu\n", k, NOS8[k]))
  }
  se_lv <- SE_LV[gads == s$cikls, se]
  sk <- saknesana_atlikums(D, "LVA", se_lv)
  cat(sprintf("   Latvijas atlikums %+.1f, SE %.1f (līknes sāknēšana %.1f, izlases %.1f), z = %.2f\n", sk$atl, sk$se, sk$se_likne, se_lv, sk$atl / sk$se))
  # Latviešu valodas plūsma tajā pašā līknē
  a <- SE_A[gads == s$cikls]
  atl_a <- a$vid - predict(fit$f, newdata = D[cnt == "LVA"])
  cat(sprintf("   Latviešu valodas plūsma: %.1f, atlikums %+.1f, SE %.1f\n", a$vid, atl_a, sqrt(sk$se_likne^2 + a$se^2)))
  D[, `:=`(specifikacija = s$nos, cikls = s$cikls, mers = s$mers, r2 = fit$r2, n_valstis = fit$n, galvena = s$galv)]
  skers[[s$nos]] <- D
  SK_REZ[[s$nos]] <- data.table(specifikacija = s$nos, cikls = s$cikls, n = fit$n, r2 = fit$r2, r2_oecd = fit_o$r2, n_oecd = fit_o$n,
                                a = fit$a, b = fit$b, lva_kumul = D[cnt == "LVA", kumul], lva_atlikums = sk$atl, lva_se = sk$se,
                                a_plusma = a$vid, a_atlikums = atl_a, a_se = sqrt(sk$se_likne^2 + a$se^2))
  cat("\n")
}
SKERS <- rbindlist(skers)
fwrite(SKERS, "output/pisa2025/izdevumi_skersgriezums.csv")
fwrite(rbindlist(SK_REZ), "output/pisa2025/izdevumi_skersgriezums_kopsavilkums.csv")
cat("Saglabāts: output/pisa2025/izdevumi_skersgriezums.csv un izdevumi_skersgriezums_kopsavilkums.csv\n\n")

## 2b. Eurostat pārbaude: Eiropas valstis, PPS, visu avotu izdevumi (fini04)
cat("Eurostat pārbaude (PPS, visu avotu izdevumi, 6×ED1+4×ED2):\n")
ES_K <- kumul(ES4[vieniba == "PPS" & limenis %in% c("ED1", "ED2")])
es_rez <- list()
for (ck in c(2022, 2025)) {
  fy <- ck - 2
  D <- merge(ar_fin_gadu(ES_K, fy), PV[gads == ck, .(cnt, lasisana)], by = "cnt")[abs(fin_gads - fy) <= 1]
  fit <- liknes_fit(D); D[, prognoze := predict(fit$f, newdata = D)][, atlikums := lasisana - prognoze]
  sk <- saknesana_atlikums(D, "LVA", SE_LV[gads == ck, se])
  cat(sprintf("  %d (fin.g. %d): n = %d, R² = %.3f; Latvija %s PPS, atlikums %+.1f (SE %.1f)\n", ck, fy, fit$n, fit$r2, pf(D[cnt == "LVA", kumul], 0), sk$atl, sk$se))
  cat("   ", D[cnt %in% VAL8][order(atlikums), paste0(cnt, " ", sprintf("%+.1f", atlikums))] |> paste(collapse = "  "), "\n")
  D[, `:=`(cikls = ck, r2 = fit$r2, n_valstis = fit$n)]; es_rez[[as.character(ck)]] <- D
}
fwrite(rbindlist(es_rez), "output/pisa2025/izdevumi_eurostat_skersgriezums.csv")
cat("Saglabāts: output/pisa2025/izdevumi_eurostat_skersgriezums.csv\n\n")

## 2c. Grafiks 71a
G <- SKERS[galvena == TRUE]
G[, grupa := fcase(cnt == "LVA", "Latvija", cnt %in% SAL7, "7 salīdzinājuma valstis", default = "citas")]
G[, etik := ifelse(cnt %in% VAL8, cnt, "")]
G[, panelis := sprintf("PISA %d lasīšana · fin. gads %d · n = %d · R² = %.2f", cikls, cikls - 2, n_valstis, r2)]
liknes <- rbindlist(lapply(unique(G$panelis), function(p) {
  d <- G[panelis == p]; f <- lm(lasisana ~ log(kumul), d); xs <- exp(seq(log(min(d$kumul)), log(max(d$kumul)), length.out = 200))
  data.table(panelis = p, kumul = xs, lasisana = predict(f, data.frame(kumul = xs)))
}))
p <- ggplot(G, aes(kumul, lasisana)) +
  geom_line(data = liknes, colour = "grey40", linewidth = 0.8) +
  geom_point(aes(colour = grupa, size = grupa), alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = etik, colour = grupa), size = 3.3, max.overlaps = 30, show.legend = FALSE) +
  scale_colour_manual(values = c(Latvija = "#c0392b", `7 salīdzinājuma valstis` = "#1f77b4", citas = "grey65"), name = NULL) +
  scale_size_manual(values = c(Latvija = 3.5, `7 salīdzinājuma valstis` = 2.6, citas = 1.8), guide = "none") +
  scale_x_continuous(labels = label_number(big.mark = " ")) +
  facet_wrap(~panelis, ncol = 2) +
  labs(title = "Lasīšanas sniegums pret kumulatīvajiem izdevumiem uz vienu skolēnu 6–15 gadu vecumā",
       subtitle = "Tuvinājums 6 × ISCED 1 + 4 × ISCED 2 gada izdevumi uz FTE skolēnu, USD pēc pirktspējas paritātes, faktiskās cenas; līkne a + b·ln(x)",
       x = "Kumulatīvie izdevumi uz vienu skolēnu, USD PPP", y = "Vidējais rezultāts lasīšanā",
       caption = "Avoti: OECD Education at a Glance (UOE finanšu dati, EAG 2026 datu kopa), PISA datubāze (58a skripts). OECD PISA 2025 attēls I.1.3 ir par dabaszinātnēm; šis ir tā lasīšanas versija.") + tema
ggsave("output/pisa2025/plots/71a_skersgriezums_2022_2025.png", p, width = 13, height = 6.5, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/71a_skersgriezums_2022_2025.png\n\n")

# === 3. REĀLĀ RINDA 2000–2023 UN KOHORTU KUMULATĪVIE IZDEVUMI ===============
sadala("3. Reālo izdevumu rinda 2000–2023 un kohortu kumulatīvie izdevumi")

# Pasaules Bankas atvasinātā rinda: daļa no IKP uz iedzīvotāju × IKP uz iedzīvotāju PPP (2021. g. $)
WBw <- dcast(WB, cnt + gads ~ radit, value.var = "vertiba")
WBw[, `:=`(wb_ED1 = prim_pc / 100 * ikp_iedz_ppp, wb_ED2 = seco_pc / 100 * ikp_iedz_ppp)]
WBl <- melt(WBw[, .(cnt, gads, ED1 = wb_ED1, ED2 = wb_ED2)], id.vars = c("cnt", "gads"), variable.name = "limenis", value.name = "wb")[!is.na(wb)]
OQ <- OE_Q[limenis %in% c("ED1", "ED2"), .(cnt, gads, limenis, oecd = vertiba)]
# Piesaiste: attiecība OECD / WB pārklāšanās gados (vismaz 2), pēc tam WB reizina ar attiecību pirms OECD sākuma
R <- merge(OQ, WBl, by = c("cnt", "gads", "limenis"))[, .(attieciba = mean(oecd / wb), n_parkl = .N, cv = sd(oecd / wb) / mean(oecd / wb)), by = .(cnt, limenis)]
cat("Piesaistes attiecība OECD (2020. g. USD PPP) / PB (2021. g. $) pārklāšanās gados, 8 valstis:\n")
print(R[cnt %in% VAL8][order(cnt, limenis)][, .(cnt, limenis, attieciba = round(attieciba, 2), n_parkl, variacija = round(cv, 2))])
SER <- merge(WBl, R, by = c("cnt", "limenis"), all.x = TRUE)
SER <- merge(SER, OQ, by = c("cnt", "gads", "limenis"), all = TRUE)
SER <- SER[, .(cnt, gads, limenis, oecd, wb, attieciba, n_parkl)]
SER <- SER[!is.na(oecd) | !is.na(wb)]
# PB piesaistītās vērtības lieto tikai PIRMS pirmā OECD gada (atpakaļ pagarināšana); iekšējos robus
# starp OECD gadiem (piem., 2006–2007) interpolē, jo PB daļās ir izlēcieni (LVA 2006: 33,6 %)
SER[, oecd_min := suppressWarnings(min(gads[!is.na(oecd)])), by = .(cnt, limenis)]
SER[, avots := fifelse(!is.na(oecd), "OECD", fifelse(!is.na(wb) & !is.na(attieciba) & n_parkl >= 2 & gads < oecd_min, "PB piesaistīts", NA_character_))]
SER[, vertiba := fifelse(avots == "OECD", oecd, wb * attieciba)]
SER <- SER[!is.na(vertiba) & gads >= 2000 & gads <= 2023]
# Pilns gadu režģis, iekšējo robu lineāra interpolācija, malās tuvākā vērtība
REZ <- CJ(cnt = unique(SER$cnt), gads = 2000:2024, limenis = c("ED1", "ED2"))
REZ <- merge(REZ, SER[, .(cnt, gads, limenis, vertiba, avots)], by = c("cnt", "gads", "limenis"), all.x = TRUE)
REZ[, `:=`(vert_aizp = {
  v <- vertiba; ok <- !is.na(v)
  if (sum(ok) >= 2) approx(gads[ok], v[ok], xout = gads, rule = 2)$y else if (sum(ok) == 1) rep(v[ok], .N) else rep(NA_real_, .N)
}, n_nov = sum(!is.na(vertiba))), by = .(cnt, limenis)]
REZ[is.na(avots) & !is.na(vert_aizp), avots := "interpolēts/pagarināts"]
REZ <- REZ[n_nov >= 3]
fwrite(REZ[, .(cnt, gads, limenis, vertiba_noverota = vertiba, vertiba = vert_aizp, avots)], "output/pisa2025/izdevumi_rinda_realie.csv")
fwrite(REZ[, .(cnt, gads, limenis, vertiba_noverota = vertiba, vertiba = vert_aizp, avots)], "data/izdevumi/processed/realie_izdevumi_uz_skolenu_2000_2024.csv")
fwrite(SER, "data/izdevumi/processed/piesaiste_oecd_pb.csv")
fwrite(ES4, "data/izdevumi/processed/eurostat_fini04_tidy.csv"); fwrite(ES9, "data/izdevumi/processed/eurostat_fine09_tidy.csv"); fwrite(WBw, "data/izdevumi/processed/pasaules_banka_tidy.csv")
cat("\nSaglabāts: output/pisa2025/izdevumi_rinda_realie.csv (", uniqueN(REZ$cnt), " valstis)\n", sep = "")

cat("\nReālie izdevumi uz skolēnu (ISCED 1 un 2 vidējais, 2020. g. USD PPP), 8 valstis:\n")
RV <- REZ[, .(vid = mean(vert_aizp), avoti = paste(sort(unique(avots)), collapse = "+")), by = .(cnt, gads)]
T8 <- dcast(RV[cnt %in% VAL8 & gads <= 2023], gads ~ cnt, value.var = "vid")
setcolorder(T8, c("gads", VAL8)); print(T8[, lapply(.SD, function(x) if (is.numeric(x)) round(x) else x)])
cat("\nLatvijas rindas avoti pa gadiem:\n"); print(REZ[cnt == "LVA" & limenis == "ED1" & gads <= 2023, .(gads, avots, vertiba = round(vert_aizp))])

## 3b. Kohortu kumulatīvie izdevumi 6–15 gadu vecumā
kohorta <- function(REZw, cikls) {
  dz <- cikls - 16
  g <- data.table(vecums = 6:15)[, `:=`(gads = dz + vecums, limenis = fifelse(vecums <= 11, "ED1", "ED2"))]
  x <- merge(g, REZw, by = c("gads", "limenis"), all.x = TRUE)
  x[, .(cikls = cikls, kohorta_dz = dz, kumul_real = sum(vert_aizp), n_gadi = sum(!is.na(vert_aizp)),
        n_noverots = sum(avots %in% c("OECD", "PB piesaistīts")), gadi = paste(range(gads), collapse = "–"),
        vid_6_11 = mean(vert_aizp[vecums <= 11]), vid_12_15 = mean(vert_aizp[vecums >= 12]))]
}
KOH <- REZ[, rbindlist(lapply(CIKLI, function(ck) kohorta(.SD, ck))), by = cnt]
KOH <- KOH[n_gadi == 10]
KOH[, dala_noverota := n_noverots / 10]
KOH <- merge(KOH, PV[, .(cnt, cikls = gads, lasisana)], by = c("cnt", "cikls"), all.x = TRUE)
fwrite(KOH, "output/pisa2025/izdevumi_kohortu_kumulativie.csv")
cat("\nKohortu kumulatīvie reālie izdevumi 6–15 g. (2020. g. USD PPP), 8 valstis; iekavās novēroto gadu daļa:\n")
KT <- KOH[cnt %in% VAL8, .(cnt, cikls, s = sprintf("%s (%.0f%%)", pf(kumul_real / 1000, 0), 100 * dala_noverota))]
print(dcast(KT, cikls ~ cnt, value.var = "s")[, c("cikls", VAL8), with = FALSE])
cat("Saglabāts: output/pisa2025/izdevumi_kohortu_kumulativie.csv\n\n")

## 3c. Grafiki 71b (reālā rinda) un 71c (trajektorija)
RV8 <- RV[cnt %in% VAL8 & gads <= 2023]
RV8[, valsts := factor(NOS8[cnt], levels = NOS8)]
p <- ggplot(RV8, aes(gads, vid, colour = cnt)) +
  geom_line(aes(linewidth = cnt == "LVA")) + geom_point(size = 1.4) +
  scale_colour_manual(values = KR8, labels = NOS8, name = NULL) + scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.7), guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) + scale_y_continuous(labels = label_number(big.mark = " ")) +
  geom_vline(xintercept = c(2008.5, 2016.5, 2020.5), linetype = "dotted", colour = "grey50") +
  annotate("text", x = c(2008.6, 2016.6, 2020.6), y = rep(Inf, 3), label = c("2009 krīze", "2016 algu modelis", "Skola2030, Covid"), hjust = 0, vjust = 1.5, size = 3, colour = "grey30") +
  labs(title = "Reālie izdevumi uz vienu skolēnu, ISCED 1 un 2 vidējais, 2000–2023",
       subtitle = "2020. gada cenas, USD pēc pirktspējas paritātes. OECD EAG no 2005./2008. g.; pirms tam Pasaules Bankas daļa × IKP uz iedzīvotāju,\npiesaistīta OECD līmenim. Somijas un Vācijas lēciens 2012. g. ir ISCED 2011 pārklasifikācija OECD datos",
       x = NULL, y = "USD PPP uz skolēnu gadā, 2020. g. cenas", caption = "Avoti: OECD EAG UOE finanšu dati; Pasaules Banka SE.XPD.PRIM.PC.ZS, SE.XPD.SECO.PC.ZS, NY.GDP.PCAP.PP.KD.") + tema
ggsave("output/pisa2025/plots/71b_realie_izdevumi_2000_2023.png", p, width = 12, height = 6.5, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/71b_realie_izdevumi_2000_2023.png\n")

TR <- KOH[cnt %in% VAL8 & !is.na(lasisana)]
TR[, valsts := factor(NOS8[cnt], levels = NOS8)]
TR_A <- merge(KOH[cnt == "LVA", .(cikls, kumul_real, dala_noverota)], SE_A[, .(cikls = gads, lasisana = vid)], by = "cikls")[, `:=`(cnt = "LVA A", valsts = "Latvija, latviešu valodas plūsma")]
p <- ggplot(TR[cnt %in% c("LVA", "LTU", "EST")], aes(kumul_real / 1000, lasisana, colour = cnt)) +
  geom_path(arrow = arrow(length = unit(0.18, "cm"), type = "closed"), linewidth = 0.8) +
  geom_path(data = TR_A, colour = "#c0392b", linetype = "dashed", linewidth = 0.7) +
  geom_point(aes(shape = dala_noverota < 0.8), size = 2.8) +
  geom_point(data = TR_A, colour = "#c0392b", shape = 1, size = 2.8) +
  ggrepel::geom_text_repel(aes(label = cikls), size = 3, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = TR_A, aes(label = paste0("A ", cikls)), colour = "#c0392b", size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = KR8, labels = NOS8, name = NULL) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c("≥ 80 % gadu novēroti", "< 80 % gadu novēroti"), name = NULL) +
  labs(title = "Trajektorija: kohortas kumulatīvie reālie izdevumi 6–15 gadu vecumā un lasīšana, Baltija 2006–2025",
       subtitle = "x: summa pa kohortas mācību gadiem (PISA gads − 16 + vecums), tūkst. USD PPP 2020. g. cenās; Latvijai arī latviešu valodas plūsma (pārtraukta līnija)",
       x = "Kohortas kumulatīvie reālie izdevumi, tūkst. USD PPP (2020. g. cenas)", y = "Vidējais rezultāts lasīšanā") + tema
ggsave("output/pisa2025/plots/71c_trajektorija_baltija.png", p, width = 11, height = 7, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/71c_trajektorija_baltija.png\n\n")

# === 4. ATLIKUMS PRET LĪKNI KATRĀ CIKLĀ ======================================
sadala("4. Atlikums pret līkni katrā ciklā, 2000–2025")

# 4a. Laikietilpīgais mērs: 6×ED1+4×ED2 finanšu gadā, tuvākajā pieejamajam PISA gads − 2, 2020. g. cenas (OECD)
cat("4a. Vienlaicīgais mērs: OECD salīdzināmo cenu kumulatīvie izdevumi finanšu gadā ≈ PISA gads − 2\n")
ATL <- list(); ATL_V <- list()
for (ck in CIKLI) {
  fy <- ck - 2
  D <- merge(ar_fin_gadu(K_Q, fy), PV[gads == ck, .(cnt, lasisana)], by = "cnt")[abs(fin_gads - fy) <= 3]
  if (nrow(D) < 12 || !"LVA" %in% D$cnt) { cat(sprintf("  %d: par maz valstu (%d) vai nav Latvijas\n", ck, nrow(D))); next }
  fit <- liknes_fit(D); D[, prognoze := predict(fit$f, newdata = D)][, atlikums := lasisana - prognoze]
  sk <- saknesana_atlikums(D, "LVA", SE_LV[gads == ck, se])
  a <- SE_A[gads == ck]
  atl_a <- if (nrow(a)) a$vid - D[cnt == "LVA", prognoze] else NA_real_
  se_a <- if (nrow(a)) sqrt(sk$se_likne^2 + a$se^2) else NA_real_
  sal <- D[cnt %in% SAL7, .(vid_atl_7 = mean(atlikums), n7 = .N)]
  cat(sprintf("  %d: n = %2d, fin.g. LVA %d, R² = %.2f, LVA izd. %s, lasīšana %.1f, prognoze %.1f, atlikums %+.1f (SE %.1f); A plūsma %+.1f (SE %.1f); 7 valstu vid. atlikums %+.1f (n = %d)\n",
              ck, fit$n, D[cnt == "LVA", fin_gads], fit$r2, pf(D[cnt == "LVA", kumul], 0), D[cnt == "LVA", lasisana], D[cnt == "LVA", prognoze], sk$atl, sk$se, atl_a, se_a, sal$vid_atl_7, sal$n7))
  ATL[[as.character(ck)]] <- data.table(mers = "vienlaicīgs (fin. gads ≈ PISA − 2)", cikls = ck, n = fit$n, r2 = fit$r2, lva_fin_gads = D[cnt == "LVA", fin_gads],
                                        lva_kumul = D[cnt == "LVA", kumul], lva_lasisana = D[cnt == "LVA", lasisana], lva_prognoze = D[cnt == "LVA", prognoze],
                                        lva_atlikums = sk$atl, lva_se = sk$se, a_lasisana = if (nrow(a)) a$vid else NA_real_, a_atlikums = atl_a, a_se = se_a,
                                        sal7_atlikums = sal$vid_atl_7, sal7_n = sal$n7, lva_rel_7 = sk$atl - sal$vid_atl_7)
  ATL_V[[as.character(ck)]] <- D[, .(mers = "vienlaicīgs", cikls = ck, cnt, fin_gads, kumul, lasisana, prognoze, atlikums, r2 = fit$r2, n = fit$n)]
}

# 4b. Kohortas mērs: kumulatīvie reālie izdevumi 6–15 g. (vismaz 70 % gadu novēroti)
cat("\n4b. Kohortas mērs: kumulatīvie reālie izdevumi 6–15 gadu vecumā (valstis ar ≥ 70 % novērotu gadu)\n")
for (ck in CIKLI) {
  D <- KOH[cikls == ck & dala_noverota >= 0.7 & !is.na(lasisana), .(cnt, kumul = kumul_real, lasisana, dala_noverota)]
  if (nrow(D) < 12 || !"LVA" %in% D$cnt) { cat(sprintf("  %d: par maz valstu (%d) vai nav Latvijas\n", ck, nrow(D))); next }
  fit <- liknes_fit(D); D[, prognoze := predict(fit$f, newdata = D)][, atlikums := lasisana - prognoze]
  sk <- saknesana_atlikums(D, "LVA", SE_LV[gads == ck, se])
  a <- SE_A[gads == ck]
  atl_a <- if (nrow(a)) a$vid - D[cnt == "LVA", prognoze] else NA_real_
  se_a <- if (nrow(a)) sqrt(sk$se_likne^2 + a$se^2) else NA_real_
  sal <- D[cnt %in% SAL7, .(vid_atl_7 = mean(atlikums), n7 = .N)]
  cat(sprintf("  %d: n = %2d, R² = %.2f, LVA kohorta %s, lasīšana %.1f, prognoze %.1f, atlikums %+.1f (SE %.1f); A plūsma %+.1f; 7 valstu vid. %+.1f (n = %d)\n",
              ck, fit$n, fit$r2, pf(D[cnt == "LVA", kumul], 0), D[cnt == "LVA", lasisana], D[cnt == "LVA", prognoze], sk$atl, sk$se, atl_a, sal$vid_atl_7, sal$n7))
  ATL[[paste0("k", ck)]] <- data.table(mers = "kohorta (6–15 g. kumulatīvi)", cikls = ck, n = fit$n, r2 = fit$r2, lva_fin_gads = NA_integer_,
                                       lva_kumul = D[cnt == "LVA", kumul], lva_lasisana = D[cnt == "LVA", lasisana], lva_prognoze = D[cnt == "LVA", prognoze],
                                       lva_atlikums = sk$atl, lva_se = sk$se, a_lasisana = if (nrow(a)) a$vid else NA_real_, a_atlikums = atl_a, a_se = se_a,
                                       sal7_atlikums = sal$vid_atl_7, sal7_n = sal$n7, lva_rel_7 = sk$atl - sal$vid_atl_7)
  ATL_V[[paste0("k", ck)]] <- D[, .(mers = "kohorta", cikls = ck, cnt, fin_gads = NA_integer_, kumul, lasisana, prognoze, atlikums, r2 = fit$r2, n = fit$n)]
}
ATL <- rbindlist(ATL); ATL_V <- rbindlist(ATL_V)
fwrite(ATL, "output/pisa2025/izdevumi_atlikumi_pa_cikliem.csv")
fwrite(ATL_V, "output/pisa2025/izdevumi_atlikumi_visas_valstis.csv")
cat("\nSaglabāts: output/pisa2025/izdevumi_atlikumi_pa_cikliem.csv un izdevumi_atlikumi_visas_valstis.csv\n")

# 4c. Vai atlikums laikā mainās: svērta lineāra tendence un atsevišķi posmi
cat("\n4c. Atlikuma tendence laikā (svērta OLS ar 1/SE²):\n")
for (m in unique(ATL$mers)) {
  A <- ATL[mers == m]
  if (nrow(A) >= 3) {
    f <- lm(lva_atlikums ~ I(cikls - 2015), data = A, weights = 1 / lva_se^2); s <- summary(f)$coefficients
    cat(sprintf("  %s: slīpums %+.2f punkti gadā (SE %.2f, p = %.3f), līmenis 2015. g. %+.1f; cikli %s\n", m, s[2, 1], s[2, 2], s[2, 4], s[1, 1], paste(A$cikls, collapse = " ")))
    A2 <- A[cikls >= 2015]
    if (nrow(A2) >= 3) { f2 <- lm(lva_atlikums ~ I(cikls - 2015), data = A2, weights = 1 / lva_se^2); s2 <- summary(f2)$coefficients
      cat(sprintf("     tikai 2015–2025: slīpums %+.2f gadā (SE %.2f)\n", s2[2, 1], s2[2, 2])) }
    A[, `:=`(d = c(NA, diff(lva_atlikums)), d_se = c(NA, sqrt(lva_se[-1]^2 + lva_se[-.N]^2)), iepr = shift(cikls))]
    cat("     pārmaiņas starp cikliem: ", A[!is.na(d), sprintf("%d→%d %+.1f (SE %.1f)", iepr, cikls, d, d_se)] |> paste(collapse = "; "), "\n")
    cat("     relatīvi pret 7 valstu vidējo atlikumu: ", A[, sprintf("%d %+.1f", cikls, lva_rel_7)] |> paste(collapse = "; "), "\n")
  }
}

# 4d. Grafiks 71d: atlikums pa cikliem
AV <- ATL_V[mers == "vienlaicīgs" & cnt %in% SAL7]
AL <- ATL[mers == "vienlaicīgs (fin. gads ≈ PISA − 2)"]
AL_long <- rbind(AL[, .(cikls, kas = "Latvija, visi", atlikums = lva_atlikums, se = lva_se)],
                 AL[!is.na(a_atlikums), .(cikls, kas = "Latvija, latviešu valodas plūsma", atlikums = a_atlikums, se = a_se)])
p <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(data = AV, aes(cikls, atlikums, group = cnt, colour = cnt), alpha = 0.6, linewidth = 0.6) +
  geom_point(data = AV, aes(cikls, atlikums, colour = cnt), alpha = 0.7, size = 1.6) +
  geom_ribbon(data = AL_long, aes(cikls, ymin = atlikums - 1.96 * se, ymax = atlikums + 1.96 * se, fill = kas), alpha = 0.15) +
  geom_line(data = AL_long, aes(cikls, atlikums, colour = kas, linetype = kas), linewidth = 1.3) +
  geom_point(data = AL_long, aes(cikls, atlikums, colour = kas), size = 3) +
  scale_colour_manual(values = c(KR8[SAL7], `Latvija, visi` = "#c0392b", `Latvija, latviešu valodas plūsma` = "#e67e22"), labels = c(NOS8[SAL7], `Latvija, visi` = "Latvija, visi", `Latvija, latviešu valodas plūsma` = "Latvija, latviešu valodas plūsma"), name = NULL) +
  scale_fill_manual(values = c(`Latvija, visi` = "#c0392b", `Latvija, latviešu valodas plūsma` = "#e67e22"), guide = "none") +
  scale_linetype_manual(values = c(`Latvija, visi` = "solid", `Latvija, latviešu valodas plūsma` = "dashed"), guide = "none") +
  scale_x_continuous(breaks = CIKLI) +
  labs(title = "Atlikums pret izdevumu līkni: faktiskā lasīšana mīnus līknes prognoze, pa PISA cikliem",
       subtitle = "Līkne katrā ciklā no jauna: lasīšana ~ a + b·ln(6×ISCED1 + 4×ISCED2, 2020. g. USD PPP, fin. gads ≈ PISA − 2)\nLatvijai 95 % josla (līknes sāknēšana pa valstīm + izlases SE); 7 valstis bez joslas",
       x = NULL, y = "Atlikums, PISA punkti") + tema
ggsave("output/pisa2025/plots/71d_atlikumi_pa_cikliem.png", p, width = 12, height = 6.5, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/71d_atlikumi_pa_cikliem.png\n\n")

# === 5. LATVIJAS HRONOLOĢIJA: IZDEVUMI UN LASĪŠANA ============================
sadala("5. Latvijas izdevumu hronoloģija pret docs/45 notikumiem")

LV <- REZ[cnt == "LVA" & gads <= 2023, .(real_ED1 = vert_aizp[limenis == "ED1"], real_ED2 = vert_aizp[limenis == "ED2"], avots = avots[limenis == "ED1"]), by = gads]
LV <- merge(LV, OE_V[cnt == "LVA" & limenis == "ED1", .(gads, fakt_usd_ED1 = vertiba)], by = "gads", all.x = TRUE)
LV <- merge(LV, OE[cnt == "LVA" & limenis == "ED1" & mers == "pc_ikp_iedz", .(gads, pc_ikp_iedz_ED1 = vertiba)], by = "gads", all.x = TRUE)
LV <- merge(LV, ES4[cnt == "LVA" & limenis == "ED1" & vieniba == "PPS", .(gads, estat_pps_ED1 = vertiba)], by = "gads", all.x = TRUE)
LV <- merge(LV, WBw[cnt == "LVA", .(gads, pb_prim_pc = prim_pc, pb_seco_pc = seco_pc, pb_izgl_pc_ikp = izgl_pc_ikp, ikp_iedz_ppp = ikp_iedz_ppp)], by = "gads", all.x = TRUE)
LV[, real_vid := (real_ED1 + real_ED2) / 2][, real_pret_2008 := 100 * real_vid / real_vid[gads == 2008]][, pieaug_pc := 100 * (real_vid / shift(real_vid) - 1)]
LV[, notikums := fcase(gads == 2006, "jauns pamatizglītības standarts", gads == 2009, "krīzes budžeta samazinājums",
                       gads == 2014, "VISC: lasītprasme prioritāte; jauns standarts", gads == 2016, "jaunais skolotāju algu modelis (2016-09)",
                       gads == 2018, "PISA 2018 mērījums; MK not. 747", gads == 2020, "Skola2030 sākums; attālinātās mācības",
                       gads == 2022, "PISA 2022 mērījums", gads == 2023, "pāreja uz mācībām valsts valodā sākas", default = "")]
fwrite(LV, "output/pisa2025/izdevumi_latvija_hronologija.csv")
cat("Latvija pa gadiem (reālie 2020. g. USD PPP uz skolēnu; indekss 2008 = 100; % no IKP uz iedz.; PB daļas):\n")
print(LV[, .(gads, real_ED1 = round(real_ED1), real_ED2 = round(real_ED2), idx_2008 = round(real_pret_2008), pieaug_pc = round(pieaug_pc, 1),
             pc_ikp_iedz = pc_ikp_iedz_ED1, estat_pps = round(estat_pps_ED1), pb_prim = pb_prim_pc, ikp_iedz_ppp = round(ikp_iedz_ppp), avots, notikums)])
cat("Saglabāts: output/pisa2025/izdevumi_latvija_hronologija.csv\n\n")

posmi <- data.table(no = c(2000, 2005, 2008, 2010, 2015, 2019, 2015), lidz = c(2005, 2008, 2010, 2015, 2019, 2023, 2022))
posmi[, `:=`(lv = sapply(seq_len(.N), function(i) 100 * (LV[gads == lidz[i], real_vid] / LV[gads == no[i], real_vid] - 1)),
             est = sapply(seq_len(.N), function(i) 100 * (RV[cnt == "EST" & gads == lidz[i], vid] / RV[cnt == "EST" & gads == no[i], vid] - 1)),
             ltu = sapply(seq_len(.N), function(i) 100 * (RV[cnt == "LTU" & gads == lidz[i], vid] / RV[cnt == "LTU" & gads == no[i], vid] - 1)),
             sal5 = sapply(seq_len(.N), function(i) { d <- RV[cnt %in% setdiff(SAL7, c("EST", "LTU")) & gads %in% c(no[i], lidz[i])]
               mean(d[, 100 * (vid[gads == lidz[i]] / vid[gads == no[i]] - 1), by = cnt]$V1, na.rm = TRUE) }))]
cat("Reālo izdevumu uz skolēnu pārmaiņa pa posmiem, %:\n"); print(posmi[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

cat("\nKohortu ekspozīcija (Latvija): vidējie reālie izdevumi kohortas mācību gados, pret iepriekšējo kohortu, un lasīšana:\n")
KL <- merge(KOH[cnt == "LVA", .(cikls, kohorta_dz, gadi, kumul_real, vid_6_11, vid_12_15, dala_noverota, lasisana)], SE_A[, .(cikls = gads, a_plusma = vid)], by = "cikls", all.x = TRUE)
KL[, `:=`(d_kumul_pc = 100 * (kumul_real / shift(kumul_real) - 1), d_lasisana = lasisana - shift(lasisana), d_a = a_plusma - shift(a_plusma))]
print(KL[, .(cikls, kohorta_dz, gadi, kumul_tukst = round(kumul_real / 1000), vid_6_11 = round(vid_6_11), vid_12_15 = round(vid_12_15), noverots = dala_noverota,
             d_kumul_pc = round(d_kumul_pc, 1), lasisana = round(lasisana, 1), d_lasisana = round(d_lasisana, 1), a_plusma = round(a_plusma, 1), d_a = round(d_a, 1))])

# Korelācija starp izdevumu pārmaiņu un lasīšanas pārmaiņu starp cikliem, 8 valstis (kohortas mērs)
cat("\nStarpciklu pārmaiņas 8 valstīs: kohortas kumulatīvo izdevumu % pārmaiņa pret lasīšanas pārmaiņu:\n")
K8 <- KOH[cnt %in% VAL8 & !is.na(lasisana)][order(cnt, cikls)]
K8[, `:=`(d_kumul_pc = 100 * (kumul_real / shift(kumul_real) - 1), d_las = lasisana - shift(lasisana), pareja = paste0(shift(cikls), "→", cikls)), by = cnt]
K8 <- K8[!is.na(d_las) & dala_noverota >= 0.7]
print(dcast(K8, pareja ~ cnt, value.var = "d_kumul_pc")[, lapply(.SD, function(x) if (is.numeric(x)) round(x) else x)])
ct <- cor.test(K8$d_kumul_pc, K8$d_las)
cat(sprintf("  Korelācija (n = %d pārejas): r = %.2f, p = %.3f\n", nrow(K8), ct$estimate, ct$p.value))
K8all <- KOH[!is.na(lasisana)][order(cnt, cikls)]
K8all[, `:=`(d_kumul_pc = 100 * (kumul_real / shift(kumul_real) - 1), d_las = lasisana - shift(lasisana), pareja = paste0(shift(cikls), "→", cikls)), by = cnt]
K8all <- K8all[!is.na(d_las) & dala_noverota >= 0.7 & cikls >= 2009]
f <- lm(d_las ~ d_kumul_pc + factor(pareja), data = K8all); s <- summary(f)$coefficients
cat(sprintf("  Visas valstis ar datiem (n = %d pārejas, %d valstis), ar pārejas FE: β = %.3f punkti uz 1 %% izdevumu pieauguma (SE %.3f, p = %.3f)\n",
            nrow(K8all), uniqueN(K8all$cnt), s[2, 1], s[2, 2], s[2, 4]))

# Grafiks 71e: Latvija, izdevumi un lasīšana vienā laika asī
LVg <- rbind(LV[, .(gads, kas = "Reālie izdevumi uz skolēnu, 2008 = 100", v = real_pret_2008)],
             PV[cnt == "LVA", .(gads, kas = "Lasīšana, visi (PISA punkti)", v = lasisana)],
             SE_A[, .(gads, kas = "Lasīšana, latviešu valodas plūsma", v = vid)])
p <- ggplot() +
  geom_vline(xintercept = c(2009, 2016, 2020, 2023), linetype = "dotted", colour = "grey55") +
  geom_text(data = data.table(gads = c(2009, 2016, 2020, 2023) + 0.1, v = Inf, kas = "Reālie izdevumi uz skolēnu, 2008 = 100",
                              lab = c("krīzes samazinājums", "algu modelis", "Skola2030, Covid", "valsts valoda")),
            aes(gads, v, label = lab), hjust = 0, vjust = 1.4, size = 2.9, colour = "grey30") +
  geom_line(data = LVg[kas == "Reālie izdevumi uz skolēnu, 2008 = 100"], aes(gads, v, colour = kas), linewidth = 1.1) +
  geom_point(data = LVg[kas == "Reālie izdevumi uz skolēnu, 2008 = 100"], aes(gads, v, colour = kas), size = 1.5) +
  facet_wrap(~kas, ncol = 1, scales = "free_y") +
  geom_line(data = LVg[kas != "Reālie izdevumi uz skolēnu, 2008 = 100"], aes(gads, v, colour = kas), linewidth = 1) +
  geom_point(data = LVg[kas != "Reālie izdevumi uz skolēnu, 2008 = 100"], aes(gads, v, colour = kas), size = 2.4) +
  scale_colour_manual(values = c(`Reālie izdevumi uz skolēnu, 2008 = 100` = "#2c3e50", `Lasīšana, visi (PISA punkti)` = "#c0392b", `Lasīšana, latviešu valodas plūsma` = "#e67e22"), guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, 2), limits = c(2000, 2025)) +
  labs(title = "Latvija: reālie izdevumi uz skolēnu un lasīšana PISA, 2000–2025", subtitle = "Izdevumi: ISCED 1 un 2 vidējais, 2020. g. USD PPP, indekss 2008 = 100. Notikumi no docs/45", x = NULL, y = NULL) + tema
ggsave("output/pisa2025/plots/71e_latvija_hronologija.png", p, width = 11, height = 8, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/71e_latvija_hronologija.png\n\n")

# === 6. KOPSAVILKUMS =========================================================
sadala("6. Kopsavilkums")
cat("Šķērsgriezums (visas specifikācijas):\n"); print(rbindlist(SK_REZ)[, .(specifikacija, n, r2 = round(r2, 3), lva_kumul = round(lva_kumul), lva_atlikums = round(lva_atlikums, 1), lva_se = round(lva_se, 1), a_atlikums = round(a_atlikums, 1))])
cat("\nAtlikumi pa cikliem:\n"); print(ATL[, .(mers, cikls, n, r2 = round(r2, 2), lva_kumul = round(lva_kumul), lva_atlikums = round(lva_atlikums, 1), lva_se = round(lva_se, 1), a_atlikums = round(a_atlikums, 1), sal7 = round(sal7_atlikums, 1), rel_7 = round(lva_rel_7, 1))])
cat("\nIzvades faili:\n  output/pisa2025/izdevumi_skersgriezums.csv, izdevumi_skersgriezums_kopsavilkums.csv, izdevumi_eurostat_skersgriezums.csv\n",
    "  output/pisa2025/izdevumi_rinda_realie.csv, izdevumi_kohortu_kumulativie.csv, izdevumi_atlikumi_pa_cikliem.csv, izdevumi_atlikumi_visas_valstis.csv, izdevumi_latvija_hronologija.csv\n",
    "  output/pisa2025/plots/71a..71e\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
