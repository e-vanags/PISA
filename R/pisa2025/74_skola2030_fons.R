# ============================================================================
# 74. Slānis 3. Skola2030 kā sistēmas fona faktors: deva pa mērījumiem un
#     mācīšanas simptomi
# ============================================================================
#
# Ko var ar esošajiem datiem (docs/46 Slānis 3): cēloņu dizaina nav, jo visas
# skolas pārgāja vienlaikus. Var divas lietas:
#
#  A. DEVAS TABULA. Katram Latvijas mērījumam (PISA, PIRLS, TIMSS, ICCS, ICILS)
#     izrēķina, cik mācību gadus pirms testa paaudze mācījās jaunajā saturā
#     (MK not. Nr. 747: 2020./21. — 1., 4., 7., 10. kl.; 2021./22. — 2., 5., 8.,
#     11.; 2022./23. — 3., 6., 9., 12.), un liek blakus latviešu valodas plūsmas
#     relatīvās vietas pārmaiņu pret iepriekšējo tā paša pētījuma ciklu
#     (output/pisa2025/vizualizacijas/V1_relativa_vieta.csv, salīdzinājuma
#     valstu kopa katram pētījumam sava). Ja Skola2030 būtu galvenais dzinējs,
#     mērījumiem ar devu 0 nebūtu Latvijai specifiska krituma, ar devu > 0 būtu.
#
#  B. MĀCĪŠANAS SIMPTOMI. Skolu anketas resursu trūkuma vienumi (SC017),
#     skolotāju tālākizglītība (SC025), skolēnu vērtētais skolotāja atbalsts
#     (TEACHSUP), disciplīna (DISCLIM), lasīšanas stimulēšana (STIMREAD):
#     latviešu valodas plūsma pret 7 salīdzinājuma valstīm 2015–2025 no
#     apvienotās simptomu tabulas (64e). Vai 2022 -> 2025 Latvijā parādās
#     "ieviešanas bez atbalsta" pazīmes: materiālu trūkums, personāla trūkums.
#
# Izvade: output/pisa2025/skola2030_deva.csv
#         output/pisa2025/skola2030_simptomi.csv
#         output/pisa2025/plots/74_skola2030_deva.png
#         output/pisa2025/logs/74_skola2030_output.txt
# ============================================================================

suppressMessages({library(data.table); library(ggplot2)})
while (sink.number() > 0) sink()
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
sink("output/pisa2025/logs/74_skola2030_output.txt", split = TRUE)
cat("74. Skola2030 fons, ", format(Sys.time()), "\n\n")

# ---------------------------------------------------------------------------
# A. Deva pa mērījumiem
# ---------------------------------------------------------------------------
jauns <- function(g, Y) (Y >= 2020 & g %in% c(1, 4, 7, 10)) | (Y >= 2021 & g %in% c(2, 5, 8, 11)) |
                        (Y >= 2022 & g %in% c(3, 6, 9, 12))
# mācību gads, kurā notiek tests (gada skaitlis = mācību gada sākums)
testa_gads <- function(pet, gads) {
  gads <- as.integer(gads)
  fifelse(grepl("PISA", pet), gads - 1L,                       # pavasaris
  fifelse(grepl("PIRLS", pet) & gads == 2021, 2021L,           # Latvijā 2021. gada rudens, 5. klase
  fifelse(grepl("PIRLS|TIMSS|ICCS|ICILS", pet), gads - 1L, gads - 1L)))
}
deva <- function(b, Yt) {
  if (Yt < 2020) return(0)
  Y <- 2020:Yt; g <- Y - b - 6
  sum(g >= 1 & jauns(g, Y))
}
R <- fread("output/pisa2025/vizualizacijas/V1_relativa_vieta.csv")
R[, b := floor(dz_gads)]
R[, Yt := testa_gads(petijums, gads)]
R[, klase_testa := Yt - b - 6]
R[, deva := mapply(deva, b, Yt)]
# PIRLS 2021 Latvijā notika 2021. gada oktobrī 5. klases sākumā: pilns gads jaunajā saturā
# ir tikai 4. klase (2020./21.), 5. klases divi mēneši netiek skaitīti
R[grepl("PIRLS", petijums) & gads == 2021, deva := 1]
setorder(R, petijums, gads)
R[, `:=`(rel_prev = shift(rel), se_prev = shift(se), gads_prev = shift(gads)), by = petijums]
R[, d_rel := rel - rel_prev]
R[, d_se := sqrt(se^2 + se_prev^2)]
R[, d_rel_sd := d_rel * rel_sd / rel]
# Saites kļūda blakus ciklu pārim no vienotās tabulas (78. skripts; 2022 -> 2025 = 1,094, OECD 2025 Vol. I atvasinātā,
# sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md). Līdz 2026-09-13 šeit bija tuvinājums sqrt(LE_2022(g)² + LE_2022(g_prev)²) ar 2025 = 3.
LE_TAB <- fread("output/pisa2025/saites_kludas.csv")[joma == "READ"]
le_pair <- function(g1, g2) vapply(seq_along(g1), function(i) { v <- LE_TAB[no == g1[i] & uz == g2[i], LE]; if (length(v)) v[1] else NA_real_ }, numeric(1))
# Relatīvajai vietai (LVA A mīnus valstu vidējais tajā pašā skalā) saites kļūda izkrīt, tāpēc
# d_se to NEIETVER (labots 2026-09-13; līdz tam bija pievienota, kas SE pārvērtēja par ~0,5–1,5 punktiem).
# le_pair paliek pieejama pārbaudēm.
D <- R[, .(petijums, gads, dz_gads = b, klase_testa, deva, rel = round(rel, 1), se = round(se, 1),
           gads_prev, d_rel = round(d_rel, 1), d_se = round(d_se, 1), d_rel_sd = round(d_rel_sd, 3))]
fwrite(D, "output/pisa2025/skola2030_deva.csv")
cat("A. Deva (mācību gadi jaunajā saturā pirms testa) un relatīvās vietas pārmaiņa\n")
print(D[gads >= 2016])

p <- ggplot(D[!is.na(d_rel) & gads >= 2016], aes(deva, d_rel, colour = petijums)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_errorbar(aes(ymin = d_rel - 1.96 * d_se, ymax = d_rel + 1.96 * d_se), width = 0.1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(gsub(" .*", "", petijums), " ", gads)), vjust = -1, size = 3, show.legend = FALSE) +
  scale_x_continuous("Mācību gadi jaunajā saturā (Skola2030) pirms testa", breaks = 0:3) +
  scale_y_continuous("Latviešu valodas plūsmas relatīvās vietas pārmaiņa\npret iepriekšējo tā paša pētījuma ciklu (punkti)") +
  labs(title = "Skola2030 deva un Latvijai specifiskā pārmaiņa mērījumos 2016–2025",
       subtitle = "Relatīvā vieta = latviešu valodas plūsma mīnus salīdzinājuma valstu vidējais; 95 % intervāls (saites kļūda relatīvajā vietā izkrīt)",
       colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("output/pisa2025/plots/74_skola2030_deva.png", p, width = 8, height = 5.5, dpi = 200)

# ---------------------------------------------------------------------------
# B. Mācīšanas un resursu simptomi: LVA A pret 7 valstīm
# ---------------------------------------------------------------------------
S <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
MAIN <- c(AUTO_SC017Q01NA = "trūkst skolotāju", AUTO_SC017Q02NA = "nepiemēroti / vāji kvalificēti skolotāji",
          AUTO_SC017Q03NA = "trūkst atbalsta personāla", AUTO_SC017Q04NA = "nepiemērots atbalsta personāls",
          AUTO_SC017Q05NA = "trūkst mācību materiālu", AUTO_SC017Q06NA = "nepiemēroti / sliktas kvalitātes materiāli",
          AUTO_SC017Q07NA = "trūkst infrastruktūras", AUTO_SC017Q08NA = "nepiemērota infrastruktūra",
          AUTO_SC025Q01NA = "skolotāju daļa tālākizglītībā pēdējos 3 mēnešos, %",
          AUTO_TEACHSUP = "skolotāja atbalsts (skolēnu vērtējums, indekss)",
          AUTO_DISCLIM = "disciplīna stundās (indekss)", AUTO_DISCLIMA = "disciplīna stundās (indekss, 2000–2012)",
          L_IDX_STIMREAD = "lasīšanas stimulēšana (indekss)", SK_MACIBU_RESURSI = "mācību resursu indekss (saskaņots)")
MAIN <- MAIN[names(MAIN) %in% names(S)]
S[, grupa := fifelse(cnt == "LVA" & plusma == "A", "LVA A", fifelse(cnt %in% SALID, cnt, NA_character_))]
S <- S[!is.na(grupa) & gads %in% c("2015", "2018", "2022", "2025")]
for (k in names(MAIN)) S[get(k) < -900, (k) := NA]
long <- melt(S[, c("grupa", "gads", "skola", "w", names(MAIN)), with = FALSE], id.vars = c("grupa", "gads", "skola", "w"),
             variable.name = "mainigais", value.name = "x")[!is.na(x)]
# skolu klasteru SE: svērto skolu vidējo izkliede
agg <- long[, .(vid = sum(x * w) / sum(w),
                se = { sk <- .SD[, .(m = sum(x * w) / sum(w), sw = sum(w)), by = skola]
                       sqrt(sum(sk$sw^2 * (sk$m - sum(x * w) / sum(w))^2) / sum(sk$sw)^2) },
                n = .N), by = .(grupa, gads, mainigais)]
sal <- agg[grupa != "LVA A", .(sal_vid = mean(vid), sal_se = sqrt(sum(se^2)) / .N, n_valstis = .N), by = .(gads, mainigais)]
lva <- agg[grupa == "LVA A"]
T <- merge(lva[, .(gads, mainigais, lva = vid, lva_se = se)], sal, by = c("gads", "mainigais"))
T[, `:=`(starpiba = lva - sal_vid, starp_se = sqrt(lva_se^2 + sal_se^2))]
T[, apraksts := MAIN[as.character(mainigais)]]
setorder(T, mainigais, gads)
T[, d_starp := starpiba - shift(starpiba), by = mainigais]
T[, d_starp_se := sqrt(starp_se^2 + shift(starp_se)^2), by = mainigais]
fwrite(T, "output/pisa2025/skola2030_simptomi.csv")
cat("\nB. Mācīšanas simptomi: LVA A mīnus 7 valstu vidējais (starpība) un tās pārmaiņa pret iepriekšējo ciklu\n")
print(dcast(T, apraksts ~ gads, value.var = "starpiba", fun.aggregate = function(v) round(v[1], 2)))
cat("\nLVA A absolūtie līmeņi\n")
print(dcast(T, apraksts ~ gads, value.var = "lva", fun.aggregate = function(v) round(v[1], 2)))
cat("\n7 valstu vidējie\n")
print(dcast(T, apraksts ~ gads, value.var = "sal_vid", fun.aggregate = function(v) round(v[1], 2)))
cat("\nStarpības pārmaiņa 2022 -> 2025 ar SE un z\n")
print(T[gads == "2025", .(apraksts, d_starp = round(d_starp, 2), se = round(d_starp_se, 2), z = round(d_starp / d_starp_se, 1))])
cat("\nPabeigts:", format(Sys.time()), "\n")
sink()
