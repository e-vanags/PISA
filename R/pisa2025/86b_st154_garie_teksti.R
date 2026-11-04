# ============================================================================
# 86b. Garu tekstu lasīšana skolā: PISA 2018 ST154Q01HA pa valstīm
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS: cik lappušu garš bija garākais teksts, kas mācību gadā bija jālasa
# dzimtās valodas stundās (PISA 2018 skolēnu anketa ST154Q01HA), Latvijā pa
# valodas plūsmām un 7 salīdzinājuma valstīs. Papildina docs/58 papildinājumu
# (2026-09-21), kur bija tikai latviešu un krievu valodas plūsma un 7 valstu
# kopējais; šeit katra valsts atsevišķi ar BRR standartkļūdām, lai ziņojuma
# lapā būtu grafiks.
#
# DATI: data/pisa2025/processed/simptomi_2018.rds (stu), LVA + 7 valstis.
# KATEGORIJAS: 1 = viena lappuse vai mazāk, 2 = 2 līdz 10, 3 = 11 līdz 50,
#   4 = 51 līdz 100, 5 = 101 līdz 500, 6 = vairāk par 500. "Virs 100" = 5 un 6.
# GRUPAS: katra valsts (visi skolēni); Latvijai arī latviešu valodas plūsma
#   (LANGN 325) un krievu valodas plūsma (LANGN 495); 7 valstu vidējais
#   (valstu daļu vienkāršais vidējais, SE no valstu SE).
# SVARI: W_FSTUWT; SE ar BRR Fay (80 replikāti, koeficients 0,5).
#
# Izvade: output/pisa2025/st154_garie_teksti.csv (grupa x kategorija),
#   output/pisa2025/st154_virs100.csv (grupa: daļa virs 100 lpp),
#   output/pisa2025/plots/86b_st154_garie_teksti.png; ziņojuma lapa 7. sadaļa.
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
if (!file.exists("R/pisa2025") && file.exists("../../R/pisa2025")) setwd("../..")
output_file <- "output/pisa2025/logs/86b_st154_garie_teksti_output.txt"
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

NOS <- c(LVA = "Latvija, visi", `LVA A` = "Latvija, latviešu valodas plūsma", `LVA B` = "Latvija, krievu valodas plūsma",
         EST = "Igaunija", LTU = "Lietuva", POL = "Polija", FIN = "Somija", CZE = "Čehija", HUN = "Ungārija", DEU = "Vācija",
         `7 valstis` = "7 valstu vidējais")
KAT <- c("viena lappuse vai mazāk", "2 līdz 10 lappuses", "11 līdz 50 lappuses", "51 līdz 100 lappuses", "101 līdz 500 lappuses", "vairāk par 500 lappusēm")

# === 1. DATI =================================================================
cat(paste(rep("=", 70), collapse = ""), "\n1. Dati\n", paste(rep("=", 70), collapse = ""), "\n\n")
S <- as.data.table(readRDS("data/pisa2025/processed/simptomi_2018.rds")$stu)
RW <- paste0("W_FSTURWT", 1:80)
S <- S[, c("CNT", "LANGN", "ST154Q01HA", "W_FSTUWT", RW, grep("^PV[0-9]+READ$", names(S), value = TRUE)), with = FALSE]
for (v in c("CNT", "W_FSTUWT", RW, grep("^PV[0-9]+READ$", names(S), value = TRUE))) if (inherits(S[[v]], "haven_labelled")) S[[v]] <- unclass(S[[v]])
S[, kat := as.integer(unclass(ST154Q01HA))]
S[, LANGN := as.integer(unclass(LANGN))]
S[!is.na(kat) & (kat < 1 | kat > 6), kat := NA_integer_]
S[, grupa := CNT]
LA <- S[CNT == "LVA" & LANGN == 325][, grupa := "LVA A"]
LB <- S[CNT == "LVA" & LANGN == 495][, grupa := "LVA B"]
S <- rbind(S, LA, LB)
cat("Derīgas atbildes pa grupām:\n")
print(S[, .(n = sum(!is.na(kat)), trukst = round(100 * mean(is.na(kat)), 1)), by = grupa], row.names = FALSE)

# === 2. DAĻAS AR BRR =========================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n2. Daļas pa kategorijām ar BRR standartkļūdām\n", paste(rep("=", 70), collapse = ""), "\n\n")
dala_brr <- function(d, cond) {
  # cond: loģisks vektors (TRUE = skaitītājā); saucējs = derīgas atbildes
  ok <- !is.na(d$kat)
  est <- function(w) 100 * sum(w[ok & cond]) / sum(w[ok])
  th <- est(d$W_FSTUWT)
  rep <- vapply(RW, function(r) est(d[[r]]), numeric(1))
  list(dala = th, se = sqrt(sum((rep - th)^2) / 20))
}
res <- rbindlist(lapply(unique(S$grupa), function(g) {
  d <- S[grupa == g]
  rbindlist(c(lapply(1:6, function(k) { r <- dala_brr(d, d$kat == k); data.table(grupa = g, kategorija = k, nos = KAT[k], dala = r$dala, se = r$se) }),
              list({ r <- dala_brr(d, d$kat >= 5); data.table(grupa = g, kategorija = 56L, nos = "virs 100 lappusēm", dala = r$dala, se = r$se) })))
}))
res[, n := S[!is.na(kat), .N, by = grupa][match(res$grupa, grupa), N]]
# 7 valstu vidējais
SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
v7 <- res[grupa %in% SAL, .(grupa = "7 valstis", dala = mean(dala), se = sqrt(sum(se^2)) / .N, n = sum(n)), by = .(kategorija, nos)]
res <- rbind(res, v7[, names(res), with = FALSE])
res[, nosaukums := NOS[grupa]]
# vidējā lasīšana pa kategorijām (10 PV vidējais; bez SE, tikai orientācijai)
PV <- grep("^PV[0-9]+READ$", names(S), value = TRUE)
las <- S[!is.na(kat), .(lasisana = mean(vapply(PV, function(p) sum(get(p) * W_FSTUWT) / sum(W_FSTUWT), numeric(1)))), by = .(grupa, kategorija = kat)]
res <- merge(res, las, by = c("grupa", "kategorija"), all.x = TRUE)
setorder(res, grupa, kategorija)
cat("Daļa virs 100 lappusēm, %:\n")
print(res[kategorija == 56, .(nosaukums, dala = round(dala, 1), se = round(se, 2), n)][order(-dala)], row.names = FALSE)
cat("\nSadalījums pa kategorijām, % (rindas = grupa):\n")
print(dcast(res[kategorija <= 6], nosaukums ~ kategorija, value.var = "dala", fun.aggregate = function(x) round(x, 1)), row.names = FALSE)

fwrite(res[kategorija <= 6, .(grupa, nosaukums, kategorija, nos, dala = round(dala, 2), se = round(se, 2), n, lasisana = round(lasisana, 1))], "output/pisa2025/st154_garie_teksti.csv")
fwrite(res[kategorija == 56, .(grupa, nosaukums, dala = round(dala, 2), se = round(se, 2), n)][order(-dala)], "output/pisa2025/st154_virs100.csv")
cat("\nSaglabāts: output/pisa2025/st154_garie_teksti.csv, st154_virs100.csv\n")

# === 3. GRAFIKS ==============================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n3. Grafiks\n", paste(rep("=", 70), collapse = ""), "\n\n")
G <- res[kategorija == 56 & grupa != "7 valstis"][order(dala)]
G[, nosaukums := factor(nosaukums, levels = nosaukums)]
G[, kr := fifelse(grupa %in% c("LVA", "LVA A", "LVA B"), "Latvija", "salīdzinājuma valsts")]
p <- ggplot(G, aes(x = dala, y = nosaukums, colour = kr)) +
  geom_errorbarh(aes(xmin = dala - 1.96 * se, xmax = dala + 1.96 * se), height = 0.25) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.0f %%", dala)), nudge_y = 0.3, size = 3.2, colour = "grey30") +
  scale_colour_manual(values = c(Latvija = "#b3261e", `salīdzinājuma valsts` = "#2a78d6"), name = NULL) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 10)) +
  labs(title = "Skolēnu daļa, kam dzimtās valodas stundās mācību gadā bija jālasa teksts, garāks par 100 lappusēm",
       subtitle = "PISA 2018 skolēnu anketa ST154Q01HA; 95 % intervāls (BRR). Latvijas plūsmas pēc mājās runātās valodas.",
       x = "% skolēnu", y = NULL, caption = "Avots: OECD PISA 2018 mikrodati; R/pisa2025/86b_st154_garie_teksti.R") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title.position = "plot")
ggsave("output/pisa2025/plots/86b_st154_garie_teksti.png", p, width = 11, height = 6, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/86b_st154_garie_teksti.png\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili: st154_garie_teksti.csv, st154_virs100.csv, plots/86b_st154_garie_teksti.png\n")
cat("Beigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
