# ============================================================================
# 79. Slānis 2: 3. un 6. klases valodas darbu lasīšanas laikrinda 2009./10.–2021./22.
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS (docs/46 Slānis 2, docs/74). Valsts ieskaišu (līdz 2012./13.) un
# diagnosticējošo darbu (no 2013./14.) publiskie rezultāti VIAA gadu lapās ir
# vienīgais ikgadējais pilna aptvēruma lasītprasmes rādītājs Latvijā. Vai tajos
# redzams tas pats paaudžu kritums, ko PISA rāda 2018., 2022. un 2025. gadā?
#
# DATI. data/valsts_parbaudes_darbi/<m. g.>/ (324 faili, AVOTI.md) un no grafiku
# etiķetēm vizuāli nolasītās vērtības data/valsts_parbaudes_darbi/nolasijumi/:
#   dd_dalas_nolasitas.csv        daļu vidējā izpilde % (grafiku etiķetes ar 2 zīmēm)
#   dd_urbanizacija_nolasita.csv  Rīga / republikas pilsētas / pilsētas / lauki
#   dd_macibvaloda_nolasita.csv   3. kl. kopējais darbs pa skolu mācību valodu (līdz 2013./14.)
#   dd_histogrammas_stats_ocr.csv Mean / Std. Dev. / N no punktu sadalījuma histogrammām (OCR)
#   dd_histogrammas_pikselu_profili.csv  histogrammu stabiņu pikseļu profili (sadalījuma daļām)
#   data/pisa2025/processed/paaudzu_panelis.csv  PISA, PIRLS, TIMSS rinda (skripts 67a)
#
# METODE.
#   Gadi NAV psihometriski saskaņoti: katru gadu cits darbs, izpilde procentos bez
#   IRT saites. Tāpēc rinda tiek lasīta tikai relatīvi: z pēc visu gadu vidējā un SD
#   katrā sērijā, rangs, un A − B starpība tajā pašā gadā (tas pats darbs abām
#   plūsmām tikai 6. klasē? nē: 6. klasē A plūsmai latviešu valoda, B plūsmai krievu
#   valoda ir DAŽĀDI darbi, tāpēc A − B ir relatīva atskaite, ne tiešs salīdzinājums;
#   3. klasē līdz 2013./14. MV grafiks dod to pašu darbu pa skolu valodām).
#   3. klasē 2009./10.–2015./16. daļas ir uzdevumi, ne prasmes; lasīšana rekonstruēta
#   pēc KOPS 2013./14. (3. uzd. = apzināta lasītprasme) un 2016./17. UI struktūras
#   (U3–U4 = lasīšana): pamatvariants Uzd3, plašais variants Uzd3 + Uzd4.
#   Paaudzes: 3. kl. m. g. Y/Y+1 → dz. gads ≈ Y − 9; 6. kl. → Y − 12; PISA = dz. + 16.
#
# IZVADE.
#   output/pisa2025/dd_lasisana_2010_2022.csv, dd_sadalijums.csv, dd_urbanizacija.csv,
#   dd_paaudzu_rinda.csv, dd_z_panelis.csv, dd_ab_starpiba.csv,
#   plots/79_dd_laikrinda.png, 79_dd_dalas_6kl.png, 79_dd_paaudzes.png,
#   79_dd_ab_starpiba.png, 79_dd_urbanizacija.png, 79_dd_sadalijums.png,
#   logs/79_diagnosticejosie_darbi_output.txt
# ============================================================================

# --- Darba direktorija -------------------------------------------------------
if (!dir.exists("R/pisa2025")) {
  p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
  if (nzchar(p)) setwd(file.path(dirname(p), "..", ".."))
}
stopifnot(dir.exists("R/pisa2025"))

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/79_diagnosticejosie_darbi_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "ggplot2", "scales", "patchwork", "ggrepel")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Visas pakotnes ielādētas.\n\n")

TEMA <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"), legend.position = "bottom",
        plot.caption = element_text(colour = "grey35", hjust = 0))
KR <- c(A = "#0b5394", B = "#c0392b", kopa = "#5d6d7e", lat_maz = "#7a57c7")
ND <- "data/valsts_parbaudes_darbi/nolasijumi"

# === 1. DATU IELASE UN TABULA dd_lasisana_2010_2022.csv ======================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("1. Nolasīto vērtību ielase un daļu tabula\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

D <- fread(file.path(ND, "dd_dalas_nolasitas.csv"), encoding = "UTF-8")
D[, gads_sak := as.integer(substr(gads, 1, 4))]
D[, programma_lv := fcase(programma == "lat_lat", "A: latviešu valoda latviešu programmās",
                          programma == "kr", "B: krievu valoda mazākumtautību programmās",
                          programma == "lat_maz", "B: latviešu valoda (otrā valoda) mazākumtautību programmās",
                          programma == "macibv", "kopā: mācībvaloda (latviešu, krievu, poļu)")]
D[, darba_veids := fifelse(gads_sak <= 2012, "ieskaite", "diagnosticējošais darbs")]
D[, avots := sprintf("VIAA media/%s (%s)", id, fails)]
cat("Nolasītas", nrow(D), "vērtības no", uniqueN(D$fails), "grafikiem.\n")
cat("Metode: visas vērtības nolasītas vizuāli no SPSS grafiku etiķetēm (2 zīmes aiz komata);",
    "OCR (tesseract) sakrita tur, kur to izdevās nolasīt (9 etiķetes pārbaudītas).\n\n")

# 3. klases lasīšanas rekonstrukcija pirms 2016./17.
rek <- D[klase == 3 & programma %in% c("macibv", "lat_lat", "kr") & dala %in% c("Uzd3", "Uzd4"),
         .(v3 = videja[dala == "Uzd3"], v4 = videja[dala == "Uzd4"]), by = .(gads, gads_sak, klase, programma, programma_lv, darba_veids, fails, id, avots)]
rek3 <- rek[, .(gads, gads_sak, klase, programma, programma_lv, darba_veids, fails, id, avots,
                dala = "Lasīšana (rekonstruēta: Uzd3)", videja = v3, metode = "vizuāli (etiķete) + uzdevuma satura pieņēmums",
                piezime = "3. kl. daļas ir uzdevumi; 3. uzd. = apzināta lasītprasme (KOPS 2013./14.); 2012./13. darbā 7 uzdevumi, struktūra nezināma")]
rek34 <- rek[, .(gads, gads_sak, klase, programma, programma_lv, darba_veids, fails, id, avots,
                 dala = "Lasīšana (rekonstruēta: Uzd3+Uzd4)", videja = round((v3 + v4) / 2, 2), metode = "vizuāli (etiķete) + uzdevuma satura pieņēmums",
                 piezime = "vidējais no 3. un 4. uzd. (nesvērts); 2016./17. UI struktūrā lasīšana = U3–U4, 2013./14. KOPS 4. uzd. = vārdšķiras tekstā")]
D2 <- rbind(D[, .(gads, gads_sak, klase, programma, programma_lv, darba_veids, fails, id, avots, dala, videja, metode, piezime)], rek3, rek34)
D2[, salidzinamiba := fcase(
  gads_sak == 2020, "attālināti (Covid); 6. kl. citāda norise",
  gads_sak == 2013, "pirmais gads ar nosaukumu 'diagnosticējošais darbs', 2014. g. standarts",
  gads_sak <= 2012, "'ieskaite'; cits darbs katru gadu, bez IRT",
  klase == 3 & gads_sak <= 2015, "3. kl. daļas = uzdevumi; lasīšana rekonstruēta",
  gads_sak == 2021, "daļēji attālināts gads; PISA 2025 paaudze 6. kl.",
  default = "cits darbs katru gadu, bez IRT saites")]
setorder(D2, klase, programma, gads_sak, dala)
fwrite(D2[, .(gads, klase, programma, programma_lv, darba_veids, dala, videja, avots, metode, piezime, salidzinamiba)],
       "output/pisa2025/dd_lasisana_2010_2022.csv")
cat("Saglabāts: output/pisa2025/dd_lasisana_2010_2022.csv (", nrow(D2), "rindas )\n\n")

# Lasīšanas rinda: 6. kl. tieši; 3. kl. no 2016./17. tieši, agrāk rekonstruēta
las <- rbind(
  D2[klase == 6 & dala == "Lasīšana", .(gads, gads_sak, klase, programma, videja, tips = "tieši")],
  D2[klase == 3 & dala == "Lasīšana", .(gads, gads_sak, klase, programma, videja, tips = "tieši")],
  D2[klase == 3 & dala == "Lasīšana (rekonstruēta: Uzd3)", .(gads, gads_sak, klase, programma, videja, tips = "rekonstruēta (Uzd3)")])
kopa <- D2[dala == "Kopā", .(gads, gads_sak, klase, programma, kopa = videja)]
las <- merge(las, kopa, by = c("gads", "gads_sak", "klase", "programma"), all.x = TRUE)
# 3. kl. nepārtraukta kopējā rinda: mācībvaloda kopā; 2015./16. (kad kopējā grafika nav) latviešu programmu darbs
k3 <- las[klase == 3 & (programma == "macibv" | (programma == "lat_lat" & gads_sak == 2015))]
k3[, programma := "kopa3"]
las <- rbind(las, k3)
cat("Lasīšanas daļa pa gadiem (A plūsma = 6. kl. lat_lat; 3. kl. lat_lat tikai 2014./15.–2015./16., citos gados kopā; kopa3 = nepārtrauktā 3. kl. rinda):\n")
print(dcast(las[programma %in% c("lat_lat", "macibv", "kr")], gads ~ klase + programma, value.var = "videja"))
cat("\n")

# === 2. SADALĪJUMS: Mean, SD, N un daļa zem 50 % ============================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("2. Punktu sadalījums: kārtotāju skaits, vidējais, SD, daļa zem 50 % no maksimuma\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

S <- fread(file.path(ND, "dd_histogrammas_stats_ocr.csv"))
V <- fread(file.path(ND, "dd_histogrammas_vizuali_parbaude.csv"))
S[, c("gads_sak", "klase", "programma") := {
  g <- as.integer(substr(fails, 1, 4)); b <- basename(fails)
  list(g, as.integer(substr(b, 1, 1)), sub("^[36]_(.*)_PS_.*$", "\\1", b))
}]
S[, gads := sprintf("%d/%02d", gads_sak, (gads_sak + 1) %% 100)]
S[fails == "2014_2015/3_lat_maz_PS_31893.png", `:=`(mean = NA, sd = NA, n = NA)]  # lapā ievietots cits grafiks
# vizuālā pārbaude: 5 histogrammas nolasītas arī ar aci
S <- merge(S, V[, .(fails, mean_viz = mean, sd_viz = sd, n_viz = n)], by = "fails", all.x = TRUE)
cat("OCR pret vizuālo nolasījumu (5 histogrammas):\n"); print(S[!is.na(mean_viz), .(fails, mean, mean_viz, sd, sd_viz, n, n_viz)])
S[!is.na(mean_viz), `:=`(mean = mean_viz, sd = sd_viz, n = n_viz)]
S <- merge(S, kopa, by = c("gads", "gads_sak", "klase", "programma"), all.x = TRUE)
S[, max_punkti := round(mean / kopa * 100)]
S[, sd_pct := round(sd / max_punkti * 100, 2)]
S[, se_pct := round(sd_pct / sqrt(n), 3)]

# pikseļu profili: daļa zem sliekšņa no maksimālā punktu skaita
P <- fread(file.path(ND, "dd_histogrammas_pikselu_profili.csv"))
P <- merge(P, S[, .(fails, max_punkti, mean)], by = "fails")
P[, pct := vertiba / max_punkti * 100]
sad <- P[, .(mean_px = sum(vertiba * svars) / sum(svars),
             zem50 = sum(svars[pct < 50]) / sum(svars) * 100,
             zem40 = sum(svars[pct < 40]) / sum(svars) * 100,
             zem60 = sum(svars[pct < 60]) / sum(svars) * 100,
             virs80 = sum(svars[pct >= 80]) / sum(svars) * 100), by = fails]
S <- merge(S, sad, by = "fails", all.x = TRUE)
S[, mean_px_novirze := round(mean_px - mean, 2)]
S[, sadalijuma_metode := fifelse(is.na(mean_px), "nav (ass kalibrācija neizdevās)", "pikseļu profils, ass kalibrēta pēc OCR atzīmēm; pārbaude: pikseļu vidējais pret uzrakstīto Mean")]
S[abs(mean_px_novirze) > 2 & !is.na(mean_px_novirze), sadalijuma_metode := paste(sadalijuma_metode, "— NOVIRZE > 2 punkti, daļas neuzticamas")]
setorder(S, klase, programma, gads_sak)
fwrite(S[, .(gads, klase, programma, fails, n_kartotaji = n, videjais_punkti = mean, sd_punkti = sd, kopa_pct = kopa, max_punkti, sd_pct, se_pct,
             zem50 = round(zem50, 1), zem40 = round(zem40, 1), zem60 = round(zem60, 1), virs80 = round(virs80, 1), mean_px_novirze, metode_stats = "OCR (tesseract), 5 vizuāli pārbaudītas", sadalijuma_metode)],
       "output/pisa2025/dd_sadalijums.csv")
cat("\nKārtotāju skaits, vidējais (punktos), max punkti, daļa zem 50 %:\n")
print(S[programma %in% c("lat_lat", "macibv", "kr"), .(gads, klase, programma, n, mean, sd, max_punkti, zem50 = round(zem50, 1), novirze = mean_px_novirze)])
cat("Saglabāts: output/pisa2025/dd_sadalijums.csv\n\n")

# === 3. URBANIZĀCIJA ==========================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("3. Urbanizācijas plaisa (Rīga − lauki) pa gadiem\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
U <- fread(file.path(ND, "dd_urbanizacija_nolasita.csv"), encoding = "UTF-8")
U[, gads_sak := as.integer(substr(gads, 1, 4))]
U[, `:=`(plaisa_riga_lauki = round(`Rīga` - Lauki, 2), plaisa_pilsetas_lauki = round((`Rīga` + `Republikas pilsētas` + `Pilsētas`) / 3 - Lauki, 2))]
U <- merge(U, S[, .(gads, klase, programma, sd_pct)], by = c("gads", "klase", "programma"), all.x = TRUE)
U[, plaisa_sd := round(plaisa_riga_lauki / sd_pct, 3)]
setorder(U, klase, programma, gads_sak)
fwrite(U[, .(gads, klase, programma, `Rīga`, `Republikas pilsētas`, `Pilsētas`, Lauki, plaisa_riga_lauki, plaisa_pilsetas_lauki, sd_pct, plaisa_sd, avots = paste0("VIAA media/", id), metode)],
       "output/pisa2025/dd_urbanizacija.csv")
print(U[, .(gads, klase, programma, Riga = `Rīga`, Lauki, plaisa = plaisa_riga_lauki, plaisa_sd)])
cat("Saglabāts: output/pisa2025/dd_urbanizacija.csv\n\n")

# === 4. (a) LAIKRINDA AR LŪZUMIEM =============================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("4. (a) Laikrinda ar darba maiņas lūzumiem\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
las[, serija := fcase(klase == 6 & programma == "lat_lat", "6. kl., A plūsma (latviešu valoda)",
                      klase == 6 & programma == "kr", "6. kl., B plūsma (krievu valoda)",
                      klase == 6 & programma == "lat_maz", "6. kl., B plūsma, latviešu kā otrā valoda",
                      klase == 3 & programma == "macibv", "3. kl., kopā (mācībvaloda)",
                      klase == 3 & programma == "lat_lat", "3. kl., A plūsma (latviešu valoda)",
                      klase == 3 & programma == "kr", "3. kl., B plūsma (krievu valoda)",
                      klase == 3 & programma == "lat_maz", "3. kl., B plūsma, latviešu kā otrā valoda")]
las[, plusma := fcase(programma == "lat_lat", "A", programma == "kr", "B", programma == "macibv", "kopa", default = "lat_maz")]
las[, x := gads_sak + 0.5]
luzumi <- data.table(x = c(2013.5, 2020.5), teksts = c("2013./14.: ieskaite → diagnosticējošais darbs,\n2014. g. standarts", "2020./21.: attālināti (Covid)"), hj = c(0, 1), dx = c(0.1, -0.1))
lp <- las[programma %in% c("lat_lat", "kr", "macibv")]
p <- ggplot(lp, aes(x, videja, colour = plusma, linetype = tips, group = interaction(klase, programma, tips))) +
  geom_vline(data = luzumi, aes(xintercept = x), linetype = "dotted", colour = "grey40") +
  geom_text(data = luzumi, aes(x = x + dx, y = 95, label = teksts, hjust = hj), inherit.aes = FALSE, size = 3, colour = "grey30") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  geom_text_repel(aes(label = sprintf("%.0f", videja)), size = 2.7, show.legend = FALSE, max.overlaps = 30) +
  facet_wrap(~ paste0(klase, ". klase"), ncol = 1) +
  scale_colour_manual(values = KR, labels = c(A = "A plūsma (latviešu valoda latviešu programmās)", B = "B plūsma (krievu valoda mazākumtautību programmās)", kopa = "kopā (mācībvaloda: latviešu, krievu, poļu)")) +
  scale_linetype_manual(values = c(`tieši` = "solid", `rekonstruēta (Uzd3)` = "dashed"), name = "lasīšanas daļa") +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  scale_y_continuous(limits = c(45, 100)) +
  labs(title = "Lasīšanas daļas vidējā izpilde (%) valsts ieskaitēs un diagnosticējošajos darbos, 2009./10.–2021./22.",
       subtitle = "Gadi NAV psihometriski saskaņoti: katru gadu cits darbs, izpilde procentos bez IRT saites. Procentu pārmaiņa var būt darba grūtības pārmaiņa.",
       x = "Mācību gads", y = "Vidējā izpilde, %", colour = NULL,
       caption = "Avots: VISC/VIAA valsts pārbaudes darbu statistika, daļu salīdzinājuma grafiki (vērtības nolasītas no etiķetēm). 3. kl. līdz 2015./16. daļas ir uzdevumi: lasīšana = 3. uzdevums (pieņēmums).") +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.box = "vertical") + guides(colour = guide_legend(nrow = 1, order = 1), linetype = guide_legend(nrow = 1, order = 2))
ggsave("output/pisa2025/plots/79_dd_laikrinda.png", p, width = 12, height = 9.5, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/79_dd_laikrinda.png\n")

# visas daļas 6. kl. A un B
d6 <- D2[klase == 6 & programma %in% c("lat_lat", "kr") & dala %in% c("Lasīšana", "Klausīšanās", "Valodas lietojums", "Rakstīšana", "Runāšana", "Kopā")]
d6[, x := gads_sak + 0.5]
p <- ggplot(d6, aes(x, videja, colour = dala)) +
  geom_vline(xintercept = c(2013.5, 2020.5), linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  facet_wrap(~ fifelse(programma == "lat_lat", "A plūsma: latviešu valoda latviešu programmās", "B plūsma: krievu valoda mazākumtautību programmās")) +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  labs(title = "6. klase: visu daļu vidējā izpilde (%) pa gadiem", x = NULL, y = "Vidējā izpilde, %", colour = "daļa",
       subtitle = "Lasīšana krīt 2014./15. un 2015./16. (54 % A plūsmā), atkal 2018./19. un 2021./22.; runāšana visu laiku ap 80 %.",
       caption = "Avots: VISC/VIAA daļu salīdzinājuma grafiki. 2015./16. atsevišķi 'pareizrakstība' nav iekļauta.") +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/pisa2025/plots/79_dd_dalas_6kl.png", p, width = 12, height = 6, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/79_dd_dalas_6kl.png\n\n")

# Lūzumu lielums: vidējais pirms un pēc
for (k in c(6, 3)) for (pr in c("lat_lat", "kr", "macibv")) {
  s <- las[klase == k & programma == pr]
  if (nrow(s) < 5) next
  cat(sprintf("%d. kl. %s: vidēji 2009/10–2012/13 %.1f, 2013/14–2019/20 %.1f, 2020/21 %.1f, 2021/22 %.1f\n", k, pr,
              mean(s[gads_sak <= 2012, videja]), mean(s[gads_sak %in% 2013:2019, videja]), s[gads_sak == 2020, videja][1], s[gads_sak == 2021, videja][1]))
}
cat("\n")

# === 5. (b) PAAUDŽU PANELIS ===================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("5. (b) Paaudzes: DD relatīvi (z, rangs) blakus PISA, PIRLS, TIMSS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
Z <- las[programma %in% c("lat_lat", "kr", "macibv", "lat_maz", "kopa3")]
Z[, dz_gads := fifelse(klase == 3, gads_sak - 9L, gads_sak - 12L)]
Z[, pisa_gads := dz_gads + 16L]
Z[, `:=`(z = round((videja - mean(videja)) / sd(videja), 2), rangs = frank(-videja), n_gadi = .N,
         z_bez_covid = round((videja - mean(videja[gads_sak != 2020])) / sd(videja[gads_sak != 2020]), 2)), by = .(klase, programma)]
# kopā daļa (Kopā) z arī
Z[, z_kopa := round((kopa - mean(kopa, na.rm = TRUE)) / sd(kopa, na.rm = TRUE), 2), by = .(klase, programma)]
fwrite(Z[, .(gads, klase, programma, serija, tips, dz_gads, pisa_gads, lasisana = videja, kopa, z, z_bez_covid, rangs, n_gadi, z_kopa)], "output/pisa2025/dd_z_panelis.csv")
cat("z pēc visu gadu vidējā un SD (lasīšanas daļa), A plūsma un kopā:\n")
print(dcast(Z[programma %in% c("lat_lat", "kopa3", "kr") & !(klase == 3 & programma == "lat_lat")], gads + dz_gads + pisa_gads ~ klase + programma, value.var = "z"))
cat("\nRangs (1 = labākais gads):\n")
print(dcast(Z[programma %in% c("lat_lat", "kopa3", "kr") & !(klase == 3 & programma == "lat_lat")], gads ~ klase + programma, value.var = "rangs"))
cat("Saglabāts: output/pisa2025/dd_z_panelis.csv\n\n")

# A − B starpība 6. kl. (cits darbs katrai plūsmai, bet tā pati gada ietekme)
AB <- dcast(las[klase == 6 & programma %in% c("lat_lat", "kr")], gads + gads_sak ~ programma, value.var = "videja")
AB[, A_min_B := round(lat_lat - kr, 2)]
ABk <- dcast(D2[klase == 6 & programma %in% c("lat_lat", "kr") & dala == "Kopā"], gads ~ programma, value.var = "videja")
AB <- merge(AB, ABk[, .(gads, kopa_A = lat_lat, kopa_B = kr)], by = "gads")
AB[, A_min_B_kopa := round(kopa_A - kopa_B, 2)]
# 3. kl. tas pats darbs pa skolu valodām (MV grafiks) līdz 2013./14.; 2014./15.–2015./16. atsevišķi darbi
MV <- fread(file.path(ND, "dd_macibvaloda_nolasita.csv"), encoding = "UTF-8")
mv3 <- dcast(MV[klase == 3], gads ~ grupa, value.var = "videja")
mv3[, A_min_B_3kl_kopa := round(`latviešu` - krievu, 2)]
ab3 <- dcast(D2[klase == 3 & programma %in% c("lat_lat", "kr") & dala == "Kopā"], gads ~ programma, value.var = "videja")
ab3[, A_min_B_3kl_kopa := round(lat_lat - kr, 2)]
AB3 <- rbind(mv3[, .(gads, A_3kl = `latviešu`, B_3kl = krievu, A_min_B_3kl_kopa, avots3 = "tas pats darbs, MV grafiks")],
             ab3[, .(gads, A_3kl = lat_lat, B_3kl = kr, A_min_B_3kl_kopa, avots3 = "atsevišķi darbi, Kopā")])
las3 <- dcast(las[klase == 3 & programma %in% c("lat_lat", "kr")], gads ~ programma, value.var = "videja")
if (nrow(las3)) { las3[, A_min_B_3kl_lasisana := round(lat_lat - kr, 2)]; AB3 <- merge(AB3, las3[, .(gads, A_min_B_3kl_lasisana)], by = "gads", all.x = TRUE) }
AB <- merge(AB, AB3, by = "gads", all = TRUE)
setorder(AB, gads)
fwrite(AB, "output/pisa2025/dd_ab_starpiba.csv")
cat("A − B starpība pa gadiem (6. kl. lasīšana un kopā; 3. kl. kopā pa skolu valodām):\n")
print(AB[, .(gads, A6 = lat_lat, B6 = kr, A_min_B, A_min_B_kopa, A_3kl, B_3kl, A_min_B_3kl_kopa)])
cat("Saglabāts: output/pisa2025/dd_ab_starpiba.csv\n\n")

# PISA, PIRLS, TIMSS panelis LVA A
PP <- fread("data/pisa2025/processed/paaudzu_panelis.csv", encoding = "UTF-8")
PA <- PP[grupa == "LVA A" & `radītājs` == "vid", .(petijums, gads, est, se, dz_gads = round(dz_gads), vecums)]
PA[, z := round((est - mean(est)) / sd(est), 2), by = petijums]
PA[, se_z := round(se / sd(est), 2), by = petijums]
# relatīvi pret salīdzinājuma valstīm (kā 70. skripta V1): LVA A − vidējais pārējām
SAL <- c("EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
REL <- PP[`radītājs` == "vid", { o <- .SD[grupa %in% SAL]; l <- .SD[grupa == "LVA A"]
  if (nrow(l) && nrow(o) >= 3) .(rel = l$est - mean(o$est), se = l$se, dz_gads = round(dz_gads[1])) else NULL }, by = .(petijums, gads)]
REL[, z_rel := round((rel - mean(rel)) / sd(rel), 2), by = petijums]
cat("PISA / PIRLS / TIMSS, LVA A: absolūtais z un relatīvais (pret 7 valstīm) z pa paaudzēm:\n")
print(merge(PA[, .(petijums, gads, dz_gads, est = round(est, 1), z)], REL[, .(petijums, gads, rel = round(rel, 1), z_rel)], by = c("petijums", "gads"), all.x = TRUE)[order(petijums, gads)])

# Paaudžu panelis: DD (3./6. kl.) un starptautiskie uz vienas dzimšanas gada ass
pan <- rbind(
  Z[(klase == 6 & programma == "lat_lat") | (klase == 3 & programma == "kopa3"), .(petijums = paste0("DD", klase, " lasīšana", fifelse(klase == 3, " (kopā; līdz 2015./16. rekonstruēta)", " (A plūsma)")), dz_gads, z, se_z = NA_real_, veids = "DD")],
  PA[, .(petijums, dz_gads, z, se_z, veids = "starptautiskais")],
  REL[, .(petijums = paste(petijums, "relatīvi pret 7 valstīm"), dz_gads, z = z_rel, se_z = NA_real_, veids = "starptautiskais, relatīvi")])
cat("\nPaaudžu panelis (z) pa dzimšanas gadiem:\n")
print(dcast(pan[petijums %in% c("DD3 lasīšana (kopā; līdz 2015./16. rekonstruēta)", "DD6 lasīšana (A plūsma)", "PISA lasīšana", "PIRLS lasīšana", "PISA lasīšana relatīvi pret 7 valstīm")], dz_gads ~ petijums, value.var = "z"))

pan[, forma := fifelse(veids == "DD", 21, fifelse(veids == "starptautiskais", 22, 24))]
pan_p <- pan[petijums %in% c("DD3 lasīšana (kopā; līdz 2015./16. rekonstruēta)", "DD6 lasīšana (A plūsma)", "PISA lasīšana", "PIRLS lasīšana", "TIMSS4 matemātika", "PISA lasīšana relatīvi pret 7 valstīm")]
pisa_k <- data.table(dz = c(1999, 2002, 2006, 2009), lab = c("PISA 2015", "PISA 2018", "PISA 2022", "PISA 2025"))
p <- ggplot(pan_p[dz_gads >= 1996], aes(dz_gads, z, colour = petijums, shape = petijums)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(data = pisa_k, aes(xintercept = dz), linetype = "dotted", colour = "grey50") +
  geom_line(data = pan_p[dz_gads >= 1996 & veids == "DD"], linewidth = 0.7) +
  geom_line(data = pan_p[dz_gads >= 1996 & veids != "DD"], linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 2.6) +
  geom_errorbar(data = pan_p[dz_gads >= 1996 & !is.na(se_z)], aes(ymin = z - 1.96 * se_z, ymax = z + 1.96 * se_z), width = 0.2, alpha = 0.6) +
  scale_x_continuous("Dzimšanas gads (paaudze); DD3 = 3. klase m. g. dz + 9, DD6 = 6. klase m. g. dz + 12", breaks = seq(1996, 2012, 1),
                     sec.axis = sec_axis(~ . + 16, name = "PISA mērījuma gads (dzimšanas gads + 16)", breaks = c(2012, 2015, 2018, 2022, 2025))) +
  scale_y_continuous("z (novirze no sērijas visu gadu vidējā, SD vienībās)") +
  scale_colour_manual(values = c(`DD3 lasīšana (kopā; līdz 2015./16. rekonstruēta)` = "#2e86c1", `DD6 lasīšana (A plūsma)` = "#0b5394", `PISA lasīšana` = "#0b0b0b", `PISA lasīšana relatīvi pret 7 valstīm` = "grey45", `PIRLS lasīšana` = "#2a78d6", `TIMSS4 matemātika` = "#eb6834")) +
  scale_shape_manual(values = c(`DD3 lasīšana (kopā; līdz 2015./16. rekonstruēta)` = 16, `DD6 lasīšana (A plūsma)` = 16, `PISA lasīšana` = 15, `PISA lasīšana relatīvi pret 7 valstīm` = 0, `PIRLS lasīšana` = 17, `TIMSS4 matemātika` = 18)) +
  labs(title = "Paaudžu panelis: valsts darbu lasīšana 3. un 6. klasē blakus PISA, PIRLS un TIMSS (latviešu valodas plūsma)",
       subtitle = "Katrai sērijai z pēc tās pašas sērijas visu gadu vidējā; DD gadi nav IRT saskaņoti, tāpēc z ir relatīvā vieta starp gadiem, ne prasmju līmenis",
       colour = NULL, shape = NULL, caption = "Avoti: VISC/VIAA (DD), OECD PISA, IEA PIRLS un TIMSS (data/pisa2025/processed/paaudzu_panelis.csv). 2020./21. DD attālināti (dz. 2011 3. kl., dz. 2008 6. kl.).") +
  TEMA + guides(colour = guide_legend(nrow = 2))
ggsave("output/pisa2025/plots/79_dd_paaudzes.png", p, width = 13, height = 7.5, dpi = 200, bg = "white")
cat("\nSaglabāts: output/pisa2025/plots/79_dd_paaudzes.png\n")

# A − B grafiks
ABl <- melt(AB[, .(gads, `6. kl. lasīšana` = A_min_B, `6. kl. kopā` = A_min_B_kopa, `3. kl. kopā` = A_min_B_3kl_kopa)], id.vars = "gads", variable.name = "raditajs", value.name = "starpiba")
ABl[, x := as.integer(substr(gads, 1, 4)) + 0.5]
p <- ggplot(ABl[!is.na(starpiba)], aes(x, starpiba, colour = raditajs)) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_vline(xintercept = c(2013.5, 2020.5), linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  labs(title = "A − B starpība tajā pašā gadā: latviešu programmas mīnus mazākumtautību programmas, procentpunkti",
       subtitle = "6. kl.: latviešu valoda (A) pret krievu valodu (B), dažādi darbi; 3. kl. līdz 2013./14. tas pats darbs pa skolu mācību valodu, 2014./15.–2015./16. atsevišķi darbi",
       x = NULL, y = "A − B, procentpunkti", colour = NULL, caption = "Avots: VISC/VIAA daļu salīdzinājuma un mācību valodas grafiki.") +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/pisa2025/plots/79_dd_ab_starpiba.png", p, width = 11, height = 6, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/79_dd_ab_starpiba.png\n\n")

# === 6. (c) 2015 → 2018 PISA LŪZUMS 6. UN 3. KLASĒ ============================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("6. (c) Vai PISA 2015 → 2018 lūzums (paaudzes 1999 → 2002) redzams 6. kl. 2011./12.–2014./15. un 3. kl. 2011./12.–2014./15.?\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
c6 <- Z[klase == 6 & programma %in% c("lat_lat", "kr") & dz_gads %in% 1997:2006, .(gads, programma, dz_gads, pisa_gads, lasisana = videja, z, rangs, kopa)]
cat("6. klase, paaudzes 1997–2006 (PISA 2013–2022):\n"); print(c6[order(programma, dz_gads)])
c3 <- Z[klase == 3 & programma %in% c("kopa3", "kr") & dz_gads %in% 2000:2006, .(gads, programma, dz_gads, pisa_gads, lasisana = videja, tips, z, rangs, kopa)]
cat("\n3. klase, paaudzes 2000–2006:\n"); print(c3[order(programma, dz_gads)])
cat("\nA − B 6. kl. lasīšanā paaudzēm 1999 (2011./12.) un 2002 (2014./15.):", AB[gads == "2011/12", A_min_B], "→", AB[gads == "2014/15", A_min_B], "\n")
cat("A − B 3. kl. kopā (tas pats darbs) paaudzēm 2000–2004:\n"); print(AB[gads %in% c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14"), .(gads, A_3kl, B_3kl, A_min_B_3kl_kopa)])
cat("\n")

# === 7. (d) 6. KL. 2021./22. (PISA 2025 PAAUDZE) ==============================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("7. (d) 6. kl. 2021./22. (dz. 2009, PISA 2025) pret iepriekšējiem gadiem un pret 2020./21.\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
d21 <- D2[klase == 6 & programma %in% c("lat_lat", "kr", "lat_maz") & gads_sak >= 2016 & dala %in% c("Lasīšana", "Klausīšanās", "Valodas lietojums", "Rakstīšana", "Runāšana", "Kopā")]
print(dcast(d21, programma + dala ~ gads, value.var = "videja"))
z21 <- Z[klase == 6 & programma %in% c("lat_lat", "kr", "lat_maz"), .(gads, programma, lasisana = videja, z, z_bez_covid, rangs, n_gadi)]
cat("\nz un rangs 2018./19.–2021./22.:\n"); print(z21[gads %in% c("2018/19", "2019/20", "2020/21", "2021/22")][order(programma, gads)])
cat("\n2021./22. pret 2016./17.–2019./20. vidējo (lasīšana):\n")
print(Z[klase == 6 & programma %in% c("lat_lat", "kr", "lat_maz"), .(videjais_2016_2019 = round(mean(videja[gads_sak %in% 2016:2019]), 2), v_2020 = videja[gads_sak == 2020], v_2021 = videja[gads_sak == 2021],
                                                                     starpiba_2021_pret_2016_2019 = round(videja[gads_sak == 2021] - mean(videja[gads_sak %in% 2016:2019]), 2), starpiba_2021_pret_2020 = round(videja[gads_sak == 2021] - videja[gads_sak == 2020], 2)), by = programma])
cat("\nSadalījums 6. kl. A plūsma (daļa zem 50 % no max punktiem):\n"); print(S[klase == 6 & programma == "lat_lat", .(gads, n, mean, max_punkti, zem50 = round(zem50, 1), zem40 = round(zem40, 1), virs80 = round(virs80, 1))])
cat("\n")

# === 8. (e) URBANIZĀCIJAS PLAISA KĀ STARPSKOLU IZKLIEDES AIZSTĀJĒJS ===========
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("8. (e) Urbanizācijas plaisa pa gadiem (docs/64 69d: A plūsmas ICC 0,20 (2015, 2018) → 0,25 (2022) → 0,30 (2025))\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
Ul <- U[programma %in% c("lat_lat", "kr", "macibv")]
Ul[, serija := paste0(klase, ". kl. ", fcase(programma == "lat_lat", "A (latviešu valoda)", programma == "kr", "B (krievu valoda)", default = "kopā (mācībvaloda)"))]
Ul[, x := gads_sak + 0.5]
for (s in unique(Ul$serija)) {
  d <- Ul[serija == s][order(gads_sak)]
  if (nrow(d) < 4) { cat(sprintf("%s: tikai %d gadi, trends nav rēķināts\n", s, nrow(d))); next }
  m <- lm(plaisa_riga_lauki ~ gads_sak, d)
  cat(sprintf("%s: plaisa Rīga − lauki vidēji %.1f pp (min %.1f %s, max %.1f %s); lineārais trends %+.2f pp gadā (p = %.2f)\n", s, mean(d$plaisa_riga_lauki), min(d$plaisa_riga_lauki), d$gads[which.min(d$plaisa_riga_lauki)],
              max(d$plaisa_riga_lauki), d$gads[which.max(d$plaisa_riga_lauki)], coef(m)[2], summary(m)$coefficients[2, 4]))
}
p1 <- ggplot(Ul, aes(x, plaisa_riga_lauki, colour = serija)) +
  geom_vline(xintercept = c(2013.5, 2020.5), linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  labs(title = "Urbanizācijas plaisa: Rīga − lauki, procentpunkti (viss darbs)", x = NULL, y = "Rīga − lauki, pp", colour = NULL) +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1))
Ul2 <- melt(Ul[, .(x, serija, `Rīga`, `Republikas pilsētas`, `Pilsētas`, Lauki)], id.vars = c("x", "serija"), variable.name = "vieta", value.name = "videja")
p2 <- ggplot(Ul2[serija == "6. kl. A (latviešu valoda)"], aes(x, videja, colour = vieta)) +
  geom_vline(xintercept = c(2013.5, 2020.5), linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  labs(title = "6. kl. A plūsma: vidējā izpilde pēc urbanizācijas", x = NULL, y = "Vidējā izpilde, %", colour = NULL,
       caption = "Avots: VISC/VIAA 'Salīdzinājums pēc urbanizācijas'. PISA A plūsmas starpskolu ICC: 0,20 (2015, 2018) → 0,25 (2022) → 0,30 (2025), docs/64.") +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/pisa2025/plots/79_dd_urbanizacija.png", p1 / p2, width = 11, height = 9, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/79_dd_urbanizacija.png\n\n")

# Sadalījuma grafiks
Sl <- S[programma %in% c("lat_lat", "kr", "macibv") & !is.na(zem50) & abs(mean_px_novirze) <= 2]
Sl[, serija := paste0(klase, ". kl. ", fcase(programma == "lat_lat", "A (latviešu valoda)", programma == "kr", "B (krievu valoda)", default = "kopā (mācībvaloda)"))]
Sl[, x := gads_sak + 0.5]
p <- ggplot(Sl, aes(x, zem50, colour = serija)) +
  geom_vline(xintercept = c(2013.5, 2020.5), linetype = "dotted", colour = "grey40") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2009:2021 + 0.5, labels = sprintf("%d/%02d", 2009:2021, (2010:2022) %% 100)) +
  labs(title = "Skolēnu daļa ar kopvērtējumu zem 50 % no maksimālā punktu skaita (viss darbs)",
       subtitle = "No punktu sadalījuma histogrammām (pikseļu profils, ass kalibrēta pēc OCR atzīmēm; pikseļu vidējais pret uzrakstīto Mean sakrīt ± 1 punkta robežās)",
       x = NULL, y = "Daļa zem 50 %, %", colour = NULL, caption = "Avots: VISC/VIAA 'Punktu sadalījums'. Maksimālais punktu skaits = Mean / Kopā % (pārbaudīts pret histogrammas labo galu).") +
  TEMA + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/pisa2025/plots/79_dd_sadalijums.png", p, width = 11, height = 6, dpi = 200, bg = "white")
cat("Saglabāts: output/pisa2025/plots/79_dd_sadalijums.png\n\n")

# === 9. (f) RINDA PAAUDŽU PANEĻA FORMĀTĀ ======================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("9. (f) Atsevišķs fails paaudžu paneļa formātā (panelis netiek mainīts)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
R6 <- merge(Z[klase == 6 & programma == "lat_lat"], S[klase == 6 & programma == "lat_lat", .(gads, n, se_pct)], by = "gads", all.x = TRUE)
S3 <- S[klase == 3 & programma %in% c("macibv", "lat_lat") & !(programma == "lat_lat" & gads_sak == 2014), .(gads, n, se_pct, programma_avots = programma)]
R3 <- merge(Z[klase == 3 & programma == "kopa3"], S3, by = "gads", all.x = TRUE)
rinda <- rbind(
  R6[, .(petijums = "DD6 lasīšana", gads = gads_sak + 1L, grupa = "LVA A", `radītājs` = "vid", est = videja, se = se_pct, n, vecums = 12.5, dz_gads = dz_gads + 0.5, publ = sprintf("%d-06", gads_sak + 1L), est_z = z, se_z = round(se_pct / sd(videja), 3), piezime = "latviešu valoda latviešu programmās; % izpilde, gadi nav IRT saskaņoti; se = SD%/sqrt(N) tikai izlases troksnis, ne darba maiņa")],
  R3[, .(petijums = "DD3 lasīšana", gads = gads_sak + 1L, grupa = fifelse(programma_avots == "lat_lat", "LVA A", "LVA"), `radītājs` = "vid", est = videja, se = se_pct, n, vecums = 9.5, dz_gads = dz_gads + 0.5, publ = sprintf("%d-06", gads_sak + 1L), est_z = z, se_z = round(se_pct / sd(videja), 3),
         piezime = fifelse(tips == "tieši", "3. kl. mācībvaloda (latviešu, krievu, poļu kopā; A ≈ 3/4); % izpilde", "3. kl. lasīšana rekonstruēta = 3. uzdevums (pieņēmums); mācībvaloda kopā"))])
rinda[gads == 2021, piezime := paste(piezime, "; attālināti (Covid)")]
setorder(rinda, petijums, gads)
fwrite(rinda, "output/pisa2025/dd_paaudzu_rinda.csv")
print(rinda[, .(petijums, gads, grupa, est, se, n, dz_gads, est_z)])
cat("Saglabāts: output/pisa2025/dd_paaudzu_rinda.csv (panelis data/pisa2025/processed/paaudzu_panelis.csv NAV mainīts)\n\n")

# === 10. KOPSAVILKUMS =========================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("10. Kopsavilkums\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat("Izvades faili:\n  output/pisa2025/dd_lasisana_2010_2022.csv\n  output/pisa2025/dd_sadalijums.csv\n  output/pisa2025/dd_urbanizacija.csv\n  output/pisa2025/dd_z_panelis.csv\n  output/pisa2025/dd_ab_starpiba.csv\n  output/pisa2025/dd_paaudzu_rinda.csv\n",
    "  output/pisa2025/plots/79_dd_laikrinda.png, 79_dd_dalas_6kl.png, 79_dd_paaudzes.png, 79_dd_ab_starpiba.png, 79_dd_urbanizacija.png, 79_dd_sadalijums.png\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
