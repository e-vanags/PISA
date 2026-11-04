# ============================================================================
# 83c. Robežjosla kā koridors ap Latvijas trajektoriju (skaidrāka vizualizācija)
#
# Ideja: kvantiļu regresijas grafiks (83) ir grūti lasāms, jo uz vienas ass ir
# relatīvi lielumi visām valstīm. Šeit tas pats saturs parādīts Latvijas laika
# rindā: no katra cikla punkta uz priekšu zīmēts koridors, kurā vēsturiski
# iekrīt 50 % (tumšāks) un 90 % (gaišāks) valstu pārmaiņu no līdzīga līmeņa.
# Nākamais Latvijas punkts vai nu ir koridorā, vai nav. Otrs attēls: Latvijas
# katra pāra procentile visu valstu sadalījumā kā stabiņi.
#
# Ievade: output/pisa2025/aploksne_pari.csv (83), output/pisa2025/korigetie_raditaji.csv (72)
# Izvade: output/pisa2025/plots/83_koridors_lasisana.png, 83_procentiles_lasisana.png,
#         output/pisa2025/aploksne_koridors.csv
# ============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
setwd(file.path(Sys.getenv("PISA_SAKNE", unset = "/Users/e_vanags/Documents/PISA datu analīze 2025")))

PA <- fread("output/pisa2025/aploksne_pari.csv", encoding = "UTF-8")
KR <- fread("output/pisa2025/korigetie_raditaji.csv", encoding = "UTF-8")
setnames(KR, names(KR)[1], "raditajs")
R1 <- PA[joma == "READ" & logs == 1]                       # secīgi ciklu pāri, visas valstis
LOGA_PLATUMS <- 30                                          # sākuma līmenis ± 30 punkti

# --- Latvijas rindas -------------------------------------------------------
lv_visa <- unique(rbind(R1[valsts == "Latvia", .(gads = no, vertiba = l_no)],
                        R1[valsts == "Latvia", .(gads = uz, vertiba = l_uz)]))[order(gads)]
lv_visa[, rinda := "Latvija, visa valsts (oficiāli)"]
A <- KR[raditajs == "vid" & !is.na(korigets), .(gads, vertiba = korigets)][order(gads)]
A[, rinda := "Latviešu valodas plūsma (koriģēti: instrumenta nobīde no 2018, iesaiste 2025)"]
LV <- rbind(lv_visa, A)

# --- koridors: no katra punkta uz nākamo ciklu ------------------------------
kv <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
KOR <- LV[, {
  g <- gads; v <- vertiba; out <- list()
  for (i in seq_len(length(g) - 1)) {
    ref <- R1[valsts != "Latvia" & abs(l_no - v[i]) <= LOGA_PLATUMS]          # visa vēsture, līdzīgs līmenis
    refp <- ref[no == g[i] & uz == g[i + 1]]                                  # tas pats pāris, līdzīgs līmenis
    gadi <- g[i + 1] - g[i]
    out[[i]] <- data.table(no = g[i], uz = g[i + 1], v_no = v[i], v_uz = v[i + 1], gadi = gadi, n_ref = nrow(ref), n_paris = nrow(refp),
      P5 = v[i] + gadi * kv(ref$gada, .05), P25 = v[i] + gadi * kv(ref$gada, .25), P50 = v[i] + gadi * kv(ref$gada, .50),
      P75 = v[i] + gadi * kv(ref$gada, .75), P95 = v[i] + gadi * kv(ref$gada, .95),
      paris_P50 = if (nrow(refp) >= 5) v[i] + gadi * kv(refp$gada, .50) else NA_real_,
      procentile = 100 * mean(ref$gada < (v[i + 1] - v[i]) / gadi),
      paris_procentile = if (nrow(refp) >= 5) 100 * mean(refp$gada < (v[i + 1] - v[i]) / gadi) else NA_real_)
  }
  rbindlist(out)
}, by = rinda]
KOR[, kur := fifelse(v_uz < P5, "zem P5 (ārpus joslas)", fifelse(v_uz > P95, "virs P95 (ārpus joslas)",
             fifelse(v_uz < P25, "apakšējā ceturtdaļa", fifelse(v_uz > P75, "augšējā ceturtdaļa", "vidējā puse"))))]
fwrite(KOR, "output/pisa2025/aploksne_koridors.csv")
cat("Koridors:\n"); print(KOR[, .(rinda = substr(rinda, 1, 22), no, uz, v_no = round(v_no), v_uz = round(v_uz), P5 = round(P5), P50 = round(P50), P95 = round(P95), n_ref, procentile = round(procentile), paris_procentile = round(paris_procentile), kur)])

# --- 1. attēls: koridors -----------------------------------------------------
LV[, rinda := factor(rinda, levels = unique(LV$rinda))]; KOR[, rinda := factor(rinda, levels = levels(LV$rinda))]
KOR[, arpus := v_uz < P5 | v_uz > P95]
# ķīļi: no sākuma punkta uz nākamā cikla joslu
kilis <- function(K, apaksa, augsa, kl) rbindlist(lapply(seq_len(nrow(K)), function(i) data.table(
  rinda = K$rinda[i], id = paste(kl, i), x = c(K$no[i], K$uz[i], K$uz[i]), y = c(K$v_no[i], K[[augsa]][i], K[[apaksa]][i]))))
K90 <- kilis(KOR, "P5", "P95", "k90"); K50 <- kilis(KOR, "P25", "P75", "k50")
p1 <- ggplot() +
  geom_polygon(data = K90, aes(x = x, y = y, group = id), fill = "#9DB4D6", alpha = 0.4) +
  geom_polygon(data = K50, aes(x = x, y = y, group = id), fill = "#5B85C4", alpha = 0.5) +
  geom_segment(data = KOR, aes(x = no, xend = uz, y = v_no, yend = P50), colour = "#1F4E9C", linetype = "dotted", linewidth = 0.5) +
  geom_point(data = KOR[!is.na(paris_P50)], aes(x = uz, y = paris_P50), shape = 4, size = 2.6, colour = "#1F4E9C", stroke = 0.9) +
  geom_line(data = LV, aes(x = gads, y = vertiba), colour = "#B3261E", linewidth = 1.1) +
  geom_point(data = LV, aes(x = gads, y = vertiba), colour = "#B3261E", size = 3) +
  geom_point(data = KOR[arpus == TRUE], aes(x = uz, y = v_uz), shape = 21, size = 6.5, colour = "#B3261E", fill = NA, stroke = 1.2) +
  geom_text(data = LV, aes(x = gads, y = vertiba, label = round(vertiba)), vjust = -1.1, size = 3, colour = "#B3261E") +
  facet_wrap(~ rinda, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)) +
  labs(title = "Latvijas lasīšanas rinda un vēsturiski parastais koridors",
       subtitle = paste0("No katra punkta uz priekšu: kur nonāk valstis, kas sākušas līdzīgā līmenī (± ", LOGA_PLATUMS, " punkti), visa PISA vēsture. ",
                         "Tumši zilā josla ietver vidējos 50 % gadījumu (P25 līdz P75), gaiši zilā 90 % (P5 līdz P95), punktētā līnija ir mediāna.\n",
                         "Krustiņš: kur tajā pašā periodā nonāca tipiska līdzīga līmeņa valsts. Aplis ap punktu: Latvija ārpus 90 % joslas."),
       x = NULL, y = "PISA lasīšanas punkti") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0), plot.subtitle = element_text(size = 9.5))
png("output/pisa2025/plots/83_koridors_lasisana.png", width = 2400, height = 2000, res = 200); print(p1); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_koridors_lasisana.png\n")

# --- 2. attēls: procentiles kā stabiņi ---------------------------------------
KOR[, paris := paste0(no, " līdz ", uz)]
KOR[, paris := factor(paris, levels = unique(paris[order(no)]))]
PR <- melt(KOR, id.vars = c("rinda", "paris"), measure.vars = c("procentile", "paris_procentile"), variable.name = "mers", value.name = "pct")
PR[, mers := factor(mers, levels = c("procentile", "paris_procentile"),
                    labels = c("pret visu vēsturi (līdzīgs līmenis)", "pret to pašu periodu (līdzīgs līmenis)"))]
p2 <- ggplot(PR[!is.na(pct)], aes(x = paris, y = pct, fill = mers)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 25, ymax = 75, fill = "#DDE6F3", alpha = 0.6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 5, fill = "#F3D6D3", alpha = 0.8) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 95, ymax = 100, fill = "#F3D6D3", alpha = 0.8) +
  geom_hline(yintercept = 50, colour = "#555555", linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = round(pct)), position = position_dodge(width = 0.7), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#5B85C4", "#B3261E"), name = NULL) +
  scale_y_continuous(limits = c(0, 104), breaks = c(0, 5, 25, 50, 75, 95, 100)) +
  facet_wrap(~ rinda, ncol = 1) +
  labs(title = "Kur Latvijas pārmaiņa iekrīt starp visām valstīm: procentile katram ciklu pārim",
       subtitle = "0 = lielākais kritums, kāds jebkad novērots līdzīgā līmenī, 100 = lielākais kāpums; 50 = tipiska valsts. Gaišā josla 25 līdz 75 ir parasts; sarkanās malas zem 5 un virs 95 ir ārpus 90 % joslas.",
       x = NULL, y = "Procentile") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "top", strip.text = element_text(face = "bold", hjust = 0), plot.subtitle = element_text(size = 9.5))
png("output/pisa2025/plots/83_procentiles_lasisana.png", width = 2400, height = 1800, res = 200); print(p2); dev.off()
cat("Saglabāts: output/pisa2025/plots/83_procentiles_lasisana.png\n")
