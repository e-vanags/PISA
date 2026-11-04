# ============================================================================
# 84b. Lasāmi grafiki ieteikumam kolēģim (docs/87): iespēju koridors ar konkrētām valstīm
#
# 1. attēls: latviešu valodas plūsmas lasīšanas vidējais 2003 līdz 2025 un koridors 2025 līdz 2035
#    (grīda P25, tipiski P50, mērķis P75; gaišāk P10 līdz P95), plus piemēru valstu faktiskās
#    trajektorijas, pārbīdītas tā, lai sāktos Latvijas 2025. gada līmenī.
# 2. attēls: skolēnu daļa zem 2. līmeņa trijās jomās, vēsture un koridors, 15 % līnija.
# Ievade: aploksne_pari.csv (83), merku_scenariji_videjie.csv, merku_scenariji_izm.csv,
#         merku_scenariji_a_limeni.csv (84), korigetie_raditaji.csv (72)
# Izvade: plots/84b_koridors_valstis.png, plots/84b_zem_L2_koridors.png, merku_piemeru_valstis.csv
# ============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
setwd(Sys.getenv("PISA_SAKNE", unset = "/Users/e_vanags/Documents/PISA datu analīze 2025"))

PA <- fread("output/pisa2025/aploksne_pari.csv", encoding = "UTF-8")
VID <- fread("output/pisa2025/merku_scenariji_videjie.csv", encoding = "UTF-8"); setnames(VID, names(VID)[1], "joma")
IZM <- fread("output/pisa2025/merku_scenariji_izm.csv", encoding = "UTF-8"); setnames(IZM, names(IZM)[1], "joma_nos")
AL <- fread("output/pisa2025/merku_scenariji_a_limeni.csv", encoding = "UTF-8"); setnames(AL, names(AL)[1], "joma")
KR <- fread("output/pisa2025/korigetie_raditaji.csv", encoding = "UTF-8"); setnames(KR, names(KR)[1], "raditajs")
LV_NOS <- c(VALSTS = "Latvija")

# --- 1. attēls -----------------------------------------------------------------
hist <- KR[raditajs == "vid", .(gads, vertiba = neapstr)]
L25 <- hist[gads == 2025, vertiba]
V <- VID[joma == "READ" & skala == "oficiāli" & kopa == "visi" & !is.na(vertiba)]
V <- V[, .(gads, kvantile, vertiba)]
V <- rbind(V, data.table(gads = 2025, kvantile = c("P10", "P25", "P50", "P75", "P95"), vertiba = L25))
W <- dcast(V, gads ~ kvantile, value.var = "vertiba")
# piemēru valstis: faktiskās 6 līdz 7 gadu pārmaiņas no līdzīga līmeņa, pārbīdītas uz 2025 sākumu
piem <- data.table(valsts = c("Polija", "Spānija", "Čehija", "Slovākija", "Itālija", "Islande", "Nīderlande"),
                   valsts_en = c("Poland", "Spain", "Czechia", "Slovak Republic", "Italy", "Iceland", "Netherlands"),
                   no = c(2000, 2006, 2009, 2012, 2018, 2018, 2018), uz = c(2006, 2012, 2015, 2018, 2025, 2025, 2025))
P <- merge(piem, PA[joma == "READ", .(valsts_en = valsts, no, uz, l_no, l_uz, parmaina, gadi)], by = c("valsts_en", "no", "uz"))
P[, `:=`(x0 = 2025, x1 = 2025 + gadi, y0 = L25, y1 = L25 + parmaina)]
P[, etikete := sprintf("%s %d līdz %d (%+.0f)", valsts, no, uz, parmaina)]
fwrite(P[, .(valsts, no, uz, limenis_no = round(l_no), limenis_uz = round(l_uz), parmaina = round(parmaina, 1), gadi)], "output/pisa2025/merku_piemeru_valstis.csv")
p1 <- ggplot() +
  geom_ribbon(data = W, aes(x = gads, ymin = P10, ymax = P95), fill = "#9DB4D6", alpha = 0.35) +
  geom_ribbon(data = W, aes(x = gads, ymin = P25, ymax = P75), fill = "#5B85C4", alpha = 0.45) +
  geom_line(data = W, aes(x = gads, y = P50), colour = "#1F4E9C", linetype = "dashed", linewidth = 0.7) +
  geom_line(data = hist, aes(x = gads, y = vertiba), colour = "#B3261E", linewidth = 1.2) +
  geom_point(data = hist, aes(x = gads, y = vertiba), colour = "#B3261E", size = 2.6) +
  geom_text(data = hist, aes(x = gads, y = vertiba, label = round(vertiba)), vjust = -1, size = 3, colour = "#B3261E") +
  geom_segment(data = P, aes(x = x0, xend = x1, y = y0, yend = y1), colour = "#333333", linewidth = 0.5, linetype = "dotted") +
  geom_point(data = P, aes(x = x1, y = y1), colour = "#333333", size = 1.8) +
  geom_text(data = P, aes(x = x1 + 0.15, y = y1, label = etikete, vjust = ifelse(valsts %in% c("Slovākija"), 1.4, ifelse(valsts == "Itālija", -0.6, 0.4))), hjust = 0, size = 2.8, colour = "#333333") +
  annotate("text", x = 2035.2, y = W[gads == 2035, P75], label = "mērķis (labākā ceturtdaļa)", hjust = 0, size = 3, colour = "#1F4E9C") +
  annotate("text", x = 2035.2, y = W[gads == 2035, P50], label = "tipiska valsts", hjust = 0, size = 3, colour = "#1F4E9C") +
  annotate("text", x = 2035.2, y = W[gads == 2035, P25], label = "grīda (sliktākā ceturtdaļa)", hjust = 0, size = 3, colour = "#1F4E9C") +
  annotate("text", x = 2035.2, y = W[gads == 2035, P95], label = "vēsturiskais maksimums", hjust = 0, size = 3, colour = "#7A8FB0") +
  annotate("text", x = 2035.2, y = W[gads == 2035, P10], label = "ļoti slikts gadījums", hjust = 0, size = 3, colour = "#7A8FB0") +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2018, 2022, 2025, 2029, 2032, 2035), limits = c(2003, 2041)) +
  labs(title = "Lasīšana: kur Latvija ir bijusi un kur vēsture ļauj nonākt līdz 2035. gadam",
       subtitle = "Sarkanā: latviešu valodas plūsmas vidējais. Zilais koridors: kas notika ar valstīm, kas kādā ciklā bija Latvijas līmenī (± 30 punkti), 4, 7 un 10 gadu laikā.\nTumšā josla: vidējā puse šo valstu (grīda līdz mērķis). Gaišā: 90 % gadījumu. Punktētās līnijas: konkrētu valstu faktiskās pārmaiņas, pārbīdītas uz Latvijas 2025. gada līmeni.",
       x = NULL, y = "PISA lasīšanas punkti") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 9))
png("output/pisa2025/plots/84b_koridors_valstis.png", width = 2600, height = 1500, res = 200); print(p1); dev.off()
cat("Saglabāts: plots/84b_koridors_valstis.png\n"); print(P[, .(valsts, no, uz, round(l_no), round(l_uz), round(parmaina, 1))])

# --- 2. attēls -----------------------------------------------------------------
JN <- c(READ = "lasīšana", MATH = "matemātika", SCIE = "dabaszinātnes")
H2 <- AL[grupa == "A visi" & skala == "oficiāli" & raditajs == "zem_L2", .(joma_nos = JN[joma], gads, vertiba)]
S2 <- IZM[skala == "oficiāli" & grepl("zem 2", mala_nos), .(joma_nos, gads, P10 = `Ļoti slikts`, P25 = `Slikts gadījums`, P50 = `Tipiska valsts`, P75 = `Labākā ceturtdaļa`, P95 = `Vēsturiskais maksimums`)]
S2 <- rbind(S2, IZM[skala == "oficiāli" & grepl("zem 2", mala_nos) & gads == 2029, .(joma_nos, gads = 2025, P10 = dala_2025, P25 = dala_2025, P50 = dala_2025, P75 = dala_2025, P95 = dala_2025)])
S2[, joma_nos := factor(joma_nos, levels = JN)]; H2[, joma_nos := factor(joma_nos, levels = JN)]
ET <- S2[gads == 2035]
p2 <- ggplot() +
  geom_ribbon(data = S2, aes(x = gads, ymin = P95, ymax = P10), fill = "#9DB4D6", alpha = 0.35) +
  geom_ribbon(data = S2, aes(x = gads, ymin = P75, ymax = P25), fill = "#5B85C4", alpha = 0.45) +
  geom_line(data = S2, aes(x = gads, y = P50), colour = "#1F4E9C", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = 15, colour = "#B3261E", linewidth = 0.5) +
  annotate("text", x = 2003.5, y = 15, label = "ES mērķis 15 %", vjust = -0.5, hjust = 0, size = 3, colour = "#B3261E") +
  geom_line(data = H2, aes(x = gads, y = vertiba), colour = "#222222", linewidth = 1.1) +
  geom_point(data = H2, aes(x = gads, y = vertiba), colour = "#222222", size = 2.2) +
  geom_text(data = ET, aes(x = 2035.3, y = P75, label = "mērķis"), hjust = 0, size = 2.8, colour = "#1F4E9C") +
  geom_text(data = ET, aes(x = 2035.3, y = P50, label = "tipiski"), hjust = 0, size = 2.8, colour = "#1F4E9C") +
  geom_text(data = ET, aes(x = 2035.3, y = P25, label = "grīda"), hjust = 0, size = 2.8, colour = "#1F4E9C") +
  facet_wrap(~ joma_nos, nrow = 1) +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2022, 2029, 2035), limits = c(2003, 2038)) +
  labs(title = "Skolēnu daļa zem 2. līmeņa: vēsture un iespēju koridors līdz 2035. gadam (latviešu valodas plūsma)",
       subtitle = "Melnā: faktiskā daļa. Tumšā josla: no grīdas (sliktākā ceturtdaļa valstu) līdz mērķim (labākā ceturtdaļa); raustītā: tipiska valsts; gaišā josla: 90 % gadījumu.",
       x = NULL, y = "% skolēnu zem 2. līmeņa") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.subtitle = element_text(size = 9))
png("output/pisa2025/plots/84b_zem_L2_koridors.png", width = 2600, height = 1200, res = 200); print(p2); dev.off()
cat("Saglabāts: plots/84b_zem_L2_koridors.png\n")
