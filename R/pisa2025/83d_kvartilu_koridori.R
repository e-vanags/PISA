# ============================================================================
# 83d. Kvartiļu koridori un Latvijas vieta tajos (divi skati)
#
# A. Līmeņa koridors: visu PISA valstu lasīšanas vidējo sadalījums katrā ciklā
#    (P10 līdz P90 gaišs, P25 līdz P75 tumšs, mediāna), un Latvijas rinda tajā.
#    Atbild: kur Latvija atrodas starp valstīm un kā tas mainās.
# B. Dinamikas koridors: kur nonāktu valsts, kas 2003. gadā sāk Latvijas līmenī
#    un tālāk katrā periodā mainās tā, kā tajā pašā periodā mainījās līdzīga
#    līmeņa valstis (5000 simulētas trajektorijas, izlozējot reālas pārmaiņas).
#    Atbild: vai Latvijas ceļš ir parasts, ņemot vērā, ko darīja visa pasaule.
#
# Ievade: aploksne_pari.csv (83), korigetie_raditaji.csv (72), B1 tabula .37 (līmeņi)
# Izvade: plots/83_kvartilu_koridori.png, aploksne_kvartilu_koridori.csv
# ============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(readxl) })
setwd(Sys.getenv("PISA_SAKNE", unset = "/Users/e_vanags/Documents/PISA datu analīze 2025"))
set.seed(83)

PA <- fread("output/pisa2025/aploksne_pari.csv", encoding = "UTF-8")
R1 <- PA[joma == "READ" & logs == 1]
KR <- fread("output/pisa2025/korigetie_raditaji.csv", encoding = "UTF-8"); setnames(KR, names(KR)[1], "raditajs")
CIKLI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)

# --- līmeņi visām valstīm no B1 tabulas .37 --------------------------------
x <- suppressMessages(as.data.frame(read_excel("data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx", sheet = "Table I.B1.2a.37", col_names = FALSE)))
val <- x[10:nrow(x), ]; val <- val[!is.na(val[[1]]) & !grepl("^OECD", val[[1]]), ]
LIM <- rbindlist(lapply(seq_along(CIKLI), function(i) data.table(valsts = val[[1]], gads = CIKLI[i], vid = suppressWarnings(as.numeric(val[[2 * i]])))))
LIM <- LIM[!is.na(vid)]
OECD <- c("Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden","Switzerland","Türkiye","United Kingdom","United States")
LIM[, kopa := fifelse(valsts %in% OECD, "OECD valstis", "citas")]
kv <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
KA <- rbind(
  LIM[valsts != "Latvia", .(kopa = "visas PISA valstis", n = .N, P10 = kv(vid, .1), P25 = kv(vid, .25), P50 = kv(vid, .5), P75 = kv(vid, .75), P90 = kv(vid, .9)), by = gads],
  LIM[valsts != "Latvia" & kopa == "OECD valstis", .(kopa = "OECD valstis", n = .N, P10 = kv(vid, .1), P25 = kv(vid, .25), P50 = kv(vid, .5), P75 = kv(vid, .75), P90 = kv(vid, .9)), by = gads])
LV_visa <- LIM[valsts == "Latvia", .(gads, vertiba = vid, rinda = "Latvija, visa valsts")]
# latviešu valodas plūsmas procentiles (55. skripts) ar to pašu instrumenta nobīdes korekciju kā vidējam (+9,62 no 2018)
AG <- fread("output/pisa2025/agrie_signali_limeni.csv", encoding = "UTF-8"); setnames(AG, "radītājs", "raditajs")
NOB <- KR[raditajs == "P10", .(gads, nobide = korigets - neapstr)]
AP <- merge(AG[grupa == "A visi" & raditajs %in% c("P10", "P25", "P50", "P75", "P90"), .(gads, raditajs, vertiba)], NOB, by = "gads")
AP[, vertiba := vertiba + nobide]
# OECD valstu tās pašas procentiles no tabulas .40 (P10, P25, P50, P75, P90 pa cikliem)
x40 <- suppressMessages(as.data.frame(read_excel("data/pisa_tehniskie/PISA2025_Vol1_AnnexB1_ch2a_performance.xlsx", sheet = "Table I.B1.2a.40", col_names = FALSE)))
v40 <- x40[10:nrow(x40), ]; v40 <- v40[!is.na(v40[[1]]) & !grepl("^OECD", v40[[1]]), ]
PN <- c("P10", "P25", "P50", "P75", "P90")
P40 <- rbindlist(lapply(seq_along(CIKLI), function(i) rbindlist(lapply(seq_along(PN), function(k)
  data.table(valsts = v40[[1]], gads = CIKLI[i], raditajs = PN[k], vid = suppressWarnings(as.numeric(v40[[2 + (i - 1) * 10 + (k - 1) * 2]])))))))
P40 <- P40[!is.na(vid) & valsts %in% OECD & valsts != "Latvia"]
KP <- P40[, .(n = .N, P10 = kv(vid, .1), P25 = kv(vid, .25), P50 = kv(vid, .5), P75 = kv(vid, .75), P90 = kv(vid, .9)), by = .(gads, raditajs)]
AP_pct <- merge(AP, P40[, .(gads, raditajs, valsts, vid)], by = c("gads", "raditajs"), allow.cartesian = TRUE)[, .(pct_oecd = 100 * mean(vid < vertiba)), by = .(gads, raditajs)]
cat("Latviešu valodas plūsmas procentiļu vieta starp OECD valstu tām pašām procentilēm (procentile):\n")
print(dcast(AP_pct, gads ~ raditajs, value.var = "pct_oecd")[, lapply(.SD, round)])
fwrite(merge(AP, AP_pct, by = c("gads", "raditajs")), "output/pisa2025/aploksne_a_plusmas_procentiles_oecd.csv")
LV_A <- KR[raditajs == "vid" & !is.na(korigets), .(gads, vertiba = korigets, rinda = "latviešu valodas plūsma (koriģēti)")]
LV <- rbind(LV_visa, LV_A)
# Latvijas procentile līmeņu sadalījumā
LV_pct <- LV[, .(gads, rinda, pct_visas = sapply(seq_len(.N), function(i) 100 * mean(LIM[valsts != "Latvia" & gads == .SD$gads[i], vid] < vertiba[i])),
                 pct_oecd = sapply(seq_len(.N), function(i) 100 * mean(LIM[valsts != "Latvia" & kopa == "OECD valstis" & gads == .SD$gads[i], vid] < vertiba[i])))]

# --- dinamikas koridors: simulētas trajektorijas ----------------------------
simul <- function(sakums_gads, sakums_vert, N = 5000, platums = 30) {
  cik <- CIKLI[CIKLI >= sakums_gads]; M <- matrix(NA_real_, N, length(cik)); M[, 1] <- sakums_vert
  for (j in 2:length(cik)) {
    pp <- R1[valsts != "Latvia" & no == cik[j - 1] & uz == cik[j]]
    for (i in seq_len(N)) {
      cand <- pp[abs(l_no - M[i, j - 1]) <= platums]
      if (nrow(cand) < 10) cand <- pp[abs(l_no - M[i, j - 1]) <= 2 * platums]
      M[i, j] <- M[i, j - 1] + sample(cand$parmaina, 1)
    }
  }
  data.table(gads = cik, P10 = apply(M, 2, kv, .1), P25 = apply(M, 2, kv, .25), P50 = apply(M, 2, kv, .5), P75 = apply(M, 2, kv, .75), P90 = apply(M, 2, kv, .9))
}
SIM_A <- simul(2003, LV_A[gads == 2003, vertiba]); SIM_A[, rinda := "latviešu valodas plūsma (koriģēti)"]
SIM_V <- simul(2003, LV_visa[gads == 2003, vertiba]); SIM_V[, rinda := "Latvija, visa valsts"]
SIM <- rbind(SIM_V, SIM_A)
# Latvijas procentile simulētajā sadalījumā (aptuveni: no kvantilēm)
fwrite(rbind(KA[, .(skats = "līmeņa koridors", rinda = kopa, gads, n, P10, P25, P50, P75, P90)],
             SIM[, .(skats = "dinamikas koridors (sākums 2003)", rinda, gads, n = 5000L, P10, P25, P50, P75, P90)]), "output/pisa2025/aploksne_kvartilu_koridori.csv")
fwrite(LV_pct, "output/pisa2025/aploksne_latvija_limena_procentiles.csv")
cat("Latvijas līmeņa procentile (visas / OECD):\n"); print(LV_pct[, .(gads, rinda, pct_visas = round(pct_visas), pct_oecd = round(pct_oecd))])
cat("\nDinamikas koridors:\n"); print(SIM[, .(rinda = substr(rinda, 1, 20), gads, P10 = round(P10), P25 = round(P25), P50 = round(P50), P75 = round(P75), P90 = round(P90))])

# --- attēls ------------------------------------------------------------------
AP[, raditajs := factor(raditajs, levels = PN, labels = c("P10 (vājākie)", "P25", "P50 (mediāna)", "P75", "P90 (stiprākie)"))]
KP[, raditajs := factor(raditajs, levels = PN, labels = levels(AP$raditajs))]
pA <- ggplot() +
  geom_ribbon(data = KP, aes(x = gads, ymin = P10, ymax = P90), fill = "#9DB4D6", alpha = 0.4) +
  geom_ribbon(data = KP, aes(x = gads, ymin = P25, ymax = P75), fill = "#5B85C4", alpha = 0.5) +
  geom_line(data = KP, aes(x = gads, y = P50), colour = "#1F4E9C", linetype = "dotted", linewidth = 0.6) +
  geom_line(data = AP, aes(x = gads, y = vertiba), colour = "#E08A00", linewidth = 1.1) +
  geom_point(data = AP, aes(x = gads, y = vertiba), colour = "#E08A00", size = 2.2) +
  facet_wrap(~ raditajs, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(2003, 2009, 2015, 2022)) +
  labs(title = "A. Latviešu valodas plūsmas skolēnu procentiles OECD valstu koridorā",
       subtitle = "Katrā panelī viena skolēnu sadalījuma procentile: oranžā ir latviešu valodas plūsma (koriģēti), zilās joslas ir OECD valstu tās pašas procentiles (tumšā P25 līdz P75, gaišā P10 līdz P90, punktētā mediāna).",
       x = NULL, y = "PISA lasīšanas punkti") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.subtitle = element_text(size = 9.5))
pB <- ggplot() +
  geom_ribbon(data = SIM_A, aes(x = gads, ymin = P10, ymax = P90), fill = "#9DB4D6", alpha = 0.4) +
  geom_ribbon(data = SIM_A, aes(x = gads, ymin = P25, ymax = P75), fill = "#5B85C4", alpha = 0.5) +
  geom_line(data = SIM_A, aes(x = gads, y = P50), colour = "#1F4E9C", linetype = "dotted", linewidth = 0.6) +
  geom_line(data = LV_A, aes(x = gads, y = vertiba), colour = "#E08A00", linewidth = 1.1) +
  geom_point(data = LV_A, aes(x = gads, y = vertiba), colour = "#E08A00", size = 2.6) +
  geom_text(data = LV_A, aes(x = gads, y = vertiba, label = round(vertiba)), vjust = -1, size = 3, colour = "#E08A00") +
  scale_x_continuous(breaks = CIKLI[CIKLI >= 2003]) +
  labs(title = "B. Dinamikas koridors: kur nonāktu valsts, kas 2003. gadā sāk latviešu valodas plūsmas līmenī",
       subtitle = "5000 simulētas trajektorijas: katrā periodā izlozēta reāla pārmaiņa no valstīm, kas tajā pašā periodā bija līdzīgā līmenī (± 30 punkti).\nTumšā josla: vidējā puse trajektoriju (P25 līdz P75); gaišā: P10 līdz P90; punktētā: mediāna. Oranžā: latviešu valodas plūsmas vidējais koriģēti.",
       x = NULL, y = "PISA lasīšanas punkti") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 9.5))
png("output/pisa2025/plots/83_kvartilu_koridori.png", width = 2600, height = 2200, res = 200)
if (requireNamespace("gridExtra", quietly = TRUE)) gridExtra::grid.arrange(pA, pB, ncol = 1, heights = c(1, 1.1)) else { print(pA); print(pB) }
dev.off()
cat("Saglabāts: output/pisa2025/plots/83_kvartilu_koridori.png\n")
