# ============================================================================
# 83b. Anketu rādītāju pārmaiņu robežjosla visās PISA valstīs 2015–2025
#      (docs/46 Slānis 15, B daļa; H24; docs/84)
# ============================================================================
#
# KĀPĒC. Latvijas anketu rādītāju pārmaiņas, kas pārsniedz rādītāja vēsturisko
# ātruma robežu visās valstīs (piemēram, SC034Q02 1 % → 53 %), ir instrumenta
# artefakta kandidāti, ne populācijas pārmaiņas. Robežjoslu veido visu valstu
# pārmaiņas neapstrādātos anketu vienumos (ne OECD indeksos, kas katrā ciklā
# centrēti uz 0).
#
# Ievade:
#   output/pisa2025/atlants_mainigie.csv (82. skripts): vienumu atlase
#   pilnie PUF faili visām valstīm (skolēnu un skolu anketas):
#     2015 data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav, CY6_MS_CMB_SCH_QQQ.sav
#     2018 data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav, SCH/CY07_MSU_SCH_QQQ.sav
#     2022 data/pisa2022/raw/CY08MSP_STU_QQQ.SAV, CY08MSP_SCH_QQQ.SAV
#     2025 ~/pisa2025_sav/CY09_MS_STU_PUF.sav, CY09_MS_SCH_PUF.sav
#
# Izvade:
#   output/pisa2025/aploksne_anketas_visi.csv       visas rindas (vienums × pāris × mērs)
#   output/pisa2025/aploksne_anketas_kandidati.csv  Latvijas pārmaiņas ārpus P1/P99 un P5/P95
#   output/pisa2025/aploksne_anketas_limeni.csv     valsts × cikls × vienums līmeņi (kešs)
#   output/pisa2025/plots/83_anketas_kandidati.png
#   output/pisa2025/logs/83b_anketu_aploksne_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(haven); library(ggplot2) })
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/83b_anketu_aploksne_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
t_sakums <- Sys.time()

S25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))
FAILI <- list(
  "2015" = c(STU = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav", SCH = "data/pisa2015/raw/CY6_MS_CMB_SCH_QQQ.sav"),
  "2018" = c(STU = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav", SCH = "data/pisa2018/raw/SCH/CY07_MSU_SCH_QQQ.sav"),
  "2022" = c(STU = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV", SCH = "data/pisa2022/raw/CY08MSP_SCH_QQQ.SAV"),
  "2025" = c(STU = file.path(S25, "CY09_MS_STU_PUF.sav"), SCH = file.path(S25, "CY09_MS_SCH_PUF.sav")))
SVARI <- list(STU = c("2015" = "W_FSTUWT", "2018" = "W_FSTUWT", "2022" = "W_FSTUWT", "2025" = "W_FSTUWT"),
              SCH = c("2015" = "W_SCHGRNRABWT", "2018" = "W_SCHGRNRABWT", "2022" = "W_SCHGRNRABWT", "2025" = "W_NRASCHBWT"))
MIN_VALSTIS <- 30
KESS <- "output/pisa2025/aploksne_anketas_limeni.csv"

# === 1. Vienumu atlase no atlanta =============================================
cat(strrep("=", 70), "\n1. Vienumu atlase no atlanta\n", strrep("=", 70), "\n\n", sep = "")
A <- fread("output/pisa2025/atlants_mainigie.csv")
K <- A[gads %in% c(2015, 2018, 2022, 2025) & fails %in% c("STU", "SCH") &
       tips %in% c("kategoriāls", "atvasināts kategoriāls") & vertibu_etikesu_skaits >= 2]
V <- K[, .(n_cikli = uniqueN(gads), n_etik = uniqueN(vertibu_etikesu_skaits),
           k = as.integer(vertibu_etikesu_skaits[1]), cikli = paste(sort(unique(gads)), collapse = " "),
           etikete = etikete[which.max(gads)], vertibu_etiketes = vertibu_etiketes[which.max(gads)],
           konstrukts = konstrukts[which.max(gads)], tips = tips[which.max(gads)]),
       by = .(fails, mainigais)][n_cikli >= 3 & n_etik == 1]
setorder(V, fails, mainigais)
cat("Atlasīti vienumi (≥ 3 no 4 cikliem, vienāds vērtību etiķešu skaits):", nrow(V), "\n")
print(table(V$fails, V$cikli))
cat("Vērtību etiķešu skaits:\n"); print(table(V$k))
cat("IKT vienumi (IC...):", sum(grepl("^IC", V$mainigais)), "; Latvija 2025 IKT anketu neadministrēja\n\n")

# === 2. Līmeņi: valsts × cikls × vienums (svērtais vidējais un augšējās kategorijas daļa)
cat(strrep("=", 70), "\n2. Līmeņu aprēķins no PUF failiem\n", strrep("=", 70), "\n\n", sep = "")

limeni_no_faila <- function(gads, fl) {
  f <- FAILI[[gads]][[fl]]; sv <- SVARI[[fl]][[gads]]
  vien <- V[fails == fl & grepl(gads, cikli, fixed = TRUE), mainigais]
  galva <- names(read_sav(f, n_max = 1))
  vien <- intersect(vien, galva)
  if (!sv %in% galva) { cat("  BRĪDINĀJUMS: svara", sv, "nav failā", f, ", lieto vienādus svarus\n"); sv <- NULL }
  cat(sprintf("  %s %s: %d vienumi, fails %s\n", gads, fl, length(vien), basename(f)))
  t0 <- Sys.time()
  kol <- c("CNT", sv, vien); X <- read_sav(f, col_select = tidyselect::all_of(kol))
  cat(sprintf("    nolasīts: %d rindas, %.1f min\n", nrow(X), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cnt <- as.character(X$CNT)
  w <- if (is.null(sv)) rep(1, nrow(X)) else as.numeric(X[[sv]])
  w[is.na(w)] <- 0
  rez <- vector("list", length(vien))
  for (i in seq_along(vien)) {
    v <- vien[i]; x <- X[[v]]
    lab <- attr(x, "labels")
    # derīgās kategorijas: etiķetētās vērtības, kas datos reāli sastopamas (trūkstošo kodus 7/8/9, 97/98/99 u.c.
    # haven jau pārvērš par NA, bet to etiķetes paliek; tāpēc ņem tikai sastopamās)
    xn <- as.numeric(x); sast <- sort(unique(xn[!is.na(xn)]))
    der <- if (!is.null(lab)) sort(unname(lab[!is.na(lab) & lab %in% sast])) else sast
    ok <- !is.na(xn) & xn %in% der & w > 0
    if (!any(ok)) next
    poz <- match(xn[ok], der)                    # 1..k
    D <- data.table(cnt = cnt[ok], w = w[ok], poz = poz, augs = as.integer(poz == length(der)))
    r <- D[, .(n = .N, videjais = sum(w * poz) / sum(w), augs_dala = sum(w * augs) / sum(w)), by = cnt]
    r[, `:=`(gads = as.integer(gads), fails = fl, mainigais = v, k_fails = length(der))]
    rez[[i]] <- r
  }
  rm(X); gc(verbose = FALSE)
  rbindlist(rez)
}

if (file.exists(KESS)) {
  cat("Lieto kešu:", KESS, "\n")
  L <- fread(KESS)
} else {
  cat("UZMANĪBU: četru skolēnu PUF failu (1,5 līdz 2,2 GB katrs) lasīšana var aizņemt 10 līdz 20 minūtes.\n")
  L <- rbindlist(lapply(names(FAILI), function(g) rbindlist(lapply(c("SCH", "STU"), function(fl) limeni_no_faila(g, fl)))))
  fwrite(L, KESS)
  cat("Saglabāts kešs:", KESS, "\n")
}
L <- L[n >= 30]   # valsts × vienums ar vismaz 30 derīgām atbildēm
cat("\nLīmeņu rindas (valsts × cikls × vienums, n ≥ 30):", nrow(L), "\n")
cat("Valstis pa cikliem:\n"); print(L[, .(valstis = uniqueN(cnt)), by = .(gads, fails)][order(fails, gads)])
lva_ic <- L[cnt == "LVA" & gads == 2025 & grepl("^IC", mainigais)]
cat("Latvija 2025 IKT vienumi ar datiem:", nrow(lva_ic), "(gaidāms 0, jo IKT anketa netika administrēta)\n")
nesakrit <- merge(unique(L[, .(fails, mainigais, k_fails)]), V[, .(fails, mainigais, k)], by = c("fails", "mainigais"))[k_fails != k]
if (nrow(nesakrit)) { cat("Vienumi, kur failā derīgo kategoriju skaits atšķiras no atlanta:\n"); print(unique(nesakrit)) }

# === 3. Pārmaiņas pa ciklu pāriem un robežjosla =================================
cat("\n", strrep("=", 70), "\n3. Pārmaiņas pa ciklu pāriem un robežjosla\n", strrep("=", 70), "\n\n", sep = "")
Lm <- melt(L, id.vars = c("cnt", "gads", "fails", "mainigais"), measure.vars = c("videjais", "augs_dala"),
           variable.name = "mers", value.name = "vertiba")
PARI <- data.table(no = c(2015, 2018, 2022, 2015, 2018, 2015), uz = c(2018, 2022, 2025, 2022, 2025, 2025))
PR <- rbindlist(lapply(seq_len(nrow(PARI)), function(i) {
  a <- Lm[gads == PARI$no[i]]; b <- Lm[gads == PARI$uz[i]]
  m <- merge(a[, .(cnt, fails, mainigais, mers, v_no = vertiba)], b[, .(cnt, fails, mainigais, mers, v_uz = vertiba)],
             by = c("cnt", "fails", "mainigais", "mers"))
  m[, `:=`(no = PARI$no[i], uz = PARI$uz[i], parmaina = v_uz - v_no)]
  m
}))
cat("Pārmaiņu rindas (valsts × vienums × pāris × mērs):", nrow(PR), "\n")

kv <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE, type = 7))
AP <- PR[cnt != "LVA", .(n_valstis = .N,
                          P1 = kv(parmaina, .01), P5 = kv(parmaina, .05), P50 = kv(parmaina, .5),
                          P95 = kv(parmaina, .95), P99 = kv(parmaina, .99),
                          citu_min = min(parmaina), citu_max = max(parmaina)),
          by = .(fails, mainigais, mers, no, uz)][n_valstis >= MIN_VALSTIS]
LV <- PR[cnt == "LVA", .(fails, mainigais, mers, no, uz, lva_no = v_no, lva_uz = v_uz, lva_parmaina = parmaina)]
AP <- merge(AP, LV, by = c("fails", "mainigais", "mers", "no", "uz"), all.x = TRUE)
# Latvijas procentile citu valstu pārmaiņu sadalījumā
PRc <- PR[cnt != "LVA"]
AP[, lva_procentile := {
  d <- PRc[fails == .BY$fails & mainigais == .BY$mainigais & mers == .BY$mers & no == .BY$no & uz == .BY$uz, parmaina]
  if (is.na(lva_parmaina)) NA_real_ else 100 * mean(d < lva_parmaina) + 50 * mean(d == lva_parmaina)
}, by = .(fails, mainigais, mers, no, uz)]
AP[, `:=`(arpus_P1_P99 = !is.na(lva_parmaina) & (lva_parmaina < P1 | lva_parmaina > P99),
          arpus_P5_P95 = !is.na(lva_parmaina) & (lva_parmaina < P5 | lva_parmaina > P95),
          lva_dati = !is.na(lva_parmaina))]
AP <- merge(AP, V[, .(fails, mainigais, k, etikete, vertibu_etiketes, konstrukts)], by = c("fails", "mainigais"))
AP[, mers_nos := ifelse(mers == "videjais", "vidējais (1..k)", "augšējās kategorijas daļa")]
setorder(AP, fails, mainigais, mers, no, uz)
setcolorder(AP, c("fails", "mainigais", "etikete", "k", "mers", "mers_nos", "no", "uz", "n_valstis",
                  "lva_no", "lva_uz", "lva_parmaina", "lva_procentile", "P1", "P5", "P50", "P95", "P99",
                  "citu_min", "citu_max", "arpus_P1_P99", "arpus_P5_P95", "lva_dati", "konstrukts", "vertibu_etiketes"))
fwrite(AP, "output/pisa2025/aploksne_anketas_visi.csv")
cat("Saglabāts: output/pisa2025/aploksne_anketas_visi.csv (", nrow(AP), " rindas)\n", sep = "")
cat("Vienumi × pāri (ar ≥ 30 valstīm):", nrow(unique(AP[, .(fails, mainigais, no, uz)])), "; no tiem ar Latvijas datiem:",
    nrow(unique(AP[lva_dati == TRUE, .(fails, mainigais, no, uz)])), "\n")
cat("Vienumi × pāri bez Latvijas datiem (IKT 2025 u.c.):\n"); print(AP[lva_dati == FALSE & mers == "videjais", .N, by = .(fails, no, uz)])

# === 4. Kandidāti ============================================================
cat("\n", strrep("=", 70), "\n4. Artefaktu kandidāti: Latvijas pārmaiņas ārpus robežjoslas\n", strrep("=", 70), "\n\n", sep = "")
KAND <- AP[lva_dati == TRUE & arpus_P5_P95 == TRUE]
KAND[, limenis := ifelse(arpus_P1_P99, "ārpus P1/P99", "ārpus P5/P95")]
setorder(KAND, -arpus_P1_P99, fails, mainigais, no, uz, mers)
fwrite(KAND, "output/pisa2025/aploksne_anketas_kandidati.csv")
cat("Saglabāts: output/pisa2025/aploksne_anketas_kandidati.csv\n")
n_test <- AP[lva_dati == TRUE, .N]
cat(sprintf("Pārbaudītas Latvijas pārmaiņas (vienums × pāris × mērs): %d\n", n_test))
cat(sprintf("Ārpus P5/P95: %d (%.1f %%; gaidāms nejauši ap 10 %%)\n", nrow(KAND), 100 * nrow(KAND) / n_test))
cat(sprintf("Ārpus P1/P99: %d (%.1f %%; gaidāms nejauši ap 2 %%)\n", sum(KAND$arpus_P1_P99), 100 * sum(KAND$arpus_P1_P99) / n_test))
cat("\nPa pāriem (ārpus P1/P99, vidējais):\n"); print(KAND[arpus_P1_P99 == TRUE & mers == "videjais", .N, by = .(fails, no, uz)][order(fails, no, uz)])
cat("\nUnikāli vienumi ārpus P1/P99 (jebkurš pāris, jebkurš mērs):", uniqueN(KAND[arpus_P1_P99 == TRUE, paste(fails, mainigais)]), "\n")
cat("Unikāli vienumi ārpus P5/P95:", uniqueN(KAND[, paste(fails, mainigais)]), "\n")

cat("\nKandidāti ārpus P1/P99 (vidējais 1..k), pēc pāra:\n")
tb <- KAND[arpus_P1_P99 == TRUE & mers == "videjais",
           .(fails, mainigais, pāris = paste(no, "→", uz), lva_no = round(lva_no, 2), lva_uz = round(lva_uz, 2),
             lva_parmaina = round(lva_parmaina, 2), P1 = round(P1, 2), P99 = round(P99, 2), procentile = round(lva_procentile, 1),
             etikete = substr(etikete, 1, 70))]
print(tb, nrows = 500)

cat("\nSC034Q02NA (nemandātie standartizētie testi) visos pāros un mēros:\n")
print(AP[mainigais == "SC034Q02NA", .(mers, no, uz, n_valstis, lva_no = round(lva_no, 3), lva_uz = round(lva_uz, 3),
                                       lva_parmaina = round(lva_parmaina, 3), P1 = round(P1, 3), P99 = round(P99, 3),
                                       lva_procentile = round(lva_procentile, 1), arpus_P1_P99)])
sc <- KAND[mainigais == "SC034Q02NA" & no == 2022 & uz == 2025]
cat("\nSC034Q02NA 2022 → 2025 kandidātos:", if (nrow(sc)) paste(unique(sc$limenis), collapse = "; ") else "NĒ", "\n")

# === 5. Grafiks ==============================================================
cat("\n", strrep("=", 70), "\n5. Grafiks\n", strrep("=", 70), "\n\n", sep = "")
H <- AP[lva_dati == TRUE & mers == "videjais"]
H[, paris := paste(no, "→", uz)]
H[, paris := factor(paris, levels = paste(PARI$no, "→", PARI$uz))]
p1 <- ggplot(H, aes(x = lva_procentile)) +
  geom_histogram(breaks = seq(0, 100, 5), fill = "#8A8F98", colour = "white", linewidth = 0.3) +
  geom_vline(xintercept = c(5, 95), linetype = "dashed", colour = "#B3261E", linewidth = 0.5) +
  facet_wrap(~ paris, ncol = 3) +
  labs(title = "Latvijas anketu vienumu pārmaiņu procentile visu valstu sadalījumā",
       subtitle = sprintf("Vidējais atbildes kods (1..k); %d vienumi ar vismaz 3 cikliem; svītras: P5 un P95", uniqueN(H$mainigais)),
       x = "Latvijas pārmaiņas procentile (0 = lielākais kritums, 100 = lielākais kāpums)", y = "Vienumu skaits") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
KK <- KAND[arpus_P1_P99 == TRUE & mers == "videjais"]
KK <- KK[order(lva_procentile)]
KK[, nos := paste0(mainigais, " ", no, "→", uz, ": ", substr(etikete, 1, 45))]
KK[, nos := factor(nos, levels = unique(nos))]
p2 <- ggplot(KK, aes(x = nos, y = lva_parmaina)) +
  geom_col(aes(fill = lva_parmaina > 0), width = 0.7) +
  geom_errorbar(aes(ymin = P1, ymax = P99), width = 0.3, colour = "#444444", linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = "#1F5FBF", `FALSE` = "#B3261E"), guide = "none") +
  coord_flip() +
  labs(title = "Kandidāti: Latvijas pārmaiņa ārpus P1 un P99 (svītra: robežjoslas P1 līdz P99)",
       x = NULL, y = "Pārmaiņa vidējā atbildes kodā (1..k)") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())
png("output/pisa2025/plots/83_anketas_kandidati.png", width = 2600, height = 1400 + 40 * max(nrow(KK), 10), res = 200)
gridExtra_ok <- requireNamespace("gridExtra", quietly = TRUE)
if (gridExtra_ok) gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1, 0.6 + 0.04 * nrow(KK))) else { print(p1) }
dev.off()
if (!gridExtra_ok) { png("output/pisa2025/plots/83_anketas_kandidati_saraksts.png", width = 2600, height = 800 + 40 * nrow(KK), res = 200); print(p2); dev.off() }
cat("Saglabāts: output/pisa2025/plots/83_anketas_kandidati.png\n")

cat("\nIzveidotie faili: aploksne_anketas_visi.csv, aploksne_anketas_kandidati.csv, aploksne_anketas_limeni.csv, plots/83_anketas_kandidati.png\n")
cat("Laiks:", round(as.numeric(difftime(Sys.time(), t_sakums, units = "mins")), 1), "min\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
