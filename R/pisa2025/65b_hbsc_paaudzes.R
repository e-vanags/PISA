# ============================================================================
# 65b. Ārējie simptomi: HBSC paaudžu nobīde pret PISA lasītprasmes pārmaiņām
# ============================================================================
#
# JAUTĀJUMS: vai HBSC rādītāju pārmaiņa tai pašai dzimšanas paaudzei agrākā vecumā
# (11 vai 13 gadi) paredz, kā mainīsies valsts PISA lasītprasme 15 gados?
#
# DATI: HBSC Data Browser (65a): 2014, 2018, 2022; 11, 13, 15 gadi; 8 valstis; visa valsts
#   (bez valodas plūsmām). PISA: valstu vidējie (output/pisa2025/pisa_valstu_videjie.csv).
# RELATĪVI: katras valsts vērtība mīnus pārējo 7 valstu vidējais (tajā pašā vilnī un
#   vecumā; PISA tajā pašā ciklā). Tā tiek izņemtas kopīgās pārmaiņas, piemēram,
#   pandēmija un vispārējs kritums 2025. gadā.
# NOTIKUMI (paaudze = dzimšanas gads; HBSC aptauja pavasarī, 11 g. ≈ dzimuši W − 11,5):
#   ex ante 2018 → 2022: 11 g. 2014 → 11 g. 2018 (paaudzes ~2002 → ~2006 = PISA 2018 → 2022)
#   ex ante 2022 → 2025: 11 g. 2018 → 13 g. 2022 (paaudzes ~2006 → ~2009 = PISA 2022 → 2025)
#   ex ante 2022 → 2025, vienāds vecums: 13 g. 2018 → 13 g. 2022
#   vienlaicīgi (ne prognoze): 15 g. 2014 → 2018 ↔ PISA 2015 → 2018; 15 g. 2018 → 2022 ↔ PISA 2018 → 2022
# ANALĪZE: katram rādītājam korelācija starp HBSC relatīvo pārmaiņu un PISA relatīvo
#   pārmaiņu (8 valstis × 2 notikumi = 16 punkti), permutāciju p (valstis sajauktas
#   notikuma ietvaros, 10 000 reizes), Benjamini–Hochberg. Iepriekš izvēlēts saliktais
#   "skolas vides un labbūtības riska" rādītājs (docs/61 HBSC pirmais skats).
#
# Izvade: output/pisa2025/hbsc_paaudzes_korelacijas.csv, hbsc_paaudzes_dati.csv
# ============================================================================

suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/65b_hbsc_paaudzes_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)

VAL <- c("LVA", "CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
H <- fread("data/hbsc/hbsc_simptomi_8valstis.csv", encoding = "UTF-8")
setnames(H, c("cnt", "gads", "vecums", "id", "raditajs", "tips", "meitenes", "zeni", "kopa"))
H <- H[cnt %in% VAL & !is.na(kopa)][, gads := as.character(gads)]
H[, rel := kopa - (sum(kopa) - kopa) / (.N - 1), by = .(id, gads, vecums)]
NOS <- unique(H[, .(id, raditajs)])

P <- fread("output/pisa2025/pisa_valstu_videjie.csv", colClasses = c(gads = "character"))[cnt %in% VAL, .(gads, cnt, y = lasisana)]
P[, rel := y - (sum(y) - y) / (.N - 1), by = gads]
dP <- function(g1, g2) merge(P[gads == g1, .(cnt, r1 = rel, y1 = y)], P[gads == g2, .(cnt, r2 = rel, y2 = y)], by = "cnt")[
  , .(cnt, pisa_d = y2 - y1, pisa_drel = r2 - r1)]

NOT <- list(
  list(nos = "2018→2022 ex ante: 11 g. 2014→2018", kopa = "ex ante (paaudze)", p = c("2018", "2022"), h1 = c("2014", 11), h2 = c("2018", 11)),
  list(nos = "2022→2025 ex ante: 11 g. 2018 → 13 g. 2022", kopa = "ex ante (paaudze)", p = c("2022", "2025"), h1 = c("2018", 11), h2 = c("2022", 13)),
  list(nos = "2022→2025 ex ante: 13 g. 2018→2022", kopa = "ex ante (vienāds vecums)", p = c("2022", "2025"), h1 = c("2018", 13), h2 = c("2022", 13)),
  list(nos = "2015→2018 vienlaicīgi: 15 g. 2014→2018", kopa = "vienlaicīgi", p = c("2015", "2018"), h1 = c("2014", 15), h2 = c("2018", 15)),
  list(nos = "2018→2022 vienlaicīgi: 15 g. 2018→2022", kopa = "vienlaicīgi", p = c("2018", "2022"), h1 = c("2018", 15), h2 = c("2022", 15)))
O <- rbindlist(lapply(NOT, function(e) {
  a <- H[gads == e$h1[1] & vecums == as.integer(e$h1[2]), .(id, cnt, r1 = rel)]
  b <- H[gads == e$h2[1] & vecums == as.integer(e$h2[2]), .(id, cnt, r2 = rel)]
  merge(merge(a, b, by = c("id", "cnt"))[, hbsc_drel := r2 - r1], dP(e$p[1], e$p[2]), by = "cnt")[, `:=`(notikums = e$nos, kopa = e$kopa)]
}))
# ex ante (vienāds vecums) pārī ar 2018→2022 ex ante
O <- rbind(O, O[notikums == NOT[[1]]$nos][, kopa := "ex ante (vienāds vecums)"])
fwrite(O, "output/pisa2025/hbsc_paaudzes_dati.csv")

cat("PISA relatīvā pārmaiņa (valsts − pārējo 7 vidējais):\n")
print(dcast(unique(O[, .(notikums, cnt, pisa_drel = round(pisa_drel, 1))]), cnt ~ notikums, value.var = "pisa_drel"), row.names = FALSE)

perm_kor <- function(x, y, grp, B = 10000) {
  r0 <- cor(x, y)
  rb <- replicate(B, { yy <- y; for (g in unique(grp)) { i <- which(grp == g); yy[i] <- y[i][sample(length(i))] }; cor(x, yy) })
  c(r = r0, p = mean(abs(rb) >= abs(r0) - 1e-12))
}
KOR <- O[, {
  if (.N < 10 || sd(hbsc_drel) == 0) NULL else {
    pk <- perm_kor(hbsc_drel, pisa_drel, notikums)
    .(n = .N, r = pk[["r"]], rho = cor(hbsc_drel, pisa_drel, method = "spearman"), p = pk[["p"]])
  }
}, by = .(kopa, id)]
KOR[, q := p.adjust(p, "BH"), by = kopa]
KOR <- merge(KOR, NOS, by = "id")
fwrite(KOR, "output/pisa2025/hbsc_paaudzes_korelacijas.csv")
for (k in unique(KOR$kopa)) {
  x <- KOR[kopa == k]
  cat(sprintf("\n%s\n%s: %d rādītāji, p < 0,05: %d, BH q < 0,10: %d\n%s\n", strrep("=", 100), k, nrow(x), sum(x$p < 0.05), sum(x$q < 0.10), strrep("=", 100)))
  print(head(x[order(p), .(id, r = round(r, 2), rho = round(rho, 2), n, p = signif(p, 2), q = signif(q, 2), raditajs = substr(raditajs, 1, 55))], 12), row.names = FALSE)
}

# Iepriekš izvēlētais saliktais riska rādītājs (+1: pieaugums = sliktāk)
KEY <- c(sch_pressure = 1, sch_satis = -1, teachersup_cutoff = -1, studsup_cutoff = -1, d_sleep = 1, lifesat_mean = -1,
         beenbullied_2_3 = 1, peersup_hi = -1, famsup_hi = -1, multihealth = 1, d_feellow = 1, d_nervous = 1)
S <- O[id %in% names(KEY)][, z := KEY[id] * hbsc_drel / sd(hbsc_drel), by = .(kopa, notikums, id)]
S <- S[, .(risks = mean(z), rad = .N, pisa_drel = pisa_drel[1]), by = .(kopa, notikums, cnt)]
cat(sprintf("\n%s\nSaliktais riska rādītājs (%d rādītāji; standartizēta relatīvā pārmaiņa, + = sliktāk)\n%s\n", strrep("=", 100), length(KEY), strrep("=", 100)))
for (k in unique(S$kopa)) {
  x <- S[kopa == k]; pk <- perm_kor(x$risks, x$pisa_drel, x$notikums)
  cat(sprintf("  %s: r = %.2f, permutāciju p = %.3f (n = %d)\n", k, pk[["r"]], pk[["p"]], nrow(x)))
}
print(dcast(S, cnt ~ notikums, value.var = c("risks", "pisa_drel"), fun.aggregate = function(x) round(x, 2))[, 1:6], row.names = FALSE)

cat(sprintf("\n%s\nLatvija: galveno rādītāju relatīvā pārmaiņa (procentpunkti vai skalas vienības) pa notikumiem\n%s\n", strrep("=", 100), strrep("=", 100)))
print(dcast(unique(O[cnt == "LVA" & id %in% names(KEY), .(id, notikums, hbsc_drel)]), id ~ notikums, value.var = "hbsc_drel",
            fun.aggregate = function(x) if (length(x)) round(x[1], 1) else NA_real_), row.names = FALSE)
cat("  PISA Latvija relatīvi:", paste(unique(O[cnt == "LVA", .(notikums, v = round(pisa_drel, 1))])[, paste0(notikums, ": ", v)], collapse = "; "), "\n")
RC <- "output/pisa2025/simptomi_modelis_C_parbaude.csv"
if (file.exists(RC)) {
  R <- fread(RC, colClasses = c(merkis = "character"))
  la <- R[grupa == "LVA A" | grupa %in% setdiff(VAL, "LVA"), .(merkis, grupa, parm_fakt)][
    , .(lva_a_rel = parm_fakt[grupa == "LVA A"] - mean(parm_fakt[grupa != "LVA A"], na.rm = TRUE)), by = merkis]
  cat("  PISA latviešu valodas plūsma relatīvi pret 7 valstīm:", paste0(la$merkis, ": ", round(la$lva_a_rel, 1), collapse = "; "), "\n")
}
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
