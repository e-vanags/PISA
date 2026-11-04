# ============================================================================
# 64n. Simptomu modelis, 5. solis (otrā versija): ex ante "simptomu impulss"
# ============================================================================
#
# KĀPĒC: 64l (pirmā ex ante versija) pārbīdīja katra skolēna pazīmes par grupas vidējo
# simptomu pārmaiņu T−2 → T−1. Tas neder: kokiem pārbīde bināriem un sastāva mainīgajiem
# (dzimums, klase, mājas valoda, skolas tips) neatspoguļo sadalījuma maiņu, un prognoze
# bija sistemātiski par zemu (MAE 26,3 pret naivā 9,1).
#
# PIEEJA: pārmaiņas T−1 → T prognoze = tā pārmaiņas T−2 → T−1 daļa, ko paskaidroja simptomi
# ārpus apmācības (slīdošā sākumpunkta modelis mērķim T−1, apmācīts uz cikliem < T−1),
# pārskalota uz perioda garumu: Δ̂_T = parm_pred_{T−1} × (T − (T−1)) / ((T−1) − (T−2)).
# Visa šī informācija (PISA T−1 anketas un rezultāti) bija publicēta pirms PISA T testa.
# Salīdzina ar naivo (0) un rezultāta driftu (faktiskā pārmaiņa T−2 → T−1, pārskalota).
# Varianti: C (64h), bez WLE (64j), valsts iekšējā dinamika (64k).
#
# Izvade: output/pisa2025/simptomi_impulss.csv, logs/64n_simptomu_impulss_output.txt
# ============================================================================

suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64n_simptomu_impulss_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
GADI <- c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)
VAR <- c(C = "output/pisa2025/simptomi_modelis_C_parbaude.csv", `bez WLE` = "output/pisa2025/simptomi_modelis_bezWLE_parbaude.csv",
         `valsts iekšējā` = "output/pisa2025/simptomi_modelis_iekseja_parbaude.csv")
R <- rbindlist(lapply(names(VAR), function(v) fread(VAR[[v]])[, .(variants = v, merkis = as.integer(merkis), grupa, parm_fakt, parm_pred)]))
R[, iepr := GADI[match(merkis, GADI) - 1]]
P <- merge(R, R[, .(variants, grupa, iepr = merkis, pred_iepr = parm_pred, fakt_iepr = parm_fakt)], by = c("variants", "grupa", "iepr"))
P[, s := (merkis - iepr) / (iepr - GADI[match(iepr, GADI) - 1])]
P[, `:=`(impulss = pred_iepr * s, drifts = fakt_iepr * s)]
P <- P[!is.na(parm_fakt) & !is.na(impulss)]
fwrite(P, "output/pisa2025/simptomi_impulss.csv")

nov <- function(x, nos, col) x[!is.na(get(col)), .(prognoze = nos, pari = .N, mae = mean(abs(parm_fakt - get(col))),
                                                    kor = if (sd(get(col)) > 0) cor(parm_fakt, get(col)) else NA_real_,
                                                    virziens = mean(sign(parm_fakt) == sign(get(col))))]
cat(strrep("=", 100), "\nAbsolūtā pārmaiņa, 7 valstis, mērķi 2009–2025\n", strrep("=", 100), "\n", sep = "")
V <- P[grupa %in% SALID][, nulle := 0]
for (v in names(VAR)) {
  x <- V[variants == v]
  print(rbind(nov(x, "naivais (0)", "nulle"), nov(x, "rezultāta drifts", "drifts"), nov(x, paste("simptomu impulss,", v), "impulss"))[
    , lapply(.SD, function(z) if (is.numeric(z)) round(z, 2) else z)], row.names = FALSE)
}
cat("\n", strrep("=", 100), "\nRelatīvā pārmaiņa (valsts − 7 valstu vidējais), 7 valstis\n", strrep("=", 100), "\n", sep = "")
V[, `:=`(rel_f = parm_fakt - mean(parm_fakt), rel_i = impulss - mean(impulss), rel_d = drifts - mean(drifts)), by = .(variants, merkis)]
print(V[, .(pari = .N, mae_impulss = round(mean(abs(rel_f - rel_i)), 2), mae_drifts = round(mean(abs(rel_f - rel_d)), 2), mae_naivais = round(mean(abs(rel_f)), 2),
            kor_impulss = round(cor(rel_f, rel_i), 2), kor_drifts = round(cor(rel_f, rel_d), 2),
            virz_impulss = round(mean(sign(rel_f) == sign(rel_i)), 2)), by = variants], row.names = FALSE)

cat("\n", strrep("=", 100), "\nLatviešu valodas plūsma (relatīvi pret 7 valstīm)\n", strrep("=", 100), "\n", sep = "")
S <- V[, .(sf = mean(parm_fakt), si = mean(impulss), sd = mean(drifts)), by = .(variants, merkis)]
L <- merge(P[grupa == "LVA A"], S, by = c("variants", "merkis"))
L[, `:=`(rel_fakt = parm_fakt - sf, rel_impulss = impulss - si, rel_drifts = drifts - sd)]
print(dcast(L, merkis + round(parm_fakt, 1) + round(rel_fakt, 1) ~ variants, value.var = c("impulss", "rel_impulss"), fun.aggregate = function(z) round(z[1], 1)), row.names = FALSE)
print(L[, .(pari = .N, virziens_rel = sum(sign(rel_fakt) == sign(rel_impulss)), mae_rel = round(mean(abs(rel_fakt - rel_impulss)), 1),
            mae_rel_naiv = round(mean(abs(rel_fakt)), 1), mae_rel_drifts = round(mean(abs(rel_fakt - rel_drifts)), 1)), by = variants], row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
