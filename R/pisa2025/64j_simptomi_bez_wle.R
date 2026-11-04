# ============================================================================
# 64j. Simptomu modelis bez WLE indeksiem (skalas nobīdes pārbaude)
# ============================================================================
#
# JAUTĀJUMS: vai C varianta atlikusī līmeņa nestabilitāte (2009: −30 punkti) nāk no
# atvasinātajiem indeksiem, kuru skala starp cikliem mainās (WLE skalas katrā ciklā
# tiek pārskalotas; ESCS un citi saliktie indeksi arī)?
#
# IZSLĒDZ: konstruktus, kuru avota mainīgais kādā ciklā ir WLE indekss (etiķetē "WLE",
# vai tas pats nosaukums citā ciklā ir WLE), vai ne-jautājums ar etiķeti "Index of",
# "ESCS", "Scale". Paliek atsevišķi anketas jautājumi un vienkārši atvasināti rādītāji
# (vecums, klase, ISCED līmeņi, skaiti, attiecības, skolas lielums).
# Viss pārējais kā C variantā (64h), visi mērķi 2006–2025.
#
# Izvade: output/pisa2025/simptomi_modelis_bezWLE_parbaude.csv, simptomi_modelis_bezWLE_shap.csv
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("64j_simptomi_bez_wle")
D <- sagatavot_datus(0.9); PIEEJ <- pieejamiba(D$ND_V)
WL <- wle_konstrukti()
izsl <- intersect(WL$wle, D$KON)
cat("Izslēgti WLE un salikto indeksu konstrukti:", length(izsl), "no", length(D$KON), "\n")
print(table(jomas(izsl)))
cat("\n", strwrap(paste(izsl, collapse = ", "), 110), sep = "\n")
cat("\nPazīmju skaits pa mērķiem (C → bez WLE):\n")
for (Tg in MERKI) cat(sprintf("  %s: %d → %d\n", Tg, length(kopigas(PIEEJ, Tg, iepr_gads(Tg))),
                              length(kopigas(PIEEJ, Tg, iepr_gads(Tg), setdiff(D$KON, izsl)))))

X <- slidosa_parbaude(D$A, PIEEJ, atlase = setdiff(D$KON, izsl))
fwrite(X$R, "output/pisa2025/simptomi_modelis_bezWLE_parbaude.csv")
fwrite(X$SH, "output/pisa2025/simptomi_modelis_bezWLE_shap.csv")

cat("\n", strrep("=", 100), "\nSalīdzinājums ar C variantu (64h)\n", strrep("=", 100), "\n", sep = "")
RC <- fread("output/pisa2025/simptomi_modelis_C_parbaude.csv", colClasses = c(merkis = "character"))
drukat_salidzinajumu(list(C = RC, `bez WLE` = X$R))
cat("\n  SHAP pa jomām, latviešu valodas plūsma relatīvi pret 7 valstīm (bez WLE):\n")
print(dcast(shap_jomas(X$SH), domens ~ merkis, value.var = "rel", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
beigt_zurnalu()
