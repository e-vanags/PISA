# ============================================================================
# 64k. Simptomu modelis: tikai valsts iekšējā dinamika
# ============================================================================
#
# JAUTĀJUMS: vai valsts (plūsmas) iekšējās simptomu pārmaiņas paredz tās iekšējās
# lasītprasmes pārmaiņas? C variants mācās arī no atšķirībām starp valstīm; šeit tās
# tiek izņemtas.
#
# DIZAINS: katram mērķa ciklam T katra pazīme un lasīšanas rezultāts tiek centrēti pa
# grupām (valsts; Latvijā valodas plūsma) ar šīs grupas vidējo apmācības ciklos (< T,
# ar svariem; tikai derīgās vērtības). Tātad nav nākotnes informācijas. Modelis mācās,
# kā novirze no grupas vēsturiskā vidējā simptomos saistās ar novirzi rezultātā.
# Paredzētais līmenis = grupas vēsturiskais vidējais + paredzētā novirze. Grupas bez
# vēstures (EST, LTU 2006. gadā) netiek vērtētas. Neatbildēts jautājums pēc centrēšanas
# = −10⁷ (lai nesakrīt ar centrētām vērtībām). Pārējais kā C variantā (64h).
#
# Izvade: output/pisa2025/simptomi_modelis_iekseja_parbaude.csv, simptomi_modelis_iekseja_shap.csv
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("64k_simptomi_valsts_iekseja")
D <- sagatavot_datus(0.9); PIEEJ <- pieejamiba(D$ND_V)
X <- slidosa_parbaude(D$A, PIEEJ, centret = TRUE)
fwrite(X$R, "output/pisa2025/simptomi_modelis_iekseja_parbaude.csv")
fwrite(X$SH, "output/pisa2025/simptomi_modelis_iekseja_shap.csv")

cat("\n", strrep("=", 100), "\nSalīdzinājums ar C variantu (64h)\n", strrep("=", 100), "\n", sep = "")
RC <- fread("output/pisa2025/simptomi_modelis_C_parbaude.csv", colClasses = c(merkis = "character"))
drukat_salidzinajumu(list(C = RC, `valsts iekšējā` = X$R))
cat("\n  SHAP pa jomām, latviešu valodas plūsma (absolūtā paredzētā pārmaiņa):\n")
print(dcast(shap_jomas(X$SH), domens ~ merkis, value.var = "lva", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
cat("\n  Tas pats relatīvi pret 7 valstīm:\n")
print(dcast(shap_jomas(X$SH), domens ~ merkis, value.var = "rel", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
beigt_zurnalu()
