# ============================================================================
# 64m. Simptomu modelis: jutīguma pārbaude (mērķi 2022 un 2025)
# ============================================================================
#
# Vai C varianta secinājumi (valstu pārmaiņu paredzamība, latviešu valodas plūsmas
# relatīvā pārmaiņa un jomu ieguldījumi) paliek, mainot analītiķa izvēles?
#
# Visi varianti ar eta = 0,1 un ≤ 1500 kokiem (ātrumam); atskaite ir tas pats ar
# pamatiestatījumiem, un C (eta 0,05, 64h) dots salīdzinājumam.
#   sēklas 2 un 3              apmācības nejaušība (apakšizlases, validācijas daļa)
#   NA slieksnis 0,8 / 0,95    kas skaitās "neuzdots"
#   pieejamība ≥ 6 valstīs     pazīmju kopa plašāka
#   dziļums 4 / 8              modeļa sarežģītība
#   tikai jautājumi            bez jebkādiem atvasinātiem rādītājiem
#   bez mājas mantu indeksiem  HOMEPOS, ESCS, WEALTH, CULTPOSS, HEDRES, ICTRES, ICTHOME
#   mērķis PV1                 viens PV, nevis PV vidējais
# Pēc tam: salīdzinājuma kopa 5 valstis (bez EST, LTU); 64i nozīmīgo pazīmju zīmes
# stabilitāte visos variantos.
#
# Izvade: output/pisa2025/simptomi_jutigums_metrikas.csv, simptomi_jutigums_jomas.csv,
#         simptomi_jutigums_pazimes.csv
# ============================================================================

source("R/pisa2025/64_simptomi_riki.R")
sakt_zurnalu("64m_simptomi_jutigums")
WL <- wle_konstrukti()
MK <- c("2022", "2025")
par01 <- PAR_C; par01$eta <- 0.1
VAR <- list(
  list(nos = "atskaite"), list(nos = "sēkla 2", seed = 2), list(nos = "sēkla 3", seed = 3),
  list(nos = "pieejamība ≥6", min_valstu = 6), list(nos = "dziļums 4", depth = 4), list(nos = "dziļums 8", depth = 8),
  list(nos = "tikai jautājumi", atlase = "jautajumi"), list(nos = "bez mājas mantām", atlase = "bez_mantam"),
  list(nos = "mērķis PV1", merkis = "PV1READ"),
  list(nos = "bez SC034Q02NA", atlase = "bez_sc034"), list(nos = "bez apšaubāmajām", atlase = "bez_apsaub"),
  list(nos = "NA 0,8", slieksnis = 0.8), list(nos = "NA 0,95", slieksnis = 0.95))
# bez_sc034: neobligāto standartizēto testu biežums (LVA 2025 radikāla sadalījuma maiņa, docs/61 3c-3)
# bez_apsaub: bez mājas mantu indeksiem un SC034Q02NA

# Starprezultāti tiek saglabāti pēc katra varianta; atkārtotā palaišanā gatavie netiek pārrēķināti
FREZ <- "data/pisa2025/processed/jutigums_rezultati.rds"
D <- NULL; sl_cur <- NA; REZ <- if (file.exists(FREZ)) readRDS(FREZ) else list()
for (v in VAR) {
  if (!is.null(REZ[[v$nos]])) { cat(sprintf("  %-18s jau aprēķināts (ielādēts)\n", v$nos)); next }
  t0 <- Sys.time()
  sl <- if (is.null(v$slieksnis)) 0.9 else v$slieksnis
  if (!identical(sl, sl_cur)) { D <- NULL; gc(verbose = FALSE); D <- sagatavot_datus(sl); sl_cur <- sl }
  PIEEJ <- pieejamiba(D$ND_V, if (is.null(v$min_valstu)) Inf else v$min_valstu)
  atl <- switch(if (is.null(v$atlase)) "visas" else v$atlase, visas = NULL,
                jautajumi = setdiff(D$KON, WL$ne_jautajumi), bez_mantam = setdiff(D$KON, WL$mantas),
                bez_sc034 = setdiff(D$KON, "AUTO_SC034Q02NA"), bez_apsaub = setdiff(D$KON, c(WL$mantas, "AUTO_SC034Q02NA")))
  par <- par01; if (!is.null(v$depth)) par$max_depth <- v$depth
  X <- slidosa_parbaude(D$A, PIEEJ, merki = MK, atlase = atl, par = par, seed = if (is.null(v$seed)) 20260912 else v$seed,
                        merkis = if (is.null(v$merkis)) "PVREAD" else v$merkis, nmax = 1500, zinot = FALSE)
  X$R[, variants := v$nos]; X$SH[, variants := v$nos]; REZ[[v$nos]] <- X; saveRDS(REZ, FREZ)
  L <- lva_rel(X$R)
  cat(sprintf("  %-18s pazīmes %s | 7 valstu |kļūda| %s | LVA A paredzētā pārmaiņa %s, relatīvā %s (%.1f min)\n", v$nos,
              paste(unique(X$R[, .(merkis, pazimes)])$pazimes, collapse = "/"),
              paste(round(metrikas(X$R)$pa_merki$abs_kluda, 1), collapse = "/"),
              paste(round(L$parm_pred, 1), collapse = "/"), paste(round(L$rel_pred, 1), collapse = "/"),
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
RC <- fread("output/pisa2025/simptomi_modelis_C_parbaude.csv", colClasses = c(merkis = "character"))[merkis %in% MK][, variants := "C (eta 0,05)"]
SC <- fread("output/pisa2025/simptomi_modelis_C_shap.csv", colClasses = c(merkis = "character"))[merkis %in% MK][, variants := "C (eta 0,05)"]
RR <- rbind(RC, rbindlist(lapply(REZ, `[[`, "R")), fill = TRUE)
SS <- rbind(SC[, .(merkis, grupa, pazime, starpiba, variants)], rbindlist(lapply(REZ, `[[`, "SH")), fill = TRUE)
VN <- unique(RR$variants)

cat("\n", strrep("=", 100), "\nMetrikas pa variantiem\n", strrep("=", 100), "\n", sep = "")
MET <- rbindlist(lapply(VN, function(n) {
  x <- RR[variants == n]; pm <- metrikas(x)$pa_merki; l7 <- lva_rel(x); l5 <- lva_rel(x, SALID5)
  merge(pm, l7[, .(merkis, lva_parm_fakt = parm_fakt, lva_parm_pred = parm_pred, rel_fakt, rel_pred)], by = "merkis")[
    , `:=`(rel5_fakt = l5$rel_fakt[match(merkis, l5$merkis)], rel5_pred = l5$rel_pred[match(merkis, l5$merkis)], variants = n)]
}))
fwrite(MET, "output/pisa2025/simptomi_jutigums_metrikas.csv")
for (Tg in MK) {
  cat(sprintf("\n  %s ← %s:\n", Tg, iepr_gads(Tg)))
  print(MET[merkis == Tg, .(variants, abs_kluda = round(abs_kluda, 1), parm_kor = round(parm_kor, 2), lva_parm_pred = round(lva_parm_pred, 1),
                            rel_pred = round(rel_pred, 1), rel5_pred = round(rel5_pred, 1))], row.names = FALSE)
  cat(sprintf("  Faktiskā: LVA A pārmaiņa %+.1f, relatīvi pret 7 valstīm %+.1f, pret 5 valstīm %+.1f\n",
              MET[merkis == Tg][1, lva_parm_fakt], MET[merkis == Tg][1, rel_fakt], MET[merkis == Tg][1, rel5_fakt]))
}

cat("\n", strrep("=", 100), "\nJomu relatīvie ieguldījumi (LVA A − 7 valstis) pa variantiem\n", strrep("=", 100), "\n", sep = "")
JOM <- rbindlist(lapply(VN, function(n) shap_jomas(SS[variants == n])[, variants := n]))
fwrite(JOM, "output/pisa2025/simptomi_jutigums_jomas.csv")
for (Tg in MK) {
  cat(sprintf("\n  %s:\n", Tg))
  print(dcast(JOM[merkis == Tg], domens ~ variants, value.var = "rel", fun.aggregate = function(x) round(sum(x), 1)), row.names = FALSE)
}

cat("\n", strrep("=", 100), "\n64i nozīmīgās pazīmes (relatīvais ieguldījums, q < 0,05): zīmes stabilitāte\n", strrep("=", 100), "\n", sep = "")
fi <- "output/pisa2025/simptomi_C_nozimiba_ieguldijumi.csv"
if (file.exists(fi)) {
  IE <- fread(fi, colClasses = c(merkis = "character"))[tips == "pazīme" & q < 0.05 & merkis %in% MK]
  PZ <- rbindlist(lapply(VN, function(n) {
    x <- SS[variants == n]
    merge(x[grupa == "LVA A", .(merkis, pazime, lva = starpiba)], x[grupa %in% SALID, .(salid = mean(starpiba)), by = .(merkis, pazime)],
          by = c("merkis", "pazime"))[, `:=`(rel = lva - salid, variants = n)]
  }))
  fwrite(PZ, "output/pisa2025/simptomi_jutigums_pazimes.csv")
  if (nrow(IE)) {
    st <- merge(IE[, .(merkis, pazime = vieniba, rel_C = rel)], PZ, by = c("merkis", "pazime"), all.x = TRUE)
    st <- st[, .(rel_C = rel_C[1], varianti = sum(!is.na(rel)), ta_pati_zime = sum(sign(rel) == sign(rel_C[1]), na.rm = TRUE),
                 rel_min = min(rel, na.rm = TRUE), rel_max = max(rel, na.rm = TRUE)), by = .(merkis, pazime)]
    print(st[order(merkis, rel_C), .(merkis, pazime, domens = jomas(pazime), rel_C = round(rel_C, 2), varianti, ta_pati_zime,
                                      rel_min = round(rel_min, 2), rel_max = round(rel_max, 2), apraksts = apraksts(pazime))], row.names = FALSE)
    cat("  varianti: cik variantos pazīme ir modelī; ta_pati_zime: cik no tiem relatīvais ieguldījums ar to pašu zīmi kā 64i\n")
  } else cat("  64i neatrada nevienu nozīmīgu pazīmi mērķiem 2022/2025.\n")
} else cat("  64i rezultātu fails nav atrasts.\n")
beigt_zurnalu()
