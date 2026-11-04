# ============================================================================
# 73. Slānis 4. Skolas tips PISA datos: kas ir, kā nav, un aizstājēji
# ============================================================================
#
# Jautājums (lietotājs 2026-09-13): vai tiešām publiskajos PISA datos nav
# skolas tipa (valsts ģimnāzija / vidusskola / pamatskola)?
#
# PĀRBAUDE (sk. arī docs/46 Slānis 4). Latvijas failos 2015–2025:
#   STRATUM   tikai atrašanās vieta: Rīga, lielpilsēta, pilsēta, lauki
#   SCHLTYPE  tikai publiska / privāta
#   PROGN     pamatizglītība / mazākumtautību / vispārējā vidējā / profesionālā
#   Skolas tipa, ģimnāzijas statusa vai klašu diapazona mainīgā nav.
#
# AIZSTĀJĒJI, kas ir datos:
#   (1) ISCEDP: vai skolas izlasē ir 10.+ klases skolēni  ->  skola ar vidusskolas
#       posmu (vidusskola vai ģimnāzija) pret tikai pamatskolu. Trokšņains: 15 gadu
#       vecumā 10. klasē ir ~3 % skolēnu, tāpēc daļa vidusskolu paliek neatpazīta.
#   (2) SC012Q01TA: direktors atbild, cik bieži uzņemot ņem vērā skolēna
#       sekmes ("vienmēr" = atlase). Pieejams visos četros ciklos.
# Kategorijas: tikai pamatskola | vidusskola bez atlases | vidusskola ar atlasi.
#
# METODE. Latviešu valodas plūsma (LANGN latviešu), 10 PV, BRR Fay 0,5
# (dalītājs 20), Rubina kombinācija (rubin_brr kā 00_ab_dati.R). Pārmaiņai
# 2018 -> 2025 pievienota saites kļūda 1,832 (OECD 2025 Vol. I, atvasināta, sk.
# data/pisa_tehniskie/PISA2025_TR_AVOTS.md; līdz 2026-09-13 sqrt(1,49² + 3²)).
#
# Izvade: output/pisa2025/skolu_tips_aizstajeji.csv
#         output/pisa2025/skolu_tips_skolas.csv
#         output/pisa2025/logs/73_skolu_tips_output.txt
# ============================================================================

suppressMessages({library(haven); library(data.table)})
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
sink("output/pisa2025/logs/73_skolu_tips_output.txt", split = TRUE)
cat("73. Skolas tipa aizstājēji, ", format(Sys.time()), "\n\n")

rubin_brr <- function(pv, w, rw, stat) {
  M <- length(pv)
  e <- vapply(pv, function(p) stat(p, w), numeric(1))
  U <- mean(vapply(seq_len(M), function(m)
    sum((vapply(rw, function(r) stat(pv[[m]], r), numeric(1)) - e[m])^2) / 20,
    numeric(1)))
  B <- stats::var(e)
  c(est = mean(e), se = sqrt(U + (1 + 1 / M) * B))
}
VID <- function(x, w) sum(x * w) / sum(w)
id_txt <- function(x) { x <- haven::zap_labels(x); sub("\\.0+$", "", trimws(as.character(x))) }

SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", "~/pisa2025_sav")
STU <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav",
         "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
         "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV",
         "2025" = file.path(SAV_DIR, "CY09_MS_STU_PUF.sav"))
SCH <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_SCH_QQQ.sav",
         "2018" = "data/pisa2018/raw/SCH/CY07_MSU_SCH_QQQ.sav",
         "2022" = "data/pisa2022/raw/CY08MSP_SCH_QQQ.SAV",
         "2025" = file.path(SAV_DIR, "CY09_MS_SCH_PUF.sav"))
A <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")

# ---------------------------------------------------------------------------
# 1. Skolu līmeņa aizstājēji katrā ciklā
# ---------------------------------------------------------------------------
skolas <- rbindlist(lapply(names(STU), function(g) {
  vn <- names(read_sav(STU[[g]], n_max = 0))
  isk <- intersect(c("ISCEDP", "ISCEDL", "ISCEDO"), vn)[1]   # 2015: ISCEDL (līmenis 2/3), vēlāk ISCEDP (244/344)
  s <- as.data.table(read_sav(STU[[g]], col_select = c("CNT", "CNTSCHID", all_of(isk))))[CNT == "LVA"]
  s[, isced := as.numeric(get(isk))]
  s[, isced := fifelse(isced < 10, isced * 100, isced)]       # ISCEDL 3 -> 300
  sk <- s[, .(n_izl = .N, dala_10plus = mean(isced >= 300, na.rm = TRUE)), by = CNTSCHID]
  hn <- names(read_sav(SCH[[g]], n_max = 0))
  szk <- intersect(c("SCHSIZE", "SCHSIZE_Q"), hn)[1]           # 2025: SCHSIZE_Q (kvartiles)
  h <- as.data.table(read_sav(SCH[[g]], col_select = c("CNT", "CNTSCHID", "SC012Q01TA", all_of(szk), "STRATUM", "SCHLTYPE")))[CNT == "LVA"]
  h[, `:=`(atlase = as.numeric(SC012Q01TA), strats = as.character(as_factor(STRATUM)),
           schsize = as.numeric(get(szk)), privata = as.numeric(SCHLTYPE) < 3)]
  sk[, CNTSCHID := id_txt(CNTSCHID)]; h[, CNTSCHID := id_txt(CNTSCHID)]
  m <- merge(sk, h[, .(CNTSCHID, atlase, strats, schsize, privata)], by = "CNTSCHID", all.x = TRUE)
  m[, gads := g]
  m[, vidusskola := dala_10plus > 0]
  m[, tips := fifelse(!vidusskola, "tikai pamatskola",
               fifelse(is.na(atlase), "vidusskola, atlase nezināma",
               fifelse(atlase == 3, "vidusskola ar atlasi", "vidusskola bez atlases")))]
  m[, atlase_kaut := fifelse(is.na(atlase), NA, atlase >= 2)]
  m
}))
fwrite(skolas, "output/pisa2025/skolu_tips_skolas.csv")
cat("Skolu skaits pa cikliem un kategorijām:\n")
print(dcast(skolas[, .N, by = .(gads, tips)], tips ~ gads, value.var = "N", fill = 0))
cat("\nUzņemšana pēc sekmēm (SC012Q01TA) visās skolās, %:\n")
print(dcast(skolas[!is.na(atlase), .(pct = round(100 * .N / sum(!is.na(skolas$atlase[skolas$gads == gads[1]])), 0)), by = .(gads, atlase)],
            atlase ~ gads, value.var = "pct"))

# ---------------------------------------------------------------------------
# 2. Latviešu valodas plūsmas lasīšana pa skolu kategorijām
# ---------------------------------------------------------------------------
LE_TAB <- fread("output/pisa2025/saites_kludas.csv")[joma == "READ"]   # viens avots, 78. skripts
LE_2018_2025 <- LE_TAB[no == 2018 & uz == 2025, LE]                   # 1,832 (līdz 2026-09-13 sqrt(1,49² + 3²) = 3,35)
stopifnot(length(LE_2018_2025) == 1)
rez <- rbindlist(lapply(names(A), function(g) {
  d <- as.data.table(A[[g]])[CNT == "LVA"]
  d[, CNTSCHID := id_txt(CNTSCHID)]
  lang <- tolower(as.character(as_factor(d$LANGN)))
  d <- d[grepl("latvie|latvian", lang)]
  d <- merge(d, skolas[gads == g, .(CNTSCHID, tips, vidusskola, atlase_kaut, strats)], by = "CNTSCHID", all.x = TRUE)
  d[is.na(tips), tips := "skola bez anketas"]
  pvn <- paste0("PV", 1:10, "READ"); rwn <- paste0("W_FSTURWT", 1:80)
  kat <- function(sub, nos) {
    pv <- lapply(pvn, function(k) sub[[k]]); rw <- lapply(rwn, function(k) sub[[k]])
    r <- rubin_brr(pv, sub$W_FSTUWT, rw, VID)
    z2 <- rubin_brr(pv, sub$W_FSTUWT, rw, function(x, w) 100 * VID(x < 407.47, w))
    l5 <- rubin_brr(pv, sub$W_FSTUWT, rw, function(x, w) 100 * VID(x >= 625.61, w))
    data.table(gads = g, kategorija = nos, n_skolenu = nrow(sub), n_skolu = uniqueN(sub$CNTSCHID),
               dala_skolenu = round(100 * sum(sub$W_FSTUWT) / sum(d$W_FSTUWT), 1),
               escs = round(VID(ifelse(is.na(sub$ESCS), 0, sub$ESCS), sub$W_FSTUWT), 2),
               vid = r[["est"]], se = r[["se"]], zem2 = z2[["est"]], zem2_se = z2[["se"]],
               l5 = l5[["est"]], l5_se = l5[["se"]])
  }
  rbind(kat(d, "visa A plūsma"),
        rbindlist(lapply(split(d, d$tips), function(s) kat(s, s$tips[1]))),
        kat(d[vidusskola == TRUE], "visas vidusskolas"),
        kat(d[!is.na(atlase_kaut) & atlase_kaut == TRUE], "atlase vismaz dažreiz (visas skolas)"),
        kat(d[!is.na(atlase_kaut) & atlase_kaut == FALSE], "atlase nekad (visas skolas)"))
}))
rez[, `:=`(vid = round(vid, 1), se = round(se, 1), zem2 = round(zem2, 1), zem2_se = round(zem2_se, 1),
           l5 = round(l5, 1), l5_se = round(l5_se, 1))]
fwrite(rez, "output/pisa2025/skolu_tips_aizstajeji.csv")

cat("\nLatviešu valodas plūsma: lasīšanas vidējais pa skolu kategorijām\n")
print(dcast(rez, kategorija ~ gads, value.var = "vid"))
cat("\nSkolēnu daļa kategorijā, %\n")
print(dcast(rez, kategorija ~ gads, value.var = "dala_skolenu"))
cat("\nZem 2. līmeņa, %\n")
print(dcast(rez, kategorija ~ gads, value.var = "zem2"))
cat("\n5.+ līmenis, %\n")
print(dcast(rez, kategorija ~ gads, value.var = "l5"))
cat("\nVidējais ESCS\n")
print(dcast(rez, kategorija ~ gads, value.var = "escs"))

# starpība "ar atlasi" pret pārējiem un pārmaiņa 2018 -> 2025
st <- rez[kategorija %in% c("vidusskola ar atlasi", "vidusskola bez atlases", "tikai pamatskola")]
starp <- st[, .(starpiba = vid[kategorija == "vidusskola ar atlasi"] - vid[kategorija == "tikai pamatskola"],
                se = sqrt(se[kategorija == "vidusskola ar atlasi"]^2 + se[kategorija == "tikai pamatskola"]^2)), by = gads]
cat("\nStarpība: vidusskola ar atlasi mīnus tikai pamatskola (A plūsma)\n"); print(starp[, .(gads, starpiba = round(starpiba, 1), se = round(se, 1))])
parm <- rez[gads %in% c("2018", "2025"), .(parmaina = vid[gads == "2025"] - vid[gads == "2018"],
             se = sqrt(se[gads == "2025"]^2 + se[gads == "2018"]^2 + LE_2018_2025^2)), by = kategorija]
parm[, z := parmaina / se]
cat("\nPārmaiņa 2018 -> 2025 pa kategorijām (ar saites kļūdu)\n"); print(parm[, .(kategorija, parmaina = round(parmaina, 1), se = round(se, 1), z = round(z, 2))])
fwrite(parm, "output/pisa2025/skolu_tips_parmaina_2018_2025.csv")
cat("\nPabeigts:", format(Sys.time()), "\n")
sink()
