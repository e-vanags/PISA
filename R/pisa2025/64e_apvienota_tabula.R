# ============================================================================
# 64e. Simptomu modelis, 2. solis: apvienotā skolēnu tabula 2000–2025 (A līmenis)
# ============================================================================
#
# Ievade: output/pisa2025/simptomi_karte_pilna.csv (64d), data/pisa_veci/qqq_*_<gads>.csv,
#         data/pisa2025/processed/simptomi_<gads>.rds
#
# Katrs skolēns (LVA + EST, LTU, POL, FIN, CZE, HUN, DEU; 9 cikli) ir viena rinda:
#   meta: gads, cnt, skola, skolens, w (W_FSTUWT), PV1READ…PV10READ (vecajos 5),
#         langn, plusma (LVA: A latviešu / B krievu pēc mājas valodas, kā 00_ab_dati.R)
#   konstrukti: katrs manuālais un automātiskais konstrukts (vismaz 2 cikli) vienā
#         kolonnā; skolu anketas konstrukti pievienoti pēc skolas
#   Q_TRUKST: anketas trūkstošo atbilžu daļa skolēnam (no tā cikla konstruktiem)
# Viena cikla mainīgie šeit netiek iekļauti (tie paliek simptomi_<gads> failos).
#
# Saskaņošana: grāmatas mājās 2022/2025 (7 kat.) un 2000 (7 kat., citas robežas)
# pārkodētas uz 2003–2018 6 kategorijām (2000 aptuveni).
# Virziena pārbaude: katram konstruktam katrā ciklā Spīrmena korelācija ar lasīšanas
# PV vidējo (visas 8 valstis); ja zīme starp cikliem mainās (|r| > 0,05), konstrukts
# tiek atzīmēts pārbaudei (iespējams, apgriezts kodējums).
#
# Izvade: data/pisa2025/processed/simptomi_apvienots.rds
#         output/pisa2025/simptomi_virziena_parbaude.csv
#         output/pisa2025/logs/64e_apvienota_tabula_output.txt
# ============================================================================

suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64e_apvienota_tabula_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
VECIE <- GADI[1:5]
K <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")
K <- K[veids != "viens cikls"]
cat("Konstrukti tabulā:", uniqueN(K$konstrukts), "(skolēnu", uniqueN(K[fails == "STU"]$konstrukts),
    ", skolu", uniqueN(K[fails == "SCH"]$konstrukts), ")\n\n")

sk_num <- function(x) suppressWarnings(as.numeric(if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x))
plusma <- function(langn, g) {
  l <- as.character(as.numeric(langn))
  if (g == "2003") ifelse(l == "428001", "A", ifelse(l == "428002", "B", NA)) else ifelse(l == "325", "A", ifelse(l == "495", "B", NA))
}
# Vienots virziens (noteikts 2026-09-12 ar vērtību etiķetēm un korelāciju ar tā paša
# cikla indeksiem BELONG, ATSCHL, ATTLNACT; skat. docs/61):
#   Likerta skalas: augstāks = vairāk piekrīt (1 nepilnīgi … 4 pilnīgi piekrīt)
#   skolu indeksi: augstāks = lielāka problēma / trūkums (kā 2015+ STUBEHA, EDUSHORT)
APGRIEZT_LIKERT <- list(
  K_PIED_ATSTUMTS = c("2003", "2012", "2015", "2018", "2022", "2025"), K_PIED_DRAUGI = c("2003", "2012", "2015", "2018", "2022", "2025"),
  K_PIED_PIEDERU = c("2003", "2012", "2015", "2018", "2022", "2025"), K_PIED_NEVEIKLS = c("2003", "2012", "2015", "2018", "2022", "2025"),
  K_PIED_VIENTULS = c("2003", "2012", "2015", "2018", "2022", "2025"), K_PIED_PATIK_CITIEM = c("2012", "2015", "2018", "2022", "2025"),
  E_SKOLA_MAZ_SAGATAVO = c("2003", "2012"), E_SKOLA_LAIKA_IZSKIESANA = c("2003", "2012"),
  E_SKOLA_DEVA_PARLIECIBU = c("2003", "2012"), E_SKOLA_NODERIGA_DARBAM = c("2003", "2012"),
  E_CENTIBAS_SVARIGAS = c("2012", "2018"))
APGRIEZT_INDEKSU <- list(SK_SKOLENU_UZVEDIBA = c("2003", "2009"), SK_MACIBU_RESURSI = c("2003", "2006", "2009", "2012"),
                         AUTO_DISCLIMA = "2000", AUTO_INTMAT = "2000")   # PISA 2000 indeksu virziens pretējs
# Vecie vienumu kodi, kas pēc etiķetes izskatās līdzīgi, bet korelācija ar lasīšanu ir
# pretēja un liela: dažādi jautājumi, netiek apvienoti (paliek tikai simptomi_<gads>).
IZSLEGT_AUTO <- c("AUTO_ST12Q01", "AUTO_ST13Q01", "AUTO_SC21Q02",
                  "AUTO_ST09Q01",      # 2000 un 2009 dažādi jautājumi (vid. 5410 / 5600)
                  "AUTO_SC053D11TA")   # valstij specifisks kods (vērtības līdz 10^7)
IZSLEGT_CIKLA <- c("AUTO_PCGIRLS 2000")   # 2000. gada vērtības neatbilst daļai (vid. 0,01)
# Skalas vai definīcijas maiņa (skalu audits 2026-09-12): šiem cikliem konstrukts tiek
# nodalīts atsevišķā kolonnā <konstrukts>__B, lai modelis nejauc divas skalas.
SADALIT <- c("AUTO_OUTHOURS 2012",                                   # 2012 ≠ 2015+ definīcija
             "AUTO_ICTHOME 2015", "AUTO_ICTHOME 2018", "AUTO_ICTSCH 2015", "AUTO_ICTSCH 2018",   # summa, citur WLE
             "AUTO_PROADMIN 2025", "AUTO_PROMGMT 2025", "AUTO_PROPAT6 2025", "AUTO_PROPAT7 2025",
             "AUTO_PROPSUPP 2025", "AUTO_PROPSCIE 2025", "SK_SKOLENI_UZ_SKOLOTAJU 2025",   # 2025 kategorijas, citur daļa/attiecība
             "L_BIBL_SKOLAS 2000")                                    # 2000 cita skala (1–5)
# Veco ciklu trūkstošo kodu noplūde (atrasts 2026-09-12): piem., 2009 SCHSIZE (9 ciparu
# lauks) kods 99999, PISA 2000 indeksos 97/98/99, proporcijās 7/9. Datos balstīts noteikums:
#   vesels skaitlis formā 9…96/97/98/99, kas ≥ 3 × lielākā pārējā vērtība, vai
#   viencipara 7/8/9, ja pārējās vērtības ≤ 4 un kods ≥ 2 × lielākā pārējā vērtība.
# (Pirmajā versijā format() vektoram deva kopīgas decimāles, "97.000", un kodi palika.)
SARGI <- list()
tirit_sargus <- function(x, k, g) {
  ok <- !is.na(x); ci <- sprintf("%.0f", x)
  s <- ok & x == round(x) & ((x >= 96 & grepl("^9+[6789]$", ci)) | x %in% c(7, 8, 9))
  if (!any(s)) return(x)
  m <- suppressWarnings(max(x[ok & !s]))
  if (!is.finite(m)) return(x)
  z <- s & ((x >= 96 & x >= 3 * m) | (x < 10 & m <= 4 & x >= 2 * m))
  if (any(z)) { SARGI[[paste(k, g)]] <<- data.table(konstrukts = k, gads = g, nomainiti = sum(z), kodi = paste(sort(unique(x[z])), collapse = ","), max_derigais = m); x[z] <- NA }
  x
}
parkodet <- function(k, g, x) {
  if (k == "AUTO_PCGIRLS" && any(x > 1.5, na.rm = TRUE)) return(x / 100)           # 2009 procenti, citur daļa
  if (k == "SK_SKOLAS_LIELUMS" && g == "2025") return(c(102, 295, 568, 1114)[x])    # SCHSIZE_Q kvartiles → starpt. mediānas
  if (k == "AUTO_AGE" && any(x > 100, na.rm = TRUE)) return(x / 12)                  # 2000: mēnešos
  if (k == "SK_SKOLENI_UZ_SKOLOTAJU" && g == "2000") return(ifelse(x >= 97, NA, x))  # 2000 sintakse: stratio (97,98,99)
  if (k == "S_KLASE") return(ifelse(x >= 90, NA, x))                                 # 96 = bez klases (2015–2022 nav deklarēts kā trūkstošs)
  if (k == "SK_DATORI_UZ_SKOLENU" && g %in% c("2000", "2003")) return(ifelse(x > 0, 1 / x, NA))   # skolēni uz datoru → datori uz skolēnu
  if (k == "L_GRAMATAS_MAJAS" && g %in% c("2022", "2025")) return(ifelse(x <= 2, 1, x - 1))
  if (k == "L_GRAMATAS_MAJAS" && g == "2000") return(c(1, 1, 2, 3, 4, 5, 6)[x])   # aptuveni
  if (k == "L_MAJAS_MACIBU_GRAMATAS" && g == "2022")                                 # 2022: 1 nav, 2–4 skaits, 5 nezinu
    return(ifelse(x %in% 2:4, 1, ifelse(x %in% 1, 2, NA)))                           # → ir (1) / nav (2), kā 2003–2018
  if (k == "SK_SKOLAS_TIPS" && g == "2009") return(4 - x)                           # 2009: 1 = valsts
  if (!is.null(APGRIEZT_LIKERT[[k]]) && g %in% APGRIEZT_LIKERT[[k]]) return(5 - x)
  if (!is.null(APGRIEZT_INDEKSU[[k]]) && g %in% APGRIEZT_INDEKSU[[k]]) return(-x)
  x
}

cikls <- function(g) {
  t0 <- Sys.time()
  if (g %in% VECIE) {
    s <- fread(sprintf("data/pisa_veci/qqq_STU_%s.csv", g), colClasses = "character")
    h <- fread(sprintf("data/pisa_veci/qqq_SCH_%s.csv", g), colClasses = "character")
    meta <- data.table(gads = g, cnt = s$CNT, skola = paste(s$CNT, s$SCHOOLID), skolens = paste(s$CNT, s$SCHOOLID, s$STIDSTD),
                       w = sk_num(s$W_FSTUWT), langn = if ("LANGN" %in% names(s)) s$LANGN else NA_character_)
    for (p in 1:10) meta[[paste0("PV", p, "READ")]] <- if (paste0("PV", p, "READ") %in% names(s)) sk_num(s[[paste0("PV", p, "READ")]]) else NA_real_
    hk <- paste(h$CNT, h$SCHOOLID)
  } else {
    X <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g)); s <- as.data.table(X$stu); h <- as.data.table(X$sch)
    meta <- data.table(gads = g, cnt = as.character(s$CNT), skola = paste(g, sk_num(s$CNTSCHID)),
                       skolens = paste(g, sk_num(s$CNTSTUID)), w = sk_num(s$W_FSTUWT), langn = as.character(sk_num(s$LANGN)))
    for (p in 1:10) meta[[paste0("PV", p, "READ")]] <- sk_num(s[[paste0("PV", p, "READ")]])
    hk <- paste(g, sk_num(h$CNTSCHID))
  }
  meta$plusma <- ifelse(meta$cnt == "LVA", plusma(meta$langn, g), NA_character_)
  kg <- K[gads == g & !(konstrukts %in% IZSLEGT_AUTO) & !(paste(konstrukts, gads) %in% IZSLEGT_CIKLA)]
  names(s) <- toupper(names(s)); names(h) <- toupper(names(h))
  sm <- match(meta$skola, hk)
  kol <- list()
  for (i in seq_len(nrow(kg))) {
    k <- kg$konstrukts[i]; v <- toupper(kg$mainigais[i])
    x <- if (kg$fails[i] == "SCH") { if (v %in% names(h)) sk_num(h[[v]])[sm] else NULL } else { if (v %in% names(s)) sk_num(s[[v]]) else NULL }
    if (is.null(x)) next
    if (g %in% VECIE) x <- tirit_sargus(x, k, g)
    x <- parkodet(k, g, x)
    k2 <- if (paste(k, g) %in% SADALIT) paste0(k, "__B") else k
    kol[[k2]] <- if (all(is.na(x) | x == round(x))) as.integer(round(x)) else x
  }
  d <- cbind(meta, as.data.table(kol))
  # Mājās runātā valoda: valodas kods ir nomināls → 1 = valsts galvenā (biežākā) valoda ciklā, 0 = cita
  if ("G_VALODA_MAJAS" %in% names(d))
    d[, G_VALODA_MAJAS := { v <- G_VALODA_MAJAS; if (all(is.na(v))) NA_integer_ else
      as.integer(v == as.numeric(names(which.max(table(v))))) }, by = cnt]
  stu_k <- names(kol)[sub("__B$", "", names(kol)) %in% kg[fails == "STU"]$konstrukts]
  d$Q_TRUKST <- if (length(stu_k)) rowMeans(is.na(as.matrix(d[, ..stu_k]))) else NA_real_
  cat(sprintf("  %s: %6d skolēni, %4d konstrukti (skolēnu %d, skolu %d), %.1f s\n", g, nrow(d), length(kol),
              length(stu_k), length(kol) - length(stu_k), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  d
}
A <- rbindlist(lapply(GADI, cikls), fill = TRUE, use.names = TRUE)
A[, PVREAD := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0("PV", 1:10, "READ")]
cat("\nApvienotā tabula:", nrow(A), "skolēni ×", ncol(A), "kolonnas\n")
print(A[, .N, by = .(gads, cnt)][, dcast(.SD, gads ~ cnt, value.var = "N")])
cat("\nLatvijas plūsmas pa gadiem:\n"); print(A[cnt == "LVA", .N, by = .(gads, plusma)][order(gads, plusma)])
cat("\nAnketas trūkstošo daļa (vidēji), LVA pret pārējām:\n")
print(A[, .(LVA = round(100 * mean(Q_TRUKST[cnt == "LVA"], na.rm = TRUE), 1), citas = round(100 * mean(Q_TRUKST[cnt != "LVA"], na.rm = TRUE), 1)), by = gads])
saveRDS(A, "data/pisa2025/processed/simptomi_apvienots.rds")

# Virziena pārbaude
meta_k <- c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD", "Q_TRUKST")
konst <- setdiff(names(A), meta_k)
vp <- rbindlist(lapply(konst, function(k) {
  A[!is.na(get(k)) & !is.na(PVREAD), .(r = if (.N >= 200 && uniqueN(get(k)) > 1) suppressWarnings(stats::cor(get(k), PVREAD, method = "spearman")) else NA_real_, n = .N), by = gads][, konstrukts := k]
}))
vp <- vp[!is.na(r)]
zimes <- vp[abs(r) > 0.05, .(pozitivi = sum(r > 0), negativi = sum(r < 0), cikli = .N), by = konstrukts]
aizd <- zimes[pozitivi > 0 & negativi > 0]
cat("\nVirziena pārbaude: konstrukti ar korelāciju vismaz 2 ciklos:", nrow(zimes[cikli >= 2]),
    "| zīme mainās:", nrow(aizd), "\n")
if (nrow(aizd)) {
  w <- dcast(vp[konstrukts %in% aizd$konstrukts], konstrukts ~ gads, value.var = "r")
  print(w[order(konstrukts)], digits = 2)
}
fwrite(vp, "output/pisa2025/simptomi_virziena_parbaude.csv")

# Noplūdušie trūkstošo vērtību kodi (vecie cikli)
SG <- rbindlist(SARGI)
cat("\nNoplūdušie trūkstošo kodi, pārvērsti par NA:", nrow(SG), "konstrukts × cikls\n")
if (nrow(SG)) print(SG[order(-nomainiti)], nrows = 200)

# Skalu audits: vai konstrukta sadalījums kādā ciklā krasi atšķiras no citiem (skalas,
# vienības vai kodējuma maiņa). Karogi: maksimuma attiecība > 3 (pozitīvām skalām),
# SD attiecība > 3, kategoriāls (≤ 6 vērtības) vienā un nepārtraukts (≥ 25) citā ciklā,
# maksimums ≥ 9995 (koda noplūde).
aud <- rbindlist(lapply(konst, function(k) {
  A[!is.na(get(k)), .(n = .N, vid = sum(get(k) * w) / sum(w), sd = sd(get(k)), min = min(get(k)), max = max(get(k)),
                      unik = uniqueN(get(k))), by = gads][, konstrukts := k]
}))
fl <- aud[n >= 200, .(cikli = .N,
                      max_att = if (all(max > 0)) max(max) / min(max) else NA_real_,
                      sd_att = if (all(sd > 0)) max(sd) / min(sd) else NA_real_,
                      kat_nep = min(unik) <= 6 & max(unik) >= 25, koda_nopl = any(max >= 9995)), by = konstrukts]
fl <- fl[cikli >= 2 & ((!is.na(max_att) & max_att > 3) | (!is.na(sd_att) & sd_att > 3) | kat_nep | koda_nopl)]
fwrite(merge(aud, fl, by = "konstrukts"), "output/pisa2025/simptomi_skalu_audits.csv")
cat("\nSkalu audits: aizdomīgi konstrukti", nrow(fl), "no", uniqueN(aud$konstrukts), "\n")
if (nrow(fl)) {
  w2 <- dcast(aud[konstrukts %in% fl$konstrukts], konstrukts ~ gads, value.var = "max", fun.aggregate = function(x) signif(x[1], 3))
  cat("  Maksimums pa cikliem:\n"); print(merge(fl[, .(konstrukts, max_att = round(max_att, 1), sd_att = round(sd_att, 1), kat_nep, koda_nopl)], w2, by = "konstrukts"), nrows = 300)
  w3 <- dcast(aud[konstrukts %in% fl$konstrukts], konstrukts ~ gads, value.var = "vid", fun.aggregate = function(x) signif(x[1], 3))
  cat("  Vidējais pa cikliem:\n"); print(w3, nrows = 300)
}
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
