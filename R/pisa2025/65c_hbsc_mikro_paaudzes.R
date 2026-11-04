# ============================================================================
# 65c. HBSC mikrodati 2001/02–2017/18: inventārs un paaudžu rinda
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS: vai Latvijas pusaudžu skolas patika, skolas spiediens un dzīves
# apmierinātība relatīvi pret 7 salīdzinājuma valstīm mainās pa dzimšanas
# paaudzēm, kad sākas lūzums (salīdzināt ar docs/73: vide 2001, prasme 2007), un
# vai PISA 2018/2022/2025 paaudzēm 11 un 13 gadu vecumā bija ex ante signāls.
#
# DATI: HBSC atvērtās piekļuves mikrodati (HBSC Data Management Centre, Bergenas
#   Universitāte): HBSC2001OAed1.0, HBSC2006OAed1.0, HBSC2010OAed1.0,
#   HBSC2014OAed1.1 (bez Lietuvas), HBSC2018OAed1.1 (data/Allrounds_SPSS/).
#   Papildinājums: HBSC Data Browser apkopojumi 2022 (visas valstis) un 2014
#   (tikai Lietuva, jo ed. 1.1 Lietuvu izņēma). Mikrodatus nedrīkst nodot
#   trešajām personām; izvadē ir tikai apkopojumi (valsts × kārta × vecums).
# VALSTIS: LVA un EST, LTU, FIN, CZE, HUN, POL, DEU (kodi = ISO numurs × 1000).
# SE: svērts vidējais ar skolu klasteru (Teilora) SE; svari tur, kur tie nav 1
#   (2010 DEU, HUN; 2014 un 2018 visām). Pārlūka rindām SE tuvinājums kā 67d.
# PAAUDZE: dzimšanas gads no dzimšanas gada un mēneša (dz_gads = vidējais grupā);
#   67d noapaļo līdz veselam gadam: b = floor(dz_gads + 0,5).
# RELATĪVI: rel7 = valsts − pārējo valstu vidējais tajā pašā kārtā un vecumā;
#   z = zīme × rel / sd_rel, kur sd_rel = kārtas-demeanotu vērtību SD pa visām
#   valstīm × kārtām (kā 67d), zīme: lielāks = labāk.
#
# VALODA: atvērtās piekļuves failos nav mainīgā, kas Latvijā šķirtu latviešu un
#   krievu valodas plūsmu (pārbaudīts: 2006 subregion ir 3 nedokumentēti strati,
#   2010 subregion = 1, 2014 id1 4 strati un m133–m135 tukši, 2018 id1 9 strati
#   un countryborn tukšs). Tāpēc viss ir visa Latvija.
#
# Izvade: output/pisa2025/hbsc_mikro_inventars.csv, hbsc_mikro_n.csv,
#   hbsc_mikro_paaudzes.csv (67d formāts + rel, z), hbsc_mikro_luzumi.csv,
#   hbsc_mikro_pisa_paaudzes.csv, hbsc_mikro_ex_ante.csv;
#   output/pisa2025/plots/65c_*.png; kešs data/Allrounds_SPSS/processed/.
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/65c_hbsc_mikro_paaudzes_output.txt"
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
cat("Darba direktorija:", getwd(), "\n")

# --- Pakotnes ----------------------------------------------------------------
suppressPackageStartupMessages({ library(haven); library(data.table); library(ggplot2); library(patchwork) })
set.seed(20260914)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("data/Allrounds_SPSS/processed", showWarnings = FALSE)
PL <- "output/pisa2025/plots"
VAL <- c("LVA", "CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL"); SAL <- setdiff(VAL, "LVA")
KODI <- c(LVA = 428000, EST = 233000, LTU = 440000, FIN = 246000, CZE = 203000, HUN = 348000, POL = 616000, DEU = 276000)
FAILI <- c(`2002` = "data/Allrounds_SPSS/HBSC2001OAed1.0_F4.sav", `2006` = "data/Allrounds_SPSS/HBSC2006OAed1.0_F1.sav",
           `2010` = "data/Allrounds_SPSS/HBSC2010OAed1.0_F4.sav", `2014` = "data/Allrounds_SPSS/HBSC2014OAed1.1_F1.sav",
           `2018` = "data/Allrounds_SPSS/2018/HBSC2018OAed1.1.sav")
KARTAS <- c(`2002` = "2001/02", `2006` = "2005/06", `2010` = "2009/10", `2014` = "2013/14", `2018` = "2017/18", `2022` = "2021/22")
# Starptautisko ziņojumu publicēšanas datumi (kā 67d HPUBL 2014–2022)
PUBL <- c(`2002` = "2004-06", `2006` = "2008-06", `2010` = "2012-05", `2014` = "2016-03", `2018` = "2020-05", `2022` = "2024-03")
num <- function(x) { x <- as.numeric(x); x }

# ---------------------------------------------------------------------------
# Rādītāju definīcijas (id kā HBSC Data Browser, kur tāds ir)
# ---------------------------------------------------------------------------
# zime: +1 lielāks = labāk, −1 lielāks = sliktāk (kā 67d HV)
RAD <- data.table(rbind(
  c("sch_satis", "patīk skola ļoti (%)", 1, "PERCENT"), c("sch_pressure", "skolas spiediens: diezgan vai ļoti (%)", -1, "PERCENT"),
  c("lifesat_mean", "dzīves apmierinātība (Kantrila kāpnes 0–10, vidējais)", 1, "MEAN_10"), c("sr_health", "pašvērtētā veselība teicama (%)", 1, "PERCENT"),
  c("multihealth", "≥ 2 psihosomatiskas sūdzības biežāk nekā reizi nedēļā (%)", -1, "PERCENT"), c("d_sleep", "miega grūtības biežāk nekā reizi nedēļā (%)", -1, "PERCENT"),
  c("d_feellow", "nomākts biežāk nekā reizi nedēļā (%)", -1, "PERCENT"), c("d_nervous", "nervozs biežāk nekā reizi nedēļā (%)", -1, "PERCENT"),
  c("d_headache", "galvassāpes biežāk nekā reizi nedēļā (%)", -1, "PERCENT"), c("d_irritable", "aizkaitināms biežāk nekā reizi nedēļā (%)", -1, "PERCENT"),
  c("beenbullied_2_3", "ņirgāšanās upuris ≥ 2–3 reizes mēnesī (%)", -1, "PERCENT"), c("bulliedothers_2_3", "ņirgājas par citiem ≥ 2–3 reizes mēnesī (%)", -1, "PERCENT"),
  c("studsup_cutoff", "klasesbiedru atbalsts ≥ 4 no 5 (%)", 1, "PERCENT"), c("teachersup_cutoff", "skolotāju atbalsts ≥ 4 no 5 (%)", 1, "PERCENT"),
  c("famsup_hi", "augsts ģimenes atbalsts ≥ 5,5 no 7 (%)", 1, "PERCENT"), c("peersup_hi", "augsts vienaudžu atbalsts ≥ 5,5 no 7 (%)", 1, "PERCENT"),
  c("talk_ma_easy", "viegli runāt ar māti (%)", 1, "PERCENT"), c("talk_pa_easy", "viegli runāt ar tēvu (%)", 1, "PERCENT"),
  c("break_wd", "brokastis katru darbdienu (%)", 1, "PERCENT"), c("MPVA_7", "≥ 60 min fiziski aktīvs 7 dienas nedēļā (%)", 1, "PERCENT"),
  c("VPA", "intensīva fiziskā aktivitāte ≥ 4 reizes nedēļā (%)", 1, "PERCENT"),
  c("tv_2h", "TV ≥ 2 h darbdienā (%)", -1, "PERCENT"), c("comp_2h", "dators ≥ 2 h darbdienā (%)", -1, "PERCENT"), c("game_2h", "datorspēles ≥ 2 h darbdienā (%)", -1, "PERCENT"),
  c("emedia_daily", "e-saziņa ar draugiem katru dienu (%)", -1, "PERCENT"), c("fas2_mean", "ģimenes labklājība FAS II (0–9, vidējais)", 1, "MEAN_9"),
  c("welloff_hi", "ģimene ļoti vai diezgan pārtikusi (%)", 1, "PERCENT"), c("acachieve_good", "pašvērtētās sekmes labas vai ļoti labas (%)", 1, "PERCENT"),
  c("hungry_any", "iet gulēt izsalcis vismaz dažreiz (%)", -1, "PERCENT")))
setnames(RAD, c("id", "nosaukums", "zime", "tips")); RAD[, zime := as.numeric(zime)]
# Iepriekš izvēlētais saliktais riska rādītājs (65b KEY; šeit + = labāk pēc zīmes)
KEY <- c("sch_pressure", "sch_satis", "teachersup_cutoff", "studsup_cutoff", "d_sleep", "lifesat_mean", "beenbullied_2_3", "peersup_hi", "famsup_hi", "multihealth", "d_feellow", "d_nervous")
PARL_ID <- c("sch_satis", "sch_pressure", "lifesat_mean", "sr_health", "multihealth", "d_sleep", "d_feellow", "d_nervous", "d_headache", "d_irritable", "beenbullied_2_3",
             "bulliedothers_2_3", "studsup_cutoff", "teachersup_cutoff", "famsup_hi", "peersup_hi", "talk_ma_easy", "talk_pa_easy", "break_wd", "MPVA_7", "VPA")
JOMAS <- list(`patika pret skolu` = "likeschool", `skolas spiediens` = "schoolpressure", `skolotāju atbalsts` = c("teacheraccept", "teachercare", "teachertust", "teachertrust"),
              `klasesbiedru atbalsts` = c("studtogether", "studhelpful", "studaccept"), `dzīves apmierinātība` = "lifesat", `pašvērtētā veselība` = "health",
              `psihosomatiskās sūdzības` = c("headache", "stomachache", "backache", "feellow", "irritable", "nervous", "sleepdifficulty", "sleepdificulty", "dizzy"),
              `ekrāna laiks` = c("tvwd", "tvwe", "compusewd", "compusewe", "playgamewd", "playgamewe"), `miegs/nogurums` = c("sleepdifficulty", "sleepdificulty"),
              brokastis = c("breakfastwd", "breakfastwe"), `fiziskās aktivitātes` = c("physact60", "physactweek", "physactavg", "timeexce", "timeexe", "hourexce"),
              `ģimenes labklājība (FAS)` = c("famcar", "fasfamcar", "bedroom", "fasbedroom", "holidays", "fasholidays", "computers", "fascomputers", "fasbathroom", "fasdishwash", "welloff", "IRFAS", "IRRELFAS_LMH", "hungry"),
              `vecāku komunikācija` = c("talkmother", "talkfather", "talkstepmo", "talkstepfa", "famhelp", "famsup", "famtalk", "famdec", "m78", "m79", "m80", "m81"),
              iebiedēšana = c("beenbullied", "bulliedothers", "cbeenbullied", "cbulliedothers", "cbullmess", "cbullpict", "fight12m"),
              `mājas valoda / tautība` = c("subregion", "id1", "REG_NO", "region", "m133", "m134", "m135", "countryborn", "countrybornmo", "countrybornfa"),
              `dizains` = c("countryno", "COUNTRYno", "schoolno", "classno", "cluster", "ClassID", "id2", "id3", "id4", "uniqueid", "UniqueID", "SEQNO", "seqno_int", "sampleweights", "M137", "weight", "monhtcollect", "yearcollect", "month", "year", "adm", "age", "AGE", "agecat", "AGECAT", "sex", "grade", "monthbirth", "yearbirth"))
joma_of <- function(v) { j <- names(JOMAS)[sapply(JOMAS, function(s) v %in% s)]; if (length(j)) j[1] else "" }

# ===========================================================================
cat(strrep("=", 100), "\n1. Inventārs: mainīgie, etiķetes, Latvijas n; saskaņotie rādītāji\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
KESS <- "data/Allrounds_SPSS/processed/hbsc_mikro_8valstis.rds"
PARLASIT <- !(file.exists(KESS) && file.exists("output/pisa2025/hbsc_mikro_inventars.csv") && file.exists("output/pisa2025/hbsc_mikro_n.csv"))
INV <- list(); NN <- list(); MIK <- list()
pct <- function(cond, valid) { z <- as.numeric(cond); z[!valid] <- NA; 100 * z }
if (!PARLASIT) cat("Kešs ir; SAV failus nelasa (dzēst", KESS, "lai pārlasītu).\n")
for (g in if (PARLASIT) names(FAILI) else character(0)) {
  cat("\n---", KARTAS[[g]], FAILI[[g]], "\n")
  d <- read_sav(FAILI[[g]])
  etik <- data.table(karta = KARTAS[[g]], gads = as.integer(g), mainigais = names(d), etikete = sapply(d, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }),
                     vertibu_etiketes = sapply(d, function(x) { l <- attr(x, "labels"); if (is.null(l)) "" else paste(paste0(l, "=", names(l)), collapse = "; ") }))
  d <- as.data.table(d); cn <- grep("^countryno$", names(d), ignore.case = TRUE, value = TRUE)
  d[, cnt := names(KODI)[match(num(get(cn)), KODI)]]; d <- d[!is.na(cnt)]
  ac <- grep("^agecat$", names(d), ignore.case = TRUE, value = TRUE)
  d[, vecums := c(11L, 13L, 15L)[num(get(ac))]][, sex := num(sex)]
  etik[, lv_n_derigi := sapply(mainigais, function(v) sum(!is.na(d[cnt == "LVA"][[v]])))]
  etik[, joma := sapply(mainigais, joma_of)]
  INV[[g]] <- etik
  cat("Valstis failā:", paste(sort(unique(d$cnt)), collapse = " "), "; mainīgo:", nrow(etik), "\n")
  # Dizains: svari, klasteri, datumi
  wv <- intersect(c("sampleweights", "M137", "weight"), names(d))[1]; d[, w := num(get(wv))]; d[is.na(w) | w <= 0, w := 1]
  sk <- intersect(c("schoolno", "id2"), names(d))[1]; kl <- intersect(c("cluster", "ClassID", "classno", "id3"), names(d))[1]
  d[, skola := paste(cnt, num(get(sk)))][, klase := paste(cnt, num(get(kl)))]
  d[, skola_ok := uniqueN(skola) > 1, by = cnt][skola_ok == FALSE, skola := klase]   # DEU 2001: skolas nav, tikai klase (arī viena); tad klase vai indivīds
  d[, skola_ok := uniqueN(skola) > 1, by = cnt][skola_ok == FALSE, skola := paste(cnt, seq_len(.N))]
  mo <- intersect(c("monhtcollect", "month"), names(d))[1]; yr <- intersect(c("yearcollect", "year"), names(d))[1]
  d[, dz := num(yearbirth) + (num(monthbirth) - 0.5) / 12]
  ag <- grep("^age$", names(d), ignore.case = TRUE, value = TRUE)
  d[is.na(dz), dz := num(get(yr)) + (num(get(mo)) - 0.5) / 12 - num(get(ag))]
  NN[[g]] <- d[, .(karta = KARTAS[[g]], gads = as.integer(g), n = .N, n_11 = sum(vecums == 11, na.rm = TRUE), n_13 = sum(vecums == 13, na.rm = TRUE), n_15 = sum(vecums == 15, na.rm = TRUE),
                   n_vecums_na = sum(is.na(vecums)), n_zeni = sum(sex == 1, na.rm = TRUE), n_meitenes = sum(sex == 2, na.rm = TRUE),
                   n_11_z = sum(vecums == 11 & sex == 1, na.rm = TRUE), n_11_m = sum(vecums == 11 & sex == 2, na.rm = TRUE), n_13_z = sum(vecums == 13 & sex == 1, na.rm = TRUE), n_13_m = sum(vecums == 13 & sex == 2, na.rm = TRUE),
                   n_15_z = sum(vecums == 15 & sex == 1, na.rm = TRUE), n_15_m = sum(vecums == 15 & sex == 2, na.rm = TRUE),
                   aptaujas_gadi = paste(sort(unique(num(get(yr)))), collapse = ","), aptaujas_menesi = paste(sort(unique(num(get(mo)))), collapse = ","),
                   svars = wv, svars_min = round(min(w), 3), svars_max = round(max(w), 3), svars_unikali = uniqueN(w), skolas = uniqueN(skola), klases = uniqueN(klase),
                   vid_vecums_11 = round(mean(num(get(ag))[vecums == 11], na.rm = TRUE), 2), dz_gads_11 = round(mean(dz[vecums == 11], na.rm = TRUE), 2),
                   dz_gads_13 = round(mean(dz[vecums == 13], na.rm = TRUE), 2), dz_gads_15 = round(mean(dz[vecums == 15], na.rm = TRUE), 2)), by = cnt]
  # Saskaņotie rādītāji
  V <- function(...) { for (v in c(...)) if (v %in% names(d)) return(num(d[[v]])); rep(NA_real_, nrow(d)) }
  inr <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi
  ls_ <- V("likeschool"); sp <- V("schoolpressure"); lf <- V("lifesat"); he <- V("health")
  compl <- lapply(c("headache", "stomachache", "backache", "feellow", "irritable", "nervous", "sleepdifficulty|sleepdificulty", "dizzy"), function(v) { x <- V(strsplit(v, "\\|")[[1]]); x[!inr(x, 1, 5)] <- NA; x })
  cm <- do.call(cbind, compl); n_ok <- rowSums(!is.na(cm)); n_freq <- rowSums(cm <= 2, na.rm = TRUE)
  st <- cbind(V("studtogether"), V("studhelpful"), V("studaccept")); st[!(st >= 1 & st <= 5)] <- NA; st_m <- rowMeans(6 - st)
  te <- cbind(V("teacheraccept"), V("teachercare"), V("teachertust", "teachertrust")); te[!(te >= 1 & te <= 5)] <- NA; te_m <- rowMeans(6 - te)
  fa <- cbind(V("famhelp"), V("famsup"), V("famtalk"), V("famdec")); fa[!(fa >= 1 & fa <= 7)] <- NA; fa_m <- rowMeans(fa)
  pe <- cbind(V("friendhelp"), V("friendcounton"), V("friendshare"), V("friendtalk")); pe[!(pe >= 1 & pe <= 7)] <- NA; pe_m <- rowMeans(pe)
  tm <- V("talkmother"); tp <- V("talkfather"); br <- V("breakfastwd"); pa <- V("physact60"); vp <- V("timeexce", "timeexe")
  tv <- V("tvwd"); co <- V("compusewd"); ga <- V("playgamewd"); em <- V("emedia6m")
  fc <- V("famcar", "fasfamcar"); fb <- V("bedroom", "fasbedroom"); fh <- V("holidays", "fasholidays"); fk <- V("computers", "fascomputers")
  fas <- (fc - 1) + (fb - 1) + (fh - 1) + (fk - 1); fas[!(inr(fc, 1, 3) & inr(fb, 1, 2) & inr(fh, 1, 4) & inr(fk, 1, 4))] <- NA
  wo <- V("welloff"); acx <- V("acachieve"); hu <- V("hungry")
  M <- d[, .(gads = as.integer(g), cnt, vecums, sex, w, skola, klase, dz)]
  M[, `:=`(sch_satis = pct(ls_ == 1, inr(ls_, 1, 4)), sch_pressure = pct(sp >= 3, inr(sp, 1, 4)), lifesat_mean = ifelse(inr(lf, 0, 10), lf, NA), sr_health = pct(he == 1, inr(he, 1, 4)),
           multihealth = pct(n_freq >= 2, n_ok >= 6), d_sleep = pct(compl[[7]] <= 2, !is.na(compl[[7]])), d_feellow = pct(compl[[4]] <= 2, !is.na(compl[[4]])),
           d_nervous = pct(compl[[6]] <= 2, !is.na(compl[[6]])), d_headache = pct(compl[[1]] <= 2, !is.na(compl[[1]])), d_irritable = pct(compl[[5]] <= 2, !is.na(compl[[5]])),
           beenbullied_2_3 = pct(V("beenbullied") >= 3, inr(V("beenbullied"), 1, 5)), bulliedothers_2_3 = pct(V("bulliedothers") >= 3, inr(V("bulliedothers"), 1, 5)),
           studsup_cutoff = pct(st_m >= 4, !is.na(st_m)), teachersup_cutoff = pct(te_m >= 4, !is.na(te_m)), famsup_hi = pct(fa_m >= 5.5, !is.na(fa_m)), peersup_hi = pct(pe_m >= 5.5, !is.na(pe_m)),
           talk_ma_easy = pct(tm <= 2, inr(tm, 1, 4)), talk_pa_easy = pct(tp <= 2, inr(tp, 1, 4)), break_wd = pct(br == 6, inr(br, 1, 6)), MPVA_7 = pct(pa == 7, inr(pa, 0, 7)),
           VPA = pct(vp <= 2, inr(vp, 1, 7)), tv_2h = pct(tv >= 4, inr(tv, 1, 9)), comp_2h = pct(co >= 4, inr(co, 1, 9)), game_2h = pct(ga >= 4, inr(ga, 1, 9)),
           emedia_daily = pct(em == 5, inr(em, 1, 5)), fas2_mean = fas, welloff_hi = pct(wo <= 2, inr(wo, 1, 5)), acachieve_good = pct(acx <= 2, inr(acx, 1, 4)), hungry_any = pct(hu <= 3, inr(hu, 1, 4)))]
  MIK[[g]] <- M; rm(d, M, cm, st, te, fa, pe); invisible(gc(verbose = FALSE))
  cat("Latvija n:", NN[[g]][cnt == "LVA", n], " (11/13/15:", NN[[g]][cnt == "LVA", paste(n_11, n_13, n_15)], ")\n")
}
if (PARLASIT) {
  INV <- rbindlist(INV); NN <- rbindlist(NN); MIK <- rbindlist(MIK, fill = TRUE)
  saveRDS(MIK, KESS); cat("\nKešs (mikrodati, tikai lokāli, nenodot):", KESS, nrow(MIK), "rindas\n")
  # Inventārs garā formā: kārta × mainīgais (visu kārtu apvienojums) × pieejamība
  VISI <- unique(INV$mainigais)
  INVL <- CJ(gads = as.integer(names(FAILI)), mainigais = VISI)
  INVL <- merge(INVL, INV[, .(gads, mainigais, pieejams = 1L, etikete, lv_n_derigi, joma, vertibu_etiketes)], by = c("gads", "mainigais"), all.x = TRUE)
  INVL[is.na(pieejams), `:=`(pieejams = 0L, etikete = "", lv_n_derigi = 0L, vertibu_etiketes = "")]
  INVL[, joma := sapply(mainigais, joma_of)][, karta := KARTAS[as.character(gads)]]
  setcolorder(INVL, c("karta", "gads", "mainigais", "pieejams", "etikete", "joma", "lv_n_derigi", "vertibu_etiketes"))
} else {
  MIK <- readRDS(KESS); NN <- fread("output/pisa2025/hbsc_mikro_n.csv")
  INVL <- fread("output/pisa2025/hbsc_mikro_inventars.csv", encoding = "UTF-8")[joma != "saskaņotais rādītājs (65c)"]
  INV <- INVL[pieejams == 1]
}
# Saskaņotie rādītāji: pieejamība pa kārtām (no mikrodatiem) + pārlūks
PIEEJ <- MIK[, lapply(.SD, function(x) as.integer(any(!is.na(x)))), by = gads, .SDcols = RAD$id]
PIEEJ <- melt(PIEEJ, id.vars = "gads", variable.name = "mainigais", value.name = "pieejams")[, mainigais := as.character(mainigais)]
PIEEJ <- merge(PIEEJ, RAD[, .(mainigais = id, etikete = paste("SASKAŅOTS RĀDĪTĀJS:", nosaukums))], by = "mainigais")
PIEEJ[, `:=`(karta = KARTAS[as.character(gads)], joma = "saskaņotais rādītājs (65c)", lv_n_derigi = NA_integer_, vertibu_etiketes = fifelse(mainigais %in% PARL_ID, "pārlūkā 2014/2018/2022 tāda pati definīcija", ""))]
fwrite(rbind(INVL, PIEEJ[, names(INVL), with = FALSE])[order(-pieejams, joma, mainigais, gads)], "output/pisa2025/hbsc_mikro_inventars.csv")
fwrite(NN[order(gads, cnt)], "output/pisa2025/hbsc_mikro_n.csv")
cat("\nMainīgo skaits pa kārtām:\n"); print(INV[, .(mainigie = .N, ar_etiketi = sum(etikete != "")), by = karta], row.names = FALSE)
cat("\nAtslēgas jomu mainīgie pa kārtām (pieejams = 1):\n")
print(dcast(INVL[joma != "" & joma != "dizains", .(p = sum(pieejams)), by = .(joma, gads)], joma ~ gads, value.var = "p"), row.names = FALSE)
cat("\nSaskaņoto rādītāju pieejamība (1 = ir mikrodatos):\n"); print(dcast(PIEEJ, mainigais ~ gads, value.var = "pieejams"), row.names = FALSE)
cat("\nLatvijas n pa kārtām, vecuma grupām un dzimumiem:\n")
print(NN[cnt == "LVA", .(karta, n, n_11, n_13, n_15, n_11_z, n_11_m, n_13_z, n_13_m, n_15_z, n_15_m, aptaujas_gadi, aptaujas_menesi, svars, svars_unikali, skolas, klases, vid_vecums_11, dz_gads_11, dz_gads_13, dz_gads_15)], row.names = FALSE)
cat("\nValstu n pa kārtām (kopā):\n"); print(dcast(NN, cnt ~ gads, value.var = "n"), row.names = FALSE)
cat("Svari (unikālu vērtību skaits; 1 = nesvērts):\n"); print(dcast(NN, cnt ~ gads, value.var = "svars_unikali"), row.names = FALSE)
cat("\nValoda / tautība: skat. skripta galveni; Latvijā nav šķirama.\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n2. Apkopojumi: valsts × kārta × vecums × rādītājs (svērts vidējais, skolu klasteru SE)\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
wmean_se <- function(x, w, cl) {
  i <- !is.na(x) & !is.na(w) & w > 0; x <- x[i]; w <- w[i]; cl <- cl[i]
  if (length(x) < 100) return(c(NA_real_, NA_real_, length(x)))
  m <- sum(w * x) / sum(w); us <- tapply(w * (x - m), cl, sum); G <- length(us)
  if (G < 2) return(c(m, sqrt(sum(w^2 * (x - m)^2)) / sum(w), length(x)))
  c(m, sqrt(G / (G - 1) * sum(us^2)) / sum(w), length(x))
}
MIK <- MIK[!is.na(vecums)]
AG <- MIK[, { r <- rbindlist(lapply(RAD$id, function(v) { e <- wmean_se(get(v), w, skola); if (is.na(e[1])) NULL else data.table(HBSC_id = v, est = e[1], se = e[2], n = as.integer(e[3])) }))
  if (nrow(r)) r[, `:=`(dz_gads = mean(dz, na.rm = TRUE), skolas = uniqueN(skola), avots = "HBSC mikrodati")] else NULL }, by = .(gads, cnt, vecums)]
cat("Mikrodatu apkopojumu:", nrow(AG), "; sērijas (rādītājs × vecums):", uniqueN(AG[, paste(HBSC_id, vecums)]), "\n")

# Pārlūks: 2022 visām valstīm un 2014 Lietuvai (mikrodatos ed. 1.1 Lietuvas nav)
HB <- fread("data/hbsc/hbsc_simptomi_8valstis.csv", encoding = "UTF-8")[cnt %in% VAL & !is.na(kopa)][, gads := as.integer(gads)]
HB <- HB[HBSC_id %in% PARL_ID & (gads == 2022 | (gads == 2014 & cnt == "LTU"))]
HB[, se := fifelse(Measure_type == "MEAN_10", 1.8 / sqrt(1500), 100 * sqrt(pmin(pmax(kopa, 1), 99) / 100 * (1 - pmin(pmax(kopa, 1), 99) / 100) / 1500))]
HBp <- HB[, .(gads, cnt, vecums, HBSC_id, est = kopa, se, n = NA_integer_, dz_gads = gads - vecums - 0.5, skolas = NA_integer_, avots = "HBSC Data Browser")]
cat("Pārlūka rindas: 2022:", nrow(HBp[gads == 2022]), "; 2014 LTU:", nrow(HBp[gads == 2014]), "\n")
# Salīdzināmība: mikrodatu 2014/2018 pret pārlūka 2014/2018 (tie paši rādītāji, tā pati definīcija)
HBv <- fread("data/hbsc/hbsc_simptomi_8valstis.csv", encoding = "UTF-8")[cnt %in% VAL & !is.na(kopa) & HBSC_id %in% PARL_ID][, gads := as.integer(gads)]
SAL <- merge(AG[gads %in% c(2014, 2018), .(gads, cnt, vecums, HBSC_id, mikro = est)], HBv[, .(gads, cnt, vecums, HBSC_id, parluks = kopa)], by = c("gads", "cnt", "vecums", "HBSC_id"))
cat("\nMikrodatu un pārlūka salīdzinājums 2014/2018 (", nrow(SAL), " punkti): korelācija ", round(cor(SAL$mikro, SAL$parluks), 3), ", vidējā absolūtā starpība ",
    round(mean(abs(SAL$mikro - SAL$parluks)), 2), "\n", sep = "")
print(SAL[, .(n = .N, r = round(cor(mikro, parluks), 3), vid_starp = round(mean(mikro - parluks), 2), abs_starp = round(mean(abs(mikro - parluks)), 2)), by = HBSC_id][order(-abs_starp)], row.names = FALSE)

A <- rbind(AG, HBp)
A <- merge(A, RAD[, .(HBSC_id = id, nosaukums, zime, tips)], by = "HBSC_id")
# Relatīvās vērtības un standartizācija (sērija = rādītājs × vecums)
A[, n_valstis := .N, by = .(HBSC_id, vecums, gads)]
A[, rel8 := est - mean(est), by = .(HBSC_id, vecums, gads)]
A[, rel7 := est - (sum(est) - est) / (.N - 1), by = .(HBSC_id, vecums, gads)]
A[n_valstis < 5, `:=`(rel8 = NA_real_, rel7 = NA_real_)]
A[, sd_rel := sd(rel8, na.rm = TRUE), by = .(HBSC_id, vecums)]
A[, `:=`(z = zime * rel8 / sd_rel, z7 = zime * rel7 / sd_rel, se_z = se / sd_rel)]
A[, b := floor(dz_gads + 0.5)]
OUT <- A[, .(bloks = "iesaiste", avots = fifelse(avots == "HBSC mikrodati", "HBSC iesaiste (mikrodati)", "HBSC iesaiste (pārlūks)"), serija = paste0("HBSC: ", nosaukums, " ", vecums, " g."),
             grupa = cnt, gads, est, se, dz_gads, publ = PUBL[as.character(gads)], zime, HBSC_id, vecums, n, skolas, b, n_valstis, rel8, rel7, sd_rel, z, z7, se_z, tips, karta = KARTAS[as.character(gads)])]
fwrite(OUT[order(HBSC_id, vecums, gads, grupa)], "output/pisa2025/hbsc_mikro_paaudzes.csv")
cat("\nhbsc_mikro_paaudzes.csv:", nrow(OUT), "rindas;", uniqueN(OUT$serija), "sērijas; dzimšanas gadi", paste(range(OUT$b), collapse = "–"), "\n")
print(OUT[, .(punkti = .N, serijas = uniqueN(serija), valstis = uniqueN(grupa), dz_gadi = paste(range(b), collapse = "–")), by = .(avots, gads)][order(gads)], row.names = FALSE)
cat("\nLatvija: dzimšanas gads (vidējais no dzimšanas datiem) pa kārtām un vecumiem:\n")
print(dcast(unique(OUT[grupa == "LVA", .(gads, vecums, dz = round(dz_gads, 1))]), vecums ~ gads, value.var = "dz"), row.names = FALSE)

# ===========================================================================
cat("\n", strrep("=", 100), "\n3a. Latvija relatīvi pret 7 valstīm pa paaudzēm: līmeņi un lūzuma gadi\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
# Saliktais rādītājs: KEY rādītāju z7 vidējais (visi ar zīmi: + = labāk), SE kā vidējā SE (konservatīvi: pieņem pilnu korelāciju)
KOMP <- OUT[HBSC_id %in% KEY & !is.na(z7), .(HBSC_id = "saliktais", z7 = mean(z7), z = mean(z), se_z = mean(se_z), rad = .N, dz_gads = dz_gads[1], b = b[1], avots = avots[1]), by = .(grupa, gads, vecums)]
LV <- rbind(OUT[grupa == "LVA" & !is.na(z7), .(grupa, gads, vecums, HBSC_id, z7, z, se_z, dz_gads, b, avots)], KOMP[grupa == "LVA", .(grupa, gads, vecums, HBSC_id, z7, z, se_z, dz_gads, b, avots)])
cat("Latvija z (pret 7 valstīm, + = labāk) pa paaudzēm; galvenie rādītāji:\n")
for (v in c("sch_satis", "sch_pressure", "lifesat_mean", "saliktais")) {
  cat("\n ", v, ":\n"); print(dcast(LV[HBSC_id == v, .(vecums, gads, v = sprintf("%.2f (b %d)", z7, b))], vecums ~ gads, value.var = "v"), row.names = FALSE)
}
cat("\nSaliktā rādītāja komponentu skaits pa kārtām (Latvija, 11 g.):", paste(KOMP[grupa == "LVA" & vecums == 11, paste0(gads, ":", rad)], collapse = " "), "\n")

# Lūzuma gads: viena pārrāvuma taisne pa dzimšanas gadu (visi vecumi kopā, svērts ar 1/se²)
luzums <- function(D, B = 1000) {
  D <- D[order(dz_gads)]; x <- D$dz_gads; y <- D$z7; w <- 1 / (D$se_z^2 + 0.01)
  ks <- sort(unique(round(x))); ks <- ks[sapply(ks, function(k) sum(x < k) >= 3 & sum(x >= k) >= 3)]
  if (length(ks) < 2) return(NULL)
  fit <- function(y) { rss <- sapply(ks, function(k) { X <- cbind(1, x - k, pmax(x - k, 0)); f <- lm.wfit(X, y, w); sum(w * f$residuals^2) })
    k <- ks[which.min(rss)]; X <- cbind(1, x - k, pmax(x - k, 0)); cf <- lm.wfit(X, y, w)$coefficients; c(k = k, pirms = cf[2], pec = cf[2] + cf[3]) }
  f0 <- fit(y); kb <- replicate(B, fit(y + rnorm(length(y), 0, D$se_z))[1])
  lin <- summary(lm(y ~ x, weights = w))$coefficients   # lineārais slīpums ar SE (ja lūzums ir rindas galā, tas ir robežas artefakts)
  data.table(luzums = f0[1], slipums_pirms = f0[2], slipums_pec = f0[3], luzums_q025 = quantile(kb, 0.025), luzums_q975 = quantile(kb, 0.975), punkti = length(x),
             maks_gads = D$b[which.max(D$z7)], min_gads = D$b[which.min(D$z7)], lin_slipums = lin[2, 1], lin_se = lin[2, 2], k_min = min(ks), k_max = max(ks))
}
LUZ <- rbindlist(lapply(c("saliktais", "sch_satis", "sch_pressure", "lifesat_mean", "teachersup_cutoff", "studsup_cutoff", "d_sleep", "multihealth", "beenbullied_2_3", "famsup_hi", "peersup_hi", "d_feellow", "d_nervous", "sr_health", "fas2_mean", "acachieve_good", "tv_2h", "comp_2h"),
                        function(v) { r <- luzums(LV[HBSC_id == v]); if (is.null(r)) NULL else r[, HBSC_id := v] }))
# Arī Igaunija un visa kopa (kontrasts kā docs/73)
LVE <- rbind(OUT[grupa == "EST" & !is.na(z7), .(grupa, gads, vecums, HBSC_id, z7, z, se_z, dz_gads, b, avots)], KOMP[grupa == "EST", .(grupa, gads, vecums, HBSC_id, z7, z, se_z, dz_gads, b, avots)])
LUZE <- rbindlist(lapply(c("saliktais", "sch_satis", "sch_pressure", "lifesat_mean"), function(v) { r <- luzums(LVE[HBSC_id == v]); if (is.null(r)) NULL else r[, HBSC_id := v] }))
LUZ <- rbind(LUZ[, grupa := "LVA"], LUZE[, grupa := "EST"])
fwrite(LUZ, "output/pisa2025/hbsc_mikro_luzumi.csv")
cat("\nLūzuma gadi (viena pārrāvuma taisne z7 ~ dzimšanas gads, visi vecumi kopā; 95 % no parametriskā bootstrap; slīpums z gadā):\n")
print(LUZ[, .(grupa, HBSC_id, luzums, q025 = luzums_q025, q975 = luzums_q975, pirms = round(slipums_pirms, 3), pec = round(slipums_pec, 3), punkti, maks_gads, min_gads,
              lin = round(lin_slipums, 3), lin_se = round(lin_se, 3), k_diapazons = paste(k_min, k_max, sep = "–"))], row.names = FALSE)
cat("Lūzums, kas sakrīt ar k diapazona galu, ir robežas artefakts (nav īsta lūzuma); tad skatās lineāro slīpumu (lin, z gadā) un tā SE.\n")
cat("Salīdzinājumam docs/73 (67d, LVA A): vide lūzums 2001. g. paaudze, prasme 2007. g. paaudze, iesaiste 2006 (kāpj).\n")

# Papildus: vidējais z7 pa dzimšanas gada joslām (visi vecumi), Latvija un katra salīdzinājuma valsts, saliktais
cat("\nSaliktais rādītājs pa dzimšanas gadu joslām (vidējais pār vecumiem un kārtām; z pret pārējām 7):\n")
KJ <- KOMP[!is.na(z7)][, josla := cut(dz_gads, c(1985, 1990, 1994, 1998, 2002, 2006, 2012), labels = c("1986–89", "1990–93", "1994–97", "1998–2001", "2002–05", "2006–11"), right = FALSE)]
print(dcast(KJ[, .(z = round(mean(z7), 2)), by = .(grupa, josla)], grupa ~ josla, value.var = "z"), row.names = FALSE)

# ===========================================================================
cat("\n", strrep("=", 100), "\n3b. PISA paaudzes (dz. 2002, 2006, 2009): Latvija 11 un 13 gados pirms PISA; ex ante korelācija (65b paplašināts)\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
P <- fread("output/pisa2025/pisa_valstu_videjie.csv", colClasses = c(gads = "character"))[cnt %in% VAL, .(gads = as.integer(gads), cnt, y = lasisana)]
P[, rel := y - (sum(y) - y) / (.N - 1), by = gads]
PISA_PA <- data.table(pisa = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))[, dz_pisa := pisa - 15.8]
# HBSC mērījumi pirms PISA tai pašai paaudzei (|dz starpība| ≤ 1,5 gadi un mērījums pirms PISA)
LVP <- LV[HBSC_id %in% c(KEY, "saliktais", "sr_health", "fas2_mean", "acachieve_good")]
EXA <- rbindlist(lapply(seq_len(nrow(PISA_PA)), function(i) { p <- PISA_PA[i]; x <- LVP[abs(dz_gads - p$dz_pisa) <= 1.5 & gads <= p$pisa & vecums < 15]
  if (nrow(x)) x[, .(pisa = p$pisa, dz_pisa = round(p$dz_pisa, 1), HBSC_id, hbsc_gads = gads, vecums, dz_hbsc = round(dz_gads, 1), z7 = round(z7, 2), se_z = round(se_z, 2), avots)] else NULL }))
EXA <- merge(EXA, P[cnt == "LVA", .(pisa = gads, pisa_lva = round(y, 1), pisa_rel = round(rel, 1))], by = "pisa", all.x = TRUE)
fwrite(EXA, "output/pisa2025/hbsc_mikro_pisa_paaudzes.csv")
cat("Latvija: HBSC z (pret 7 valstīm, + = labāk) 11 un 13 gadu vecumā tai pašai paaudzei pirms PISA (saliktais un galvenie):\n")
print(dcast(EXA[HBSC_id %in% c("saliktais", "sch_satis", "sch_pressure", "lifesat_mean", "acachieve_good")], HBSC_id + vecums ~ pisa, value.var = "z7", fun.aggregate = function(z) round(mean(z), 2)), row.names = FALSE)
cat("PISA Latvija lasīšana relatīvi pret 7 valstīm:", paste(P[cnt == "LVA"][order(gads), paste0(gads, ": ", round(rel, 1))], collapse = "; "), "\n")
cat("\nPISA 2018/2022/2025 paaudzes (dz. ~2002, ~2006, ~2009), visi KEY rādītāji 11 un 13 gados:\n")
print(dcast(EXA[pisa >= 2018 & HBSC_id != "saliktais"], HBSC_id ~ pisa + vecums, value.var = "z7", fun.aggregate = function(z) round(mean(z), 2)), row.names = FALSE)

# Ex ante korelācija starp valstīm (65b dizains, vairāk notikumu)
dP <- function(g1, g2) merge(P[gads == g1, .(cnt, r1 = rel)], P[gads == g2, .(cnt, r2 = rel)], by = "cnt")[, .(cnt, pisa_drel = r2 - r1)]
NOT <- list(
  list(nos = "ex ante: 13 g. 2002→2006 ↔ PISA 2003→2009", kopa = "ex ante (paaudze)", p = c(2003, 2009), h1 = c(2002, 13), h2 = c(2006, 13)),
  list(nos = "ex ante: 13 g. 2006→2010 ↔ PISA 2009→2012", kopa = "ex ante (paaudze)", p = c(2009, 2012), h1 = c(2006, 13), h2 = c(2010, 13)),
  list(nos = "ex ante: 11 g. 2010→2014 ↔ PISA 2015→2018", kopa = "ex ante (paaudze)", p = c(2015, 2018), h1 = c(2010, 11), h2 = c(2014, 11)),
  list(nos = "ex ante: 11 g. 2014→2018 ↔ PISA 2018→2022", kopa = "ex ante (paaudze)", p = c(2018, 2022), h1 = c(2014, 11), h2 = c(2018, 11)),
  list(nos = "ex ante: 11 g. 2018 → 13 g. 2022 ↔ PISA 2022→2025", kopa = "ex ante (paaudze)", p = c(2022, 2025), h1 = c(2018, 11), h2 = c(2022, 13)),
  list(nos = "vienlaicīgi: 15 g. 2006→2010 ↔ PISA 2006→2009", kopa = "vienlaicīgi", p = c(2006, 2009), h1 = c(2006, 15), h2 = c(2010, 15)),
  list(nos = "vienlaicīgi: 15 g. 2010→2014 ↔ PISA 2009→2015", kopa = "vienlaicīgi", p = c(2009, 2015), h1 = c(2010, 15), h2 = c(2014, 15)),
  list(nos = "vienlaicīgi: 15 g. 2014→2018 ↔ PISA 2015→2018", kopa = "vienlaicīgi", p = c(2015, 2018), h1 = c(2014, 15), h2 = c(2018, 15)),
  list(nos = "vienlaicīgi: 15 g. 2018→2022 ↔ PISA 2018→2022", kopa = "vienlaicīgi", p = c(2018, 2022), h1 = c(2018, 15), h2 = c(2022, 15)))
ZZ <- rbind(OUT[!is.na(z7), .(HBSC_id, cnt = grupa, gads, vecums, z7)], KOMP[!is.na(z7), .(HBSC_id, cnt = grupa, gads, vecums, z7)])
O <- rbindlist(lapply(NOT, function(e) {
  a <- ZZ[gads == e$h1[1] & vecums == e$h1[2], .(HBSC_id, cnt, z1 = z7)]; b <- ZZ[gads == e$h2[1] & vecums == e$h2[2], .(HBSC_id, cnt, z2 = z7)]
  merge(merge(a, b, by = c("HBSC_id", "cnt"))[, hbsc_dz := z2 - z1], dP(e$p[1], e$p[2]), by = "cnt")[, `:=`(notikums = e$nos, kopa = e$kopa)] }))
perm_kor <- function(x, y, grp, B = 10000) { r0 <- cor(x, y)
  rb <- replicate(B, { yy <- y; for (g in unique(grp)) { i <- which(grp == g); yy[i] <- y[i][sample(length(i))] }; cor(x, yy) }); c(r = r0, p = mean(abs(rb) >= abs(r0) - 1e-12)) }
KOR <- O[, { if (.N < 10 || sd(hbsc_dz) == 0) NULL else { pk <- perm_kor(hbsc_dz, pisa_drel, notikums); .(n = .N, notikumi = uniqueN(notikums), r = pk[["r"]], rho = cor(hbsc_dz, pisa_drel, method = "spearman"), p = pk[["p"]]) } }, by = .(kopa, HBSC_id)]
KOR[, q := p.adjust(p, "BH"), by = kopa]
fwrite(rbind(KOR, fill = TRUE), "output/pisa2025/hbsc_mikro_ex_ante.csv"); fwrite(O, "output/pisa2025/hbsc_mikro_ex_ante_dati.csv")
cat("\nHBSC z pārmaiņa (paaudze pret iepriekšējo, + = labāk) ↔ PISA relatīvā pārmaiņa; 8 valstis × notikumi; permutāciju p (valstis sajauktas notikuma ietvaros), BH q:\n")
for (k in unique(KOR$kopa)) { x <- KOR[kopa == k]; cat(sprintf("\n%s: %d rādītāji, p < 0,05: %d, BH q < 0,10: %d\n", k, nrow(x), sum(x$p < 0.05), sum(x$q < 0.10)))
  print(x[order(p), .(HBSC_id, n, notikumi, r = round(r, 2), rho = round(rho, 2), p = signif(p, 2), q = signif(q, 2))], row.names = FALSE) }
cat("\nSaliktais rādītājs (KEY vidējais z): Latvijas HBSC pārmaiņa un PISA relatīvā pārmaiņa pa notikumiem:\n")
print(O[HBSC_id == "saliktais" & cnt == "LVA", .(notikums, hbsc_dz = round(hbsc_dz, 2), pisa_drel = round(pisa_drel, 1))], row.names = FALSE)
cat("\nSalīdzinājums ar 65b (tikai pārlūks, 2 ex ante notikumi, 16 punkti): tur saliktā riska r = +0,22 (p = 0,45), neviens rādītājs pēc BH.\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n3c. Valodas plūsmas\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
cat("Atvērtās piekļuves failos Latvijai nav aptaujas valodas, mājas valodas vai tautības mainīgā (skat. galveni un docs/78).\n",
    "2006. gada subregion (3 strati: 56, 21 un 10 skolas) varētu būt valoda vai reģions, bet dokumentācijā tas nav aprakstīts;\n",
    "bez apstiprinājuma tas netiek lietots. Krievu valodas plūsmas trajektoriju no šiem datiem atsevišķi noteikt nevar.\n", sep = "")

# ===========================================================================
cat("\n", strrep("=", 100), "\n4. Grafiki\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
TEMA <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text(face = "bold"),
                                              plot.caption = element_text(colour = "grey40", hjust = 0), legend.position = "bottom")
zim <- function(p, nos, w = 11, h = 7) { f <- file.path(PL, paste0(nos, ".png")); ggsave(f, p, width = w, height = h, dpi = 200, bg = "white"); cat("Saglabāts:", f, "\n") }
NOSL <- c(sch_satis = "Patīk skola ļoti", sch_pressure = "Skolas spiediens (zīme pagriezta: + = mazāk)", lifesat_mean = "Dzīves apmierinātība", saliktais = "Saliktais rādītājs (12 KEY rādītāju z vidējais)")
LUZL <- data.table(x = c(2001, 2007), et = c("67d vide 2001", "67d prasme 2007"))
av <- function(a) fifelse(grepl("mikro", a), "HBSC mikrodati", "HBSC Data Browser")
D1 <- rbind(LV, LVE)[HBSC_id %in% names(NOSL)][, `:=`(rad = factor(NOSL[HBSC_id], NOSL), vec = factor(paste(vecums, "g."), c("11 g.", "13 g.", "15 g.")), grupa = fifelse(grupa == "LVA", "Latvija", "Igaunija"), avots = av(avots))]
p1 <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(data = LUZL, aes(xintercept = x), linetype = "dashed", colour = "#b3261e", linewidth = 0.4) +
  geom_text(data = LUZL, aes(x = x, y = 1.9, label = et), colour = "#b3261e", size = 2.6, hjust = 0, nudge_x = 0.2) +
  geom_line(data = D1[grupa == "Igaunija"], aes(dz_gads, z7, group = vec, colour = vec), linewidth = 0.4, alpha = 0.4, linetype = "dotted") +
  geom_ribbon(data = D1[grupa == "Latvija"], aes(dz_gads, ymin = z7 - 1.96 * se_z, ymax = z7 + 1.96 * se_z, group = vec, fill = vec), alpha = 0.12) +
  geom_line(data = D1[grupa == "Latvija"], aes(dz_gads, z7, colour = vec), linewidth = 0.7) +
  geom_point(data = D1[grupa == "Latvija"], aes(dz_gads, z7, colour = vec, shape = avots), size = 2.4, fill = "white") +
  scale_shape_manual(values = c("HBSC mikrodati" = 16, "HBSC Data Browser" = 21), name = NULL) +
  scale_colour_manual(values = c("11 g." = "#2a78d6", "13 g." = "#0f8a6e", "15 g." = "#eb6834"), name = "Vecums (Latvija; Igaunija punktēta)") + scale_fill_manual(values = c("11 g." = "#2a78d6", "13 g." = "#0f8a6e", "15 g." = "#eb6834"), guide = "none") +
  scale_x_continuous("Dzimšanas gads (paaudze)", breaks = seq(1986, 2012, 2), sec.axis = sec_axis(~ . + 16, name = "PISA gads (dzimšanas gads + 16)", breaks = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))) +
  coord_cartesian(xlim = c(1985.5, 2011.5), ylim = c(-2.2, 2.2)) + facet_wrap(~ rad, ncol = 2) +
  labs(y = "z pret 7 salīdzinājuma valstu vidējo (+ = labāk)", title = "HBSC mikrodati: Latvijas pusaudžu skolas patika, spiediens un dzīves apmierinātība pa paaudzēm",
       subtitle = "HBSC 2001/02–2017/18 mikrodati un 2021/22 pārlūks; katrs punkts = paaudze × vecums; josla = 95 % (skolu klasteru SE); sarkanās līnijas = 67d stāvokļu lūzumi (docs/73)",
       caption = "Avots: HBSC atvērtās piekļuves mikrodati (HBSC Data Management Centre, Bergenas Universitāte; HBSC2001OAed1.0 … HBSC2018OAed1.1) un HBSC Data Browser 2022.\nVisa Latvija, bez valodas plūsmām; Igaunija punktēta salīdzinājumam. Skripts 65c, docs/78.") + TEMA
zim(p1, "65c_latvija_paaudzes", 14, 9)

# Lexis: saliktais rādītājs, x dzimšanas gads, y vecums, krāsa z; diagonāles = aptaujas gadi
D2 <- KOMP[grupa == "LVA" & !is.na(z7)][, avots := av(avots)]
DG <- data.table(g = c(2002, 2006, 2010, 2014, 2018, 2022))[, .(x = c(g - 16, g - 10), y = c(16, 10)), by = g]
p2 <- ggplot() + geom_line(data = DG, aes(x, y, group = g), colour = "grey75", linewidth = 0.3) +
  geom_text(data = DG[y == 16], aes(x, y, label = paste("aptauja", KARTAS[as.character(g)])), colour = "grey50", size = 2.5, hjust = 1.05, vjust = -0.2, angle = -45) +
  geom_line(data = D2[, .SD[.N >= 2], by = b], aes(dz_gads, vecums, group = b), colour = "grey40", linewidth = 0.3) +
  geom_point(data = D2, aes(dz_gads, vecums, fill = z7, shape = avots), size = 7, colour = "black", stroke = 0.4) + geom_text(data = D2, aes(dz_gads, vecums, label = sprintf("%.1f", z7)), size = 2.3) +
  scale_shape_manual(values = c("HBSC mikrodati" = 21, "HBSC Data Browser" = 22), name = NULL) +
  scale_fill_gradient2(low = "#b3261e", mid = "white", high = "#2a78d6", midpoint = 0, limits = c(-1.5, 1.5), oob = scales::squish, name = "Saliktais z pret 7 valstīm (+ = labāk)") +
  geom_vline(data = LUZL, aes(xintercept = x), linetype = "dashed", colour = "#b3261e", linewidth = 0.4) +
  scale_x_continuous("Dzimšanas gads (paaudze)", breaks = seq(1986, 2012, 2), sec.axis = sec_axis(~ . + 16, name = "PISA gads (dzimšanas gads + 16)", breaks = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))) +
  scale_y_continuous("Vecums HBSC aptaujā", breaks = c(11, 13, 15)) + coord_cartesian(xlim = c(1985, 2012), ylim = c(9.5, 16.5)) +
  labs(title = "Paaudžu diagramma: Latvijas saliktais HBSC labbūtības un skolas vides rādītājs katrā mērījumā", subtitle = "Punkts = paaudze × vecums (dzimšanas gads no dzimšanas datiem); diagonāles = aptaujas kārtas; sarkanās līnijas = 67d lūzumi (vide 2001, prasme 2007)",
       caption = "Avots: HBSC mikrodati 2001/02–2017/18 un pārlūks 2021/22; 12 KEY rādītāji kā 65b. Skripts 65c, docs/78.") + TEMA + theme(legend.position = "right")
zim(p2, "65c_lexis_saliktais", 13, 6.5)

# PISA paaudzes: KEY rādītāji 11 un 13 gados
D3 <- EXA[pisa >= 2015 & HBSC_id != "saliktais"][, `:=`(rad = HBSC_id, pa = factor(paste0("PISA ", pisa, " paaudze (dz. ~", round(dz_pisa), ")")), vec = factor(paste(vecums, "g.")), avots = av(avots))]
p3 <- ggplot(D3, aes(z7, rad, colour = vec, shape = avots)) + geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = z7 - 1.96 * se_z, xmax = z7 + 1.96 * se_z), width = 0, alpha = 0.5, position = position_dodge(0.5), orientation = "y") + geom_point(size = 2.6, position = position_dodge(0.5), fill = "white") +
  scale_shape_manual(values = c("HBSC mikrodati" = 16, "HBSC Data Browser" = 21), name = NULL) + scale_colour_manual(values = c("11 g." = "#2a78d6", "13 g." = "#0f8a6e"), name = "Vecums HBSC aptaujā") +
  facet_wrap(~ pa, nrow = 1) + labs(x = "z pret 7 salīdzinājuma valstu vidējo (+ = labāk)", y = NULL, title = "Ex ante: PISA paaudžu HBSC rādītāji 11 un 13 gadu vecumā pirms PISA (Latvija)",
                                    subtitle = "Latvijas relatīvā vieta tai pašai dzimšanas paaudzei 4–5 gadus pirms PISA; 95 % intervāls", caption = "Avots: HBSC mikrodati un pārlūks; skripts 65c, docs/78.") + TEMA
zim(p3, "65c_pisa_paaudzes_ex_ante", 13, 6.5)

# Ex ante korelācija: saliktais
D4 <- O[HBSC_id == "saliktais"]
D4l <- D4[, .(et = sprintf("r = %.2f", cor(hbsc_dz, pisa_drel))), by = .(kopa, notikums)]
p4 <- ggplot(D4, aes(hbsc_dz, pisa_drel)) + geom_hline(yintercept = 0, colour = "grey70") + geom_vline(xintercept = 0, colour = "grey70") +
  geom_smooth(method = "lm", se = FALSE, colour = "grey50", linewidth = 0.5, formula = y ~ x) + geom_point(aes(colour = cnt == "LVA"), size = 2.5) + geom_text(aes(label = cnt), size = 2.4, vjust = -0.8) +
  geom_text(data = D4l, aes(x = -Inf, y = Inf, label = et), hjust = -0.2, vjust = 1.5, size = 3) + scale_colour_manual(values = c(`TRUE` = "#b3261e", `FALSE` = "#2a78d6"), guide = "none") +
  facet_wrap(~ notikums, ncol = 5, labeller = label_wrap_gen(28)) + labs(x = "HBSC saliktā z pārmaiņa starp paaudzēm (+ = labāk)", y = "PISA lasīšanas relatīvā pārmaiņa (punkti)",
                                                                       title = "Vai HBSC pārmaiņa starp paaudzēm paredz PISA relatīvo pārmaiņu? 8 valstis, 9 notikumi (65b paplašināts uz mikrodatiem)",
                                                                       subtitle = "Augšējā rinda: ex ante (tā pati paaudze, HBSC 11 vai 13 g. pirms PISA); apakšējā: vienlaicīgi (15 g.)", caption = "Skripts 65c, docs/78.") + TEMA
zim(p4, "65c_ex_ante_korelacijas", 15, 7)

# ===========================================================================
cat("\n", strrep("=", 100), "\n5. Kopsavilkums un izvades faili\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
cat("Izvade:\n  output/pisa2025/hbsc_mikro_inventars.csv, hbsc_mikro_n.csv, hbsc_mikro_paaudzes.csv, hbsc_mikro_luzumi.csv,\n",
    "  hbsc_mikro_pisa_paaudzes.csv, hbsc_mikro_ex_ante.csv, hbsc_mikro_ex_ante_dati.csv;\n  ", PL, "/65c_latvija_paaudzes.png, 65c_lexis_saliktais.png, 65c_pisa_paaudzes_ex_ante.png, 65c_ex_ante_korelacijas.png\n", sep = "")
cat("\nPabeigts:", format(Sys.time()), "\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
